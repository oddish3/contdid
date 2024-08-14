ucb_cvge <- function(h0, hhat, sigh, zast, theta, A) {
  loss <- max(abs(hhat - h0))
  tmax <- max(abs((hhat - h0) / sigh))
  check <- matrix(0, length(zast), length(A))
  for (i in 1:length(zast)) {
    for (j in 1:length(A)) {
      check[i, j] <- as.numeric(tmax <= zast[i] + A[j] * theta)
    }
  }
  return(list(check = check, loss = loss))
}

# Define the dgp_function as before
dgp_function <- function(dgp) {
  if (dgp == 1) {
    dist <- function(n) runif(n, 0, 1)
    func <- function(x) x^2
    func_deriv <- function(x) 2*x
    E_func <- 1/3
    E_deriv <- 1
  }

  if (dgp == 2) {
    dist <- function(n) rtriangle(n, a=0, b=1, c=1)
    func <- function(x) 3*x - 1
    func_deriv <- function(x) 3
    E_func <- 1/2
    E_deriv <- 3
  }

  if (dgp == 3) {
    dist <- function(n) runif(n, 0, 1)
    func <- function(x) x^3
    func_deriv <- function(x) 3*x^2
    E_func <- 1/4
    E_deriv <- 1
  }

  if (dgp == 4) {
    dist <- function(n) rbeta(n, 2, 3)
    func <- function(x) (x^2 + x) / 2
    func_deriv <- function(x) x + 1/2
    E_func <- 5/12
    E_deriv <- 7/5
  }

  return(list(
    dist = dist,
    func = func,
    func_deriv = func_deriv,
    E_func = E_func,
    E_deriv = E_deriv
  ))
}

# Now, modify the gdata function to incorporate the dgp_function
gdata <- function(n, dgp, prop_treated = 0.8, noise_sd = 1, index_sd = 15, unobs_het_sd = 15) {
  # Get the appropriate dist, func, and func_deriv based on the dgp value
  dgp_params <- dgp_function(dgp)

  # Generate treatment assignment
  treatment <- rbinom(n, 1, prop_treated)

  # Generate dose for treated units using the dist function
  dose <- rep(0, n)
  dose[treatment == 1] <- dgp_params$dist(sum(treatment))

  # Generate indexes
  index_base <- rnorm(n, mean = 100, sd = index_sd)  # Reduced SD for base index
  index_unobs_het <- rnorm(n, mean = 0, sd = unobs_het_sd)  # Reduced SD for unobserved heterogeneity

  # Generate outcomes at time 1 (pre-treatment)
  y1 <- index_base + index_unobs_het + rnorm(n, mean = 0, sd = noise_sd)

  # Calculate treatment effect for treated units using the func function
  treatment_effect <- ifelse(treatment == 1, dgp_params$func(dose), 0)

  # Generate outcomes at time 2 (post-treatment)
  treat_noise <- treatment_effect + rnorm(n, mean = 0, sd = 0)
  noisy_treatment_effect <- treat_noise[treatment == 1]
  y2 <- y1 + treat_noise

  # Calculate change in outcomes
  dy <- y2 - y1

  # Calculate true ATT
  att <- mean(treatment_effect[treatment == 1])

  # Calculate observed ATE (difference-in-differences)
  diff_treated <- mean(dy[treatment == 1])
  diff_control <- mean(dy[treatment == 0])
  observed_ate <- diff_treated - diff_control

  # Calculate true ACR
  true_acr <- mean(dgp_params$func_deriv(dose[dose > 0]))

  x_grid <- seq(0, 1, length.out = 1000)
  func_values <- dgp_params$func(x_grid)
  func_deriv_values <- dgp_params$func_deriv(x_grid)

  df <- data.frame(
    y1 = y1,
    y2 = y2,
    dy = dy,
    treatment = treatment,
    dose = dose
  )

  return(list(
    data = df,
    info = list(
      pop_att = att,
      obs_ate = observed_ate,
      true_acr = true_acr,
      func_values = func_values,
      func_deriv_values = func_deriv_values,
      noisy_treatment_effect = noisy_treatment_effect
    )
  ))
}

# # # Run the function
# set.seed(123)  # for reproducibility
# results <- gdata(100, dgp = 1)  # Replace 1 with the appropriate dgp value
#
# # Print results
# cat("Population ATT:", results$pop_att, "\n")
# cat("Sample ATT:", results$samp_att, "\n")
# cat("Estimated ATE:", results$est_att, "\n")
# cat("Population ACR:", results$pop_acr, "\n")
# cat("Estimated ACR:", results$est_acr, "\n")



# TWFE estimator
run_twfe <- function(data) {
  twfe_model <- lm(dy ~ treatment + dose, data = data)
  coef_summary <- summary(twfe_model)$coefficients
  list(
    acr_estimate = coef_summary["dose", "Estimate"],
    acr_se = coef_summary["dose", "Std. Error"],
    acr_pval = coef_summary["dose", "Pr(>|t|)"],
    acr_tval = coef_summary["dose", "t value"]
  )
}

# B-spline estimator
run_feols_bspline <- function(data) {
  # browser()
  tryCatch({
    if (any(data$dose < 0 | data$dose > 1, na.rm = TRUE)) {
      knots <- c(0.4, 0.5, 0.6)
    } else {
      quantiles <- quantile(data$dose, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      if (length(unique(quantiles)) == 3) {
        margin <- 0.05
        knots <- pmax(margin, pmin(1 - margin, quantiles))
      } else {
        knots <- c(0.4, 0.5, 0.6)
      }
    }

    formula <- bquote(
      dy ~ splines2::bSpline(dose, knots = .(knots), degree = 3, intercept = TRUE) - 1
    )

    model <- suppressWarnings(
      fixest::feols(
        eval(formula),
        data = data,
        # cluster = ~ individual
      )
    )

    att_SR <- predict(model)
    mean_att_SR <- mean(att_SR)

    spline_dosage_SR <- bSpline(data$dose,
                                knots = knots,
                                degree = 3,
                                intercept = TRUE)
    n_treated_SR <- sum(data$dose > 0)
    infl_reg_SR <- model$residuals * spline_dosage_SR %*%
      (MASS::ginv(t(spline_dosage_SR) %*% spline_dosage_SR / n_treated_SR))
    infl_att_SR <- infl_reg_SR %*% t(spline_dosage_SR)
    se_att_SR <- sqrt(mean(colMeans(infl_att_SR^2)) / n_treated_SR)

    derivative_spline_dosage_SR <- dbs(data$dose,
                                       knots = knots,
                                       degree = 3,
                                       intercept = TRUE)
    coefficients_SR <- coef(model)
    acrt_SR <- as.vector(derivative_spline_dosage_SR %*% coefficients_SR)

    acrt_SR_filtered <- acrt_SR[data$dose > 0]
    ACR_hat_0 <- mean(acrt_SR_filtered)

    list(
      att_estimate = mean_att_SR,
      att_se = se_att_SR,
      acr_estimate = ACR_hat_0
    )
  }, error = function(e) {
    return(list(
      att_estimate = NA_real_,
      att_se = NA_real_,
      acr_estimate = NA_real_,
    ))
  })
}

calculate_point_metrics <- function(estimate, true_value, se, lower_ci = NULL, upper_ci = NULL) {
  # browser()
  bias <- estimate - true_value
  rmse <- sqrt(bias^2)

  if (is.null(lower_ci) || is.null(upper_ci)) {
    coverage <- !is.na(se) && (true_value >= estimate - 1.96*se) && (true_value <= estimate + 1.96*se)
  } else {
    coverage <- (true_value >= lower_ci) && (true_value <= upper_ci)
  }

  list(
    bias = bias,
    abs_bias = abs(bias),
    rmse = rmse,
    coverage = coverage
  )
}

# ucb_cvge <- function(h0, hhat, sigh, zast, theta, A = 0.05) {
#   loss <- max(abs(hhat - h0))
#   tmax <- max(abs((hhat - h0) / sigh))
#   check <- matrix(0, length(zast), length(A))
#   for (i in seq_along(zast)) {
#     for (j in seq_along(A)) {
#       check[i, j] <- as.numeric(tmax <= zast[i] + A[j] * theta)
#     }
#   }
#   browser()
#   return(list(check = check, loss = loss, tmax = tmax))
# }


