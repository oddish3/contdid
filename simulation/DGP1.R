# Define the dgp_function as before
dgp_function <- function(dgp) {
  if (dgp == 1) {
    dist <- function(n) rbeta(n, 2, 2)  # Symmetric beta distribution
    func <- function(x) -4*(x - 0.5)^2 + 1
    func_deriv <- function(x) -8*(x - 0.5)
    E_func <- 0.8
    E_deriv <- 0
  }
  if (dgp == 2) {
    dist <- function(n) rbeta(n, 1.5, 3)  # Right-skewed beta distribution
    func <- function(x) x * (1 - x)
    func_deriv <- function(x) 1 - 2*x
    E_func <- 7 / 36  # Analytical solution: 7/36
    E_deriv <- 1 / 3  # Analytical solution: 1/3
  }
  if (dgp == 3) {
    dist <- function(n) rbeta(n, 0.5, 0.5)  # U-shaped beta distribution
    func <- function(x) 4 * (x - 0.5)^2
    func_deriv <- function(x) 8 * (x - 0.5)
    E_func <- 1 / 2  # Analytical solution: 1/2
    E_deriv <- 0  # Analytical solution: 0
  }
  if (dgp == 4) {
    dist <- function(n) runif(n, 0, 1)  # Uniform distribution
    func <- function(x) (sin(pi * x) + 1) / 2  # Shifted and scaled sine function
    func_deriv <- function(x) pi * cos(pi * x) / 2
    E_func <- (1/pi) + 1/2  # Analytical solution: 0.5
    E_deriv <- 0  # Analytical solution: 5/3
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
gdata <- function(n, dgp, prop_treated = 0.8, noise_sd = 0.5, index_sd = 0.5, unobs_het_sd = 0.5, control_trend_mean = 2, control_trend_sd = 0.1) {
  dgp_params <- dgp_function(dgp)

  # Generate treatment assignment
  treatment <- rbinom(n, 1, prop_treated)

  # Generate dose for treated units
  dose <- ifelse(treatment == 1, dgp_params$dist(n), 0)

  # Generate indexes
  index_base <- rnorm(n, mean = 100, sd = index_sd)
  index_unobs_het_t1 <- rnorm(n, mean = 0, sd = unobs_het_sd)
  index_unobs_het_t2 <- rnorm(n, mean = 0, sd = unobs_het_sd)
  control_trend <- rnorm(n, mean = control_trend_mean, sd = control_trend_sd)

  # Generate potential outcomes
  y1 <- index_base + index_unobs_het_t1 + rnorm(n, mean = 0, sd = noise_sd)
  y2_0 <- y1 + control_trend + index_unobs_het_t2 + rnorm(n, mean = 0, sd = noise_sd)
  treatment_effect <- dgp_params$func(dose)
  y2_1 <- y2_0 + treatment_effect

  # Observed outcomes
  y2 <- ifelse(treatment == 1, y2_1, y2_0)

  # Calculate change in outcomes (ΔY)
  delta_y <- y2 - y1

  # Calculate ATT⁰
  att_o <- mean(delta_y[treatment == 1]) - mean(delta_y[treatment == 0])

  # Calculate true ATT and ACR
  pop_att <- dgp_params$E_func
  pop_acr <- dgp_params$E_deriv

  # Calculate true ACR

  x_grid <- seq(0, 1, length.out = 1000)
  func_values <- dgp_params$func(x_grid) + 2
  func_deriv_values <- dgp_params$func_deriv(x_grid)


  df <- data.frame(
    dy = delta_y,
    treatment = treatment,
    dose = dose,
    treatment_effect = treatment_effect
  )

  return(list(
    data = df,
    info = list(
      pop_att = pop_att,
      observed_att = att_o,
      pop_acr = pop_acr,
      treatment_effect_func = dgp_params$func,
      func_values = func_values,
      func_deriv_values = func_deriv_values
    )
  ))
}

# # # Run the function
# set.seed(123)  # for reproducibility
# results <- gdata(100000, dgp = 4)
# data <- results$data
# mean(data$dy[data$treatment == 1]) - mean(data$dy[data$treatment == 0])

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
  bias <- estimate - true_value
  squared_error <- bias^2
  rmse <- sqrt(squared_error)

  if (is.null(lower_ci) || is.null(upper_ci)) {
    lower_ci <- estimate - 1.96 * se
    upper_ci <- estimate + 1.96 * se
  }

  coverage <- (true_value >= lower_ci) && (true_value <= upper_ci)
  ci_length <- upper_ci - lower_ci

  list(
    bias = bias,
    abs_bias = abs(bias),
    squared_error = squared_error,
    rmse = rmse,
    coverage = coverage,
    se = se,
    ci_length = ci_length,
    lower_ci = lower_ci,
    upper_ci = upper_ci
  )
}

ucb_cvge <- function(h0, hhat, sigh, zast, theta, A) {
  # browser()
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

write_debug_info <- function(message, file_path = "debug_output.txt") {
  cat(paste0(Sys.time(), " - ", message, "\n"), file = file_path, append = TRUE)
}
log_dimensions <- function(name, obj) {
  if (is.vector(obj)) {
    write_debug_info(paste(name, "length:", length(obj)))
  } else if (is.matrix(obj) || is.data.frame(obj)) {
    write_debug_info(paste(name, "dimensions:", paste(dim(obj), collapse = "x")))
  } else {
    write_debug_info(paste(name, "type:", class(obj)))
  }
}
