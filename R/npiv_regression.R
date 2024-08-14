#' Non-parametric Instrumental Variable Regression
#'
#' @param data A data frame containing the variables for the regression.
#' @param treatment_col Name of the treatment variable column.
#' @param outcome_col Name of the outcome variable column.
#' @param id_col Name of the ID variable column (optional).
#' @param alpha Significance level(s) for confidence intervals.
#' @param nx Number of points for the grid of x values.
#' @param nL Maximum resolution level for J.
#' @param r B-spline order.
#' @param M Ex ante upper bound on sup_x h_0(x).
#'
#' @return A list containing the results of the NPIV regression.
#' @export
#'
npiv_regression <- function(data,
                            treatment_col,
                            outcome_col,
                            id_col = NULL,
                            alpha = c(0.05), # 0.1. 0.01
                            nx = 1000,
                            nL = 9,
                            r = 4,
                            M = 5) {
  # set.seed(1234567)
  pb <- progress::progress_bar$new(total = 7, format = "[:bar] :percent eta: :eta")

  # Check if specified columns exist in the data
  validate_columns <- function(data, columns) {
    if (!all(columns %in% names(data))) {
      stop("Specified column(s) not found in the data.")
    }
  }
  validate_columns(data, c(treatment_col, outcome_col, id_col))
  # Prepare data
  data_full <- data
  data <- data[data[[treatment_col]] != 0, ]

  # Check if there are any non-zero treatment values
  if (nrow(data) == 0) {
    stop("Error: No non-zero treatment values found in the data.")
  }

  # Extract and sort x and y
  sorted_indices <- order(data[[treatment_col]])
  x <- data[[treatment_col]][sorted_indices]
  y <- data[[outcome_col]][sorted_indices]
  id <- if (!is.null(id_col)) data[[id_col]][sorted_indices] else NULL

  n <- length(x)

  # Pre-compute
  TJ <- 2^((0:nL) + 0) + r - 1
  CJ <- c(0, cumsum(TJ))

  # Create grid for x
  # Xx <- seq(0, 1, length.out = nx)
  # Xx_sub <- Xx[Xx>0.01 & Xx<=0.99]
  Xx <- seq(0, 1, length.out = nx + 1)  # +1 because MATLAB's 0:1/nx:1 includes both endpoints
  Xx_sub <- Xx[Xx > 0.01 & Xx <= 0.99]

  # Compute basis functions
  compute_basis_functions <- function(x, nL, r, CJ, derivative = FALSE) {
    result <- matrix(0, nrow = length(x), ncol = CJ[length(CJ)])
    for (ll in 0:nL) {
      result[, (CJ[ll + 1] + 1):CJ[ll + 2]] <-
        if (derivative) bspline(x, ll, r)$DX else bspline(x, ll, r)$XX
    }
    result
  }

  PP <- compute_basis_functions(x, nL, r, CJ)
  Px <- compute_basis_functions(Xx_sub, nL, r, CJ)
  Dx <- compute_basis_functions(Xx_sub, nL, r, CJ, derivative = TRUE)
  pb$tick()

  # Compute resolution levels
  Jhat_result <- jhat(PP, PP, CJ, CJ, TJ, M, n, nL)
  Lhat <- Jhat_result$LL

  # browser()
  # set.seed(1234)
  Jlep_result <- jlep(Lhat, Px, PP, PP, as.integer(CJ), as.integer(CJ), as.integer(TJ), y, as.integer(n), 1000)
  Llep <- Jlep_result$LL
  thet <- Jlep_result$theta
  pb$tick()

  Ltil <- max(min(Llep, Lhat - 1), 0)

  # Compute estimator and pre-asymptotic standard error
  npiv_result <- npiv_estimate_cpp(Ltil, Px, PP, PP, CJ, CJ, y, n)
  hhat <- npiv_result$hhat
  sigh <- npiv_result$sigh
  # browser()
  spline_dosage_SR <- npiv_result$basis_function

  npiv_result_derivative <- npiv_estimate_cpp(Ltil, Dx, PP, PP, CJ, CJ, y, n)
  dhat <- npiv_result_derivative$hhat
  sigd <- npiv_result_derivative$sigh
  spline_dosage_SR1 <- npiv_result_derivative$basis_function
  pb$tick()

  # Compute critical value for UCB
  hzast <- ucb_cv(Ltil, Lhat, Px, PP, PP, CJ, CJ, y, n, 1000, 0, alpha)
  dzast <- ucb_cv(Ltil, Lhat, Dx, PP, PP, CJ, CJ, y, n, 1000, 0, alpha)


  # Calculate ATT^o and TWFE
  data_full$binary <- as.integer(data_full[[treatment_col]] > 0)
  binarised <- tryCatch({
    # Fit the model using fixest
    model <- fixest::feols(as.formula(paste(outcome_col, "~ binary")), data = data_full)

    # Check if the "binary" coefficient is present
    if (!"binary" %in% names(coef(model))) {
      stop("Error: The binary treatment variable was removed due to collinearity.")
    }

    # Extract the coefficient and standard error for "binary"
    estimate <- coef(model)["binary"]
    std_error <- fixest::se(model)["binary"]

    # Calculate degrees of freedom
    df <- model$nobs - length(coef(model))  # n - k - 1

    # Calculate the critical t value for a 95% confidence level
    t_value <- qt(0.975, df)

    # Calculate margin of error
    margin_of_error <- t_value * std_error

    # Calculate confidence intervals
    lower_ci <- estimate - margin_of_error
    upper_ci <- estimate + margin_of_error

    # Return model along with confidence intervals
    list(
      model = model,
      estimate = estimate,
      std_error = std_error,
      lower_ci = lower_ci,
      upper_ci = upper_ci
    )

  }, error = function(e) {
    stop(paste("Error in binarised regression:", e$message))
  })

  # Precalculate UCBs
  adjustment_term <- thet * log(log(TJ[Llep + 1]))

  ATT_upper_UCB <- hhat + (hzast + adjustment_term) * npiv_result$sigh
  ATT_lower_UCB <- hhat - (hzast + adjustment_term) * npiv_result$sigh

  ACR_upper_UCB <- dhat + (dzast + adjustment_term) * npiv_result_derivative$sigh
  ACR_lower_UCB <- dhat - (dzast + adjustment_term) * npiv_result_derivative$sigh

  # Construct eta and compute its sample variance
  E_Y_D0 <- mean(data_full[[outcome_col]][data_full[[treatment_col]] == 0])
  u_hat <- y - E_Y_D0 - hhat[findInterval(x, Xx_sub)]

  compute_eta_values <- function(data, treatment_col, outcome_col, Xx_sub, dhat, spline_dosage_SR, spline_dosage_SR1, u_hat) {
    # browser()
    D <- data[[treatment_col]]
    Y <- data[[outcome_col]]

    ACR_D <- dhat[findInterval(D, Xx_sub)]
    E_ACR <- mean(ACR_D)

    psi_D_derivative <- spline_dosage_SR1[findInterval(D, Xx_sub), ]
    E_dpsi <- colMeans(psi_D_derivative)

    psi_D <- spline_dosage_SR[findInterval(D, Xx_sub), ]
    E_psi_psi_inv <- MASS::ginv(t(psi_D) %*% psi_D / nrow(data))

    correction_term <- (psi_D %*% E_psi_psi_inv %*% E_dpsi) * u_hat

    ACR_D - E_ACR + as.vector(correction_term)
  }

  eta_values <- compute_eta_values(data, treatment_col, outcome_col, Xx_sub, dhat, spline_dosage_SR, spline_dosage_SR1, u_hat)

  variance_ACR <- mean(eta_values^2)
  se_ACR <- sqrt(variance_ACR / length(eta_values))
  pb$tick()

  ACR_estimate <- mean(dhat[findInterval(data[[treatment_col]], Xx_sub)])

  # Compute statistics
  n_treated <- sum(data_full[[treatment_col]] > 0)
  df <- n_treated - ncol(spline_dosage_SR)
  t_statistic <- ACR_estimate / se_ACR
  p_value_ACR <- 2 * stats::pt(-abs(t_statistic), df=df)

  t_crit <- stats::qt(0.975, df=df)

  ci_lower_ACR <- ACR_estimate - t_crit * se_ACR
  ci_upper_ACR <- ACR_estimate + t_crit * se_ACR

  # Return results
  # browser()
  list(
    x = x,
    y = y,
    Xx = Xx_sub,
    TJ = TJ,
    Llep = Llep,
    Ltil = Ltil,
    hhat = hhat,
    sigh = sigh,
    sigd = sigd,
    hzast = hzast,
    thet = thet,
    binarised = binarised,
    ATT_upper_UCB = ATT_upper_UCB,
    ATT_lower_UCB = ATT_lower_UCB,
    dzast = dzast,
    ACR_estimate = ACR_estimate,
    se_ACR = se_ACR,
    dhat = dhat,
    t_statistic_ACR = t_statistic,
    p_value_ACR = p_value_ACR,
    ACR_upper_UCB = ACR_upper_UCB,
    ACR_lower_UCB = ACR_lower_UCB,
    ci_lower_ACR = ci_lower_ACR,
    ci_upper_ACR = ci_upper_ACR
  )
}
