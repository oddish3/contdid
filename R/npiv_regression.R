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
  # pb <- progress::progress_bar$new(total = 7, format = "[:bar] :percent eta: :eta")

  # Check if specified columns exist in the data
  validate_columns <- function(data, columns) {
    if (!all(columns %in% names(data))) {
      stop("Specified column(s) not found in the data.")
    }
  }
  validate_columns(data, c(treatment_col, outcome_col, id_col))
  # Prepare data
  data_full <- data
  ndata <- dim(data_full)
  data_n <- ndata[1]
  data <- data[data[[treatment_col]] != 0, ]

  log_dimensions("data", data)
  log_dimensions("data_full", data_full)

  # Check if there are any non-zero treatment values
  if (nrow(data) == 0) {
    stop("Error: No non-zero treatment values found in the data.")
  }

  # Extract and sort x and y
  sorted_indices <- order(data[[treatment_col]])
  x <- data[[treatment_col]][sorted_indices]
  y <- data[[outcome_col]][sorted_indices]
  id <- if (!is.null(id_col)) data[[id_col]][sorted_indices] else NULL

  # log_dimensions("x", x)
  # log_dimensions("y", y)

  n <- length(x)
  # write_debug_info(paste("n:", n))
  # Pre-compute
  TJ <- 2^((0:nL) + 0) + r - 1
  CJ <- c(0, cumsum(TJ))
  # log_dimensions("TJ", TJ)
  # log_dimensions("CJ", CJ)
  # Create grid for x
  # Xx <- seq(0, 1, length.out = nx)
  # Xx_sub <- Xx[Xx>0.01 & Xx<=0.99]
  Xx <- seq(0, 1, length.out = nx + 1)  # +0 because MATLAB's 0:1/nx:1 includes both endpoints removed + 1
  Xx_sub <- Xx[Xx > 0.01 & Xx <= 0.99]
  # log_dimensions("Xx", Xx)
  # log_dimensions("Xx_sub", Xx_sub)
  # Compute basis functions
  compute_basis_functions <- function(x, nL, r, CJ, derivative = FALSE) {
    result <- matrix(0, nrow = length(x), ncol = CJ[length(CJ)])
    for (ll in 0:nL) {
      result[, (CJ[ll + 1] + 1):CJ[ll + 2]] <-
        if (derivative) bspline(x, ll, r)$DX else bspline(x, ll, r)$XX
    }
    result
  }

  tryCatch({
    PP <- compute_basis_functions(x, nL, r, CJ)
    Px <- compute_basis_functions(Xx_sub, nL, r, CJ)
    Dx <- compute_basis_functions(Xx_sub, nL, r, CJ, derivative = TRUE)
    # write_debug_info("Basis functions computed successfully")
  }, error = function(e) {
    # write_debug_info(paste("Error in computing basis functions:", e$message))
    stop(e)
  })
  # log_dimensions("PP", PP)
  # log_dimensions("Px", Px)
  # log_dimensions("Dx", Dx)
  # pb$tick()
  # browser()
  # Compute resolution levels
  tryCatch({
    Jhat_result <- jhat(PP, PP, CJ, CJ, TJ, M, n, nL)
    Lhat <- Jhat_result$LL
    # write_debug_info(paste("Jhat computed. Lhat =", Lhat))
  }, error = function(e) {
    # write_debug_info(paste("Error in jhat computation:", e$message))
    stop(e)
  })
  # write_debug_info(paste("Lhat:", Lhat))
  # browser()
  # set.seed(1234)
  tryCatch({
    Jlep_result <- jlep(Lhat, Px, PP, PP, as.integer(CJ), as.integer(CJ), as.integer(TJ), y, as.integer(n), 1000)
    Llep <- Jlep_result$LL
    thet <- Jlep_result$theta
    # write_debug_info(paste("Jlep computed. Llep =", Llep, "theta =", thet))
  }, error = function(e) {
    # write_debug_info(paste("Error in jlep computation:", e$message))
    stop(e)
  })
  # pb$tick()
  # write_debug_info(paste("Llep:", Llep))
  # write_debug_info(paste("thet:", thet))
  Ltil <- max(min(Llep, Lhat - 1), 0)
  # write_debug_info(paste("Ltil calculated:", Ltil))
  # write_debug_info(paste("Ltil:", Ltil))
  # Compute estimator and pre-asymptotic standard error
  tryCatch({
    npiv_result <- npiv_estimate_cpp(Ltil, Px, PP, PP, CJ, CJ, y, n)
    hhat <- npiv_result$hhat
    sigh <- npiv_result$sigh
    spline_dosage_SR <- npiv_result$basis_function
    # write_debug_info("NPIV estimation completed successfully")
  }, error = function(e) {
    # write_debug_info(paste("Error in NPIV estimation:", e$message))
    stop(e)
  })
  # log_dimensions("hhat", hhat)
  # log_dimensions("sigh", sigh)
  # log_dimensions("spline_dosage_SR", spline_dosage_SR)

  tryCatch({
    npiv_result_derivative <- npiv_estimate_cpp(Ltil, Dx, PP, PP, CJ, CJ, y, n)
    dhat <- npiv_result_derivative$hhat
    sigd <- npiv_result_derivative$sigh
    spline_dosage_SR1 <- npiv_result_derivative$basis_function
    # pb$tick()
    # write_debug_info("NPIV derivative estimation completed successfully")
  }, error = function(e) {
    # write_debug_info(paste("Error in NPIV derivative estimation:", e$message))
    stop(e)
  })
  # log_dimensions("dhat", dhat)
  # log_dimensions("sigd", sigd)
  # log_dimensions("spline_dosage_SR1", spline_dosage_SR1)
  # pb$tick()

  # Compute critical value for UCB
  tryCatch({
    hzast <- ucb_cv(Ltil, Lhat, Px, PP, PP, CJ, CJ, y, n, 1000, 0, alpha)
    dzast <- ucb_cv(Ltil, Lhat, Dx, PP, PP, CJ, CJ, y, n, 1000, 0, alpha)
    # write_debug_info("UCB critical values computed successfully")
  }, error = function(e) {
    # write_debug_info(paste("Error in computing UCB critical values:", e$message))
    stop(e)
  })
  # log_dimensions("hzast", hzast)
  # log_dimensions("dzast", dzast)

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
  tryCatch({
    # Precalculate UCBs
    adjustment_term <- thet * log(log(TJ[Llep + 1]))
    ATT_upper_UCB <- hhat + (hzast + adjustment_term) * npiv_result$sigh
    ATT_lower_UCB <- hhat - (hzast + adjustment_term) * npiv_result$sigh
    ACR_upper_UCB <- dhat + (dzast + adjustment_term) * npiv_result_derivative$sigh
    ACR_lower_UCB <- dhat - (dzast + adjustment_term) * npiv_result_derivative$sigh
    # write_debug_info("UCBs calculated successfully")
  }, error = function(e) {
    # write_debug_info(paste("Error in calculating UCBs:", e$message))
    stop(e)
  })
  # log_dimensions("ATT_upper_UCB", ATT_upper_UCB)
  # log_dimensions("ATT_lower_UCB", ATT_lower_UCB)
  # log_dimensions("ACR_upper_UCB", ACR_upper_UCB)
  # log_dimensions("ACR_lower_UCB", ACR_lower_UCB)
  # Construct eta and compute its sample variance
  tryCatch({
    E_Y_D0 <- mean(data_full[[outcome_col]][data_full[[treatment_col]] == 0])
    u_hat <- y - E_Y_D0 - hhat[findInterval(x, Xx_sub)]
    # write_debug_info("E_Y_D0 and u_hat calculated successfully")
  }, error = function(e) {
    # write_debug_info(paste("Error in calculating E_Y_D0 or u_hat:", e$message))
    stop(e)
  })
  # log_dimensions("E_Y_D0", E_Y_D0)
  # log_dimensions("u_hat", u_hat)

  compute_eta_values <- function(data, treatment_col, outcome_col, Xx_sub, dhat, spline_dosage_SR, spline_dosage_SR1, u_hat) {
    log_info <- function(message) {
      # write_debug_info(paste("compute_eta_values:", message))
    }

    log_dim <- function(name, obj) {
      if (is.vector(obj)) {
        # log_info(paste(name, "length:", length(obj)))
      } else if (is.matrix(obj) || is.data.frame(obj)) {
        # log_info(paste(name, "dimensions:", paste(dim(obj), collapse = "x")))
      } else {
        # log_info(paste(name, "type:", class(obj)))
      }
    }

    # log_dim("data", data)
    # log_dim("Xx_sub", Xx_sub)
    # log_dim("dhat", dhat)
    # log_dim("spline_dosage_SR", spline_dosage_SR)
    # log_dim("spline_dosage_SR1", spline_dosage_SR1)
    # log_dim("u_hat", u_hat)

    D <- data[[treatment_col]]
    Y <- data[[outcome_col]]
    # log_dim("D", D)
    # log_dim("Y", Y)

    D_min <- min(D)
    D_max <- max(D)
    Xx_min <- min(Xx_sub)
    Xx_max <- max(Xx_sub)
    log_info(paste("D range:", D_min, "to", D_max))
    # log_info(paste("Xx_sub range:", Xx_min, "to", Xx_max))

    # Clamp D values to the range of Xx_sub
    D_clamped <- pmax(pmin(D, Xx_max), Xx_min)
    if (any(D != D_clamped)) {
      log_info(paste("Warning:", sum(D != D_clamped), "D values clamped to Xx_sub range"))
    }

    intervals <- findInterval(D_clamped, Xx_sub)
    log_dim("intervals", intervals)

    ACR_D <- dhat[intervals]
    log_dim("ACR_D", ACR_D)

    E_ACR <- mean(ACR_D)
    log_info(paste("E_ACR:", E_ACR))

    psi_D_derivative <- spline_dosage_SR1[intervals, ]
    log_dim("psi_D_derivative", psi_D_derivative)

    E_dpsi <- colMeans(psi_D_derivative)
    log_dim("E_dpsi", E_dpsi)

    psi_D <- spline_dosage_SR[intervals, ]
    log_dim("psi_D", psi_D)

    E_psi_psi_inv <- MASS::ginv(t(psi_D) %*% psi_D / nrow(data))
    log_dim("E_psi_psi_inv", E_psi_psi_inv)

    correction_term <- (psi_D %*% E_psi_psi_inv %*% E_dpsi) * u_hat
    log_dim("correction_term", correction_term)

    result <- ACR_D - E_ACR + as.vector(correction_term)
    log_dim("result", result)

    return(result)
  }

  # write_debug_info("Starting compute_eta_values")
  tryCatch({
    eta_values <- compute_eta_values(data, treatment_col, outcome_col, Xx_sub, dhat, spline_dosage_SR, spline_dosage_SR1, u_hat)
    # write_debug_info(paste("eta_values computed successfully. Length:", length(eta_values)))
  }, error = function(e) {
    # write_debug_info(paste("Error in compute_eta_values:", e$message))
    stop(e)
  })
  # write_debug_info("Calculating variance_ACR")
  tryCatch({
    variance_ACR <- mean(eta_values^2)
    # write_debug_info(paste("variance_ACR calculated:", variance_ACR))
  }, error = function(e) {
    # write_debug_info(paste("Error in calculating variance_ACR:", e$message))
    stop(e)
  })
  # write_debug_info("Calculating se_ACR")
  tryCatch({
    se_ACR <- sqrt(variance_ACR / length(eta_values))
    # write_debug_info(paste("se_ACR calculated:", se_ACR))
  }, error = function(e) {
    # write_debug_info(paste("Error in calculating se_ACR:", e$message))
    stop(e)
  })
  # pb$tick()

  # write_debug_info("Calculating ACR_estimate")
  tryCatch({
    ACR_estimate <- mean(dhat[findInterval(data[[treatment_col]], Xx_sub)])
    # write_debug_info(paste("ACR_estimate calculated:", ACR_estimate))
  }, error = function(e) {
    # write_debug_info(paste("Error in calculating ACR_estimate:", e$message))
    stop(e)
  })
  # Compute statistics
  # write_debug_info("Computing statistics")
  tryCatch({
    n_treated <- sum(data_full[[treatment_col]] > 0)
    df <- n_treated - ncol(spline_dosage_SR)
    t_statistic <- ACR_estimate / se_ACR
    p_value_ACR <- 2 * stats::pt(-abs(t_statistic), df=df)
    t_crit <- stats::qt(0.975, df=df)
    ci_lower_ACR <- ACR_estimate - t_crit * se_ACR
    ci_upper_ACR <- ACR_estimate + t_crit * se_ACR
    # write_debug_info("All statistics computed successfully")
  }, error = function(e) {
    # write_debug_info(paste("Error in computing statistics:", e$message))
    stop(e)
  })

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
