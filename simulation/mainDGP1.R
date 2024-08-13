# Clear memory
rm(list=ls())

# Load required libraries
library(fixest)
library(splines2)
library(parallel)
library(doParallel)
library(doSNOW)
library(doRNG)
library(foreach)
library(truncnorm)
library(triangle)
library(tibble)
# remove.packages("contdid")
# if (!requireNamespace("contdid", quietly = TRUE)) {
#   devtools::install_github("oddish3/contdid")
# }

# setwd("~/Documents/uni/master-dissertation/contdid")
# devtools::load_all()
library(contdid)



# Set parameters
nrep <- 1000  # Monte Carlo replications
address <- "/home/oddish3/Documents/uni/master-dissertation/code-cont/simulation"
setwd(address)

# Source functions
source(paste0(address, "/", "DGP1.R"))

# Set seed
seed1 <- 1234
set.seed(seed1)

# Helper function for sup norm loss
calculate_sup_norm_loss <- function(estimated_func, true_func, grid) {
  max(abs(estimated_func(grid) - true_func(grid)))
}

# Function to calculate metrics for point estimates
calculate_point_metrics <- function(estimate, true_value, se) {
  # browser()
  bias <- estimate - true_value
  list(
    bias = bias,
    abs_bias = abs(bias),
    rmse = sqrt(bias^2),
    coverage = !is.na(se) && (true_value >= estimate - 1.96*se) && (true_value <= estimate + 1.96*se)
  )
}

# Function to calculate metrics for function estimates
calculate_function_metrics <- function(estimated_func, true_func, ucb_func, eval_points) {
  true_values <- sapply(eval_points, true_func)
  estimated_values <- sapply(eval_points, estimated_func)
  ucb_values <- lapply(eval_points, ucb_func)
  
  bias <- mean(estimated_values - true_values)
  rmse <- sqrt(mean((estimated_values - true_values)^2))
  sup_norm_loss <- max(abs(estimated_values - true_values))
  
  ucb_coverage <- mean(
    sapply(seq_along(eval_points), function(i) {
      true_values[i] >= ucb_values[[i]]$lower && true_values[i] <= ucb_values[[i]]$upper
    })
  )
  
  list(
    bias = bias,
    rmse = rmse,
    sup_norm_loss = sup_norm_loss,
    ucb_coverage = ucb_coverage
  )
}

# NPIV estimator
run_npiv <- function(data) {
  npiv_result <- npiv_regression(
    data = data, 
    treatment_col = "dose",
    outcome_col = "dy",
  )
  #extract values
  Xx_sub <- npiv_result$Xx
  hhat <- npiv_result$hhat
  hzast <- npiv_result$hzast
  dzast <- npiv_result$dzast #deriv
  thet <- npiv_result$thet
  TJ <- npiv_result$TJ
  Llep <- npiv_result$Llep
  
  list(
    att_estimate = npiv_result[["binarised"]][["estimate"]][["binary"]],
    att_se = npiv_result[["binarised"]][["std_error"]][["binary"]],
    att_upper_ucb = npiv_result[["ATT_upper_UCB"]],
    att_lower_ucb = npiv_result[["ATT_lower_UCB"]],
    att_point_low = npiv_result[["binarised"]][["lower_ci"]],
    att_point_up = npiv_result[["binarised"]][["upper_ci"]],
    acr_estimate = npiv_result$ACR_estimate,
    acr_se = npiv_result$se_ACR,
    acr_t_val = npiv_result[["t_statistic"]],
    acr_p_val = npiv_result[["p_value"]],
    acr_lower_ucb = npiv_result[["ACR_lower_UCB"]],
    acr_upper_ucb = npiv_result[["ACR_upper_UCB"]],
    acr_point_low = npiv_result[["ci_lower_ACR"]],
    acr_point_up = npiv_result[["ci_upper_ACR"]]
  )
}

# TWFE estimator
run_twfe <- function(data) {
  twfe_model <- lm(dy ~ treatment + dose, data = data)
  coef_summary <- summary(twfe_model)$coefficients
  list(
    acr_estimate = coef_summary["dose", "Estimate"],
    acr_se = coef_summary["dose", "Std. Error"]
  )
}

# B-spline estimator
run_feols_bspline <- function(data) {
  tryCatch({
    if (any(data$dose < 0 | data$dose > 1, na.rm = TRUE)) {
      knots <- c(0.3, 0.5, 0.6)
    } else {
      quantiles <- quantile(data$dose, probs = c(0.25, 0.5, 0.75))
      margin <- 0.05
      knots <- pmax(margin, pmin(1 - margin, quantiles))
    }
    
    formula <- bquote(
      dy ~ bSpline(dose, knots = .(knots), degree = 3, intercept = TRUE) - 1
    )
    
    model <- suppressWarnings(
      feols(
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
      acr_estimate = ACR_hat_0,
      acr_se = NA
    )
  }, error = function(e) {
    return(list(
      att_estimate = NA_real_,
      att_se = NA_real_,
      acr_estimate = NA_real_,
      acr_se = NA_real_
    ))
  })
}

n = 100
dgp = 1

# Main simulation loop
for (nn in 2:2) {
  for (dgp in 1:4) {
    # Set sample size
    n <- switch(nn, 100, 500, 1000)
    
    # Run Monte Carlo simulation
    source(paste0(address, "/", "/sim-DGP1.R"))
    
  }
}
