rm(list=ls())
library(parallel)
library(doParallel)
library(foreach)
library(contdid)
library(doRNG)
library(tidyverse)
library(fixest)
library(splines2)
library(triangle)
run_simulation_non_parallel <- function(n) {
  results <- list()

  for (rep in 1:2) {
    # Generate data
    dat <- gdata(n = 100, dgp = 1)
    data <- dat[["data"]]
    info <- dat[["info"]]
    # write.csv(data, "/home/oddish3/Documents/M_folder/CCK-OG/CCK/last.csv")

    # NPIV
    npiv_result <- npiv_regression(
      data = data,
      treatment_col = "dose",
      outcome_col = "dy"
    )
    true_att <- info[["pop_att"]]
    npiv_att_estimate <- npiv_result[["binarised"]][["estimate"]][["binary"]]
    npiv_att_se <- npiv_result[["binarised"]][["std_error"]][["binary"]]
    npiv_att_l <- npiv_result[["binarised"]][["lower_ci"]]
    npiv_att_h <- npiv_result[["binarised"]][["upper_ci"]]
    npiv_metrics <- calculate_point_metrics(npiv_att_estimate, true_att, npiv_att_se, npiv_att_l, npiv_att_h)

    # Compute true func
    x_grid <- seq(0, 1, length.out = length(info$func_values))
    closest_indices <- sapply(npiv_result$Xx, function(x) which.min(abs(x_grid - x)))
    h0 <- info$func_values[closest_indices]
    d0 <- info$func_deriv_values[closest_indices]

    # Compute UCB coverage
    A <- log(log(npiv_result$TJ[npiv_result$Ltil + 1]))

    # Compute coverage for ATT
    att_coverage_result <- ucb_cvge(
      h0 = h0,
      hhat = npiv_result$hhat,
      sigh = npiv_result$sigh,
      zast = npiv_result$hzast[["95%"]],
      theta = npiv_result$thet,
      A = A
    )

    # NPIV ACR
    true_acr <- info[["true_acr"]]
    npiv_acr_estimate <- npiv_result[["ACR_estimate"]]
    npiv_acr_se <- npiv_result[["se_ACR"]]
    npiv_acr_l <- npiv_result[["ci_lower_ACR"]]
    npiv_acr_h <- npiv_result[["ci_upper_ACR"]]
    npiv_metrics_acr <- calculate_point_metrics(npiv_acr_estimate, true_acr, npiv_acr_se, npiv_acr_l, npiv_acr_h)

    # Compute coverage for ACR
    acr_coverage_result <- ucb_cvge(
      h0 = d0,
      hhat = npiv_result$dhat,
      sigh = npiv_result$sigd,
      zast = npiv_result$dzast[["95%"]],
      theta = npiv_result$thet,
      A = A
    )

    # UCB widths
    att_ucb_width <- mean(npiv_result$ATT_upper_UCB - npiv_result$ATT_lower_UCB)
    acr_ucb_width <- mean(npiv_result$ACR_upper_UCB - npiv_result$ACR_lower_UCB)

    # TWFE
    twfe_result <- run_twfe(data)
    twfe_att_estimate <- twfe_result[["acr_estimate"]]
    twfe_att_se <- twfe_result[["acr_se"]]
    twfe_metrics <- calculate_point_metrics(twfe_att_estimate, true_att, twfe_att_se)

    # B-spline
    bspline_result <- run_feols_bspline(data)
    bspline_att_estimate <- bspline_result[["acr_estimate"]]
    bspline_att_se <- bspline_result[["att_se"]]
    bspline_metrics <- calculate_point_metrics(bspline_att_estimate, true_att, bspline_att_se)

    # Store results
    results[[rep]] <- data.frame(
      n = n,
      dgp = 1,
      true_att = true_att,
      npiv_att_estimate = npiv_att_estimate,
      npiv_att_se = npiv_att_se,
      npiv_att_bias = npiv_metrics[["bias"]],
      npiv_att_mse = npiv_metrics[["rmse"]],
      npiv_att_pcoverage = as.numeric(npiv_metrics[["coverage"]]),
      att_ucb_coverage = as.numeric(att_coverage_result[["check"]][1,1]),
      att_ucb_loss = att_coverage_result[["loss"]],
      att_ucb_width = att_ucb_width,
      true_acr = true_acr,
      npiv_acr_estimate = npiv_acr_estimate,
      npiv_acr_se = npiv_acr_se,
      npiv_acr_bias = npiv_metrics_acr[["bias"]],
      npiv_acr_mse = npiv_metrics_acr[["rmse"]],
      npiv_acr_pcoverage = as.numeric(npiv_metrics_acr[["coverage"]]),
      acr_ucb_coverage = as.numeric(acr_coverage_result[["check"]][1,1]),
      acr_ucb_loss = acr_coverage_result[["loss"]],
      acr_ucb_width = acr_ucb_width,
      twfe_estimate = twfe_att_estimate,
      twfe_se = twfe_att_se,
      twfe_bias = twfe_metrics[["bias"]],
      twfe_mse = twfe_metrics[["rmse"]],
      twfe_pcoverage = as.numeric(twfe_metrics[["coverage"]]),
      bspline_estimate = bspline_att_estimate,
      bspline_se = bspline_att_se,
      bspline_bias = bspline_metrics[["bias"]],
      bspline_mse = bspline_metrics[["rmse"]],
      bspline_pcoverage = as.numeric(bspline_metrics[["coverage"]])
    )
  }

  return(do.call(rbind, results))
}
source("~/Documents/uni/master-dissertation/contdid/simulation/DGP1.R")
# debugonce(npiv_regression)
results <- run_simulation_non_parallel(n = 100)  # or whatever sample size you want
