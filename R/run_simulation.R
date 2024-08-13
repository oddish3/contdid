run_simulation <- function(n_values = c(100, 500, 1000), dgp, nrep = 1000) {
  results <- foreach(
    n = rep(n_values, each = nrep),
    rep = rep(1:nrep, times = length(n_values)),
    .combine = 'rbind',
    .packages = c("contdid", "stats", "splines2", "fixest", "MASS")
  ) %dorng% {
    # Generate data
    dat <- gdata(n = n, dgp = dgp)
    data <- dat[["data"]]
    info <- dat[["info"]]

    # Run estimators and calculate metrics
    # NPIV
    npiv_result <- npiv_regression(
      data = data,
      treatment_col = "dose",
      outcome_col = "dy",
    )
    true_att <- info[["pop_att"]]
    npiv_att_estimate <- npiv_result[["binarised"]][["estimate"]][["binary"]]
    npiv_att_se <- npiv_result[["binarised"]][["std_error"]][["binary"]]
    npiv_att_l <- npiv_result[["binarised"]][["lower_ci"]]
    npiv_att_h <- npiv_result[["binarised"]][["upper_ci"]]
    npiv_metrics <- calculate_point_metrics(npiv_att_estimate, true_att, npiv_att_se, npiv_att_l, npiv_att_h)

    # UCB coverage
    h0 <- info[["func_values"]][order(npiv_result[["Xx"]])]
    ucb_result <- ucb_cvge(
      h0 = h0,
      hhat = npiv_result[["hhat"]],
      sigh = npiv_result[["sigh"]],
      zast = npiv_result[["hzast"]],
      theta = npiv_result[["thet"]]
    )

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

    # Collate all metrics into a single data frame
    data.frame(
      rep = rep,
      n = n,
      dgp = dgp,
      true_att = true_att,
      npiv_estimate = npiv_att_estimate,
      npiv_se = npiv_att_se,
      npiv_bias = npiv_metrics[["bias"]],
      npiv_mse = npiv_metrics[["rmse"]],
      npiv_coverage = as.numeric(npiv_metrics[["coverage"]]),
      ucb_coverage = mean(ucb_result[["check"]]),
      ucb_loss = ucb_result[["loss"]],
      twfe_estimate = twfe_att_estimate,
      twfe_se = twfe_att_se,
      twfe_bias = twfe_metrics[["bias"]],
      twfe_mse = twfe_metrics[["rmse"]],
      twfe_coverage = as.numeric(twfe_metrics[["coverage"]]),
      bspline_estimate = bspline_att_estimate,
      bspline_se = bspline_att_se,
      bspline_bias = bspline_metrics[["bias"]],
      bspline_mse = bspline_metrics[["rmse"]],
      bspline_coverage = as.numeric(bspline_metrics[["coverage"]])
    )
  }
  return(results)
}
