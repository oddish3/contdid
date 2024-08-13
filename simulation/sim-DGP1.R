# Set up parallel processing
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
registerDoRNG()

# Set seed
seed <- 1234  # You can change this or make it a parameter
set.seed(seed)
iseed <- floor(seed + n * 2598 + dgp * 711)
set.seed(iseed)

clusterExport(cl, c("dgp_function", "gdata", "run_npiv", "run_twfe", "run_feols_bspline", "calculate_point_metrics",
                    "calculate_sup_norm_loss", "calculate_function_metrics", "npiv_regression"))


# Run Monte Carlo simulation
run_simulation <- function(dgp, n, nrep) {
  results <- foreach(
    rep = 1:nrep,
    .combine = 'rbind',
    .packages = c("tibble", "triangle", "contdid", "splines2", "fixest")
  ) %dorng% {
    # Generate data
    df <- gdata(n, dgp)
    data <- df$data
    # # Run estimators
    npiv_result <- run_npiv(data)
    # twfe_result <- run_twfe(data)
    # bspline_result <- run_feols_bspline(data)
    # 
    # # Calculate metrics for point estimates
    # npiv_att_metrics <- calculate_point_metrics(
    #   npiv_result[["att_estimate"]],
    #   df$info$pop_att,
    #   npiv_result[["att_se"]]
    # )
    # npiv_acr_metrics <- calculate_point_metrics(
    #   npiv_result$acr_estimate,
    #   df$info$pop_acr,
    #   npiv_result$acr_se
    # )
    # twfe_acr_metrics <- calculate_point_metrics(
    #   twfe_result$acr_estimate,
    #   df$info$pop_acr,
    #   twfe_result$acr_se
    # )
    # bspline_att_metrics <- calculate_point_metrics(
    #   bspline_result$att_estimate,
    #   df$info$pop_att,
    #   bspline_result$att_se
    # )
    # bspline_acr_metrics <- calculate_point_metrics(
    #   bspline_result$acr_estimate,
    #   df$info$pop_acr,
    #   NA
    # )
  
    # Return results for this replication
    c(
      data = data
      # dgp = dgp,
      # n = n,
      # rep = rep,
      # true_att = data$pop_att,
      # true_acr = data$pop_acr,
      # npiv_att = unlist(npiv_att_metrics),
      # npiv_acr = unlist(npiv_acr_metrics),
      # twfe_acr = unlist(twfe_acr_metrics),
      # bspline_att = unlist(bspline_att_metrics),
      # bspline_acr = unlist(bspline_acr_metrics)
    )
  }
  
  return(as_tibble(results))
}

# Stop cluster
stopCluster(cl)


# # Convert results to an array
# mc <- array(unlist(results), dim = c(nrow(results[[1]]), ncol(results[[1]]), length(results)))
# mc <- t(matrix(mc, 28, nrep))
# 
# # Calculate summary statistics
# mean.mc <- colMeans(mc, na.rm = TRUE)
# median.mc <- apply(mc, 2, median, na.rm = TRUE)
# bias.mc <- colMeans(mc - mc[,1], na.rm = TRUE)
# median.bias.mc <- apply(mc - mc[,1], 2, median, na.rm = TRUE)
# sd.mc <- sqrt(colMeans(mc^2, na.rm = TRUE) - colMeans(mc, na.rm = TRUE)^2)
# rmse.mc <- sqrt(colMeans((mc - mc[,1])^2, na.rm = TRUE))
# mae.mc <- colMeans(abs(mc - mc[,1]), na.rm = TRUE)
# 
# # Create summary tables for each estimator
# # (Your code for creating summary tables goes here)
# 
# # Save results
# write.csv(mc.summary, file = paste0(address, "/Results.panel/mc.summary.dgp-", dgp, ".n-", n, ".csv"))

