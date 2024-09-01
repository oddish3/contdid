
event_study_npiv <- function(data,
                             treatment_col,
                             outcome_col,
                             time_col,
                             event_time_col,
                             id_col = NULL,
                             base_year = 1983,
                             event_window = c(-3, 3),
                             alpha = 0.05,
                             nx = 1000,
                             nL = 9,
                             r = 4,
                             M = 5) {

  # Calculate relative time to event
  data$relative_time <- data[[time_col]] - data[[event_time_col]]

  # Filter data to be within the event window
  data_filtered <- data[data$relative_time >= event_window[1] &
                          data$relative_time <= event_window[2], ]

  # Initialize results list
  results <- list()

  # Iterate over unique relative times
  for (t in sort(unique(data_filtered$relative_time))) {
    if (t == 0) next  # Skip base year

    # Subset data for current relative time
    data_t <- data_filtered[data_filtered$relative_time == t, ]

    # Run NPIV regression
    npiv_result <- npiv_regression(
      data = data_t,
      treatment_col = treatment_col,
      outcome_col = outcome_col,
      id_col = id_col,
      alpha = alpha,
      nx = nx,
      nL = nL,
      r = r,
      M = M
    )

    # Calculate ATT metrics
    att_estimate <- npiv_result[["binarised"]][["estimate"]][["binary"]]
    att_se <- npiv_result[["binarised"]][["std_error"]][["binary"]]
    att_ci_lower <- npiv_result[["binarised"]][["lower_ci"]]
    att_ci_upper <- npiv_result[["binarised"]][["upper_ci"]]

    # Calculate ACR metrics
    acr_estimate <- npiv_result[["ACR_estimate"]]
    acr_se <- npiv_result[["se_ACR"]]
    acr_ci_lower <- npiv_result[["ci_lower_ACR"]]
    acr_ci_upper <- npiv_result[["ci_upper_ACR"]]


    # Store results
    results[[as.character(t)]] <- list(
      relative_time = t,
      ATT_estimate = att_estimate,
      ATT_se = att_se,
      ATT_ci_lower = att_ci_lower,
      ATT_ci_upper = att_ci_upper,
      ACR_estimate = acr_estimate,
      ACR_se = acr_se,
      ACR_ci_lower = acr_ci_lower,
      ACR_ci_upper = acr_ci_upper
    )
  }

  # Convert results to a data frame
  results_df <- do.call(rbind, lapply(results, data.frame))

  return(list(
    results = results_df,
    data = data_filtered
  ))
}
