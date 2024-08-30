#' Event Study Nonparametric Instrumental Variable (NPIV) Function
#'
#' This function performs an event study using a nonparametric instrumental variable regression.
#'
#' @param data A data frame containing the data.
#' @param treatment_col Name of the treatment column as a string.
#' @param outcome_col Name of the outcome column as a string.
#' @param time_col Name of the time column as a string.
#' @param event_time_col Name of the event time column as a string.
#' @param id_col (Optional) Name of the ID column as a string.
#' @param base_year Base year for the event study (default is 1983).
#' @param event_window A vector specifying the time window around the event (default is c(-3, 3)).
#' @param alpha Significance level for confidence intervals (default is 0.05).
#' @param nx Number of grid points for the nonparametric regression (default is 1000).
#' @param nL Number of terms in the series (default is 9).
#' @param r Resolution level (default is 4).
#' @param M Smoothing parameter (default is 5).
#' @return A list containing the event study results and the filtered data.
#' @export
#'
#' @examples
#' # Example usage
#' result <- event_study_npiv(data, "treatment", "outcome", "time", "event_time")
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
