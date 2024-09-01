rm(list=ls())
# Load necessary libraries
library(dplyr)
library(contdid)

# Define event times (excluding 0)
event_times <- setdiff(-3:3, 0)

# Function to load and prepare data for a specific event time
prepare_data <- function(event_time, data_dir) {
  file_name <- paste0(data_dir, "data_time_", event_time, ".RData")
  load(file_name)

  # Add necessary columns for event_study_npiv function
  data_time$time <- 1983 + event_time
  data_time$event_time <- 1983

  return(data_time)
}

# Combine data for all event times
data_dir <- "~/Documents/uni/master-dissertation/code-cont/"
all_data <- do.call(rbind, lapply(event_times, function(t) prepare_data(t, data_dir)))

# Run the event study
results <- event_study_npiv(
  data = all_data,
  treatment_col = "medicare_share_1983",
  outcome_col = "d_capital_labor_ratio",
  time_col = "time",
  event_time_col = "event_time",
  id_col = "hospital_id",
  base_year = 1983,
  event_window = c(-3, 3),
  alpha = c(0.10, 0.05, 0.01),
  nx = 1000,
  nL = 9,
  r = 4,
  M = 5
)

# Print results
print(results$results)

