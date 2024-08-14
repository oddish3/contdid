library(testthat)
library(readr)
library(dplyr)
library(purrr)
library(YourPackageName)  # Replace with your actual package name

# Helper function to load CSV files
load_csv_files <- function(directory) {
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) {
    stop("No CSV files found in the specified directory")
  }
  data_list <- map(csv_files, ~{
    var_name <- tools::file_path_sans_ext(basename(.x))
    var_name <- sub("^123_", "", var_name)  # Remove "123_" prefix
    data <- read_csv(.x, col_types = cols(), col_names = FALSE)
    if (ncol(data) == 1) {
      setNames(list(as.numeric(data[[1]])), var_name)
    } else {
      setNames(list(data), var_name)
    }
  }) %>% flatten()
  return(data_list)
}

# Test function
test_matlab_equality <- function(r_result, matlab_data, tolerance = 1e-2) {
  # ... (rest of the function remains the same)
}

# Main test
test_that("Short Test: R results match MATLAB results", {
  # Load MATLAB CSV files
  matlab_data <- load_csv_files(test_path("data", "matlab_results"))

  # Load R data
  data <- read.csv(test_path("data", "medicare1.csv"))

  # Run your R function
  r_result <- npiv_regression(data, "medicare_share_1983", "d_capital_labor_ratio")

  # Compare results
  test_matlab_equality(r_result, matlab_data)
})
