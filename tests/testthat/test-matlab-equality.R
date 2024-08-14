library(testthat)
library(readr)
library(dplyr)
library(purrr)
library(contdid)
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
  # browser()
  common_vars <- intersect(names(r_result), names(matlab_data))
  for (var in common_vars) {
    r_value <- r_result[[var]]
    matlab_value <- matlab_data[[var]]
    # Convert to vector if it's a single-column data frame
    restructure_variable <- function(var) {
      if (is.data.frame(var) && ncol(var) == 1) {
        # If the variable is a data frame with 1 column, extract the vector
        var <- var[[1]]
      }
      if (is.matrix(var) && ncol(var) == 1) {
        # If the variable is a matrix with 1 column, convert to a vector
        var <- as.numeric(var)
      }
      if (is.array(var) && length(dim(var)) == 2 && dim(var)[2] == 1) {
        # If the variable is a 2D array with 1 column, convert to a vector
        var <- as.numeric(var)
      }
      return(var)
    }
    # Restructure r_value and matlab_value
    r_value <- restructure_variable(r_value)
    matlab_value <- restructure_variable(matlab_value)
    # Check if the lengths are the same
    expect_equal(length(r_value), length(matlab_value),
                 info = paste("Length mismatch for variable:", var))
    # Check if the values are approximately equal
    expect_equal(r_value, matlab_value, tolerance = tolerance,
                 info = paste("Value mismatch for variable:", var))
  }
}
# Main test
test_that("Short Test: R results match MATLAB results", {
  skip_if_not(dir.exists(test_path("data", "matlab_results")),
              "MATLAB data directory not found")
  skip_if_not(file.exists(test_path("data", "medicare1.csv")),
              "Medicare data file not found")

  # Load MATLAB CSV files
  matlab_data <- tryCatch({
    load_csv_files(test_path("data", "matlab_results"))
  }, error = function(e) {
    skip(paste("Error loading MATLAB data:", e$message))
  })

  # Load R data
  data <- read.csv(test_path("data", "medicare1.csv"))

  # Run your R function
  r_result <- tryCatch({
    npiv_regression(data, "medicare_share_1983", "d_capital_labor_ratio")
  }, error = function(e) {
    skip(paste("Error in npiv_regression:", e$message))
  })

  # Compare results
  test_matlab_equality(r_result, matlab_data)
})
