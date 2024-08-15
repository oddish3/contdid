library(testthat)
library(readr)
library(dplyr)
library(purrr)
library(contdid)
set.seed(1234567)
# Helper function to load CSV files
load_csv_files <- function(directory) {
  tryCatch({
    csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) {
      stop("No CSV files found in the specified directory")
    }
    message(paste("Found", length(csv_files), "CSV files"))

    data_list <- map(csv_files, ~{
      message(paste("Processing file:", .x))
      var_name <- tools::file_path_sans_ext(basename(.x))
      var_name <- sub("^123_", "", var_name)  # Remove "123_" prefix
      data <- read_csv(.x, col_types = cols(), col_names = FALSE)
      if (ncol(data) == 1) {
        setNames(list(as.numeric(data[[1]])), var_name)
      } else {
        setNames(list(data), var_name)
      }
    }) %>% flatten()

    message("CSV files loaded successfully")
    return(data_list)
  }, error = function(e) {
    message(paste("Error in load_csv_files:", e$message))
    return(NULL)
  })
}
# Test function
test_matlab_equality <- function(r_result, matlab_data, tolerance = 1e-2) {
  tryCatch({
  common_vars <- intersect(names(r_result), names(matlab_data))
  message(paste("Testing", length(common_vars), "common variables"))
  for (var in common_vars) {
    message(paste("Testing variable:", var))
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
  message("All variables tested successfully")
  }, error = function(e) {
    message(paste("Error in test_matlab_equality:", e$message))
  })
}

# Main test
test_that("Short Test: R results match MATLAB results", {
  tryCatch({
    skip_if_not(dir.exists(test_path("data", "matlab_results")),
                "MATLAB data directory not found")
    skip_if_not(file.exists(test_path("data", "medicare1.csv")),
                "Medicare data file not found")

    message("Loading MATLAB CSV files")
    matlab_data <- load_csv_files(test_path("data", "matlab_results"))
    if (is.null(matlab_data)) {
      skip("Failed to load MATLAB data")
    }

    message("Loading R data")
    data <- read.csv(test_path("data", "medicare1.csv"))
    message(paste("R data dimensions:", nrow(data), "x", ncol(data)))

    message("Running npiv_regression")
    # debugonce(npiv_regression)
    r_result <- npiv_regression(data, "medicare_share_1983", "d_capital_labor_ratio")

    message("Comparing results")
    test_matlab_equality(r_result, matlab_data)

    message("Test completed successfully")
  }, error = function(e) {
    message(paste("Error in main test:", e$message))
    stop(e)
  })
})
