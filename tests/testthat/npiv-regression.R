# File: tests/testthat/test-npiv_regression.R

# Test 1: Basic functionality
test_that("npiv_regression works with valid inputs", {
  # Create sample data
  set.seed(123)
  data <- read.csv("/home/oddish3/Documents/uni/master-dissertation/code-cont/data.csv")

  # Run function
  result <- npiv_regression(data, "dose", "dy")

  # Check if result is a list and contains expected components
  expect_type(result, "list")
  expect_true(all(c("x", "y", "Xx", "hhat", "dhat", "ACR_estimate") %in% names(result)))

  # Check that there are no NULL values in the result list
  expect_true(all(!sapply(result, is.null)),
              info = "The result list contains NULL values")
})
