# Test 2: Error handling
test_that("npiv_regression handles errors correctly", {
  # Test with missing column
  data <- data.frame(treatment = rnorm(100), outcome = rnorm(100))
  expect_error(npiv_regression(data, "treatment", "non_existent_column"))

  # Test with all zero treatments
  data$treatment <- 0
  expect_error(npiv_regression(data, "treatment", "outcome"))
})
