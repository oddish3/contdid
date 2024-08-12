# Test 1: Basic functionality
test_that("npiv_regression works with valid inputs", {
  # Create sample data
  set.seed(123)
  data <- data.frame(
    treatment = rnorm(100),
    outcome = rnorm(100),
    id = 1:100
  )

  # Run function
  result <- npiv_regression(data, "treatment", "outcome", "id")

  # Check if result is a list and contains expected components
  expect_type(result, "list")
  expect_true(all(c("x", "y", "Xx", "hhat", "dhat", "ACR_estimate") %in% names(result)))
})
