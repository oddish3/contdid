# Test 3: Output consistency
test_that("npiv_regression output is consistent", {
  set.seed(123)
  data <- data.frame(
    treatment = rnorm(100),
    outcome = rnorm(100),
    id = 1:100
  )

  result1 <- npiv_regression(data, "treatment", "outcome", "id")
  result2 <- npiv_regression(data, "treatment", "outcome", "id")

  expect_equal(result1$ACR_estimate, result2$ACR_estimate)
  expect_equal(result1$se_ACR, result2$se_ACR)
})
