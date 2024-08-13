# File: tests/testthat/test-medicare-npiv-regression.R

library(testthat)
library(tidyverse)
library(contdid)

# Load test data
test_data_path <- "/home/oddish3/Downloads/medicare1(1).RData"

test_that("NPIV regression and plotting work correctly for Medicare data", {
  # Skip test if data file doesn't exist
  skip_if_not(file.exists(test_data_path), "Test data file not found")

  # Load data
  load(test_data_path)

  # Check if data loaded correctly
  expect_true(exists("medicare1"), "Medicare data not loaded correctly")

  # Run NPIV regression
  result <- npiv_regression(
    data = medicare1,
    treatment_col = "medicare_share_1983",
    outcome_col = "d_capital_labor_ratio"
  )

  # Check if result is not NULL and has expected structure
  expect_false(is.null(result), "NPIV regression result is NULL")
  expect_true(is.list(result), "NPIV regression result is not a list")
  expect_true(all(c("x", "y", "Xx", "hhat", "dhat") %in% names(result)),
              "NPIV regression result missing expected components")

  # Check if result components have expected lengths
  # expect_equal(length(result$x), length(result$y),
  #              "x and y in result have different lengths")
  # expect_equal(length(result$Xx), length(result$hhat),
  #              "Xx and hhat in result have different lengths")

  # Test plotting functions (we can't easily test the visual output,
  # but we can check if they run without error)
  # expect_no_error({
  #   plot(result[["x"]], result[["y"]], pch = 20, col = rgb(0.75, 0.75, 0.75, 0.5),
  #        xlab = 'Medicare Share 1983', ylab = 'Capital-Labor Ratio',
  #        main = 'Nonparametric Regression: Capital-Labor Ratio vs Medicare Share')
  #   lines(result[["Xx"]], result[["hhat"]], col = 'black', lwd = 2)
  #   lines(result[["Xx"]], result[["hhat"]] + (result[["hzast"]] + result[["thet"]] * log(log(result[["TJ"]][[result[["Llep"]] + 1]]))) * result[["sigh"]],
  #         col = 'black', lty = 2, lwd = 2)
  #   lines(result[["Xx"]], result[["hhat"]] - (result[["hzast"]] + result[["thet"]] * log(log(result[["TJ"]][[result[["Llep"]] + 1]]))) * result[["sigh"]],
  #         col = 'black', lty = 2, lwd = 2)
  #   legend('topright', legend = c('Data', 'Estimated Derivative', 'Confidence Bands'),
  #          pch = c(20, NA, NA), lty = c(NA, 1, 2), col = c('grey', 'black', 'black'))
  # }, "Error in plotting first graph")
  #
  # expect_no_error({
  #   plot(result[["Xx"]], result[["dhat"]], pch = 20, col = rgb(0.75, 0.75, 0.75, 0.5),
  #        xlab = 'Medicare Share 1983', ylab = 'Capital-Labor Ratio',
  #        main = 'Nonparametric Regression: Capital-Labor Ratio vs Medicare Share')
  #   lines(result[["Xx"]], result[["dhat"]][result[["Xx"]] %in% result[["Xx"]]], col = 'black', lwd = 2)
  #   lines(result[["Xx"]], result[["dhat"]] + (result[["dzast"]] + result[["thet"]] * log(log(result[["TJ"]][[result[["Llep"]] + 1]]))) * result[["sigd"]],
  #         col = 'black', lty = 2, lwd = 2)
  #   lines(result[["Xx"]], result[["dhat"]] - (result[["dzast"]] + result[["thet"]] * log(log(result[["TJ"]][[result[["Llep"]] + 1]]))) * result[["sigd"]],
  #         col = 'black', lty = 2, lwd = 2)
  #   legend('topright', legend = c('Data', 'Estimated Derivative', 'Confidence Bands'),
  #          pch = c(20, NA, NA), lty = c(NA, 1, 2), col = c('grey', 'black', 'black'))
  # }, "Error in plotting second graph")
  #
  # # Clean up (optional, as testthat usually does this automatically)
  # dev.off()
})
