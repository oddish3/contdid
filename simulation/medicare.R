rm(list=ls())
load("/home/oddish3/Downloads/medicare1(1).RData")
library(tidyverse)
library(contdid)
debugonce(npiv_regression)
result <- npiv_regression(
  data = medicare1,
  treatment_col = "medicare_share_1983",
  outcome_col = "d_capital_labor_ratio"
)
# result <- data.frame(result)
plot(result[["x"]], result[["y"]], pch = 20, col = rgb(0.75, 0.75, 0.75, 0.5),
     xlab = 'Medicare Share 1983', ylab = 'Capital-Labor Ratio',
     main = 'Nonparametric Regression: Capital-Labor Ratio vs Medicare Share')
lines(result[["Xx"]], result[["hhat"]], col = 'black', lwd = 2)
lines(result[["Xx"]], result[["hhat"]] + (result[["hzast"]] + result[["thet"]] * log(log(result[["TJ"]][[result[["Llep"]] + 1]]))) * result[["sigh"]],
      col = 'black', lty = 2, lwd = 2)
lines(result[["Xx"]], result[["hhat"]] - (result[["hzast"]] + result[["thet"]] * log(log(result[["TJ"]][[result[["Llep"]] + 1]]))) * result[["sigh"]],
      col = 'black', lty = 3, lwd = 2)
# Add legend
legend('topright', legend = c('Data', 'Estimated Derivative', 'Confidence Bands'),
       pch = c(20, NA, NA), lty = c(NA, 1, 2), col = c('grey', 'black', 'black'))
plot(result[["Xx"]], result[["dhat"]], pch = 20, col = rgb(0.75, 0.75, 0.75, 0.5),
     xlab = 'Medicare Share 1983', ylab = 'Capital-Labor Ratio',
     main = 'Nonparametric Regression: Capital-Labor Ratio vs Medicare Share')
lines(result[["Xx"]], result[["dhat"]][result[["Xx"]] %in% result[["Xx"]]], col = 'black', lwd = 2)
lines(result[["Xx"]], result[["dhat"]] + (result[["dzast"]] + result[["thet"]] * log(log(result[["TJ"]][[result[["Llep"]] + 1]]))) * result[["sigd"]],
      col = 'black', lty = 2, lwd = 2)
lines(result[["Xx"]], result[["dhat"]] - (result[["dzast"]] + result[["thet"]] * log(log(result[["TJ"]][[result[["Llep"]] + 1]]))) * result[["sigd"]],
      col = 'black', lty = 2, lwd = 2)
# Add legend
legend('topright', legend = c('Data', 'Estimated Derivative', 'Confidence Bands'),
       pch = c(20, NA, NA), lty = c(NA, 1, 2), col = c('grey', 'black', 'black'))
dev.off()
