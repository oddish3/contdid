rm(list=ls())
devtools::load_all()
library(contdid)
library(tidyverse)
source("~/Documents/uni/master-dissertation/contdid/simulation/DGP1.R")
set.seed(123)  # for reproducibility
results <- gdata(100, dgp = 4)
data <- results$data
info <- results$info
# data <- data %>%
#   filter((dose > 0.25 & dose < 0.75) | dose == 0) %>%
#   mutate(dose = (dose - min(dose)) / (max(dose) - min(dose)))

# debugonce(npiv_regression)
npiv_result <- npiv_regression(data, "dose", "dy")
npiv_result[["hzast"]]
npiv_result[["dzast"]]

mean(data$dy[data$treatment == 1]) - mean(data$dy[data$treatment == 0])
npiv_result[["binarised"]][["estimate"]][["binary"]]

# Assume npiv_result[["Xx"]] contains the x-values where hhat is computed
x_grid <- npiv_result[["Xx"]]

# Find the closest indices in info$func_values corresponding to npiv_result[["Xx"]]
info_x_values <- seq(0, 1, length.out = length(info$func_values))

# Find the closest indices in info_x_values corresponding to npiv_result[["Xx"]]
closest_indices <- sapply(npiv_result[["Xx"]], function(x) which.min(abs(info_x_values - x)))

# Subset the data based on these indices
data1 <- data.frame(
  x = npiv_result[["Xx"]],
  func_values = info[["func_values"]][closest_indices],
  ATT_upper_UCB = npiv_result[["ATT_upper_UCB"]],
  ATT_lower_UCB = npiv_result[["ATT_lower_UCB"]],
  hhat = npiv_result[["hhat"]]
)

ggplot(data1, aes(x = x)) +
  geom_line(aes(y = func_values, color = "func_values")) +
  geom_line(aes(y = ATT_upper_UCB, color = "ATT_upper_UCB")) +
  geom_line(aes(y = ATT_lower_UCB, color = "ATT_lower_UCB")) +
  geom_line(aes(y = hhat, color = "hhat"), linetype = "dashed") +
  labs(y = "Values", title = "Function Values with Upper and Lower Confidence Bounds over npiv_result[['Xx']]") +
  scale_color_manual("",
                     breaks = c("func_values", "ATT_upper_UCB", "ATT_lower_UCB", "hhat"),
                     values = c("blue", "red", "green", "purple")) +
  theme_minimal()

# Assuming the function derivative values are evaluated on a uniform grid between 0 and 1
info_x_values <- seq(0, 1, length.out = length(info[["func_deriv_values"]]))

# Find the closest indices in info_x_values corresponding to npiv_result[["Xx"]]
closest_indices <- sapply(npiv_result[["Xx"]], function(x) which.min(abs(info_x_values - x)))

# Subset the data based on these indices
data2 <- data.frame(
  x = npiv_result[["Xx"]],
  func_deriv_values = info[["func_deriv_values"]][closest_indices],
  ACR_upper_UCB = npiv_result[["ACR_upper_UCB"]],  # Assuming these match in length
  ACR_lower_LCB = npiv_result[["ACR_lower_UCB"]],
  dhat = npiv_result[["dhat"]]
)

# Plot the data
ggplot(data2, aes(x = x)) +
  geom_line(aes(y = func_deriv_values, color = "func_deriv_values")) +
  geom_line(aes(y = ACR_upper_UCB, color = "ACR_upper_UCB")) +
  geom_line(aes(y = ACR_lower_LCB, color = "ACR_lower_LCB")) +
  geom_line(aes(y = dhat, color = "dhat"), linetype = "dashed") +  # Add estimated function
  labs(y = "Derivative Values", title = "Derivative Values with Upper and Lower Confidence Bounds") +
  scale_color_manual("",
                     breaks = c("func_deriv_values", "ACR_upper_UCB", "ACR_lower_LCB", "dhat"),
                     values = c("blue", "red", "green", "purple")) +
  theme_minimal()


