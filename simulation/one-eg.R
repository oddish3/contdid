# NPIV Regression Analysis
rm(list=ls())
# Load required libraries
library(ggplot2)
library(contdid)
library(stats)
library(splines2)
source("~/Documents/uni/master-dissertation/contdid/simulation/DGP1.R")
# Load data
set.seed(1234567)
n = 100
dgp = 1
dat <- gdata(n = n, dgp = dgp)
data <- dat[["data"]]
info <- dat[["info"]]
# write.csv(data, "/home/oddish3/Documents/M_folder/CCK-OG/CCK/last.csv")
# data <- read.csv('/home/oddish3/Documents/M_folder/CCK-OG/CCK/last.csv')

# Set parameters
alpha <- c(0.05)
nx <- 1000
nL <- 8
r <- 4
M <- 5

# Run NPIV regression
# debugonce(npiv_regression)
result <- npiv_regression(
  data = data,
  treatment_col = "dose",
  outcome_col = "dy",
  alpha = alpha,
  nx = nx,
  nL = nL,
  r = r,
  M = M
)

# Extract non-zero treatment and outcome
x <- data$dose[data$treatment == 1]
y <- data$dy[data$treatment == 1]

# Create data frames for plotting
df_scatter <- data.frame(x = x, y = y)
df_lines <- data.frame(
  x = result$Xx,
  hhat = result$hhat,
  hhat_upper = result$ATT_upper_UCB,
  hhat_lower = result$ATT_lower_UCB,
  dhat = result$dhat,
  dhat_upper = result$ACR_upper_UCB,
  dhat_lower = result$ACR_lower_UCB
)

# Figure 1: Regression Function
ggplot() +
  geom_point(data = df_scatter, aes(x = x, y = y), color = "gray75", alpha = 0.5) +
  geom_line(data = df_lines, aes(x = x, y = hhat), color = "black", size = 1) +
  geom_line(data = df_lines, aes(x = x, y = hhat_upper), linetype = "dashed", color = "black", size = 1) +
  geom_line(data = df_lines, aes(x = x, y = hhat_lower), linetype = "dashed", color = "black", size = 1) +
  labs(x = "Dose", y = "Outcome", title = "NPIV Regression Function") +
  theme_minimal() +
  theme(text = element_text(size = 14))

# print(p1)

# Figure 2: Derivative
p2 <- ggplot(df_lines) +
  geom_line(aes(x = x, y = dhat), color = "black", size = 1) +
  geom_line(aes(x = x, y = dhat_upper), linetype = "dashed", color = "black", size = 1) +
  geom_line(aes(x = x, y = dhat_lower), linetype = "dashed", color = "black", size = 1) +
  labs(x = "Dose", y = "Derivative", title = "NPIV Regression Derivative") +
  theme_minimal() +
  theme(text = element_text(size = 14))

print(p2)

# Print additional diagnostic information
cat("\nAdditional Diagnostic Information:\n")
cat("Mean UCB width (hhat):", mean(result$ATT_upper_UCB - result$ATT_lower_UCB), "\n")
cat("Mean UCB width (dhat):", mean(result$ACR_upper_UCB - result$ACR_lower_UCB), "\n")

# Extract the true function values
x_grid <- seq(0, 1, length.out = length(info$func_values))
closest_indices <- sapply(result$Xx, function(x) which.min(abs(x_grid - x)))

# Use these indices to select the corresponding true function values
true_func <- info$func_values[closest_indices]

# Compute A
A <- log(log(result$TJ[result$Ltil + 1]))

# Compute coverage
coverage_result <- ucb_cvge(
  h0 = true_func,
  hhat = result$hhat,
  sigh = result[["sigh"]],
  zast = result[["hzast"]][["95%"]],
  theta = result[["thet"]],
  A = A
)

# Print results
cat("\nUCB Coverage Results:\n")
cat("Coverage:", coverage_result$check[1, 1], "\n")
cat("Sup-norm loss:", coverage_result$loss, "\n")

# Compute pointwise coverage
pointwise_coverage <- mean(true_func >= result$ATT_lower_UCB & true_func <= result$ATT_upper_UCB)
cat("Pointwise coverage:", pointwise_coverage, "\n")

# Visualize the coverage
ggplot() +
  geom_ribbon(data = df_lines, aes(x = x, ymin = hhat_lower, ymax = hhat_upper), fill = "lightblue", alpha = 0.5) +
  geom_line(data = df_lines, aes(x = x, y = hhat), color = "blue", size = 1) +
  geom_line(aes(x = result$Xx, y = true_func), color = "red", size = 1) +
  labs(x = "Dose", y = "Outcome", title = "NPIV Regression Function with True Function") +
  theme_minimal() +
  theme(text = element_text(size = 14))



