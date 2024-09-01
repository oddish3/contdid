# Clear the workspace
rm(list = ls())

# Load necessary libraries
library(fixest)
library(dplyr)
library(broom)
library(lmtest)
library(tidyverse)
library(haven)
library(knitr)
library(sandwich)

# Load the dataset
data <- read_dta("application/113746-V1/longdiff/co/longdiff_col.dta")

# Convert bplregcol to a factor variable
data <- data %>% mutate(bplregcol = as.factor(bplregcol))

# Remove rows with missing bplregcol values
data <- data %>% filter(!is.na(bplregcol))

# Create dummy variables for bplregcol
dummies <- model.matrix(~ bplregcol, data)
dummies <- dummies[, -1]  # Remove the first column to avoid multicollinearity

# Calculate log of population density
data$log_lndens <- log(data$lndens)

# Combine dummy variables with the original data
data_with_dummies <- cbind(data, dummies)

# Ensure weights are included in the data (assuming wtbpl exists in the data)
weights <- data_with_dummies$wtbpl

# Run the regression with weights
model_A <- lm(dlit ~ poveda + log_lndens + nivel_de_vida + .,
              data = data_with_dummies[, c("dlit", "poveda", "log_lndens", "nivel_de_vida", colnames(dummies))],
              weights = weights)

# Calculate robust standard errors
robust_se_A <- coeftest(model_A, vcov = vcovHC(model_A, type = "HC1"))

# Display summary with robust standard errors
robust_se_A
