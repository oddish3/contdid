rm(list=ls())

library(dplyr)
library(ggplot2)
library(tidyverse)

set.seed(42)  # For reproducibility

# Load the data
data <- readRDS("~/Downloads/combined_data.rds")

# Filter for years 2009 and 2010
data_filtered <- data %>%
  filter(year %in% c(2009, 2010))

# Function to prepare data for DiD analysis
prepare_data <- function(data, treat_prob = 0.8) {
  n_units <- length(unique(data$la_name))

  # Generate treatment assignment
  treated <- rbinom(n_units, 1, treat_prob) == 1

  # Generate doses for treated units
  doses <- ifelse(treated, runif(n_units), 0)

  # Add treatment and dose information to the data
  data %>%
    group_by(la_name) %>%
    mutate(
      treated = rep(treated[cur_group_id()], n()),
      dose = rep(doses[cur_group_id()], n()),
      period = ifelse(year == 2009, 1, 2),

      # Treatment effect only applies in period 2 for treated units
      effect = ifelse(period == 2 & treated, dose^2, 0),

      # Simulated deaths after accounting for treatment
      deaths_sim = ifelse(period == 2 & treated,
                          rbinom(n(), total_deaths, 1 - effect),
                          total_deaths),

      # Calculate death rates per 100,000 population
      deathrate_sim = (deaths_sim / population) * 100000,

      # Calculate the change in death rate due to treatment
      true_att_calc = ifelse(period == 2, deathrate_sim - deaths_per_100k, 0)
    ) %>%
    ungroup()
}

# Prepare the data
prepared_data <- prepare_data(data_filtered)
# print(prepared_data %>% filter(la_name == "Swindon"))

# Calculate the true average treatment effect
true_ate <- mean(prepared_data$effect[prepared_data$treated & prepared_data$period == 2]) -
  mean(prepared_data$effect[!prepared_data$treated & prepared_data$period == 2])

true_att <- mean(prepared_data$deaths_sim[prepared_data$treated & prepared_data$period == 2]) -
  mean(prepared_data$deaths_sim[!prepared_data$treated & prepared_data$period == 2])

# Estimate treatment effect using DiD regression
dat <- prepared_data %>%
  group_by(la_name) %>% # Replace 'grouping_variable' with the variable that distinguishes different groups if needed
  mutate(deaths_period1 = ifelse(period == 1, total_deaths, NA)) %>%  # Capture deaths in period 1
  fill(deaths_period1, .direction = "downup") %>%  # Fill in the missing value for period 2
  mutate(dy = ifelse(period == 2, deaths_sim - deaths_period1, NA)) %>%  # Subtract deaths in period 1 from deaths_sim in period 2
  filter(period == 2) %>%  # Keep only rows where period == 2
  ungroup()  # Ungroup the data if you used group_by()
dat$binary <- ifelse(dat$dose > 0, 1, 0)
res <- lm(dy ~ binary, data = dat)
estimated_effect <- coef(res)[2]

# Print results
cat("True ATE (effect):", true_ate, "\n")
cat("True ATT (deaths):", true_att, "\n")
cat("Estimated effect (OLS):", estimated_effect, "\n")


# Visualize the distribution of doses
ggplot(prepared_data %>% filter(period == 2), aes(x = dose)) +
  geom_histogram(binwidth = 0.05) +
  labs(title = "Distribution of Treatment Doses", x = "Dose", y = "Count")

# Visualize the treatment effect
ggplot(prepared_data, aes(x = factor(period), y = deaths_sim, color = factor(treated))) +
  geom_boxplot() +
  labs(title = "Deaths by Period and Treatment Status",
       x = "Period", y = "Deaths",
       color = "Treated") +
  theme_minimal()
