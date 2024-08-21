rm(list=ls())
library(generics)
library(tidyverse)
library(tidyr)
library(dplyr)
set.seed(123)
# Load the data (assuming you've already done this)
data <- readRDS("~/Downloads/combined_data.rds")
unique(data$year)

# Step 1: Assign pre/post periods
data <- data %>%
  mutate(period = if_else(year <= 2010, "pre", "post"),
         death_prob = total_deaths / population )
# Step 2: Filter for the years we're interested in and exclude specific la_name
data <- data %>%
  filter(year %in% c(2010:2011))

data <- data %>%
  group_by(la_name) %>%
  filter(n_distinct(period) == 2) %>%
  ungroup()

data <- data %>%
  group_by(la_name) %>%
  mutate(
    treated = if_else(runif(1) > 0.5, 1, 0),
    dose = if_else(treated == 1, runif(1), 0)
  ) %>%
  ungroup()

data <- data %>%
  group_by(la_name) %>%
  mutate(baseline_rate = mean(deaths_per_100k[period == "pre"])) %>%
  ungroup()

treatment_effect <- function(d) d  # f(d) = d

data <- data %>%
  mutate(
    te = treatment_effect(dose),
    deaths_remove = if_else(period == "post" & treated == 1,
                            rbinom(n(), size = round(total_deaths), prob = te),
                            0),
    deaths_sim = total_deaths - deaths_remove,
    deaths_per_100k_sim = (deaths_sim / population) * 100000
  )

avg_death_rate <- mean(data$deaths_per_100k)

treatment_effect <- function(d, avg_rate) {
  return(2.5 * d * avg_rate / 100000)  # Scale to match deaths per person
}

data <- data %>%
  mutate(
    te = if_else(treated == 1, treatment_effect(dose, avg_death_rate), 0),
    deaths_remove = if_else(period == "post" & treated == 1,
                            rbinom(n(), size = total_deaths, prob = te),
                            0),
    deaths_sim = total_deaths - deaths_remove,
    deaths_per_100k_sim = (deaths_sim / population) * 100000
  )

# Calculate E[ΔY|D = 0]
e_delta_y_d0 <- data %>%
  filter(treated == 0) %>%
  summarize(mean_change = mean(deaths_per_100k[period == "post"] - baseline_rate)) %>%
  pull(mean_change)

print(paste("E[ΔY|D = 0] =", e_delta_y_d0))

# Calculate E[ΔY|D > 0]
e_delta_y_d_positive <- data %>%
  filter(treated == 1) %>%
  summarize(mean_change = mean(deaths_per_100k_sim[period == "post"] - baseline_rate)) %>%
  pull(mean_change)

print(paste("E[ΔY|D > 0] =", e_delta_y_d_positive))

# Calculate the difference
att_difference <- e_delta_y_d_positive - e_delta_y_d0
print(paste("ATT (E[ΔY|D = 0] - E[ΔY|D > 0]) =", att_difference))

# Calculate change in outcome between periods
data <- data %>%
  group_by(la_name) %>%
  mutate(
    deaths_per_100k_change = deaths_per_100k_sim - first(deaths_per_100k_sim),
    dose_indicator = if_else(dose > 0, 1, 0)
  ) %>%
  ungroup()

# Filter for the second period (post-treatment)
data_post <- data %>%
  filter(period == "post")

# Run regression
regression_model <- lm(deaths_per_100k_change ~ dose_indicator, data = data_post)

# Print regression summary
summary(regression_model)

