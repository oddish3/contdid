# Clear environment and set seed
rm(list=ls())
set.seed(123)

# Load required libraries
library(dplyr)

# Load the data (assuming you've already done this)
data <- readRDS("~/Downloads/combined_data.rds")

# Step 1: Assign pre/post periods
data <- data %>%
  mutate(
    period = if_else(year <= 2010, "pre", "post"),
    death_prob = total_deaths / population
  )

# Step 2: Filter for all available years and exclude specific la_name if needed
data <- data %>%
  filter(year >= 2001 & year <= 2021)

# Ensure each la_name has data for all years
data <- data %>%
  group_by(la_name) %>%
  filter(n_distinct(year) == 21) %>%
  ungroup()

# Assign treatment status and dose
data <- data %>%
  group_by(la_name) %>%
  mutate(
    treated = if_else(runif(1) > 0.5, 1, 0),
    dose = if_else(treated == 1 & year >= 2011, runif(1), 0)
  ) %>%
  ungroup()

# Calculate baseline rate (using pre-2011 data)
data <- data %>%
  group_by(la_name) %>%
  mutate(baseline_rate = mean(deaths_per_100k[year < 2011])) %>%
  ungroup()

# Define treatment effect function
avg_death_rate <- mean(data$deaths_per_100k)
treatment_effect <- function(d, avg_rate) {
  return(0.5 * d * avg_rate / 100000)  # Scale to match deaths per person
}

# Apply treatment effect and simulate new deaths
data <- data %>%
  mutate(
    te = if_else(treated == 1 & year >= 2011, treatment_effect(dose, avg_death_rate), 0),
    deaths_remove = if_else(year >= 2011 & treated == 1,
                            rbinom(n(), size = total_deaths, prob = te),
                            0),
    deaths_sim = total_deaths - deaths_remove,
    deaths_per_100k_sim = (deaths_sim / population) * 100000
  )

# Calculate E[ΔY|D = 0]
e_delta_y_d0 <- data %>%
  filter(treated == 0) %>%
  group_by(la_name) %>%
  summarize(
    pre_mean = mean(deaths_per_100k[year < 2011]),
    post_mean = mean(deaths_per_100k[year >= 2011]),
    change = post_mean - pre_mean
  ) %>%
  summarize(mean_change = mean(change)) %>%
  pull(mean_change)

print(paste("E[ΔY|D = 0] =", e_delta_y_d0))

# Calculate E[ΔY|D > 0]
e_delta_y_d_positive <- data %>%
  filter(treated == 1) %>%
  group_by(la_name) %>%
  summarize(
    pre_mean = mean(deaths_per_100k[year < 2011]),
    post_mean = mean(deaths_per_100k_sim[year >= 2011]),
    change = post_mean - pre_mean
  ) %>%
  summarize(mean_change = mean(change)) %>%
  pull(mean_change)

print(paste("E[ΔY|D > 0] =", e_delta_y_d_positive))

# Calculate the difference (ATT)
att_difference <- e_delta_y_d_positive - e_delta_y_d0
print(paste("ATT (E[ΔY|D = 0] - E[ΔY|D > 0]) =", att_difference))





