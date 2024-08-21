rm(list=ls())

library(dplyr)
set.seed(123)
# Load the data (assuming you've already done this)
data <- readRDS("~/Downloads/combined_data.rds")

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
  return(0.5 * d * avg_rate / 100000)  # Scale to match deaths per person
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
att_difference <- e_delta_y_d0 - e_delta_y_d_positive
print(paste("ATT (E[ΔY|D = 0] - E[ΔY|D > 0]) =", att_difference))








calculate_att_d <- function(data, d, bandwidth = 0.1) {
  # browser()
  treated <- data %>%
    filter(period == "post" & abs(dose - d) < bandwidth & treated == 1)

  control <- data %>%
    filter(period == "post" & treated == 0)

  att_d <- mean(treated$baseline_rate - treated$deaths_per_100k_sim) -
    mean(control$baseline_rate - control$deaths_per_100k)

  return(att_d)
}

dose_points <- seq(0.1, 1, by = 0.1)
att_d_estimates <- sapply(dose_points, calculate_att_d, data = data)

atto_estimate <- mean(att_d_estimates)
print(paste("Estimated ATTo:", atto_estimate))

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
att_difference <- e_delta_y_d0 - e_delta_y_d_positive
print(paste("ATT (E[ΔY|D = 0] - E[ΔY|D > 0]) =", att_difference))
