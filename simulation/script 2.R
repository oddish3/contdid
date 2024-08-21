rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(123)

# Parameters
n_counties <- 1000
n_periods <- 2
base_death_rate <- 0.01
treatment_prob <- 0.8
func <- function(x) x  # Linear function

# Generate panel data
sim_data <- expand_grid(
  county_id = 1:n_counties,
  period = 0:1
) %>%
  as_tibble() %>%
  arrange(county_id, period)

# Assign treatment status and dose (fixed across periods for each county)
sim_data <- sim_data %>%
  group_by(county_id) %>%
  mutate(
    treated = rbinom(1, size = 1, prob = treatment_prob),
    dose = if_else(treated == 1, runif(1), 0),
    population = rpois(1, lambda = 10000)
  ) %>%
  ungroup()

# Calculate the expected ATT
expected_att <- treatment_prob * 0.5  # E[func(dose)] for treated units

# Generate potential outcomes
sim_data <- sim_data %>%
  group_by(county_id) %>%
  mutate(
    epsilon = rnorm(n(), mean = 0, sd = 0.001),  # Error term

    # Potential outcomes
    Y_0 = base_death_rate + epsilon,
    Y_1 = Y_0 - func(dose) * (period == 1),

    # Observed outcome
    Y_obs = if_else(treated == 1 & period == 1, Y_1, Y_0),

    # True treatment effect (only for treated units in post-treatment period)
    true_effect = if_else(treated == 1 & period == 1, Y_0 - Y_1, 0)
  ) %>%
  ungroup()

# Calculate observed deaths
sim_data <- sim_data %>%
  mutate(
    observed_deaths = rbinom(n(), size = population, prob = Y_obs),
    observed_death_rate = observed_deaths / population
  )

# Calculate ATT
att_simulated <- sim_data %>%
  filter(period == 1) %>%
  summarize(att = mean(true_effect[treated == 1])) %>%
  pull(att)

# Display results
cat("Simulated ATT:", att_simulated, "\n")
cat("Expected ATT:", expected_att, "\n")
cat("Difference:", att_simulated - expected_att, "\n")

# Visualize treatment effect vs dose
ggplot(sim_data %>% filter(period == 1 & treated == 1),
       aes(x = dose, y = true_effect)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Treatment Effect vs Dose",
       x = "Treatment Dose",
       y = "Treatment Effect") +
  theme_minimal()

