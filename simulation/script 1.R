library(dplyr)
library(tidyr)
library(ggplot2)
set.seed(123)

# Parameters
n_counties <- 10000
base_death_rate <- 0.01
treatment_prob <- 0.8
func <- function(x) x  # Linear function

# Generate data
sim_data <- tibble(
  county_id = rep(1:n_counties, each = 2),
  period = rep(c(1, 2), n_counties),
  population = rep(10000, n_counties * 2)  # Fixed population for simplicity
)

# Assign treatment status and doses
sim_data <- sim_data %>%
  group_by(county_id) %>%
  mutate(
    treated = rbinom(1, size = 1, prob = treatment_prob),
    dose = if_else(treated == 1, runif(1, 0, 1), 0)
  ) %>%
  ungroup()

# Generate potential outcomes and calculate treatment effects
sim_data <- sim_data %>%
  mutate(
    Y_0 = 0,  # Untreated outcome set to 0
    Y_d = Y_0 - func(dose) * (period == 2),  # Potential outcome with treatment
    treatment_effect = Y_0 - Y_d,

    # Probabilistically generate deaths
    baseline_deaths = rbinom(n(), size = population, prob = base_death_rate),
    prob_death_reduction = pmin(treatment_effect, 1),  # Ensure probability doesn't exceed 1
    deaths_removed = rbinom(n(), size = baseline_deaths, prob = prob_death_reduction),
    observed_deaths = baseline_deaths - deaths_removed,
    observed_death_rate = observed_deaths / population,

    # Observed outcome (based on probabilistic deaths)
    Y_obs = observed_death_rate - base_death_rate  # Adjust to make untreated outcome 0 on average
  )

# Calculate ATT(d|d') for each unique dose
att_d_given_d <- sim_data %>%
  filter(period == 2, treated == 1) %>%
  group_by(dose) %>%
  summarize(att = mean(Y_0 - Y_obs)) %>%
  ungroup()

# Calculate ATTo
att_o <- weighted.mean(att_d_given_d$att,
                       w = table(sim_data$dose[sim_data$treated == 1 & sim_data$period == 2]))

# Calculate ATE(d)
ate_d <- sim_data %>%
  filter(period == 2) %>%
  summarize(ate = mean(Y_0 - Y_obs)) %>%
  pull(ate)

# Display results
cat("ATTo:", att_o, "\n")
cat("ATE(d):", ate_d, "\n")
cat("Expected ATTo:", 0.5 * treatment_prob, "\n")

# Visualize treatment effect vs dose
ggplot(sim_data %>% filter(period == 2 & treated == 1),
       aes(x = dose, y = Y_0 - Y_obs)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Treatment Effect vs Dose",
       x = "Treatment Dose",
       y = "Observed Treatment Effect (Y_0 - Y_obs)") +
  theme_minimal()

# Visualize outcomes over time
ggplot(sim_data, aes(x = factor(period), y = Y_obs, group = county_id, color = factor(treated))) +
  geom_line(alpha = 0.1) +
  stat_summary(aes(group = treated), fun = mean, geom = "line", size = 1.5) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("Control", "Treated")) +
  labs(title = "Outcomes Over Time",
       x = "Period",
       y = "Observed Outcome",
       color = "Treatment Status") +
  theme_minimal()
