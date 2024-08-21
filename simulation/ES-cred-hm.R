rm(list=ls())

# Load required libraries
library(dplyr)
library(ggplot2)
library(fixest)
library(car)

# Set seed for reproducibility
set.seed(42)

# Load the data (assuming it's in your working directory)
data <- readRDS("~/Downloads/combined_data.rds")

# Define treatment year and randomly assign treatment
treatment_year <- 2010
data <- data %>%
  group_by(la_name) %>%
  mutate(treated = sample(c(0, 1), 1, prob = c(0.5, 0.5))) %>%
  ungroup()

# Create event time variable
data <- data %>%
  mutate(event_time = year - treatment_year)

# Function to simulate cumulative effect
simulate_cumulative_effect <- function(data, max_effect = 0.3, years_to_max = 5) {
  data %>%
    mutate(
      dose = ifelse(treated == 1, runif(n(), 0, 1), 0),
      cumulative_effect = case_when(
        event_time < 0 ~ 0,
        event_time >= years_to_max ~ max_effect * dose,
        TRUE ~ (max_effect * dose * event_time) / years_to_max
      ),
      deaths_sim = rbinom(n(), total_deaths, 1 - cumulative_effect)
    )
}

# Apply the cumulative effect
data <- simulate_cumulative_effect(data)

# Calculate ATT_es(e) as per the specified formula
att_es <- data %>%
  group_by(treated, year) %>%
  summarize(mean_deaths = mean(deaths_sim)) %>%
  ungroup() %>%
  pivot_wider(names_from = treated, values_from = mean_deaths, names_prefix = "treated_") %>%
  mutate(
    diff = treated_1 - treated_0,
    diff_from_base = diff - diff[year == treatment_year]
  ) %>%
  filter(year >= treatment_year) %>%
  select(year, diff_from_base) %>%
  rename(true_att_es = diff_from_base)

# Event Study Regression
event_study_model <- feols(deaths_sim ~ i(event_time, treated, ref = 0) |
                             la_name + year,
                           data = data)
summary(event_study_model)

# Extract coefficients and create a dataframe
coef_df <- data.frame(
  event_time = as.numeric(sub(".*?(-?\\d+).*", "\\1", names(coef(event_study_model)))),
  estimate = coef(event_study_model),
  se = sqrt(diag(vcov(event_study_model)))
) %>%
  filter(!is.na(event_time))

# Add confidence intervals
coef_df <- coef_df %>%
  mutate(
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se
  )

# Create a mapping of years to event_time
att_es <- att_es %>%
  mutate(event_time = year - treatment_year)

# Perform the left join using the mapped event_time
coef_df <- coef_df %>%
  left_join(att_es, by = "event_time") %>%
  mutate(true_att_es = ifelse(is.na(true_att_es), 0, true_att_es))

# Plot results
ggplot(coef_df, aes(x = event_time)) +
  geom_point(aes(y = estimate), color = "blue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "blue") +
  geom_point(aes(y = true_att_es), color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Event Time", y = "Effect", title = "Event Study Estimates vs True ATT_es")

# Print comparison of estimated vs true ATT_es
print("Estimated vs True ATT_es:")
print(coef_df)

# Calculate average treatment effect
att_calculation <- data %>%
  filter(event_time >= 0) %>%
  group_by(treated) %>%
  summarize(mean_deaths = mean(deaths_sim)) %>%
  spread(treated, mean_deaths) %>%
  mutate(att = `1` - `0`)

print("Calculated ATT:")
print(att_calculation$att)



