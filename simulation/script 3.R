rm(list=ls())

library(dplyr)
library(ggplot2)
library(triangle)  # For rtriangle function

# DGP function
dgp_function <- function(dgp) {
  if (dgp == 1) {
    dist <- function(n) runif(n, 0, 1)  # Uniform distribution
    func <- function(x) x  # Linear function
    func_deriv <- function(x) rep(1, length(x))
    E_func <- 1/2
    E_deriv <- 1
  }
  if (dgp == 2) {
    dist <- function(n) rtriangle(n, a=0, b=1, c=0.3)  # Right-skewed triangular
    func <- function(x) x^2  # Quadratic function
    func_deriv <- function(x) 2*x
    E_func <- 13/60
    E_deriv <- 2/3
  }
  if (dgp == 3) {
    dist <- function(n) rbeta(n, 2, 5)  # Beta distribution
    func <- function(x) (3*x^2 - 2*x^3) / 3  # Cubic function
    func_deriv <- function(x) 2*x - 2*x^2
    E_func <- 2/21
    E_deriv <- 10/21
  }

  return(list(
    dist = dist,
    func = func,
    func_deriv = func_deriv,
    E_func = E_func,
    E_deriv = E_deriv
  ))
}

simulate_death_reduction <- function(data, dgp_num = 1, treatment_prob = 0.8) {
  set.seed(123)  # for reproducibility

  # Get DGP functions
  dgp <- dgp_function(dgp_num)

  # Select two years of data
  years <- sort(unique(data$year))
  selected_years <- tail(years, 2)
  data <- data %>% filter(year %in% selected_years)

  # Add treatment and dose columns
  data <- data %>%
    group_by(la_name) %>%
    mutate(
      treatment = rbinom(1, 1, treatment_prob),  # 80% of local authorities are treated
      treatment_dose = ifelse(treatment == 1, dgp$dist(1), 0),  # Use DGP distribution for treated units, 0 for untreated
      post_period = ifelse(year == selected_years[2], 1, 0)
    ) %>%
    ungroup()

  # Simulate death reduction
  data <- data %>%
    mutate(
      ate = dgp$func(treatment_dose),
      deaths_remove = if_else(post_period == 1 & treatment == 1,
                              rbinom(n(), size = total_deaths, prob = ate), 0),
      deaths_sim = total_deaths - deaths_remove,
      true_ate_prob = deaths_remove / total_deaths,
      death_rate_sim = (deaths_sim / population) * 100000,
      true_att_calc = deaths_per_100k - death_rate_sim  # Note the change in order
    )

  # Calculate change in death rate from period 1 to period 2, including natural variation
  data <- data %>%
    group_by(la_name) %>%
    mutate(
      delta_y = deaths_per_100k[post_period == 1] - deaths_per_100k[post_period == 0],
      delta_y_sim = death_rate_sim[post_period == 1] - death_rate_sim[post_period == 0]
    ) %>%
    ungroup()

  # Calculate ATT^o
  att_o <- mean(data$delta_y_sim[data$treatment == 1]) - mean(data$delta_y_sim[data$treatment == 0])

  # Calculate sample expectation of treatment effect
  sample_expectation <- mean(data$ate[data$treatment == 1])

  # Calculate true ATT^o
  true_att_o <- -dgp$E_func * mean(data$deaths_per_100k[data$post_period == 0 & data$treatment == 1])

  # Calculate average treatment effect on the treated
  avg_att <- data %>%
    filter(treatment == 1, post_period == 1) %>%
    summarise(
      avg_att = mean(ate),
      avg_att_deathrate = mean(true_att_calc)
    )

  list(data = data, att_o = att_o, sample_expectation = sample_expectation,
       true_att_o = true_att_o, avg_att = avg_att)
}

# Load the data
data <- readRDS("~/Downloads/combined_data.rds")

# Run the simulation for each DGP
results <- lapply(1:3, function(dgp_num) {
  result <- simulate_death_reduction(data, dgp_num)
  c(dgp = dgp_num,
    att_o = result$att_o,
    sample_expectation = result$sample_expectation,
    true_att_o = result$true_att_o,
    avg_att = result$avg_att$avg_att,
    avg_att_deathrate = result$avg_att$avg_att_deathrate)
})

# Combine results
results_df <- do.call(rbind, results) %>% as.data.frame()

# Print results
print(results_df)

# Plot the distribution of treatment doses and effects for DGP 1
result <- simulate_death_reduction(data, 3)
ggplot(result$data %>% filter(treatment == 1),
       aes(x = treatment_dose, y = delta_y_sim)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(x = "Treatment Dose", y = "Change in Simulated Death Rate",
       title = "Relationship between Treatment Dose and Change in Death Rate (DGP 1)") +
  theme_minimal()




