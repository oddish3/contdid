rm(list=ls())

library(dplyr)
library(ggplot2)

set.seed(42)  # For reproducibility

# Function to simulate data
simulate_data <- function(n_units = 1000, n_periods = 2, treat_prob = 0.8) {
  # Generate treatment assignment
  treated <- rbinom(n_units, 1, treat_prob) == 1
  
  # Generate doses for treated units
  doses <- ifelse(treated, runif(n_units), 0)
  
  # Generate a random population size for each unit (between 500 and 1500)
  population <- sample(500:1500, n_units, replace = TRUE)
  
  # Generate panel data
  data <- expand.grid(unit = 1:n_units, period = 1:n_periods) %>%
    mutate(
      treated = rep(treated, each = n_periods),
      dose = rep(doses, each = n_periods),
      population = rep(population, each = n_periods),
      
      # Treatment effect only applies in period 2 for treated units
      effect = ifelse(period == 2 & treated, dose^2, 0),
      
      # Baseline deaths (Poisson distribution)
      deaths = rpois(n(), lambda = 100),  
      
      # Deaths removed due to treatment
      deaths_remove = ifelse(period == 2 & treated, rbinom(n(), deaths, effect), 0),
      
      # Simulated deaths after accounting for treatment
      deaths_sim = deaths - deaths_remove,
      
      # True ATE probability calculation
      true_ate_prob = ifelse(deaths > 0, deaths_remove / deaths, 0),
      
      # Calculate death rates per 100,000 population
      deathrate_sim = (deaths_sim / population) * 100000,
      true_death_rate = (deaths / population) * 100000,
      
      # Calculate the change in death rate due to treatment
      true_att_calc = ifelse(period == 2, deathrate_sim - true_death_rate, 0)
    )
  
  return(data)
}

# Example usage
data <- simulate_data()

# Calculate the true average treatment effect
mean(data$effect[data$treated & data$period == 2]) - mean(data$effect[!data$treated & data$period == 2])
mean(data$deaths_sim[data$treated & data$period == 2]) - mean(data$deaths_sim[!data$treated & data$period == 2])

print(data %>% filter(unit == 1))

dat <- data %>%
  group_by(unit) %>% # Replace 'grouping_variable' with the variable that distinguishes different groups if needed
  mutate(deaths_period1 = ifelse(period == 1, deaths, NA)) %>%  # Capture deaths in period 1
  fill(deaths_period1, .direction = "downup") %>%  # Fill in the missing value for period 2
  mutate(dy = ifelse(period == 2, deaths_sim - deaths_period1, NA)) %>%  # Subtract deaths in period 1 from deaths_sim in period 2
  filter(period == 2) %>%  # Keep only rows where period == 2
  ungroup()  # Ungroup the data if you used group_by()
dat$binary <- ifelse(dat$dose > 0, 1, 0)
lm(dy ~ binary, data = dat)


data %>% filter(period == 2) %>% summarise(true_ate = mean(true_ate_prob))



