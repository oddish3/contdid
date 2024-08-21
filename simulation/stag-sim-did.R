# Clear environment
rm(list=ls())

# Load necessary libraries
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Load and clean data
data <-  read_delim("~/Downloads/DrugsAlcohol.txt") %>%
  filter(County != "District of Columbia, DC") %>%
  mutate(Population = as.double(Population)) %>%
  group_by(County) %>%
  mutate(Missing = if_else(any(Deaths == "Missing"), 1, 0),
         MissingPop = if_else(any(Population == 0), 1, 0),
         allSupp = if_else(all(Deaths == "Suppressed"), 1, 0),
         meanSupp = mean(Deaths == "Suppressed")) %>%
  ungroup() %>%
  mutate(Suppressed = if_else(Deaths == "Suppressed", 1, 0),
         Deaths = if_else(Deaths == "Suppressed" | Deaths == "Missing", NA, Deaths)) %>%
  filter(Missing == 0 & MissingPop == 0) %>%
  select(-c(Notes, `Year Code`, `Crude Rate`)) %>%
  rename(CountyCode = `County Code`) %>%
  mutate(Year = as.double(Year),
         Deaths = as.double(Deaths),
         Population = as.double(Population),
         death_rate = (Deaths/(Population/100000)),
         max_death = max(death_rate, na.rm = TRUE)*(Population/100000),
         Deaths = if_else(Suppressed == 1, sample(c(1:9), 1, replace = FALSE), Deaths),
         Deaths = if_else(Suppressed == 1 & Deaths > max_death, round(max_death, 0), Deaths),
         StateCode = str_sub(CountyCode, 1, 2),
         death_rate = Deaths/(Population/100000)) %>%
  group_by(CountyCode) %>%
  mutate(Zeros = if_else(any(Deaths == 0), 1, 0),
         allZeros = if_else(all(Deaths ==0), 1, 0),
         meanZero = mean(Deaths == 0)) %>%
  ungroup() %>%
  filter(meanZero < 0.10 & allSupp ==0 & meanSupp < 0.50)

# Parameters
pct_effect <- 0.05
treated_units <- 6
iterations <- 1
set.seed(1234)

# Select treated and control groups
states <- sample(unique(data$StateCode), 50, replace = FALSE)
treated_group <- states[1:treated_units]
control_group <- states[(treated_units + 1):(treated_units * 2)]

# Add treatment indicator
dat <- data %>% filter(Year %in% c(2005:2006)) %>%
  mutate(treated = if_else(StateCode %in% treated_group, 1, 0),
         post_period = if_else(Year >= 2006, 1, 0)) # Assuming 2006 is the treatment year

# Calculate the treatment effect (DiD)
dat_c <- dat %>%
  group_by(CountyCode) %>%
  mutate(true_treatment_effect = if_else(treated == 1 & post_period == 1, pct_effect, 0),
         ate = true_treatment_effect,
         deaths_remove = if_else(post_period == 0, 0, sum(rbinom(Deaths, 1, prob = ate))),
         deaths_sim = Deaths - deaths_remove,
         true_ate_prob = deaths_remove / Deaths,
         deathrate_sim = (deaths_sim / Population) * 100000,
         y = deathrate_sim,
         true_death_rate = (Deaths / Population) * 100000,
         true_att_calc = (deathrate_sim - true_death_rate)) %>%
  ungroup() %>%
  mutate(sd_deathrate = sd(true_death_rate),
         pre_treat_deathrate = mean(true_death_rate[treated == 1 & post_period == 0]))

# Calculate the true treatment effects
avg_att_c <- dat_c %>%
  filter(post_period == 1) %>%
  pull(ate) %>%
  mean()

avg_att_deathrate_c <- dat_c %>%
  filter(post_period == 1) %>%
  pull(true_att_calc) %>%
  mean()

# Display the results
print(paste("Average ATT (ATE):", avg_att_c))
print(paste("Average ATT Death Rate Change:", avg_att_deathrate_c))
