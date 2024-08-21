rm(list=ls())

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

data <-  read_delim("~/Downloads/DrugsAlcohol.txt")  %>%
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

pct_effect <- c(0.05)
treated <- c(6)
iterations <- 1

simulation_para <- bind_rows(replicate(iterations, expand_grid(treated, pct_effect), simplify = FALSE))
names(simulation_para) <- c("notreated", "pct_effect")
simulation_para$iteration <- 1:dim(simulation_para)[1]

#### draw three treatment groups
no_treated <- simulation_para$notreated
effect <- simulation_para$pct_effect
states <- sample(unique(data$StateCode), 50, replace = FALSE)
g1 <- states[1: (no_treated/3)]
g2 <- states[((no_treated/3) + 1) : (2*(no_treated/3))]
g3 <- states[(2*(no_treated/3) + 1) : (3*(no_treated/3))]

dat <- data %>%
  mutate(first_treated = case_when(StateCode %in% g1 ~ 2006,
                                   StateCode %in% g2 ~ 2012,
                                   StateCode %in% g3 ~ 2016,
                                   TRUE ~ 0),
         first_treated_sa = if_else(first_treated == 0, 1000, first_treated),
         treated = if_else(first_treated == 0, 0, 1),
         post_period = if_else(treated == 1 & Year >= first_treated, 1, 0),
         time_since_treatment = if_else(first_treated == 0, -1000, Year - first_treated),
         group_specific_effect = case_when(first_treated == 2006 ~ (effect / 3),
                                           first_treated == 2012 ~ (effect),
                                           first_treated == 2016 ~ (effect * 3),
                                           TRUE ~ 0))
### county level data
dat_c <- dat %>%
  group_by(CountyCode) %>%
  mutate(ate = case_when(post_period == 0 ~ 0,
                         post_period == 1 ~ group_specific_effect / ((max(time_since_treatment)/ 2) + 1)), ### divide group specific ate by number of post treatment periods specific to each treatment group
         ate = if_else(post_period == 1, cumsum(ate), 0)) %>%
#
# dat_c %>% filter(CountyCode == "30013") %>% summarise(ate = sum(ate))
# dat_c %>% filter(CountyCode == "22015") %>% summarise(ate = sum(ate))
# dat_c %>% filter(CountyCode == "48001") %>% summarise(ate = sum(ate))
#
 ### event time ate increases over time
  rowwise() %>%
  mutate(deaths_remove = if_else(post_period == 0, 0, sum(rbinom(Deaths, 1, prob = ate))),
         deaths_sim = Deaths - deaths_remove,
         true_ate_prob = deaths_remove/Deaths,
         deathrate_sim = (deaths_sim/Population)*100000,
         y = deathrate_sim,
         true_death_rate = (Deaths/Population)*100000,
         true_att_calc = (deathrate_sim - true_death_rate)) %>%
  ungroup() %>%
  mutate(sd_deathrate = sd(true_death_rate),### sd
         pre_treat_deathrate = mean(true_death_rate[treated == 1 & Year <= first_treated])) %>% ### pre treatment mean roa for treated units
  mutate(time = as.integer(Year),
         id = as.character(CountyCode),
         y = as.numeric(y),
         time_to_treatment = time_since_treatment)

#### calculate the true treatment effects to save
### county level
avg_att_c <- dat_c %>%
  filter(post_period == 1) %>%
  pull(ate) %>%
  mean()

avg_att_deathrate_c <- dat_c %>%
  filter(post_period == 1) %>%
  pull(true_att_calc) %>%
  mean()
print(avg_att_c)
print(avg_att_deathrate_c)



