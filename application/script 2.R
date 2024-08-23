rm(list=ls())

# Load necessary libraries
library(dplyr)
library(fixest)
library(broom)
library(haven)
library(tidyverse)
library(knitr)
library(modelsummary)
library(kableExtra)

data <- read_dta("application/dataverse_files/Data Files/county_gb_main.dta")

# Define county controls
county_controls <- c(
  "shr_farmland", "pop_b_1940", "pop_w_1940", "postal_b", "mig_bw_state_b",
  "mig_wi_state_b", "confed_symbol_N", "lynch_black", "naacp_chptrs1941",
  "dissimilarity_po", "isolation_po", "alpha_po", "none_b", "educ_lo_1940_b",
  "educ_hs_1940_b", "none_w", "educ_lo_1940_w", "educ_hs_1940_w", "hotel_own_w",
  "hotel_own_b", "eating_own_w", "eating_own_b", "lforce_w", "lforce_b",
  "incwage_w", "incwage_b", "own_w_1940", "own_b_1940", "man_estab_1940",
  "man_worker_1940", "man_wages_1940", "man_output_1940", "man_vadd_1940",
  "warsup_com_1940", "warsup_oth_1940", "warfac_ind_1940", "warfac_mil_1940",
  "war_total_1940"
)

# Define missing controls
county_controls_missing <- c(
  "shr_farmland_miss", "pop_b_1940_miss", "pop_w_1940_miss",
  "mig_bw_state_b_miss", "mig_wi_state_b_miss", "confed_symbol_N_miss",
  "lynch_black_miss", "dissimilarity_po_miss", "isolation_po_miss",
  "alpha_po_miss", "none_b_miss", "educ_lo_1940_b_miss", "educ_hs_1940_b_miss",
  "none_w_miss", "educ_lo_1940_w_miss", "educ_hs_1940_w_miss", "own_w_1940_miss",
  "own_b_1940_miss", "man_estab_1940_miss", "man_worker_1940_miss",
  "man_wages_1940_miss", "man_output_1940_miss", "man_vadd_1940_miss",
  "war_total_1940_miss", "warsup_com_1940_miss", "warsup_oth_1940_miss",
  "warfac_ind_1940_miss", "warfac_mil_1940_miss"
)

# Create asinh transformed variables
for (var in county_controls) {
  data[[paste0("asinh_", var)]] <- asinh(data[[var]])
}

# Create interaction terms
for (var in county_controls) {
  data[[paste0("postint_", var)]] <- data[[paste0("asinh_", var)]] * data$after
}

# Create missing indicator interactions
for (var in county_controls_missing) {
  data[[paste0("mis_", var)]] <- data[[var]] * data$after
}

# Create misscontrols indicator
data$misscontrols <- as.numeric(rowSums(data[county_controls_missing] == 1) > 0)

# Panel A: Full Sample
# remove data where a_gb_tot is na

# Column 1: Basic DiD without fixed effects
model1 <- feols(a_gb_tot ~ a_killed_w + a_killed_w_after + after,
                data = data, cluster = "county_code")
summary(model1)

# Column 2: DiD with state fixed effects
model2 <- feols(a_gb_tot ~ a_killed_w + a_killed_w_after + after | stateid,
                data = data, cluster = "county_code")

# Column 3: DiD with county controls, year FE, and state FE
controls <- paste(c(paste0("postint_", county_controls),
                    paste0("mis_", county_controls_missing),
                    paste0("asinh_", county_controls),
                    county_controls_missing),
                  collapse = " + ")
formula3 <- as.formula(paste("a_gb_tot ~ a_killed_w + a_killed_w_after +", controls, "| year + stateid"))

model3 <- feols(formula3, data = data, cluster = "county_code")

# Column 4: DiD with county FE and year FE
model4 <- feols(a_gb_tot ~ a_killed_w_after | county_code + year,
                data = data, cluster = "county_code")

# Column 5: DiD with county FE and state-by-year FE
model5 <- feols(a_gb_tot ~ a_killed_w_after | county_code + stateid^year,
                data = data, cluster = "county_code")

# Column 6: County-level linear time trends, county FE, year FE
model6 <- feols(a_gb_tot ~ a_killed_w_after + i(year) + year[county_code],
                data = data,
                cluster = "county_code")

# Update the models list
models <- list(model1, model2, model3, model4, model5, model6)

library(modelsummary)

# Prepare the list of models
model_list <- list(model1, model2, model3, model4, model5, model6)

# Generate the regression table
regression_table <- modelsummary(model_list,
                                 estimate = "{estimate}{stars}",
                                 statistic = "({std.error})",
                                 coef_map = c("a_killed_w_after" = "Coefficient of a_killed_w_after"),
                                 gof_omit = "AIC|BIC|Log.Lik",
                                 output = "latex"
  )

# Print LaTeX code
cat(regression_table)




# models <- list(
#   "Basic DiD" = model1,
#   "State FE" = model2,
#   "Controls + State & Year FE" = model3,
#   "County & Year FE" = model4,
#   "County & State-by-Year FE" = model5,
#   "County-Trends + County & Year FE" = model6
# )


# ES
# Run the regression
model <- feols(a_gb_tot ~ taumin3_a_killed_w + taumin2_a_killed_w +
                 taupos1_a_killed_w + taupos2_a_killed_w + taupos3_a_killed_w +
                 taupos4_a_killed_w + taupos5_a_killed_w + taupos6_a_killed_w +
                 taupos7_a_killed_w + taupos8_a_killed_w + taupos9_a_killed_w |
                 county_code + year,
               data = data,
               cluster = "county_code")

# Extract coefficients and confidence intervals
results <- tidy(model, conf.int = TRUE) %>%
  filter(grepl("tau", term))

# Add year information
results <- results %>%
  mutate(year = case_when(
    term == "taumin3_a_killed_w" ~ 1939,
    term == "taumin2_a_killed_w" ~ 1940,
    term == "taupos1_a_killed_w" ~ 1947,
    term == "taupos2_a_killed_w" ~ 1948,
    term == "taupos3_a_killed_w" ~ 1949,
    term == "taupos4_a_killed_w" ~ 1950,
    term == "taupos5_a_killed_w" ~ 1951,
    term == "taupos6_a_killed_w" ~ 1952,
    term == "taupos7_a_killed_w" ~ 1953,
    term == "taupos8_a_killed_w" ~ 1954,
    term == "taupos9_a_killed_w" ~ 1955
  ))

# Add the reference year (1941)
results <- rbind(results, data.frame(
  term = "reference",
  estimate = 0,
  std.error = 0,
  statistic = 0,
  p.value = 1,
  conf.low = 0,
  conf.high = 0,
  year = 1941
))

# Create the plot
p <- ggplot(results, aes(x = year, y = estimate)) +
  geom_point(color = "black", shape = 15, size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1941, linetype = "solid") +
  geom_rect(aes(xmin = 1942, xmax = 1946, ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Year", y = "Estimate") +
  theme(legend.position = "none")
print(p)

# ------------------------------------------------------------
# my did cont
data <- data %>% filter(!is.na(a_gb_tot) & !is.na(a_killed_w))
calc_period_avg <- function(df, var) {
  df %>%
    group_by(stateid, period = case_when(
      year <= 1941 ~ "pre",
      year >= 1947 ~ "post",
      TRUE ~ "interim"
    )) %>%
    summarise(avg = mean({{var}}, na.rm = TRUE)) %>%
    ungroup()
}

# Calculate averages for N_0 (number of Green Book establishments)
avg_N_0 <- calc_period_avg(data, a_gb_tot)

# Calculate average casualties for the pre-treatment period
avg_casualties_pre <- data %>%
  filter(year <= 1941) %>%
  group_by(stateid) %>%
  summarise(avg_casualties = mean(a_killed_w, na.rm = TRUE))

# Reshape the N_0 averages to wide format
avg_N_0_wide <- avg_N_0 %>%
  pivot_wider(names_from = period, values_from = avg, names_prefix = "N_0_")

# Merge the datasets
diff_data <- avg_N_0_wide %>%
  left_join(avg_casualties_pre, by = "stateid")

# Calculate the difference in N_0
diff_data <- diff_data %>%
  mutate(diff_N_0 = N_0_post - N_0_pre)

# Apply asinh transformation
diff_data <- diff_data %>%
  mutate(
    asinh_diff_N_0 = diff_N_0,  # Keep this transformation if desired
    raw_avg_casualties = avg_casualties  # Use raw casualty data
  )
summary(diff_data$raw_avg_casualties)

# Run the regression
model <- feols(asinh_diff_N_0 ~ raw_avg_casualties, data = diff_data)
summary(model)


diff_data <- diff_data %>%
  mutate(
    norm_casualties = (raw_avg_casualties - min(raw_avg_casualties)) /
      (max(raw_avg_casualties) - min(raw_avg_casualties))
  )

model_norm <- lm(asinh_diff_N_0 ~ norm_casualties, data = diff_data)
summary(model_norm)

library(contdid)
res <- npiv_regression(treatment_col = "norm_casualties",
                       outcome_col = "asinh_diff_N_0", data = diff_data)

# Define the cont_twfe_weights function
cont_twfe_weights <- function(l, D) {
  wt <- ( ( mean(D[D>=l]) - mean(D) ) * mean(1*(D>=l)) ) / var(D)
  wt
}

# Prepare the data
dose <- diff_data$norm_casualties
dy <- diff_data$asinh_diff_N_0  # Using literacy as the outcome

dL <- min(dose[dose>0])
dU <- max(dose)

# Create dose grid
dose_grid <- seq(dL, dU, length.out=100)

# Density plot of the dose
dose_density_plot <- ggplot(data.frame(dose=dose[dose>0]), aes(x=dose)) +
  geom_density(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid), max(dose_grid))) +
  ylab("Density") +
  xlab("Dose (Poveda)") +
  ylim(c(0,3)) +
  labs(title="Density of Malaria Ecology (Poveda)")

# print(dose_density_plot)

# Calculate TWFE weights
twfe_weights <- sapply(dose_grid, cont_twfe_weights, D=dose)

# Create dataframe for plotting
plot_df <- data.frame(dose_grid = dose_grid, twfe_weights = twfe_weights)

# TWFE weights plot
twfe_weights_plot <- ggplot(data=plot_df, aes(x = dose_grid, y = twfe_weights)) +
  geom_line(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid), max(dose_grid))) +
  ylab("TWFE weights") +
  xlab("Dose (Poveda)") +
  geom_vline(xintercept = mean(dose), colour="black", linewidth = 0.5, linetype = "dotted") +
  ylim(c(0,3)) +
  labs(title="TWFE weights for White Casualties")

# print(twfe_weights_plot)

library(gridExtra)

grid.arrange(dose_density_plot, twfe_weights_plot, ncol=2)


att_df <- data.frame(
  dose = res[["Xx"]],
  att = res[["hhat"]],
  upper = res[["ATT_upper_UCB"]],
  lower = res[["ATT_lower_UCB"]],
  se = res[["sigh"]]
)
# Calculate 95% CI
att_df$ci_lower <- att_df$att - 1.96 * att_df$se
att_df$ci_upper <- att_df$att + 1.96 * att_df$se
att_plot <- ggplot(att_df, aes(x = dose)) +
  # UCB (wider, lighter ribbon)
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.2) +
  # 95% CI (narrower, darker ribbon)
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "blue", alpha = 0.2) +
  # ATT line
  geom_line(aes(y = att), color = "blue", size = 1) +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Nonparametric Estimates of ATT(d|d)",
       subtitle = "With 95% CI (dark blue) and Uniform Confidence Bands (light blue)",
       x = "Malaria Ecology (Poveda)",
       y = "Average Treatment Effect on the Treated") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
    legend.position = "none"
  )

acr_df <- data.frame(
  dose = res[["Xx"]],
  acr = res[["dhat"]],
  upper = res[["ACR_upper_UCB"]],
  lower = res[["ACR_lower_UCB"]],
  se = res[["sigd"]]
)

acr_plot <- ggplot(acr_df, aes(x = dose)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgreen", alpha = 0.3) +
  geom_line(aes(y = acr), color = "darkgreen", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Derivative of ATT(d|d): Average Causal Response",
       x = "Malaria Ecology (Poveda)",
       y = "Average Causal Response") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(att_plot)
print(acr_plot)
###








