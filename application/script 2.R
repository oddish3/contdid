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
library(gridExtra)
library(contdid)

data <- read_dta("application/dataverse_files/Data Files/county_gb_main.dta")
# sort(unique(data$year))

# exploratory data analysis
# dd <- data %>% filter(year >=1947& a_killed_w <1) %>% select(a_killed_w, year, county_code)
# plot(dd$a_killed_w)
# sort(unique(dd$a_killed_w_after))


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
# cat(regression_table)
calculate_change <- function(data) {
  base_year <- 1941
  base_values <- data %>%
    filter(year == base_year) %>%
    select(county_code, gb_tot) %>%
    rename(base_gb_tot = gb_tot)

  data %>%
    left_join(base_values, by = "county_code") %>%
    group_by(county_code, year) %>%
    reframe(change_gb_tot = gb_tot - base_gb_tot) %>%
    arrange(county_code, year)
}

# Calculate change data
change_data <- calculate_change(data)

# Prepare the final dataset
final_data <- data %>%
  left_join(change_data, by = c("county_code", "year"), relationship = "many-to-many") %>%
  mutate(
    killed_w_binary = ifelse(killed_w > 0, 1, 0),
    post_1948 = ifelse(year >= 1948, 1, 0),
    killed_w_binary_post = killed_w_binary * post_1948
  ) %>%
  select(county_code, year, killed_w_binary, post_1948, killed_w_binary_post, change_gb_tot, stateid)

# Remove rows with NA values
final_data <- final_data %>%
  filter(!is.na(killed_w_binary) & !is.na(change_gb_tot))

# Run the modified model 5
model5_modified <- feols(change_gb_tot ~ killed_w_binary_post | county_code + stateid^year,
                         data = final_data,
                         cluster = "county_code")

# Summary of the model
summary(model5_modified)


######


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
# model <- feols(a_gb_tot ~ taumin3_a_killed_w + taumin2_a_killed_w +
#                  taupos1_a_killed_w + taupos2_a_killed_w + taupos3_a_killed_w +
#                  taupos4_a_killed_w + taupos5_a_killed_w + taupos6_a_killed_w +
#                  taupos7_a_killed_w + taupos8_a_killed_w + taupos9_a_killed_w |
#                  county_code + year,
#                data = data,
#                cluster = "county_code")
#
# # Extract coefficients and confidence intervals
# results <- tidy(model, conf.int = TRUE) %>%
#   filter(grepl("tau", term))
#
# # Add year information
# results <- results %>%
#   mutate(year = case_when(
#     term == "taumin3_a_killed_w" ~ 1939,
#     term == "taumin2_a_killed_w" ~ 1940,
#     term == "taupos1_a_killed_w" ~ 1947,
#     term == "taupos2_a_killed_w" ~ 1948,
#     term == "taupos3_a_killed_w" ~ 1949,
#     term == "taupos4_a_killed_w" ~ 1950,
#     term == "taupos5_a_killed_w" ~ 1951,
#     term == "taupos6_a_killed_w" ~ 1952,
#     term == "taupos7_a_killed_w" ~ 1953,
#     term == "taupos8_a_killed_w" ~ 1954,
#     term == "taupos9_a_killed_w" ~ 1955
#   ))
#
# # Add the reference year (1941)
# results <- rbind(results, data.frame(
#   term = "reference",
#   estimate = 0,
#   std.error = 0,
#   statistic = 0,
#   p.value = 1,
#   conf.low = 0,
#   conf.high = 0,
#   year = 1941
# ))
#
# # Create the plot
# p <- ggplot(results, aes(x = year, y = estimate)) +
#   geom_point(color = "black", shape = 15, size = 3) +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_vline(xintercept = 1941, linetype = "solid") +
#   geom_rect(aes(xmin = 1942, xmax = 1946, ymin = -Inf, ymax = Inf),
#             fill = "grey90", alpha = 0.5) +
#   theme_minimal() +
#   labs(x = "Year", y = "Estimate") +
#   theme(legend.position = "none")
# print(p)

# data <- data %>% filter(!is.na(a_gb_tot))
# data$event_time <- 1941
# debugonce(event_study_npiv)
# result <- event_study_npiv(
#   data = data,
#   treatment_col = "a_killed_w",
#   outcome_col = "a_gb_tot",
#   time_col = "year",
#   event_time_col = "event_time",
#   base_year = 1941,
#   event_window = c(-2, 7),
#   alpha = 0.05,
#   nx = 1000,
#   nL = 9,
#   r = 4,
#   M = 5
# )

# ------------------------------------------------------------
# my did cont

# Step 1: Calculate average for variables with and without 'a_' prefix by period
average_totals <- data %>%
  mutate(period = case_when(
    year < 1942 ~ "before_1942",
    year > 1945 ~ "after_1945",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  group_by(county_code, period) %>%
  summarise(
    avg_a_gb_tot = mean(a_gb_tot, na.rm = TRUE),
    avg_gb_tot = mean(gb_tot, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Calculate the change in averages between pre-1942 and post-1945 for each county_code
change_in_a_gb_tot <- average_totals %>%
  select(county_code, period, avg_a_gb_tot) %>%
  spread(key = period, value = avg_a_gb_tot) %>%
  mutate(change_a_gb_tot = after_1945 - before_1942) %>%
  select(county_code, change_a_gb_tot)

# Spread avg_gb_tot next
change_in_gb_tot <- average_totals %>%
  select(county_code, period, avg_gb_tot) %>%
  spread(key = period, value = avg_gb_tot) %>%
  mutate(change_gb_tot = after_1945 - before_1942) %>%
  select(county_code, change_gb_tot)

# Merge the two change datasets together
change_in_totals <- change_in_a_gb_tot %>%
  inner_join(change_in_gb_tot, by = "county_code")

# Step 3: Extract one unique value for variables with and without 'a_' prefix per county_code
killed_unique <- data %>%
  group_by(county_code) %>%
  summarise(
    a_killed_w = first(a_killed_w),
    killed_w = first(killed_w)
  ) %>%
  ungroup()

# Step 4: Merge the changes in gb_tot with killed_w for both variables with and without 'a_' prefix
final_data <- change_in_totals %>%
  inner_join(killed_unique, by = "county_code")

# Step 5: Run the regression of the change in gb_tot on a_killed_w
regression_result <- lm(change_a_gb_tot ~ a_killed_w, data = final_data)
summary(regression_result)

final_data$binary <- ifelse(final_data$a_killed_w > 0, 1, 0)
regression_result_binary <- lm(change_a_gb_tot ~ binary, data = final_data)
summary(regression_result_binary)

mean_a_killed_w <- mean(final_data$a_killed_w, na.rm = TRUE)
mean_change <- mean(final_data$change_a_gb_tot, na.rm = TRUE)
increase_killed_w <- 0.10 * mean_a_killed_w
increase_change <- 0.0065 * mean_change
ratio <- increase_change / increase_killed_w

regression_result <- lm(change_gb_tot ~ killed_w, data = final_data)
summary(regression_result)

# final_data$binary <- ifelse(final_data$a_killed_w > 0, 1, 0)
# regression_result_binary <- lm(change_gb_tot ~ binary, data = final_data)
# summary(regression_result_binary)

transform_killed_w <- function(x) {
  if (min(x) == max(x)) return(x)  # Handle the case where all values are the same
  (x - min(x)) / (max(x) - min(x))
}

final_data$killed_w_transformed <- transform_killed_w(final_data$killed_w)

regression_result <- lm(change_gb_tot ~ killed_w_transformed, data = final_data)
summary(regression_result)

final_data$binary <- ifelse(final_data$killed_w_transformed > 0, 1, 0)
regression_result_binary <- lm(change_gb_tot ~ binary, data = final_data)
summary(regression_result_binary)

# Define the cont_twfe_weights function
cont_twfe_weights <- function(l, D) {
  wt <- ( ( mean(D[D>=l]) - mean(D) ) * mean(1*(D>=l)) ) / var(D)
  wt
}

# Prepare the data
dose <- final_data$killed_w_transformed
dy <- final_data$change_gb_tot

dL <- min(dose[dose>0])
dU <- max(dose)

# Create dose grid
dose_grid <- seq(dL, dU, length.out=100)

# Density plot of the dose
dose_density_plot <-ggplot(data.frame(dose=dose[dose>0]), aes(x=dose)) +
  geom_density(colour = "darkblue", linewidth = 1.2, fill = "lightblue", alpha = 0.4) +
  geom_vline(xintercept = mean(dose), colour="red", linewidth = 1, linetype = "dashed") +
  xlim(c(min(dose_grid), max(dose_grid))) +
  ylab("Density") +
  xlab("Dose (Casualties)") +
  ylim(c(0,3)) +
  labs(title="Density of Normalised Casualties", subtitle="Red line indicates mean dose level") +
  theme_minimal()

# Calculate TWFE weights
twfe_weights <- sapply(dose_grid, cont_twfe_weights, D=dose)
mean_weight <- mean(twfe_weights)

# Create dataframe for plotting
plot_df <- data.frame(dose_grid = dose_grid, twfe_weights = twfe_weights)

# TWFE weights plot
twfe_weights_plot <- ggplot(data=plot_df, aes(x = dose_grid, y = twfe_weights)) +
  geom_line(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid), max(dose_grid))) +
  ylab("TWFE Weights") +
  xlab("Dose (Casualties)") +
  geom_vline(xintercept = mean(dose), colour="red", linewidth = 1, linetype = "dashed") +
  ylim(c(0, max(twfe_weights) + 0.5)) +
  labs(title="TWFE Weights for Normalised White Casualties", subtitle="Red line indicates mean dose level") +
  theme_minimal()

green_twfe <- grid.arrange(dose_density_plot, twfe_weights_plot, ncol=2)
# ggsave("/home/oddish3/Documents/uni/master-dissertation/diss/figures/green_twfe.png", green_twfe, width=12, height=6, units="in", dpi=300)

# debugonce(npiv_regression)
res <- npiv_regression(treatment_col = "killed_w_transformed",
                       outcome_col = "change_gb_tot", data = final_data)

# Add binarised_estimate and acr_estimate to the data frames
binarised_estimate <- res[["binarised"]][["estimate"]]
acr_estimate <- res[["ACR_estimate"]]

# Create ATT Data Frame
att_df <- data.frame(
  dose = res[["Xx"]],
  att = res[["hhat"]],
  upper = res[["ATT_upper_UCB"]],
  lower = res[["ATT_lower_UCB"]],
  se = res[["sigh"]]
)

# Create ACR Data Frame
acr_df <- data.frame(
  dose = res[["Xx"]],
  acr = res[["dhat"]],
  upper = res[["ACR_upper_UCB"]],
  lower = res[["ACR_lower_UCB"]],
  se = res[["sigd"]]
)

att_df$ci_lower <- att_df$att - 1.96 * att_df$se
att_df$ci_upper <- att_df$att + 1.96 * att_df$se
acr_df$ci_lower <- acr_df$acr - 1.96 * acr_df$se
acr_df$ci_upper <- acr_df$acr + 1.96 * acr_df$se

# Function to create a clean, minimalist plot
create_clean_plot <- function(data, y_var, y_label) {
  ggplot(data, aes(x = dose)) +
    # Wider confidence interval ribbon
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#66C2A4", alpha = 0.2) +
    # Narrower confidence interval ribbon
    geom_ribbon(aes(ymin = !!sym(y_var) - 1.96 * se, ymax = !!sym(y_var) + 1.96 * se),
                fill = "#2B8C6B", alpha = 0.3) +
    # Main effect line
    geom_line(aes(y = !!sym(y_var)), color = "#007358", linewidth = 1) +
    # Zero reference line
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray30", linewidth = 0.5) +
    # Adjust x-axis to range from 0 to 1
    scale_x_continuous(expand = c(0.01, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0.01, 0)) +
    labs(x = "Dose (Casualties)",
         y = y_label) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.title = element_text(size = 12),  # Removed 'face = "bold"' to make axis titles normal
      axis.text = element_text(size = 10, color = "black"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
      legend.position = "none"
    )
}


# Create ATT plot
att_plot <- create_clean_plot(
  att_df,
  "att",
  "Average Treatment Effect on\nChange in Green Book Establishments"
)

# Create ACR plot
acr_plot <- create_clean_plot(
  acr_df,
  "acr",
  "Average Causal Response on\nChange in Green Book Establishments"
)

# Save individual plots
# ggsave(filename = "/home/oddish3/Documents/uni/master-dissertation/diss/figures/g_att_plot.png",
#        plot = att_plot, width = 8, height = 6, dpi = 300, bg = "white")
# ggsave(filename = "/home/oddish3/Documents/uni/master-dissertation/diss/figures/g_acr_plot.png",
#        plot = acr_plot, width = 8, height = 6, dpi = 300, bg = "white")

find_ranges <- function(dose_levels, gap = 0.01) {
  dose_levels <- sort(dose_levels)
  split_points <- c(0, which(diff(dose_levels) > gap), length(dose_levels))

  ranges <- lapply(seq_along(split_points[-1]), function(i) {
    range_start <- split_points[i] + 1
    range_end <- split_points[i + 1]
    range(dose_levels[range_start:range_end])
  })

  ranges
}

# Check for significant pointwise confidence intervals
significant_ci <- att_df %>%
  dplyr::filter(ci_lower > 0 | ci_upper < 0)

# Check for significant UCBs
significant_ucb <- att_df %>%
  dplyr::filter(lower > 0 | upper < 0)

# Display the dose levels where the intervals are significantly different from zero
significant_ci_dose_levels <- significant_ci$dose
significant_ucb_dose_levels <- significant_ucb$dose

# Calculate the ranges for pointwise confidence intervals
significant_ci_ranges <- find_ranges(significant_ci_dose_levels)

# Calculate the ranges for UCBs
significant_ucb_ranges <- find_ranges(significant_ucb_dose_levels)

list(
  significant_ci_ranges = significant_ci_ranges,
  significant_ucb_ranges = significant_ucb_ranges
)

# Check for significant pointwise confidence intervals
significant_ci <- acr_df %>%
  dplyr::filter(ci_lower > 0 | ci_upper < 0)

# Check for significant UCBs
significant_ucb <- acr_df %>%
  dplyr::filter(lower > 0 | upper < 0)

# Display the dose levels where the intervals are significantly different from zero
significant_ci_dose_levels <- significant_ci$dose
significant_ucb_dose_levels <- significant_ucb$dose

# Calculate the ranges for pointwise confidence intervals
significant_ci_ranges <- find_ranges(significant_ci_dose_levels)

# Calculate the ranges for UCBs
significant_ucb_ranges <- find_ranges(significant_ucb_dose_levels)

list(
  significant_ci_ranges = significant_ci_ranges,
  significant_ucb_ranges = significant_ucb_ranges
)


# Step 1: Calculate change in gb_tot relative to 1941
calculate_change <- function(data) {
  base_year <- 1941

  # Calculate base values for each county_code in 1941
  base_values <- data %>%
    filter(year == base_year) %>%
    select(county_code, gb_tot) %>%
    rename(base_gb_tot = gb_tot)

  data %>%
    left_join(base_values, by = "county_code") %>%
    group_by(county_code, year) %>%
    reframe(change_gb_tot = gb_tot - base_gb_tot) %>%
    arrange(county_code, year)
}

change_data <- calculate_change(data)

# Step 2: Merge with killed_w data
final_data <- data %>%
  left_join(change_data, by = c("county_code", "year"), relationship = "many-to-many") %>%
  select(county_code, year, killed_w, change_gb_tot)

# Step 3: Transform killed_w variable
transform_killed_w <- function(x) {
  if (min(x) == max(x)) return(x)  # Handle the case where all values are the same
  (x - min(x)) / (max(x) - min(x))
}
final_data <- final_data %>% filter(!is.na(killed_w) & !is.na(change_gb_tot))
final_data$killed_w_transformed <- transform_killed_w(final_data$killed_w)

years <- sort(unique(final_data$year))
years <- setdiff(years, 1941)

# Initialize a list to store results
results <- list()

# Run npiv_regression for each year
for (year in years) {
  # Subset the data for the current year and all previous years
  subset_data <- final_data[final_data$year == year, ]

  # Run the regression
  model <- npiv_regression(treatment_col = "killed_w_transformed",
                           outcome_col = "change_gb_tot",
                           data = subset_data)

  # Store the result
  results[[as.character(year)]] <- model

  # Print progress (optional)
  cat("Completed regression for year:", year, "\n")
}

all_objects <- ls()
keep_object <- c("results", "years")
rm(list = setdiff(all_objects, keep_object))

# Initialize empty data frames
df_binary <- data.frame(year = integer(), estimate = numeric(), se = numeric())
df_acr <- data.frame(year = integer(), estimate = numeric(), se = numeric())

# Iterate through years and extract data
for (year in years) {
  year_key <- as.character(year)

  # Binary estimates
  binary_estimate <- results[[year_key]][["binarised"]][["estimate"]][["binary"]]
  binary_se <- results[[year_key]][["binarised"]][["std_error"]][["binary"]]
  df_binary <- rbind(df_binary, data.frame(year = as.integer(year),
                                           estimate = binary_estimate,
                                           se = binary_se))

  # ACR estimates
  acr_estimate <- results[[year_key]][["ACR_estimate"]]
  acr_se <- results[[year_key]][["se_ACR"]]
  df_acr <- rbind(df_acr, data.frame(year = as.integer(year),
                                     estimate = acr_estimate,
                                     se = acr_se))
}

# Function to create event study plot
create_es_plot <- function(df) {
  ggplot(df, aes(x = year, y = estimate)) +
    geom_point(size = 2, color = "black") +
    geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  width = 0.2, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_rect(aes(xmin = 1942, xmax = 1946, ymin = -Inf, ymax = Inf),
              fill = "gray90", alpha = 0.5) +
    annotate("point", x = 1941, y = 0, color = "red", size = 3, shape = 18) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 5)) +
    theme_cowplot() +
    labs(x = "Year",
         y = "Estimate") +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),  # Add this line
      panel.border = element_blank(),  # Change this line
      panel.background = element_blank()  # Add this line
    )
}

# Create plots
plot_binary <- create_es_plot(df_binary)
plot_acr <- create_es_plot(df_acr)

# Display plots
print(plot_binary)
print(plot_acr)

# Optionally, save plots
# ggsave("/home/oddish3/Documents/uni/master-dissertation/diss/figures/binary_event_study.png", plot_binary, width=12, height=6, units="in", dpi=300)
# ggsave("/home/oddish3/Documents/uni/master-dissertation/diss/figures/acr_event_study.png", plot_acr, width=12, height=6, units="in", dpi=300)

