rm(list=ls())

# Load necessary libraries
library(fixest)
library(dplyr)
library(broom)
library(sandwich)
library(kableExtra)
library(knitr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(contdid)

# Load the dataset
data <- haven::read_dta("application/113746-V1/longdiff/co/longdiff_col.dta")

# Convert bplregcol to a factor variable and create dummy variables
data$bplregcol <- as.factor(data$bplregcol)
data <- fastDummies::dummy_cols(data, select_columns = "bplregcol", remove_selected_columns = TRUE)

# Define control variable sets
holdridge <- c("ecozone_stdry", "ecozone_stwet", "ecozone_trdry", "ecozone_trwet", "ecozone_warm")
conflict <- c("vioearly", "violate")
endowment <- c("cafetera", "carbon", "ganadera_neuva", "mktaccess", "manuf", "nivel_de_vida", "lndens")
diseases <- c("helminth_nh", "hookworm", "leishmaniasis", "yelfev")
allthree <- c("helminth_nh", "hookworm", "leishmaniasis", "yelfev", "land_inadeq",
              grep("^vio", names(data), value=TRUE),
              "cafetera", "carbon", "ganadera_neuva", "mktaccess", "manuf", "nivel_de_vida")

# Function to run regression and extract results
run_reg_set <- function(depvar, malaria_var, controls, data) {
  formula <- as.formula(paste(depvar, "~", malaria_var, "+",
                              paste(c(grep("^bplregcol_", names(data), value=TRUE), controls), collapse = "+")))

  model <- feols(formula, data = data, weights = ~wtbpl, vcov = "HC1")

  coef <- coef(model)[malaria_var]
  se <- sqrt(vcov(model)[malaria_var, malaria_var])
  p_value <- summary(model)[["coeftable"]][2,4]
  stars <- ifelse(p_value < 0.01, "***", ifelse(p_value < 0.05, "**", ifelse(p_value < 0.1, "*", "")))

  return(list(coef = coef, se = se, stars = stars, r2 = r2(model)))
}

# Define dependent variables and control sets
dep_vars <- c("dlit", "dsch", "dscore")
control_sets <- list(
  "None (basic specification)" = character(0),
  "Conflict" = conflict,
  "Economic activity" = endowment,
  "Other diseases" = diseases,
  "Full controls" = allthree
)

# Run regressions for Poveda and Mellinger measures
results_poveda <- lapply(dep_vars, function(dv) {
  sapply(control_sets, function(cs) run_reg_set(dv, "poveda", cs, data))
})

results_mell <- lapply(dep_vars, function(dv) {
  sapply(control_sets, function(cs) run_reg_set(dv, "mell", cs, data))
})

format_results <- function(results_list, malaria_var) {
  formatted <- lapply(seq_along(results_list), function(i) {
    dv <- dep_vars[i]
    res <- results_list[[i]]
    data.frame(
      "Dependent.Variable" = rep(dv, ncol(res)),
      "Controls" = colnames(res),
      "Coefficient" = sprintf("%.3f%s", res["coef",], res["stars",]),
      "SE" = sprintf("(%.3f)", res["se",])
    )
  })
  do.call(rbind, formatted)
}

# Use the function
table_poveda <- format_results(results_poveda, "poveda")
table_mell <- format_results(results_mell, "mell")

# Merge the datasets
merged_table <- full_join(table_poveda, table_mell,
                          by = c("Dependent.Variable", "Controls"),
                          suffix = c("_Poveda", "_Mellinger"))

# Merge the datasets
merged_table <- full_join(table_poveda, table_mell,
                          by = c("Dependent.Variable", "Controls"),
                          suffix = c("_Poveda", "_Mellinger"))

# Reshape the data
reshaped_data <- merged_table %>%
  pivot_wider(
    id_cols = Controls,
    names_from = Dependent.Variable,
    values_from = c(Coefficient_Poveda, SE_Poveda, Coefficient_Mellinger, SE_Mellinger),
    names_sep = "_"
  ) %>%
  select(
    Controls,
    Literacy_Poveda = Coefficient_Poveda_dlit,
    Literacy_SE_Poveda = SE_Poveda_dlit,
    Schooling_Poveda = Coefficient_Poveda_dsch,
    Schooling_SE_Poveda = SE_Poveda_dsch,
    Income_Poveda = Coefficient_Poveda_dscore,
    Income_SE_Poveda = SE_Poveda_dscore,
    Literacy_Mellinger = Coefficient_Mellinger_dlit,
    Literacy_SE_Mellinger = SE_Mellinger_dlit,
    Schooling_Mellinger = Coefficient_Mellinger_dsch,
    Schooling_SE_Mellinger = SE_Mellinger_dsch,
    Income_Mellinger = Coefficient_Mellinger_dscore,
    Income_SE_Mellinger = SE_Mellinger_dscore
  )

# Combine coefficient and SE columns
combine_coef_se <- function(coef, se) {
  paste0(coef, "\n", se)
}

reshaped_data_combined <- reshaped_data %>%
  mutate(
    Literacy_Poveda = combine_coef_se(Literacy_Poveda, Literacy_SE_Poveda),
    Schooling_Poveda = combine_coef_se(Schooling_Poveda, Schooling_SE_Poveda),
    Income_Poveda = combine_coef_se(Income_Poveda, Income_SE_Poveda),
    Literacy_Mellinger = combine_coef_se(Literacy_Mellinger, Literacy_SE_Mellinger),
    Schooling_Mellinger = combine_coef_se(Schooling_Mellinger, Schooling_SE_Mellinger),
    Income_Mellinger = combine_coef_se(Income_Mellinger, Income_SE_Mellinger)
  ) %>%
  select(Controls,
         Literacy_Poveda, Schooling_Poveda, Income_Poveda,
         Literacy_Mellinger, Schooling_Mellinger, Income_Mellinger)

# Create LaTeX table
latex_table <- reshaped_data_combined %>%
  kbl(format = "latex",
      booktabs = TRUE,
      caption = "Malaria ecology studies comparison",
      col.names = c("Controls",
                    "Literacy", "Years of schooling", "Income index",
                    "Literacy", "Years of schooling", "Income index"),
      align = c("l", rep("c", 6))) %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  add_header_above(c(" " = 1,
                     "Malaria ecology (Poveda)" = 3,
                     "Malaria ecology (Mellinger)" = 3))

# Print the LaTeX table
# cat(latex_table)

# Basic specification for literacy, using Poveda measure

# Fit the model directly with additional components using grep
model <- feols(dscore~mell,
               data = data, weights = ~wtbpl, vcov = "HC1")
# Print out the model summary
summary(model)

# summary(data$poveda)
# summary(data$mell)
data$poveda <- ifelse(data$poveda > 1, 1, data$poveda)
data$mell <- ifelse(data$mell > 1, 1, data$mell)
data <- data %>% filter(!is.na(poveda & dsch) & poveda < 1)

perform_analysis <- function(dose_var, dep_var, final_data) {
  dose <- final_data[[dose_var]]
  dy <- final_data[[dep_var]]

  twfe_formula <- as.formula(paste(dep_var, "~", dose_var))
  twfe <- feols(twfe_formula, data = final_data, weights = ~wtbpl, vcov = "HC1")

  dL <- min(dose[dose > 0])
  dU <- max(dose)

  # Create dose grid
  dose_grid <- seq(dL, dU, length.out = 100)

  # Density plot of the dose
  dose_density_plot <- ggplot(data.frame(dose = dose[dose > 0]), aes(x = dose)) +
    geom_density(colour = "darkblue", linewidth = 1.2, fill = "lightblue", alpha = 0.4) +
    geom_vline(xintercept = mean(dose), colour = "red", linewidth = 1, linetype = "dashed") +
    xlim(c(min(dose_grid), max(dose_grid))) +
    ylab("Density") +
    xlab("Dose (Malaria Index)") +
    ylim(c(0, 3)) +
    labs(title = paste("Density of ", dose_var), subtitle = "Red line indicates mean dose level") +
    theme_minimal()

  # Calculate TWFE weights
  twfe_weights <- sapply(dose_grid, cont_twfe_weights, D = dose)
  mean_weight <- mean(twfe_weights)

  # Create dataframe for plotting
  plot_df <- data.frame(dose_grid = dose_grid, twfe_weights = twfe_weights)

  # TWFE weights plot
  twfe_weights_plot <- ggplot(data = plot_df, aes(x = dose_grid, y = twfe_weights)) +
    geom_line(colour = "darkblue", linewidth = 1.2) +
    xlim(c(min(dose_grid), max(dose_grid))) +
    ylab("TWFE Weights") +
    xlab("Dose (Malaria Index)") +
    geom_vline(xintercept = mean(dose), colour = "red", linewidth = 1, linetype = "dashed") +
    ylim(c(0, max(twfe_weights) + 0.5)) +
    labs(title = paste("TWFE Weights for ", dose_var), subtitle = "Red line indicates mean dose level") +
    theme_minimal()

  # Combine plots (can be commented out if not needed)
  green_twfe <- grid.arrange(dose_density_plot, twfe_weights_plot, ncol = 2)

  # Perform npiv_regression
  res <- npiv_regression(treatment_col = dose_var, outcome_col = dep_var, data = final_data)

  # Prepare data for ATT and ACR plots
  att_df <- data.frame(
    dose = res[["Xx"]],
    att = res[["hhat"]],
    upper = res[["ATT_upper_UCB"]],
    lower = res[["ATT_lower_UCB"]],
    se = res[["sigh"]]
  )

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

  # Create ATT and ACR plots
  att_plot <- create_clean_plot(att_df, "att", "Average Treatment Effect on\nChange in Dependent Variable")
  acr_plot <- create_clean_plot(acr_df, "acr", "Average Causal Response on\nChange in Dependent Variable")

  # Find significant dose levels
  significant_ci <- att_df %>%
    dplyr::filter(ci_lower > 0 | ci_upper < 0)

  significant_ucb <- att_df %>%
    dplyr::filter(lower > 0 | upper < 0)

  significant_ci_dose_levels <- significant_ci$dose
  significant_ucb_dose_levels <- significant_ucb$dose

  significant_ci_ranges <- find_ranges(significant_ci_dose_levels)
  significant_ucb_ranges <- find_ranges(significant_ucb_dose_levels)

  # Repeat for ACR
  significant_ci_acr <- acr_df %>%
    dplyr::filter(ci_lower > 0 | ci_upper < 0)

  significant_ucb_acr <- acr_df %>%
    dplyr::filter(lower > 0 | upper < 0)

  significant_ci_dose_levels_acr <- significant_ci_acr$dose
  significant_ucb_dose_levels_acr <- significant_ucb_acr$dose

  significant_ci_ranges_acr <- find_ranges(significant_ci_dose_levels_acr)
  significant_ucb_ranges_acr <- find_ranges(significant_ucb_dose_levels_acr)

  # Return the results as a list for inspection
  list(
    twfe = twfe,
    dose_twfe_plot = green_twfe,
    att_plot = att_plot,
    acr_plot = acr_plot,
    significant_ci_ranges = significant_ci_ranges,
    significant_ucb_ranges = significant_ucb_ranges,
    significant_ci_ranges_acr = significant_ci_ranges_acr,
    significant_ucb_ranges_acr = significant_ucb_ranges_acr,
    res = res["summary"]
  )
}

cont_twfe_weights <- function(l, D) {
  # Estimate density function
  density_obj <- density(D)
  fD <- function(x) {
    approx(density_obj$x, density_obj$y, xout = x, yleft = 0, yright = 0)$y
  }

  # Calculate weight
  (l - mean(D)) / var(D) * fD(l)
}

create_clean_plot <- function(data, y_var, y_label) {
  ggplot(data, aes(x = dose)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#66C2A4", alpha = 0.2) +
    geom_ribbon(aes(ymin = !!sym(y_var) - 1.96 * se, ymax = !!sym(y_var) + 1.96 * se),
                fill = "#2B8C6B", alpha = 0.3) +
    geom_line(aes(y = !!sym(y_var)), color = "#007358", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray30", linewidth = 0.5) +
    scale_x_continuous(expand = c(0.01, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0.01, 0)) +
    labs(x = "Dose (Malaria Index)", y = y_label) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10, color = "black"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
      legend.position = "none"
    )
}

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

# Example usage:
dep_vars <- c("dlit", "dsch", "dscore")
dose_vars <- c("poveda", "mell")

results <- list()
for (dep_var in dep_vars) {
  for (dose_var in dose_vars) {
    result_key <- paste0(dose_var, "_", dep_var)
    results[[result_key]] <- perform_analysis(dose_var, dep_var, data)
  }
}
plot_to_save <- results[["poveda_dlit"]][["dose_twfe_plot"]]
ggsave(
  filename = "/home/oddish3/Documents/uni/master-dissertation/diss/figures/povedatw.png",  # or .pdf, .jpg, etc.
  plot = plot_to_save,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

plot_to_sav2 <- results[["poveda_dlit"]][["att_plot"]]
ggsave(
  filename = "/home/oddish3/Documents/uni/master-dissertation/diss/figures/povedaatt.png",
  plot = plot_to_sav2,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)
