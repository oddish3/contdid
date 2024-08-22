rm(list=ls())
library(fixest)
library(broom)
library(tidyverse)
library(haven)
library(binsreg)
library(contdid)
longdiff_col <- read_dta("application/113746-V1/longdiff/co/longdiff_col.dta")

run_regression <- function(data, mal_measure, controls = NULL) {
  dep_vars <- c("dlit", "dsch", "dscore")
  results <- list()

  for (dv in dep_vars) {
    formula <- as.formula(paste(dv, "~", mal_measure, "+ bplregcol", ifelse(!is.null(controls), paste("+", paste(controls, collapse = "+")), "")))
    model <- feols(formula, data = data, weights = ~ sqrt(wtbpl), vcov = "hetero")
    results[[dv]] <- tidy(model) %>%
      filter(term == mal_measure) %>%
      select(estimate, std.error)
  }

  bind_rows(results, .id = "dep_var")
}

# Run regressions and create table
table_data <- bind_rows(
  run_regression(longdiff_col, "poveda") %>% mutate(row = "None (basic specification)", measure = "Poveda"),
  run_regression(longdiff_col, "poveda", c("vioearly", "violate")) %>% mutate(row = "Conflict", measure = "Poveda"),
  run_regression(longdiff_col, "poveda", c("cafetera", "carbon", "ganadera_neuva", "mktaccess", "manuf", "nivel_de_vida", "lndens")) %>% mutate(row = "Economic activity", measure = "Poveda"),
  run_regression(longdiff_col, "poveda", c("helminth_nh", "hookworm", "leishmaniasis", "yelfev")) %>% mutate(row = "Other diseases", measure = "Poveda"),
  run_regression(longdiff_col, "mell") %>% mutate(row = "None (basic specification)", measure = "Mellinger"),
  run_regression(longdiff_col, "mell", c("vioearly", "violate")) %>% mutate(row = "Conflict", measure = "Mellinger"),
  run_regression(longdiff_col, "mell", c("cafetera", "carbon", "ganadera_neuva", "mktaccess", "manuf", "nivel_de_vida", "lndens")) %>% mutate(row = "Economic activity", measure = "Mellinger"),
  run_regression(longdiff_col, "mell", c("helminth_nh", "hookworm", "leishmaniasis", "yelfev")) %>% mutate(row = "Other diseases", measure = "Mellinger")
)

# Format table
formatted_table <- table_data %>%
  pivot_wider(
    id_cols = c(row, measure),
    names_from = dep_var,
    values_from = c(estimate, std.error),
    names_glue = "{dep_var}_{.value}"
  ) %>%
  select(row, measure,
         dlit_estimate, dlit_std.error,
         dsch_estimate, dsch_std.error,
         dscore_estimate, dscore_std.error)

# Print formatted table
print(formatted_table, n = Inf)

# Basic specification for literacy, using Poveda measure
basic_lit_model <- feols(dsch ~ poveda + bplregcol,
                         data = longdiff_col,
                         weights = ~ sqrt(wtbpl),
                         vcov = "hetero")

# Print the summary of the model
summary(basic_lit_model)

data <- longdiff_col

data <- data %>% filter(!is.na(poveda & dsch) & poveda < 1)

# Define the cont_twfe_weights function
cont_twfe_weights <- function(l, D) {
  wt <- ( ( mean(D[D>=l]) - mean(D) ) * mean(1*(D>=l)) ) / var(D)
  wt
}

# Prepare the data
dose <- data$poveda
dy <- data$dsch  # Using literacy as the outcome

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

print(dose_density_plot)

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
  labs(title="TWFE weights for Malaria Ecology (Poveda)")

print(twfe_weights_plot)

library(gridExtra)

grid.arrange(dose_density_plot, twfe_weights_plot, ncol=2)

res <- npiv_regression(treatment_col = "poveda", outcome_col = "dsch", data = data)

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
