rm(list = ls())

library(fixest)
library(broom)
library(tidyverse)
library(haven)
library(knitr)

# Load data
longdiff_col <- read_dta("application/113746-V1/longdiff/co/longdiff_col.dta")

# Define control variables
controls <- list(
  base = "bplregcol",
  conflict = c("vioearly", "violate"),
  economic = c("cafetera", "carbon", "ganadera_neuva", "mktaccess", "manuf", "nivel_de_vida", "lndens"),
  diseases = c("helminth_nh", "hookworm", "leishmaniasis", "yelfev")
)

# Function to run regression and extract results
run_regression <- function(data, dep_var, mal_measure, additional_controls = NULL) {
  formula <- as.formula(paste(dep_var, "~", mal_measure, "+ bplregcol",
                              ifelse(!is.null(additional_controls),
                                     paste("+", paste(additional_controls, collapse = "+")),
                                     "")))

  model <- feols(formula, data = data, weights = ~ wtbpl, vcov = "hetero")

  coef <- coef(model)[mal_measure]
  se <- sqrt(vcov(model)[mal_measure, mal_measure])

  c(estimate = coef, std.error = se)
}

# Run regressions for all specifications
specifications <- list(
  list(name = "Base", controls = NULL),
  list(name = "Conflict", controls = controls$conflict),
  list(name = "Economic", controls = controls$economic),
  list(name = "Diseases", controls = controls$diseases)
)

dep_vars <- c("dlit", "dsch", "dscore")
mal_measures <- c("poveda", "mell")

results <- expand_grid(
  specification = specifications,
  dep_var = dep_vars,
  mal_measure = mal_measures
) %>%
  mutate(
    result = pmap(list(dep_var, mal_measure, specification),
                  ~run_regression(longdiff_col, ..1, ..2, ..3$controls))
  ) %>%
  unnest_wider(result)

# Format results
formatted_results <- results %>%
  pivot_longer(
    cols = c(estimate.poveda, estimate.mell),
    names_to = "estimate_type",
    values_to = "estimate"
  ) %>%
  mutate(
    estimate_type = str_remove(estimate_type, "estimate."),
    estimate_numeric = as.numeric(estimate),  # Convert to numeric
    std.error_numeric = as.numeric(std.error),  # Convert to numeric
    estimate = sprintf("%.4f", estimate_numeric),
    std.error = sprintf("(%.4f)", std.error_numeric),
    significance = case_when(
      is.na(estimate_numeric) ~ "",  # Handle NA values
      abs(estimate_numeric / std.error_numeric) > 2.576 ~ "***",
      abs(estimate_numeric / std.error_numeric) > 1.96 ~ "**",
      abs(estimate_numeric / std.error_numeric) > 1.645 ~ "*",
      TRUE ~ ""
    ),
    formatted = ifelse(is.na(estimate_numeric),
                       NA_character_,
                       paste0(estimate, significance, "\n", std.error))
  ) %>%
  filter(mal_measure == estimate_type) %>%
  select(specification, dep_var, mal_measure, formatted) %>%
  pivot_wider(
    names_from = c(mal_measure, dep_var),
    values_from = formatted,
    names_glue = "{mal_measure}_{dep_var}"
  ) %>%
  mutate(specification_name = map_chr(specification, ~.x$name)) %>%
  select(specification_name, everything(), -specification)

# Print results
print(formatted_results, n = Inf)
