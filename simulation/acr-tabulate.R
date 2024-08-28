# Clear environment and load libraries
rm(list = ls())
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Load the data
data <- readRDS("~/Documents/uni/master-dissertation/contdid/simulation/results_list2.rds")
all_results <- do.call(rbind, data)

# Calculate mean results
mean_results <- all_results %>%
  group_by(n, dgp) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Function to compute summary statistics for an estimator
compute_summary <- function(data, estimator_prefix) {
  data %>%
    group_by(n, dgp) %>%
    summarise(
      estimator = estimator_prefix,
      avg_bias = mean(!!sym(paste0(estimator_prefix, "_bias"))),
      median_bias = median(!!sym(paste0(estimator_prefix, "_bias"))),
      rms = sqrt(mean(!!sym(paste0(estimator_prefix, "_mse")))),
      asymp_var = var(!!sym(paste0(estimator_prefix, "_estimate"))),
      coverage = mean(!!sym(paste0(estimator_prefix, "_pcoverage"))),
      cil = mean(2 * !!sym(paste0(estimator_prefix, "_se"))),
      ucb_cov = if (estimator_prefix == "npiv_acr") {
        mean(!!sym("acr_ucb_coverage"))  # Replace with the actual column name
      } else {
        NA_real_
      },
      .groups = "drop"
    )
}

npiv_summary <- compute_summary(all_results, "npiv_acr")
twfe_summary <- compute_summary(all_results, "twfe")
bspline_summary <- compute_summary(all_results, "bspline")

# Combine all summaries
all_summaries <- bind_rows(npiv_summary, twfe_summary, bspline_summary)

# Reshape the data to have separate columns for each statistic and DGP
summary_wide <- all_summaries %>%
  pivot_longer(
    cols = avg_bias:ucb_cov,
    names_to = "statistic",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = c(dgp, statistic),
    values_from = value,
    names_sep = "_"
  ) %>%
  select(estimator, n, starts_with("1_"), starts_with("2_"), starts_with("3_"), starts_with("4_"))

# Function to create a formatted subtable for each DGP
create_dgp_subtable <- function(data, dgp_num) {
  data %>%
    select(estimator, n, starts_with(paste0(dgp_num, "_"))) %>%
    rename_with(~sub(paste0(dgp_num, "_"), "", .), starts_with(paste0(dgp_num, "_"))) %>%
    mutate(across(where(is.numeric), ~format(round(., 3), nsmall = 3))) %>%
    kable(format = "latex", booktabs = TRUE, align = "lrrrrrrrr") %>%
    kable_styling(latex_options = c("scale_down")) %>%
    add_header_above(c(" " = 2, "DGP" = 7)) %>%
    column_spec(1:2, bold = TRUE)
}

# Create subtables for each DGP
dgp1_table <- create_dgp_subtable(summary_wide, 1)
dgp2_table <- create_dgp_subtable(summary_wide, 2)
dgp3_table <- create_dgp_subtable(summary_wide, 3)
dgp4_table <- create_dgp_subtable(summary_wide, 4)

# Combine all subtables into a single LaTeX table
latex_table <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Results for All DGPs}\n",
  "\\begin{subtable}{.5\\linewidth}\n",
  dgp1_table,
  "\\end{subtable}%\n",
  "\\begin{subtable}{.5\\linewidth}\n",
  dgp2_table,
  "\\end{subtable}\n",
  "\\begin{subtable}{.5\\linewidth}\n",
  dgp3_table,
  "\\end{subtable}%\n",
  "\\begin{subtable}{.5\\linewidth}\n",
  dgp4_table,
  "\\end{subtable}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item Note: This table presents results for all four DGPs. NPIV, TWFE, and B-spline estimators are compared across various metrics.\n",
  "\\end{tablenotes}\n",
  "\\end{table}"
)

# Print the LaTeX code
cat(latex_table)

