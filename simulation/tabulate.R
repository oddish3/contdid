# Clear environment
rm(list = ls())

# Load necessary libraries
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Load the data
data <- readRDS("~/Documents/uni/master-dissertation/contdid/simulation/results_list2.rds")
all_results <- do.call(rbind, data)

# Compute statistics for ATT for the npiv estimator, including ucb_cov
npiv_summary <- all_results %>%
  group_by(n, dgp) %>%
  summarise(
    estimator = "npiv",
    att_avg_bias = mean(npiv_att_bias),
    median_bias = median(npiv_att_bias),
    rms = sqrt(mean(npiv_att_mse)),
    asymp_var = var(npiv_att_estimate),
    coverage = mean(npiv_att_pcoverage),
    cil = mean(2 * npiv_att_se),
    ucb_cov = mean(att_ucb_coverage)
  )

# Compute statistics for ATT for the bspline estimator (ucb_cov left blank)
bspline_summary <- all_results %>%
  group_by(n, dgp) %>%
  summarise(
    estimator = "bspline",
    att_avg_bias = mean(bspline_bias),
    median_bias = median(bspline_bias),
    rms = sqrt(mean(bspline_mse)),
    asymp_var = var(bspline_estimate),
    coverage = mean(bspline_pcoverage),
    cil = mean(2 * bspline_se),
    ucb_cov = NA_real_  # Placeholder for ucb_cov in bspline
  )

# Combine the summaries
att_summary <- bind_rows(npiv_summary, bspline_summary)

# Reshape the data to have separate columns for each statistic
att_summary_long <- att_summary %>%
  pivot_longer(
    cols = att_avg_bias:ucb_cov,
    names_to = "statistic",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = "statistic",
    values_from = "value"
  ) %>%
  relocate(estimator, .before = dgp)

# Print the reshaped summary
print(att_summary_long)

# Reshape the data to match the desired format
table_data <- att_summary_long %>%
  select(estimator, dgp, att_avg_bias, median_bias, rms, asymp_var, coverage, cil, ucb_cov) %>%
  mutate(dgp = paste0("DGP", dgp)) %>%
  rename(
    "Av. Bias" = att_avg_bias,
    "Med. Bias" = median_bias,
    "RMSE" = rms,
    "Asy. V" = asymp_var,
    "Cover" = coverage,
    "CIL" = cil,
    "UCB Cov" = ucb_cov
  ) %>%
  pivot_wider(
    names_from = dgp,
    values_from = c("Av. Bias", "Med. Bias", "RMSE", "Asy. V", "Cover", "CIL")
  )

create_subtable <- function(data, dgp) {
  subtable <- data %>%
    filter(dgp == !!dgp) %>%
    select(estimator, att_avg_bias, median_bias, rms, asymp_var, coverage, cil, ucb_cov) %>%
    rename(
      "Av. Bias" = att_avg_bias,
      "Med. Bias" = median_bias,
      "RMSE" = rms,
      "Asy. V" = asymp_var,
      "Cover" = coverage,
      "CIL" = cil,
      "UCB Cov" = ucb_cov
    )

  kable(subtable, format = "html", escape = FALSE, caption = paste("DGP", dgp)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
    column_spec(1, bold = TRUE)
}

# Create subtables
subtable1 <- create_subtable(att_summary_long, 1)
subtable2 <- create_subtable(att_summary_long, 2)
subtable3 <- create_subtable(att_summary_long, 3)
subtable4 <- create_subtable(att_summary_long, 4)

create_dgp_subtable <- function(data, dgp_num) {
  data %>%
    filter(dgp == dgp_num) %>%
    select(n, estimator, att_avg_bias, median_bias, rms, asymp_var, coverage, cil, ucb_cov) %>%
    mutate(across(where(is.numeric), ~ format(round(., 3), nsmall = 3))) %>%
    mutate(ucb_cov = ifelse(estimator == "B-spline", "--", ucb_cov)) %>%
    rename(
      "Av. Bias" = att_avg_bias,
      "Med. Bias" = median_bias,
      "RMSE" = rms,
      "Asy. V" = asymp_var,
      "Cover" = coverage,
      "CIL" = cil,
      "UCB Cov" = ucb_cov
    )
}

# Create subtables for each DGP
dgp1 <- create_dgp_subtable(att_summary_long, 1)
dgp2 <- create_dgp_subtable(att_summary_long, 2)
dgp3 <- create_dgp_subtable(att_summary_long, 3)
dgp4 <- create_dgp_subtable(att_summary_long, 4)


create_quadrant <- function(data, dgp_num) {
  kable(data, format = "latex", booktabs = TRUE, align = "lrrrrrrrr") %>%
    kable_styling(latex_options = c("scale_down")) %>%
    add_header_above(c(" " = 2, "DGP " = 7)) %>%
    column_spec(1:2, bold = TRUE)
}

# Create LaTeX code for each quadrant
q1 <- create_quadrant(dgp1, 1)
q2 <- create_quadrant(dgp2, 2)
q3 <- create_quadrant(dgp3, 3)
q4 <- create_quadrant(dgp4, 4)

# Combine quadrants into a single LaTeX table
latex_table <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Results for All DGPs}\n",
  "\\begin{subtable}{.5\\linewidth}\n",
  q1,
  "\\end{subtable}%\n",
  "\\begin{subtable}{.5\\linewidth}\n",
  q2,
  "\\end{subtable}\n",
  "\\begin{subtable}{.5\\linewidth}\n",
  q3,
  "\\end{subtable}%\n",
  "\\begin{subtable}{.5\\linewidth}\n",
  q4,
  "\\end{subtable}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item Note: This table presents results for all four DGPs. NPIV and B-spline estimators are compared across various metrics. UCB Cov is not available for the B-spline estimator.\n",
  "\\end{tablenotes}\n",
  "\\end{table}"
)

# Print the LaTeX code
cat(latex_table)



































