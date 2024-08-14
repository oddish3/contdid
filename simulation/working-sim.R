rm(list=ls())
library(parallel)
library(doParallel)
library(foreach)
library(doRNG)
library(tidyverse)
library(fixest)
library(splines2)

devtools::load_all("~/Documents/uni/master-dissertation/contdid")
library(contdid)

# Source necessary functions
source("~/Documents/uni/master-dissertation/contdid/simulation/DGP1.R")
source("~/Documents/uni/master-dissertation/contdid/simulation/run_simulation.R")
n <- c(100, 500, 1000)
nrep <- 20
# Create cluster
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Set seed
seed1 <- 1234
set.seed(seed1)

# Export necessary functions to the cluster
clusterExport(cl, c("dgp_function", "gdata", "run_twfe", "run_feols_bspline", "calculate_point_metrics", "ucb_cvge"))

# Run simulations for all DGPs

results_list <- list()

for (dgp in 1:4) {
  cat("Processing DGP:", dgp, "\n")
  dgp_results <- list()
  for (sample_size in n) {
    cat("  Sample size:", sample_size, "\n")
    tryCatch({
      dgp_results[[as.character(sample_size)]] <- run_simulation(n = sample_size, dgp = dgp, nrep = nrep)
      cat("    Completed successfully\n")
    }, error = function(e) {
      cat("    Error occurred:", conditionMessage(e), "\n")
    })
  }
  results_list[[dgp]] <- do.call(rbind, dgp_results)
  cat("DGP", dgp, "completed\n\n")
}

# Combine all results into a single data frame
all_results <- do.call(rbind, results_list)

# Calculate mean results
mean_results <- all_results %>%
  group_by(n, dgp) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Save results
# saveRDS(all_results, file = "all_simulation_results.rds")
# saveRDS(mean_results, file = "mean_simulation_results.rds")

# Print summary of results
cat("Summary of results:\n")
print(summary(all_results))

# Stop cluster
stopCluster(cl)

cat("Simulation completed.\n")
