rm(list=ls())
library(tictoc)
library(parallel)
library(doParallel)
library(foreach)
library(doRNG)
library(tidyverse)
library(fixest)
library(splines2)
library(triangle)
# remove.packages("contdid")
# devtools::build()
# devtools::install()
# devtools::load_all("~/Documents/uni/master-dissertation/contdid")
library(contdid)

# Source necessary functions
source("~/Documents/uni/master-dissertation/contdid/simulation/DGP1.R")
source("~/Documents/uni/master-dissertation/contdid/simulation/run_simulation.R")

# Set seed
seed1 <- 1234
set.seed(seed1)
n <- c(100, 500, 1000,2500)
nrep <- 1000

# Create cluster
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Export necessary functions to the cluster
clusterExport(cl, c("dgp_function", "gdata", "run_twfe", "run_feols_bspline", "calculate_point_metrics", "ucb_cvge",
                    "write_debug_info", "log_dimensions"))

# Run simulations for all DGPs

results_list <- list()
tic()
for (dgp in 1:4) {
  dgp_results <- list()
  for (sample_size in n) {
    tryCatch({
      dgp_results[[as.character(sample_size)]] <- run_simulation(n = sample_size, dgp = dgp, nrep = nrep)
    }, error = function(e) {
      cat("    Error occurred:", conditionMessage(e), "\n")
    })
  }
  results_list[[dgp]] <- do.call(rbind, dgp_results)
}

# Stop cluster
stopCluster(cl)
toc()
# saveRDS(results_list, file = "simulation/results_list2.rds")
# Combine all results into a single data frame
all_results <- do.call(rbind, results_list)

# Calculate mean results
mean_results <- all_results %>%
  group_by(n, dgp) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")


