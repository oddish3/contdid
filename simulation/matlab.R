library(contdid)
library(triangle)
source("~/Documents/uni/master-dissertation/contdid/simulation/DGP1.R")
# Define dgp_function (as you provided)
dgp_function <- function(dgp) {
  if (dgp == 1) {
    dist <- function(n) runif(n, 0, 1)
    func <- function(x) x^2
    func_deriv <- function(x) 2*x
    E_func <- 1/3
    E_deriv <- 1
  }
  # ... (other DGPs)
  return(list(
    dist = dist,
    func = func,
    func_deriv = func_deriv,
    E_func = E_func,
    E_deriv = E_deriv
  ))
}

# Define gdata function (as modified earlier)
gdata <- function(n, dgp, prop_treated = 0.8, noise_sd = 0.1013251) {
  dgp_params <- dgp_function(dgp)
  treatment <- rbinom(n, 1, prop_treated)
  dose <- rep(0, n)
  dose[treatment == 1] <- dgp_params$dist(sum(treatment))
  dy <- rep(0, n)
  dy[treatment == 1] <- dgp_params$func(dose[treatment == 1])
  dy[treatment == 0] <- rnorm(sum(treatment == 0), mean = 0, sd = noise_sd)
  dy <- dy + rnorm(n, mean = 0, sd = noise_sd)
  att <- mean(dgp_params$func(dose[treatment == 1]))
  true_acr <- mean(dgp_params$func_deriv(dose[dose > 0]))
  x_grid <- seq(0, 1, length.out = 1000)
  func_values <- dgp_params$func(x_grid)
  func_deriv_values <- dgp_params$func_deriv(x_grid)
  df <- data.frame(
    id = 1:n,
    dy = dy,
    dose = dose,
    treatment = treatment
  )
  return(list(
    data = df,
    info = list(
      pop_att = att,
      true_acr = true_acr,
      func_values = func_values,
      func_deriv_values = func_deriv_values
    )
  ))
}

# MWE function
investigate_ucb_npiv <- function(n = 500, dgp = 1) {
  # Generate data
  cat("Generating data...\n")
  dat <- gdata(n = n, dgp = dgp)
  data <- dat$data
  info <- dat$info

  # Print data summary
  cat("Data summary:\n")
  print(summary(data))

  # Run NPIV regression
  cat("\nRunning NPIV regression...\n")
  npiv_result <- npiv_regression(
    data = data,
    treatment_col = "dose",
    outcome_col = "dy"
  )

  # Print NPIV result summary
  cat("NPIV result summary:\n")
  print(summary(npiv_result$hhat))
  print(summary(npiv_result$sigh))

  # Calculate UCB coverage
  cat("\nCalculating UCB coverage...\n")
  h0 <- info$func_values[order(npiv_result$Xx)]
  ucb_result <- ucb_cvge(
    h0 = h0,
    hhat = npiv_result$hhat,
    sigh = npiv_result$sigh,
    zast = npiv_result$hzast,
    theta = npiv_result$thet,
    A = log(log(length(npiv_result$Xx)))  # Add this line
  )

  # Print UCB result summary
  cat("UCB result summary:\n")
  print(summary(ucb_result$check))
  cat("UCB coverage:", mean(ucb_result$check), "\n")
  cat("UCB loss:", ucb_result$loss, "\n")
  cat("UCB tmax:", ucb_result$tmax, "\n")

  # Plot true function vs. estimated function
  plot(npiv_result$Xx, h0, type = "l", col = "blue",
       xlab = "Dose", ylab = "Outcome",
       main = "True vs. Estimated Function")
  lines(npiv_result$Xx, npiv_result$hhat, col = "red")
  legend("topleft", legend = c("True", "Estimated"),
         col = c("blue", "red"), lty = 1)
  return(list(
    data = data,
    info = info,
    npiv_result = npiv_result,
    ucb_result = ucb_result
  ))
}

# Run the investigation
set.seed(123)  # for reproducibility
results <- investigate_ucb_npiv(n = 500, dgp = 1)

# Save data for MATLAB
matlab_data <- data.frame(
  dose = results$npiv_result$Xx,
  true_func = results$h0,
  estimated_func = results$npiv_result$hhat,
  std_error = results$npiv_result$sigh,
  tmax = results$ucb_result$tmax
)
write.csv(matlab_data, "/home/oddish3/Documents/M_folder/CCK-OG/CCK/npiv_data.csv", row.names = FALSE)

# Save UCB parameters
ucb_params <- data.frame(
  zast = results$npiv_result$hzast,
  theta = results$npiv_result$thet,
  A = log(log(length(results$npiv_result$Xx)))
)
write.csv(ucb_params, "/home/oddish3/Documents/M_folder/CCK-OG/CCK/ucb_params.csv", row.names = FALSE)

# Print confirmation
cat("Data saved for MATLAB analysis in 'npiv_data.csv' and 'ucb_params.csv'\n")

# Print key results for inspection
cat("\nKey results for inspection:\n")
cat("UCB loss:", results$ucb_result$loss, "\n")
cat("UCB tmax:", results$ucb_result$tmax, "\n")
cat("UCB coverage:", mean(results$ucb_result$check), "\n")
cat("zast:", results$npiv_result$hzast, "\n")
cat("theta:", results$npiv_result$thet, "\n")
cat("A:", log(log(length(results$npiv_result$Xx))), "\n")
