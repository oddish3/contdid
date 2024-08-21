library(truncnorm)
remove.packages("contdid")
devtools::build()
devtools::install()
devtools::load_all("~/Documents/uni/master-dissertation/contdid")
set.seed(123)
gdata <- function(n, prop_treated = 0.8) {

  # Generate treatment assignment
  treatment <- rbinom(n, 1, prop_treated)

  # Generate dose for treated units
  dose <- rep(0, n)
  dose[treatment == 1] <- rtruncnorm(sum(treatment == 1), a = 0.011, b = 0.99, 0.5, 0.16)

  dy <- rep(0, n)
  dy[treatment == 1] <- -4*(dose[treatment == 1] - 0.5)^2 + 1
  # Create dataframe
  df <- data.frame(
    dy = dy,
    treatment = treatment,
    dose = dose
  )

  return(df)
}

data <- gdata(1000)
mean(data$dy[data$treatment == 1]) - mean(data$dy[data$treatment == 0])
result <- npiv_regression(
  data = data,
  treatment_col = "dose",
  outcome_col = "dy",
)
