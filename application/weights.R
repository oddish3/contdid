# 1. Causal response weight for D > 0
w1_acr <- function(l, D) {
  (mean(D[D >= l]) - mean(D)) * mean(1*(D >= l)) / var(D)
}

# 2. Causal response weight for D = 0
w0_acr <- function(D) {
  (mean(D[D > 0]) - mean(D)) * mean(1*(D > 0)) / var(D)
}

# 3. Levels weight for D > 0
w1_lev <- function(l, D, fD) {
  (l - mean(D)) / var(D) * fD(l)
}

# 4. Levels weight for D = 0
w0_lev <- function(D) {
  -mean(D) * mean(1*(D == 0)) / var(D)
}

# 5. Scaled levels weight
ws <- function(l, D, fD) {
  l * (l - mean(D)) / var(D) * fD(l)
}

# 6. Scaled 2x2 weight for D > 0
w1_2x2 <- function(l, h, D, fD) {
  ((h - l)^2 * fD(h) * fD(l)) / var(D)
}

# 7. Scaled 2x2 weight for D = 0
w0_2x2 <- function(h, D, fD) {
  (h^2 * fD(h) * mean(1*(D == 0))) / var(D)
}

# Note: fD is the probability density function of D, which should be provided separately
