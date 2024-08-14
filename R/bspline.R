#' B-spline calculation
#'
#' This function calculates B-spline basis functions.
#'
#' @param x Input vector. The points at which to evaluate the B-spline basis functions.
#' @param l Resolution level. Determines the number of basis functions.
#' @param r Order of the B-spline. Determines the polynomial degree (degree = order - 1).
#' @param kts Optional. Vector of knots for the B-spline. If NULL, knots will be generated automatically.
#'
#' @return A matrix of B-spline basis function values.
#'
#' @export
bspline <- function(x, l, r, kts = NULL) {
  N <- length(x)
  m <- 2^l - 1
  r <- r + 1

  # Define the augmented knot set
  if (is.null(kts)) {
    if (l == 0) {
      kts <- c(rep(0, r-1), rep(1, r-1))
    } else if (l >= 1) {
      kts <- c(rep(0, r-2), seq(0, 1, length.out = 2^l + 1), rep(1, r-2))
    }
  }

  # Initialize for recursion
  BB <- array(0, dim = c(N, m + 2*r - 2, r-1))
  for (i in 1:N) {
    ix <- which(x[i] >= kts[(r-1):(r+m-1)] & x[i] <= kts[r:(r+m)])[1]
    BB[i, ix + r - 2, 1] <- 1
  }

  # Recursion
  for (j in 2:(r-1)) {
    for (i in 1:(m + 2*r - 2 - j)) {
      if (i + j + 1 <= m + 2*r) {
        if (kts[i + j - 1] - kts[i] != 0) {
          a1 <- (x - kts[i]) / (kts[i + j - 1] - kts[i])
        } else {
          a1 <- rep(1, N)
        }
        if (kts[i + j] - kts[i + 1] != 0) {
          a2 <- (x - kts[i + 1]) / (kts[i + j] - kts[i + 1])
        } else {
          a2 <- rep(0, N)
        }
        BB[, i, j] <- a1 * BB[, i, j - 1] + (1 - a2) * BB[, i + 1, j - 1]
      } else if(i + j <= m + 2*r) {
        if (kts[i + j] - kts[i] != 0) {
          a1 <- (x - kts[i]) / (kts[i + j] - kts[i])
        } else {
          a1 <- rep(0, N)
        }
        BB[, i, j] <- a1 * BB[, i, j - 1]
      }
    }
  }
  XX <- BB[, 1:(2^l + r - 2), r - 1]

  # Calculate derivative
  DX <- matrix(0, nrow = N, ncol = m + r - 1)
  for (i in 1:(m + r - 1)) {
    if (kts[i + r - 2] - kts[i] != 0) {
      a1 <- rep(1 / (kts[i + r - 2] - kts[i]), N)
    } else {
      a1 <- rep(0, N)
    }
    if (kts[i + r - 1] - kts[i + 1] != 0) {
      a2 <- rep(1 / (kts[i + r - 1] - kts[i + 1]), N)
    } else {
      a2 <- rep(0, N)
    }
    if (i < m + r - 1) {
      DX[, i] <- (r - 2) * (a1 * BB[, i, r - 2] - a2 * BB[, i + 1, r - 2])
    } else {
      DX[, i] <- (r - 2) * (a1 * BB[, i, r - 2])
    }
  }

  return(list(XX = XX, DX = DX))
}
