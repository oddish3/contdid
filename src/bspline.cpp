#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
List bspline(NumericVector x, int l, int r) {
  int N = x.size();
  int m = std::pow(2, l) - 1;
  r = r + 1;

  // Define the augmented knot set
  std::vector<double> kts;
  if (l == 0) {
    kts.resize(2 * (r - 1), 0.0);
    std::fill(kts.begin() + (r - 1), kts.end(), 1.0);
  } else if (l >= 1) {
    kts.resize(2 * (r - 2) + m + 2, 0.0);
    for (int i = 0; i <= m; ++i) {
      kts[r - 2 + i] = static_cast<double>(i) / std::pow(2, l);
    }
    std::fill(kts.begin() + r - 2 + m + 1, kts.end(), 1.0);
  }

  // Initialize for recursion
  arma::cube BB(N, m + 2 * r - 2, r - 1, arma::fill::zeros);
  for (int i = 0; i < N; ++i) {
    for (int j = r - 1; j <= r + m - 1; ++j) {
      if (x[i] >= kts[j] && x[i] < kts[j + 1]) {
        BB(i, j - (r - 1), 0) = 1.0;
        break;
      }
    }
  }

  // Recursion
  for (int j = 1; j < r - 1; ++j) {
    for (int i = 0; i < m + 2 * r - 2 - j; ++i) {
      arma::vec a1(N, arma::fill::zeros);
      if (kts[i + j] - kts[i] != 0) {
        a1 = (x - kts[i]) / (kts[i + j] - kts[i]);
      }

      arma::vec a2(N, arma::fill::zeros);
      if (kts[i + j + 1] - kts[i + 1] != 0) {
        a2 = (x - kts[i + 1]) / (kts[i + j + 1] - kts[i + 1]);
      }

      BB.slice(j).col(i) = a1 % BB.slice(j-1).col(i) + (1 - a2) % BB.slice(j-1).col(i+1);
    }
  }

  arma::mat XX = BB.slice(r - 2).cols(0, std::pow(2, l) + r - 3);

  // Calculate derivative
  arma::mat DX(N, m + r - 1, arma::fill::zeros);
  for (int i = 0; i < m + r - 1; ++i) {
    arma::vec a1(N, arma::fill::zeros);
    if (kts[i + r - 2] - kts[i] != 0) {
      a1.fill(1.0 / (kts[i + r - 2] - kts[i]));
    }

    arma::vec a2(N, arma::fill::zeros);
    if (kts[i + r - 1] - kts[i + 1] != 0) {
      a2.fill(1.0 / (kts[i + r - 1] - kts[i + 1]));
    }

    if (i < m + r - 2) {
      DX.col(i) = (r - 2) * (a1 % BB.slice(r - 3).col(i) - a2 % BB.slice(r - 3).col(i + 1));
    } else {
      DX.col(i) = (r - 2) * (a1 % BB.slice(r - 3).col(i));
    }
  }

  return List::create(Named("XX") = XX, Named("DX") = DX);
}
