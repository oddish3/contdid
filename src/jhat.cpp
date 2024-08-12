#include <RcppArmadillo.h>
#include "shat.h"
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// Helper function to print matrix summary
void print_matrix_summary(const arma::mat& matrix, const std::string& name) {
  // Rcpp::Rcout << "Matrix " << name << " dimensions: " << matrix.n_rows << "x" << matrix.n_cols << std::endl;
  // Rcpp::Rcout << "First few elements of " << name << ":" << std::endl;
  for (uword i = 0; i < std::min(10u, matrix.n_rows); ++i) {
    for (uword j = 0; j < std::min(5u, matrix.n_cols); ++j) {
      Rcpp::Rcout << std::setprecision(10) << std::setw(15) << matrix(i, j) << " ";
    }
    Rcpp::Rcout << std::endl;
  }
  Rcpp::Rcout << std::endl;
}

// [[Rcpp::export]]
Rcpp::List jhat(const arma::mat& PP, const arma::mat& BB,
                const arma::vec& CJ, const arma::vec& CK,
                const arma::vec& TJ, double M, int n, int nL) {
  arma::vec lb(nL + 1);
  arma::vec ub(nL + 1);

  for (int ll = 1; ll <= nL + 1; ++ll) {
    double s;
    try {
      arma::mat P_sub = PP.cols(CJ(ll-1), CJ(ll) - 1);
      arma::mat B_sub = BB.cols(CK(ll-1), CK(ll) - 1);
      s = shat(P_sub, B_sub);
    } catch (const std::exception& e) {
      Rcpp::warning("Exception caught in jhat: %s", e.what());
      s = 1e-20;
    }
    double J = TJ(ll-1);
    lb(ll-1) = J * std::sqrt(std::log(J)) * std::max(0.0, 1.0 / s);
  }

  ub.head(nL) = lb.subvec(1, nL);
  ub(nL) = arma::datum::inf;

  double threshold = 2 * M * std::sqrt(static_cast<double>(n));

  arma::uvec L = arma::find(lb <= threshold && threshold <= ub);

  int LL;
  int flag;

  if (!L.empty()) {
    LL = L(0);
    flag = 0;
  } else {
    L = arma::find(lb <= threshold);
    if (!L.empty()) {
      LL = L.back();
      flag = 1;
    } else {
      LL = 0;
      flag = 2;
    }
  }

  LL = std::max(LL, 1);

  return Rcpp::List::create(
    Rcpp::Named("LL") = LL,
    Rcpp::Named("flag") = flag
  );
}
