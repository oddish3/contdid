#include <RcppArmadillo.h>
#include "shat.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// Helper function to print matrix summary
void print_matrix_summary(const arma::mat& matrix, const std::string& name) {
  // Rcpp::Rcout << "Matrix " << name << " dimensions: " << matrix.n_rows << "x" << matrix.n_cols << std::endl;
  // Rcpp::Rcout << "First few elements of " << name << ":" << std::endl;
  for (arma::uword i = 0; i < std::min(static_cast<arma::uword>(10), matrix.n_rows); ++i) {
    for (arma::uword j = 0; j < std::min(static_cast<arma::uword>(5), matrix.n_cols); ++j) {
      Rcpp::Rcout << std::setprecision(10) << std::setw(15) << matrix(i, j) << " ";
    }
    Rcpp::Rcout << std::endl;
  }
  Rcpp::Rcout << std::endl;
}

// [[Rcpp::export]]
Rcpp::List jhat(const arma::mat& PP, const arma::mat& BB,
                const arma::vec& CJ, const arma::vec& CK,
                const arma::vec& TJ, double M, arma::uword n, arma::uword nL) {
  // Rcpp::Rcout << "Entering jhat function" << std::endl;
  // Rcpp::Rcout << "Input dimensions: PP(" << PP.n_rows << "x" << PP.n_cols << "), BB(" << BB.n_rows << "x" << BB.n_cols << ")" << std::endl;
  // Rcpp::Rcout << "CJ size: " << CJ.n_elem << ", CK size: " << CK.n_elem << ", TJ size: " << TJ.n_elem << std::endl;
  // Rcpp::Rcout << "M: " << M << ", n: " << n << ", nL: " << nL << std::endl;

  arma::vec lb(nL + 1);
  arma::vec ub(nL + 1);

  for (arma::uword ll = 1; ll <= nL + 1; ++ll) {
    double s;
    try {
      arma::uword start_col = static_cast<arma::uword>(CJ(ll-1));
      arma::uword end_col = static_cast<arma::uword>(CJ(ll) - 1);
      arma::mat P_sub = PP.cols(start_col, end_col);
      start_col = static_cast<arma::uword>(CK(ll-1));
      end_col = static_cast<arma::uword>(CK(ll) - 1);
      arma::mat B_sub = BB.cols(start_col, end_col);

      // Rcpp::Rcout << "Submatrix dimensions for ll=" << ll << ": P_sub(" << P_sub.n_rows << "x" << P_sub.n_cols
                  // << "), B_sub(" << B_sub.n_rows << "x" << B_sub.n_cols << ")" << std::endl;

      s = shat(P_sub, B_sub);
      // Rcpp::Rcout << "shat result for ll=" << ll << ": " << s << std::endl;
    } catch (const std::exception& e) {
      Rcpp::warning("Exception caught in jhat for ll=%d: %s", ll, e.what());
      s = 1e-20;
    }
    double J = TJ(ll-1);
    lb(ll-1) = J * std::sqrt(std::log(J)) * std::max(0.0, 1.0 / s);
  }

  ub.head(nL) = lb.subvec(1, nL);
  ub(nL) = arma::datum::inf;

  double threshold = 2 * M * std::sqrt(static_cast<double>(n));
  // Rcpp::Rcout << "Threshold: " << threshold << std::endl;

  arma::uvec L = arma::find(lb <= threshold && threshold <= ub);
  arma::uword LL;
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

  LL = std::max(LL, static_cast<arma::uword>(1));

  // Rcpp::Rcout << "Final LL: " << LL << ", flag: " << flag << std::endl;
  // Rcpp::Rcout << "Exiting jhat function" << std::endl;

  return Rcpp::List::create(
    Rcpp::Named("LL") = LL,
    Rcpp::Named("flag") = flag
  );
}
