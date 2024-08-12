#include "npiv.h"

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List npiv(const arma::mat& P, const arma::mat& B, const arma::vec& y) {
  // Compute intermediate matrices
  arma::mat BtB = B.t() * B;
  arma::mat BtB_pinv = arma::pinv(BtB);
  arma::mat PtB = P.t() * B;

  // Compute Q (corrected to match MATLAB version)
  arma::mat Q = arma::pinv(PtB * BtB_pinv * B.t() * P) * PtB * BtB_pinv;

  // Compute c
  arma::vec c = Q * B.t() * y;

  // Compute uhat
  arma::vec uhat = y - P * c;

  // Scale Q by length of y (to match MATLAB version)
  Q *= y.n_elem;

  // Prepare the return list
  return Rcpp::List::create(
    Rcpp::Named("c") = std::move(c),
    Rcpp::Named("uhat") = std::move(uhat),
    Rcpp::Named("Q") = std::move(Q)
  );
}
