#include <Rcpp.h>
#define MAX_CHR 26

using namespace Rcpp;

// [[Rcpp::export("..convert2posX")]]
NumericVector convert2posX(NumericVector chr, NumericVector bp, NumericVector posX) {
  int n_marker = bp.size();
  NumericVector pos(n_marker);
  for (int i = 0; i < n_marker; i++) {
    pos[i] = bp[i] + posX[chr[i] - 1];
  }
  return pos;
}
