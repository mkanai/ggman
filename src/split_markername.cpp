#include <Rcpp.h>

using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericMatrix split_markername(std::vector<std::string> markers, int n = 2, char sep = ':', bool replaceXYMT = false) {
  int n_marker = markers.size();
  NumericMatrix ret(n_marker, n);

  for (int i = 0; i < n_marker; i++){
    size_t current = 0, found;
    for (int j = 0; j < n; j++) {
      if ((found = markers[i].find_first_of(sep, current)) != std::string::npos) {
        std::string t = std::string(markers[i], current, found - current);
        if (replaceXYMT && j == 0) {
          if (t == "X") {
            ret(i, j) = 23;
          } else if (t == "Y") {
            ret(i, j) = 24;
          } else if (t == "XY") {
            ret(i, j) = 25;
          } else if (t == "MT") {
            ret(i, j) = 26;
          }
        } else {
          ret(i, j) = std::atoi(t.c_str());
        }
        current = found + 1;
      } else {
        ret(i, j) = std::atoi(std::string(markers[i], current, markers[i].size() - current).c_str());
        for (int k = j + 1; k < n; k++) {
          ret(i, k) = NA_REAL;
        }
        j = n;
      }
    }
  }

  return ret;
}
