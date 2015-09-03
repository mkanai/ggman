#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix split_markername(std::vector<std::string> markers, int n = 2, char sep = ':') {
  int n_marker = markers.size();
  NumericMatrix ret(n_marker, n);

  for (int i = 0; i < n_marker; i++){
    size_t current = 0, found;
    for (int j = 0; j < n; j++) {
      if ((found = markers[i].find_first_of(sep, current)) != string::npos) {
        ret(i, j) = std::atoi(string(markers[i], current, found - current).c_str());
        current = found + 1;
      } else {
        ret(i, j) = std::atoi(string(markers[i], current, markers[i].size() - current).c_str());
        for (int k = j + 1; k < n; k++) {
          ret(i, k) = NA_REAL;
        }
        j = n;
      }
    }
  }

  return ret;
}
