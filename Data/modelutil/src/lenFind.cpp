#include <Rcpp.h>
using namespace Rcpp;
// Data in must be of same length, even, and ordered
// Else, function will fail.

// [[Rcpp::export]]
NumericVector lenFind(NumericVector Time, NumericVector Infected) {
  int n = Time.size();
  int m = n / 2;
  NumericVector out(m);
  int i = 1;
  int it;

  for (it = 0; it < m; it++, i+=2)
    out[it] = Time[i] - Time[i - 1];

  return out;
}
