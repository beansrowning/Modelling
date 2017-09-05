#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector Croots(NumericVector x) {
  int n = x.size();
  LogicalVector out(n);
  int i;

  for (i = 0; i < n; i++)
    if (i == 0)
      if (x[i] > 0)
        out[i] = TRUE;
      else
        out[i] = FALSE;
    else if (x[i] > 0)
      if (x[i - 1] == 0)
        out[i] = TRUE;
      else
        out[i] = FALSE;
    else if (x[i - 1] > 0)
      out[i] = TRUE;
    else
      out[i] = FALSE;

  return out;
}

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

// [[Rcpp::export]]
bool epidemic(NumericMatrix x) {
  NumericVector Time = x( _ , 0 );
  NumericVector I = x( _ , 1 );
  LogicalVector idx = Croots(I);
  NumericVector outbreaks = lenFind(Time[idx], I[idx]);

  int idy = which_max(outbreaks);
  if (outbreaks[idy] >= 365) {
    return TRUE;
  } else {
    return FALSE;
  }
}
