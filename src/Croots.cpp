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
