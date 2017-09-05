#include <Rcpp.h>
#include <Croots.cpp>
#include <lenfind.cpp>

using namespace Rcpp;

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
