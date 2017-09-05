#include <Rcpp.h>
using namespace Rcpp;

// Subsetting routine
LogicalVector subset(NumericVector iter, int num) {
  int n = iter.size();
  LogicalVector idx(n);
  for (int i = 0; i < n; i++) {
    idx[i] = (iter[i] == num);
  }
  return idx;
}
// Same outbreak length finder routine as last time
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
NumericVector epidemicDensity(DataFrame x) {
  NumericVector Time = x["time"];
  NumericVector iter = x["iter"];
  NumericVector I = x["I"];
  // NumericVector runs = tail(iter, 1);
  NumericVector tot;
  // double count;

  for (int i = 1; i < 10000; i++) {
    LogicalVector set = subset(iter, i);
    NumericVector outbreaks = lenFind(Time[set], I[set]);
    int maxidx = which_max(outbreaks);
    int maxVal = outbreaks[maxidx];
    if (maxVal >= 365) {
      count++;
    }
  }
  // proportion of runs at each point which had outbreaks > 365 days
  return (count / as<double>(runs));

}
