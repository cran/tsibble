#include <Rcpp.h>
using namespace Rcpp;

// Lagged Differences
// [[Rcpp::export]]
NumericVector diff_cpp(NumericVector x, int lag, int differences, double fill) {
  int n = x.size();
  if (lag * differences >= n) {
    return(x[-1]);
  }

  NumericVector y(n);
  for (int i = 0; i < n; i++) {
    if (i < lag) {
      y[i] = fill;
    } else {
      y[i] = x[i] - x[i - lag];
    }
  }

  if (differences == 1) {
    return y;
  }

  return(diff_cpp(y, lag, differences - 1, fill));
}
