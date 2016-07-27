#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]

NumericVector swp(NumericVector d, NumericVector at, NumericVector pred){
  int pn = d[3];
  for (int i=0; i < pn; i++){
    pred[][][i] * at[i];
  }
  return pred;
}
