rm(list=ls())

library(Rcpp)


cppFunction('NumericVector matdim(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(2);
  out[0] = nrow;
  out[1] = ncol;
  return out;
}')

m<-matrix(1:90, 10,9)

matdim(m)

library(inline)

src<-'
  Rcpp::NumericMatrix inmat(A);
  Rcpp::NumericVector outvec = Rcpp::Dimension(inmat);
  return outvec;
'

fun<- cxxfunction(signature(A='numeric'), src, plugin='Rcpp')
fun(matrix(1:90, 10,9))



