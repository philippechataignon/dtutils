#include <Rcpp.h>
using namespace Rcpp;

template<typename T>
T Ccumprod_type(T x, IntegerVector rows) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  T ret = clone(x);
  T na(1);
  bool copy_na = false;

  for(int g=0; g<ngrps; g++) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
      R_xlen_t r1 = nrows == 0 ? i - 1 : rows[i - 1] - 1;
      if(T::is_na(x[i])) {
        ret[r] = x[i];
        na[0] = x[i];
        copy_na = true;
      } else if (i == f) {
        ret[r] = x[i];
        copy_na = false;
      } else if (copy_na) {
        ret[r] = na[0];
      } else {
        ret[r] = ret[r1] * x[i];
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
List Ccumprod_by(List x, IntegerVector rows) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Ccumprod_type<NumericVector>(as<NumericVector>(*it), rows);
    } else if(is<IntegerVector>(*it)){
      *it = Ccumprod_type<IntegerVector>(as<IntegerVector>(*it), rows);
    } else if(is<ComplexVector>(*it)){
      *it = Ccumprod_type<ComplexVector>(as<ComplexVector>(*it), rows);
    } else {
      stop("cumprod error: unimplemented type");
    }
  }
  return x;
}
