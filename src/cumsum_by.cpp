#include <Rcpp.h>
using namespace Rcpp;

template<typename T>
T Ccumsum_type(T x, IntegerVector rows) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  T ret = clone(x);

  for(int g=0; g<ngrps; g++) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
      R_xlen_t r1 = nrows == 0 ? i - 1 : rows[i - 1] - 1;
      if (i == f) {
        ret[r] = x[i];
      } else {
        ret[r] = ret[r1] + x[i];
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
List Ccumsum_by(List x, IntegerVector rows) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Ccumsum_type<NumericVector>(as<NumericVector>(*it), rows);
    } else if(is<IntegerVector>(*it)){
      *it = Ccumsum_type<IntegerVector>(as<IntegerVector>(*it), rows);
    } else if(is<ComplexVector>(*it)){
      *it = Ccumsum_type<ComplexVector>(as<ComplexVector>(*it), rows);
    } else {
      stop("cumsum error: unimplemented type");
    }
  }
  return x;
}
