#include <Rcpp.h>
using namespace Rcpp;

template<typename T>
T Cshift_t(T x, IntegerVector rows, bool inplace = false) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  T ret = inplace ? x : clone(x);

  for(int g = 0; g < ngrps; g++) {
    T last_val(1);
    T tmp_val(1);
    R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    last_val[0] = T::get_na();
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r = nrows == 0 ? i : rows[i] - 1;
      tmp_val[0] = x[r];
      ret[r] = last_val[0];
      last_val[0] = tmp_val[0];
    }
  }
  return ret;
}

// [[Rcpp::export]]
List Cshift_by(List x, IntegerVector rows, bool inplace = false) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Cshift_t<NumericVector>(as<NumericVector>(*it), rows, inplace);
    } else if(is<IntegerVector>(*it)){
      *it = Cshift_t<IntegerVector>(as<IntegerVector>(*it), rows, inplace);
    } else if(is<StringVector>(*it)){
      *it = Cshift_t<StringVector> (as<StringVector>(*it),  rows, inplace);
    } else if(is<LogicalVector>(*it)){
      *it = Cshift_t<LogicalVector>(as<LogicalVector>(*it), rows, inplace);
    } else if(is<ComplexVector>(*it)){
      *it = Cshift_t<ComplexVector>(as<ComplexVector>(*it), rows, inplace);
    } else {
      stop("shift error: unimplemented type");
    }
  }
  return x;
}
