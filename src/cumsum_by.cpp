#include <Rcpp.h>
using namespace Rcpp;

template<typename T>
T Ccumope_type(T x, IntegerVector rows, int type) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  T ret = clone(x);
  T last_val(1);
  LogicalVector first(ngrps);
  for(int i=0; i < ngrps; i++) {
    first[i] = true;
  }
  R_xlen_t f, l, r, p;
  for(int g=0; g < ngrps; g++) {
    f = grps[g] - 1; // start indice of group g (indiceC = indiceR - 1)
    l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    for(R_xlen_t i = f; i < l; i++) {
      if (nrows == 0) {
        r = i;
        p = i - 1;
      } else {
        r = rows[i] - 1;
        if (i >= 1) {
          p = rows[i - 1] - 1;
        }
      }
      if (first[g]) {
        ret[r] = x[r];
        first[g] = false;
      } else {
          if (type == 1) {
            ret[r] = ret[p] + x[r];
        } else if (type == 2) {
            ret[r] = ret[p] * x[r];
        }
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
List Ccumope_by(List x, IntegerVector rows, int type) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Ccumope_type<NumericVector>(as<NumericVector>(*it), rows, type);
    } else if(is<IntegerVector>(*it)){
      *it = Ccumope_type<IntegerVector>(as<IntegerVector>(*it), rows, type);
    } else {
      stop("cumsum error: unimplemented type");
    }
  }
  return x;
}
