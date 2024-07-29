#include <Rcpp.h>
#include <omp.h>

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

  T ret(x.size());
  #pragma omp parallel for
  for(int g=0; g < ngrps; g++) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (indiceC = indiceR - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    bool first_g = true;
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r = (nrows == 0) ? i : rows[i] - 1;
      if (T::is_na(x[r])) {
        if (i == f) {
          ret[r] = 0;
        } else {
          R_xlen_t r1 = (nrows == 0) ? i - 1: rows[i - 1] - 1;
          ret[r] = ret[r1];
        }
      } else if (first_g) {
        ret[r] = x[r];
        first_g = false;
      } else {
          R_xlen_t r1 = (nrows == 0) ? i - 1: rows[i - 1] - 1;
          if (type == 1) {
            ret[r] = ret[r1] + x[r];
        } else if (type == 2) {
            ret[r] = ret[r1] * x[r];
        } else if (type == 3) {
            ret[r] = x[r] < ret[r1] ? x[r] : ret[r1];
        } else if (type == 4) {
            ret[r] = x[r] > ret[r1] ? x[r] : ret[r1];
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
    } else if(is<LogicalVector>(*it)){
      *it = Ccumope_type<LogicalVector>(as<LogicalVector>(*it), rows, type);
    } else {
      stop("cumsum error: unimplemented type");
    }
  }
  return x;
}
