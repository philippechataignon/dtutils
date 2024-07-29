#include <Rcpp.h>
#include <omp.h>

using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]

template<typename T>
NumericVector Cweightedsum_t(T x, NumericVector wt, IntegerVector rows, bool na_rm) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (n != wt.size())
    stop("x and wt must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();
  NumericVector ret(ngrps);

  #pragma omp parallel for
  for(int g=0; g < ngrps; g++) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (indiceC = indiceR - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r = (nrows == 0) ? i : rows[i] - 1;
      if (T::is_na(wt[r])) {
        continue;
      } else if (T::is_na(x[r])) {
        if (na_rm) {
          continue;
        } else {
          ret[g] = NA_REAL;
          break;
        }
      } else {
        ret[g] += x[r] * wt[r];
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
DataFrame Cweightedsum(DataFrame x, NumericVector wt, IntegerVector rows, bool na_rm) {
  for(DataFrame::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)) {
      *it = Cweightedsum_t<NumericVector>(as<NumericVector>(*it), wt, rows, na_rm);
    } else if(is<IntegerVector>(*it)) {
      *it = Cweightedsum_t<IntegerVector>(as<IntegerVector>(*it), wt, rows, na_rm);
    } else if(is<LogicalVector>(*it)) {
      *it = Cweightedsum_t<LogicalVector>(as<LogicalVector>(*it), wt, rows, na_rm);
    } else {
      stop("weightedsum error: unimplemented type");
    }
  }
  return x;
}
