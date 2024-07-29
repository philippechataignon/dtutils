#include <Rcpp.h>
#include <omp.h>

using namespace Rcpp;
// [[Rcpp::plugins(openmp)]]

template<typename T>
T Cshift_t(T x, IntegerVector rows, int shift, T fill, bool inplace = false) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();
  bool is_lead = (shift > 0);
  bool is_lag = (shift < 0);
  if (is_lag) {
      shift = -shift;
  }

  T ret = inplace ? x : clone(x);

  #pragma omp parallel for
  for(int g = 0; g < ngrps; ++g) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    if (is_lead) {
      for(R_xlen_t i = f; i < l; ++i) {
        R_xlen_t r = nrows == 0 ? i : rows[i] - 1;
        if (i < l - shift) {
          R_xlen_t rs = nrows == 0 ? i + shift: rows[i + shift] - 1;
          ret[r] = x[rs];
        } else {
          ret[r] = fill[0];
        }
      }
    } else if (is_lag) {
      for(R_xlen_t i = l - 1; i >= f; --i) {
        R_xlen_t r = nrows == 0 ? i : rows[i] - 1;
        if (i >= f + shift) {
          R_xlen_t rs = nrows == 0 ? i - shift: rows[i - shift] - 1;
          ret[r] = x[rs];
        } else {
          ret[r] = fill[0];
        }
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
List Cshift_by(List x, IntegerVector rows, int n, RObject fill = R_NilValue, bool inplace = false) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Cshift_t<NumericVector>(as<NumericVector>(*it), rows, n, as<NumericVector>(fill), inplace);
    } else if(is<IntegerVector>(*it)){
      *it = Cshift_t<IntegerVector>(as<IntegerVector>(*it), rows, n, as<IntegerVector>(fill), inplace);
    } else if(is<StringVector>(*it)){
      *it = Cshift_t<StringVector> (as<StringVector>(*it),  rows, n, as<StringVector>(fill), inplace);
    } else if(is<LogicalVector>(*it)){
      *it = Cshift_t<LogicalVector>(as<LogicalVector>(*it), rows, n, as<LogicalVector>(fill), inplace);
    } else if(is<ComplexVector>(*it)){
      *it = Cshift_t<ComplexVector>(as<ComplexVector>(*it), rows, n, as<ComplexVector>(fill), inplace);
    } else {
      stop("shift error: unimplemented type");
    }
  }
  return x;
}

