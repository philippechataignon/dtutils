#include <Rcpp.h>
using namespace Rcpp;

template<typename T>
T Cna_fill_type(T x, IntegerVector rows, unsigned int type, bool inplace, T na) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  T ret = inplace ? x : clone(x);

  for(int g=0; g<ngrps; g++) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)

    if (type == 1 || type== 3) {
      for(R_xlen_t i = f + 1; i < l; i++) {
        R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
        R_xlen_t r1 = nrows == 0 ? i - 1 : rows[i - 1] - 1;
        if(T::is_na(ret[r]) && !T::is_na(ret[r1])) {
          ret[r] = ret[r1];
        }
      }
    }

    if (type == 2 || type== 3) {
      for(R_xlen_t i = l - 1; i > f; i--) {
        R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
        R_xlen_t r1 = nrows == 0 ? i - 1 : rows[i - 1] - 1;
        if(T::is_na(ret[r1]) && !T::is_na(ret[r])) {
          ret[r1] = ret[r];
        }
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
List Cna_fill_by(List x, IntegerVector rows, unsigned int type, bool inplace) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Cna_fill_type<NumericVector>(as<NumericVector>(*it), rows, type, inplace, NumericVector::create(NA_REAL));
    } else if(is<IntegerVector>(*it)){
      *it = Cna_fill_type<IntegerVector>(as<IntegerVector>(*it), rows, type, inplace, IntegerVector::create(NA_INTEGER));
    } else if(is<StringVector>(*it)){
      *it = Cna_fill_type<StringVector> (as<StringVector>(*it),  rows, type, inplace, CharacterVector::create(NA_STRING));
    } else if(is<LogicalVector>(*it)){
      *it = Cna_fill_type<LogicalVector>(as<LogicalVector>(*it), rows, type, inplace, LogicalVector::create(NA_LOGICAL));
    } else {
      stop("na_fill error: unimplemented type");
    }
  }
  return x;
}
