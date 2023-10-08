#include <Rcpp.h>
using namespace Rcpp;

template<typename T>
T Cna_fill_type(T x, IntegerVector rows, T fill, unsigned int type = 1, bool inplace = false) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");
  if (fill.size() != 1)
    stop("fill must be a 1-length vector");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  T ret = inplace ? x : clone(x);

  if (type == 0) {
    for(R_xlen_t i = 0; i<n; i++) {
      if(T::is_na(x[i])) {
        ret[i] = fill[0];
      }
    }
  } else {
    // type 1, 2, 3, 4
    T last_val(1);
    for(int g = 0; g < ngrps; g++) {
      R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
      R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)

      if (type == 4) {
        last_val[0] = T::get_na();
        for(R_xlen_t i = l - 1; i >= f; i--) {
          R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
          if (T::is_na(ret[r])) {
            ret[r] = last_val[0];
          } else {
            last_val[0] = x[r];
          }
        }
      }
      if (type == 1 || type== 3 || type == 4) {
        last_val[0] = T::get_na();
        for(R_xlen_t i = f; i < l; i++) {
          R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
          if (T::is_na(ret[r])) {
            ret[r] = last_val[0];
          } else {
            last_val[0] = x[r];
          }
        }
      }
      if (type == 2 || type== 3) {
        last_val[0] = T::get_na();
        for(R_xlen_t i = l - 1; i >= f; i--) {
          R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
          if (T::is_na(ret[r])) {
            ret[r] = last_val[0];
          } else {
            last_val[0] = x[r];
          }
        }
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
List Cna_fill_by(List x, IntegerVector rows, unsigned int type = 1, bool inplace = false, RObject fill = R_NilValue) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Cna_fill_type<NumericVector>(as<NumericVector>(*it), rows, as<NumericVector>(fill), type, inplace);
    } else if(is<IntegerVector>(*it)){
      *it = Cna_fill_type<IntegerVector>(as<IntegerVector>(*it), rows, as<IntegerVector>(fill), type, inplace);
    } else if(is<StringVector>(*it)){
      *it = Cna_fill_type<StringVector> (as<StringVector>(*it),  rows, as<StringVector>( fill), type, inplace);
    } else if(is<LogicalVector>(*it)){
      *it = Cna_fill_type<LogicalVector>(as<LogicalVector>(*it), rows, as<LogicalVector>(fill), type, inplace);
    } else if(is<ComplexVector>(*it)){
      *it = Cna_fill_type<ComplexVector>(as<ComplexVector>(*it), rows, as<ComplexVector>(fill), type, inplace);
    } else {
      stop("na_fill error: unimplemented type");
    }
  }
  return x;
}
