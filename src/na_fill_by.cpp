#include <Rcpp.h>
using namespace Rcpp;

template<typename T>
T Cna_replace_t(T x, T fill, bool inplace = false) {
  if (fill.size() != 1)
    stop("fill must be a 1-length vector");

  T ret = inplace ? x : clone(x);

  #pragma omp parallel for
  for(R_xlen_t i = 0; i < x.size(); ++i) {
    if(T::is_na(x[i])) {
      ret[i] = fill[0];
    }
  }
  return ret;
}

template<typename T>
T Cna_fill_t(T x, IntegerVector rows, unsigned int type = 1, bool inplace = false) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();
  T ret = inplace ? x : clone(x);

  if (type == 1 || type== 3) {
    #pragma omp parallel for
    for(int g = 0; g < ngrps; ++g) {
      R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
      R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
      for(R_xlen_t i = f + 1; i < l; ++i) {
        R_xlen_t r = nrows == 0 ? i : rows[i] - 1;
        R_xlen_t rs = nrows == 0 ? i - 1: rows[i - 1] - 1;
        if (T::is_na(ret[r])) {
          ret[r] = ret[rs];
        } else {
          ret[r] = x[r];
        }
      }
    }
  }

  if (type == 2 || type== 3) {
    #pragma omp parallel for
    for(int g = 0; g < ngrps; ++g) {
      R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
      R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
      for(R_xlen_t i = l - 2; i >= f; --i) {
        R_xlen_t r = nrows == 0 ? i : rows[i] - 1;
        R_xlen_t rs = nrows == 0 ? i + 1: rows[i + 1] - 1;
        if (T::is_na(ret[r])) {
          ret[r] = ret[rs];
        } else {
          if (type == 2) {
            ret[r] = x[r];
          }
        }
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
List Cna_fill_by(List x, IntegerVector rows, unsigned int type = 1, bool inplace = false) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Cna_fill_t<NumericVector>(as<NumericVector>(*it), rows, type, inplace);
    } else if(is<IntegerVector>(*it)){
      *it = Cna_fill_t<IntegerVector>(as<IntegerVector>(*it), rows, type, inplace);
    } else if(is<StringVector>(*it)){
      *it = Cna_fill_t<StringVector> (as<StringVector>(*it),  rows, type, inplace);
    } else if(is<LogicalVector>(*it)){
      *it = Cna_fill_t<LogicalVector>(as<LogicalVector>(*it), rows, type, inplace);
    } else if(is<ComplexVector>(*it)){
      *it = Cna_fill_t<ComplexVector>(as<ComplexVector>(*it), rows, type, inplace);
    } else {
      stop("na_fill error: unimplemented type");
    }
  }
  return x;
}

// [[Rcpp::export]]
List Cna_replace(List x, RObject fill = R_NilValue, bool inplace=false) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Cna_replace_t<NumericVector>(as<NumericVector>(*it), as<NumericVector>(fill), inplace);
    } else if(is<IntegerVector>(*it)){
      *it = Cna_replace_t<IntegerVector>(as<IntegerVector>(*it), as<IntegerVector>(fill), inplace);
    } else if(is<StringVector>(*it)){
      *it = Cna_replace_t<StringVector> (as<StringVector>(*it),  as<StringVector>(fill), inplace);
    } else if(is<LogicalVector>(*it)){
      *it = Cna_replace_t<LogicalVector>(as<LogicalVector>(*it), as<LogicalVector>(fill), inplace);
    } else if(is<ComplexVector>(*it)){
      *it = Cna_replace_t<ComplexVector>(as<ComplexVector>(*it), as<ComplexVector>(fill), inplace);
    } else {
      stop("na_replace error: unimplemented type");
    }
  }
  return x;
}
