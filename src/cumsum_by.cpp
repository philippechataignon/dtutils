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
  T na(1);
  T cum(1);
  bool copy_na = false;

  for(int g=0; g<ngrps; g++) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
      if(T::is_na(x[i])) {
        ret[r] = x[i];
        na[0] = x[i];
        copy_na = true;
      } else if (i == f) {
        if (type == 5) {
          cum[0] = 1 - x[i];
        } else {
          cum[0] = x[i];
        }
        ret[r] = x[i];
        copy_na = false;
      } else if (copy_na) {
        ret[r] = na[0];
      } else {
        if (type == 1) {
          cum[0] = cum[0] + x[i];
        } else if (type == 2) {
          cum[0] = cum[0] * x[i];
        } else if (type == 3) {
          if (x[i] > cum[0]) {
            cum[0] = x[i];
          }
        } else if (type == 4) {
          if (x[i] < cum[0]) {
            cum[0] = x[i];
          }
        } else if (type == 5) {
          cum[0] = cum[0] * (1 - x[i]);
        }
        if (type == 5) {
          ret[r] = 1 - cum[0];
        } else {
          ret[r] = cum[0];
        }
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
List Ccumsum_by(List x, IntegerVector rows) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Ccumope_type<NumericVector>(as<NumericVector>(*it), rows, 1);
    } else if(is<IntegerVector>(*it)){
      *it = Ccumope_type<IntegerVector>(as<IntegerVector>(*it), rows, 1);
    } else {
      stop("cumsum error: unimplemented type");
    }
  }
  return x;
}
// [[Rcpp::export]]
List Ccumprod_by(List x, IntegerVector rows) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Ccumope_type<NumericVector>(as<NumericVector>(*it), rows, 2);
    } else if(is<IntegerVector>(*it)){
      *it = Ccumope_type<IntegerVector>(as<IntegerVector>(*it), rows, 2);
    } else {
      stop("cumprod error: unimplemented type");
    }
  }
  return x;
}

// [[Rcpp::export]]
List Ccummax_by(List x, IntegerVector rows) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Ccumope_type<NumericVector>(as<NumericVector>(*it), rows, 3);
    } else if(is<IntegerVector>(*it)){
      *it = Ccumope_type<IntegerVector>(as<IntegerVector>(*it), rows, 3);
    } else {
      stop("cummax error: unimplemented type");
    }
  }
  return x;
}
// [[Rcpp::export]]
List Ccummin_by(List x, IntegerVector rows) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Ccumope_type<NumericVector>(as<NumericVector>(*it), rows, 4);
    } else if(is<IntegerVector>(*it)){
      *it = Ccumope_type<IntegerVector>(as<IntegerVector>(*it), rows, 4);
    } else {
      stop("cummin error: unimplemented type");
    }
  }
  return x;
}
// [[Rcpp::export]]
List Ccumsurv_by(List x, IntegerVector rows) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Ccumope_type<NumericVector>(as<NumericVector>(*it), rows, 5);
    } else if(is<IntegerVector>(*it)){
      *it = Ccumope_type<IntegerVector>(as<IntegerVector>(*it), rows, 5);
    } else {
      stop("cumsurv error: unimplemented type");
    }
  }
  return x;
}