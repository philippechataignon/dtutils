#include <Rcpp.h>
#include <omp.h>

using namespace Rcpp;

template<typename T>
T Ccoalesce_type(T x, IntegerVector rows) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  T ret(ngrps);

  #pragma omp parallel for
  for(int g=0; g<ngrps; g++) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    // init return value with NA
    ret[g] = T::get_na();
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
      if(!T::is_na(x[r])) {
        ret[g] = x[r];
        break;
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
List Ccoalesce_by(List x, IntegerVector rows, CharacterVector names) {
  for(List::iterator it = x.begin(); it != x.end(); ++it) {
    if(is<NumericVector>(*it)){
      *it = Ccoalesce_type<NumericVector>(as<NumericVector>(*it), rows);
    } else if(is<IntegerVector>(*it)){
      *it = Ccoalesce_type<IntegerVector>(as<IntegerVector>(*it), rows);
    } else if(is<StringVector>(*it)){
      *it = Ccoalesce_type<StringVector> (as<StringVector> (*it), rows);
    } else if(is<LogicalVector>(*it)){
      *it = Ccoalesce_type<LogicalVector>(as<LogicalVector>(*it), rows);
    } else if(is<ComplexVector>(*it)){
      *it = Ccoalesce_type<ComplexVector>(as<ComplexVector>(*it), rows);
    } else {
      stop("coalesce_list error: unimplemented type");
    }
  }
  x.names() = names;
  return x;
}
