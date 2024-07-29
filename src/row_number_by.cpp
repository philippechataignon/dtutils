#include <Rcpp.h>
#include <omp.h>

using namespace Rcpp;
// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export]]
IntegerVector Crow_number_by(R_xlen_t n, IntegerVector rows) {
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  IntegerVector ret(n);

  #pragma omp parallel for
  for(int g=0; g<ngrps; g++) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    int num = 1;
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
      ret[r] = num++;
    }
  }
  return ret;
}
// [[Rcpp::export]]
LogicalVector Cfirst_by(R_xlen_t n, IntegerVector rows) {
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  LogicalVector ret(n);

  #pragma omp parallel for
  for(int g=0; g<ngrps; g++) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
      ret[r] = i == f;
    }
  }
  return ret;
}

// [[Rcpp::export]]
LogicalVector Clast_by(R_xlen_t n, IntegerVector rows) {
  R_xlen_t nrows = rows.size();

  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");

  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  LogicalVector ret(n);

  #pragma omp parallel for
  for(int g=0; g<ngrps; g++) {
    R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
      ret[r] = i == (l - 1);
    }
  }
  return ret;
}

// [[Rcpp::export]]
LogicalVector Cany_by(LogicalVector x, IntegerVector rows) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();
  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");
  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();
  LogicalVector ret(ngrps);

  #pragma omp parallel for
  for(int g=0; g<ngrps; g++) {
    // f = first index of group g
    R_xlen_t f = grps[g] - 1;
    // l = last index, n if last group
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1;
    ret[g] = false;
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
      if (x[r] == true) {
        ret[g] = true;
        break;
      }
    }
  }
  return ret;
}

// [[Rcpp::export]]
LogicalVector Call_by(LogicalVector x, IntegerVector rows) {
  R_xlen_t n = x.size();
  R_xlen_t nrows = rows.size();
  if (nrows != 0 && n != rows.size())
    stop("x and rows must have the same length");
  if (!rows.hasAttribute("starts"))
    stop("rows must have 'starts' attribute");
  IntegerVector grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();
  LogicalVector ret(ngrps);

  #pragma omp parallel for
  for(int g=0; g<ngrps; g++) {
    // f = first index of group g
    R_xlen_t f = grps[g] - 1;
    // l = last index, n if last group
    R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1;
    ret[g] = true;
    for(R_xlen_t i = f; i < l; i++) {
      R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
      if (LogicalVector::is_na(x[r]) || x[r] == false) {
        ret[g] = false;
        break;
      }
    }
  }
  return ret;
}
