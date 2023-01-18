#include <cpp11.hpp>

using namespace cpp11;

[[cpp11::register]]
logicals Cfirst_by(int n, integers rows) {
  int nrows = rows.size();
  integers grps = rows.attr("starts");
  R_xlen_t ngrps = grps.size();

  writable::logicals ret(n);

  for(int g=0; g<ngrps; g++) {
    int f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    int l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    for(int i = f; i < l; i++) {
      int r  = nrows == 0 ? i : rows[i] - 1;
      ret[r] = i == f;
    }
  }
  return ret;
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
