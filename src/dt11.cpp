#include "cpp11.hpp"
using namespace cpp11;
namespace writable = cpp11::writable;

[[cpp11::register]]
logicals Cfirst_by(int n, integers rows, integers grps) {
  int nrows = rows.size();
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
}
