#include "cpp11.hpp"
using namespace cpp11;
namespace writable = cpp11::writable;

[[cpp11::register]]
logicals Cfirst_by(int n, integers rows, integers grps) {
  writable::logicals ret(n);
  for(int g=0; g < grps.size(); g++) {
    int f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    int l = g == (grps.size() - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
    for(int i = f; i < l; i++) {
      int r  = rows.size() == 0 ? i : rows[i] - 1;
      ret[r] = i == f;
    }
  }
  return ret;
}
