#include "cpp11.hpp"
using namespace cpp11;
namespace writable = cpp11::writable;

[[cpp11::register]]
logicals Cfirst_by(int n, integers rows, integers grps) {
  writable::logicals ret(n);
  for(int i=0; i < ret.size(); i++) {
    ret[i] = FALSE;
  }
  for(int g=0; g < grps.size(); g++) {
    int f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
    ret[f] = TRUE;
  }
  return ret;
}

[[cpp11::register]]
logicals Clast_by(int n, integers rows, integers grps) {
  writable::logicals ret(n);
  for(int i=0; i < ret.size(); i++) {
    ret[i] = FALSE;
  }
  for(int g=0; g < grps.size(); g++) {
    int l = g == (grps.size() - 1) ? n : grps[g + 1] - 1;
    ret[l - 1] = TRUE;
  }
  return ret;
}

[[cpp11::register]]
integers Crow_number_by(int n, integers rows, integers grps) {
  writable::integers ret(n);
  int nrows = rows.size();

  for(int g=0; g < grps.size(); g++) {
    int f = grps[g] - 1;
    int l = g == (grps.size() - 1) ? n : grps[g + 1] - 1;
    int num = 1;
    for(int i = f; i < l; i++) {
      int r  = nrows == 0 ? i : rows[i] - 1;
      ret[r] = num++;
    }
  }
  return ret;
}