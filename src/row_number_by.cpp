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
