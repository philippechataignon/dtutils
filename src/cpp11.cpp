// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// row_number_by.cpp
logicals Cfirst_by(int n, integers rows, integers grps);
extern "C" SEXP _dtutils_Cfirst_by(SEXP n, SEXP rows, SEXP grps) {
  BEGIN_CPP11
    return cpp11::as_sexp(Cfirst_by(cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<integers>>(rows), cpp11::as_cpp<cpp11::decay_t<integers>>(grps)));
  END_CPP11
}
// row_number_by.cpp
logicals Clast_by(int n, integers rows, integers grps);
extern "C" SEXP _dtutils_Clast_by(SEXP n, SEXP rows, SEXP grps) {
  BEGIN_CPP11
    return cpp11::as_sexp(Clast_by(cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<integers>>(rows), cpp11::as_cpp<cpp11::decay_t<integers>>(grps)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_dtutils_Cfirst_by", (DL_FUNC) &_dtutils_Cfirst_by, 3},
    {"_dtutils_Clast_by",  (DL_FUNC) &_dtutils_Clast_by,  3},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_dtutils(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
