#include <cpp11.hpp>

template<typename T>
T Ctest_t(T input)
{
  T ret = input;
  return ret;
}

[[cpp11::register]]
cpp11::writable::list Ctest(
    cpp11::writable::list input
)
{
  for (auto col: input) {
    switch (TYPEOF(col)) {
      case LGLSXP:
        col = Ctest_t<cpp11::writable::logicals>(cpp11::as_cpp<cpp11::logicals>(col));
        break;
      case INTSXP:
        col = Ctest_t<cpp11::writable::integers>(cpp11::as_cpp<cpp11::integers>(col));
        break;
      case REALSXP:
        col = Ctest_t<cpp11::writable::doubles>(cpp11::as_cpp<cpp11::doubles>(col));
        break;
      case STRSXP:
        col = Ctest_t<cpp11::writable::strings>(cpp11::as_cpp<cpp11::strings>(col));
        break;
      default:
        cpp11::stop("na_fill error: unimplemented type");
    }
  }
  return input;
}