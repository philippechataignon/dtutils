#include <cpp11.hpp>

template<typename R, typename T, typename B>
R Cna_fill_type(T input, cpp11::integers rows, cpp11::integers starts, int type, B na) {
  R ret = input;
  B tmp;
  R_xlen_t n = input.size();

  for(int g = 0; g < starts.size(); g++) {
    R_xlen_t f = starts[g] - 1;
    R_xlen_t l = g == (starts.size() - 1) ? n : starts[g + 1] - 1;

    if (type == 1 || type== 3) {
      tmp = na;
      for(R_xlen_t i = f; i < l; i++) {
        R_xlen_t r = n == 0 ? i : rows[i] - 1;
        if(cpp11::is_na(input[r])) {
          ret[r] = tmp;
        } else {
          tmp = ret[r];
        }
      }
    }

    if (type == 2 || type== 3) {
      tmp = na;
      for(R_xlen_t i = l - 1; i >= f; i--) {
        R_xlen_t r = n == 0 ? i : rows[i] - 1;
        if(cpp11::is_na(input[r])) {
          ret[r] = tmp;
        } else {
          tmp = ret[r];
        }
      }
    }
  }
  return ret;
}

[[cpp11::register]]
cpp11::writable::list Cna_fill_by(cpp11::writable::list input,
                                  cpp11::integers rows,
                                  cpp11::integers starts,
                                  int type) {
  for (auto col: input) {
    switch (TYPEOF(col)) {
      // case LGLSXP:
      //   col = Cna_fill_type<cpp11::writable::logicals>(cpp11::as_cpp<cpp11::logicals>(col), rows, starts, type);
      //   break;
      case INTSXP:
        col = Cna_fill_type<cpp11::writable::integers, cpp11::integers, int>(
          cpp11::as_cpp<cpp11::integers>(col),
          rows,
          starts,
          type,
          NA_INTEGER
        );
        break;
      case REALSXP:
        col = Cna_fill_type<cpp11::writable::doubles, cpp11::doubles, double>(
          cpp11::as_cpp<cpp11::doubles>(col),
          rows,
          starts,
          type,
          NA_REAL
        );
        break;
      // case STRSXP:
      //   col = Cna_fill_type<cpp11::writable::strings>(cpp11::as_cpp<cpp11::strings>(col), rows, starts, type);
      //   break;
      // default:
      //   cpp11::stop("na_fill error: unimplemented type");
    }
  }
  return input;
}