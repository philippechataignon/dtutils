#include <cpp11.hpp>

template<typename T>
T Cna_fill_type(T x, cpp11::integers rows, cpp11::integers starts, int type = 1) {
  // R_xlen_t n = x.size();
  // R_xlen_t nrows = rows.size();
  //
  // if (nrows != 0 && n != rows.size())
  //   stop("x and rows must have the same length");
  // if (!rows.hasAttribute("starts"))
  //   stop("rows must have 'starts' attribute");
  // if (fill.size() != 1)
  //   stop("fill must be a 1-length vector");
  //
  // IntegerVector grps = rows.attr("starts");
  // R_xlen_t ngrps = grps.size();
  //
  T ret = x;
  //
  // if (type == 0)
  // for(R_xlen_t i = 0; i<n; i++) {
  //   if(T::is_na(x[i])) {
  //     ret[i] = fill[0];
  //   }
  // } else {
  //   for(int g=0; g<ngrps; g++) {
  //     R_xlen_t f = grps[g] - 1; // start indice of group g (C indice = R indice - 1)
  //     R_xlen_t l = g == (ngrps - 1) ? n : grps[g + 1] - 1; // last indice (n if last group)
  //
  //     if (type == 4) {
  //       for(R_xlen_t i = l - 1; i > f; i--) {
  //         R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
  //         R_xlen_t r1 = nrows == 0 ? i - 1 : rows[i - 1] - 1;
  //         if(T::is_na(ret[r1]) && !T::is_na(ret[r])) {
  //           ret[r1] = ret[r];
  //         }
  //       }
  //     }
  //     if (type == 1 || type== 3 || type == 4) {
  //       for(R_xlen_t i = f + 1; i < l; i++) {
  //         R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
  //         R_xlen_t r1 = nrows == 0 ? i - 1 : rows[i - 1] - 1;
  //         if(T::is_na(ret[r]) && !T::is_na(ret[r1])) {
  //           ret[r] = ret[r1];
  //         }
  //       }
  //     }
  //
  //     if (type == 2 || type== 3) {
  //       for(R_xlen_t i = l - 1; i > f; i--) {
  //         R_xlen_t r  = nrows == 0 ? i : rows[i] - 1;
  //         R_xlen_t r1 = nrows == 0 ? i - 1 : rows[i - 1] - 1;
  //         if(T::is_na(ret[r1]) && !T::is_na(ret[r])) {
  //           ret[r1] = ret[r];
  //         }
  //       }
  //     }
  //   }
  // }
  return ret;
}

[[cpp11::register]]
cpp11::writable::list Cna_fill_by(cpp11::writable::list input,
                                  cpp11::integers rows, cpp11::integers starts, int type) {
  for (auto col: input) {
    switch (TYPEOF(col)) {
      case STRSXP:
        col = Cna_fill_type<cpp11::writable::strings>(cpp11::as_cpp<cpp11::strings>(col), rows, starts, type);
        break;
      case LGLSXP:
        col = Cna_fill_type<cpp11::writable::logicals>(cpp11::as_cpp<cpp11::logicals>(col), rows, starts, type);
        break;
      case REALSXP:
        col = Cna_fill_type<cpp11::writable::doubles>(cpp11::as_cpp<cpp11::doubles>(col), rows, starts, type);
        break;
      case INTSXP:
        col = Cna_fill_type<cpp11::writable::integers>(cpp11::as_cpp<cpp11::integers>(col), rows, starts, type);
        break;
      default:
        cpp11::stop("na_fill error: unimplemented type");
    }
  }
  return input;
}