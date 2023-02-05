#include <cpp11.hpp>

// cpp11::writable::integers Cna_fill_type(cpp11::integers input, cpp11::integers rows, cpp11::integers starts, int type) {
//   cpp11::writable::integers out = input;
//   for (int i = 0; i < input.size(); i++) {
//     if (cpp11::is_na(input[i])) {
//       out[i] = 0;
//     } else {
//       out[i] = input[i];
//     }
//   }
//   return input;
// }

template<typename R, typename T, typename F>
R Cna_fill_type(T input, cpp11::integers rows, cpp11::integers starts, int type, F fill) {
  R out = input;
  for (int i = 0; i < input.size(); i++) {
    if (cpp11::is_na(input[i])) {
      out[i] = fill;
    } else {
      out[i] = input[i];
    }
  }
  return out;
}

//  } else {
//    for(int g=0; g < starts.size(); g++) {
//      R_xlen_t f = starts[g] - 1; // start indice of group g (C indice = R indice - 1)
//      R_xlen_t l = g == (starts.size() - 1) ? x.size() : starts[g + 1] - 1; // last indice (n if last group)
//
//      if (type == 4) {
//        for(R_xlen_t i = l - 1; i > f; i--) {
//          R_xlen_t r  = x.size() == 0 ? i : rows[i] - 1;
//          R_xlen_t r1 = x.size() == 0 ? i - 1 : rows[i - 1] - 1;
//          if(cpp11::is_na(ret[r1]) && !cpp11::is_na(ret[r])) {
//            ret[r1] = ret[r];
//          }
//        }
//      }
//      if (type == 1 || type== 3 || type == 4) {
//        for(R_xlen_t i = f + 1; i < l; i++) {
//          R_xlen_t r  = x.size() == 0 ? i : rows[i] - 1;
//          R_xlen_t r1 = x.size() == 0 ? i - 1 : rows[i - 1] - 1;
//          if(cpp11::is_na(ret[r]) && !cpp11::is_na(ret[r1])) {
//            ret[r] = ret[r1];
//          }
//        }
//      }
//
//      if (type == 2 || type== 3) {
//        for(R_xlen_t i = l - 1; i > f; i--) {
//          R_xlen_t r  = x.size() == 0 ? i : rows[i] - 1;
//          R_xlen_t r1 = x.size() == 0 ? i - 1 : rows[i - 1] - 1;
//          if(cpp11::is_na(ret[r1]) && !cpp11::is_na(ret[r])) {
//            ret[r1] = ret[r];
//          }
//        }
//      }
//    }
//  }

[[cpp11::register]]
cpp11::writable::list Cna_fill_by(cpp11::writable::list input,
                                  cpp11::integers rows, cpp11::integers starts, int type, cpp11::sexp fill) {
  for (auto col: input) {
    switch (TYPEOF(col)) {
      // case LGLSXP:
      //   col = Cna_fill_type<cpp11::writable::logicals>(cpp11::as_cpp<cpp11::logicals>(col), rows, starts, type);
      //   break;
      case INTSXP:
        col = Cna_fill_type<cpp11::writable::integers, cpp11::integers, int>(cpp11::as_cpp<cpp11::integers>(col), rows, starts, type, cpp11::as_cpp<int>(fill));
        break;
      case REALSXP:
        col = Cna_fill_type<cpp11::writable::doubles, cpp11::doubles, double>(cpp11::as_cpp<cpp11::doubles>(col), rows, starts, type, cpp11::as_cpp<double>(fill));
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