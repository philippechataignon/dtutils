#include "cpp11.hpp"
#include <vector>
#include <set>
using namespace cpp11;
namespace writable = cpp11::writable;

[[cpp11::register]]
list Crle_cpp(doubles x) {
  std::vector<int> lengths;
  std::vector<double> values;

  // Initialise first value
  int i = 0;
  double prev = x[0];
  values.push_back(prev);
  lengths.push_back(1);

  for(auto it = x.begin() + 1; it != x.end(); ++it) {
    if (prev == *it) {
      lengths[i]++;
    } else {
      values.push_back(*it);
      lengths.push_back(1);
      i++;
      prev = *it;
    }
  }
  return writable::list({
    "lengths"_nm = lengths,
    "values"_nm = values
  });
}

[[cpp11::register]]
logicals Cnfirst_by(strings x) {
  writable::logicals ret(x.size());
  std::set<std::string> values;
  r_string prev = x[0];
  values.insert(x[0]);
  ret[0] = TRUE;
  int i = 1;
  for(auto it = x.begin() + 1; it != x.end(); ++it) {
    if (prev != *it && values.count(*it) == 0) {
      values.insert(*it);
      ret[i] = TRUE;
      prev = *it;
    } else {
      ret[i] FALSE;
    }
    i++;
  }
  return ret;
}
