#include "sonar.h"

#include <algorithm>
#include <fstream>
#include <iterator>
#include <numeric>

namespace sonar {

std::vector<unsigned> parse_file(std::filesystem::path filename) {
  std::ifstream file{filename};
  return parse_input(file);
}

std::vector<unsigned> parse_input(std::istream &input) {
  std::vector<unsigned> values{};
  while (not input.eof()) {
    unsigned value = 0;
    input >> value;
    if (input.good()) {
      values.push_back(value);
    }
  }
  return values;
}

unsigned count_increase(std::vector<unsigned> const &input) {
  return std::inner_product(input.cbegin(), std::prev(input.cend()),
                            std::next(input.cbegin()), 0, std::plus<>{},
                            [](auto a, auto b) { return (a < b) ? 1 : 0; });
}

std::vector<unsigned> window_sums(std::vector<unsigned> const &input) {
  std::vector<unsigned> pairs{};
  std::transform(input.cbegin(), std::prev(input.cend()),
                 std::next(input.cbegin()), std::back_inserter(pairs),
                 std::plus<>{});

  std::vector<unsigned> triplets{};
  std::transform(pairs.cbegin(), std::prev(pairs.cend()),
                 std::next(std::next(input.cbegin())),
                 std::back_inserter(triplets), std::plus<>{});

  return triplets;
}

} // namespace sonar
