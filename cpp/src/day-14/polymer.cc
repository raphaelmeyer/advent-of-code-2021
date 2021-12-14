#include "polymer.h"

#include <algorithm>
#include <fstream>
#include <iterator>
#include <numeric>
#include <string>

namespace polymer {

namespace {

Polymer replicate_once(Rules const &rules, Polymer const &base) {
  return {std::accumulate(base.pairs.begin(), base.pairs.end(), Pairs{},
                          [&rules](auto &&polymers, auto pair) {
                            auto rule = rules.find(pair.first);
                            if (rule != rules.end()) {
                              polymers[rule->second.one] += pair.second;
                              polymers[rule->second.two] += pair.second;
                            } else {
                              polymers[pair.first] += pair.second;
                            }

                            return polymers;
                          }),
          base.last};
}

} // namespace

Template parse_file(std::filesystem::path filename) {
  std::ifstream file{filename};
  return parse_input(file);
}

Template parse_input(std::istream &input) {
  std::string line{};
  input >> line;

  auto const polymers = std::inner_product(
      line.begin(), std::prev(line.end()), std::next(line.begin()), Polymer{},
      [](auto &&polymers, auto pair) {
        polymers.pairs[pair]++;
        return Polymer{polymers.pairs, pair.right};
      },
      [](auto a, auto b) {
        return Pair{a, b};
      });

  Rules rules{};
  while (std::getline(input, line)) {
    if (line.empty()) {
      continue;
    }

    std::istringstream in{line};
    char left{};
    char right{};
    char middle{};
    std::string arrow{};
    in >> left >> right >> arrow >> middle;
    rules[{left, right}] = {{left, middle}, {middle, right}};
  }

  return {polymers, rules};
}

Polymer replicate(Rules const &rules, Polymer const &base, std::size_t steps) {
  auto polymers = base;
  for (std::size_t step = 0; step < steps; ++step) {
    polymers = replicate_once(rules, polymers);
  }
  return polymers;
}

uint64_t common_diff(Polymer const &polymers) {
  auto const count = count_literals(polymers);

  auto const [min, max] =
      std::minmax_element(count.begin(), count.end(),
                          [](auto a, auto b) { return a.second < b.second; });

  return max->second - min->second;
}

Count count_literals(Polymer const &polymers) {
  return std::accumulate(polymers.pairs.begin(), polymers.pairs.end(),
                         Count{{polymers.last, 1}},
                         [](auto &&count, auto pair) {
                           count[pair.first.left] += pair.second;
                           return count;
                         });
}

} // namespace polymer
