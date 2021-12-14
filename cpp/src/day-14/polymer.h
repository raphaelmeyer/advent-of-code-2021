#pragma once

#include <filesystem>
#include <istream>
#include <map>

namespace polymer {

struct Pair {
  char left{};
  char right{};

  friend auto operator<=>(const Pair &, const Pair &) = default;
};

using Pairs = std::map<Pair, uint64_t>;

struct Polymer {
  Pairs pairs{};
  char last{};
};

struct Rule {
  Pair one{};
  Pair two{};
};

using Rules = std::map<Pair, Rule>;

struct Template {
  Polymer base{};
  Rules rules{};
};

using Count = std::map<char, uint64_t>;

Template parse_file(std::filesystem::path filename);
Template parse_input(std::istream &input);

Polymer replicate(Rules const &rules, Polymer const &base, std::size_t steps);
uint64_t common_diff(Polymer const &polymers);

Count count_literals(Polymer const &polymers);

} // namespace polymer

constexpr polymer::Pair operator"" _p(char const *literal, std::size_t size) {
  if (size == 2) {
    return {literal[0], literal[1]};
  }
  return {};
}
