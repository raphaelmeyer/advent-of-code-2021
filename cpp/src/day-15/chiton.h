#pragma once

#include <filesystem>
#include <istream>
#include <map>
#include <string>
#include <vector>

namespace chiton {

struct RiskMap {
  std::size_t width;
  std::size_t height;
  std::vector<int> levels{};

  template <typename Func>
  void for_each_neighbor(std::size_t location, Func func) const {
    auto const x = location % width;
    if (x > 0) {
      func(location - 1);
    }
    if (x + 1 < width) {
      func(location + 1);
    }
    if (location >= width) {
      func(location - width);
    }
    if (location + width < levels.size()) {
      func(location + width);
    }
  }

  using Pos = std::tuple<std::size_t, std::size_t>;
  std::size_t index(Pos pos) const {
    return std::get<0>(pos) + std::get<1>(pos) * width;
  }

  auto &at(Pos pos) { return levels.at(index(pos)); }
  auto at(Pos pos) const { return levels.at(index(pos)); }
};

RiskMap parse_file(std::filesystem::path filename);
RiskMap parse_input(std::istream &input);

RiskMap make_full_map(RiskMap const &risk_map);

int risk_level(RiskMap const &risk_map);

} // namespace chiton
