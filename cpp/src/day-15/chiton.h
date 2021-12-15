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

  std::vector<std::size_t> paths(std::size_t location) const;

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
