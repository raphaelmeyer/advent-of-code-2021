#pragma once

#include <array>
#include <filesystem>
#include <istream>

namespace dirac {

constexpr std::size_t One = 0;
constexpr std::size_t Two = 1;

using Track = std::array<int, 2>;

struct Practice {
  std::array<int, 2> score{};
  int rolls{};
};

using Wins = std::array<int64_t, 2>;

Track parse_file(std::filesystem::path filename);
Track parse_input(std::istream &input);

Practice practice(Track start);
int practice_score(Practice result);

Wins play(Track start);

} // namespace dirac
