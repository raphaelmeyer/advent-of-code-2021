#include "octopus.h"

#include <iterator>
#include <string>
#include <vector>

namespace {

std::array<std::tuple<int, int>, 8> neighbors(int row, int col) {
  return {{{row - 1, col - 1},
           {row - 1, col},
           {row - 1, col + 1},
           {row, col - 1},
           {row, col + 1},
           {row + 1, col - 1},
           {row + 1, col},
           {row + 1, col + 1}}};
}

} // namespace

namespace octopus {

Levels parse_file(std::filesystem::path filename) {

  std::ifstream file{filename};
  return parse_input(file);
}
Levels parse_input(std::istream &input) {
  Levels levels{};

  std::for_each(
      levels.begin(), levels.end(), [&input](std::array<int, 10> &row) {
        std::string line{};
        input >> line;
        std::transform(line.begin(), line.end(), row.begin(), [](auto ch) {
          if ('0' <= ch && ch <= '9') {
            return ch - '0';
          }
          return 0;
        });
      });

  return levels;
}

int count_flashes(Levels levels, int repeat) {
  int flashes = 0;

  for (int i = 0; i < repeat; ++i) {
    flashes += step(levels);
  }
  return flashes;
}

int until_sync(Levels levels) {
  int steps = 1;
  while (step(levels) != 100) {
    ++steps;
  }
  return steps;
}

int step(Levels &levels) {
  std::for_each(levels.begin(), levels.end(), [](auto &row) {
    std::for_each(row.begin(), row.end(), [](auto &level) { ++level; });
  });

  int flashes = 0;

  for (bool ignite = true; ignite;) {
    ignite = false;
    for (int j = 0; j < 10; ++j) {
      for (int i = 0; i < 10; ++i) {
        if (levels.at(j).at(i) > 9) {
          levels.at(j).at(i) = 0;
          ++flashes;
          for (auto [row, col] : neighbors(j, i)) {
            if (0 <= row && row < 10 && 0 <= col && col < 10) {
              if (levels.at(row).at(col) != 0) {
                ++levels.at(row).at(col);
                if (levels.at(row).at(col) > 9) {
                  ignite = true;
                }
              }
            }
          }
        }
      }
    }
  }

  return flashes;
}

} // namespace octopus
