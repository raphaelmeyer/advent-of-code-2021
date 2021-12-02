#pragma once

#include <filesystem>
#include <fstream>
#include <istream>
#include <vector>

namespace dive {

enum class Direction { Forward, Up, Down };

struct Command {
  Direction direction;
  int value;

  bool operator==(Command const &) const = default;
};

struct Position {
  int64_t horizontal;
  int64_t depth;

  int64_t aim = 0;

  bool operator==(Position const &) const = default;
};

using Plan = std::vector<Command>;

Plan parse_file(std::filesystem::path filename);
Plan parse_input(std::istream &input);

Position course(Plan const &plan);
Position course_corrected(Plan const &plan);

} // namespace dive
