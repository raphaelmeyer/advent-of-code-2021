#include "dive.h"

#include <algorithm>
#include <numeric>
#include <sstream>

namespace dive {

Plan parse_file(std::filesystem::path filename) {
  std::ifstream file{filename};
  return parse_input(file);
}

Plan parse_input(std::istream &input) {

  Plan plan{};
  std::string line{};
  while (std::getline(input, line)) {
    std::string direction{};
    int value{};

    std::istringstream iss{line};
    iss >> direction >> value;

    if (direction == "forward") {
      plan.push_back({Direction::Forward, value});
    } else if (direction == "down") {
      plan.push_back({Direction::Down, value});
    } else if (direction == "up") {
      plan.push_back({Direction::Up, value});
    }
  }

  return plan;
}

Position course(Plan const &plan) {
  return std::accumulate(
      plan.cbegin(), plan.cend(), Position{0, 0},
      [](auto position, auto command) {
        switch (command.direction) {
        case Direction::Forward:
          return Position{position.horizontal + command.value, position.depth};
        case Direction::Down:
          return Position{position.horizontal, position.depth + command.value};
        case Direction::Up:
          return Position{position.horizontal, position.depth - command.value};
        default:
          return Position{};
        }
      });
}

Position course_corrected(Plan const &plan) {
  return std::accumulate(
      plan.cbegin(), plan.cend(), Position{0, 0, 0},
      [](auto position, auto command) {
        switch (command.direction) {
        case Direction::Forward:
          return Position{position.horizontal + command.value,
                          position.depth + command.value * position.aim,
                          position.aim};
        case Direction::Down:
          return Position{position.horizontal, position.depth,
                          position.aim + command.value};
        case Direction::Up:
          return Position{position.horizontal, position.depth,
                          position.aim - command.value};
        default:
          return Position{};
        }
      });
}

} // namespace dive
