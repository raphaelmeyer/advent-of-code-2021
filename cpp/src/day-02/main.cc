#include <iostream>

#include "dive.h"

int main() {
  auto const plan = dive::parse_file("data/day-02.txt");

  auto const position = dive::course(plan);
  auto const position_corrected = dive::course_corrected(plan);

  std::cout << "Part 1: " << position.horizontal * position.depth << "\n";
  std::cout << "Part 2: "
            << position_corrected.horizontal * position_corrected.depth << "\n";
}
