#include <iostream>

#include "amphipod.h"

int main() {
  auto const burrow = amphipod::parse_file("data/day-23.txt");

  auto const cost = amphipod::organize(burrow);

  std::cout << "Part 1: " << cost << "\n";
  std::cout << "Part 2: " << 0 << "\n";
}
