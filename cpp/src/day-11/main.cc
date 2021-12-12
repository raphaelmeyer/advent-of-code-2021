#include <iostream>

#include "octopus.h"

int main() {
  auto const levels = octopus::parse_file("data/day-11.txt");

  auto const flashes = octopus::count_flashes(levels, 100);
  auto const sync = octopus::until_sync(levels);

  std::cout << "Part 1: " << flashes << "\n";
  std::cout << "Part 2: " << sync << "\n";
}