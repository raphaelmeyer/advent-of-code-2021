#include <iostream>

#include "scanner.h"

int main() {
  auto const scanners = scanner::parse_file("data/day-19.txt");

  auto const corrections = find_scanners(scanners);

  auto const beacons = scanner::number_of_beacons(scanners, corrections);
  auto const distance = scanner::max_manhatten_distance(corrections);

  std::cout << "Part 1: " << beacons << "\n";
  std::cout << "Part 2: " << distance << "\n";
}
