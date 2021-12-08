#include <iostream>

#include "seven_segment.h"

int main() {
  auto const signals = seven_segment::parse_file("data/day-08.txt");

  auto const uniques = seven_segment::count_unique_output(signals);
  auto const sum = seven_segment::value_sum(signals);

  std::cout << "Part 1: " << uniques << "\n";
  std::cout << "Part 2: " << sum << "\n";
}