#include <iostream>

#include "sonar.h"

int main() {

  auto const input = sonar::parse_file("data/day-01.txt");
  auto sums = sonar::window_sums(input);

  std::cout << "Part 1: " << sonar::count_increase(input) << "\n";
  std::cout << "Part 2: " << sonar::count_increase(sums) << "\n";
}