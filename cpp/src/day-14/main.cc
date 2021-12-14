#include <iostream>

#include "polymer.h"

int main() {
  auto const [base, rules] = polymer::parse_file("data/day-14.txt");

  auto const polymers = polymer::replicate(rules, base, 10);
  auto const diff = polymer::common_diff(polymers);

  auto const strong_polymers = polymer::replicate(rules, polymers, 30);
  auto const strong_diff = polymer::common_diff(strong_polymers);

  std::cout << "Part 1: " << diff << "\n";
  std::cout << "Part 2: " << strong_diff << "\n";
}