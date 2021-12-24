#include <iostream>

#include "dirac_dice.h"

int main() {
  auto const start = dirac::parse_file("data/day-21.txt");

  auto const practice_result = dirac::practice(start);
  auto const practice_score = dirac::practice_score(practice_result);

  std::cout << "Part 1: " << practice_score << "\n";
  std::cout << "Part 2: " << 0 << "\n";
}
