#include <iostream>

#include "dirac_dice.h"

int main() {
  auto const start = dirac::parse_file("data/day-21.txt");

  auto const practice_result = dirac::practice(start);
  auto const practice_score = dirac::practice_score(practice_result);

  auto const wins = dirac::play(start);
  auto const winning = std::max(wins[dirac::One], wins[dirac::Two]);

  std::cout << "Part 1: " << practice_score << "\n";
  std::cout << "Part 2: " << winning << "\n";
}
