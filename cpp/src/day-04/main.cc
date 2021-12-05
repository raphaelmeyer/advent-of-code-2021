#include <iostream>

#include "bingo.h"

int main() {
  auto const game = bingo::parse_file("data/day-04.txt");

  auto const first_winning = bingo::find_first_winning(game);
  auto const score_first = bingo::score(game, first_winning);

  auto const last_winning = bingo::find_last_winning(game);
  auto const score_last = bingo::score(game, last_winning);

  std::cout << "Part 1: " << score_first << "\n";
  std::cout << "Part 2: " << score_last << "\n";
}