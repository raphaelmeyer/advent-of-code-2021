#pragma once

#include <filesystem>
#include <istream>
#include <map>
#include <vector>

namespace bingo {

struct Position {
  std::size_t column = 0;
  std::size_t row = 0;
};

using Board = std::map<int, Position>;

struct Game {
  std::vector<int> numbers{};
  std::vector<Board> boards{};
};

struct Winning {
  int number = 0;
  std::size_t board = 0;
};

Game parse_file(std::filesystem::path filename);

Game parse_input(std::istream &input);

int score(Game const &game, Winning winning);

Winning find_first_winning(Game const &game);
Winning find_last_winning(Game const &game);

int sum_not_marked(Game const &game, Winning winning);

} // namespace bingo
