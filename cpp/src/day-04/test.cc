#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "bingo.h"

namespace {

std::string example{
    R"(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
)"};

TEST_CASE("parse input") {
  std::istringstream input{example};
  auto const game = bingo::parse_input(input);

  SUBCASE("parse drawn numbers") {

    REQUIRE(game.numbers.size() == 27);
    REQUIRE(game.numbers.at(0) == 7);
    REQUIRE(game.numbers.at(1) == 4);
    REQUIRE(game.numbers.at(13) == 16);
    REQUIRE(game.numbers.at(23) == 19);
  }

  SUBCASE("parse bingo boards") {
    REQUIRE(game.boards.size() == 3);

    REQUIRE(game.boards.at(0).size() == 25);
    REQUIRE(game.boards.at(1).size() == 25);
    REQUIRE(game.boards.at(2).size() == 25);

    auto const &second_board = game.boards.at(1);

    REQUIRE(second_board.find(3) != second_board.end());
    REQUIRE(second_board.find(5) != second_board.end());
    REQUIRE(second_board.find(21) != second_board.end());

    REQUIRE(second_board.at(3).row == 0);
    REQUIRE(second_board.at(3).column == 0);

    REQUIRE(second_board.at(5).row == 1);
    REQUIRE(second_board.at(5).column == 4);

    REQUIRE(second_board.at(21).row == 4);
    REQUIRE(second_board.at(21).column == 1);
  }
}

TEST_CASE("first winning board") {
  std::istringstream input{example};
  auto const game = bingo::parse_input(input);

  auto const result = bingo::find_first_winning(game);

  REQUIRE(result.number == 24);
  REQUIRE(result.board == 2);
}

TEST_CASE("last winning board") {
  std::istringstream input{example};
  auto const game = bingo::parse_input(input);

  auto const result = bingo::find_last_winning(game);

  REQUIRE(result.number == 13);
  REQUIRE(result.board == 1);
}

TEST_CASE("sum up unmarked on winning board") {
  std::istringstream input{example};
  auto const game = bingo::parse_input(input);

  SUBCASE("first winning") {
    auto const not_marked = sum_not_marked(game, {24, 2});
    REQUIRE(not_marked == 188);
  }

  SUBCASE("last winning") {
    auto const not_marked = sum_not_marked(game, {13, 1});
    REQUIRE(not_marked == 148);
  }
}

TEST_CASE("calculate score for first winning") {
  std::istringstream input{example};
  auto const game = bingo::parse_input(input);

  SUBCASE("first winning") { REQUIRE(bingo::score(game, {24, 2}) == 4512); }
  SUBCASE("last winning") { REQUIRE(bingo::score(game, {13, 1}) == 1924); }
}

} // namespace