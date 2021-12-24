#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "dirac_dice.h"

namespace {

std::string example{
    R"(Player 1 starting position: 4
Player 2 starting position: 8
)"};

TEST_CASE("example input") {
  std::istringstream input{example};
  auto const start = dirac::parse_input(input);

  SUBCASE("part 1") {
    auto const result = dirac::practice(start);
    auto const score = dirac::practice_score(result);
    REQUIRE(score == 739785);
  }
}

TEST_CASE("parse input") {
  std::istringstream input{example};
  auto const start = dirac::parse_input(input);

  REQUIRE(start[dirac::One] == 4);
  REQUIRE(start[dirac::Two] == 8);
}

} // namespace
