#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "octopus.h"

namespace {

std::string example{
    R"(5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
)"};

TEST_CASE("example input part 1") {
  std::istringstream input{example};
  auto const levels = octopus::parse_input(input);

  REQUIRE(octopus::count_flashes(levels, 10) == 204);
  REQUIRE(octopus::count_flashes(levels, 100) == 1656);
}

TEST_CASE("example input part 2") {
  std::istringstream input{example};
  auto const levels = octopus::parse_input(input);

  REQUIRE(octopus::until_sync(levels) == 195);
}

TEST_CASE("parse input") {
  std::istringstream input{example};
  auto const levels = octopus::parse_input(input);

  REQUIRE(levels.size() == 10);
  REQUIRE(levels.at(0).size() == 10);
  REQUIRE(levels.at(9).size() == 10);

  REQUIRE(levels.at(0).at(0) == 5);
  REQUIRE(levels.at(0).at(9) == 3);
  REQUIRE(levels.at(1).at(2) == 4);
  REQUIRE(levels.at(9).at(9) == 6);
}

TEST_CASE("step with no flashes") {
  std::istringstream input{example};
  auto levels = octopus::parse_input(input);

  auto const flashes = octopus::step(levels);

  REQUIRE(flashes == 0);

  REQUIRE(levels.at(0) == std::array{6, 5, 9, 4, 2, 5, 4, 3, 3, 4});
  REQUIRE(levels.at(5) == std::array{5, 2, 7, 8, 6, 3, 5, 7, 5, 6});
  REQUIRE(levels.at(6) == std::array{3, 2, 8, 7, 9, 5, 2, 8, 3, 2});
}

TEST_CASE("step with flash") {
  std::string start{R"(8807476555
5089087054
8597889608
8485769600
8700908800
6600088989
6800005943
0000007456
9000000876
8700006848)"};

  std::istringstream input{start};
  auto levels = octopus::parse_input(input);

  auto const flashes = octopus::step(levels);

  REQUIRE(flashes == 45);

  REQUIRE(levels.at(0) == std::array{0, 0, 5, 0, 9, 0, 0, 8, 6, 6});
  REQUIRE(levels.at(3) == std::array{9, 7, 0, 0, 0, 0, 0, 0, 4, 1});
  REQUIRE(levels.at(7) == std::array{2, 2, 1, 1, 1, 3, 0, 0, 0, 0});
}

} // namespace
