#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "chiton.h"

namespace {

std::string example{
    R"(1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
)"};

TEST_CASE("example input part 1") {
  std::istringstream input{example};
  auto const risk_map = chiton::parse_input(input);

  SUBCASE("part 1") {
    auto const risk_level = chiton::risk_level(risk_map);

    REQUIRE(risk_level == 40);
  }

  SUBCASE("part 2") {
    auto const full_map = chiton::make_full_map(risk_map);
    auto const risk_level = chiton::risk_level(full_map);

    REQUIRE(risk_level == 315);
  }
}

TEST_CASE("parse input") {
  std::istringstream input{example};
  auto const risk_map = chiton::parse_input(input);

  REQUIRE(risk_map.width == 10);
  REQUIRE(risk_map.height == 10);

  REQUIRE(risk_map.at({0, 0}) == 1);
  REQUIRE(risk_map.at({2, 2}) == 3);
  REQUIRE(risk_map.at({4, 2}) == 5);
  REQUIRE(risk_map.at({9, 5}) == 7);
  REQUIRE(risk_map.at({9, 7}) == 9);
}

} // namespace
