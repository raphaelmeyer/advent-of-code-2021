#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "dive.h"

std::string example{R"(forward 5
down 5
forward 8
up 3
down 8
forward 2
)"};

TEST_CASE("parse input") {
  std::istringstream input{example};
  auto const plan = dive::parse_input(input);

  REQUIRE(plan.size() == 6);
  CHECK(plan.at(0) == dive::Command{dive::Direction::Forward, 5});
  CHECK(plan.at(1) == dive::Command{dive::Direction::Down, 5});
  CHECK(plan.at(3) == dive::Command{dive::Direction::Up, 3});
}

TEST_CASE("dive course") {
  SUBCASE("forward") {
    REQUIRE(dive::course({{dive::Direction::Forward, 7}}) ==
            dive::Position{7, 0});
  }

  SUBCASE("down") {
    REQUIRE(dive::course({{dive::Direction::Down, 11}}) ==
            dive::Position{0, 11});
  }

  SUBCASE("up") {
    REQUIRE(dive::course({{dive::Direction::Down, 11},
                          {dive::Direction::Up, 3}}) == dive::Position{0, 8});
  }

  SUBCASE("example") {
    std::istringstream input{example};
    auto const plan = dive::parse_input(input);

    REQUIRE(dive::course(plan) == dive::Position{15, 10});
  }
}

TEST_CASE("after reading the manual") {
  SUBCASE("forward") {
    REQUIRE(dive::course_corrected({{dive::Direction::Forward, 11}}) ==
            dive::Position{11, 0, 0});
  }

  SUBCASE("forward, aiming down") {
    REQUIRE(dive::course_corrected(
                {{dive::Direction::Down, 5}, {dive::Direction::Forward, 10}}) ==
            dive::Position{10, 50, 5});
  }

  SUBCASE("correcting aim up") {
    REQUIRE(dive::course_corrected({{dive::Direction::Down, 5},
                                    {dive::Direction::Up, 3},
                                    {dive::Direction::Forward, 10}}) ==
            dive::Position{10, 20, 2});
  }

  SUBCASE("example") {
    std::istringstream input{example};
    auto const plan = dive::parse_input(input);

    REQUIRE(dive::course_corrected(plan) == dive::Position{15, 60, 10});
  }
}
