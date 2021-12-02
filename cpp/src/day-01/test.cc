#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <sstream>

#include "sonar.h"

std::string example{R"(199
200
208
210
200
207
240
269
260
263
)"};

TEST_CASE("parse input") {
  std::istringstream stream{example};
  auto const input = sonar::parse_input(stream);

  REQUIRE(input.size() == 10);
  REQUIRE(input.front() == 199);
  REQUIRE(input.back() == 263);
}

TEST_CASE("count increase") {
  SUBCASE("all increasing") { REQUIRE(sonar::count_increase({1, 2, 3}) == 2); }

  SUBCASE("no increasing") { REQUIRE(sonar::count_increase({7, 5, 3}) == 0); }

  SUBCASE("example input") {
    REQUIRE(sonar::count_increase(
                {199, 200, 208, 210, 200, 207, 240, 269, 260, 263}) == 7);
  }
}

TEST_CASE("sliding window sums") {
  SUBCASE("there are N - 2 entries (window size 3)") {
    auto const actual = sonar::window_sums({0, 0, 1, 2, 3, 4, 5});
    REQUIRE(actual.size() == 5);
  }

  SUBCASE("each entry is the sum of elements of the window") {
    std::vector<unsigned> expected{321, 324, 354, 654};
    auto const actual = sonar::window_sums({1, 20, 300, 4, 50, 600});
    REQUIRE(actual.size() == expected.size());
    for (size_t i = 0; i < actual.size(); ++i) {
      CHECK(actual[i] == expected[i]);
    }
  }

  SUBCASE("example input") {
    std::vector<unsigned> expected{607, 618, 618, 617, 647, 716, 769, 792};
    REQUIRE(sonar::window_sums({199, 200, 208, 210, 200, 207, 240, 269, 260,
                                263}) == expected);
  }
}
