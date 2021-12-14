#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "polymer.h"

namespace {

std::string example{
    R"(NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
)"};

TEST_CASE("example input part 1") {
  std::istringstream input{example};
  auto const [base, rules] = polymer::parse_input(input);

  auto const polymers = polymer::replicate(rules, base, 10);

  REQUIRE(polymer::common_diff(polymers) == 1588);
}

TEST_CASE("example input part 2") {
  std::istringstream input{example};
  auto const [base, rules] = polymer::parse_input(input);

  auto const polymers = polymer::replicate(rules, base, 40);

  REQUIRE(polymer::common_diff(polymers) == 2188189693529);
}

TEST_CASE("replicate once") {
  std::istringstream input{example};
  auto const [base, rules] = polymer::parse_input(input);

  auto const polymers = polymer::replicate(rules, base, 1);
  auto const pairs = polymers.pairs;

  REQUIRE(pairs.size() == 6);

  REQUIRE(pairs.find("NC"_p) != pairs.end());
  REQUIRE(pairs.at("NC"_p) == 1);

  REQUIRE(pairs.find("CN"_p) != pairs.end());
  REQUIRE(pairs.at("CN"_p) == 1);

  REQUIRE(pairs.find("NB"_p) != pairs.end());
  REQUIRE(pairs.at("NB"_p) == 1);

  REQUIRE(pairs.find("BC"_p) != pairs.end());
  REQUIRE(pairs.at("BC"_p) == 1);

  REQUIRE(pairs.find("CH"_p) != pairs.end());
  REQUIRE(pairs.at("CH"_p) == 1);

  REQUIRE(pairs.find("HB"_p) != pairs.end());
  REQUIRE(pairs.at("HB"_p) == 1);
}

TEST_CASE("replicate twice") {
  std::istringstream input{example};
  auto const [base, rules] = polymer::parse_input(input);

  auto const polymers = polymer::replicate(rules, base, 2);
  auto const pairs = polymers.pairs;

  SUBCASE("do not change last") { REQUIRE(polymers.last == 'B'); }

  REQUIRE(pairs.size() == 8);

  REQUIRE(pairs.find("BB"_p) != pairs.end());
  REQUIRE(pairs.at("BB"_p) == 2);

  REQUIRE(pairs.find("BC"_p) != pairs.end());
  REQUIRE(pairs.at("BC"_p) == 2);

  REQUIRE(pairs.find("BH"_p) != pairs.end());
  REQUIRE(pairs.at("BH"_p) == 1);

  REQUIRE(pairs.find("CN"_p) != pairs.end());
  REQUIRE(pairs.at("CN"_p) == 1);

  REQUIRE(pairs.find("NB"_p) != pairs.end());
  REQUIRE(pairs.at("NB"_p) == 2);
}

TEST_CASE("replicate four times") {
  std::istringstream input{example};
  auto const [base, rules] = polymer::parse_input(input);

  auto const polymers = polymer::replicate(rules, base, 4);
  auto const pairs = polymers.pairs;

  SUBCASE("do not change last") { REQUIRE(polymers.last == 'B'); }

  REQUIRE(pairs.find("CC"_p) != pairs.end());
  REQUIRE(pairs.at("CC"_p) == 2);

  REQUIRE(pairs.find("CN"_p) != pairs.end());
  REQUIRE(pairs.at("CN"_p) == 3);

  REQUIRE(pairs.find("BB"_p) != pairs.end());
  REQUIRE(pairs.at("BB"_p) == 9);

  REQUIRE(pairs.find("NH"_p) != pairs.end());
  REQUIRE(pairs.at("NH"_p) == 1);
}

TEST_CASE("count literals") {
  std::istringstream input{example};
  auto const [base, rules] = polymer::parse_input(input);

  auto const polymers = polymer::replicate(rules, base, 10);

  auto const count = polymer::count_literals(polymers);

  REQUIRE(count.size() == 4);

  REQUIRE(count.find('B') != count.end());
  REQUIRE(count.at('B') == 1749);

  REQUIRE(count.find('C') != count.end());
  REQUIRE(count.at('C') == 298);

  REQUIRE(count.find('H') != count.end());
  REQUIRE(count.at('H') == 161);

  REQUIRE(count.find('N') != count.end());
  REQUIRE(count.at('N') == 865);
}

TEST_CASE("count even more literals") {
  std::istringstream input{example};
  auto const [base, rules] = polymer::parse_input(input);

  auto const polymers = polymer::replicate(rules, base, 40);

  auto const count = polymer::count_literals(polymers);

  REQUIRE(count.find('B') != count.end());
  REQUIRE(count.at('B') == 2192039569602);

  REQUIRE(count.find('H') != count.end());
  REQUIRE(count.at('H') == 3849876073);
}

TEST_CASE("parse input") {
  std::istringstream input{example};
  auto const [base, rules] = polymer::parse_input(input);

  REQUIRE(base.last == 'B');

  auto const pairs = base.pairs;
  REQUIRE(pairs.size() == 3);

  REQUIRE(pairs.find("NN"_p) != pairs.end());
  REQUIRE(pairs.find("NC"_p) != pairs.end());
  REQUIRE(pairs.find("CB"_p) != pairs.end());

  REQUIRE(pairs.at("NN"_p) == 1);
  REQUIRE(pairs.at("NC"_p) == 1);
  REQUIRE(pairs.at("CB"_p) == 1);

  REQUIRE(rules.size() == 16);

  REQUIRE(rules.find("CH"_p) != rules.end());
  REQUIRE(rules.find("BH"_p) != rules.end());
  REQUIRE(rules.find("HN"_p) != rules.end());

  REQUIRE(rules.at("CH"_p).one == "CB"_p);
  REQUIRE(rules.at("CH"_p).two == "BH"_p);

  REQUIRE(rules.at("BH"_p).one == "BH"_p);
  REQUIRE(rules.at("BH"_p).two == "HH"_p);

  REQUIRE(rules.at("HN"_p).one == "HC"_p);
  REQUIRE(rules.at("HN"_p).two == "CN"_p);
}

} // namespace
