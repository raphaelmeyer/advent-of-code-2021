#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "seven_segment.h"

namespace {

std::string example{
    R"(be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
)"};

TEST_CASE("example input part 1") {
  std::istringstream input{example};
  auto const signals = seven_segment::parse_input(input);

  REQUIRE(seven_segment::count_unique_output(signals) == 26);
}

TEST_CASE("example input part 2") {
  std::istringstream input{example};
  auto const signals = seven_segment::parse_input(input);

  REQUIRE(seven_segment::value_sum(signals) == 61229);
}

TEST_CASE("parse input") {
  std::istringstream input{example};
  auto const signals = seven_segment::parse_input(input);

  REQUIRE(signals.size() == 10);

  REQUIRE(signals.at(0).patterns.size() == 10);
  REQUIRE(signals.at(0).patterns.at(0) == "be");

  REQUIRE(signals.at(9).patterns.size() == 10);
  REQUIRE(signals.at(9).patterns.at(1) == "gcf");
  REQUIRE(signals.at(9).patterns.at(9) == "fegbdc");

  REQUIRE(signals.at(9).output.size() == 4);
  REQUIRE(signals.at(9).output.at(0) == "fgae");
  REQUIRE(signals.at(9).output.at(3) == "bagce");
}

TEST_CASE("determine output value") {
  std::istringstream input{example};
  auto const signals = seven_segment::parse_input(input);
  REQUIRE(seven_segment::find_value(signals.at(0)) == 8394);
  REQUIRE(seven_segment::find_value(signals.at(3)) == 9361);
  REQUIRE(seven_segment::find_value(signals.at(6)) == 4548);
}

TEST_CASE("find mapping") {
  auto const mapping =
      seven_segment::find_mapping({"acedgfb", "cdfbe", "gcdfa", "fbcad", "dab",
                                   "cefabd", "cdfgeb", "eafb", "cagedb", "ab"});

  REQUIRE(mapping.size() == 7);

  REQUIRE(mapping.find('d') != mapping.end());
  REQUIRE(mapping.at('d') == seven_segment::A);

  REQUIRE(mapping.find('a') != mapping.end());
  REQUIRE(mapping.at('a') == seven_segment::C);

  REQUIRE(mapping.find('f') != mapping.end());
  REQUIRE(mapping.at('f') == seven_segment::D);

  REQUIRE(mapping.find('b') != mapping.end());
  REQUIRE(mapping.at('b') == seven_segment::F);

  REQUIRE(mapping.find('c') != mapping.end());
  REQUIRE(mapping.at('c') == seven_segment::G);
}

TEST_CASE("derive digit from mapping") {
  auto const mapping = seven_segment::find_mapping(
      {"fbegcd", "cbd", "adcefb", "dageb", "afcb", "bc", "aefdc", "ecdab",
       "fgdeca", "fcdbega"});

  REQUIRE(seven_segment::digit("cb", mapping) == 1);
  REQUIRE(seven_segment::digit("cedba", mapping) == 3);
  REQUIRE(seven_segment::digit("gadfec", mapping) == 6);
  REQUIRE(seven_segment::digit("efabcd", mapping) == 9);
}

TEST_CASE("decode number") {
  std::istringstream input{example};
  auto const signals = seven_segment::parse_input(input);

  auto const mapping = seven_segment::find_mapping(signals.at(1).patterns);
  REQUIRE(seven_segment::derive_number(signals.at(1).output, mapping) == 9781);
}

} // namespace
