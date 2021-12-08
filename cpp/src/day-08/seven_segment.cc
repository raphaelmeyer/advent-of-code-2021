#include "seven_segment.h"

#include <algorithm>
#include <iterator>
#include <numeric>
#include <sstream>
#include <string_view>

namespace seven_segment {

namespace {

class Line : public std::string {
  friend std::istream &operator>>(std::istream &in_stream, Line &line) {
    return std::getline(in_stream, line);
  }
};

constexpr std::string_view symbols{"abcdefg"};

std::map<int, int> const digits{{A + B + C + E + F + G, 0},
                                {C + F, 1},
                                {A + C + D + E + G, 2},
                                {A + C + D + F + G, 3},
                                {B + C + D + F, 4},
                                {A + B + D + F + G, 5},
                                {A + B + D + E + F + G, 6},
                                {A + C + F, 7},
                                {A + B + C + D + E + F + G, 8},
                                {A + B + C + D + F + G, 9}};

} // namespace

Signals parse_file(std::filesystem::path filename) {
  std::ifstream file{filename};
  return parse_input(file);
}

Signals parse_input(std::istream &input) {
  std::vector<std::string> lines{};
  std::copy(std::istream_iterator<Line>{input}, std::istream_iterator<Line>{},
            std::back_inserter(lines));

  return std::accumulate(
      lines.begin(), lines.end(), Signals{},
      [](auto signals, auto const &line) {
        signals.emplace_back();

        std::istringstream in_line{line};

        std::copy_n(std::istream_iterator<std::string>(in_line), 10,
                    std::back_inserter(signals.back().patterns));

        auto skip_pipe = std::istream_iterator<std::string>(in_line);

        std::copy_n(std::istream_iterator<std::string>(in_line), 4,
                    signals.back().output.begin());

        return signals;
      });
}

std::size_t count_unique_output(Signals const &signals) {
  return std::accumulate(
      signals.begin(), signals.end(), 0, [](auto count, auto const &signal) {
        return count + std::count_if(signal.output.begin(), signal.output.end(),
                                     [](auto const &output) {
                                       auto const segments = output.size();
                                       return segments == 2 || segments == 3 ||
                                              segments == 4 || segments == 7;
                                     });
      });
}

int value_sum(Signals const &signals) {
  return std::accumulate(signals.begin(), signals.end(), 0,
                         [](auto value, auto const &signal) {
                           return value + find_value(signal);
                         });
}

int find_value(Signal const &signal) {
  auto const mapping = find_mapping(signal.patterns);
  return derive_number(signal.output, mapping);
}

Mapping find_mapping(Patterns const &patterns) {
  std::map<char, std::size_t> occurences{};
  std::for_each(symbols.begin(), symbols.end(), [&](auto symbol) {
    occurences[symbol] = std::count_if(
        patterns.begin(), patterns.end(), [&](auto const &pattern) {
          return pattern.find(symbol) != std::string::npos;
        });
  });

  auto const one =
      *std::find_if(patterns.begin(), patterns.end(),
                    [](auto const &pattern) { return pattern.size() == 2; });

  auto const four =
      *std::find_if(patterns.begin(), patterns.end(),
                    [](auto const &pattern) { return pattern.size() == 4; });

  auto const seven =
      *std::find_if(patterns.begin(), patterns.end(),
                    [](auto const &pattern) { return pattern.size() == 3; });

  auto const a = seven[seven.find_first_not_of(one)];

  auto const c =
      *std::find_if(symbols.begin(), symbols.end(), [&](auto symbol) {
        return symbol != a && occurences[symbol] == 8;
      });

  auto const b =
      *std::find_if(symbols.begin(), symbols.end(),
                    [&](auto symbol) { return occurences[symbol] == 6; });

  auto const e =
      *std::find_if(symbols.begin(), symbols.end(),
                    [&](auto symbol) { return occurences[symbol] == 4; });

  auto const f =
      *std::find_if(symbols.begin(), symbols.end(),
                    [&](auto symbol) { return occurences[symbol] == 9; });

  auto const d = four[four.find_first_not_of(std::string{b, c, f})];

  auto const g =
      symbols[symbols.find_first_not_of(std::string{a, b, c, d, e, f})];

  return {{a, A}, {b, B}, {c, C}, {d, D}, {e, E}, {f, F}, {g, G}};
}

int derive_number(Outputs const &output, Mapping const &mapping) {
  return std::accumulate(output.begin(), output.end(), 0,
                         [&mapping](auto value, auto const &symbol) {
                           return 10 * value + digit(symbol, mapping);
                         });
}

int digit(std::string const &signal, Mapping const &mapping) {
  auto const digit = std::accumulate(signal.begin(), signal.end(), 0,
                                     [&mapping](auto value, auto symbol) {
                                       return value + mapping.at(symbol);
                                     });

  return digits.at(digit);
}

} // namespace seven_segment
