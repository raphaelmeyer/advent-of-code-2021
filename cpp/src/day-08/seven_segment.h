#pragma once

#include <filesystem>
#include <fstream>
#include <map>
#include <string>
#include <vector>

namespace seven_segment {

constexpr int A = 0b0000001;
constexpr int B = 0b0000010;
constexpr int C = 0b0000100;
constexpr int D = 0b0001000;
constexpr int E = 0b0010000;
constexpr int F = 0b0100000;
constexpr int G = 0b1000000;

using Patterns = std::vector<std::string>;
using Outputs = std::array<std::string, 4>;

struct Signal {
  Patterns patterns{};
  Outputs output{};
};
using Signals = std::vector<Signal>;

using Mapping = std::map<char, int>;

Signals parse_file(std::filesystem::path filename);
Signals parse_input(std::istream &input);

std::size_t count_unique_output(Signals const &signals);
int value_sum(Signals const &signals);

int find_value(Signal const &signal);
Mapping find_mapping(Patterns const &patterns);
int derive_number(Outputs const &output, Mapping const &mapping);
int digit(std::string const &signal, Mapping const &mapping);

} // namespace seven_segment
