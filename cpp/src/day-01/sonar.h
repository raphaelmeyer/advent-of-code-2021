#pragma once

#include <filesystem>
#include <vector>

namespace sonar {

std::vector<unsigned> parse_file(std::filesystem::path file);
std::vector<unsigned> parse_input(std::istream &input);

unsigned count_increase(std::vector<unsigned> const &input);
std::vector<unsigned> window_sums(std::vector<unsigned> const &input);

} // namespace sonar
