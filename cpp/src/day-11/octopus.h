#pragma once

#include <array>
#include <filesystem>
#include <fstream>

namespace octopus {

using Levels = std::array<std::array<int, 10>, 10>;

Levels parse_file(std::filesystem::path filename);
Levels parse_input(std::istream &input);

int count_flashes(Levels levels, int repeat);
int until_sync(Levels levels);

int step(Levels &levels);

} // namespace octopus
