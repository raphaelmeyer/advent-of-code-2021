#pragma once

#include "matrix.h"

#include <filesystem>
#include <istream>
#include <map>
#include <vector>

namespace scanner {

using Beacons = std::vector<matrix::Vector>;

struct Scanner {
  Beacons beacons{};
};
using Scanners = std::vector<Scanner>;

struct Relation {
  std::size_t a{};
  std::size_t b{};

  auto operator<=>(const Relation &) const = default;
};

using BeaconMatches = std::vector<Relation>;

using Matches = std::map<Relation, BeaconMatches>;

struct Correction {
  matrix::Matrix rotation{};
  matrix::Vector translation{};
};
using Corrections = std::map<std::size_t, Correction>;

Scanners parse_file(std::filesystem::path filename);
Scanners parse_input(std::istream &input);

Corrections find_scanners(Scanners const &scanners);
std::size_t number_of_beacons(Scanners const &scanners,
                              Corrections const &corrections);
int max_manhatten_distance(Corrections const &corrections);

} // namespace scanner
