#pragma once

#include <filesystem>
#include <istream>

namespace amphipod {

using Types = std::size_t;
using Locations = std::size_t;

struct Type {
  constexpr static Types const A = 0;
  constexpr static Types const B = 1;
  constexpr static Types const C = 2;
  constexpr static Types const D = 3;
  constexpr static Types const None = 7;
};

struct Location {
  constexpr static Locations const LeftWall = 0;
  constexpr static Locations const LeftHall = 1;
  constexpr static Locations const LeftCenter = 2;
  constexpr static Locations const Center = 3;
  constexpr static Locations const RightCenter = 4;
  constexpr static Locations const RightHall = 5;
  constexpr static Locations const RightWall = 6;

  constexpr static Locations const A_Top = 7;
  constexpr static Locations const B_Top = 8;
  constexpr static Locations const C_Top = 9;
  constexpr static Locations const D_Top = 10;
  constexpr static Locations const A_Bottom = 11;
  constexpr static Locations const B_Bottom = 12;
  constexpr static Locations const C_Bottom = 13;
  constexpr static Locations const D_Bottom = 14;
};

using Burrow = std::array<Types, 15>;

Burrow parse_file(std::filesystem::path filename);
Burrow parse_input(std::istream &input);

int organize(Burrow const &burrow);

} // namespace amphipod
