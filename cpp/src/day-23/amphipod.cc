#include "amphipod.h"

#include "organizer.h"

#include <array>
#include <fstream>
#include <iterator>
#include <memory>
#include <numeric>
#include <string>

namespace amphipod {

Burrow parse_file(std::filesystem::path filename) {
  std::ifstream file{filename};
  return parse_input(file);
}

Burrow parse_input(std::istream &input) {
  std::array const locations{Location::A_Top,    Location::B_Top,
                             Location::C_Top,    Location::D_Top,
                             Location::A_Bottom, Location::B_Bottom,
                             Location::C_Bottom, Location::D_Bottom};

  auto loc = locations.begin();
  Burrow burrow{};
  std::fill(burrow.begin(), burrow.end(), Type::None);

  return std::accumulate(std::istream_iterator<char>(input),
                         std::istream_iterator<char>(), burrow,
                         [&](auto &&burrow, auto ch) {
                           switch (ch) {
                           default:
                             return burrow;

                           case 'A':
                             burrow.at(*loc) = Type::A;
                             break;

                           case 'B':
                             burrow.at(*loc) = Type::B;
                             break;

                           case 'C':
                             burrow.at(*loc) = Type::C;
                             break;

                           case 'D':
                             burrow.at(*loc) = Type::D;
                             break;
                           }
                           ++loc;
                           return burrow;
                         });
}

int organize(Burrow const &burrow) {
  auto amphipod = std::make_unique<Organizer>();

  auto const cost = amphipod->move(burrow);
  return cost.value_or(0);
}

} // namespace amphipod
