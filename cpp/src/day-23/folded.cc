#include "organizer.h"

namespace amphipod {

PathLengths Folded::side_paths() {
  return {{{Location::A_Bottom, Location::A_Top}, 1},
          {{Location::B_Bottom, Location::B_Top}, 1},
          {{Location::C_Bottom, Location::C_Top}, 1},
          {{Location::D_Bottom, Location::D_Top}, 1}};
}

Rooms Folded::rooms() {
  return {Location::A_Bottom, Location::B_Bottom,
          Location::C_Bottom, Location::D_Bottom,

          Location::A_Top,    Location::B_Top,
          Location::C_Top,    Location::D_Top};
}

RoomsByType Folded::rooms_by_type() {
  return {{{Location::A_Bottom, Location::A_Top},
           {Location::B_Bottom, Location::B_Top},
           {Location::C_Bottom, Location::C_Top},
           {Location::D_Bottom, Location::D_Top}}};
}

Burrow Folded::organized() {
  return {Type::None,   Type::None,   Type::None,   Type::None,
          Type::None,   Type::None,   Type::None,

          Type::A,      Type::B,      Type::C,      Type::D,      //
          Type::A,      Type::B,      Type::C,      Type::D,      //
          Type::Unused, Type::Unused, Type::Unused, Type::Unused, //
          Type::Unused, Type::Unused, Type::Unused, Type::Unused};
}

Prepare Folded::prepare() {
  return [](auto const &burrow) { return burrow; };
}

} // namespace amphipod