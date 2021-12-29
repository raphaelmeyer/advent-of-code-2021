#include "organizer.h"

namespace amphipod {

PathLengths Unfolded::side_paths() {
  return {{{Location::A_Upper, Location::A_Top}, 1},
          {{Location::B_Upper, Location::B_Top}, 1},
          {{Location::C_Upper, Location::C_Top}, 1},
          {{Location::D_Upper, Location::D_Top}, 1},

          {{Location::A_Lower, Location::A_Upper}, 1},
          {{Location::B_Lower, Location::B_Upper}, 1},
          {{Location::C_Lower, Location::C_Upper}, 1},
          {{Location::D_Lower, Location::D_Upper}, 1},

          {{Location::A_Bottom, Location::A_Lower}, 1},
          {{Location::B_Bottom, Location::B_Lower}, 1},
          {{Location::C_Bottom, Location::C_Lower}, 1},
          {{Location::D_Bottom, Location::D_Lower}, 1}};
}

Rooms Unfolded::rooms() {
  return {Location::A_Bottom, Location::B_Bottom,
          Location::C_Bottom, Location::D_Bottom,

          Location::A_Lower,  Location::B_Lower,
          Location::C_Lower,  Location::D_Lower,

          Location::A_Upper,  Location::B_Upper,
          Location::C_Upper,  Location::D_Upper,

          Location::A_Top,    Location::B_Top,
          Location::C_Top,    Location::D_Top};
}
RoomsByType Unfolded::rooms_by_type() {
  return {{{Location::A_Bottom, Location::A_Lower, Location::A_Upper,
            Location::A_Top},
           {Location::B_Bottom, Location::B_Lower, Location::B_Upper,
            Location::B_Top},
           {Location::C_Bottom, Location::C_Lower, Location::C_Upper,
            Location::C_Top},
           {Location::D_Bottom, Location::D_Lower, Location::D_Upper,
            Location::D_Top}}};
}

Burrow Unfolded::organized() {
  return {Type::None, Type::None, Type::None, Type::None,
          Type::None, Type::None, Type::None,

          Type::A,    Type::B,    Type::C,    Type::D, //
          Type::A,    Type::B,    Type::C,    Type::D, //
          Type::A,    Type::B,    Type::C,    Type::D, //
          Type::A,    Type::B,    Type::C,    Type::D};
}

Prepare Unfolded::prepare() {
  return [](auto const &burrow) {
    auto unfolded = burrow;

    unfolded.at(Location::A_Upper) = Type::D;
    unfolded.at(Location::B_Upper) = Type::C;
    unfolded.at(Location::C_Upper) = Type::B;
    unfolded.at(Location::D_Upper) = Type::A;
    unfolded.at(Location::A_Lower) = Type::D;
    unfolded.at(Location::B_Lower) = Type::B;
    unfolded.at(Location::C_Lower) = Type::A;
    unfolded.at(Location::D_Lower) = Type::C;

    return unfolded;
  };
}

} // namespace amphipod
