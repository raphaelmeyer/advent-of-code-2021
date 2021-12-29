#pragma once

#include <functional>
#include <map>
#include <optional>
#include <vector>

#include "amphipod.h"

namespace amphipod {

struct ValidMove {
  Burrow burrow{};
  int cost{};
};

using ValidMoves = std::vector<ValidMove>;

using PathLengths = std::map<std::pair<Types, Locations>, int>;
using Rooms = std::vector<Locations>;
using RoomsByType = std::array<std::vector<Locations>, 4>;
using Prepare = std::function<Burrow(Burrow const &)>;

struct Folded {
  static PathLengths side_paths();
  static Rooms rooms();
  static RoomsByType rooms_by_type();
  static Burrow organized();
  static Prepare prepare();
};

struct Unfolded {
  static PathLengths side_paths();
  static Rooms rooms();
  static RoomsByType rooms_by_type();
  static Burrow organized();
  static Prepare prepare();
};

class Organizer {
public:
  static std::unique_ptr<Organizer> createFolded();
  static std::unique_ptr<Organizer> createUnfolded();

  int least_cost(Burrow const &burrow);

  std::optional<int> move(Burrow const &burrow);
  ValidMoves valid_moves(Burrow const &burrow);

  Burrow prepare(Burrow const &burrow);

  struct Edge {
    Types room{};
    Locations hall{};
    auto operator<=>(Edge const &) const = default;
  };

  struct Path {
    std::vector<Locations> way{};
    int cost{};
  };

  using Paths = std::map<Edge, Path>;
  static Paths generate_paths(Rooms const &side_rooms,
                              PathLengths const &side_paths);

private:
  template <typename Diagram>
  Organizer(Diagram)
      : prepare_{Diagram::prepare()},                                    //
        paths_{generate_paths(Diagram::rooms(), Diagram::side_paths())}, //
        room_by_type_{Diagram::rooms_by_type()}                          //
  {
    costs_.insert(std::make_pair(Diagram::organized(), 0));
  }

  void move_into_room(ValidMoves &moves, Burrow const &burrow);
  void get_out_of_the_way_or_wrong_room(ValidMoves &moves,
                                        Burrow const &burrow);
  void teleport_swap(Locations room, Locations hall, Types type,
                     ValidMoves &moves, Burrow const &burrow);

  std::map<Burrow, std::optional<int>> costs_{};
  Prepare prepare_{};

  Paths const paths_;
  RoomsByType const room_by_type_;
};

} // namespace amphipod
