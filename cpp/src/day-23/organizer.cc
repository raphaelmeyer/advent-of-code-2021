#include "organizer.h"

#include <algorithm>
#include <set>

namespace amphipod {

namespace {

PathLengths const common_path_lengths{
    {{Location::A_Top, Location::LeftHall}, 2},
    {{Location::A_Top, Location::LeftCenter}, 2},

    {{Location::B_Top, Location::LeftCenter}, 2},
    {{Location::B_Top, Location::Center}, 2},

    {{Location::C_Top, Location::Center}, 2},
    {{Location::C_Top, Location::RightCenter}, 2},

    {{Location::D_Top, Location::RightCenter}, 2},
    {{Location::D_Top, Location::RightHall}, 2},

    {{Location::LeftWall, Location::LeftHall}, 1},
    {{Location::LeftHall, Location::LeftCenter}, 2},
    {{Location::LeftCenter, Location::Center}, 2},
    {{Location::Center, Location::RightCenter}, 2},
    {{Location::RightCenter, Location::RightHall}, 2},
    {{Location::RightHall, Location::RightWall}, 1}};

constexpr std::array const halls{Location::LeftWall,    Location::LeftHall,
                                 Location::LeftCenter,  Location::Center,
                                 Location::RightCenter, Location::RightHall,
                                 Location::RightWall};

constexpr std::array const types{Type::A, Type::B, Type::C, Type::D};

std::array cost_by_type{1, 10, 100, 1000};

} // namespace

std::unique_ptr<Organizer> Organizer::createFolded() {
  return std::unique_ptr<Organizer>{new Organizer(Folded{})};
}

std::unique_ptr<Organizer> Organizer::createUnfolded() {
  return std::unique_ptr<Organizer>{new Organizer(Unfolded{})};
}

int Organizer::least_cost(Burrow const &burrow) {
  auto prepared = prepare(burrow);
  auto const cost = move(prepared);
  return cost.value_or(0);
}

std::optional<int> Organizer::move(Burrow const &burrow) {
  if (costs_.contains(burrow)) {
    return costs_.at(burrow);
  }

  std::vector<int> costs{};
  auto const moves = valid_moves(burrow);
  for (auto const &valid_move : moves) {
    auto const cost = move(valid_move.burrow);
    if (cost.has_value()) {
      costs.push_back(valid_move.cost + cost.value());
    }
  }

  auto min_cost = std::min_element(costs.begin(), costs.end());
  if (min_cost != costs.end()) {
    costs_.emplace(burrow, *min_cost);
    return *min_cost;
  }

  costs_.emplace(burrow, std::nullopt);
  return {};
}

ValidMoves Organizer::valid_moves(Burrow const &burrow) {
  ValidMoves moves{};

  move_into_room(moves, burrow);
  get_out_of_the_way_or_wrong_room(moves, burrow);

  return moves;
}

Burrow Organizer::prepare(Burrow const &burrow) { return prepare_(burrow); }

void Organizer::move_into_room(ValidMoves &moves, Burrow const &burrow) {
  // for each spot in the hallway
  for (auto hall : halls) {
    auto const type = burrow.at(hall);
    if (type != Type::None) {

      // from bottom to top, find first empty or other
      auto const &rooms = room_by_type_.at(type);
      auto const room =
          std::find_if(rooms.begin(), rooms.end(),
                       [&](auto room) { return burrow.at(room) != type; });

      // check if it's empty
      if (room != rooms.end() && burrow.at(*room) == Type::None) {

        // move if way is not obstructed
        teleport_swap(*room, hall, type, moves, burrow);
      }
    }
  }
}

void Organizer::get_out_of_the_way_or_wrong_room(ValidMoves &moves,
                                                 Burrow const &burrow) {
  // for each side room
  for (auto const type : types) {

    // from top to bottom, find first not empty
    auto rooms = room_by_type_.at(type);
    auto it = rooms.rbegin();
    while (it != rooms.rend() && burrow.at(*it) == Type::None) {
      ++it;
    }

    if (it != rooms.rend()) {
      auto const room = *it;

      // i'm in the wrong room
      if (burrow.at(room) != type) {
        auto const other = burrow.at(room);

        // find a spot in the hallway
        for (auto const hall : halls) {
          if (burrow.at(hall) == Type::None) {
            teleport_swap(room, hall, other, moves, burrow);
          }
        }
      } else if (burrow.at(room) == type) {
        // correct room, but check if someone else is behind me
        if (std::any_of(it, rooms.rend(),
                        [&](auto room) { return burrow.at(room) != type; })) {

          // find a spot in the hallway
          for (auto const hall : halls) {
            if (burrow.at(hall) == Type::None) {
              teleport_swap(room, hall, type, moves, burrow);
            }
          }
        }
      }
    }
  }
}

void Organizer::teleport_swap(Locations room, Locations hall, Types type,
                              ValidMoves &moves, Burrow const &burrow) {
  auto const &path = paths_.at({room, hall});
  if (std::all_of(path.way.begin(), path.way.end(), [&](auto location) {
        return burrow.at(location) == Type::None;
      })) {
    auto update = burrow;

    std::swap(update.at(room), update.at(hall));

    moves.push_back({update, path.cost * cost_by_type.at(type)});
  }
}

Organizer::Paths Organizer::generate_paths(Rooms const &side_rooms,
                                           PathLengths const &side_paths) {
  Paths paths{};

  auto path_lengths = common_path_lengths;
  for (auto const &side_path : side_paths) {
    path_lengths.insert(side_path);
  }

  for (auto from : side_rooms) {
    Paths pathfinding{};
    std::set<Locations> visited{};
    std::map<Locations, int> unvisited{};

    pathfinding.emplace(Edge{from, from}, Path{{}, 0});

    auto current = from;
    bool done = false;

    while (not done) {
      auto const &current_path = pathfinding.at({from, current});

      for (auto const &length : path_lengths) {
        Locations next{};
        if (length.first.first == current) {
          next = length.first.second;
        } else if (length.first.second == current) {
          next = length.first.first;
        } else {
          continue;
        }

        auto const forbidden =
            (std::find(halls.begin(), halls.end(), current) != halls.end()) &&
            (std::find(side_rooms.begin(), side_rooms.end(), next) !=
             side_rooms.end());

        if ((not forbidden) && (not visited.contains(next))) {

          auto const updated_cost = current_path.cost + length.second;
          auto updated_way = current_path.way;
          if (current != from) {
            updated_way.push_back(current);
          }

          if (pathfinding.contains({from, next})) {
            auto &next_path = pathfinding.at({from, next});
            if (updated_cost < next_path.cost) {
              next_path.cost = updated_cost;
              next_path.way = updated_way;
            }
          } else {
            pathfinding.emplace(Edge{from, next},
                                Path{updated_way, updated_cost});
          }

          unvisited[next] = pathfinding.at({from, next}).cost;
        }
      }

      visited.insert(current);
      unvisited.erase(current);

      auto it_next =
          std::min_element(unvisited.begin(), unvisited.end(),
                           [](auto a, auto b) { return a.second < b.second; });

      if (it_next != unvisited.end()) {
        current = it_next->first;
      } else {
        done = true;
      }
    }

    for (auto const &path : pathfinding) {
      if ((std::find(side_rooms.begin(), side_rooms.end(), path.first.room) !=
           side_rooms.end()) &&
          (std::find(halls.begin(), halls.end(), path.first.hall) !=
           halls.end())) {
        paths.emplace(path);
      }
    }
  }

  return paths;
}

} // namespace amphipod
