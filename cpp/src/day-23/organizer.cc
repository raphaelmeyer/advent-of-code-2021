#include "organizer.h"

#include <algorithm>
#include <set>

namespace amphipod {

namespace {

std::map<std::pair<Types, Locations>, int> path_lengths{
    {{Location::A_Bottom, Location::A_Top}, 1},
    {{Location::B_Bottom, Location::B_Top}, 1},
    {{Location::C_Bottom, Location::C_Top}, 1},
    {{Location::D_Bottom, Location::D_Top}, 1},

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

constexpr std::array const rooms{Location::A_Bottom, Location::B_Bottom,
                                 Location::C_Bottom, Location::D_Bottom,
                                 Location::A_Top,    Location::B_Top,
                                 Location::C_Top,    Location::D_Top};

constexpr std::array const halls{Location::LeftWall,    Location::LeftHall,
                                 Location::LeftCenter,  Location::Center,
                                 Location::RightCenter, Location::RightHall,
                                 Location::RightWall};

constexpr std::array const types{Type::A, Type::B, Type::C, Type::D};

std::map<Types, std::array<Locations, 2>> const room_by_type{
    {Type::A, {Location::A_Bottom, Location::A_Top}},
    {Type::B, {Location::B_Bottom, Location::B_Top}},
    {Type::C, {Location::C_Bottom, Location::C_Top}},
    {Type::D, {Location::D_Bottom, Location::D_Top}}};

std::array cost_by_type{1, 10, 100, 1000};

} // namespace

Organizer::Paths Organizer::generate_paths() {
  Paths paths{};

  for (auto from : rooms) {
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
            (std::find(rooms.begin(), rooms.end(), next) != rooms.end());

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
      if ((std::find(rooms.begin(), rooms.end(), path.first.room) !=
           rooms.end()) &&
          (std::find(halls.begin(), halls.end(), path.first.hall) !=
           halls.end())) {
        paths.emplace(path);
      }
    }
  }

  return paths;
}

Organizer::Organizer() : paths_{generate_paths()} {
  auto const organized = std::make_pair<Burrow, int>(
      {Type::None, Type::None, Type::None, Type::None, Type::None, Type::None,
       Type::None,

       Type::A, Type::B, Type::C, Type::D, Type::A, Type::B, Type::C, Type::D},
      0);

  costs_.insert(organized);
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

  // move from hallway into room

  for (auto hall : halls) {
    auto const type = burrow.at(hall);
    if (type != Type::None) {
      auto const &rooms = room_by_type.at(type);
      auto const room =
          std::find_if(rooms.begin(), rooms.end(),
                       [&](auto room) { return burrow.at(room) != type; });
      if (room != rooms.end()) {
        if (burrow.at(*room) == Type::None) {
          auto const &path = paths_.at({
              *room,
              hall,
          });
          if (std::all_of(path.way.begin(), path.way.end(), [&](auto loc) {
                return burrow.at(loc) == Type::None;
              })) {
            auto update = burrow;
            update.at(hall) = Type::None;
            update.at(*room) = type;
            moves.push_back({update, path.cost * cost_by_type.at(type)});
          }
        }
      }
    }
  }

  // move out of wrong room

  for (auto const type : types) {
    for (auto const room : room_by_type.at(type)) {

      // todo : . . . <wrong> _ _ _
      if (burrow.at(room) != Type::None && burrow.at(room) != type) {
        auto const other = burrow.at(room);
        for (auto const hall : halls) {
          if (burrow.at(hall) == Type::None) {
            auto const &path = paths_.at({room, hall});
            if (std::all_of(path.way.begin(), path.way.end(), [&](auto loc) {
                  return burrow.at(loc) == Type::None;
                })) {
              auto update = burrow;
              update.at(room) = Type::None;
              update.at(hall) = other;
              moves.push_back({update, path.cost * cost_by_type.at(other)});
            }
          }
        }
      }
    }
  }

  // move out of own room because another amphipod is trapped
  // . . . <ok> _ <wrong> _

  for (auto const type : types) {
    auto rooms = room_by_type.at(type);
    auto it = rooms.rbegin();
    while (it != rooms.rend() && burrow.at(*it) == Type::None) {
      ++it;
    }
    if (it != rooms.rend() && burrow.at(*it) == type) {
      auto const me = *it;
      if (std::any_of(it, rooms.rend(),
                      [&](auto room) { return burrow.at(room) != type; })) {
        for (auto const hall : halls) {
          auto const &path = paths_.at({me, hall});
          if (std::all_of(path.way.begin(), path.way.end(), [&](auto loc) {
                return burrow.at(loc) == Type::None;
              })) {
            auto update = burrow;
            update.at(me) = Type::None;
            update.at(hall) = type;
            moves.push_back({update, path.cost * cost_by_type.at(type)});
          }
        }
      }
    }
  }

  return moves;
}

} // namespace amphipod
