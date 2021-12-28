#pragma once

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

class Organizer {
public:
  Organizer();

  std::optional<int> move(Burrow const &burrow);
  ValidMoves valid_moves(Burrow const &burrow);

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
  static Paths generate_paths();

private:
  std::map<Burrow, std::optional<int>> costs_{};
  Paths const paths_{};
};

} // namespace amphipod
