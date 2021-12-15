#include "chiton.h"

#include <fstream>
#include <iterator>
#include <numeric>
#include <stdexcept>

namespace chiton {

RiskMap parse_file(std::filesystem::path filename) {
  std::ifstream file{filename};
  return parse_input(file);
}

RiskMap parse_input(std::istream &input) {
  return std::accumulate(std::istream_iterator<std::string>(input),
                         std::istream_iterator<std::string>(), RiskMap{},
                         [](auto &&risk_map, std::string const &line) {
                           if (risk_map.width == 0) {
                             risk_map.width = line.size();
                           } else if (risk_map.width != line.size()) {
                             throw std::invalid_argument{"wrong dimension"};
                           }
                           std::transform(line.begin(), line.end(),
                                          std::back_inserter(risk_map.levels),
                                          [](auto ch) {
                                            if ('0' <= ch && ch <= '9')
                                              return ch - '0';
                                            return 0;
                                          });
                           risk_map.height++;
                           return risk_map;
                         });
}

RiskMap make_full_map(RiskMap const &risk_map) {
  RiskMap full_map{};
  full_map.width = 5 * risk_map.width;
  full_map.height = 5 * risk_map.height;

  auto calc_risk = [](auto x, auto y, auto risk) {
    auto new_risk = x + y + risk;
    while (new_risk > 9) {
      new_risk -= 9;
    }
    return new_risk;
  };

  for (std::size_t tile_y = 0; tile_y < 5; ++tile_y) {
    for (std::size_t source_y = 0; source_y < risk_map.height; ++source_y) {
      for (std::size_t tile_x = 0; tile_x < 5; ++tile_x) {
        for (std::size_t source_x = 0; source_x < risk_map.height; ++source_x) {
          auto const source = risk_map.index({source_x, source_y});
          full_map.levels.push_back(
              calc_risk(tile_x, tile_y, risk_map.levels.at(source)));
        }
      }
    }
  }

  return full_map;
}

int risk_level(RiskMap const &risk_map) {
  struct Location {
    bool visited{false};
    int cost{std::numeric_limits<int>::max()};
  };

  std::vector<Location> locations{risk_map.levels.size(), Location{}};
  std::map<std::size_t, int> unvisited{};

  auto const target = risk_map.levels.size() - 1;

  std::size_t current = 0;
  locations.at(current).cost = 0;

  while (current != target) {

    risk_map.for_each_neighbor(current, [&](auto next) {
      auto &next_location = locations.at(next);
      if (not next_location.visited) {
        auto const updated_cost =
            locations.at(current).cost + risk_map.levels.at(next);
        next_location.cost = std::min(updated_cost, next_location.cost);
        unvisited[next] = next_location.cost;
      }
    });

    locations.at(current).visited = true;
    unvisited.erase(current);

    auto it_next =
        std::min_element(unvisited.begin(), unvisited.end(),
                         [](auto a, auto b) { return a.second < b.second; });
    if (it_next == unvisited.end()) {
      throw std::logic_error{"no solution"};
    }
    current = it_next->first;
  }

  return locations.back().cost;
}

} // namespace chiton
