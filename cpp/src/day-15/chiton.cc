#include "chiton.h"

#include <fstream>
#include <iterator>
#include <numeric>
#include <stdexcept>

namespace chiton {

std::vector<std::size_t> RiskMap::paths(std::size_t location) const {
  std::vector<std::size_t> p{};

  auto const x = location % width;
  auto const y = location / width;

  if (x > 0) {
    p.push_back(index({x - 1, y}));
  }
  if (x < width - 1) {
    p.push_back(index({x + 1, y}));
  }
  if (y > 0) {
    p.push_back(index({x, y - 1}));
  }
  if (y < height - 1) {
    p.push_back(index({x, y + 1}));
  }
  return p;
}

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

RiskMap make_full_map(RiskMap const &risk_map) { return risk_map; }

int risk_level(RiskMap const &risk_map) {
  struct Location {
    bool visited{false};
    int cost{std::numeric_limits<int>::max()};
  };

  std::vector<Location> locations{risk_map.levels.size(), Location{}};

  auto const target = risk_map.levels.size() - 1;

  std::size_t current = 0;
  locations.at(current).cost = 0;

  while (current != target) {
    for (auto next : risk_map.paths(current)) {
      auto &next_location = locations.at(next);
      if (not next_location.visited) {
        auto const updated_cost =
            locations.at(current).cost + risk_map.levels.at(next);
        next_location.cost = std::min(updated_cost, next_location.cost);
      }
    }
    locations.at(current).visited = true;

    int next_cost = std::numeric_limits<int>::max();
    for (std::size_t i = 0; i < locations.size(); ++i) {
      auto &location = locations.at(i);
      if (not location.visited) {
        if (location.cost < next_cost) {
          next_cost = location.cost;
          current = i;
        }
      }
    }
  }

  return locations.back().cost;
}

} // namespace chiton
