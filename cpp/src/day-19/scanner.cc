#include "scanner.h"

#include <algorithm>
#include <fstream>
#include <numeric>
#include <set>

namespace scanner {

using namespace matrix;

namespace {

int distance_square(Vector const &a, Vector const &b) {
  auto const dx = a[0] - b[0];
  auto const dy = a[1] - b[1];
  auto const dz = a[2] - b[2];
  return dx * dx + dy * dy + dz * dz;
}

Matches find_matches(Scanners const &scanners) {
  struct Dist {
    int distance{};
    std::size_t a{};
    std::size_t b{};
  };

  std::vector<std::vector<Dist>> dist{};

  for (std::size_t s = 0; s < scanners.size(); ++s) {
    auto const &scanner = scanners.at(s);
    dist.push_back({});
    for (std::size_t a = 0; a < scanner.beacons.size(); ++a) {
      for (std::size_t b = a + 1; b < scanner.beacons.size(); ++b) {
        auto const distance =
            distance_square(scanner.beacons.at(a), scanner.beacons.at(b));
        dist.at(s).push_back({distance, a, b});
      }
    }
  }

  struct Match {
    Relation scanner{};
    Relation beacon{};
    auto operator<=>(const Match &) const = default;
  };

  std::map<Match, int> count_matches{};

  for (std::size_t s1 = 0; s1 < scanners.size(); ++s1) {
    for (std::size_t s2 = s1 + 1; s2 < scanners.size(); ++s2) {
      for (auto const &d1 : dist.at(s1)) {
        for (auto const &d2 : dist.at(s2)) {
          if (d1.distance == d2.distance) {
            count_matches[{{s1, s2}, {d1.a, d2.a}}]++;
            count_matches[{{s1, s2}, {d1.a, d2.b}}]++;
            count_matches[{{s1, s2}, {d1.b, d2.a}}]++;
            count_matches[{{s1, s2}, {d1.b, d2.b}}]++;
          }
        }
      }
    }
  }

  Matches matches{};

  for (auto const &count : count_matches) {
    if (count.second >= 5) {
      matches[{count.first.scanner.a, count.first.scanner.b}].push_back(
          {count.first.beacon.a, count.first.beacon.b});
      matches[{count.first.scanner.b, count.first.scanner.a}].push_back(
          {count.first.beacon.b, count.first.beacon.a});
    }
  }

  return matches;
}

} // namespace

Scanners parse_file(std::filesystem::path filename) {
  std::ifstream file{filename};
  return parse_input(file);
}

Scanners parse_input(std::istream &input) {
  Scanners scanners{};

  std::string line{};

  while (std::getline(input, line)) {
    if (not line.starts_with("--- scanner")) {
      throw std::invalid_argument{"input error"};
    }

    scanners.push_back({});

    std::getline(input, line);
    while (not line.empty()) {
      std::istringstream in_line{line};
      char comma{};
      Vector beacon{};
      in_line >> beacon[0] >> comma >> beacon[1] >> comma >> beacon[2];
      scanners.back().beacons.push_back(beacon);
      std::getline(input, line);
    }
  }

  return scanners;
}

Corrections find_scanners(Scanners const &scanners) {
  auto matches = find_matches(scanners);

  auto const rotations = rotation_matrices();

  auto remove_match = [&matches](std::size_t scanner) {
    for (auto it = matches.begin(); it != matches.end();) {
      if (it->first.a == scanner) {
        it = matches.erase(it);
      } else {
        ++it;
      }
    }
  };

  // start with scanner 0 as origin
  Corrections done{};
  done.emplace(0, Correction{rotations.at(0), Vector{0, 0, 0}});
  remove_match(0);

  while (done.size() < scanners.size()) {
    std::size_t max_matches = 0;
    std::size_t base = 0;
    std::size_t candidate = 0;

    for (auto const &match : matches) {
      if (done.contains(match.first.b)) {
        if (max_matches < match.second.size()) {
          max_matches = match.second.size();
          candidate = match.first.a;
          base = match.first.b;
        }
      }
    }

    if (candidate == 0) {
      throw std::logic_error{"no suitable match"};
    }

    auto const relations = matches.at({candidate, base});

    for (auto const &rotation : rotations) {

      auto const a = mult(
          rotation, scanners.at(candidate).beacons.at(relations.front().a));

      auto const b =
          add(done.at(base).translation,
              mult(done.at(base).rotation,
                   scanners.at(base).beacons.at(relations.front().b)));

      Vector const translation{b[0] - a[0], b[1] - a[1], b[2] - a[2]};

      auto const &beacons_a = scanners.at(candidate).beacons;
      auto const &beacons_b = scanners.at(base).beacons;

      auto const count =
          std::count_if(relations.begin(), relations.end(), [&](auto relation) {
            auto const beacon_a =
                add(translation, mult(rotation, beacons_a.at(relation.a)));

            auto const beacon_b =
                add(done.at(base).translation,
                    mult(done.at(base).rotation, beacons_b.at(relation.b)));

            return beacon_a == beacon_b;
          });

      if (count > 5) {
        done.emplace(candidate, Correction{rotation, translation});
        remove_match(candidate);
        break;
      }
    }
  }

  return done;
}

std::size_t number_of_beacons(Scanners const &scanners,
                              Corrections const &corrections) {

  std::set<Vector> beacons{};

  for (std::size_t s = 0; s < scanners.size(); ++s) {
    auto const &rotation = corrections.at(s).rotation;
    auto const &translation = corrections.at(s).translation;
    for (auto const &beacon : scanners.at(s).beacons) {
      auto const transformed = add(translation, mult(rotation, beacon));
      beacons.insert(transformed);
    }
  }

  return beacons.size();
}

int max_manhatten_distance(Corrections const &corrections) {

  std::vector<int> distances{};

  for (auto it = corrections.begin(); it != corrections.end(); ++it) {
    for (auto jt = std::next(it); jt != corrections.end(); ++jt) {
      distances.push_back(
          manhatten_distance(it->second.translation, jt->second.translation));
    }
  }

  return *std::max_element(distances.begin(), distances.end());
}

} // namespace scanner
