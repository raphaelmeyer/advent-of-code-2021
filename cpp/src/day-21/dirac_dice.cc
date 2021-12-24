#include "dirac_dice.h"

#include <fstream>

namespace dirac {

namespace {

class PracticeDie {
public:
  int roll() {
    count_ += 3;
    return next() + next() + next();
  }

  int count() const { return count_; }

private:
  int next() {
    auto const current = value_;
    value_ = (value_ >= 100) ? 1 : value_ + 1;
    return current;
  }

  int value_ = 1;
  int count_ = 0;
};

int move(int from, int by) {
  auto const to = from + (by % 10);
  return (to <= 10) ? to : to - 10;
}

} // namespace

Track parse_file(std::filesystem::path filename) {
  std::ifstream file{filename};
  return parse_input(file);
}

Track parse_input(std::istream &input) {
  Track start{};

  std::for_each(start.begin(), start.end(), [&](auto &space) {
    std::string line{};
    std::getline(input, line);
    auto pos = line.find_last_of(':');
    space = std::stoi(line.substr(pos + 1));
  });

  return start;
}

Practice practice(Track start) {
  PracticeDie die{};

  Practice result{};
  auto track = start;
  auto player = One;

  for (;;) {
    auto const roll = die.roll();
    result.rolls = die.count();

    track[player] = move(track[player], roll);
    result.score[player] += track[player];

    if (result.score[player] >= 1000) {
      return result;
    }

    player = (player == One) ? Two : One;
  }

  return {};
}

int practice_score(Practice result) {
  return result.rolls * std::min(result.score[One], result.score[Two]);
}

Wins play([[maybe_unused]] Track start) { return {}; }

} // namespace dirac
