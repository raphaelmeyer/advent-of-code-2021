#include "dirac_dice.h"

#include <fstream>
#include <map>
#include <optional>
#include <vector>

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

using DiracDice = std::map<int, int>;

DiracDice dirac_dice() {
  std::vector<int> const die{1, 2, 3};
  DiracDice rolls{};
  for (auto first : die) {
    for (auto second : die) {
      for (auto third : die) {
        rolls[first + second + third]++;
      }
    }
  }

  return rolls;
}

class DiracGame {
public:
  Wins play(int player, Track const track, Score const score) {
    if (score[One] >= 21) {
      return {1, 0};
    }
    if (score[Two] >= 21) {
      return {0, 1};
    }

    auto &state = state_.at(track.at(One))
                      .at(track.at(Two))
                      .at(score.at(One))
                      .at(score.at(Two))
                      .at(player);

    if (state) {
      return *state;
    }

    Wins wins{};

    for (auto roll : dice_) {
      auto new_track = track;
      auto new_score = score;
      new_track[player] = move(new_track[player], roll.first);
      new_score[player] += new_track[player];

      auto const next = (player == One) ? Two : One;
      auto [one, two] = play(next, new_track, new_score);

      wins[One] += roll.second * one;
      wins[Two] += roll.second * two;
    }

    state = wins;
    return wins;
  }

private:
  DiracDice const dice_{dirac_dice()};

  // track player 1, track player 2, score player 1, score player 2, player
  using State = std::array<
      std::array<
          std::array<std::array<std::array<std::optional<Wins>, 2>, 21>, 21>,
          11>,
      11>;
  State state_{};
};

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

Wins play(Track start) {
  auto game = std::make_unique<DiracGame>();

  Score score{};
  return game->play(One, start, score);
}

} // namespace dirac
