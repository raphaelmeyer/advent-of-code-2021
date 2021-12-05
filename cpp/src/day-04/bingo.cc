#include "bingo.h"

#include <algorithm>
#include <fstream>
#include <iterator>
#include <numeric>
#include <sstream>
#include <string>
#include <utility>

namespace {

template <typename OutIterator>
void read_numbers(std::string_view const input, char delim,
                  OutIterator output) {
  std::string space_separated{};
  std::replace_copy(input.cbegin(), input.cend(),
                    std::back_inserter(space_separated), delim, ' ');

  std::istringstream read_numbers{space_separated};
  std::copy(std::istream_iterator<int>{read_numbers},
            std::istream_iterator<int>{}, output);
}

bingo::Winning play(bingo::Game const &game, std::size_t until_num_of_wins) {
  struct Count {
    std::array<size_t, 5> column{};
    std::array<size_t, 5> row{};
    bool wins = false;
  };
  std::vector<Count> marked(game.boards.size(), Count{});

  std::size_t num_of_wins = 0;

  for (auto number : game.numbers) {
    for (size_t i = 0; i < game.boards.size(); ++i) {
      auto &board = game.boards[i];
      auto &count = marked[i];

      if (count.wins) {
        continue;
      }

      auto found = board.find(number);
      if (found != board.end()) {
        auto const pos = found->second;
        ++count.row[pos.row];
        ++count.column[pos.column];

        if (count.row[pos.row] == 5 || count.column[pos.column] == 5) {
          count.wins = true;
          ++num_of_wins;
        }
        if (num_of_wins == until_num_of_wins) {
          return {number, i};
        }
      }
    }
  }
  return {};
}

} // namespace

namespace bingo {

Game parse_file(std::filesystem::path filename) {
  std::ifstream file{filename};
  return parse_input(file);
}

Game parse_input(std::istream &input) {
  Game game{};

  std::string line{};
  std::getline(input, line);

  read_numbers(line, ',', std::back_inserter(game.numbers));

  std::for_each(std::istream_iterator<int>{input}, std::istream_iterator<int>{},
                [&boards = game.boards](int number) {
                  if (boards.empty()) {
                    boards.emplace_back();
                  };
                  auto pos = boards.back().size();
                  if (pos == 25) {
                    boards.emplace_back();
                    pos = 0;
                  }
                  boards.back().emplace(
                      std::make_pair(number, Position{pos % 5, pos / 5}));
                });

  return game;
}

int score(Game const &game, Winning winning) {
  auto const not_marked = sum_not_marked(game, winning);
  return winning.number * not_marked;
}

Winning find_first_winning(Game const &game) { return play(game, 1); }

Winning find_last_winning(Game const &game) {
  return play(game, game.boards.size());
}

int sum_not_marked(Game const &game, Winning winning) {
  std::vector<int> marked{};
  std::copy(game.numbers.begin(),
            std::next(std::find(game.numbers.begin(), game.numbers.end(),
                                winning.number)),
            std::back_inserter(marked));

  return std::accumulate(game.boards[winning.board].begin(),
                         game.boards[winning.board].end(), 0,
                         [&marked](auto sum, auto number) {
                           if (std::find(marked.begin(), marked.end(),
                                         number.first) == marked.end()) {
                             sum += number.first;
                           }
                           return sum;
                         });
}

} // namespace bingo