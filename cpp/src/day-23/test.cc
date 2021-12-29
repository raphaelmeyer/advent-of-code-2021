#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "amphipod.h"
#include "organizer.h"

namespace {

std::string example{
    R"(#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
)"};

TEST_CASE("example input part 1") {
  std::istringstream input{example};
  auto const burrow = amphipod::parse_input(input);

  auto const cost = amphipod::organize(burrow);

  REQUIRE(cost == 12521);
}

TEST_CASE("example input part 2") {
  std::istringstream input{example};
  auto const burrow = amphipod::parse_input(input);

  auto const cost = amphipod::organize_all(burrow);

  REQUIRE(cost == 44169);
}

TEST_CASE("parse input") {
  std::istringstream input{example};
  auto const burrow = amphipod::parse_input(input);

  REQUIRE(burrow.at(amphipod::Location::A_Top) == amphipod::Type::B);
  REQUIRE(burrow.at(amphipod::Location::A_Bottom) == amphipod::Type::A);
  REQUIRE(burrow.at(amphipod::Location::B_Top) == amphipod::Type::C);
  REQUIRE(burrow.at(amphipod::Location::B_Bottom) == amphipod::Type::D);
  REQUIRE(burrow.at(amphipod::Location::C_Top) == amphipod::Type::B);
  REQUIRE(burrow.at(amphipod::Location::C_Bottom) == amphipod::Type::C);
  REQUIRE(burrow.at(amphipod::Location::D_Top) == amphipod::Type::D);
  REQUIRE(burrow.at(amphipod::Location::D_Bottom) == amphipod::Type::A);

  REQUIRE(burrow.at(amphipod::Location::LeftHall) == amphipod::Type::None);
  REQUIRE(burrow.at(amphipod::Location::Center) == amphipod::Type::None);

  REQUIRE(burrow.at(amphipod::Location::C_Upper) == amphipod::Type::Unused);
  REQUIRE(burrow.at(amphipod::Location::D_Lower) == amphipod::Type::Unused);
}

TEST_CASE("unfold") {
  std::istringstream input{example};
  auto organizer = amphipod::Organizer::createUnfolded();
  auto const unfolded = organizer->prepare(amphipod::parse_input(input));

  REQUIRE(unfolded.at(amphipod::Location::B_Top) == amphipod::Type::C);
  REQUIRE(unfolded.at(amphipod::Location::D_Bottom) == amphipod::Type::A);

  // #D#C#B#A#
  // #D#B#A#C#

  REQUIRE(unfolded.at(amphipod::Location::A_Upper) == amphipod::Type::D);
  REQUIRE(unfolded.at(amphipod::Location::B_Upper) == amphipod::Type::C);
  REQUIRE(unfolded.at(amphipod::Location::C_Upper) == amphipod::Type::B);
  REQUIRE(unfolded.at(amphipod::Location::D_Upper) == amphipod::Type::A);
  REQUIRE(unfolded.at(amphipod::Location::A_Lower) == amphipod::Type::D);
  REQUIRE(unfolded.at(amphipod::Location::B_Lower) == amphipod::Type::B);
  REQUIRE(unfolded.at(amphipod::Location::C_Lower) == amphipod::Type::A);
  REQUIRE(unfolded.at(amphipod::Location::D_Lower) == amphipod::Type::C);

  REQUIRE(unfolded.at(amphipod::Location::RightWall) == amphipod::Type::None);
  REQUIRE(unfolded.at(amphipod::Location::LeftCenter) == amphipod::Type::None);
}

TEST_CASE("generate paths") {
  using namespace amphipod;
  auto const paths =
      Organizer::generate_paths(Folded::rooms(), Folded::side_paths());

  REQUIRE(paths.size() == 56);

  REQUIRE(paths.contains({Location::A_Top, Location::LeftWall}));
  auto path = paths.at({Location::A_Top, Location::LeftWall});
  REQUIRE(path.cost == 3);
  REQUIRE(path.way.size() == 1);
  REQUIRE(path.way.at(0) == Location::LeftHall);

  REQUIRE(paths.contains({Location::A_Bottom, Location::RightWall}));
  path = paths.at({Location::A_Bottom, Location::RightWall});
  REQUIRE(path.cost == 10);
  REQUIRE(path.way.size() == 5);
  REQUIRE(path.way.at(0) == Location::A_Top);
  REQUIRE(path.way.at(1) == Location::LeftCenter);
  REQUIRE(path.way.at(2) == Location::Center);
  REQUIRE(path.way.at(3) == Location::RightCenter);
  REQUIRE(path.way.at(4) == Location::RightHall);

  REQUIRE(paths.contains({Location::C_Bottom, Location::LeftCenter}));
  path = paths.at({Location::C_Bottom, Location::LeftCenter});
  REQUIRE(path.cost == 5);
  REQUIRE(path.way.size() == 2);
  REQUIRE(path.way.at(0) == Location::C_Top);
  REQUIRE(path.way.at(1) == Location::Center);
}

TEST_CASE("generate paths unfolded") {
  using namespace amphipod;
  auto const paths =
      Organizer::generate_paths(Unfolded::rooms(), Unfolded::side_paths());

  REQUIRE(paths.size() == 16 * 7);

  REQUIRE(paths.contains({Location::C_Lower, Location::RightWall}));
  auto path = paths.at({Location::C_Lower, Location::RightWall});
  REQUIRE(path.cost == 7);
  REQUIRE(path.way.size() == 4);
  REQUIRE(path.way.at(0) == Location::C_Upper);
  REQUIRE(path.way.at(1) == Location::C_Top);
  REQUIRE(path.way.at(2) == Location::RightCenter);
  REQUIRE(path.way.at(3) == Location::RightHall);

  REQUIRE(paths.contains({Location::B_Bottom, Location::LeftCenter}));
  path = paths.at({Location::B_Bottom, Location::LeftCenter});
  REQUIRE(path.cost == 5);
  REQUIRE(path.way.size() == 3);
}

TEST_CASE("find valid moves") {
  using namespace amphipod;
  auto organizer = Organizer::createFolded();

  SUBCASE("already organized") {
    Burrow const burrow{Type::None, Type::None, Type::None, Type::None,
                        Type::None, Type::None, Type::None,

                        Type::A,    Type::B,    Type::C,    Type::D,
                        Type::A,    Type::B,    Type::C,    Type::D};

    auto const moves = organizer->valid_moves(burrow);

    REQUIRE(moves.size() == 0);
  }

  SUBCASE("last amphipod") {
    Burrow const burrow{Type::None, Type::None, Type::None, Type::None,
                        Type::None, Type::None, Type::A,

                        Type::None, Type::B,    Type::C,    Type::D,
                        Type::A,    Type::B,    Type::C,    Type::D};

    auto const moves = organizer->valid_moves(burrow);

    REQUIRE(moves.size() == 1);
    REQUIRE(moves.at(0).burrow.at(Location::A_Top) == Type::A);
    REQUIRE(moves.at(0).burrow.at(Location::RightWall) == Type::None);
  }

  SUBCASE("blocking another amphipod") {
    Burrow const burrow{Type::B,    Type::A,    Type::None, Type::None,
                        Type::None, Type::None, Type::None,

                        Type::None, Type::None, Type::C,    Type::D,
                        Type::A,    Type::B,    Type::C,    Type::D};

    auto const moves = organizer->valid_moves(burrow);

    REQUIRE(moves.size() == 1);
    REQUIRE(moves.at(0).burrow.at(Location::LeftWall) == Type::B);
    REQUIRE(moves.at(0).burrow.at(Location::LeftHall) == Type::None);
    REQUIRE(moves.at(0).burrow.at(Location::A_Top) == Type::A);
  }

  SUBCASE("get out of wrong room") {
    Burrow const burrow{Type::A,    Type::None, Type::None, Type::None,
                        Type::None, Type::None, Type::None,

                        Type::B,    Type::None, Type::C,    Type::D,
                        Type::A,    Type::B,    Type::C,    Type::D};

    auto const moves = organizer->valid_moves(burrow);

    REQUIRE(moves.size() == 6);

    REQUIRE(moves.at(0).burrow.at(Location::LeftWall) == Type::A);
    REQUIRE(moves.at(3).burrow.at(Location::LeftWall) == Type::A);
    REQUIRE(moves.at(5).burrow.at(Location::LeftWall) == Type::A);

    REQUIRE(moves.at(0).burrow.at(Location::A_Top) == Type::None);
    REQUIRE(moves.at(3).burrow.at(Location::A_Top) == Type::None);
    REQUIRE(moves.at(5).burrow.at(Location::A_Top) == Type::None);
  }

  SUBCASE("get out of the way") {
    Burrow const burrow{Type::None, Type::None, Type::None, Type::None,
                        Type::None, Type::None, Type::None,

                        Type::A,    Type::B,    Type::C,    Type::D,
                        Type::A,    Type::B,    Type::A,    Type::D};

    auto const moves = organizer->valid_moves(burrow);

    REQUIRE(moves.size() == 7);

    REQUIRE(moves.at(1).burrow.at(Location::C_Top) == Type::None);
    REQUIRE(moves.at(2).burrow.at(Location::C_Top) == Type::None);
    REQUIRE(moves.at(6).burrow.at(Location::C_Top) == Type::None);
  }
}

} // namespace
