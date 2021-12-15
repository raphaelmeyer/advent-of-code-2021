#include <iostream>

#include "chiton.h"

int main() {
  auto const risk_map = chiton::parse_file("data/day-15.txt");

  auto const risk_level = chiton::risk_level(risk_map);

  auto const full_map = chiton::make_full_map(risk_map);
  auto const full_risk_level = chiton::risk_level(full_map);

  std::cout << "Part 1: " << risk_level << "\n";
  std::cout << "Part 2: " << full_risk_level << "\n";
}