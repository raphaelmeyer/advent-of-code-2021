#pragma once

#include <array>
#include <vector>

namespace matrix {

using Matrix = std::array<std::array<int, 3>, 3>;
using Vector = std::array<int, 3>;

Vector mult(Matrix const &m, Vector const &v);
Matrix mult(Matrix const &a, Matrix const &b);

Vector add(Vector const &a, Vector const &b);

int manhatten_distance(Vector const &a, Vector const &b);

std::vector<Matrix> rotation_matrices();

} // namespace matrix
