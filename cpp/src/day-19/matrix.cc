#include "matrix.h"

#include <numeric>

namespace matrix {

namespace {

constexpr std::array<Matrix, 3> A{{

    {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}},
    {{{0, 1, 0}, {0, 0, 1}, {1, 0, 0}}},
    {{{0, 0, 1}, {1, 0, 0}, {0, 1, 0}}}

}};

constexpr std::array<Matrix, 4> B{{

    {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}},
    {{{-1, 0, 0}, {0, -1, 0}, {0, 0, 1}}},
    {{{-1, 0, 0}, {0, 1, 0}, {0, 0, -1}}},
    {{{1, 0, 0}, {0, -1, 0}, {0, 0, -1}}}

}};

constexpr std::array<Matrix, 2> C{{

    {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}},
    {{{0, 0, -1}, {0, -1, 0}, {-1, 0, 0}}},

}};

} // namespace

Vector mult(Matrix const &m, Vector const &v) {
  Vector result{};
  for (std::size_t i = 0; i < 3; ++i) {
    for (std::size_t j = 0; j < 3; ++j) {
      result[i] += m[i][j] * v[j];
    }
  }
  return result;
}

Matrix mult(Matrix const &a, Matrix const &b) {
  Matrix result{};
  for (std::size_t i = 0; i < 3; ++i) {
    for (std::size_t j = 0; j < 3; ++j) {
      for (std::size_t k = 0; k < 3; ++k) {
        result[i][j] += a[j][k] * b[k][i];
      }
    }
  }
  return result;
}

Vector add(Vector const &a, Vector const &b) {
  return Vector{a[0] + b[0], a[1] + b[1], a[2] + b[2]};
}

std::vector<Matrix> rotation_matrices() {
  std::vector<Matrix> matrices{};
  for (auto const &a : A) {
    for (auto const &b : B) {
      for (auto const &c : C) {
        matrices.push_back(mult(mult(a, b), c));
      }
    }
  }
  return matrices;
}

int manhatten_distance(Vector const &a, Vector const &b) {
  return std::inner_product(a.begin(), a.end(), b.begin(), 0, std::plus<>(),
                            [](auto p, auto q) { return std::abs(p - q); });
}

} // namespace matrix
