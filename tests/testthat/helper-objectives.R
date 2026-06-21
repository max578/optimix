# Shared test objectives: simple deterministic functions with known minima.

sphere <- function(x) {
  sum(x^2)
}

shifted_sphere <- function(x) {
  sum((x - 1)^2)
}

rastrigin <- function(x) {
  10 * length(x) + sum(x^2 - 10 * cos(2 * pi * x))
}

# A permutation cost (minimised by the identity permutation, value 0) and its
# exact swap-delta, for testing the combinatorial engine and the delta_eval hook.
sort_cost <- function(perm) {
  sum(abs(perm - seq_along(perm)))
}

sort_delta <- function(perm, i, j) {
  (abs(perm[j] - i) + abs(perm[i] - j)) -
    (abs(perm[i] - i) + abs(perm[j] - j))
}
