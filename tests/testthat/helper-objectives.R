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
