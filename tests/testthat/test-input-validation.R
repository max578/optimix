# Input-validation contract: unexpected or wrong-type arguments must be caught
# with a caller-facing error, never accepted silently. These guard the gaps the
# corner-to-corner conformance sweep surfaced.

test_that("the facade rejects a non-logical maximise", {
  f <- function(x) sum(x^2)
  expect_error(optimix(f, c(-1, -1), c(1, 1), method = "base_optim", maximise = "yes"),
               "maximise")
  expect_error(optimix(f, c(-1, -1), c(1, 1), method = "base_optim",
                       maximise = c(TRUE, FALSE)), "maximise")
})

test_that("the facade rejects a non-numeric max_evals", {
  f <- function(x) sum(x^2)
  expect_error(optimix(f, c(-1, -1), c(1, 1), method = "base_optim", max_evals = "lots"),
               "max_evals")
})

test_that("method must be a single string", {
  f <- function(x) sum(x^2)
  expect_error(optimix(f, c(-1, -1), c(1, 1), method = 42), "method")
  expect_error(optimix(f, c(-1, -1), c(1, 1), method = c("gensa", "cmaes")), "method")
})

test_that("space_permutation requires a whole number", {
  expect_error(space_permutation(2.5), "whole number")
  expect_true(S7::S7_inherits(space_permutation(5), space_permutation))
})

test_that("optimix_map validates n_starts and tol", {
  f <- function(x) sum((x - 0.3)^2)
  expect_error(optimix_map(f, c(-2, -2), c(2, 2), n_starts = "many"), "n_starts")
  expect_error(optimix_map(f, c(-2, -2), c(2, 2), tol = "x"), "tol")
})
