# Budget discipline: an explicit max_evals must bound the true number of
# objective evaluations (counted by an external closure, not by the engine)
# up to at most one generation/population of slack, and the reported counts
# must equal the true count. A budget below one minimal generation is a clean,
# actionable error, never a silent overrun.

.counting_sphere <- function() {
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  list(
    counter = counter,
    fn = function(x) {
      counter$n <- counter$n + 1L
      sum(x^2)
    }
  )
}

test_that("base_optim honours an explicit max_evals", {
  h <- .counting_sphere()
  d <- 2L
  budget <- 50L
  prob <- optim_problem(
    fn = h$fn, space = space_box(rep(-5, d), rep(5, d)),
    seed = 1, max_evals = budget
  )
  res <- optimix(prob, method = "base_optim")
  expect_identical(res$counts[["function"]], h$counter$n)
  # Slack: one L-BFGS-B iteration costs about 2d + 1 evaluations.
  expect_lte(h$counter$n, budget + 2L * d + 1L)
})

test_that("deoptim honours an explicit max_evals", {
  skip_if_not_installed("DEoptim")
  h <- .counting_sphere()
  d <- 10L
  # The confirmed 42x-overrun case: this ran 2100 evaluations before the fix.
  budget <- 50L
  prob <- optim_problem(
    fn = h$fn, space = space_box(rep(-5, d), rep(5, d)),
    seed = 2, max_evals = budget
  )
  res <- optimix(prob, method = "deoptim")
  expect_identical(res$counts[["function"]], h$counter$n)
  # Slack: one population (at most the default NP = 10d).
  expect_lte(h$counter$n, budget + 10L * d)
})

test_that("deoptimr honours an explicit max_evals", {
  skip_if_not_installed("DEoptimR")
  h <- .counting_sphere()
  d <- 5L
  budget <- 50L
  prob <- optim_problem(
    fn = h$fn, space = space_box(rep(-5, d), rep(5, d)),
    seed = 3, max_evals = budget
  )
  res <- optimix(prob, method = "deoptimr")
  expect_identical(res$counts[["function"]], h$counter$n)
  expect_lte(h$counter$n, budget + 10L * d)
})

test_that("cmaes honours an explicit max_evals", {
  skip_if_not_installed("cmaes")
  h <- .counting_sphere()
  d <- 2L
  budget <- 60L
  prob <- optim_problem(
    fn = h$fn, space = space_box(rep(-5, d), rep(5, d)),
    seed = 4, max_evals = budget
  )
  res <- optimix(prob, method = "cmaes")
  expect_identical(res$counts[["function"]], h$counter$n)
  # Slack: one generation of lambda = 4 + floor(3 log d) offspring.
  expect_lte(h$counter$n, budget + 4L + as.integer(floor(3 * log(d))))
})

test_that("a budget below one minimal deoptim generation is a clean error", {
  skip_if_not_installed("DEoptim")
  expect_error(
    optimix(sphere, lower = rep(-5, 10), upper = rep(5, 10),
            method = "deoptim", max_evals = 7),
    "at least 8 evaluations"
  )
})

test_that("a budget below one minimal deoptimr generation is a clean error", {
  skip_if_not_installed("DEoptimR")
  expect_error(
    optimix(sphere, lower = rep(-5, 10), upper = rep(5, 10),
            method = "deoptimr", max_evals = 7),
    "at least 8 evaluations"
  )
})

test_that("a budget below one cmaes generation is a clean error", {
  skip_if_not_installed("cmaes")
  expect_error(
    optimix(sphere, lower = c(-5, -5), upper = c(5, 5),
            method = "cmaes", max_evals = 5),
    "at least 6 evaluations"
  )
})

test_that("the default budgets and floors are unchanged when max_evals is NA", {
  skip_if_not_installed("DEoptim")
  # Without an explicit budget the engine keeps its historical sizing (the
  # oracle suite pins the exact reproduction); this guards the NA branch of
  # the budget fit against accidental tightening.
  h <- .counting_sphere()
  prob <- optim_problem(
    fn = h$fn, space = space_box(c(-5, -5), c(5, 5)), seed = 6
  )
  res <- optimix(prob, method = "deoptim")
  # NP = 10d = 20, itermax = max(20, 400 %/% 20) = 20: 20 * 21 evaluations.
  expect_identical(h$counter$n, 420L)
  expect_identical(res$counts[["function"]], 420L)
})
