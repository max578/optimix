test_that("gensa minimises a quadratic", {
  skip_if_not_installed("GenSA")
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)), seed = 1
  )
  res <- optimix(prob, method = "gensa")
  expect_lt(res$value, 1e-2)
})

test_that("deoptim minimises a quadratic", {
  skip_if_not_installed("DEoptim")
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)), seed = 2
  )
  res <- optimix(prob, method = "deoptim")
  expect_lt(res$value, 1e-1)
})

test_that("nloptr DIRECT-L minimises a quadratic", {
  skip_if_not_installed("nloptr")
  res <- optimix(
    sphere,
    lower = c(-5, -5), upper = c(5, 5), method = "nloptr_directl"
  )
  expect_lt(res$value, 1e-2)
})

test_that("cmaes minimises a quadratic", {
  skip_if_not_installed("cmaes")
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)), seed = 3
  )
  res <- optimix(prob, method = "cmaes")
  expect_lt(res$value, 1e-2)
})

test_that("deoptimr minimises a quadratic", {
  skip_if_not_installed("DEoptimR")
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)), seed = 4
  )
  res <- optimix(prob, method = "deoptimr")
  expect_lt(res$value, 1e-1)
})

test_that("dfoptim Hooke-Jeeves minimises a quadratic", {
  skip_if_not_installed("dfoptim")
  res <- optimix(
    sphere,
    lower = c(-5, -5), upper = c(5, 5), method = "dfoptim_hjkb"
  )
  expect_lt(res$value, 1e-2)
})

test_that("nloptr BOBYQA minimises a quadratic", {
  skip_if_not_installed("nloptr")
  res <- optimix(
    sphere,
    lower = c(-5, -5), upper = c(5, 5), method = "nloptr_bobyqa"
  )
  expect_lt(res$value, 1e-2)
})

test_that("gensa is bitwise reproducible under a problem seed", {
  skip_if_not_installed("GenSA")
  # Guards the decision to keep the R-level seed alongside control$seed:
  # GenSA also draws from R's RNG stream, so control$seed alone is not
  # enough for bitwise reproducibility (probed 2026-07-05).
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)), seed = 21
  )
  set.seed(101)
  first <- optimix(prob, method = "gensa")
  set.seed(2024)
  second <- optimix(prob, method = "gensa")
  expect_identical(first$par, second$par)
})

test_that("bayesopt reports a failed GP fit truthfully", {
  skip_if_not_installed("DiceKriging")
  # max_evals = 3 in d = 2 leaves an initial design of two points, below
  # DiceKriging's minimum (rows must exceed the dimension), so the first GP
  # fit fails deterministically; the result must say so rather than claim a
  # completed EGO run.
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)),
    seed = 4, max_evals = 3
  )
  res <- optimix(prob, method = "bayesopt")
  expect_identical(res$convergence, 1L)
  expect_match(res$message, "GP fit failed at step 1")
  expect_identical(res$diagnostics$gp_failures, 1L)
})

test_that("bayesopt rejects a one-evaluation budget cleanly", {
  skip_if_not_installed("DiceKriging")
  expect_error(
    optimix(sphere, lower = c(-5, -5), upper = c(5, 5),
            method = "bayesopt", max_evals = 1),
    "at least 2"
  )
})

test_that("bayesopt survives a constant objective within budget", {
  skip_if_not_installed("DiceKriging")
  # A flat response drives every EI argmax to the same point; the duplicate
  # guard replaces it with a seeded uniform draw, so the design never
  # degenerates and the run ends at the budget, not in a km failure.
  prob <- optim_problem(
    fn = function(x) 1, space = space_box(c(-5, -5), c(5, 5)),
    seed = 6, max_evals = 10
  )
  res <- optimix(prob, method = "bayesopt")
  expect_identical(res$value, 1)
  expect_lte(res$counts[["function"]], 10L)
})

test_that("bayesopt (EGO) solves a quadratic in few evaluations", {
  skip_if_not_installed("DiceKriging")
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)),
    seed = 8, max_evals = 45
  )
  res <- optimix(prob, method = "bayesopt")
  expect_lt(res$value, 1e-1)
  expect_lte(res$counts[["function"]], 50)
})

test_that("deoptimr rejects a NaN objective value (documented behaviour, F10)", {
  skip_if_not_installed("DEoptimR")
  # The contract requires fn to return a single finite number, so NaN is a
  # contract violation; engines differ in their response, and the audit
  # decision F10 (FINDINGS_AND_FIX_PLAN.md) documents DEoptimR's: JDEoptim
  # hard-asserts a NaN-free fitness population rather than tolerating or
  # penalising it. Pinning the assertion text keeps any drift visible -- a
  # DEoptimR release change, or a future finite-guard wrapper silently
  # becoming the default.
  prob <- optim_problem(
    fn = function(x) NaN, space = space_box(c(-1, -1), c(1, 1)), seed = 1
  )
  expect_error(optimix(prob, method = "deoptimr"), "anyNA(fpop)", fixed = TRUE)
})

test_that("a seed makes a stochastic engine reproducible", {
  skip_if_not_installed("DEoptim")
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)), seed = 42
  )
  first <- optimix(prob, method = "deoptim")
  second <- optimix(prob, method = "deoptim")
  expect_equal(first$par, second$par)
})

test_that("a pinned box dimension is handled by every engine", {
  # Minimum at (0, 0.3, -0.4); the first coordinate is pinned at 0 by its
  # equal bounds, so the optimiser must solve only the two free coordinates.
  shifted <- function(x) sum((x - c(0, 0.3, -0.4))^2)
  for (m in c("base_optim", "auto")) {
    res <- optimix(shifted, c(0, -5, -5), c(0, 5, 5), method = m)
    expect_equal(res$par[[1L]], 0)
    expect_lt(res$value, 1e-3)
  }
})

test_that("a fully pinned box returns the single feasible point", {
  res <- optimix(sphere, c(2, -1), c(2, -1), method = "base_optim")
  expect_equal(res$par, c(2, -1))
  expect_equal(res$value, sphere(c(2, -1)))
  expect_equal(res$convergence, 0L)
  expect_identical(res$provenance$engine, "degenerate")
})

test_that("the pinned reduction leaves a non-degenerate fit untouched", {
  # The reduction is a pure restriction of the same objective, so a problem
  # with no pinned coordinate must return exactly what direct dispatch gives.
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)), seed = 11L
  )
  with_reduction <- optimix(prob, method = "base_optim")
  reference <- optimix:::.run_base_optim(prob)
  expect_identical(with_reduction$par, reference$par)
  expect_identical(with_reduction$value, reference$value)
})
