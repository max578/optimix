test_that("gensa minimises a quadratic", {
  skip_if_not_installed("GenSA")
  res <- optimix(sphere, lower = c(-5, -5), upper = c(5, 5), method = "gensa")
  expect_lt(res$value, 1e-2)
})

test_that("deoptim minimises a quadratic", {
  skip_if_not_installed("DEoptim")
  res <- optimix(sphere, lower = c(-5, -5), upper = c(5, 5), method = "deoptim")
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
  res <- optimix(sphere, lower = c(-5, -5), upper = c(5, 5), method = "cmaes")
  expect_lt(res$value, 1e-2)
})

test_that("deoptimr minimises a quadratic", {
  skip_if_not_installed("DEoptimR")
  res <- optimix(sphere, lower = c(-5, -5), upper = c(5, 5), method = "deoptimr")
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

test_that("cmaes_ipop (restart CMA) minimises a quadratic", {
  skip_if_not_installed("cmaes")
  res <- optimix(
    sphere,
    lower = c(-5, -5), upper = c(5, 5), method = "cmaes_ipop"
  )
  expect_lt(res$value, 1e-2)
})

test_that("cmaes_ipop is reproducible under a seed", {
  skip_if_not_installed("cmaes")
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)), seed = 3
  )
  expect_equal(
    optimix(prob, method = "cmaes_ipop")$par,
    optimix(prob, method = "cmaes_ipop")$par
  )
})

test_that("bayesopt (EGO) solves a quadratic in few evaluations", {
  skip_if_not_installed("DiceKriging")
  res <- optimix(
    sphere,
    lower = c(-5, -5), upper = c(5, 5), method = "bayesopt", max_evals = 45
  )
  expect_lt(res$value, 1e-1)
  expect_lte(res$counts[["function"]], 50)
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
