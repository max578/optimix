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
