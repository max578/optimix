# Independent-Oracle gate: each adapter must reproduce the result of calling its
# source package directly with the same settings (to <= 1e-8 on a shared grid),
# so the bake-off later compares correct wrappers, not wrapper bugs.

test_that("the gensa adapter reproduces GenSA::GenSA", {
  skip_if_not_installed("GenSA")
  lower <- c(-5, -5)
  upper <- c(4, 6)
  prob <- optim_problem(
    fn = sphere, space = space_box(lower, upper), seed = 7
  )
  res <- optimix(prob, method = "gensa")
  set.seed(7)
  direct <- GenSA::GenSA(
    par = NULL, fn = sphere, lower = lower, upper = upper,
    control = list(max.call = 1000, seed = 7)
  )
  expect_equal(res$value, direct$value, tolerance = 1e-8)
  expect_equal(res$par, as.numeric(direct$par), tolerance = 1e-8)
})

test_that("the deoptim adapter reproduces DEoptim::DEoptim", {
  skip_if_not_installed("DEoptim")
  lower <- c(-5, -5)
  upper <- c(4, 6)
  d <- 2L
  np <- 10L * d
  itermax <- max(20L, (200L * d) %/% np)
  prob <- optim_problem(
    fn = sphere, space = space_box(lower, upper), seed = 11
  )
  res <- optimix(prob, method = "deoptim")
  set.seed(11)
  direct <- DEoptim::DEoptim(
    fn = sphere, lower = lower, upper = upper,
    control = DEoptim::DEoptim.control(NP = np, itermax = itermax, trace = FALSE)
  )
  expect_equal(
    unname(res$par), unname(as.numeric(direct$optim$bestmem)),
    tolerance = 1e-8
  )
  expect_equal(res$value, direct$optim$bestval, tolerance = 1e-8)
})

test_that("the nloptr adapter reproduces nloptr::nloptr DIRECT-L", {
  skip_if_not_installed("nloptr")
  lower <- c(-5, -5)
  upper <- c(4, 6)
  start <- (lower + upper) / 2
  prob <- optim_problem(fn = sphere, space = space_box(lower, upper))
  res <- optimix(prob, method = "nloptr_directl")
  direct <- nloptr::nloptr(
    x0 = start, eval_f = sphere, lb = lower, ub = upper,
    opts = list(algorithm = "NLOPT_GN_DIRECT_L", maxeval = 1000, xtol_rel = 1e-8)
  )
  expect_equal(res$value, direct$objective, tolerance = 1e-8)
  expect_equal(res$par, direct$solution, tolerance = 1e-8)
})

test_that("the cmaes adapter reproduces cmaes::cma_es", {
  skip_if_not_installed("cmaes")
  lower <- c(-5, -5)
  upper <- c(4, 6)
  start <- (lower + upper) / 2
  lambda <- 4L + floor(3 * log(2))
  maxit <- max(10L, 1000L %/% lambda)
  prob <- optim_problem(fn = sphere, space = space_box(lower, upper), seed = 5)
  res <- optimix(prob, method = "cmaes")
  set.seed(5)
  direct <- cmaes::cma_es(
    par = start, fn = sphere, lower = lower, upper = upper,
    control = list(maxit = maxit)
  )
  expect_equal(res$value, direct$value, tolerance = 1e-8)
  expect_equal(res$par, as.numeric(direct$par), tolerance = 1e-8)
})

test_that("the deoptimr adapter reproduces DEoptimR::JDEoptim", {
  skip_if_not_installed("DEoptimR")
  lower <- c(-5, -5)
  upper <- c(4, 6)
  np <- 20L
  maxiter <- max(20L, (200L * 2L) %/% np)
  prob <- optim_problem(fn = sphere, space = space_box(lower, upper), seed = 9)
  res <- optimix(prob, method = "deoptimr")
  set.seed(9)
  direct <- suppressWarnings(DEoptimR::JDEoptim(
    lower = lower, upper = upper, fn = sphere,
    NP = np, maxiter = maxiter, trace = FALSE
  ))
  expect_equal(res$par, as.numeric(direct$par), tolerance = 1e-8)
  expect_equal(res$value, direct$value, tolerance = 1e-8)
})

test_that("the dfoptim adapter reproduces dfoptim::hjkb", {
  skip_if_not_installed("dfoptim")
  lower <- c(-5, -5)
  upper <- c(4, 6)
  start <- (lower + upper) / 2
  prob <- optim_problem(fn = sphere, space = space_box(lower, upper))
  res <- optimix(prob, method = "dfoptim_hjkb")
  direct <- dfoptim::hjkb(
    par = start, fn = sphere, lower = lower, upper = upper,
    control = list(maxfeval = 1000, info = FALSE)
  )
  expect_equal(res$value, direct$value, tolerance = 1e-8)
  expect_equal(res$par, as.numeric(direct$par), tolerance = 1e-8)
})

test_that("the bobyqa adapter reproduces nloptr::nloptr BOBYQA", {
  skip_if_not_installed("nloptr")
  lower <- c(-5, -5)
  upper <- c(4, 6)
  start <- (lower + upper) / 2
  prob <- optim_problem(fn = sphere, space = space_box(lower, upper))
  res <- optimix(prob, method = "nloptr_bobyqa")
  direct <- nloptr::nloptr(
    x0 = start, eval_f = sphere, lb = lower, ub = upper,
    opts = list(algorithm = "NLOPT_LN_BOBYQA", maxeval = 1000, xtol_rel = 1e-8)
  )
  expect_equal(res$value, direct$objective, tolerance = 1e-8)
  expect_equal(res$par, direct$solution, tolerance = 1e-8)
})

test_that("a smoof sphere is solved when smoof is installed", {
  skip_if_not_installed("smoof")
  skip_if_not_installed("GenSA")
  fn <- smoof::makeSphereFunction(dimensions = 2L)
  lower <- smoof::getLowerBoxConstraints(fn)
  upper <- smoof::getUpperBoxConstraints(fn)
  res <- optimix(
    function(x) fn(x),
    lower = lower, upper = upper, method = "gensa"
  )
  expect_lt(res$value, 1e-1)
})
