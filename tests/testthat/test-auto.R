test_that("auto races on a smooth quadratic and solves it (tier 3)", {
  skip_if_not_installed("nloptr")
  skip_if_not_installed("GenSA")
  res <- optimix(sphere, lower = rep(-5, 5), upper = rep(5, 5))
  expect_equal(res$provenance$tier, 3L)
  expect_lt(res$value, 1e-4)
})

test_that("auto races on a rugged landscape (tier 3)", {
  skip_if_not_installed("GenSA")
  skip_if_not_installed("DEoptimR")
  res <- optimix(rastrigin, lower = rep(-5.12, 5), upper = rep(5.12, 5))
  expect_equal(res$provenance$tier, 3L)
  expect_gte(length(res$provenance$race), 2L)
})

test_that("auto routes an expensive objective to the surrogate engine", {
  skip_if_not_installed("DiceKriging")
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-5, -5), c(5, 5)),
    objective = objective_expensive(), max_evals = 45
  )
  res <- optimix(prob, method = "auto")
  expect_equal(res$provenance$engine, "bayesopt")
})

test_that("the race method returns a tier-3 result", {
  skip_if_not_installed("GenSA")
  skip_if_not_installed("DEoptimR")
  res <- optimix(sphere, lower = c(-5, -5), upper = c(5, 5), method = "race")
  expect_equal(res$provenance$tier, 3L)
  expect_lt(res$value, 1e-1)
})

test_that("auto falls back from the surrogate when the problem is too high-dimensional", {
  skip_if_not_installed("GenSA")
  prob <- optim_problem(
    fn = function(x) sum(x^2), space = space_box(rep(-5, 20), rep(5, 20)),
    objective = objective_expensive(), max_evals = 200
  )
  res <- optimix(prob, method = "auto")
  expect_false(identical(res$provenance$engine, "bayesopt"))
  expect_true(res$provenance$engine %in%
    c("gensa", "deoptimr", "deoptim", "base_optim"))
})

test_that("the race method rejects a non-box design space cleanly", {
  prob <- optim_problem(
    fn = function(p) sum(abs(diff(p))),
    space = space_permutation(6L), max_evals = 200
  )
  expect_error(optimix(prob, method = "race"), "box design space")
})

test_that("the auto race stays within an explicit evaluation budget", {
  skip_if_not_installed("GenSA")
  skip_if_not_installed("nloptr")
  counter <- new.env()
  counter$n <- 0L
  fn <- function(x) {
    counter$n <- counter$n + 1L
    sum((x - 1)^2)
  }
  set.seed(11)
  prob <- optim_problem(
    fn = fn, space = space_box(lower = c(-5, -5), upper = c(5, 5)),
    max_evals = 400, seed = 11
  )
  res <- optimix(prob, method = "auto")
  # The budget is above the tier-3 gate (6 * 50 ELA points), so the race
  # fires; race trials + ELA sample + final run must fit inside max_evals,
  # and the reported count must equal the true number of objective calls.
  expect_identical(res$provenance$tier, 3L)
  expect_lte(counter$n, 400L)
  expect_equal(res$counts[["function"]], counter$n)
})

test_that("an explicit race under a too-small budget errors actionably", {
  skip_if_not_installed("GenSA")
  skip_if_not_installed("nloptr")
  expect_error(
    optimix(sphere, lower = c(-5, -5), upper = c(5, 5), method = "race",
            max_evals = 10),
    "Raise `max_evals`",
    fixed = TRUE
  )
})

test_that("auto recovers a known maximum through the ELA-seeded race", {
  skip_if_not_installed("GenSA")
  skip_if_not_installed("nloptr")
  set.seed(21)
  prob <- optim_problem(
    fn = function(x) -sum((x - 2)^2),
    space = space_box(lower = c(-5, -5), upper = c(5, 5)),
    maximise = TRUE, max_evals = 400, seed = 21
  )
  res <- optimix(prob, method = "auto")
  expect_identical(res$provenance$tier, 3L)
  expect_equal(res$par, c(2, 2), tolerance = 1e-2)
  expect_gt(res$value, -1e-3)
})

test_that("a failing engine is dropped from the race and recorded", {
  skip_if_not_installed("nloptr")
  register_optimiser(optim_engine(
    name = "test_exploder", pkg = "base", accepts = "space_box",
    global = TRUE, available = function() TRUE,
    run = function(problem) stop("deliberate test failure")
  ))
  on.exit(rm(list = "test_exploder", envir = .engine_registry), add = TRUE)
  prob <- optim_problem(
    fn = shifted_sphere, space = space_box(lower = c(-5, -5), upper = c(5, 5)),
    max_evals = 200, seed = 7
  )
  plan <- list(
    race = c("nloptr_bobyqa", "test_exploder"),
    ela = list(best = c(0, 0)), n_ela = 0L, features = NULL,
    why = "unit race"
  )
  res <- .run_race(prob, plan)
  expect_identical(res$provenance$engine, "nloptr_bobyqa")
  expect_match(
    res$provenance$why,
    "test_exploder failed during the race: deliberate test failure",
    fixed = TRUE
  )
  expect_named(res$provenance$race, "nloptr_bobyqa")
})

test_that("the race re-throws cleanly when every trial engine fails", {
  for (nm in c("test_exploder_a", "test_exploder_b")) {
    register_optimiser(optim_engine(
      name = nm, pkg = "base", accepts = "space_box",
      global = TRUE, available = function() TRUE,
      run = function(problem) stop("deliberate test failure")
    ))
  }
  on.exit(
    rm(list = c("test_exploder_a", "test_exploder_b"),
       envir = .engine_registry),
    add = TRUE
  )
  prob <- optim_problem(
    fn = sphere, space = space_box(lower = c(-5, -5), upper = c(5, 5)),
    max_evals = 200
  )
  plan <- list(
    race = c("test_exploder_a", "test_exploder_b"),
    ela = list(best = c(0, 0)), n_ela = 0L, features = NULL,
    why = "unit race"
  )
  expect_error(.run_race(prob, plan), "Every raced engine failed")
})

test_that("every auto plan carries its selection tier", {
  lower <- c(-5, -5)
  upper <- c(5, 5)
  # A noisy objective takes the tier-1 rule route.
  noisy <- optim_problem(
    fn = sphere, space = space_box(lower = lower, upper = upper),
    objective = objective_noisy(sd = 0.1), max_evals = 400
  )
  expect_identical(.auto_select(noisy)$tier, 1L)
  # A budget under the tier-3 gate takes the tier-1 rule route too.
  small <- optim_problem(
    fn = sphere, space = space_box(lower = lower, upper = upper),
    max_evals = 60
  )
  expect_identical(.auto_select(small)$tier, 1L)
  # An ample budget on a deterministic box problem escalates to the race.
  skip_if_not_installed("GenSA")
  skip_if_not_installed("nloptr")
  set.seed(5)
  big <- optim_problem(
    fn = sphere, space = space_box(lower = lower, upper = upper),
    max_evals = 400
  )
  expect_identical(.auto_select(big)$tier, 3L)
})
