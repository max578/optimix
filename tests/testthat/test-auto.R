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
