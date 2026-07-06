# Sign-duality metamorphic invariant: minimising f and maximising -f are the
# same search, so a seeded run of each (same seed, same box, same budget) must
# return the same argmin and opposite best values. One test per box engine, so
# a missing backend skips that engine cleanly instead of hiding inside a loop.
# Two box engines are deliberately excluded: bayesopt (its per-step EI
# acquisition loop is far too slow for a metamorphic sweep) and proxymix_map
# (a mixture-valued mapper, not a point solver, so par-level duality is not
# its contract).

.expect_sign_duality <- function(engine) {
  f <- function(x) sum((x - 1)^2)
  neg_f <- function(x) -sum((x - 1)^2)
  lower <- c(-4, -4)
  upper <- c(4, 4)
  # max_evals = 400 keeps each engine quick at d = 2 while clearing every
  # engine's floor (the DE family needs >= 8 evaluations, cmaes one full
  # generation of lambda = 6).
  res_min <- optimix(
    optim_problem(
      fn = f, space = space_box(lower, upper), seed = 42L, max_evals = 400L
    ),
    method = engine
  )
  res_max <- optimix(
    optim_problem(
      fn = neg_f, space = space_box(lower, upper), maximise = TRUE,
      seed = 42L, max_evals = 400L
    ),
    method = engine
  )
  expect_equal(res_min$par, res_max$par, tolerance = 1e-6)
  expect_equal(res_min$value, -res_max$value, tolerance = 1e-8)
}

test_that("sign duality holds for base_optim", {
  .expect_sign_duality("base_optim")
})

test_that("sign duality holds for gensa", {
  skip_if_not_installed("GenSA")
  .expect_sign_duality("gensa")
})

test_that("sign duality holds for deoptim", {
  skip_if_not_installed("DEoptim")
  .expect_sign_duality("deoptim")
})

test_that("sign duality holds for deoptimr", {
  skip_if_not_installed("DEoptimR")
  .expect_sign_duality("deoptimr")
})

test_that("sign duality holds for nloptr_directl", {
  skip_if_not_installed("nloptr")
  .expect_sign_duality("nloptr_directl")
})

test_that("sign duality holds for nloptr_bobyqa", {
  skip_if_not_installed("nloptr")
  .expect_sign_duality("nloptr_bobyqa")
})

test_that("sign duality holds for dfoptim_hjkb", {
  skip_if_not_installed("dfoptim")
  .expect_sign_duality("dfoptim_hjkb")
})

test_that("sign duality holds for cmaes", {
  skip_if_not_installed("cmaes")
  .expect_sign_duality("cmaes")
})
