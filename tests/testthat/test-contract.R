test_that("space_box validates its bounds", {
  expect_error(space_box(lower = c(0, 0), upper = 1), "same length")
  expect_error(space_box(lower = c(2, 0), upper = c(1, 1)), "less than or equal")
  expect_error(space_box(lower = numeric(0), upper = numeric(0)), "at least one")
  sp <- space_box(lower = c(-1, -1), upper = c(1, 1))
  expect_equal(.space_dim(sp), 2L)
})

test_that("objective specifications construct as declared", {
  expect_equal(objective_deterministic()@kind, "deterministic")
  noisy <- objective_noisy(sd = 0.2)
  expect_equal(noisy@kind, "noisy")
  expect_equal(noisy@noise_sd, 0.2)
  expect_equal(objective_expensive()@kind, "expensive")
})

test_that("optim_problem validates warm_start length", {
  expect_error(
    optim_problem(
      fn = sphere,
      space = space_box(c(-1, -1), c(1, 1)),
      warm_start = c(0, 0, 0)
    ),
    "warm_start"
  )
  prob <- optim_problem(fn = sphere, space = space_box(c(-1, -1), c(1, 1)))
  expect_true(S7::S7_inherits(prob, optim_problem))
  expect_equal(prob@objective@kind, "deterministic")
})

test_that("optim_problem rejects an out-of-bounds warm start on a box", {
  sp <- space_box(c(-1, -1), c(1, 1))
  expect_error(
    optim_problem(fn = sphere, space = sp, warm_start = c(0, 2)),
    "warm_start\\[2\\]"
  )
  expect_error(
    optim_problem(fn = sphere, space = sp, warm_start = c(-1.5, 0)),
    "warm_start\\[1\\]"
  )
  expect_error(
    optim_problem(fn = sphere, space = sp, warm_start = c(NA_real_, 0)),
    "warm_start"
  )
  prob <- optim_problem(fn = sphere, space = sp, warm_start = c(0.5, -0.5))
  expect_equal(prob@warm_start, c(0.5, -0.5))
  edge <- optim_problem(fn = sphere, space = sp, warm_start = c(-1, 1))
  expect_equal(edge@warm_start, c(-1, 1))
})

test_that("objective specifications reject a negative noise sd", {
  expect_error(objective_noisy(sd = -1), "non-negative")
  expect_error(objective_spec(kind = "noisy", noise_sd = -0.5), "non-negative")
  expect_true(is.na(objective_noisy()@noise_sd))
})
