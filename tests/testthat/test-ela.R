test_that("ELA features separate a smooth quadratic from a rugged landscape", {
  lower <- c(-5, -5)
  upper <- c(5, 5)
  set.seed(1)
  smooth <- optim_problem(fn = sphere, space = space_box(lower, upper))
  s_smooth <- .ela_sample(smooth, .ela_size(2))
  f_smooth <- .ela_features(s_smooth$X, s_smooth$y)
  rugged <- optim_problem(fn = rastrigin, space = space_box(lower, upper))
  s_rugged <- .ela_sample(rugged, .ela_size(2))
  f_rugged <- .ela_features(s_rugged$X, s_rugged$y)
  expect_gt(f_smooth[["quad_r2"]], 0.97)
  expect_lt(f_rugged[["quad_r2"]], f_smooth[["quad_r2"]])
})

test_that("the Latin-hypercube design stays within the box", {
  design <- .ela_lhs(50, c(-1, -2), c(1, 2))
  expect_true(all(design[, 1] >= -1 & design[, 1] <= 1))
  expect_true(all(design[, 2] >= -2 & design[, 2] <= 2))
  expect_equal(nrow(design), 50L)
})

test_that("the ELA sample's best point follows the problem orientation", {
  lower <- c(-3, -3)
  upper <- c(3, 3)
  set.seed(42)
  min_prob <- optim_problem(fn = sphere, space = space_box(lower, upper))
  s_min <- .ela_sample(min_prob, 60L)
  set.seed(42)
  max_prob <- optim_problem(
    fn = sphere, space = space_box(lower, upper), maximise = TRUE
  )
  s_max <- .ela_sample(max_prob, 60L)
  # Same seed, so the two problems share one design; only the orientation
  # of `best` may differ. Re-evaluating the objective at each `best` checks
  # the row against the sampled values it is supposed to summarise.
  expect_identical(s_min$X, s_max$X)
  expect_equal(sphere(s_min$best), min(s_min$y))
  expect_equal(sphere(s_max$best), max(s_max$y))
  expect_gt(sphere(s_max$best), sphere(s_min$best))
})
