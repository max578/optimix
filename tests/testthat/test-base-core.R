test_that("the base core minimises a quadratic with no Suggests needed", {
  res <- optimix(
    sphere,
    lower = c(-5, -5), upper = c(4, 4),
    method = "base_optim"
  )
  expect_s3_class(res, "optimix_result")
  expect_lt(res$value, 1e-6)
  expect_true(all(abs(res$par) < 1e-3))
  expect_equal(res$convergence, 0L)
})

test_that("maximise flips the sign correctly", {
  res <- optimix(
    function(x) -sum(x^2),
    lower = c(-5, -5), upper = c(4, 4),
    method = "base_optim", maximise = TRUE
  )
  expect_gt(res$value, -1e-6)
  expect_true(all(abs(res$par) < 1e-3))
})

test_that("dots are forwarded to the objective", {
  with_shift <- function(x, shift) sum((x - shift)^2)
  res <- optimix(
    with_shift,
    lower = c(-5, -5), upper = c(5, 5), shift = 2,
    method = "base_optim"
  )
  expect_true(all(abs(res$par - 2) < 1e-3))
})
