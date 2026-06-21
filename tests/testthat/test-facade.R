test_that("auto returns an optim-compatible result from an installed engine", {
  res <- optimix(sphere, lower = c(-5, -4), upper = c(4, 5))
  expect_s3_class(res, "optimix_result")
  expect_true(
    all(c("par", "value", "counts", "convergence", "message") %in% names(res))
  )
  expect_lt(res$value, 1e-2)
  expect_false(is.null(res$provenance$engine))
})

test_that("coercers and accessors behave", {
  res <- optimix(sphere, c(-5, -5), c(4, 4), method = "base_optim")
  expect_equal(coef(res), res$par)
  bare <- as_optim(res)
  expect_true(
    all(c("par", "value", "counts", "convergence", "message") %in% names(bare))
  )
  frame <- as.data.frame(res)
  expect_s3_class(frame, "data.frame")
  expect_true(all(c("x1", "x2", "value", "optimiser") %in% names(frame)))
})

test_that("print and summary emit their headers", {
  res <- optimix(sphere, c(-5, -5), c(4, 4), method = "base_optim")
  expect_output(print(res), "optimix_result")
  expect_output(summary(res), "Optimisation result")
})

test_that("unknown and unavailable engines fail with guidance", {
  expect_error(
    optimix(sphere, c(-1, -1), c(1, 1), method = "nope"),
    "Unknown optimiser"
  )
})
