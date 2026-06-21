test_that("optimix_map finds both optima of a double well (native)", {
  res <- optimix_map(
    function(x) (x[1]^2 - 1)^2 + x[2]^2,
    lower = c(-2, -2), upper = c(2, 2)
  )
  expect_s3_class(res, "optimix_map")
  expect_gte(res$n, 2L)
  best2 <- res$modes[1:2, , drop = FALSE]
  expect_true(all(abs(abs(best2[, 1]) - 1) < 0.1))
  expect_true(all(abs(best2[, 2]) < 0.1))
})

test_that("optimix_map collapses a single well to one optimum", {
  res <- optimix_map(sphere, lower = c(-3, -3), upper = c(3, 3))
  expect_equal(res$n, 1L)
  expect_true(all(abs(res$modes[1, ]) < 0.05))
})

test_that("optimix_map prints", {
  res <- optimix_map(sphere, lower = c(-2, -2), upper = c(2, 2))
  expect_output(print(res), "optimix_map")
})
