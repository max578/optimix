# The native mapper: multi-start local search + clustering. Tests marked
# "(native)" mock .proxymix_ready to FALSE so they exercise the native branch
# even on machines where proxymix is installed; the "default path" test takes
# whichever premium path is available.

test_that("optimix_map finds both optima of a double well (native)", {
  testthat::local_mocked_bindings(
    .proxymix_ready = function() FALSE, .package = "optimix"
  )
  set.seed(101)
  res <- optimix_map(
    function(x) (x[1]^2 - 1)^2 + x[2]^2,
    lower = c(-2, -2), upper = c(2, 2)
  )
  expect_s3_class(res, "optimix_map")
  expect_identical(res$source, "native")
  expect_gte(res$n, 2L)
  best2 <- res$modes[1:2, , drop = FALSE]
  expect_true(all(abs(abs(best2[, 1]) - 1) < 0.1))
  expect_true(all(abs(best2[, 2]) < 0.1))
})

test_that("the native map keeps d = 1 optima as rows (regression)", {
  # The k x 1 centroid matrix used to collapse to 1 x k, so the k distinct
  # optima of a one-dimensional multimodal objective were reported as the
  # coordinates of one bogus optimum.
  testthat::local_mocked_bindings(
    .proxymix_ready = function() FALSE, .package = "optimix"
  )
  set.seed(102)
  res <- optimix_map(function(x) (x[1]^2 - 1)^2, lower = -2, upper = 2)
  expect_identical(res$source, "native")
  expect_gte(res$n, 2L)
  expect_true(is.matrix(res$modes))
  expect_identical(ncol(res$modes), 1L)
  expect_identical(nrow(res$modes), res$n)
  locs <- sort(res$modes[1:2, 1])
  expect_lt(abs(locs[1] + 1), 0.1)
  expect_lt(abs(locs[2] - 1), 0.1)
})

test_that(".cluster_optima separates two clusters and merges duplicates", {
  # Two tight three-point clouds in 2-d resolve to their two centroids.
  pts <- rbind(
    c(0.00, 0.00), c(0.02, 0.00), c(0.00, 0.02),
    c(1.00, 1.00), c(1.02, 1.00), c(1.00, 1.02)
  )
  cent <- optimix:::.cluster_optima(
    pts, lower = c(0, 0), upper = c(2, 2), tol = 0.05
  )
  expect_identical(dim(cent), c(2L, 2L))
  cent <- cent[order(cent[, 1]), , drop = FALSE]
  expect_equal(cent[1, ], colMeans(pts[1:3, , drop = FALSE]))
  expect_equal(cent[2, ], colMeans(pts[4:6, , drop = FALSE]))

  # All-duplicated points collapse to a single row.
  dup <- matrix(rep(c(0.5, -0.5), each = 4L), ncol = 2L)
  one <- optimix:::.cluster_optima(
    dup, lower = c(-1, -1), upper = c(1, 1), tol = 0.05
  )
  expect_identical(dim(one), c(1L, 2L))
  expect_equal(one[1, ], c(0.5, -0.5))
})

test_that("optimix_map collapses a single well to one optimum (native)", {
  testthat::local_mocked_bindings(
    .proxymix_ready = function() FALSE, .package = "optimix"
  )
  set.seed(103)
  res <- optimix_map(sphere, lower = c(-3, -3), upper = c(3, 3))
  expect_equal(res$n, 1L)
  expect_true(all(abs(res$modes[1, ]) < 0.05))
})

test_that("optimix_map resolves a double well on the default path", {
  # Unmocked: takes the proxymix branch when it is available, the native one
  # otherwise. Either way the best mode must be a genuine optimum.
  set.seed(104)
  res <- optimix_map(
    function(x) (x[1]^2 - 1)^2 + x[2]^2,
    lower = c(-2, -2), upper = c(2, 2)
  )
  expect_s3_class(res, "optimix_map")
  expect_true(res$source %in% c("native", "proxymix"))
  expect_gte(res$n, 1L)
  expect_lt(res$values[1], 0.05)
  expect_lt(abs(abs(res$modes[1, 1]) - 1), 0.15)
  expect_lt(abs(res$modes[1, 2]), 0.15)
})

test_that("optimix_map prints", {
  set.seed(105)
  res <- optimix_map(sphere, lower = c(-2, -2), upper = c(2, 2))
  expect_output(print(res), "optimix_map")
})
