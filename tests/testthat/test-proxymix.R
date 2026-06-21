# These run only when a proxymix new enough to map objectives is installed.

test_that("optimix_map upgrades to a proxymix mixture when available", {
  skip_if_not_installed("proxymix")
  skip_if_not(utils::packageVersion("proxymix") >= "0.8.0")
  m <- optimix_map(
    function(x) (x[1]^2 - 1)^2 + x[2]^2,
    lower = c(-2, -2), upper = c(2, 2)
  )
  expect_equal(m$source, "proxymix")
  expect_false(is.null(m$map))
  expect_match(class(m$map)[1], "gmm")
  expect_gte(m$n, 1L)
})

test_that("the proxymix_map engine returns a mixture-valued map", {
  skip_if_not_installed("proxymix")
  skip_if_not(utils::packageVersion("proxymix") >= "0.8.0")
  res <- optimix(sphere, lower = c(-3, -3), upper = c(3, 3),
                 method = "proxymix_map")
  expect_equal(res$provenance$engine, "proxymix_map")
  expect_false(is.null(res$map))
})
