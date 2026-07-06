test_that("space_permutation validates its size", {
  expect_error(space_permutation(1), "at least 2")
  expect_true(S7::S7_inherits(space_permutation(5), space_permutation))
})

test_that("perm_sa minimises a permutation cost", {
  res <- optimix(
    optim_problem(fn = sort_cost, space = space_permutation(8), seed = 1),
    method = "perm_sa"
  )
  expect_lt(res$value, 4)
  expect_length(res$par, 8)
  expect_setequal(res$par, 1:8)
})

test_that("the swap-delta equals the full recompute", {
  set.seed(2)
  perm <- sample.int(6)
  i <- 2L
  j <- 5L
  swapped <- perm
  swapped[c(i, j)] <- perm[c(j, i)]
  expect_equal(sort_delta(perm, i, j), sort_cost(swapped) - sort_cost(perm))
})

test_that("delta_eval is interchangeable with full evaluation (metamorphic)", {
  with_delta <- optimix(
    optim_problem(
      fn = sort_cost, space = space_permutation(8), seed = 1,
      delta_fn = sort_delta
    ),
    method = "perm_sa"
  )
  without_delta <- optimix(
    optim_problem(fn = sort_cost, space = space_permutation(8), seed = 1),
    method = "perm_sa"
  )
  expect_equal(with_delta$par, without_delta$par)
  expect_equal(with_delta$value, without_delta$value)
})

test_that("a wrong delta_fn is rejected at startup", {
  # This delta_fn claims every swap improves the cost by 0.01; before the
  # fix it silently steered the whole anneal.
  bad_delta <- function(perm, i, j) -0.01
  expect_error(
    optimix(
      optim_problem(
        fn = sort_cost, space = space_permutation(8), seed = 1,
        delta_fn = bad_delta
      ),
      method = "perm_sa"
    ),
    "`delta_fn` disagrees with `fn`",
    fixed = TRUE
  )
})

test_that("a delta_fn run reports the true re-evaluated value", {
  res <- optimix(
    optim_problem(
      fn = sort_cost, space = space_permutation(8), seed = 1,
      delta_fn = sort_delta
    ),
    method = "perm_sa"
  )
  expect_identical(as.numeric(res$value), as.numeric(sort_cost(res$par)))
  expect_true(res$diagnostics$delta_used)
})

test_that("counts separate true fn evaluations from delta evaluations", {
  with_delta <- optimix(
    optim_problem(
      fn = sort_cost, space = space_permutation(8), seed = 1,
      delta_fn = sort_delta
    ),
    method = "perm_sa"
  )
  without_delta <- optimix(
    optim_problem(fn = sort_cost, space = space_permutation(8), seed = 1),
    method = "perm_sa"
  )
  # Under a delta_fn nearly every move is a delta call, so the true fn count
  # collapses to the handful of full evaluations.
  expect_lt(
    with_delta$counts[["function"]],
    without_delta$counts[["function"]]
  )
  expect_gt(with_delta$diagnostics$delta_evals, 100L)
  expect_identical(without_delta$diagnostics$delta_evals, 0L)
})

test_that("warm_start must be a permutation of 1:n", {
  expect_error(
    optimix(
      optim_problem(
        fn = sort_cost, space = space_permutation(5),
        warm_start = c(1, 2, 2, 4, 5)
      ),
      method = "perm_sa"
    ),
    "`warm_start` must be a permutation",
    fixed = TRUE
  )
  ok <- optimix(
    optim_problem(
      fn = sort_cost, space = space_permutation(5), seed = 3,
      warm_start = 5:1
    ),
    method = "perm_sa"
  )
  expect_setequal(ok$par, 1:5)
})

test_that("auto routes a permutation space to perm_sa", {
  res <- optimix(
    optim_problem(fn = sort_cost, space = space_permutation(8), seed = 1),
    method = "auto"
  )
  expect_equal(res$provenance$engine, "perm_sa")
})

test_that("a box engine refuses a permutation space", {
  skip_if_not_installed("GenSA")
  expect_error(
    optimix(
      optim_problem(fn = sort_cost, space = space_permutation(8)),
      method = "gensa"
    ),
    "does not handle"
  )
})
