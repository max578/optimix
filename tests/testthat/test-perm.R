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
