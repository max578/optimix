# These run only when a proxymix new enough to map objectives is installed,
# gated on the package's own readiness probe. A too-old proxymix skips; a
# ready proxymix must pass for real -- no tryCatch-and-skip.

test_that("optimix_map upgrades to a proxymix mixture when available", {
  skip_if_not_installed("proxymix")
  skip_if_not(optimix:::.proxymix_ready())
  set.seed(1)
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
  skip_if_not(optimix:::.proxymix_ready())
  prob <- optim_problem(
    fn = sphere, space = space_box(c(-3, -3), c(3, 3)), seed = 7
  )
  res <- optimix(prob, method = "proxymix_map")
  expect_equal(res$provenance$engine, "proxymix_map")
  expect_false(is.null(res$map))
  # The counts are real evaluations now, not NA.
  expect_true(is.finite(res$counts[["function"]]))
  expect_gt(res$counts[["function"]], 0)
})

test_that("the adapter reproduces a direct proxymix call (oracle)", {
  skip_if_not_installed("proxymix")
  skip_if_not(optimix:::.proxymix_ready())
  f <- function(x) (x[1]^2 - 1)^2
  prob <- optim_problem(
    fn = f, space = space_box(lower = -2, upper = 2), seed = 42
  )
  res <- optimix(prob, method = "proxymix_map")
  # The independent oracle: the same seeded call made directly to proxymix
  # must yield the same best mode and mode count as the adapter.
  set.seed(42L)
  fit <- proxymix::from_objective(
    objective = f, lower = -2, upper = 2, minimise = TRUE
  )
  modes <- proxymix::gmm_modes(fit)
  expect_equal(res$par, as.numeric(modes$modes[1L, ]), tolerance = 1e-6)
  expect_identical(res$diagnostics$n_modes, modes$n)
})

test_that("proxymix_map flags a budget it cannot forward", {
  skip_if_not_installed("proxymix")
  skip_if_not(optimix:::.proxymix_ready())
  prob <- optim_problem(
    fn = function(x) (x[1]^2 - 1)^2,
    space = space_box(lower = -2, upper = 2), seed = 3, max_evals = 50
  )
  res <- optimix(prob, method = "proxymix_map")
  expect_match(res$message, "max_evals", fixed = TRUE)
})

test_that("the proxymix engine's par is always feasible", {
  skip_if_not_installed("proxymix")
  skip_if_not(optimix:::.proxymix_ready(), "installed proxymix lacks the mapper")
  # Same concave driver as the map-side regression: before the feasibility
  # filter the engine could return an out-of-box mixture mean as `par`.
  prob <- optim_problem(
    fn = function(x) -x[1]^2 - x[2]^2,
    space = space_box(c(-2, -2), c(2, 2)),
    seed = 31
  )
  # Two honest outcomes exist: a feasible best mode, or -- when every
  # mixture mean drifts outside the box, as this concave driver forces --
  # the engine's clean no-feasible-mode error. What must never happen is
  # an out-of-box `par`.
  out <- tryCatch(
    suppressWarnings(optimix(prob, method = "proxymix_map")),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    expect_match(conditionMessage(out), "no mode inside the bounds")
    # refusal: this is optimix declining to produce a result at all (no
    # feasible mode), not a generic R error -- the class must carry the
    # orchestra-wide `_refusal` marker so integration/refusal_contract.R's
    # is_orchestra_decline() recognises it with no optimix-specific code.
    expect_s3_class(out, "optimix_refusal")
    expect_true(any(class(out) == "orchestra_refusal"))
  } else {
    expect_true(all(out$par >= -2 & out$par <= 2))
  }
})

test_that("OPT refusal: the no-feasible-mode error is a classed refusal condition", {
  skip_if_not_installed("proxymix")
  skip_if_not(optimix:::.proxymix_ready(), "installed proxymix lacks the mapper")
  # Force the all-infeasible branch deterministically by stubbing
  # gmm_modes() to return a mode far outside the bounds, rather than relying
  # on a seed that happens to drift every mixture mean out of the box.
  fake_fit <- structure(list(converged = TRUE), class = "gmm_fit")
  testthat::local_mocked_bindings(
    from_objective = function(...) fake_fit,
    gmm_modes = function(...) list(modes = matrix(c(100, 100), nrow = 1L),
                                   n = 1L),
    .package = "proxymix"
  )
  prob <- optim_problem(
    fn = function(x) sum(x^2), space = space_box(c(-2, -2), c(2, 2)), seed = 1
  )
  err <- tryCatch(
    optimix(prob, method = "proxymix_map"),
    error = function(e) e
  )
  expect_s3_class(err, "optimix_refusal")
  expect_true(any(class(err) == "orchestra_refusal"))
  expect_true(any(class(err) == "error"))
  expect_match(conditionMessage(err), "no mode inside the bounds")
})
