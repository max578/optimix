# The orchestra manifest emitter: a point optimum is a "parameters" manifest;
# the payload is integrity-hashed; and the proxymix mixture engine additionally
# carries the posterior-over-optima so a consumer always knows point vs posterior.

test_that("a point optimiser emits a conformant parameters manifest", {
  res <- optimix(function(x) sum((x - 0.5)^2), c(-2, -2), c(2, 2),
                 method = "base_optim")
  m <- as_orchestra_manifest(res)
  expect_s3_class(m, "optimix::orchestra_manifest")
  expect_identical(m@inferential_target, "parameters")
  expect_identical(m@emitter_package, "optimix")
  expect_identical(m@method, "optimix:base_optim")
  # the optimum rides in a one-row params table (the best parameter vector + value)
  expect_equal(nrow(m@params), 1L)
  expect_true("value" %in% names(m@params))
  expect_equal(unname(unlist(m@params[, c("par1", "par2")])), res$par,
               tolerance = 1e-8)
  # a point engine has no uncertainty quantification -- and says so honestly
  expect_false(m@metadata$uncertainty_available)
  expect_true(is.null(m@metadata$solution_map))
  # integrity verifies
  expect_true(verify_manifest(m)$ok)
})

test_that("a tampered payload is detected", {
  res <- optimix(function(x) sum(x^2), c(-2, -2), c(2, 2), method = "base_optim")
  m <- as_orchestra_manifest(res)
  expect_true(verify_manifest(m)$ok)
  m@params$value <- m@params$value + 1            # tamper with the optimum
  expect_false(verify_manifest(m)$ok)
})

test_that("a non-converged result lifts to an abstaining manifest", {
  # An engine that stops without converging must surface as an abstention in
  # the typed summary -- point vs posterior stays explicit downstream -- with
  # the engine's own message as the reason.
  register_optimiser(optim_engine(
    name = "test_nonconverged", pkg = "base", accepts = "space_box",
    available = function() TRUE,
    run = function(problem) {
      .new_result(
        par = c(0, 0), value = 1,
        counts = c(`function` = 3L, gradient = NA_integer_),
        convergence = 1L, message = "stopped before converging (unit stub)",
        engine = "test_nonconverged", problem = problem
      )
    }
  ))
  on.exit(rm(list = "test_nonconverged", envir = .engine_registry), add = TRUE)
  res <- optimix(sphere, c(-1, -1), c(1, 1), method = "test_nonconverged")
  m <- as_orchestra_manifest(res)
  expect_true(m@summary$abstained)
  expect_identical(
    m@summary$abstain_reason, "stopped before converging (unit stub)"
  )
  expect_identical(m@summary$metrics$convergence, 1L)
  expect_false(m@metadata$converged)
  # an abstaining manifest still hashes and verifies: abstention is a verdict,
  # not a corruption
  expect_true(verify_manifest(m)$ok)
})

test_that("verify_manifest rejects a non-manifest input cleanly", {
  for (bad in list(42, list(ok = TRUE), "manifest")) {
    v <- verify_manifest(bad)
    expect_false(v$ok)
    expect_identical(v$message, "not an orchestra_manifest")
  }
})

test_that("run_id is content-derived and an explicit one passes through", {
  res <- optimix(sphere, c(-2, -2), c(2, 2), method = "base_optim")
  first <- as_orchestra_manifest(res)
  second <- as_orchestra_manifest(res)
  # the same deterministic optimum hashes to the same identifier, so re-lifts
  # of one result are recognisably the same run downstream
  expect_identical(first@run_id, second@run_id)
  expect_match(first@run_id, "^optimix-[0-9a-f]{12}$")
  explicit <- as_orchestra_manifest(res, run_id = "my-run-001")
  expect_identical(explicit@run_id, "my-run-001")
})

test_that("the proxymix mixture engine carries the posterior over the optima", {
  skip_if_not_installed("proxymix")
  skip_if_not(optimix:::.proxymix_ready(), "installed proxymix lacks the mapper")
  # a multimodal objective; the proxymix engine maps it to a mixture over optima
  f <- function(x) sum(x^2) + 0.6 * sin(6 * x[1]) * sin(6 * x[2])
  res <- optimix(f, c(-2, -2), c(2, 2), method = "proxymix_map")
  m <- as_orchestra_manifest(res)
  expect_identical(m@inferential_target, "parameters")
  # the niche: a posterior over the optimum is available and flagged as such,
  # with the queryable mixture carried in metadata for a consumer that wants it
  expect_true(m@metadata$uncertainty_available)
  expect_false(is.null(m@metadata$solution_map))
  expect_true(verify_manifest(m)$ok)
})
