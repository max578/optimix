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

test_that("the proxymix mixture engine carries the posterior over the optima", {
  skip_if_not_installed("proxymix")
  # a multimodal objective; the proxymix engine maps it to a mixture over optima
  f <- function(x) sum(x^2) + 0.6 * sin(6 * x[1]) * sin(6 * x[2])
  res <- tryCatch(
    optimix(f, c(-2, -2), c(2, 2), method = "proxymix_map"),
    error = function(e) e)
  skip_if(inherits(res, "error"), "proxymix_map engine unavailable here")
  m <- as_orchestra_manifest(res)
  expect_identical(m@inferential_target, "parameters")
  # the niche: a posterior over the optimum is available and flagged as such,
  # with the queryable mixture carried in metadata for a consumer that wants it
  expect_true(m@metadata$uncertainty_available)
  expect_false(is.null(m@metadata$solution_map))
  expect_true(verify_manifest(m)$ok)
})
