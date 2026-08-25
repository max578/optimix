# The orchestra manifest emitter: a point optimum is a "parameters" manifest;
# the payload is integrity-hashed; and the proxymix mixture engine additionally
# carries the posterior-over-optima so a consumer always knows point vs posterior.

test_that("a point optimiser emits a conformant parameters manifest", {
  res <- optimix(function(x) sum((x - 0.5)^2), c(-2, -2), c(2, 2),
                 method = "base_optim")
  m <- as_orchestra_manifest(res)
  # OPT-01: the emitted class must be the bare, unnamespaced "orchestra_manifest"
  # token -- S7_inherits()/inherits() dispatch off the class *string*, and the
  # federation's reference class (sourced outside any package namespace) has
  # no "optimix::" prefix. A namespaced class name silently fails every
  # federation consumer's `S7_inherits()`/`inherits()` check.
  expect_s3_class(m, "orchestra_manifest")
  expect_false(any(grepl("^optimix::", class(m))))
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

test_that("OPT-02: MANIFEST_VERSION tracks the federation's reference constant", {
  # Independent oracle: read the reference contract's own constant directly
  # off disk (regex-extracted, not sourced -- sourcing the reference file
  # pulls in PESTO/proxymix/S7 generics that are not this package's business)
  # rather than trusting optimix's own copy of the number. Skips gracefully
  # when the ORCHESTRA_dev workspace is not checked out alongside optimix_dev
  # (e.g. a CRAN build), since the two are siblings, not a package dependency.
  # Walked from `getwd()` upward rather than a fixed relative depth, since
  # that depth differs between `devtools::test()` (package root) and
  # `testthat::test_file()` (tests/testthat).
  ref_path <- NULL
  probe <- normalizePath(getwd(), mustWork = FALSE)
  for (i1 in seq_len(8L)) {
    cand <- file.path(probe, "ORCHESTRA_dev", "integration",
                      "orchestra_manifest.R")
    if (file.exists(cand)) {
      ref_path <- cand
      break
    } # ends if, candidate found at this level
    parent <- dirname(probe)
    if (identical(parent, probe)) break  # reached filesystem root
    probe <- parent
  } # ends i1, walking up from getwd()
  skip_if(is.null(ref_path),
          "ORCHESTRA_dev reference contract not checked out alongside")
  ref_lines <- readLines(ref_path, warn = FALSE)
  hit <- grep('^MANIFEST_VERSION\\s*<-\\s*"', ref_lines, value = TRUE)
  skip_if(length(hit) == 0L, "reference constant not found (format drift)")
  ref_version <- sub('.*"([^"]+)".*', "\\1", hit[1])
  expect_identical(MANIFEST_VERSION, ref_version)
})

test_that("OPT-03: the payload hash strips the version-varying serialize header", {
  # The reference recipe (integration/orchestra_manifest.R .sha256()):
  # serialize at format version 2, drop its fixed 14-byte header (magic,
  # format, writer/minimum R versions -- the only bytes that vary by R
  # version), then hash the remainder. Reimplemented independently here
  # (not calling optimix:::.hash_payload) so the test is a real oracle: it
  # fails against the pre-fix implementation, which hashes the raw
  # serialize() output header and all.
  params <- data.frame(par1 = 0.5, par2 = -1.25, value = 0.125)
  seed <- 7L
  obj <- list(params, NULL, NULL, NULL, seed, NULL)
  raw <- serialize(obj, connection = NULL, version = 2L)
  raw <- raw[-seq_len(14L)]
  expected <- paste0("sha256:", as.character(tools::sha256sum(bytes = raw)))
  got <- optimix:::.hash_payload(params, NULL, NULL, NULL, seed, NULL)
  expect_identical(got, expected)

  # The defect this replaces: hashing the unstripped serialize() output makes
  # the digest depend on the writer's R version. Demonstrate the stripped
  # recipe is invariant to exactly the bytes that carry that dependency (the
  # 14-byte header) by re-stamping a synthetic "other R version" into a copy
  # of the header and confirming the STRIPPED hash is unaffected.
  raw_full <- serialize(obj, connection = NULL, version = 2L)
  raw_full_other_version <- raw_full
  raw_full_other_version[7:10] <- as.raw(c(4, 6, 0, 0))  # pretend R 4.6.0
  stripped_a <- raw_full[-seq_len(14L)]
  stripped_b <- raw_full_other_version[-seq_len(14L)]
  expect_identical(stripped_a, stripped_b)
})
