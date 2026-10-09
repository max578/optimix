# OPT-09: optimix's only manifest oracle was its own verify_manifest() --
# self-consistency, not correspondence to the orchestra's reference contract.
# This file runs an optimix-emitted manifest through the reference
# implementation's own `verify_manifest()` / `consume_manifest()`, so a schema
# or hash-recipe drift between optimix's local implementation and the
# reference fails here even if optimix's own `test-manifest.R` is green.
#
# The reference implementation is not part of this package. Set the
# environment variable OPTIMIX_MANIFEST_REFERENCE to its file to run these
# tests; they skip when it is unset.

.locate_orchestra_dev <- function() {
  cand <- Sys.getenv("OPTIMIX_MANIFEST_REFERENCE")
  if (nzchar(cand) && file.exists(cand)) cand else NULL
}

test_that("OPT-09: an optimix manifest passes the federation's own conformance checks", {
  ref_path <- .locate_orchestra_dev()
  skip_if(is.null(ref_path),
          "orchestra manifest reference implementation not available")
  # Sourcing the reference file registers `as_orchestra_manifest` S7 methods
  # dispatched on `PESTO::pesto_ensemble_manifest` and `proxymix::gmm_fit`, so
  # both packages must be attachable for it to source cleanly -- Suggests-only
  # for optimix itself, so this whole test skips (not fails) without them.
  skip_if_not_installed("PESTO")
  skip_if_not_installed("proxymix")

  ref <- new.env(parent = globalenv())
  suppressMessages(sys.source(ref_path, envir = ref))

  res <- optimix(function(x) sum((x - 0.5)^2), c(-2, -2), c(2, 2),
                 method = "base_optim")
  m <- as_orchestra_manifest(res)

  # -- required slots present + typed (mirrors the harness's §6 emitter check) --
  required <- c("manifest_version", "emitter_package", "emitter_version",
                "inferential_target", "run_id", "method", "data_hash")
  present <- all(vapply(required, function(s) {
    length(S7::prop(m, s)) == 1L && nzchar(as.character(S7::prop(m, s)))
  }, logical(1)))
  expect_true(present)

  # -- the REFERENCE's S7_inherits() recognises optimix's class (OPT-01) --
  expect_true(S7::S7_inherits(m, ref$orchestra_manifest))

  # -- the REFERENCE's own verify_manifest() (not optimix's) accepts the hash --
  expect_true(ref$verify_manifest(m)$ok)

  expect_identical(m@emitter_package, "optimix")

  # -- serialise round-trip preserves + verifies under the reference's checks --
  rt <- unserialize(serialize(m, NULL))
  expect_true(S7::S7_inherits(rt, ref$orchestra_manifest))
  expect_true(ref$verify_manifest(rt)$ok)

  # -- the REFERENCE's consume_manifest() accepts a version-matched manifest --
  expect_identical(m@manifest_version, ref$MANIFEST_VERSION)
  expect_silent(ref$consume_manifest(m))

  # -- the reference's version-mismatch refusal fires as designed (informative) --
  expect_error(ref$consume_manifest(m, accept = "9.9.9"),
              "not in the accepted set")

  # -- the reference's corrupted-hash refusal fires on a tampered optimix manifest --
  m_corrupt <- m
  m_corrupt@params$value <- m_corrupt@params$value + 1
  expect_error(ref$consume_manifest(m_corrupt), "integrity check failed")
})
