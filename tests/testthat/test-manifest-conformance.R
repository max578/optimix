# OPT-09: optimix's only manifest oracle was its own verify_manifest() --
# self-consistency, not correspondence to the federation's reference contract
# (ORCHESTRA_dev/integration/orchestra_manifest.R, sourced by
# ORCHESTRA_dev/integration/test_manifest_conformance.R for PESTO, proxymix and
# cdzoo). This file is optimix's own copy of that same independent-oracle
# check: it sources the REFERENCE implementation (read-only; never modified
# from here) and runs an optimix-emitted manifest through the reference's own
# `verify_manifest()` / `consume_manifest()` -- not optimix's copies of those
# functions. A schema or hash-recipe drift between optimix's local
# implementation and the reference (the OPT-01/02/03 class of defect) fails
# here even if optimix's own `test-manifest.R` is green.
#
# optimix_dev and ORCHESTRA_dev are sibling workspaces, not a package
# dependency -- the whole suite here skips gracefully (not fails) when
# ORCHESTRA_dev is not checked out alongside (e.g. a CRAN build, a checkout
# with only this one repository).

.locate_orchestra_dev <- function() {
  probe <- normalizePath(getwd(), mustWork = FALSE)
  for (i1 in seq_len(8L)) {
    cand <- file.path(probe, "ORCHESTRA_dev", "integration",
                      "orchestra_manifest.R")
    if (file.exists(cand)) return(cand)
    parent <- dirname(probe)
    if (identical(parent, probe)) break  # reached filesystem root
    probe <- parent
  } # ends i1, walking up from getwd()
  NULL
}

test_that("OPT-09: an optimix manifest passes the federation's own conformance checks", {
  ref_path <- .locate_orchestra_dev()
  skip_if(is.null(ref_path),
          "ORCHESTRA_dev reference contract not checked out alongside")
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
