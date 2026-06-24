# manifest.R -- the orchestra contract-emitting result object.
#
# optimix is the orchestra's optimisation member, so an optimisation result must
# be expressible in the federation's shared manifest contract -- the versioned,
# hashed, provenance-complete S7 object every member emits or consumes as its
# primary inference-result type (ORCHESTRA binding #1; manifest_spec v0.1). This
# file carries a stand-in `orchestra_manifest` class whose property names and
# integrity hash match the canonical reference implementation
# (`ORCHESTRA_dev/integration/orchestra_manifest.R`) field for field, so a
# consumer -- decideR, conductoR -- reads an optimix manifest with no special
# casing. The contract is defined here (rather than depended on) because the
# canonical implementation lives in the composition layer, not an installable
# package.
#
# An optimum is, in orchestra terms, a point estimate of the best parameter
# vector, so the emit target is `"parameters"` -- the same target proxymix's
# `from_objective` mixture emits. The crucial, carefully-provisioned honesty: an
# optimisation NEED can be outside any one engine's competence, so the manifest
# records WHICH engine ran and WHETHER an uncertainty quantification (a
# posterior over the optimum) is available. A point engine emits a single
# optimum with `metadata$uncertainty_available = FALSE`; the proxymix mixture
# engine additionally carries the queryable mixture (all modes + uncertainty) in
# `metadata$solution_map`. A downstream consumer therefore always knows whether
# it received a point or a posterior, and never mistakes one for the other.

# The manifest schema version this member emits. Tracks the reference
# implementation; bump only with an additive (x.y) or breaking (x.0) change there.
MANIFEST_VERSION <- "1.1.0-draft"

# The inferential-target enum, kept identical to the reference contract so a
# consumer's dispatch never sees an unknown token.
.INFERENTIAL_TARGETS <- c("parameters", "predictions",
                          "treatment_effects", "decisions",
                          "breeding_values", "marker_associations")

#' Payload integrity hash matching the orchestra contract
#'
#' SHA-256 over the load-bearing data slots only (the schema, version and
#' timestamp are metadata and are not hashed -- spec section 8.4). The
#' `"sha256:"` prefix and the slot ordering match the reference implementation so
#' a manifest hashed by optimix verifies under the federation's
#' `verify_manifest()`.
#'
#' @param params,outputs,weights,obs_target,seed,summary The load-bearing slots.
#'
#' @returns A single `"sha256:"`-prefixed hex string.
#' @noRd
#' @keywords internal
.hash_payload <- function(params, outputs, weights, obs_target, seed,
                          summary = NULL) {
  obj <- list(params, outputs, weights, obs_target, seed, summary)
  raw <- serialize(obj, connection = NULL)
  paste0("sha256:", digest::digest(raw, algo = "sha256", serialize = FALSE))
}

#' A typed home for an optimisation verdict (contract v1.1)
#'
#' Mirrors the reference contract's `manifest_summary()`: the result's headline
#' label, an abstain flag (set when the optimiser did not converge), and a list
#' of key metrics. Integrity-hashed with the rest of the payload.
#'
#' @param headline A single non-empty status string.
#' @param abstained A single logical; `TRUE` when the result is not trustworthy
#'   (e.g. non-convergence).
#' @param metrics A named list of scalar metrics.
#' @param abstain_reason A single string, or `NA`.
#'
#' @returns A classed `manifest_summary` list.
#' @noRd
#' @keywords internal
manifest_summary <- function(headline, abstained = FALSE, metrics = list(),
                             abstain_reason = NA_character_) {
  stopifnot(is.character(headline), length(headline) == 1L, nzchar(headline),
            is.logical(abstained), length(abstained) == 1L, is.list(metrics))
  structure(list(headline = headline, abstained = isTRUE(abstained),
                 abstain_reason = abstain_reason, metrics = metrics),
            class = "manifest_summary")
}

# --- the contract class ------------------------------------------------------

#' The orchestra ensemble-manifest contract (optimix-side implementation)
#'
#' A versioned, hashed, provenance-complete S7 result object, property-compatible
#' with the federation's reference `orchestra_manifest`. An `optimix_result` is
#' adapted into one through [as_orchestra_manifest()]: the optimum rides in
#' `params` (`inferential_target = "parameters"`), the engine, convergence and
#' uncertainty-availability ride in `metadata` and the typed `summary`, and the
#' payload is integrity-hashed so a tampered optimum is detected downstream.
#'
#' @usage NULL
#'
#' @returns An S7 object of class `orchestra_manifest`.
#'
#' @seealso [as_orchestra_manifest()], [verify_manifest()]
#' @export
orchestra_manifest <- S7::new_class(
  "orchestra_manifest",
  package = "optimix",
  properties = list(
    manifest_version   = S7::new_property(S7::class_character,
                                          default = MANIFEST_VERSION),
    emitter_package    = S7::class_character,
    emitter_version    = S7::class_character,
    inferential_target = S7::class_character,
    run_id             = S7::class_character,
    method             = S7::class_character,
    seed               = S7::new_property(S7::class_integer,
                                          default = NA_integer_),
    params             = S7::new_property(S7::class_data.frame,
                                          default = data.frame()),
    outputs            = S7::new_property(S7::class_any, default = NULL),
    weights            = S7::new_property(S7::class_any, default = NULL),
    obs_target         = S7::new_property(S7::class_any, default = NULL),
    obs_schema         = S7::new_property(S7::class_any, default = NULL),
    summary            = S7::new_property(S7::class_any, default = NULL),
    consumed_manifests = S7::new_property(S7::class_list, default = list()),
    metadata           = S7::new_property(S7::class_list, default = list()),
    timestamp          = S7::class_POSIXct,
    data_hash          = S7::class_character
  ),
  validator = function(self) {
    errs <- character(0)
    if (length(self@run_id) != 1L || !nzchar(self@run_id)) {
      errs <- c(errs, "`run_id` must be a single non-empty string")
    }
    if (length(self@inferential_target) != 1L ||
        !self@inferential_target %in% .INFERENTIAL_TARGETS) {
      errs <- c(errs, sprintf("`inferential_target` must be one of %s",
                              paste(.INFERENTIAL_TARGETS, collapse = ", ")))
    }
    if (length(self@data_hash) != 1L) {
      errs <- c(errs, "`data_hash` must be a single string")
    }
    if (!is.null(self@summary) && !inherits(self@summary, "manifest_summary")) {
      errs <- c(errs, "`summary`, when set, must be a manifest_summary() object")
    }
    if (length(errs) == 0L) NULL else paste(errs, collapse = "; ")
  }
)

#' Verify an orchestra manifest's payload integrity
#'
#' Recomputes the payload hash from the object's own load-bearing slots and
#' compares it to the stored `data_hash`. A mismatch means the payload was
#' modified after emission. Matches the reference contract's `verify_manifest()`.
#'
#' @param m An `orchestra_manifest` object.
#'
#' @returns A list with logical `ok` and a human-readable `message`.
#'
#' @seealso [as_orchestra_manifest()]
#'
#' @examples
#' res <- optimix(function(x) sum(x^2), c(-5, -5), c(5, 5),
#'                method = "base_optim")
#' m <- as_orchestra_manifest(res)
#' verify_manifest(m)
#'
#' @export
verify_manifest <- function(m) {
  if (!S7::S7_inherits(m, orchestra_manifest)) {
    return(list(ok = FALSE, message = "not an orchestra_manifest"))
  }
  recomputed <- .hash_payload(m@params, m@outputs, m@weights,
                              m@obs_target, m@seed, m@summary)
  ok <- identical(recomputed, m@data_hash)
  list(ok = ok,
       message = if (ok) "data_hash verified"
                 else "data_hash MISMATCH -- manifest payload was modified")
}

# --- the emit generic --------------------------------------------------------

#' Emit an optimix result as an orchestra manifest
#'
#' The federation's emit / adapt generic. The `optimix_result` method turns an
#' optimisation result into the shared `orchestra_manifest` contract:
#' `inferential_target = "parameters"`, the optimum carried as a one-row `params`
#' table (the best parameter vector and its objective value), and the engine
#' provenance -- which optimiser ran, why it was chosen, convergence, the true
#' evaluation count, and **whether an uncertainty quantification is available** --
#' in the typed `summary` and `metadata`. When the proxymix mixture engine ran,
#' `metadata$solution_map` carries the queryable mixture over all optima, so a
#' downstream consumer that needs a posterior over the optimum (not just a point)
#' can read it; a point engine sets `metadata$uncertainty_available = FALSE`. The
#' payload is integrity-hashed so a consumer can verify it with
#' [verify_manifest()].
#'
#' @param x An `optimix_result` object.
#' @param ... Further arguments passed to methods.
#' @param run_id Optional character run identifier; a content hash is derived
#'   when omitted, so two identical optima get the same identifier.
#' @param seed Optional integer recorded for reproducibility.
#'
#' @returns An `orchestra_manifest` S7 object.
#'
#' @seealso [verify_manifest()], [optimix()]
#'
#' @examples
#' res <- optimix(function(x) sum((x - 0.5)^2), c(-2, -2), c(2, 2),
#'                method = "base_optim")
#' as_orchestra_manifest(res)
#'
#' @export
as_orchestra_manifest <- function(x, ...) {
  UseMethod("as_orchestra_manifest")
}

#' @rdname as_orchestra_manifest
#' @export
as_orchestra_manifest.optimix_result <- function(x, ..., run_id = NULL,
                                                 seed = NA_integer_) {
  par <- x$par
  nm <- if (!is.null(names(par))) names(par) else paste0("par", seq_along(par))
  params <- as.data.frame(as.list(stats::setNames(par, nm)),
                          stringsAsFactors = FALSE)
  params$value <- x$value

  engine <- x$provenance$engine
  evals <- tryCatch(x$counts[["function"]], error = function(e) NA_real_)
  converged <- isTRUE(x$convergence == 0L)
  has_uncertainty <- !is.null(x$map)
  n_modes <- x$diagnostics$n_modes %||% (if (has_uncertainty) NA_integer_ else 1L)

  summ <- manifest_summary(
    headline = sprintf("optimum f = %.6g via %s", x$value, engine),
    abstained = !converged,
    abstain_reason = if (!converged) (x$message %||% "did not converge")
                     else NA_character_,
    metrics = list(
      value                 = x$value,
      engine                = engine,
      why                   = x$provenance$why,
      convergence           = as.integer(x$convergence),
      evals                 = evals,
      n_modes               = n_modes,
      uncertainty_available = has_uncertainty))

  meta <- list(
    derived               = FALSE,
    engine                = engine,
    why                   = x$provenance$why,
    converged             = converged,
    evals                 = evals,
    dim                   = length(par),
    n_modes               = n_modes,
    # The carefully-provisioned honesty: a point optimum vs a posterior over the
    # optimum. The queryable mixture rides here (unhashed metadata) when the
    # proxymix engine produced one; a classical point engine leaves it NULL.
    uncertainty_available = has_uncertainty,
    solution_map          = x$map)

  ev <- as.character(utils::packageVersion("optimix"))
  seed <- as.integer(seed)
  dh <- .hash_payload(params, NULL, NULL, NULL, seed, summ)
  rid <- run_id %||% paste0(
    "optimix-",
    substr(sub("^sha256:", "", .hash_payload(params, NULL, NULL, NULL, seed,
                                             summ)), 1L, 12L))

  orchestra_manifest(
    manifest_version   = MANIFEST_VERSION,
    emitter_package    = "optimix",
    emitter_version    = ev,
    inferential_target = "parameters",
    run_id             = rid,
    method             = paste0("optimix:", engine),
    seed               = seed,
    params             = params,
    summary            = summ,
    consumed_manifests = list(),
    metadata           = meta,
    timestamp          = Sys.time(),
    data_hash          = dh)
}

#' Null-coalescing operator
#'
#' @param a,b The candidate and the fallback.
#'
#' @returns `a` unless it is `NULL`, in which case `b`.
#' @noRd
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a
