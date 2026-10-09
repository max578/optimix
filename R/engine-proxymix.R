# engine-proxymix.R -- The optional mixture-valued mapper from proxymix.
#
# When a new-enough proxymix is installed, optimix can return a queryable
# Gaussian-mixture map over the optima rather than a single point, via
# proxymix::from_objective() + proxymix::gmm_modes(). This is the distinctive
# member for the low-dimensional, multimodal "map all the good solutions"
# regime; the selector routes to it only when it is available.

#' Is proxymix new enough to map objectives?
#'
#' The objective mapper entered proxymix at version 0.8.0, so the adapter
#' requires both that proxymix is installed and that it exposes
#' `from_objective()`.
#'
#' @returns A single logical.
#' @noRd
#' @keywords internal
.proxymix_ready <- function() {
  if (!requireNamespace("proxymix", quietly = TRUE)) {
    return(FALSE)
  }
  ver_ok <- utils::packageVersion("proxymix") >= "0.8.0"
  fun_ok <- exists(
    "from_objective",
    where = asNamespace("proxymix"),
    inherits = FALSE
  )
  isTRUE(ver_ok) && isTRUE(fun_ok)
}

#' proxymix mixture-valued mapper adapter
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result` whose `$map` holds the fitted mixture and
#'   whose `counts[["function"]]` records the true objective evaluations spent
#'   by the fit. The problem's `seed` is honoured; `max_evals` cannot be (the
#'   installed `from_objective()` exposes no evaluation-budget argument), and
#'   a set budget is flagged in the result's `message` rather than silently
#'   dropped.
#' @noRd
#' @keywords internal
.run_proxymix <- function(problem) {
  sp <- problem@space
  # Honour the problem's seed: from_objective() is importance-sampled, so an
  # unseeded call is irreproducible. Wrap fn in a counter so the result's
  # counts are recorded rather than NA.
  .maybe_seed(problem)
  fn <- problem@fn
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  counted_fn <- function(x) {
    counter$n <- counter$n + 1L
    fn(x)
  }
  fit <- proxymix::from_objective(
    objective = counted_fn,
    lower = sp@lower,
    upper = sp@upper,
    minimise = !isTRUE(problem@maximise)
  )
  modes <- proxymix::gmm_modes(fit)
  # A mixture component's mean can sit outside the design box; the result's
  # `par` must be feasible, so the best in-bounds mode is reported and an
  # all-infeasible fit is a clean failure rather than an out-of-box "optimum".
  keep <- .feasible_rows(modes$modes, sp@lower, sp@upper)
  if (!any(keep)) {
    # refusal contract: optimix will produce NO result at all here (every
    # mixture mean fell outside the design box), so this is a genuine
    # `_refusal`, not an ordinary validation error. Raised as a classed
    # condition -- `structure(..., class = c("<pkg>_refusal",
    # "orchestra_refusal", "error", "condition"))` -- so a caller can
    # `tryCatch` on the specific class, and the orchestra-wide predicate
    # `is_orchestra_decline()` (integration/refusal_contract.R) recognises it
    # from `class(e)` with no optimix-specific code.
    stop(structure(
      class = c("optimix_refusal", "orchestra_refusal", "error", "condition"),
      list(
        message = paste(
          "The proxymix mixture returned no mode inside the bounds;",
          "use `optimix_map()` or another engine for this problem."
        ),
        call = NULL
      )
    ))
  }
  best <- as.numeric(modes$modes[which(keep)[1L], ])
  val <- counted_fn(best)
  # from_objective() (checked against proxymix 0.15.1) has no budget-like
  # argument to forward `max_evals` to, so say so instead of ignoring it.
  msg <- if (!is.na(problem@max_evals)) {
    "proxymix objective map (`max_evals` is not honoured by this engine)"
  } else {
    "proxymix objective map"
  }
  .new_result(
    par = best,
    value = val,
    counts = c(`function` = counter$n, gradient = NA_integer_),
    convergence = if (isTRUE(fit@converged)) 0L else 1L,
    message = msg,
    engine = "proxymix_map",
    map = fit,
    diagnostics = list(n_modes = modes$n),
    problem = problem
  )
}
