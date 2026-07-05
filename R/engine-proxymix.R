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
  # counts are honest rather than NA.
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
  best <- as.numeric(modes$modes[1L, ])
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
