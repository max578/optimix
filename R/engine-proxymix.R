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
#' @returns An `optimix_result` whose `$map` holds the fitted mixture.
#' @noRd
#' @keywords internal
.run_proxymix <- function(problem) {
  sp <- problem@space
  fit <- proxymix::from_objective(
    objective = problem@fn,
    lower = sp@lower,
    upper = sp@upper,
    minimise = !isTRUE(problem@maximise)
  )
  modes <- proxymix::gmm_modes(fit)
  best <- as.numeric(modes$modes[1L, ])
  .new_result(
    par = best,
    value = problem@fn(best),
    counts = c(`function` = NA_integer_, gradient = NA_integer_),
    convergence = if (isTRUE(fit@converged)) 0L else 1L,
    message = "proxymix objective map",
    engine = "proxymix_map",
    map = fit,
    diagnostics = list(n_modes = modes$n),
    problem = problem
  )
}
