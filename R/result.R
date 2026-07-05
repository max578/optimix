# result.R -- The result object and its downstream methods.
#
# The result is a plain S3 list deliberately shaped like the return value of
# stats::optim() -- $par, $value, $counts, $convergence, $message -- so existing
# optim()-style code works unchanged, with optimix extras ($provenance,
# $map, $diagnostics, $problem) added alongside.

# The constructor --------------------------------------------------------------

#' Build an optimix result object
#'
#' The result embeds the problem it solved, including the objective closure
#' and its environment, so it is a session object rather than a durable one;
#' [as_optim()] gives the minimal durable form for storage.
#'
#' @param par The best parameter vector found, in the original design space.
#' @param value The objective value at `par`, in the original orientation.
#' @param counts A named numeric vector of evaluation counts.
#' @param convergence An integer convergence code (0 indicates success).
#' @param message A single human-readable status string.
#' @param engine The name of the engine that produced the result.
#' @param why A single string explaining why the engine was chosen.
#' @param map An optional mixture-valued solution map, or `NULL`.
#' @param diagnostics A list of engine-specific diagnostics.
#' @param problem The [optim_problem] that was solved.
#' @returns An object of class `optimix_result`.
#' @noRd
#' @keywords internal
.new_result <- function(par, value, counts, convergence, message, engine,
                        why = "user-specified", map = NULL,
                        diagnostics = list(), problem = NULL) {
  structure(
    list(
      par = par,
      value = value,
      counts = counts,
      convergence = convergence,
      message = message,
      provenance = list(engine = engine, why = why),
      map = map,
      diagnostics = diagnostics,
      problem = problem
    ),
    class = "optimix_result"
  )
}

# Display and coercion methods -------------------------------------------------

#' @export
print.optimix_result <- function(x, ...) {
  cat("<optimix_result>\n")
  cat(sprintf("  optimiser : %s\n", x$provenance$engine))
  cat(sprintf("  value     : %.6g\n", x$value))
  cat(sprintf(
    "  par       : %s\n",
    paste(format(signif(x$par, 5)), collapse = ", ")
  ))
  evals <- x$counts[["function"]]
  cat(sprintf(
    "  evals     : %s\n",
    if (is.na(evals)) "NA" else as.character(evals)
  ))
  cat(sprintf("  converged : %s\n", if (x$convergence == 0L) "yes" else "no"))
  if (!is.null(x$map)) {
    cat("  map       : a mixture-valued solution map is attached ($map)\n")
  }
  invisible(x)
}

#' @export
summary.optimix_result <- function(object, ...) {
  cat("Optimisation result\n")
  cat(sprintf("  Chosen optimiser : %s\n", object$provenance$engine))
  cat(sprintf("  Reason           : %s\n", object$provenance$why))
  cat(sprintf("  Best value       : %.8g\n", object$value))
  cat(sprintf(
    "  Best parameters  : %s\n",
    paste(format(signif(object$par, 6)), collapse = ", ")
  ))
  evals <- object$counts[["function"]]
  cat(sprintf(
    "  Evaluations used : %s\n",
    if (is.na(evals)) "NA" else as.character(evals)
  ))
  cat(sprintf(
    "  Converged        : %s\n",
    if (object$convergence == 0L) "yes" else "no"
  ))
  if (!is.null(object$map)) {
    n_modes <- object$diagnostics$n_modes
    cat(sprintf(
      "  Optima mapped    : %s\n",
      if (is.null(n_modes)) "see $map" else as.character(n_modes)
    ))
  }
  invisible(object)
}

#' @export
coef.optimix_result <- function(object, ...) {
  object$par
}

#' @export
as.data.frame.optimix_result <- function(x, ...) {
  par <- x$par
  nm <- if (!is.null(names(par))) names(par) else paste0("x", seq_along(par))
  out <- as.data.frame(
    as.list(stats::setNames(par, nm)),
    stringsAsFactors = FALSE
  )
  out$value <- x$value
  out$optimiser <- x$provenance$engine
  out
}

#' @export
plot.optimix_result <- function(x, ...) {
  if (!is.null(x$map)) {
    message(
      "A mixture-valued map is attached; plot it with proxymix on `$map`."
    )
    return(invisible(x))
  }
  trace <- x$diagnostics$bestvalit
  if (!is.null(trace)) {
    graphics::plot(
      seq_along(trace), trace, type = "l",
      xlab = "iteration", ylab = "best value so far",
      main = sprintf("Convergence (%s)", x$provenance$engine), ...
    )
    return(invisible(x))
  }
  message("No convergence trace or solution map to plot for this result.")
  invisible(x)
}

#' Coerce an optimisation result to a base optim() list
#'
#' Returns the bare `stats::optim()`-shaped components of a result, for code
#' paths that expect exactly that list and nothing more.
#'
#' @param x An object to coerce.
#' @param ... Unused; for method consistency.
#'
#' @returns A list with `par`, `value`, `counts`, `convergence`, and `message`.
#' @examples
#' res <- optimix(function(x) sum(x^2), c(-5, -5), c(5, 5),
#'                method = "base_optim")
#' as_optim(res)
#' @export
as_optim <- function(x, ...) {
  UseMethod("as_optim")
}

#' @export
as_optim.optimix_result <- function(x, ...) {
  list(
    par = x$par,
    value = x$value,
    counts = x$counts,
    convergence = x$convergence,
    message = x$message
  )
}
