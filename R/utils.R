# utils.R -- Internal helpers shared across the contract, the facade, and the
# engine adapters: problem validation, objective wrapping, search-control
# extraction, and feasibility checks.

#' Validate an optim_problem at construction
#'
#' @param self The [optim_problem] being constructed.
#' @returns `NULL` when valid, otherwise a character vector of problems.
#' @noRd
#' @keywords internal
.validate_problem <- function(self) {
  d <- .space_dim(self@space)
  msgs <- character(0)
  if (length(self@maximise) != 1L) {
    msgs <- c(msgs, "`maximise` must be a single logical")
  }
  if (length(self@max_evals) != 1L) {
    msgs <- c(msgs, "`max_evals` must be a single number or NA")
  } else if (!is.na(self@max_evals) && self@max_evals <= 0) {
    msgs <- c(msgs, "`max_evals` must be positive")
  }
  warm <- self@warm_start
  if (length(warm) > 0L && !is.na(d) && length(warm) != d) {
    msgs <- c(msgs, "`warm_start` must match the design-space dimension")
  }
  if (length(self@seed) != 1L) {
    msgs <- c(msgs, "`seed` must be a single number or NA")
  }
  if (!is.null(self@delta_fn) && !is.function(self@delta_fn)) {
    msgs <- c(msgs, "`delta_fn` must be a function or NULL")
  }
  if (length(msgs) == 0L) NULL else paste(msgs, collapse = "; ")
}

#' Dimension of a design space
#'
#' @param space An [opt_space].
#' @returns The integer dimension, or `NA` for an unrecognised space.
#' @noRd
#' @keywords internal
.space_dim <- function(space) {
  if (S7::S7_inherits(space, space_box)) {
    length(space@lower)
  } else if (S7::S7_inherits(space, space_permutation)) {
    as.integer(space@n)
  } else {
    NA_integer_
  }
}

#' Canonical name of a design space
#'
#' @param space An [opt_space].
#' @returns A single string naming the space class.
#' @noRd
#' @keywords internal
.space_class_name <- function(space) {
  if (S7::S7_inherits(space, space_box)) {
    "space_box"
  } else if (S7::S7_inherits(space, space_permutation)) {
    "space_permutation"
  } else {
    "unknown"
  }
}

#' Build the function an engine minimises
#'
#' Wraps the user objective so that every engine minimises a single function:
#' maximisation is handled by a sign flip, and a counter records the number of
#' evaluations for the result's `counts` field.
#'
#' @param problem An [optim_problem].
#' @returns A list with `g` (the function to minimise), `evals` (a function
#'   returning the evaluation count), and `sign` (1 for minimise, -1 for
#'   maximise).
#' @noRd
#' @keywords internal
.make_objective <- function(problem) {
  fn <- problem@fn
  sign <- if (isTRUE(problem@maximise)) -1 else 1
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  g <- function(x) {
    counter$n <- counter$n + 1L
    sign * fn(x)
  }
  list(g = g, evals = function() counter$n, sign = sign)
}

#' Starting point for a search
#'
#' @param problem An [optim_problem].
#' @returns The warm-start vector when supplied, otherwise the box midpoint.
#' @noRd
#' @keywords internal
.start_point <- function(problem) {
  warm <- problem@warm_start
  if (length(warm) > 0L) {
    warm
  } else {
    (problem@space@lower + problem@space@upper) / 2
  }
}

#' Evaluation budget with an engine fallback
#'
#' @param problem An [optim_problem].
#' @param fallback The integer budget to use when the problem sets none.
#' @returns A single integer budget.
#' @noRd
#' @keywords internal
.budget <- function(problem, fallback) {
  me <- problem@max_evals
  if (is.na(me)) as.integer(fallback) else as.integer(me)
}

#' Set the RNG seed when the problem requests one
#'
#' @param problem An [optim_problem].
#' @returns `NULL`, invisibly; called for the side effect.
#' @noRd
#' @keywords internal
.maybe_seed <- function(problem) {
  s <- problem@seed
  if (!is.na(s)) set.seed(as.integer(s))
  invisible(NULL)
}

#' Check that the facade received a usable objective
#'
#' @param fn The first argument to [optimix()].
#' @returns `TRUE`, invisibly, or stops with a caller-facing error.
#' @noRd
#' @keywords internal
.check_fn <- function(fn) {
  if (!is.function(fn)) {
    stop(call. = FALSE, "`fn` must be a function (or an `optim_problem`).")
  }
  invisible(TRUE)
}

#' Check that bounds were supplied to the facade
#'
#' @param lower,upper The bound arguments to [optimix()].
#' @returns `TRUE`, invisibly, or stops with a caller-facing error.
#' @noRd
#' @keywords internal
.check_bounds <- function(lower, upper) {
  if (is.null(lower) || is.null(upper)) {
    stop(
      call. = FALSE,
      "Supply `lower` and `upper` bounds (or pass an `optim_problem`)."
    )
  }
  invisible(TRUE)
}

#' Check an engine can handle a problem
#'
#' @param engine An [optim_engine].
#' @param problem An [optim_problem].
#' @returns `TRUE`, invisibly, or stops with a caller-facing error.
#' @noRd
#' @keywords internal
.check_engine_fits <- function(engine, problem) {
  space_name <- .space_class_name(problem@space)
  if (!space_name %in% engine@accepts) {
    stop(call. = FALSE, sprintf(
      "Optimiser `%s` does not handle a `%s` design space.",
      engine@name, space_name
    ))
  }
  d <- .space_dim(problem@space)
  if (!is.na(d) && (d < engine@dim_min || d > engine@dim_max)) {
    stop(call. = FALSE, sprintf(
      "Optimiser `%s` supports dimensions %s-%s; this problem has %d.",
      engine@name, engine@dim_min, engine@dim_max, d
    ))
  }
  invisible(TRUE)
}
