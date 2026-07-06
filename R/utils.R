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
  me <- self@max_evals
  if (length(me) != 1L) {
    msgs <- c(msgs, "`max_evals` must be a single number or NA")
  } else if (is.nan(me)) {
    msgs <- c(msgs, "`max_evals` must not be NaN")
  } else if (!is.na(me)) {
    if (!is.finite(me)) {
      msgs <- c(msgs, "`max_evals` must be a finite number or NA")
    } else if (me %% 1 != 0) {
      msgs <- c(msgs, "`max_evals` must be a whole number")
    } else if (me < 1 || me > .Machine$integer.max) {
      msgs <- c(msgs, sprintf(
        "`max_evals` must be between 1 and %d", .Machine$integer.max
      ))
    }
  }
  warm <- self@warm_start
  if (length(warm) > 0L && !is.na(d) && length(warm) != d) {
    msgs <- c(msgs, "`warm_start` must match the design-space dimension")
  } else if (length(warm) > 0L && S7::S7_inherits(self@space, space_box)) {
    lo <- self@space@lower
    hi <- self@space@upper
    bad <- which(!is.finite(warm) | warm < lo | warm > hi)
    if (length(bad) > 0L) {
      msgs <- c(msgs, sprintf(
        "`warm_start[%d]` (%s) is outside its bounds [%g, %g]",
        bad[1L], format(warm[bad[1L]]), lo[bad[1L]], hi[bad[1L]]
      ))
    }
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

#' Reduce a box problem over its pinned dimensions
#'
#' A `space_box` admits a pinned coordinate (one with `lower == upper`), but the
#' local quasi-Newton engines that drive `stats::optim(method = "L-BFGS-B")`
#' fail with a non-finite finite-difference value when they step a pinned
#' variable, and `GenSA` rejects a pinned coordinate outright. Reducing the
#' problem to its free coordinates before dispatch makes a legal design space
#' work for every engine at once, rather than patching each backend.
#'
#' The reduction is a pure restriction of the same objective: the reduced `fn`
#' evaluates the original `fn` with the pinned coordinates held at their fixed
#' value, so a non-degenerate problem (no pinned coordinate) is returned
#' untouched (`NULL`) and the result is bit-identical to dispatching directly.
#'
#' @param problem An [optim_problem].
#'
#' @returns `NULL` when there is nothing to reduce -- the space is not a
#'   `space_box`, or no coordinate is pinned. Otherwise a list with `free` (a
#'   logical vector marking the free coordinates), `fixed_vals` (the full vector
#'   of pinned values, valid on the pinned coordinates), and `reduced` (the
#'   [optim_problem] over the free coordinates, or `NULL` when every coordinate
#'   is pinned).
#' @noRd
#' @keywords internal
.reduce_pinned <- function(problem) {
  sp <- problem@space
  if (!S7::S7_inherits(sp, space_box)) {
    return(NULL)
  }
  pinned <- sp@lower == sp@upper
  if (!any(pinned)) {
    return(NULL)
  }
  free <- !pinned
  fixed_vals <- sp@lower

  # ---- Restrict the objective to the free coordinates ---------------------
  # The reduced fn holds the pinned coordinates fixed and varies only the free
  # ones, so it is the original objective restricted to the free subspace.
  reduced <- if (any(free)) {
    fn <- problem@fn
    reduced_fn <- function(z) {
      x <- fixed_vals
      x[free] <- z
      fn(x)
    }
    warm <- if (length(problem@warm_start) > 0L) {
      problem@warm_start[free]
    } else {
      numeric(0)
    }
    optim_problem(
      fn = reduced_fn,
      space = space_box(lower = sp@lower[free], upper = sp@upper[free]),
      objective = problem@objective,
      maximise = problem@maximise,
      max_evals = problem@max_evals,
      warm_start = warm,
      seed = problem@seed,
      delta_fn = problem@delta_fn
    )
  } else {
    NULL
  }

  list(free = free, fixed_vals = fixed_vals, reduced = reduced)
}

#' Solve a fully pinned box problem
#'
#' When every coordinate of a box is pinned (`lower == upper`) the design space
#' is the single feasible point, so there is nothing to search: the optimum is
#' that point and its objective value. Returns a converged result with one
#' function evaluation, recorded against a `degenerate` engine.
#'
#' @param problem An [optim_problem] whose box has every coordinate pinned.
#' @param fixed_vals The numeric vector of pinned coordinate values (the single
#'   feasible point).
#'
#' @returns An `optimix_result` at the single feasible point.
#' @noRd
#' @keywords internal
.solve_fully_pinned <- function(problem, fixed_vals) {
  .new_result(
    par = fixed_vals,
    value = problem@fn(fixed_vals),
    counts = c(`function` = 1L, gradient = NA_integer_),
    convergence = 0L,
    message = "all dimensions pinned; single feasible point",
    engine = "degenerate",
    why = "every bound is pinned (lower == upper)",
    problem = problem
  )
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

#' Check a scalar logical argument
#'
#' @param x The argument value to validate: it must be a single `TRUE` or
#'   `FALSE`, so a vector, an `NA`, or a non-logical value is rejected.
#' @param arg A single string naming the argument, used in the error message.
#' @returns `TRUE`, invisibly, or stops with a caller-facing error.
#' @noRd
#' @keywords internal
.check_scalar_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(call. = FALSE, sprintf(
      "`%s` must be a single `TRUE` or `FALSE`.", arg
    ))
  }
  invisible(TRUE)
}

#' Check a scalar numeric argument
#'
#' @param x The argument value to validate: it must be a single non-`NA`
#'   number, so a vector, an `NA`, or a non-numeric value is rejected.
#' @param arg A single string naming the argument, used in the error message.
#' @param null_ok A single logical; when `TRUE` a `NULL` value passes, and the
#'   error message offers `NULL` as the accepted alternative.
#' @param positive A single logical; when `TRUE` the value must additionally
#'   be strictly greater than zero.
#' @returns `TRUE`, invisibly, or stops with a caller-facing error.
#' @noRd
#' @keywords internal
.check_scalar_number <- function(x, arg, null_ok = FALSE, positive = FALSE) {
  if (null_ok && is.null(x)) {
    return(invisible(TRUE))
  }
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && (!positive || x > 0)
  if (!ok) {
    msg <- if (positive) {
      sprintf("`%s` must be a single positive number.", arg)
    } else if (null_ok) {
      sprintf("`%s` must be a single number, or `NULL`.", arg)
    } else {
      sprintf("`%s` must be a single number.", arg)
    }
    stop(call. = FALSE, msg)
  }
  invisible(TRUE)
}

#' Check a scalar string argument
#'
#' @param x The argument value to validate: it must be a single character
#'   value, so a vector or a non-character value is rejected.
#' @param arg A single string naming the argument, used in the default error
#'   message.
#' @param msg An optional complete error message that overrides the default,
#'   for a call site whose message carries extra guidance.
#' @returns `TRUE`, invisibly, or stops with a caller-facing error.
#' @noRd
#' @keywords internal
.check_scalar_string <- function(x, arg, msg = NULL) {
  if (!is.character(x) || length(x) != 1L) {
    if (is.null(msg)) {
      msg <- sprintf("`%s` must be a single string.", arg)
    }
    stop(call. = FALSE, msg)
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

#' Which rows of a matrix lie inside a box
#'
#' A feasibility filter for mode matrices coming back from an external
#' mapper: a fitted mixture's component means can drift outside the design
#' box, and the contract requires every reported optimum to be feasible.
#'
#' @param m A numeric matrix, one candidate point per row.
#' @param lower,upper Numeric bound vectors of length `ncol(m)`.
#' @returns A logical vector with one entry per row of `m`.
#' @noRd
#' @keywords internal
.feasible_rows <- function(m, lower, upper) {
  vapply(
    seq_len(nrow(m)),
    function(i) all(m[i, ] >= lower & m[i, ] <= upper),
    logical(1L)
  )
}
