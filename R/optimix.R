# optimix.R -- The user-facing facade and the dispatcher.
#
# optimix() is the one verb: an optim()-shaped easy path (fn + lower + upper)
# that builds a problem internally, plus a power path that accepts a prepared
# optim_problem. minimise()/maximise() are readable aliases. .optimise() picks
# the engine (by name or via the selector), checks it fits, and runs it.

#' Optimise a function
#'
#' The single entry point. Pass a function with `lower` and `upper` bounds for
#' the easy path, or a prepared [optim_problem()] for full control. With
#' `method = "auto"` (the default) the best available optimiser is chosen for
#' the problem and recorded in the result's provenance.
#'
#' @param fn The objective function, taking a numeric vector and returning a
#'   single finite number; or a prepared [optim_problem()], in which case
#'   `lower`, `upper`, `maximise`, and `max_evals` are ignored.
#' @param lower A numeric vector of lower bounds (easy path).
#' @param upper A numeric vector of upper bounds (easy path).
#' @param ... Further arguments passed on to `fn` at each evaluation.
#' @param method The optimiser to use: `"auto"` (the default) to select one per
#'   instance, `"race"` to race the installed global engines and commit to the
#'   leader, or the name of a registered engine (see [list_optimisers()]).
#' @param maximise A single logical; maximise rather than minimise. Defaults to
#'   `FALSE`.
#' @param max_evals A soft budget of objective evaluations, or `NULL` to let the
#'   engine use its own default.
#' @param goal What to return: `"optimum"` (the default) for the single best
#'   point, or `"map"` for the full set of optima with a posterior over the
#'   optimum-set. `goal = "map"` steers `method = "auto"` to a map-emitting engine
#'   (proxymix's `from_objective` mixture); it is ignored when a specific `method`
#'   is named. Grounded in the 2026-06-24 from_objective routing study: the mixture
#'   engine uniquely wins find-all-modes / uncertainty problems but loses
#'   single-point jobs on cost, so it is auto-selected only for `goal = "map"`. For
#'   the richer queryable map object see [optimix_map()].
#'
#' @returns An `optimix_result`: a list shaped like a [stats::optim()] result
#'   (`par`, `value`, `counts`, `convergence`, `message`) with optimix extras
#'   (`provenance`, `map`, `diagnostics`, `problem`). It has
#'   `print()`, `summary()`, `plot()`, `coef()`, `as.data.frame()`, and
#'   [as_optim()] methods. Results embed the problem, including the objective
#'   closure and its environment, and are therefore session objects;
#'   [as_optim()] gives the minimal durable form for storage.
#' @examples
#' # Easy path: minimise a quadratic on a box.
#' optimix(function(x) sum(x^2), lower = c(-5, -5), upper = c(5, 5),
#'         method = "base_optim")
#'
#' # Power path: a prepared problem.
#' prob <- optim_problem(
#'   fn = function(x) sum((x - 1)^2),
#'   space = space_box(lower = c(-5, -5), upper = c(5, 5))
#' )
#' optimix(prob, method = "base_optim")
#' @export
optimix <- function(fn, lower = NULL, upper = NULL, ..., method = "auto",
                    maximise = FALSE, max_evals = NULL,
                    goal = c("optimum", "map")) {
  goal <- match.arg(goal)
  if (S7::S7_inherits(fn, optim_problem)) {
    problem <- fn
  } else {
    .check_fn(fn)
    .check_bounds(lower, upper)
    if (!is.logical(maximise) || length(maximise) != 1L || is.na(maximise)) {
      stop(call. = FALSE, "`maximise` must be a single `TRUE` or `FALSE`.")
    }
    if (!is.null(max_evals) &&
        (!is.numeric(max_evals) || length(max_evals) != 1L || is.na(max_evals))) {
      stop(call. = FALSE, "`max_evals` must be a single number, or `NULL`.")
    }
    dots <- list(...)
    obj_fn <- if (length(dots) > 0L) {
      function(x) do.call(fn, c(list(x), dots))
    } else {
      fn
    }
    problem <- optim_problem(
      fn = obj_fn,
      space = space_box(lower = lower, upper = upper),
      maximise = isTRUE(maximise),
      max_evals = if (is.null(max_evals)) NA_real_ else as.numeric(max_evals)
    )
  }
  .optimise(problem, method = method, goal = goal)
}

#' @rdname optimix
#' @export
minimise <- function(fn, lower, upper, ...) {
  optimix(fn, lower = lower, upper = upper, ..., maximise = FALSE)
}

#' @rdname optimix
#' @export
maximise <- function(fn, lower, upper, ...) {
  optimix(fn, lower = lower, upper = upper, ..., maximise = TRUE)
}

#' Resolve a method and run it
#'
#' Dispatches `"auto"` (tier-1/2/3 escalation), `"race"` (an explicit race of
#' the installed global engines), or a named engine.
#'
#' @param problem An [optim_problem].
#' @param method `"auto"`, `"race"`, or a registered engine name.
#' @param goal `"optimum"` or `"map"`; steers `"auto"` only (ignored for a named
#'   method or `"race"`).
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.optimise <- function(problem, method = "auto", goal = "optimum") {
  if (!is.character(method) || length(method) != 1L) {
    stop(call. = FALSE, paste(
      "`method` must be a single string: an engine name,",
      "\"auto\", or \"race\". See `list_optimisers()`."
    ))
  }

  # ---- Preserve the caller's RNG state across a seeded solve --------------
  # A problem seed makes the engines call set.seed(), which would otherwise
  # leave the session RNG continuing the problem's stream after the call.
  # Save the global .Random.seed (when one exists) and restore it on exit, so
  # a seeded optimix() call is invisible to the caller's subsequent draws.
  # The reduced-problem recursion below re-enters this function; the inner
  # save/restore is a no-op nested inside the outer one, so composing with
  # `add = TRUE` keeps both correct.
  if (!is.na(problem@seed)) {
    has_rng <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old_rng <- if (has_rng) {
      get(".Random.seed", envir = globalenv(), inherits = FALSE)
    }
    on.exit(
      if (has_rng) {
        assign(".Random.seed", old_rng, envir = globalenv())
      } else if (
        exists(".Random.seed", envir = globalenv(), inherits = FALSE)
      ) {
        rm(".Random.seed", envir = globalenv())
      },
      add = TRUE
    )
  }

  # ---- Reduce any pinned (lower == upper) box dimensions ------------------
  # A pinned coordinate is a legal design space the `space_box` validator
  # admits, but the local engines step it outside its bound (a non-finite
  # finite-difference value) and `GenSA` rejects it. Solving only the free
  # coordinates -- a pure restriction of the same objective -- lets every
  # engine handle a pinned dimension uniformly. A non-degenerate problem is
  # returned untouched, so this is a no-op there.
  reduced <- .reduce_pinned(problem)
  if (!is.null(reduced)) {
    if (is.null(reduced$reduced)) {
      return(.solve_fully_pinned(problem, reduced$fixed_vals))
    }
    inner <- .optimise(reduced$reduced, method = method, goal = goal)
    par <- reduced$fixed_vals
    par[reduced$free] <- inner$par
    inner$par <- par
    inner$problem <- problem
    # A mixture map produced on the reduced problem lives in the reduced
    # coordinates, so lifting `$par` alone would leave it inconsistent with
    # the full-dimensional result. Dropping it is the honest option.
    if (!is.null(inner$map)) {
      warning(call. = FALSE, paste(
        "The mixture map is not available for a problem with pinned",
        "dimensions; dropping `$map` from the result."
      ))
      inner$map <- NULL
    }
    inner$provenance$why <- sprintf(
      "%s; solved over %d free of %d dimensions (pinned reduction)",
      inner$provenance$why, sum(reduced$free), length(reduced$free)
    )
    return(inner)
  }

  if (identical(method, "race")) {
    return(.optimise_race(problem))
  }
  if (identical(method, "auto")) {
    plan <- .auto_select(problem, goal = goal)
    if (isTRUE(plan$tier == 3L)) return(.run_race(problem, plan))
    name <- plan$engine
    why <- plan$why
    tier <- plan$tier
  } else {
    name <- method
    why <- "user-specified"
    tier <- NA_integer_
  }
  engine <- .get_engine(name)
  if (is.null(engine)) {
    stop(call. = FALSE, sprintf(
      "Unknown optimiser `%s`. See `list_optimisers()` for the choices.", name
    ))
  }
  if (!isTRUE(engine@available())) {
    stop(call. = FALSE, sprintf(
      "Optimiser `%s` needs package `%s`. Install it, or use method = \"auto\".",
      name, engine@pkg
    ))
  }
  .check_engine_fits(engine, problem)
  res <- engine@run(problem)
  res$provenance$engine <- name
  res$provenance$why <- why
  res$provenance$tier <- tier
  res
}

#' Race the installed global engines and commit to the leader
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.optimise_race <- function(problem) {
  if (!S7::S7_inherits(problem@space, space_box)) {
    stop(call. = FALSE, sprintf(
      paste("Racing is for continuous box design spaces; this problem has a",
            "`%s` space. Use method = \"auto\"."),
      .space_class_name(problem@space)
    ))
  }
  installed <- .installed_engine_names()
  race_set <- .race_candidates(installed)
  if (length(race_set) < 2L) {
    stop(
      call. = FALSE,
      "Racing needs at least two installed global engines, e.g. 'GenSA'."
    )
  }
  plan <- list(
    race = race_set, ela = list(best = .start_point(problem)),
    n_ela = 0L, features = NULL,
    why = sprintf("explicit race of %d engines", length(race_set))
  )
  .run_race(problem, plan)
}
