# contract.R -- The unified optimisation-problem contract.
#
# A typed problem object (S7) describing one optimisation problem: its design
# space, the nature of its objective, and the search controls. The easy facade
# optimix() builds one of these internally from fn + lower + upper; power users
# construct it directly for noise, warm-starts, budgets, and seeds.

# Design spaces -----------------------------------------------------------

#' Design-space base class
#'
#' Abstract parent for the design spaces an [optim_problem()] can search. Each
#' concrete space declares the geometry an optimiser must respect. The only
#' concrete space in this release is [space_box()].
#'
#' @returns Not constructed directly; see [space_box()].
#' @export
opt_space <- S7::new_class("opt_space", abstract = TRUE)

#' Continuous box-constrained design space
#'
#' A continuous, real-valued design space bounded below and above by vectors of
#' equal length. This is the design space the easy facade [optimix()]
#' constructs from its `lower` and `upper` arguments.
#'
#' @param lower A numeric vector of lower bounds, one per dimension. Every
#'   element must be finite and not greater than its `upper` counterpart.
#' @param upper A numeric vector of upper bounds, the same length as `lower`.
#'
#' @returns An S7 object of class `space_box`.
#' @examples
#' space_box(lower = c(-5, -5), upper = c(5, 5))
#' @export
space_box <- S7::new_class(
  "space_box",
  parent = opt_space,
  properties = list(
    lower = S7::class_numeric,
    upper = S7::class_numeric
  ),
  validator = function(self) {
    if (length(self@lower) < 1L) {
      "`lower` must have at least one element"
    } else if (length(self@lower) != length(self@upper)) {
      "`lower` and `upper` must have the same length"
    } else if (any(!is.finite(self@lower)) || any(!is.finite(self@upper))) {
      "`lower` and `upper` must be finite"
    } else if (any(self@lower > self@upper)) {
      "every `lower` bound must be less than or equal to its `upper` bound"
    } else {
      NULL
    }
  }
)

# Combinatorial design spaces ---------------------------------------------

#' Permutation design space
#'
#' A combinatorial design space whose points are permutations of `seq_len(n)`.
#' Use it for ordering and assignment problems via the powerful path:
#' `optim_problem(fn, space = space_permutation(n))`, where `fn` takes a
#' permutation (an integer vector).
#'
#' @param n A single integer of at least 2: the number of items to order.
#'
#' @returns An S7 object of class `space_permutation`.
#' @examples
#' space_permutation(5)
#' @export
space_permutation <- S7::new_class(
  "space_permutation",
  parent = opt_space,
  properties = list(
    n = S7::class_numeric
  ),
  validator = function(self) {
    if (length(self@n) != 1L) {
      "`n` must be a single integer"
    } else if (is.na(self@n) || self@n %% 1 != 0) {
      "`n` must be a whole number"
    } else if (self@n < 2) {
      "`n` must be at least 2"
    } else {
      NULL
    }
  }
)

# Objective specification -------------------------------------------------

#' Objective specification
#'
#' Describes the nature of the objective so the selector and the engines can
#' treat it appropriately. Construct one with [objective_deterministic()] or
#' [objective_noisy()] rather than calling the class directly.
#'
#' @param kind A single string: `"deterministic"`, `"noisy"`, or `"expensive"`.
#' @param noise_sd A single number, an optional estimate of the observation
#'   noise standard deviation, or `NA` when unknown.
#'
#' @returns An S7 object of class `objective_spec`.
#' @export
objective_spec <- S7::new_class(
  "objective_spec",
  properties = list(
    kind = S7::new_property(S7::class_character, default = "deterministic"),
    noise_sd = S7::new_property(S7::class_numeric, default = NA_real_)
  ),
  validator = function(self) {
    if (length(self@kind) != 1L) {
      "`kind` must be a single string"
    } else if (!self@kind %in% c("deterministic", "noisy", "expensive")) {
      "`kind` must be \"deterministic\", \"noisy\", or \"expensive\""
    } else if (length(self@noise_sd) != 1L) {
      "`noise_sd` must be a single number or NA"
    } else if (!is.na(self@noise_sd) && self@noise_sd < 0) {
      "`noise_sd` must be non-negative"
    } else {
      NULL
    }
  }
)

#' Declare a deterministic objective
#'
#' Marks the objective as deterministic, so repeated evaluations at the same
#' point return the same value. This is the default for [optim_problem()].
#'
#' @returns An [objective_spec] with `kind = "deterministic"`.
#' @examples
#' objective_deterministic()
#' @export
objective_deterministic <- function() {
  objective_spec(kind = "deterministic")
}

#' Declare a noisy objective
#'
#' Marks the objective as stochastic, so the selector prefers a noise-tolerant
#' optimiser.
#'
#' @param sd A single non-negative number, an optional estimate of the
#'   observation-noise standard deviation. Use `NA` when it is unknown.
#'
#' @returns An [objective_spec] with `kind = "noisy"`.
#' @examples
#' objective_noisy(sd = 0.1)
#' @export
objective_noisy <- function(sd = NA_real_) {
  objective_spec(kind = "noisy", noise_sd = as.numeric(sd))
}

#' Declare an expensive objective
#'
#' Marks the objective as expensive to evaluate, so the selector prefers a
#' surrogate-model (Bayesian-optimisation) engine that spends few evaluations.
#'
#' @returns An [objective_spec] with `kind = "expensive"`.
#' @examples
#' objective_expensive()
#' @export
objective_expensive <- function() {
  objective_spec(kind = "expensive")
}

# The problem object ------------------------------------------------------

#' A unified optimisation problem
#'
#' The typed contract that every optimix engine consumes. The easy facade
#' [optimix()] builds one of these from `fn`, `lower`, and `upper`; construct it
#' directly to set a noisy objective, a warm start, an evaluation budget, or a
#' seed.
#'
#' @param fn The objective function. It takes a single numeric vector -- a
#'   point in the design space -- and returns a single finite number to be
#'   minimised (or maximised when `maximise = TRUE`).
#' @param space The design space to search, such as a [space_box()].
#' @param objective An [objective_spec] describing the objective; defaults to
#'   [objective_deterministic()].
#' @param maximise A single logical. When `TRUE` the objective is maximised
#'   rather than minimised. Defaults to `FALSE`.
#' @param max_evals A single number, the soft budget of objective evaluations,
#'   or `NA` to let each engine use its own default.
#' @param warm_start A numeric vector to start the search from, or a
#'   zero-length vector for none. When supplied it must match the space
#'   dimension.
#' @param seed A single integer seed for reproducible stochastic search, or
#'   `NA` for none.
#' @param delta_fn An optional incremental-scoring function with signature
#'   `function(perm, i, j)` returning the change in the objective when the
#'   permutation entries at positions `i` and `j` are swapped. When supplied, a
#'   combinatorial engine uses it for fast incremental scoring instead of
#'   re-evaluating `fn`. `NULL` (the default) means none.
#'
#' @usage NULL
#' @returns An S7 object of class `optim_problem`.
#' @examples
#' optim_problem(
#'   fn = function(x) sum(x^2),
#'   space = space_box(lower = c(-5, -5), upper = c(5, 5))
#' )
#' @export
optim_problem <- S7::new_class(
  "optim_problem",
  properties = list(
    fn = S7::class_function,
    space = opt_space,
    objective = S7::new_property(
      objective_spec,
      default = objective_deterministic()
    ),
    maximise = S7::new_property(S7::class_logical, default = FALSE),
    max_evals = S7::new_property(S7::class_numeric, default = NA_real_),
    warm_start = S7::new_property(S7::class_numeric, default = numeric(0)),
    seed = S7::new_property(S7::class_numeric, default = NA_real_),
    delta_fn = S7::new_property(S7::class_any, default = NULL)
  ),
  validator = function(self) {
    .validate_problem(self)
  }
)
