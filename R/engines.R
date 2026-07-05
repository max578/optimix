# engines.R -- Adapters for the base solver and the installed CRAN engines.
#
# Each adapter takes an optim_problem, maps it to the backend's native call, and
# returns a standard optimix_result. They all minimise the wrapped objective
# from .make_objective() and convert the best value back to the original
# orientation with the stored sign.

# Budget-fitting helpers -------------------------------------------------------

#' Detect an explicit evaluation budget
#'
#' @param problem An [optim_problem].
#' @returns `TRUE` when the caller set `max_evals`, `FALSE` when the engine
#'   default applies.
#' @noRd
#' @keywords internal
.explicit_budget <- function(problem) {
  !is.na(problem@max_evals)
}

#' Fit a differential-evolution population to an explicit budget
#'
#' A DE run costs `NP * (itermax + 1)` evaluations (the initial population plus
#' one population per generation; verified against `DEoptim` and `DEoptimR`
#' with a counting closure, 2026-07-05), so an explicit budget is honoured by
#' shrinking `NP` first (never below `np_min`, the engine's hard minimum) and
#' then deriving `itermax >= 1`. Because `itermax + 1 <= budget %/% NP`, the
#' fitted run never exceeds the budget.
#'
#' @param budget The explicit integer evaluation budget.
#' @param np_default The engine's default population size.
#' @param np_min The engine's minimum legal population size.
#' @param engine The engine name, for the error message.
#' @returns A list with `np` and `itermax`.
#' @noRd
#' @keywords internal
.fit_de_budget <- function(budget, np_default, np_min, engine) {
  np <- min(np_default, max(np_min, budget %/% 2L))
  if (2L * np > budget) {
    stop(call. = FALSE, sprintf(
      paste("Optimiser `%s` needs at least %d evaluations (its minimum",
            "population of %d for the initial population plus one",
            "generation); raise `max_evals` or choose another engine."),
      engine, 2L * np_min, np_min
    ))
  }
  list(np = np, itermax = max(1L, budget %/% np - 1L))
}

# The zero-dependency core -----------------------------------------------------

#' Base R optimiser adapter (the zero-dependency core)
#'
#' Runs `stats::optim()` with the L-BFGS-B method, which respects box bounds and
#' needs nothing beyond base R, so optimix is always usable with no suggested
#' packages installed.
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_base_optim <- function(problem) {
  obj <- .make_objective(problem)
  sp <- problem@space
  d <- length(sp@lower)
  # `maxit` counts L-BFGS-B iterations, and each iteration costs roughly
  # 1 + 2d evaluations (the objective plus a central-difference numerical
  # gradient), so an explicit budget maps to iterations by that ratio -- an
  # approximation that can overshoot by about one iteration's worth of
  # evaluations. Without an explicit budget the historical default holds.
  maxit <- if (.explicit_budget(problem)) {
    max(1L, .budget(problem, 500L) %/% (2L * d + 1L))
  } else {
    500L
  }
  .maybe_seed(problem)
  fit <- stats::optim(
    par = .start_point(problem),
    fn = obj$g,
    method = "L-BFGS-B",
    lower = sp@lower,
    upper = sp@upper,
    control = list(maxit = maxit)
  )
  .new_result(
    par = fit$par,
    value = obj$sign * fit$value,
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = as.integer(fit$convergence),
    message = if (is.null(fit$message)) "" else fit$message,
    engine = "base_optim",
    diagnostics = list(method = "L-BFGS-B"),
    problem = problem
  )
}

# Global engines ---------------------------------------------------------------

#' Generalised simulated annealing adapter (GenSA)
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_gensa <- function(problem) {
  obj <- .make_objective(problem)
  sp <- problem@space
  control <- list(max.call = .budget(problem, 1000L))
  if (!is.na(problem@seed)) {
    control$seed <- as.integer(problem@seed)
  }
  start <- if (length(problem@warm_start) > 0L) problem@warm_start else NULL
  # control$seed drives GenSA's own generator, yet the backend also draws from
  # R's RNG stream, so the R-level seed is not redundant: with control$seed
  # alone the result is not bitwise reproducible (probed 2026-07-05).
  .maybe_seed(problem)
  fit <- GenSA::GenSA(
    par = start,
    fn = obj$g,
    lower = sp@lower,
    upper = sp@upper,
    control = control
  )
  .new_result(
    par = as.numeric(fit$par),
    value = obj$sign * fit$value,
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = 0L,
    message = "GenSA finished",
    engine = "gensa",
    diagnostics = list(counts = fit$counts),
    problem = problem
  )
}

#' Differential evolution adapter (DEoptim)
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_deoptim <- function(problem) {
  obj <- .make_objective(problem)
  sp <- problem@space
  d <- length(sp@lower)
  budget <- .budget(problem, 200L * d)
  if (.explicit_budget(problem)) {
    # An explicit budget is honoured by shrinking the population before the
    # run length (see .fit_de_budget); DEoptim's hard minimum is NP = 4 (a
    # smaller NP is silently reset to 10 * d, which would blow the budget).
    fitted <- .fit_de_budget(budget, 10L * d, 4L, "deoptim")
    np <- fitted$np
    itermax <- fitted$itermax
  } else {
    np <- 10L * d
    itermax <- max(20L, budget %/% np)
  }
  .maybe_seed(problem)
  control <- DEoptim::DEoptim.control(NP = np, itermax = itermax, trace = FALSE)
  # DEoptim raises an advisory warning whenever NP < 10 * d; under an explicit
  # budget the shrunken population is deliberate, so exactly that warning is
  # muffled while everything else propagates.
  fit <- withCallingHandlers(
    DEoptim::DEoptim(
      fn = obj$g,
      lower = sp@lower,
      upper = sp@upper,
      control = control
    ),
    warning = function(w) {
      if (grepl("at least ten times the length", conditionMessage(w),
                fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  best <- fit$optim
  .new_result(
    par = as.numeric(best$bestmem),
    value = obj$sign * best$bestval,
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = 0L,
    message = "DEoptim finished",
    engine = "deoptim",
    diagnostics = list(bestvalit = fit$member$bestvalit),
    problem = problem
  )
}

#' DIRECT-L global adapter (nloptr)
#'
#' Uses the deterministic DIRECT-L algorithm from NLopt, a strong global
#' derivative-free method that ships free inside `nloptr`.
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_nloptr <- function(problem) {
  obj <- .make_objective(problem)
  sp <- problem@space
  opts <- list(
    algorithm = "NLOPT_GN_DIRECT_L",
    maxeval = .budget(problem, 1000L),
    xtol_rel = 1e-8
  )
  fit <- nloptr::nloptr(
    x0 = .start_point(problem),
    eval_f = obj$g,
    lb = sp@lower,
    ub = sp@upper,
    opts = opts
  )
  .new_result(
    par = fit$solution,
    value = obj$sign * fit$objective,
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = if (fit$status > 0L) 0L else 1L,
    message = fit$message,
    engine = "nloptr_directl",
    diagnostics = list(status = fit$status, iterations = fit$iterations),
    problem = problem
  )
}

#' CMA-ES adapter (cmaes)
#'
#' Covariance-matrix-adaptation evolution strategy, the strong default for
#' multimodal and ill-conditioned continuous problems. The per-generation
#' population size sets how the evaluation budget maps to iterations.
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_cmaes <- function(problem) {
  obj <- .make_objective(problem)
  sp <- problem@space
  d <- length(sp@lower)
  lambda <- 4L + floor(3 * log(d))
  budget <- .budget(problem, 1000L)
  if (.explicit_budget(problem)) {
    # Each cma_es iteration evaluates exactly lambda offspring, so the run
    # costs maxit * lambda evaluations: an explicit budget is honoured by
    # deriving maxit without the default floor of 10 iterations, and a budget
    # below one generation cannot run at all.
    if (budget < lambda) {
      stop(call. = FALSE, sprintf(
        paste("Optimiser `cmaes` needs at least %d evaluations (one",
              "generation of lambda = %d); raise `max_evals` or choose",
              "another engine."),
        lambda, lambda
      ))
    }
    maxit <- budget %/% lambda
  } else {
    maxit <- max(10L, budget %/% lambda)
  }
  .maybe_seed(problem)
  fit <- cmaes::cma_es(
    par = .start_point(problem),
    fn = obj$g,
    lower = sp@lower,
    upper = sp@upper,
    control = list(maxit = maxit)
  )
  .new_result(
    par = as.numeric(fit$par),
    value = obj$sign * fit$value,
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = as.integer(fit$convergence),
    message = if (is.null(fit$message)) "CMA-ES finished" else fit$message,
    engine = "cmaes",
    diagnostics = list(cmaes_counts = fit$counts),
    problem = problem
  )
}

#' Self-adaptive differential evolution adapter (DEoptimR)
#'
#' The jDE variant of differential evolution, which adapts its own control
#' parameters during the run.
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_deoptimr <- function(problem) {
  obj <- .make_objective(problem)
  sp <- problem@space
  d <- length(sp@lower)
  budget <- .budget(problem, 200L * d)
  if (.explicit_budget(problem)) {
    # Same budget fit as deoptim: JDEoptim costs NP * (maxiter + 1)
    # evaluations and hard-asserts NP >= 4.
    fitted <- .fit_de_budget(budget, 10L * d, 4L, "deoptimr")
    np <- fitted$np
    maxiter <- fitted$itermax
  } else {
    np <- 10L * d
    maxiter <- max(20L, budget %/% np)
  }
  .maybe_seed(problem)
  # Stopping at the evaluation budget is the intended behaviour here, so the
  # "maximum number of iterations" warning is expected; it is captured in
  # `convergence` and `message` below rather than raised as a bare warning.
  fit <- withCallingHandlers(
    DEoptimR::JDEoptim(
      lower = sp@lower,
      upper = sp@upper,
      fn = obj$g,
      NP = np,
      maxiter = maxiter,
      trace = FALSE
    ),
    warning = function(w) {
      if (grepl("maximum number of iterations", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  .new_result(
    par = as.numeric(fit$par),
    value = obj$sign * fit$value,
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = as.integer(fit$convergence),
    message = if (fit$convergence == 0L) {
      "JDEoptim converged"
    } else {
      "JDEoptim stopped at the iteration budget"
    },
    engine = "deoptimr",
    diagnostics = list(iter = fit$iter),
    problem = problem
  )
}

# Local refiners ---------------------------------------------------------------

#' Hooke-Jeeves pattern-search adapter (dfoptim)
#'
#' A bounded direct-search method: derivative-free, deterministic, and a strong
#' cheap local refiner.
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_dfoptim_hjkb <- function(problem) {
  obj <- .make_objective(problem)
  sp <- problem@space
  fit <- dfoptim::hjkb(
    par = .start_point(problem),
    fn = obj$g,
    lower = sp@lower,
    upper = sp@upper,
    control = list(maxfeval = .budget(problem, 1000L), info = FALSE)
  )
  .new_result(
    par = as.numeric(fit$par),
    value = obj$sign * fit$value,
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = as.integer(fit$convergence),
    message = "Hooke-Jeeves finished",
    engine = "dfoptim_hjkb",
    diagnostics = list(feval = fit$feval, niter = fit$niter),
    problem = problem
  )
}

#' BOBYQA local adapter (nloptr)
#'
#' Bound-constrained quadratic-approximation local search, an efficient
#' derivative-free refiner for smooth objectives.
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_nloptr_bobyqa <- function(problem) {
  obj <- .make_objective(problem)
  sp <- problem@space
  opts <- list(
    algorithm = "NLOPT_LN_BOBYQA",
    maxeval = .budget(problem, 1000L),
    xtol_rel = 1e-8
  )
  fit <- nloptr::nloptr(
    x0 = .start_point(problem),
    eval_f = obj$g,
    lb = sp@lower,
    ub = sp@upper,
    opts = opts
  )
  .new_result(
    par = fit$solution,
    value = obj$sign * fit$objective,
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = if (fit$status > 0L) 0L else 1L,
    message = fit$message,
    engine = "nloptr_bobyqa",
    diagnostics = list(status = fit$status, iterations = fit$iterations),
    problem = problem
  )
}

# Surrogate engines ------------------------------------------------------------

#' Bayesian-optimisation (EGO) adapter, built on DiceKriging
#'
#' Efficient global optimisation for expensive objectives: fit a Gaussian-
#' process surrogate to the evaluated points, then at each step pick the point
#' that maximises Expected Improvement and evaluate the true objective there.
#' The acquisition is itself optimised by optimix (a cheap inner search over the
#' surrogate, which costs no true-objective evaluations), so the expensive
#' budget is spent only on the initial design plus one true evaluation per step.
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_bayesopt <- function(problem) {
  obj <- .make_objective(problem)
  sp <- problem@space
  d <- length(sp@lower)
  total <- .budget(problem, 15L * d + 25L)
  # EGO needs an initial design of at least one point plus one true step, so
  # a budget of one evaluation cannot run at all.
  if (total < 2L) {
    stop(call. = FALSE, paste(
      "Optimiser `bayesopt` needs `max_evals` of at least 2 (an initial",
      "design point plus one EGO step)."
    ))
  }
  n_init <- min(max(2L * d + 2L, 5L), total - 1L)
  .maybe_seed(problem)
  acq_method <- .prefer_engine(
    c("nloptr_directl", "gensa", "base_optim"), .installed_engine_names()
  )

  # The GP-fit and surrogate-prediction warnings (hyper-parameter optimisation
  # notes, near-singular designs) are expected per BO step and do not change the
  # decision, which always keeps the best true evaluation -- so they are
  # captured here rather than raised as bare warnings.
  fit_gp <- function(x_design, y) {
    suppressWarnings(tryCatch(
      DiceKriging::km(
        design = as.data.frame(x_design), response = y,
        covtype = "matern5_2", nugget.estim = TRUE,
        control = list(trace = FALSE)
      ),
      error = function(e) NULL
    ))
  }

  x_design <- .ela_lhs(n_init, sp@lower, sp@upper)
  y <- apply(x_design, 1L, obj$g)
  gp_failures <- 0L
  early_msg <- NULL
  step <- 0L
  while (obj$evals() < total) {
    step <- step + 1L
    gp <- fit_gp(x_design, y)
    if (is.null(gp)) {
      # A failed surrogate fit ends the run early; the result reports that
      # truthfully instead of claiming a completed EGO run.
      gp_failures <- gp_failures + 1L
      early_msg <- sprintf(
        paste("EGO (DiceKriging), %d evaluations; GP fit failed at step %d,",
              "returned best of design"),
        obj$evals(), step
      )
      break
    }
    y_min <- min(y)
    neg_ei <- function(x) {
      pred <- suppressWarnings(stats::predict(
        gp, newdata = as.data.frame(matrix(x, nrow = 1L)),
        type = "UK", checkNames = FALSE, light.return = TRUE
      ))
      s <- pred$sd
      if (s < 1e-9) return(0)
      z <- (y_min - pred$mean) / s
      -((y_min - pred$mean) * stats::pnorm(z) + s * stats::dnorm(z))
    }
    acq <- optimix(
      neg_ei, lower = sp@lower, upper = sp@upper,
      method = acq_method, max_evals = 100L * d
    )
    x_new <- acq$par
    # A repeated EI argmax duplicates a design row -- the classic trigger for
    # a singular kriging matrix -- so a duplicate is replaced by a seeded
    # uniform draw (pure exploration) before it enters the design.
    tol <- 1e-8 * max(sp@upper - sp@lower)
    dup <- any(vapply(
      seq_len(nrow(x_design)),
      function(i) max(abs(x_design[i, ] - x_new)) <= tol,
      logical(1L)
    ))
    if (dup) {
      x_new <- stats::runif(d, sp@lower, sp@upper)
    }
    x_design <- rbind(x_design, x_new)
    y <- c(y, obj$g(x_new))
  }

  best_i <- which.min(y)
  .new_result(
    par = as.numeric(x_design[best_i, ]),
    value = obj$sign * y[best_i],
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = if (is.null(early_msg)) 0L else 1L,
    message = if (is.null(early_msg)) {
      sprintf("EGO (DiceKriging), %d evaluations", obj$evals())
    } else {
      early_msg
    },
    engine = "bayesopt",
    diagnostics = list(
      n_init = n_init, surrogate = "matern5_2 GP", gp_failures = gp_failures
    ),
    problem = problem
  )
}
