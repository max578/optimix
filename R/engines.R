# engines.R -- Adapters for the base solver and the installed CRAN engines.
#
# Each adapter takes an optim_problem, maps it to the backend's native call, and
# returns a standard optimix_result. They all minimise the wrapped objective
# from .make_objective() and convert the best value back to the original
# orientation with the stored sign.

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
  maxit <- .budget(problem, 500L)
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
  np <- 10L * d
  itermax <- max(20L, .budget(problem, 200L * d) %/% np)
  .maybe_seed(problem)
  control <- DEoptim::DEoptim.control(NP = np, itermax = itermax, trace = FALSE)
  fit <- DEoptim::DEoptim(
    fn = obj$g,
    lower = sp@lower,
    upper = sp@upper,
    control = control
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
  maxit <- max(10L, .budget(problem, 1000L) %/% lambda)
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
  np <- 10L * d
  maxiter <- max(20L, .budget(problem, 200L * d) %/% np)
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

#' Restart-capable CMA-ES adapter (IPOP, built on cmaes)
#'
#' Plain CMA-ES has no restart mechanism and is easily trapped in multimodal
#' landscapes (the 2026-06-21 bake-off showed this). This native IPOP wrapper
#' restarts `cmaes::cma_es()` from a fresh random start with a doubled
#' population each time, keeping the best result across restarts until the
#' evaluation budget is spent -- the standard recipe for a competitive CMA-ES.
#'
#' @param problem An [optim_problem].
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_cmaes_ipop <- function(problem) {
  obj <- .make_objective(problem)
  sp <- problem@space
  d <- length(sp@lower)
  total <- .budget(problem, 1000L)
  .maybe_seed(problem)
  lambda <- 4L + floor(3 * log(d))
  best_par <- .start_point(problem)
  best_val <- obj$g(best_par)
  restart <- 0L
  while (obj$evals() < total && restart < 20L) {
    remaining <- total - obj$evals()
    maxit <- max(10L, remaining %/% lambda)
    start <- if (restart == 0L) {
      .start_point(problem)
    } else {
      stats::runif(d, sp@lower, sp@upper)
    }
    fit <- tryCatch(
      cmaes::cma_es(
        par = start, fn = obj$g, lower = sp@lower, upper = sp@upper,
        control = list(maxit = maxit)
      ),
      error = function(e) NULL
    )
    if (!is.null(fit) && fit$value < best_val) {
      best_val <- fit$value
      best_par <- fit$par
    }
    lambda <- lambda * 2L
    restart <- restart + 1L
  }
  .new_result(
    par = as.numeric(best_par),
    value = obj$sign * best_val,
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = 0L,
    message = sprintf("IPOP-CMA-ES, %d restart(s)", restart),
    engine = "cmaes_ipop",
    diagnostics = list(restarts = restart),
    problem = problem
  )
}

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
  while (obj$evals() < total) {
    gp <- fit_gp(x_design, y)
    if (is.null(gp)) break
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
    x_design <- rbind(x_design, x_new)
    y <- c(y, obj$g(x_new))
  }

  best_i <- which.min(y)
  .new_result(
    par = as.numeric(x_design[best_i, ]),
    value = obj$sign * y[best_i],
    counts = c(`function` = obj$evals(), gradient = NA_integer_),
    convergence = 0L,
    message = sprintf("EGO (DiceKriging), %d evaluations", obj$evals()),
    engine = "bayesopt",
    diagnostics = list(n_init = n_init, surrogate = "matern5_2 GP"),
    problem = problem
  )
}
