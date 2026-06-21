# registry.R -- The engine specification and the optimiser registry.
#
# An optim_engine bundles the metadata the selector reads with the adapter that
# runs the engine. Engines live in a package-level registry; the built-ins are
# added at load time, and users add their own with register_optimiser().

#' An optimiser engine specification
#'
#' Bundles the metadata that drives selection with the adapter that maps a
#' [optim_problem] to a backend optimiser and returns an
#' [optimix_result][optimix]. The `accepts`, `global`, `noise_tolerant`,
#' `emits_map`, and dimension fields are exactly what `method = "auto"` reads
#' when it chooses an engine, so registering an engine and teaching the selector
#' about it are one and the same act.
#'
#' @param name A single string naming the engine; this is the value passed to
#'   `method =`.
#' @param pkg A single string naming the package the engine needs, or `"base"`
#'   when it needs nothing beyond base R.
#' @param accepts A character vector of the design-space class names the engine
#'   can search, such as `"space_box"`.
#' @param global A single logical, `TRUE` if the engine performs global search.
#' @param uses_gradient A single logical, `TRUE` if the engine uses gradients.
#' @param noise_tolerant A single logical, `TRUE` if the engine handles noisy
#'   objectives well.
#' @param stochastic A single logical, `TRUE` if the engine is randomised and
#'   therefore honours a seed.
#' @param emits_map A single logical, `TRUE` if the engine returns a
#'   mixture-valued solution map.
#' @param dim_min,dim_max Single numbers bounding the problem dimensions the
#'   engine supports.
#' @param available A function of no arguments returning a single logical: is
#'   the engine usable in the current session?
#' @param run A function taking an [optim_problem] and returning an
#'   `optimix_result`.
#'
#' @returns An S7 object of class `optim_engine`.
#' @examples
#' optim_engine(
#'   name = "my_solver",
#'   pkg = "base",
#'   available = function() TRUE,
#'   run = function(problem) NULL
#' )
#' @export
optim_engine <- S7::new_class(
  "optim_engine",
  properties = list(
    name = S7::class_character,
    pkg = S7::new_property(S7::class_character, default = "base"),
    accepts = S7::new_property(S7::class_character, default = "space_box"),
    global = S7::new_property(S7::class_logical, default = FALSE),
    uses_gradient = S7::new_property(S7::class_logical, default = FALSE),
    noise_tolerant = S7::new_property(S7::class_logical, default = FALSE),
    stochastic = S7::new_property(S7::class_logical, default = FALSE),
    emits_map = S7::new_property(S7::class_logical, default = FALSE),
    dim_min = S7::new_property(S7::class_numeric, default = 1),
    dim_max = S7::new_property(S7::class_numeric, default = Inf),
    available = S7::class_function,
    run = S7::class_function
  )
)

# The registry is a private environment keyed by engine name.
.engine_registry <- new.env(parent = emptyenv())

#' Register an optimiser engine
#'
#' Adds an [optim_engine] to the registry so it can be selected by name or by
#' `method = "auto"`. Registering an engine with the same name as an existing
#' one replaces it.
#'
#' @param engine An [optim_engine] to register.
#'
#' @returns The engine name, invisibly.
#' @examples
#' engine <- optim_engine(
#'   name = "my_solver",
#'   pkg = "base",
#'   available = function() TRUE,
#'   run = function(problem) NULL
#' )
#' register_optimiser(engine)
#' "my_solver" %in% list_optimisers()$name
#' @export
register_optimiser <- function(engine) {
  if (!S7::S7_inherits(engine, optim_engine)) {
    stop(call. = FALSE, "`engine` must be an `optim_engine` object.")
  }
  assign(engine@name, engine, envir = .engine_registry)
  invisible(engine@name)
}

#' Fetch a registered engine by name
#'
#' @param name A single engine name.
#' @returns The [optim_engine], or `NULL` when no such engine is registered.
#' @noRd
#' @keywords internal
.get_engine <- function(name) {
  if (exists(name, envir = .engine_registry, inherits = FALSE)) {
    get(name, envir = .engine_registry, inherits = FALSE)
  } else {
    NULL
  }
}

#' Names of the engines whose package is installed
#'
#' @returns A character vector of available engine names.
#' @noRd
#' @keywords internal
.installed_engine_names <- function() {
  nm <- ls(.engine_registry)
  if (length(nm) == 0L) return(character(0))
  keep <- vapply(nm, function(n) isTRUE(.get_engine(n)@available()), logical(1))
  nm[keep]
}

#' List the registered optimisers
#'
#' Reports every registered engine, whether its backend package is installed,
#' and the metadata `method = "auto"` uses to choose between them.
#'
#' @returns A data frame, one row per engine, with columns `name`, `package`,
#'   `installed`, `global`, `noise_tolerant`, and `emits_map`.
#' @examples
#' list_optimisers()
#' @export
list_optimisers <- function() {
  nm <- ls(.engine_registry)
  if (length(nm) == 0L) {
    return(data.frame(
      name = character(0), package = character(0),
      installed = logical(0), global = logical(0),
      noise_tolerant = logical(0), emits_map = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  engines <- lapply(nm, .get_engine)
  out <- data.frame(
    name = vapply(engines, function(e) e@name, character(1)),
    package = vapply(engines, function(e) e@pkg, character(1)),
    installed = vapply(engines, function(e) isTRUE(e@available()), logical(1)),
    global = vapply(engines, function(e) isTRUE(e@global), logical(1)),
    noise_tolerant = vapply(
      engines, function(e) isTRUE(e@noise_tolerant), logical(1)
    ),
    emits_map = vapply(engines, function(e) isTRUE(e@emits_map), logical(1)),
    stringsAsFactors = FALSE
  )
  out[order(out$name), , drop = FALSE]
}

#' Register the built-in engines
#'
#' Called once from `.onLoad`. The base engine needs nothing beyond base R; the
#' rest are gated on their `Suggests` package, and `proxymix_map` additionally
#' requires a proxymix new enough to expose the objective mapper.
#'
#' @returns `NULL`, invisibly.
#' @noRd
#' @keywords internal
.register_builtin_engines <- function() {
  register_optimiser(optim_engine(
    name = "base_optim", pkg = "base", accepts = "space_box",
    global = FALSE, available = function() TRUE, run = .run_base_optim
  ))
  register_optimiser(optim_engine(
    name = "gensa", pkg = "GenSA", accepts = "space_box",
    global = TRUE, noise_tolerant = TRUE, stochastic = TRUE,
    available = function() requireNamespace("GenSA", quietly = TRUE),
    run = .run_gensa
  ))
  register_optimiser(optim_engine(
    name = "deoptim", pkg = "DEoptim", accepts = "space_box",
    global = TRUE, noise_tolerant = TRUE, stochastic = TRUE,
    available = function() requireNamespace("DEoptim", quietly = TRUE),
    run = .run_deoptim
  ))
  register_optimiser(optim_engine(
    name = "nloptr_directl", pkg = "nloptr", accepts = "space_box",
    global = TRUE,
    available = function() requireNamespace("nloptr", quietly = TRUE),
    run = .run_nloptr
  ))
  register_optimiser(optim_engine(
    name = "cmaes", pkg = "cmaes", accepts = "space_box",
    global = TRUE, stochastic = TRUE,
    available = function() requireNamespace("cmaes", quietly = TRUE),
    run = .run_cmaes
  ))
  register_optimiser(optim_engine(
    name = "deoptimr", pkg = "DEoptimR", accepts = "space_box",
    global = TRUE, noise_tolerant = TRUE, stochastic = TRUE,
    available = function() requireNamespace("DEoptimR", quietly = TRUE),
    run = .run_deoptimr
  ))
  register_optimiser(optim_engine(
    name = "dfoptim_hjkb", pkg = "dfoptim", accepts = "space_box",
    global = FALSE,
    available = function() requireNamespace("dfoptim", quietly = TRUE),
    run = .run_dfoptim_hjkb
  ))
  register_optimiser(optim_engine(
    name = "nloptr_bobyqa", pkg = "nloptr", accepts = "space_box",
    global = FALSE,
    available = function() requireNamespace("nloptr", quietly = TRUE),
    run = .run_nloptr_bobyqa
  ))
  register_optimiser(optim_engine(
    name = "cmaes_ipop", pkg = "cmaes", accepts = "space_box",
    global = TRUE, stochastic = TRUE,
    available = function() requireNamespace("cmaes", quietly = TRUE),
    run = .run_cmaes_ipop
  ))
  register_optimiser(optim_engine(
    name = "bayesopt", pkg = "DiceKriging", accepts = "space_box",
    global = TRUE, dim_max = 15,
    available = function() requireNamespace("DiceKriging", quietly = TRUE),
    run = .run_bayesopt
  ))
  register_optimiser(optim_engine(
    name = "proxymix_map", pkg = "proxymix", accepts = "space_box",
    global = TRUE, emits_map = TRUE, dim_max = 10,
    available = .proxymix_ready, run = .run_proxymix
  ))
  invisible(NULL)
}
