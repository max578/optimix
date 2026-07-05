test_that("the built-in engines are registered", {
  tab <- list_optimisers()
  expect_s3_class(tab, "data.frame")
  expect_true(all(
    c(
      "base_optim", "perm_sa", "gensa", "deoptim", "nloptr_directl", "cmaes",
      "deoptimr", "dfoptim_hjkb", "nloptr_bobyqa", "bayesopt",
      "proxymix_map"
    )
    %in% tab$name
  ))
  expect_true(tab$installed[tab$name == "base_optim"])
  expect_type(tab$installed, "logical")
})

test_that("a user engine can be registered and invoked", {
  engine <- optim_engine(
    name = "constant_zero",
    pkg = "base",
    accepts = "space_box",
    available = function() TRUE,
    run = function(problem) {
      structure(
        list(
          par = problem@space@lower * 0,
          value = 0,
          counts = c(`function` = 1L, gradient = NA_integer_),
          convergence = 0L,
          message = "ok",
          provenance = list(engine = "constant_zero", why = "test"),
          map = NULL, diagnostics = list(), problem = problem
        ),
        class = "optimix_result"
      )
    }
  )
  register_optimiser(engine)
  on.exit(rm(list = "constant_zero", envir = .engine_registry), add = TRUE)
  expect_true("constant_zero" %in% list_optimisers()$name)
  res <- optimix(sphere, c(-1, -1), c(1, 1), method = "constant_zero")
  expect_s3_class(res, "optimix_result")
  expect_equal(res$value, 0)
})

test_that("optim_engine validates its specification", {
  expect_error(
    optim_engine(
      name = "bad_dims", dim_min = 5, dim_max = 2,
      available = function() TRUE, run = function(problem) NULL
    ),
    "dim_min"
  )
  expect_error(
    optim_engine(
      name = "bad_accepts", accepts = character(0),
      available = function() TRUE, run = function(problem) NULL
    ),
    "accepts"
  )
  expect_error(
    optim_engine(
      name = "",
      available = function() TRUE, run = function(problem) NULL
    ),
    "name"
  )
  expect_error(
    optim_engine(
      name = c("two", "names"),
      available = function() TRUE, run = function(problem) NULL
    ),
    "name"
  )
})

test_that("replacing a built-in warns; a user engine replaces silently", {
  original <- .get_engine("base_optim")
  on.exit(
    assign("base_optim", original, envir = .engine_registry),
    add = TRUE
  )
  clone <- optim_engine(
    name = "base_optim", pkg = "base", accepts = "space_box",
    available = function() TRUE, run = function(problem) NULL
  )
  expect_warning(register_optimiser(clone), "built-in engine `base_optim`")
  user <- optim_engine(
    name = "user_engine", pkg = "base",
    available = function() TRUE, run = function(problem) NULL
  )
  on.exit(rm(list = "user_engine", envir = .engine_registry), add = TRUE)
  expect_silent(register_optimiser(user))
  expect_silent(register_optimiser(user))
})

test_that("a dot-prefixed engine name is listed and counted as installed", {
  dot <- optim_engine(
    name = ".hidden_engine", pkg = "base",
    available = function() TRUE, run = function(problem) NULL
  )
  register_optimiser(dot)
  on.exit(rm(list = ".hidden_engine", envir = .engine_registry), add = TRUE)
  expect_true(".hidden_engine" %in% list_optimisers()$name)
  expect_true(".hidden_engine" %in% .installed_engine_names())
})
