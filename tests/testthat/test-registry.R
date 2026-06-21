test_that("the built-in engines are registered", {
  tab <- list_optimisers()
  expect_s3_class(tab, "data.frame")
  expect_true(all(
    c(
      "base_optim", "perm_sa", "gensa", "deoptim", "nloptr_directl", "cmaes",
      "deoptimr", "dfoptim_hjkb", "nloptr_bobyqa", "cmaes_ipop", "bayesopt",
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
          archive = NULL, map = NULL, diagnostics = list(), problem = problem
        ),
        class = "optimix_result"
      )
    }
  )
  register_optimiser(engine)
  expect_true("constant_zero" %in% list_optimisers()$name)
  res <- optimix(sphere, c(-1, -1), c(1, 1), method = "constant_zero")
  expect_s3_class(res, "optimix_result")
  expect_equal(res$value, 0)
})
