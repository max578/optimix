test_that("auto returns an optim-compatible result from an installed engine", {
  res <- optimix(sphere, lower = c(-5, -4), upper = c(4, 5))
  expect_s3_class(res, "optimix_result")
  expect_true(
    all(c("par", "value", "counts", "convergence", "message") %in% names(res))
  )
  expect_lt(res$value, 1e-2)
  expect_false(is.null(res$provenance$engine))
})

test_that("coercers and accessors behave", {
  res <- optimix(sphere, c(-5, -5), c(4, 4), method = "base_optim")
  expect_equal(coef(res), res$par)
  bare <- as_optim(res)
  expect_true(
    all(c("par", "value", "counts", "convergence", "message") %in% names(bare))
  )
  frame <- as.data.frame(res)
  expect_s3_class(frame, "data.frame")
  expect_true(all(c("x1", "x2", "value", "optimiser") %in% names(frame)))
})

test_that("print and summary emit their headers", {
  res <- optimix(sphere, c(-5, -5), c(4, 4), method = "base_optim")
  expect_output(print(res), "optimix_result")
  expect_output(summary(res), "Optimisation result")
})

test_that("unknown and unavailable engines fail with guidance", {
  expect_error(
    optimix(sphere, c(-1, -1), c(1, 1), method = "nope"),
    "Unknown optimiser"
  )
})

test_that("a seeded problem does not disturb the session RNG stream", {
  prob <- optim_problem(
    fn = sphere,
    space = space_box(lower = c(-1, -1), upper = c(1, 1)),
    seed = 99
  )
  set.seed(42)
  before <- stats::runif(3)
  set.seed(42)
  invisible(optimix(prob, method = "base_optim"))
  after <- stats::runif(3)
  expect_identical(before, after)
})

test_that("a seeded solve leaves no .Random.seed when none existed", {
  # Stash and remove any prior global RNG state so the call under test starts
  # from a session with no .Random.seed, then restore it for later tests.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    stash <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", stash, envir = globalenv()), add = TRUE)
    rm(".Random.seed", envir = globalenv())
  }
  prob <- optim_problem(
    fn = sphere,
    space = space_box(lower = c(-1, -1), upper = c(1, 1)),
    seed = 7
  )
  invisible(optimix(prob, method = "base_optim"))
  expect_false(
    exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  )
})

test_that("pinned dimensions are reduced and recorded in provenance", {
  res <- optimix(
    function(x) sum((x - c(0.5, 2, -0.25))^2),
    lower = c(-1, 2, -1), upper = c(1, 2, 1),
    method = "base_optim"
  )
  expect_equal(res$par, c(0.5, 2, -0.25), tolerance = 1e-4)
  expect_match(
    res$provenance$why,
    "solved over 2 free of 3 dimensions (pinned reduction)",
    fixed = TRUE
  )
})

test_that("the fully pinned short-circuit still works", {
  res <- optimix(
    function(x) sum(x^2), lower = c(1, 2), upper = c(1, 2),
    method = "base_optim"
  )
  expect_equal(res$par, c(1, 2))
  expect_equal(res$value, 5)
  expect_equal(res$provenance$engine, "degenerate")
})

test_that("a mixture map is dropped when pinned dimensions are reduced", {
  engine <- optim_engine(
    name = "map_stub", pkg = "base", accepts = "space_box",
    emits_map = TRUE, available = function() TRUE,
    run = function(problem) {
      start <- .start_point(problem)
      .new_result(
        par = start,
        value = problem@fn(start),
        counts = c(`function` = 1L, gradient = NA_integer_),
        convergence = 0L,
        message = "stub",
        engine = "map_stub",
        map = list(stub = TRUE),
        problem = problem
      )
    }
  )
  register_optimiser(engine)
  on.exit(rm(list = "map_stub", envir = .engine_registry), add = TRUE)
  expect_warning(
    res <- optimix(
      function(x) sum(x^2), lower = c(0, -1), upper = c(0, 1),
      method = "map_stub"
    ),
    "mixture map"
  )
  expect_null(res$map)
  unpinned <- optimix(
    function(x) sum(x^2), lower = c(-1, -1), upper = c(1, 1),
    method = "map_stub"
  )
  expect_false(is.null(unpinned$map))
})
