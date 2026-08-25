# Capability routing in the auto selector (the 2026-06-24 from_objective study):
# goal = "map" reaches the mixture engine (its unique win); the default never
# does (the mixture loses single-point jobs on cost); the route is data-driven
# off the registry's emits_map metadata and falls back honestly when no map
# engine fits.

test_that("goal = 'map' routes auto to the proxymix mixture engine", {
  skip_if_not_installed("proxymix")
  f <- function(x) sum(x^2) + 0.6 * sin(6 * x[1]) * sin(6 * x[2])
  res <- optimix(f, c(-2, -2), c(2, 2), goal = "map")
  expect_identical(res$provenance$engine, "proxymix_map")
  expect_false(is.null(res$map))                      # a posterior over the optima
  expect_true(grepl("goal = map", res$provenance$why))
})

test_that("the default goal does not select the mixture engine", {
  res <- optimix(function(x) sum((x - 0.3)^2), c(-2, -2), c(2, 2))
  expect_false(identical(res$provenance$engine, "proxymix_map"))
  expect_true(is.null(res$map))                       # a single best point
})

test_that("goal = 'map' falls back honestly when no map engine fits the dimension", {
  # proxymix_map's range is p <= 10; a 12-D problem must fall back to the single
  # best optimum and say so -- never fabricate a map (the careful provisioning).
  prob <- optim_problem(
    fn = function(x) sum(x^2),
    space = space_box(lower = rep(-2, 12L), upper = rep(2, 12L)))
  plan <- optimix:::.auto_select(prob, goal = "map")
  expect_false(identical(plan$engine, "proxymix_map"))
  expect_true(grepl("no map-emitting engine", plan$why))
  # refusal: the fallback is an "assumptions not met" (goal = map cannot be
  # honoured) outcome, not just a string a caller has to grep. The plan carries
  # a typed marker, and the *result* optimix() returns is classed so the
  # federation's is_orchestra_decline() (integration/refusal_contract.R)
  # recognises it with no optimix-specific code.
  expect_true(isTRUE(plan$goal_abstained))
  res <- optimix(prob, method = "auto", goal = "map")
  expect_true(any(grepl("_abstention$", class(res))))
})

test_that("map routing is data-driven off the emits_map metadata", {
  inst <- optimix:::.installed_engine_names()
  # within range -> a map engine is offered iff proxymix is installed
  in_range <- optimix:::.map_engines(inst, d = 5L)
  if ("proxymix_map" %in% inst) {
    expect_true("proxymix_map" %in% in_range)
  }
  # above proxymix_map's dim_max (10) -> excluded
  expect_false("proxymix_map" %in% optimix:::.map_engines(inst, d = 20L))
  # the metadata the route reads is exposed
  lo <- list_optimisers()
  expect_true(isTRUE(lo$emits_map[lo$name == "proxymix_map"]))
})
