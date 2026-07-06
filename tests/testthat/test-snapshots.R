# Print-method snapshots on fully deterministic results. Both fixtures are
# chosen so every printed field is arithmetic-exact rather than seed-lucky:
# the base_optim solve lands on the integer optimum of a pure quadratic (par
# exactly c(1, 1), value exactly 0, a fixed evaluation count), and the map
# objective puts its four minima at the exact corners of the box, which the
# bounded local search hits exactly from every start. No timings, eval-count
# noise, or trailing-digit BLAS variation can enter these snapshots.

test_that("print and summary of a deterministic base_optim result are stable", {
  prob <- optim_problem(
    fn = shifted_sphere, space = space_box(c(-4, -4), c(4, 4)), seed = 1
  )
  res <- optimix(prob, method = "base_optim")
  expect_snapshot(print(res))
  expect_snapshot(summary(res))
})

test_that("print of a deterministic native optimix_map result is stable", {
  testthat::local_mocked_bindings(
    .proxymix_ready = function() FALSE, .package = "optimix"
  )
  # -x1^2 - x2^2 is minimised at the four exact box corners (+/-2, +/-2) with
  # value exactly -8; L-BFGS-B's active bounds end every start exactly on a
  # corner, so the clustered modes print identically on every run.
  set.seed(106)
  map_res <- optimix_map(
    function(x) -x[1]^2 - x[2]^2, lower = c(-2, -2), upper = c(2, 2)
  )
  expect_snapshot(print(map_res))
})
