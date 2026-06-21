# engine-perm.R -- Simulated annealing over permutations.
#
# A native combinatorial engine for permutation design spaces: swap-based
# simulated annealing with Metropolis acceptance and geometric cooling, needing
# nothing beyond base R. When the problem supplies a delta_fn (the incremental-
# scoring hook), each move costs an O(1)-style delta rather than a full
# re-evaluation -- the recurring combinatorial hot-path primitive made
# first-class.

#' Permutation simulated-annealing adapter
#'
#' @param problem An [optim_problem] over a [space_permutation()].
#' @returns An `optimix_result` whose `par` is the best permutation found.
#' @noRd
#' @keywords internal
.run_perm_sa <- function(problem) {
  n <- as.integer(problem@space@n)
  fn <- problem@fn
  sign <- if (isTRUE(problem@maximise)) -1 else 1
  delta_fn <- problem@delta_fn
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  total <- .budget(problem, 100L * n)
  .maybe_seed(problem)

  full_score <- function(perm) {
    counter$n <- counter$n + 1L
    sign * fn(perm)
  }
  # The minimisation-orientation change from swapping positions i and j.
  swap_delta <- function(perm, perm_val, i, j) {
    counter$n <- counter$n + 1L
    if (!is.null(delta_fn)) {
      sign * delta_fn(perm, i, j)
    } else {
      prop <- perm
      prop[c(i, j)] <- perm[c(j, i)]
      sign * fn(prop) - perm_val
    }
  }

  cur <- if (length(problem@warm_start) == n) {
    as.integer(problem@warm_start)
  } else {
    sample.int(n)
  }
  cur_val <- full_score(cur)
  best <- cur
  best_val <- cur_val

  # Temperature ladder calibrated from the spread of some candidate swap deltas.
  n_probe <- min(30L, max(1L, total %/% 10L))
  probe <- vapply(seq_len(n_probe), function(ignore) {
    ij <- sample.int(n, 2L)
    abs(swap_delta(cur, cur_val, ij[1L], ij[2L]))
  }, numeric(1))
  t0 <- max(stats::sd(probe), 1e-6)
  steps <- max(1L, total - counter$n)
  alpha <- (1 / 1000)^(1 / steps)
  temp <- t0

  while (counter$n < total) {
    ij <- sample.int(n, 2L)
    i <- ij[1L]
    j <- ij[2L]
    d <- swap_delta(cur, cur_val, i, j)
    if (d < 0 || stats::runif(1) < exp(-d / temp)) {
      cur[c(i, j)] <- cur[c(j, i)]
      cur_val <- cur_val + d
      if (cur_val < best_val) {
        best_val <- cur_val
        best <- cur
      }
    }
    temp <- temp * alpha
  }

  .new_result(
    par = as.integer(best),
    value = sign * best_val,
    counts = c(`function` = counter$n, gradient = NA_integer_),
    convergence = 0L,
    message = sprintf("permutation SA, %d moves", counter$n),
    engine = "perm_sa",
    diagnostics = list(t0 = t0, delta_used = !is.null(delta_fn)),
    problem = problem
  )
}
