# engine-perm.R -- Simulated annealing over permutations.
#
# A native combinatorial engine for permutation design spaces: swap-based
# simulated annealing with Metropolis acceptance and geometric cooling, needing
# nothing beyond base R. When the problem supplies a delta_fn (the incremental-
# scoring hook), each move costs an O(1)-style delta rather than a full
# re-evaluation -- the recurring combinatorial hot-path primitive made
# first-class. The delta contract is verified against one full recompute at
# startup, re-synced periodically during the anneal, and the reported value is
# always a fresh re-evaluation of fn at the best permutation.

#' Permutation simulated-annealing adapter
#'
#' @param problem An [optim_problem] over a [space_permutation()].
#' @returns An `optimix_result` whose `par` is the best permutation found and
#'   whose `value` is a full re-evaluation of `fn` at that permutation.
#'   `counts[["function"]]` counts true objective evaluations only;
#'   `diagnostics$delta_evals` counts the `delta_fn` calls, which never
#'   touch `fn`.
#' @noRd
#' @keywords internal
.run_perm_sa <- function(problem) {
  n <- as.integer(problem@space@n)
  fn <- problem@fn
  sign <- if (isTRUE(problem@maximise)) -1 else 1
  delta_fn <- problem@delta_fn
  counter <- new.env(parent = emptyenv())
  counter$moves <- 0L   # budget units: one per scored proposal or probe
  counter$fn <- 0L      # true objective evaluations
  counter$delta <- 0L   # delta_fn evaluations, which never touch fn
  total <- .budget(problem, 100L * n)
  .maybe_seed(problem)

  # Scoring closures -----------------------------------------------------------
  # A full re-evaluation in minimisation orientation, outside the move budget
  # (the initial score, the delta check, re-syncs, and the final value).
  rescore <- function(perm) {
    counter$fn <- counter$fn + 1L
    sign * fn(perm)
  }
  full_score <- function(perm) {
    counter$moves <- counter$moves + 1L
    rescore(perm)
  }
  # The minimisation-orientation change from swapping positions i and j.
  swap_delta <- function(perm, perm_val, i, j) {
    counter$moves <- counter$moves + 1L
    if (!is.null(delta_fn)) {
      counter$delta <- counter$delta + 1L
      sign * delta_fn(perm, i, j)
    } else {
      counter$fn <- counter$fn + 1L
      prop <- perm
      prop[c(i, j)] <- perm[c(j, i)]
      sign * fn(prop) - perm_val
    }
  }

  # The starting permutation and the delta-contract check ---------------------
  cur <- if (length(problem@warm_start) == n) {
    ws <- as.integer(problem@warm_start)
    if (!identical(sort(ws), seq_len(n))) {
      stop(call. = FALSE, sprintf(
        "`warm_start` must be a permutation of 1:%d for this space.", n
      ))
    }
    ws
  } else {
    sample.int(n)
  }
  cur_val <- full_score(cur)
  best <- cur
  best_val <- cur_val

  # A wrong delta_fn corrupts the anneal silently, so verify the incremental
  # contract on one random swap against a full recompute before trusting it
  # (costs one evaluation). Both paths draw the pair, so the RNG stream --
  # and hence the anneal trajectory -- is identical with and without a
  # delta_fn.
  check <- sample.int(n, 2L)
  if (!is.null(delta_fn)) {
    prop <- cur
    prop[c(check[1L], check[2L])] <- cur[c(check[2L], check[1L])]
    d_full <- sign * (rescore(prop) - cur_val)
    counter$delta <- counter$delta + 1L
    d_claim <- delta_fn(cur, check[1L], check[2L])
    if (abs(d_claim - d_full) > 1e-8 * max(1, abs(d_full))) {
      stop(call. = FALSE, sprintf(
        paste(
          "`delta_fn` disagrees with `fn` for swap (%d, %d):",
          "`delta_fn` gives %.8g but a full re-evaluation gives %.8g."
        ),
        check[1L], check[2L], d_claim, d_full
      ))
    }
  }

  # The anneal -----------------------------------------------------------------
  # The temperature ladder is calibrated from the spread of some candidate
  # swap deltas.
  n_probe <- min(30L, max(1L, total %/% 10L))
  probe <- vapply(seq_len(n_probe), function(ignore) {
    ij <- sample.int(n, 2L)
    abs(swap_delta(cur, cur_val, ij[1L], ij[2L]))
  }, numeric(1))
  t0 <- max(stats::sd(probe), 1e-6)
  steps <- max(1L, total - counter$moves)
  alpha <- (1 / 1000)^(1 / steps)
  temp <- t0

  # Re-sync the accumulated value with a full re-evaluation every ~n^2
  # accepted delta moves, so a long delta-driven anneal cannot drift.
  resync <- n^2
  accepted <- 0L

  while (counter$moves < total) {
    ij <- sample.int(n, 2L)
    i <- ij[1L]
    j <- ij[2L]
    d <- swap_delta(cur, cur_val, i, j)
    if (d < 0 || stats::runif(1) < exp(-d / temp)) {
      cur[c(i, j)] <- cur[c(j, i)]
      accepted <- accepted + 1L
      cur_val <- if (!is.null(delta_fn) && accepted %% resync == 0) {
        rescore(cur)
      } else {
        cur_val + d
      }
      if (cur_val < best_val) {
        best_val <- cur_val
        best <- cur
      }
    }
    temp <- temp * alpha
  }

  # Report a fresh evaluation at the best permutation, never the accumulated
  # value: residual floating-point drift dies here, and the startup check has
  # already caught gross delta_fn contract violations.
  true_best <- rescore(best)

  .new_result(
    par = as.integer(best),
    value = sign * true_best,
    counts = c(`function` = counter$fn, gradient = NA_integer_),
    convergence = 0L,
    message = sprintf("permutation SA, %d moves", counter$moves),
    engine = "perm_sa",
    diagnostics = list(
      t0 = t0,
      delta_used = !is.null(delta_fn),
      delta_evals = counter$delta
    ),
    problem = problem
  )
}
