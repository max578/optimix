# ela.R -- Native exploratory-landscape-analysis features.
#
# A small, dependency-free ELA feature set computed from an initial design and
# used by the tier-2 selector. flacco -- the usual ELA package -- is off CRAN, so
# these are computed natively from base R: a Latin-hypercube design, two
# meta-model fits (a linear and a separable-quadratic adjusted R-squared), and
# the shape of the objective distribution. The meta-model R-squared is the
# primary smoothness signal (Mersmann et al. 2011): a landscape that a quadratic
# fits well is smooth and suits a cheap local optimiser, whereas a poor
# quadratic fit signals ruggedness or multimodality and needs a global search.

# Sampling design --------------------------------------------------------------

#' Initial-design size for the ELA sample
#'
#' @param d The problem dimension.
#' @returns A single integer sample size.
#' @noRd
#' @keywords internal
.ela_size <- function(d) {
  max(50L, 20L * as.integer(d))
}

#' Latin-hypercube design over a box
#'
#' @param n The number of points.
#' @param lower,upper The box bounds.
#' @returns An `n` by `length(lower)` numeric matrix.
#' @noRd
#' @keywords internal
.ela_lhs <- function(n, lower, upper) {
  d <- length(lower)
  design <- matrix(0, nrow = n, ncol = d)
  for (j in seq_len(d)) {
    strata <- (sample.int(n) - stats::runif(n)) / n
    design[, j] <- lower[j] + strata * (upper[j] - lower[j])
  }
  design
}

#' Sample an initial design and evaluate the objective on it
#'
#' The `best` row respects the problem's orientation: consumers (the tier-3
#' race) warm-start from it, so on a maximisation problem it must be the
#' largest sampled value, not the smallest.
#'
#' @param problem An [optim_problem].
#' @param n The design size.
#' @returns A list with `X` (the design), `y` (its objective values), and `best`
#'   (the design row with the best value under the problem's orientation: the
#'   largest `y` when `problem@maximise` is `TRUE`, the smallest otherwise).
#' @noRd
#' @keywords internal
.ela_sample <- function(problem, n) {
  sp <- problem@space
  x_design <- .ela_lhs(n, sp@lower, sp@upper)
  y <- apply(x_design, 1L, problem@fn)
  best_i <- if (isTRUE(problem@maximise)) which.max(y) else which.min(y)
  list(X = x_design, y = as.numeric(y), best = x_design[best_i, ])
}

# Meta-model features ----------------------------------------------------------

#' Adjusted R-squared of a least-squares fit
#'
#' @param design The model matrix (with an intercept column).
#' @param y The response.
#' @returns The adjusted R-squared, or `NA` if the fit is degenerate.
#' @noRd
#' @keywords internal
.r_squared_adj <- function(design, y) {
  fit <- tryCatch(stats::.lm.fit(design, y), error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  sse <- sum(fit$residuals^2)
  sst <- sum((y - mean(y))^2)
  r2 <- if (sst <= 0) 1 else 1 - sse / sst
  n <- length(y)
  p <- ncol(design) - 1L
  if (n - p - 1L <= 0L) return(r2)
  1 - (1 - r2) * (n - 1L) / (n - p - 1L)
}

#' Compute the ELA feature vector for a design
#'
#' @param X The initial design.
#' @param y Its objective values.
#' @returns A named numeric vector: `lin_r2`, `quad_r2`, `y_skew`, `y_kurt`,
#'   `y_cv`.
#' @noRd
#' @keywords internal
.ela_features <- function(X, y) {
  x_std <- scale(X)
  x_std[is.nan(x_std)] <- 0
  lin_r2 <- .r_squared_adj(cbind(1, x_std), y)
  quad_r2 <- .r_squared_adj(cbind(1, x_std, x_std^2), y)
  m <- mean(y)
  s <- stats::sd(y)
  if (is.na(s) || s == 0) {
    skew <- 0
    kurt <- 0
    cv <- 0
  } else {
    skew <- mean((y - m)^3) / s^3
    kurt <- mean((y - m)^4) / s^4 - 3
    cv <- s / (abs(m) + .Machine$double.eps)
  }
  c(lin_r2 = lin_r2, quad_r2 = quad_r2, y_skew = skew, y_kurt = kurt, y_cv = cv)
}
