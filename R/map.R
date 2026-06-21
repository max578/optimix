# map.R -- Multimodal solution mapping: return the distinct optima, not one.
#
# optimix_map() answers "where are ALL the good solutions?" rather than "what is
# the single best?". Natively it runs a local search from many Latin-hypercube
# starts and clusters the converged points into distinct optima (base R, always
# available). When a new-enough proxymix is installed it instead returns a
# calibrated Gaussian-mixture map over the optima -- the premium, queryable form.

#' Build an optimix_map result
#'
#' @param modes A matrix of optima, one per row, best first.
#' @param values The objective value at each optimum.
#' @param n The number of distinct optima.
#' @param source `"native"` or `"proxymix"`.
#' @param map The proxymix mixture, or `NULL`.
#' @returns An object of class `optimix_map`.
#' @noRd
#' @keywords internal
.new_map <- function(modes, values, n, source, map = NULL) {
  structure(
    list(modes = modes, values = values, n = n, source = source, map = map),
    class = "optimix_map"
  )
}

#' Cluster converged points into distinct optima
#'
#' @param pts A matrix of converged points, one per row.
#' @param lower,upper The box bounds (for scale-invariant clustering).
#' @param tol The merge distance, as a fraction of the box width.
#' @returns A matrix of cluster centroids, one optimum per row.
#' @noRd
#' @keywords internal
.cluster_optima <- function(pts, lower, upper, tol) {
  if (nrow(pts) <= 1L) return(pts)
  width <- upper - lower
  width[width == 0] <- 1
  scaled <- sweep(pts, 2L, width, "/")
  dmat <- stats::dist(scaled)
  if (all(dmat == 0)) return(pts[1L, , drop = FALSE])
  groups <- stats::cutree(stats::hclust(dmat, method = "complete"), h = tol)
  centroids <- t(vapply(
    sort(unique(groups)),
    function(g) colMeans(pts[groups == g, , drop = FALSE]),
    numeric(ncol(pts))
  ))
  centroids
}

#' Map the optima of a function
#'
#' Returns the distinct optima of `fn` over a box, rather than a single argmin.
#' Natively it runs a local search from many starts and clusters the results;
#' when a new-enough proxymix is installed it returns a calibrated
#' Gaussian-mixture map over the optima instead.
#'
#' @param fn The objective function: takes a numeric vector, returns a single
#'   finite number to be minimised.
#' @param lower,upper Numeric vectors of box bounds, of equal length.
#' @param ... Further arguments passed on to `fn`.
#' @param n_starts The number of local-search starts (native path); defaults to
#'   `max(20, 10 * dimension)`.
#' @param tol The merge distance for clustering optima, as a fraction of the box
#'   width. Defaults to `0.05`.
#' @param max_evals The per-start local-search budget, or `NULL` for the default.
#'
#' @returns An `optimix_map`: a list with `modes` (a matrix, one optimum per
#'   row, best first), `values`, `n`, `source` (`"native"` or `"proxymix"`),
#'   and, on the proxymix path, `map` (the queryable mixture).
#' @examples
#' # A double well with two optima at (+/-1, 0).
#' optimix_map(
#'   function(x) (x[1]^2 - 1)^2 + x[2]^2,
#'   lower = c(-2, -2), upper = c(2, 2)
#' )
#' @export
optimix_map <- function(fn, lower, upper, ..., n_starts = NULL,
                        tol = 0.05, max_evals = NULL) {
  .check_fn(fn)
  .check_bounds(lower, upper)
  d <- length(lower)
  dots <- list(...)
  obj_fn <- if (length(dots) > 0L) {
    function(x) do.call(fn, c(list(x), dots))
  } else {
    fn
  }

  if (.proxymix_ready()) {
    fit <- proxymix::from_objective(
      objective = obj_fn, lower = lower, upper = upper, minimise = TRUE
    )
    modes <- proxymix::gmm_modes(fit)
    m <- modes$modes
    vals <- apply(m, 1L, obj_fn)
    ord <- order(vals)
    return(.new_map(
      m[ord, , drop = FALSE], vals[ord], modes$n, "proxymix", map = fit
    ))
  }

  n_starts <- if (is.null(n_starts)) max(20L, 10L * d) else as.integer(n_starts)
  starts <- .ela_lhs(n_starts, lower, upper)
  pts <- matrix(0, nrow = n_starts, ncol = d)
  for (i in seq_len(n_starts)) {
    prob <- optim_problem(
      fn = obj_fn, space = space_box(lower, upper),
      warm_start = starts[i, ],
      max_evals = if (is.null(max_evals)) NA_real_ else as.numeric(max_evals)
    )
    pts[i, ] <- optimix(prob, method = "base_optim")$par
  }
  centroids <- .cluster_optima(pts, lower, upper, tol)
  vals <- apply(centroids, 1L, obj_fn)
  ord <- order(vals)
  .new_map(centroids[ord, , drop = FALSE], vals[ord], nrow(centroids), "native")
}

#' @export
print.optimix_map <- function(x, ...) {
  cat(sprintf("<optimix_map> %d optima found (%s)\n", x$n, x$source))
  shown <- min(x$n, 5L)
  for (i in seq_len(shown)) {
    cat(sprintf(
      "  [%d] value %.6g at %s\n", i, x$values[i],
      paste(format(signif(x$modes[i, ], 4)), collapse = ", ")
    ))
  }
  if (x$n > shown) {
    cat(sprintf("  ... and %d more\n", x$n - shown))
  }
  invisible(x)
}
