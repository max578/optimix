# Map the optima of a function

Returns the distinct optima of `fn` over a box, rather than a single
argmin. Natively it runs a local search from many starts and clusters
the results; when a new-enough proxymix is installed it returns a
calibrated Gaussian-mixture map over the optima instead.

## Usage

``` r
optimix_map(
  fn,
  lower,
  upper,
  ...,
  n_starts = NULL,
  tol = 0.05,
  max_evals = NULL
)
```

## Arguments

- fn:

  The objective function: takes a numeric vector, returns a single
  finite number to be minimised.

- lower, upper:

  Numeric vectors of box bounds, of equal length.

- ...:

  Further arguments passed on to `fn`.

- n_starts:

  The number of local-search starts (native path); defaults to
  `max(20, 10 * dimension)`.

- tol:

  The merge distance for clustering optima, as a fraction of the box
  width. Defaults to `0.05`.

- max_evals:

  The per-start local-search budget, or `NULL` for the default.

## Value

An `optimix_map`: a list with `modes` (a matrix, one optimum per row,
best first), `values`, `n`, `source` (`"native"` or `"proxymix"`), and,
on the proxymix path, `map` (the queryable mixture).

## See also

Other results:
[`as_optim()`](https://max578.github.io/optimix/reference/as_optim.md),
[`optimix()`](https://max578.github.io/optimix/reference/optimix.md)

## Examples

``` r
# A double well with two optima at (+/-1, 0).
optimix_map(
  function(x) (x[1]^2 - 1)^2 + x[2]^2,
  lower = c(-2, -2), upper = c(2, 2)
)
#> <optimix_map> 3 optima found (proxymix)
#>   [1] value 8.57672e-05 at 0.9954000, 0.0000456
#>   [2] value 0.000133001 at -0.994300,  0.001852
#>   [3] value 2.59149 at 1.149, 1.578
```
