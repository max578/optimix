# Optimise a function

The single entry point. Pass a function with `lower` and `upper` bounds
for the easy path, or a prepared
[`optim_problem()`](https://max578.github.io/optimix/reference/optim_problem.md)
for full control. With `method = "auto"` (the default) the best
available optimiser is chosen for the problem and recorded in the
result's provenance.

## Usage

``` r
optimix(
  fn,
  lower = NULL,
  upper = NULL,
  ...,
  method = "auto",
  maximise = FALSE,
  max_evals = NULL,
  goal = c("optimum", "map")
)

minimise(fn, lower, upper, ...)

maximise(fn, lower, upper, ...)
```

## Arguments

- fn:

  The objective function, taking a numeric vector and returning a single
  finite number; or a prepared
  [`optim_problem()`](https://max578.github.io/optimix/reference/optim_problem.md),
  in which case `lower`, `upper`, `maximise`, and `max_evals` are
  ignored.

- lower:

  A numeric vector of lower bounds (easy path).

- upper:

  A numeric vector of upper bounds (easy path).

- ...:

  Further arguments passed on to `fn` at each evaluation.

- method:

  The optimiser to use: `"auto"` (the default) to select one per
  instance, `"race"` to race the installed global engines and commit to
  the leader, or the name of a registered engine (see
  [`list_optimisers()`](https://max578.github.io/optimix/reference/list_optimisers.md)).

- maximise:

  A single logical; maximise rather than minimise. Defaults to `FALSE`.

- max_evals:

  A soft budget of objective evaluations, or `NULL` to let the engine
  use its own default.

- goal:

  What to return: `"optimum"` (the default) for the single best point,
  or `"map"` for the full set of optima with a posterior over the
  optimum-set. `goal = "map"` steers `method = "auto"` to a map-emitting
  engine (proxymix's `from_objective` mixture); it is ignored when a
  specific `method` is named. For the richer queryable map object see
  [`optimix_map()`](https://max578.github.io/optimix/reference/optimix_map.md).

## Value

An `optimix_result`: a list shaped like a
[`stats::optim()`](https://rdrr.io/r/stats/optim.html) result (`par`,
`value`, `counts`, `convergence`, `message`) with optimix extras
(`provenance`, `map`, `diagnostics`, `problem`). It has
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`coef()`](https://rdrr.io/r/stats/coef.html),
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), and
[`as_optim()`](https://max578.github.io/optimix/reference/as_optim.md)
methods. Results embed the problem, including the objective closure and
its environment, and are therefore session objects;
[`as_optim()`](https://max578.github.io/optimix/reference/as_optim.md)
gives the minimal durable form for storage.

## See also

Other results:
[`as_optim()`](https://max578.github.io/optimix/reference/as_optim.md),
[`optimix_map()`](https://max578.github.io/optimix/reference/optimix_map.md)

## Examples

``` r
# Easy path: minimise a quadratic on a box.
optimix(function(x) sum(x^2), lower = c(-5, -5), upper = c(5, 5),
        method = "base_optim")
#> <optimix_result>
#>   optimiser : base_optim
#>   value     : 0
#>   par       : 0, 0
#>   evals     : 5
#>   converged : yes

# Power path: a prepared problem.
prob <- optim_problem(
  fn = function(x) sum((x - 1)^2),
  space = space_box(lower = c(-5, -5), upper = c(5, 5))
)
optimix(prob, method = "base_optim")
#> <optimix_result>
#>   optimiser : base_optim
#>   value     : 0
#>   par       : 1, 1
#>   evals     : 20
#>   converged : yes
```
