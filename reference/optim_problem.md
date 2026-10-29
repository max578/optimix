# A unified optimisation problem

The typed contract that every optimix engine consumes. The easy facade
[`optimix()`](https://max578.github.io/optimix/reference/optimix.md)
builds one of these from `fn`, `lower`, and `upper`; construct it
directly to set a noisy objective, a warm start, an evaluation budget,
or a seed.

## Arguments

- fn:

  The objective function. It takes a single numeric vector – a point in
  the design space – and returns a single finite number to be minimised
  (or maximised when `maximise = TRUE`).

- space:

  The design space to search, such as a
  [`space_box()`](https://max578.github.io/optimix/reference/space_box.md).

- objective:

  An
  [objective_spec](https://max578.github.io/optimix/reference/objective_spec.md)
  describing the objective; defaults to
  [`objective_deterministic()`](https://max578.github.io/optimix/reference/objective_deterministic.md).

- maximise:

  A single logical. When `TRUE` the objective is maximised rather than
  minimised. Defaults to `FALSE`.

- max_evals:

  A single number, the soft budget of objective evaluations, or `NA` to
  let each engine use its own default.

- warm_start:

  A numeric vector to start the search from, or a zero-length vector for
  none. When supplied it must match the space dimension.

- seed:

  A single integer seed for reproducible stochastic search, or `NA` for
  none.

- delta_fn:

  An optional incremental-scoring function with signature
  `function(perm, i, j)` returning the change in the objective when the
  permutation entries at positions `i` and `j` are swapped. When
  supplied, a combinatorial engine uses it for fast incremental scoring
  instead of re-evaluating `fn`. `NULL` (the default) means none.

## Value

An S7 object of class `optim_problem`.

## See also

Other design spaces:
[`opt_space()`](https://max578.github.io/optimix/reference/opt_space.md),
[`space_box()`](https://max578.github.io/optimix/reference/space_box.md),
[`space_permutation()`](https://max578.github.io/optimix/reference/space_permutation.md)

## Examples

``` r
optim_problem(
  fn = function(x) sum(x^2),
  space = space_box(lower = c(-5, -5), upper = c(5, 5))
)
#> <optimix::optim_problem>
#>  @ fn        : function (x)  
#>  @ space     : <optimix::space_box>
#>  .. @ lower: num [1:2] -5 -5
#>  .. @ upper: num [1:2] 5 5
#>  @ objective : <optimix::objective_spec>
#>  .. @ kind    : chr "deterministic"
#>  .. @ noise_sd: num NA
#>  @ maximise  : logi FALSE
#>  @ max_evals : num NA
#>  @ warm_start: num(0) 
#>  @ seed      : num NA
#>  @ delta_fn  : NULL
```
