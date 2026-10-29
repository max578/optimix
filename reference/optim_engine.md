# An optimiser engine specification

Bundles the metadata that drives selection with the adapter that maps a
[optim_problem](https://max578.github.io/optimix/reference/optim_problem.md)
to a backend optimiser and returns an
[optimix_result](https://max578.github.io/optimix/reference/optimix.md).
The `accepts`, `global`, `noise_tolerant`, `emits_map`, and dimension
fields are exactly what `method = "auto"` reads when it chooses an
engine, so registering an engine and teaching the selector about it are
one and the same act.

## Usage

``` r
optim_engine(
  name = character(0),
  pkg = "base",
  accepts = "space_box",
  global = FALSE,
  uses_gradient = FALSE,
  noise_tolerant = FALSE,
  stochastic = FALSE,
  emits_map = FALSE,
  dim_min = 1,
  dim_max = Inf,
  available = function() NULL,
  run = function() NULL
)
```

## Arguments

- name:

  A single string naming the engine; this is the value passed to
  `method =`.

- pkg:

  A single string naming the package the engine needs, or `"base"` when
  it needs nothing beyond base R.

- accepts:

  A character vector of the design-space class names the engine can
  search, such as `"space_box"`.

- global:

  A single logical, `TRUE` if the engine performs global search.

- uses_gradient:

  A single logical, `TRUE` if the engine uses gradients.

- noise_tolerant:

  A single logical, `TRUE` if the engine handles noisy objectives well.

- stochastic:

  A single logical, `TRUE` if the engine is randomised and therefore
  honours a seed.

- emits_map:

  A single logical, `TRUE` if the engine returns a mixture-valued
  solution map.

- dim_min, dim_max:

  Single numbers bounding the problem dimensions the engine supports.

- available:

  A function of no arguments returning a single logical: is the engine
  usable in the current session?

- run:

  A function taking an
  [optim_problem](https://max578.github.io/optimix/reference/optim_problem.md)
  and returning an `optimix_result`.

## Value

An S7 object of class `optim_engine`.

## See also

Other engines and registry:
[`list_optimisers()`](https://max578.github.io/optimix/reference/list_optimisers.md),
[`register_optimiser()`](https://max578.github.io/optimix/reference/register_optimiser.md)

## Examples

``` r
optim_engine(
  name = "my_solver",
  pkg = "base",
  available = function() TRUE,
  run = function(problem) NULL
)
#> <optimix::optim_engine>
#>  @ name          : chr "my_solver"
#>  @ pkg           : chr "base"
#>  @ accepts       : chr "space_box"
#>  @ global        : logi FALSE
#>  @ uses_gradient : logi FALSE
#>  @ noise_tolerant: logi FALSE
#>  @ stochastic    : logi FALSE
#>  @ emits_map     : logi FALSE
#>  @ dim_min       : num 1
#>  @ dim_max       : num Inf
#>  @ available     : function ()  
#>  @ run           : function (problem)  
```
