# Register an optimiser engine

Adds an
[optim_engine](https://max578.github.io/optimix/reference/optim_engine.md)
to the registry so it can be selected by name or by `method = "auto"`.
Registering an engine with the same name as an existing one replaces it;
replacing one of the built-in engines additionally emits a warning,
since it changes what `method = "auto"` can dispatch to.

## Usage

``` r
register_optimiser(engine)
```

## Arguments

- engine:

  An
  [optim_engine](https://max578.github.io/optimix/reference/optim_engine.md)
  to register.

## Value

The engine name, invisibly.

## See also

Other engines and registry:
[`list_optimisers()`](https://max578.github.io/optimix/reference/list_optimisers.md),
[`optim_engine()`](https://max578.github.io/optimix/reference/optim_engine.md)

## Examples

``` r
engine <- optim_engine(
  name = "my_solver",
  pkg = "base",
  available = function() TRUE,
  run = function(problem) NULL
)
register_optimiser(engine)
"my_solver" %in% list_optimisers()$name
#> [1] TRUE
```
