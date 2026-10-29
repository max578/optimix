# Declare a deterministic objective

Marks the objective as deterministic, so repeated evaluations at the
same point return the same value. This is the default for
[`optim_problem()`](https://max578.github.io/optimix/reference/optim_problem.md).

## Usage

``` r
objective_deterministic()
```

## Value

An
[objective_spec](https://max578.github.io/optimix/reference/objective_spec.md)
with `kind = "deterministic"`.

## See also

Other objective declarations:
[`objective_expensive()`](https://max578.github.io/optimix/reference/objective_expensive.md),
[`objective_noisy()`](https://max578.github.io/optimix/reference/objective_noisy.md),
[`objective_spec()`](https://max578.github.io/optimix/reference/objective_spec.md)

## Examples

``` r
objective_deterministic()
#> <optimix::objective_spec>
#>  @ kind    : chr "deterministic"
#>  @ noise_sd: num NA
```
