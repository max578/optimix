# Declare an expensive objective

Marks the objective as expensive to evaluate, so the selector prefers a
surrogate-model (Bayesian-optimisation) engine that spends few
evaluations.

## Usage

``` r
objective_expensive()
```

## Value

An
[objective_spec](https://max578.github.io/optimix/reference/objective_spec.md)
with `kind = "expensive"`.

## See also

Other objective declarations:
[`objective_deterministic()`](https://max578.github.io/optimix/reference/objective_deterministic.md),
[`objective_noisy()`](https://max578.github.io/optimix/reference/objective_noisy.md),
[`objective_spec()`](https://max578.github.io/optimix/reference/objective_spec.md)

## Examples

``` r
objective_expensive()
#> <optimix::objective_spec>
#>  @ kind    : chr "expensive"
#>  @ noise_sd: num NA
```
