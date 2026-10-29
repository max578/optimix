# Declare a noisy objective

Marks the objective as stochastic, so the selector prefers a
noise-tolerant optimiser.

## Usage

``` r
objective_noisy(sd = NA_real_)
```

## Arguments

- sd:

  A single non-negative number, an optional estimate of the
  observation-noise standard deviation. Use `NA` when it is unknown.

## Value

An
[objective_spec](https://max578.github.io/optimix/reference/objective_spec.md)
with `kind = "noisy"`.

## See also

Other objective declarations:
[`objective_deterministic()`](https://max578.github.io/optimix/reference/objective_deterministic.md),
[`objective_expensive()`](https://max578.github.io/optimix/reference/objective_expensive.md),
[`objective_spec()`](https://max578.github.io/optimix/reference/objective_spec.md)

## Examples

``` r
objective_noisy(sd = 0.1)
#> <optimix::objective_spec>
#>  @ kind    : chr "noisy"
#>  @ noise_sd: num 0.1
```
