# Objective specification

Describes the nature of the objective so the selector and the engines
can treat it appropriately. Construct one with
[`objective_deterministic()`](https://max578.github.io/optimix/reference/objective_deterministic.md)
or
[`objective_noisy()`](https://max578.github.io/optimix/reference/objective_noisy.md)
rather than calling the class directly.

## Usage

``` r
objective_spec(kind = "deterministic", noise_sd = NA_real_)
```

## Arguments

- kind:

  A single string: `"deterministic"`, `"noisy"`, or `"expensive"`.

- noise_sd:

  A single number, an optional estimate of the observation noise
  standard deviation, or `NA` when unknown.

## Value

An S7 object of class `objective_spec`.

## See also

Other objective declarations:
[`objective_deterministic()`](https://max578.github.io/optimix/reference/objective_deterministic.md),
[`objective_expensive()`](https://max578.github.io/optimix/reference/objective_expensive.md),
[`objective_noisy()`](https://max578.github.io/optimix/reference/objective_noisy.md)

## Examples

``` r
objective_spec(kind = "noisy", noise_sd = 0.1)
#> <optimix::objective_spec>
#>  @ kind    : chr "noisy"
#>  @ noise_sd: num 0.1
```
