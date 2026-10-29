# Continuous box-constrained design space

A continuous, real-valued design space bounded below and above by
vectors of equal length. This is the design space the easy facade
[`optimix()`](https://max578.github.io/optimix/reference/optimix.md)
constructs from its `lower` and `upper` arguments.

## Usage

``` r
space_box(lower = integer(0), upper = integer(0))
```

## Arguments

- lower:

  A numeric vector of lower bounds, one per dimension. Every element
  must be finite and not greater than its `upper` counterpart.

- upper:

  A numeric vector of upper bounds, the same length as `lower`.

## Value

An S7 object of class `space_box`.

## See also

Other design spaces:
[`opt_space()`](https://max578.github.io/optimix/reference/opt_space.md),
[`optim_problem()`](https://max578.github.io/optimix/reference/optim_problem.md),
[`space_permutation()`](https://max578.github.io/optimix/reference/space_permutation.md)

## Examples

``` r
space_box(lower = c(-5, -5), upper = c(5, 5))
#> <optimix::space_box>
#>  @ lower: num [1:2] -5 -5
#>  @ upper: num [1:2] 5 5
```
