# Coerce an optimisation result to a base optim() list

Returns the bare
[`stats::optim()`](https://rdrr.io/r/stats/optim.html)-shaped components
of a result, for code paths that expect exactly that list and nothing
more.

## Usage

``` r
as_optim(x, ...)
```

## Arguments

- x:

  An object to coerce.

- ...:

  Unused; for method consistency.

## Value

A list with `par`, `value`, `counts`, `convergence`, and `message`.

## See also

Other results:
[`optimix()`](https://max578.github.io/optimix/reference/optimix.md),
[`optimix_map()`](https://max578.github.io/optimix/reference/optimix_map.md)

## Examples

``` r
res <- optimix(function(x) sum(x^2), c(-5, -5), c(5, 5),
               method = "base_optim")
as_optim(res)
#> $par
#> [1] 0 0
#> 
#> $value
#> [1] 0
#> 
#> $counts
#> function gradient 
#>        5       NA 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL"
#> 
```
