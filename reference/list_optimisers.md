# List the registered optimisers

Reports every registered engine, whether its backend package is
installed, and the metadata `method = "auto"` uses to choose between
them.

## Usage

``` r
list_optimisers()
```

## Value

A data frame, one row per engine, with columns `name`, `package`,
`installed`, `global`, `noise_tolerant`, and `emits_map`.

## See also

Other engines and registry:
[`optim_engine()`](https://max578.github.io/optimix/reference/optim_engine.md),
[`register_optimiser()`](https://max578.github.io/optimix/reference/register_optimiser.md)

## Examples

``` r
list_optimisers()
#>              name     package installed global noise_tolerant emits_map
#> 1      base_optim        base      TRUE  FALSE          FALSE     FALSE
#> 2        bayesopt DiceKriging      TRUE   TRUE          FALSE     FALSE
#> 3           cmaes       cmaes      TRUE   TRUE          FALSE     FALSE
#> 4         deoptim     DEoptim      TRUE   TRUE           TRUE     FALSE
#> 5        deoptimr    DEoptimR      TRUE   TRUE           TRUE     FALSE
#> 6    dfoptim_hjkb     dfoptim      TRUE  FALSE          FALSE     FALSE
#> 7           gensa       GenSA      TRUE   TRUE           TRUE     FALSE
#> 8   nloptr_bobyqa      nloptr      TRUE  FALSE          FALSE     FALSE
#> 9  nloptr_directl      nloptr      TRUE   TRUE          FALSE     FALSE
#> 10        perm_sa        base      TRUE   TRUE          FALSE     FALSE
#> 11   proxymix_map    proxymix      TRUE   TRUE          FALSE      TRUE
```
