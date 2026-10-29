# Permutation design space

A combinatorial design space whose points are permutations of
`seq_len(n)`. Use it for ordering and assignment problems via the
powerful path: `optim_problem(fn, space = space_permutation(n))`, where
`fn` takes a permutation (an integer vector).

## Usage

``` r
space_permutation(n = integer(0))
```

## Arguments

- n:

  A single integer of at least 2: the number of items to order.

## Value

An S7 object of class `space_permutation`.

## See also

Other design spaces:
[`opt_space()`](https://max578.github.io/optimix/reference/opt_space.md),
[`optim_problem()`](https://max578.github.io/optimix/reference/optim_problem.md),
[`space_box()`](https://max578.github.io/optimix/reference/space_box.md)

## Examples

``` r
space_permutation(5)
#> <optimix::space_permutation>
#>  @ n: num 5
```
