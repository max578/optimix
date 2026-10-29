# Design-space base class

Abstract parent for the design spaces an
[`optim_problem()`](https://max578.github.io/optimix/reference/optim_problem.md)
can search. Each concrete space declares the geometry an optimiser must
respect. The concrete spaces in this release are
[`space_box()`](https://max578.github.io/optimix/reference/space_box.md)
and
[`space_permutation()`](https://max578.github.io/optimix/reference/space_permutation.md).

## Usage

``` r
opt_space()
```

## Value

Not constructed directly; see
[`space_box()`](https://max578.github.io/optimix/reference/space_box.md).

## See also

Other design spaces:
[`optim_problem()`](https://max578.github.io/optimix/reference/optim_problem.md),
[`space_box()`](https://max578.github.io/optimix/reference/space_box.md),
[`space_permutation()`](https://max578.github.io/optimix/reference/space_permutation.md)
