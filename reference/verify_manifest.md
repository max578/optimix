# Verify an orchestra manifest's payload integrity

Recomputes the payload hash from the object's own load-bearing slots and
compares it to the stored `data_hash`. A mismatch means the payload was
modified after emission. Matches the reference contract's
`verify_manifest()`.

## Usage

``` r
verify_manifest(m)
```

## Arguments

- m:

  An `orchestra_manifest` object.

## Value

A list with logical `ok` and a human-readable `message`.

## See also

[`as_orchestra_manifest()`](https://max578.github.io/optimix/reference/as_orchestra_manifest.md)

Other orchestra manifest:
[`as_orchestra_manifest()`](https://max578.github.io/optimix/reference/as_orchestra_manifest.md),
[`orchestra_manifest()`](https://max578.github.io/optimix/reference/orchestra_manifest.md)

## Examples

``` r
res <- optimix(function(x) sum(x^2), c(-5, -5), c(5, 5),
               method = "base_optim")
m <- as_orchestra_manifest(res)
verify_manifest(m)
#> $ok
#> [1] TRUE
#> 
#> $message
#> [1] "data_hash verified"
#> 
```
