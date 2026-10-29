# The orchestra ensemble-manifest contract (optimix-side implementation)

A versioned, hashed, provenance-complete S7 result object,
property-compatible with the federation's reference
`orchestra_manifest`. An `optimix_result` is adapted into one through
[`as_orchestra_manifest()`](https://max578.github.io/optimix/reference/as_orchestra_manifest.md):
the optimum rides in `params` (`inferential_target = "parameters"`), the
engine, convergence and uncertainty-availability ride in `metadata` and
the typed `summary`, and the payload is integrity-hashed so a tampered
optimum is detected downstream.

## Value

An S7 object of class `orchestra_manifest`.

## See also

[`as_orchestra_manifest()`](https://max578.github.io/optimix/reference/as_orchestra_manifest.md),
[`verify_manifest()`](https://max578.github.io/optimix/reference/verify_manifest.md)

Other orchestra manifest:
[`as_orchestra_manifest()`](https://max578.github.io/optimix/reference/as_orchestra_manifest.md),
[`verify_manifest()`](https://max578.github.io/optimix/reference/verify_manifest.md)

## Examples

``` r
# A minimal hand-built manifest; as_orchestra_manifest() fills these
# slots (and a real payload hash) from an optimix result.
m <- orchestra_manifest(
  emitter_package = "optimix",
  emitter_version = "0.0.0.9000",
  inferential_target = "parameters",
  run_id = "example-0001",
  method = "optimix:base_optim",
  params = data.frame(par1 = 0.5, value = 0.25),
  timestamp = Sys.time(),
  data_hash = "sha256:unhashed-example"
)
m@run_id
#> [1] "example-0001"
```
