# Emit an optimix result as an orchestra manifest

The federation's emit / adapt generic. The `optimix_result` method turns
an optimisation result into the shared `orchestra_manifest` contract:
`inferential_target = "parameters"`, the optimum carried as a one-row
`params` table (the best parameter vector and its objective value), and
the engine provenance – which optimiser ran, why it was chosen,
convergence, the true evaluation count, and whether an uncertainty
quantification is available – in the typed `summary` and `metadata`.
When the proxymix mixture engine ran, `metadata$solution_map` carries
the queryable mixture over all optima, so a downstream consumer that
needs a posterior over the optimum (not just a point) can read it; a
point engine sets `metadata$uncertainty_available = FALSE`. The payload
is integrity-hashed so a consumer can verify it with
[`verify_manifest()`](https://max578.github.io/optimix/reference/verify_manifest.md).

## Usage

``` r
as_orchestra_manifest(x, ...)

# S3 method for class 'optimix_result'
as_orchestra_manifest(x, ..., run_id = NULL, seed = NA_integer_)
```

## Arguments

- x:

  An `optimix_result` object.

- ...:

  Further arguments passed to methods.

- run_id:

  Optional character run identifier; a content hash is derived when
  omitted, so two identical optima get the same identifier.

- seed:

  Optional integer recorded for reproducibility.

## Value

An `orchestra_manifest` S7 object.

## See also

[`verify_manifest()`](https://max578.github.io/optimix/reference/verify_manifest.md),
[`optimix()`](https://max578.github.io/optimix/reference/optimix.md)

Other orchestra manifest:
[`orchestra_manifest()`](https://max578.github.io/optimix/reference/orchestra_manifest.md),
[`verify_manifest()`](https://max578.github.io/optimix/reference/verify_manifest.md)

## Examples

``` r
res <- optimix(function(x) sum((x - 0.5)^2), c(-2, -2), c(2, 2),
               method = "base_optim")
as_orchestra_manifest(res)
#> <orchestra_manifest>
#>  @ manifest_version  : chr "2.0.0-draft"
#>  @ emitter_package   : chr "optimix"
#>  @ emitter_version   : chr "0.2.0.9000"
#>  @ inferential_target: chr "parameters"
#>  @ run_id            : chr "optimix-063f7f4c635c"
#>  @ method            : chr "optimix:base_optim"
#>  @ seed              : int NA
#>  @ params            :'data.frame':  1 obs. of  3 variables:
#>  .. $ par1 : num 0.5
#>  .. $ par2 : num 0.5
#>  .. $ value: num 0
#>  @ outputs           : NULL
#>  @ weights           : NULL
#>  @ obs_target        : NULL
#>  @ obs_schema        : NULL
#>  @ summary           :List of 4
#>  .. $ headline      : chr "optimum f = 0 via base_optim"
#>  .. $ abstained     : logi FALSE
#>  .. $ abstain_reason: chr NA
#>  .. $ metrics       :List of 7
#>  ..  ..$ value                : num 0
#>  ..  ..$ engine               : chr "base_optim"
#>  ..  ..$ why                  : chr "user-specified"
#>  ..  ..$ convergence          : int 0
#>  ..  ..$ evals                : int 20
#>  ..  ..$ n_modes              : int 1
#>  ..  ..$ uncertainty_available: logi FALSE
#>  .. - attr(*, "class")= chr "manifest_summary"
#>  @ consumed_manifests: list()
#>  @ metadata          :List of 9
#>  .. $ derived              : logi FALSE
#>  .. $ engine               : chr "base_optim"
#>  .. $ why                  : chr "user-specified"
#>  .. $ converged            : logi TRUE
#>  .. $ evals                : int 20
#>  .. $ dim                  : int 2
#>  .. $ n_modes              : int 1
#>  .. $ uncertainty_available: logi FALSE
#>  .. $ solution_map         : NULL
#>  @ timestamp         : POSIXct[1:1], format: "2026-10-29 03:56:17"
#>  @ data_hash         : chr "sha256:063f7f4c635cd5292ad0a2a9162d7f6292044017f3ab936eb08bf3e9de7bd995"
```
