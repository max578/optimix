# optimix

<!-- badges: start -->
[![R-CMD-check](https://github.com/max578/optimix/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/max578/optimix/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

optimix gives a curated portfolio of efficient optimisers one unified
problem contract and one verb. `optimix(fn, lower, upper)` behaves like
`stats::optim()`; `method = "auto"` (the default) selects an optimiser for
the problem at hand -- per-instance algorithm selection, not a fixed
favourite -- and every result records which engine ran and why.

## Usage

``` r
library(optimix)

result <- optimix(
  function(x) sum((x - 1)^2),
  lower = c(-5, -5),
  upper = c(5, 5)
)
result$par
result$provenance$why
```

The result satisfies the `optim()` return contract (`par`, `value`,
`counts`, `convergence`, `message`), so existing code keeps working, with
the optimix extras (`provenance`, `diagnostics`, an optional optima map)
alongside. `minimise()` and `maximise()` are readable aliases. For noise,
budgets, warm starts, seeds, or permutation spaces, build an
`optim_problem()` and pass it to the same verb.

## What makes it different

- **Per-instance selection with provenance.** `auto` routes on the declared
  problem structure (noisy, expensive, combinatorial, budget), and races
  the leading globals on a slice of the budget when the budget allows.
  Validated against a computed single-best baseline and a virtual-best
  oracle on held-out functions: auto beats the single-best at constrained
  budgets and matches its quality with roughly forty percent fewer
  evaluations at generous ones. A feature-based selection shortcut was
  tried and rejected on held-out evidence, so the selector stays
  feature-free.
- **A budget is a budget.** An explicit `max_evals` is enforced across
  every engine -- population engines shrink rather than overrun, and the
  race charges its own trials and landscape sample against the total.
- **One contract, continuous and combinatorial.** Box spaces and
  permutation spaces share the same problem object, result shape, and
  selector; `delta_fn` lets swap moves be scored incrementally, with the
  contract verified against a full re-evaluation at run time.
- **Dependency-light by construction.** The base solver runs with no
  suggested package installed; every engine loads lazily behind
  `Suggests`, and `auto` only routes to what is actually available.
- **Maps, not just argmins.** `optimix_map()` returns the distinct optima
  of a multimodal problem; with proxymix installed it upgrades to a
  calibrated Gaussian-mixture map over the optima.

## The engines

| Engine | Regime | Backend |
|---|---|---|
| `base_optim` | local, always available | `stats::optim()` L-BFGS-B |
| `gensa` | global, all-round | GenSA |
| `deoptimr` | global, noise-tolerant | DEoptimR (jDE) |
| `deoptim` | global | DEoptim |
| `nloptr_directl` | global, deterministic | nloptr (DIRECT-L) |
| `cmaes` | global, ill-conditioned | cmaes |
| `nloptr_bobyqa` | local, derivative-free | nloptr (BOBYQA) |
| `dfoptim_hjkb` | local, pattern search | dfoptim (Hooke-Jeeves) |
| `bayesopt` | expensive objectives | DiceKriging (EGO) |
| `perm_sa` | permutation spaces | native simulated annealing |
| `proxymix_map` | multimodal maps | proxymix (mixture-valued) |

The portfolio is curated empirically: an engine enters by winning (or
justifying itself in) a reproducible bake-off on quality, speed, and
maintenance risk, and each wrapper must reproduce its source package's
result before it is admitted. One engine has already been retired by its
own re-measurement -- the curation rule applies in both directions.

## Related work

`optimx` and `gloptim` unify many optimisers behind one interface, and
`nloptr` exposes a large algorithm library; optimix differs in the
per-instance `auto` selector with recorded provenance, the enforced
evaluation budgets, the typed contract spanning noisy, expensive, and
combinatorial regimes, and the mixture-valued optima map. If you want to
call one specific algorithm with its native knobs, those packages remain
excellent choices -- optimix is for when the right algorithm is part of
the question.

## Installation

``` r
# development version (private repository)
remotes::install_github("max578/optimix")
```

optimix needs R 4.5 or later. The package works out of the box; installing
any of the suggested engine packages widens what `auto` can route to, and
`list_optimisers()` shows what is available.

## Learn more

Four vignettes walk the surface: *Getting started with optimix*, *The auto
selector and racing*, *Combinatorial optimisation and delta evaluation*,
and *Mapping optima and the orchestra manifest*.

## Contributing

Bug reports and suggestions are welcome as
[GitHub issues](https://github.com/max578/optimix/issues).

## Citation

``` r
citation("optimix")
```

## Licence

MIT. See `LICENSE.md`.
