# optimix 0.0.0.9000

## New features

* **Orchestra membership.** optimix now emits the federation's `orchestra_manifest`
  contract: `as_orchestra_manifest()` lifts an `optimix_result` into a `parameters`
  manifest -- the optimum rides in `params`, and the engine, convergence, the true
  evaluation count, and **whether an uncertainty quantification is available** ride
  in the typed `summary`/`metadata`. A point engine sets
  `metadata$uncertainty_available = FALSE`; the proxymix mixture engine carries the
  queryable posterior over all optima in `metadata$solution_map`, so a downstream
  consumer always knows point vs posterior (some optimisation needs are outside any
  one engine -- the contract makes that explicit). `verify_manifest()` checks
  payload integrity; `digest` is a new dependency for the payload hash.
* Initial development scaffold. The package provides the unified problem
  contract (`optim_problem()`, `space_box()`, `objective_deterministic()`,
  `objective_noisy()`), the engine registry (`optim_engine()`,
  `register_optimiser()`, `list_optimisers()`), the `optimix()` facade with
  readable `minimise()` / `maximise()` aliases and a rule-based
  `method = "auto"` selector, and an `stats::optim()`-compatible result object
  with `print()`, `summary()`, `plot()`, `coef()`, `as.data.frame()`, and
  `as_optim()` methods.
* Engine adapters for the base solver (`stats::optim()` L-BFGS-B, the
  zero-dependency core) and, gated behind their suggested packages, `DEoptim`,
  `GenSA`, `nloptr` (DIRECT-L), and the optional mixture-valued mapper from
  `proxymix`.
* Combinatorial and mixed search spaces (`space_permutation()`, `opt_space()`),
  the `objective_expensive()` contract, the `optimix_map()` helper for mapping a
  problem across engines, and an incremental `delta_eval` scoring hook for engines
  that can re-score a local move without a full re-evaluation.

## Bug fixes

* Facade and contract inputs surfaced by a corner-to-corner conformance audit are
  now validated up front rather than failing silently or opaquely downstream: a
  non-logical `maximise=` (e.g. `"yes"`) is rejected instead of being coerced to
  `FALSE`; an engine that cannot handle a permutation space errors with an
  actionable message instead of an opaque crash; and `method = "auto"` on an
  expensive, high-dimensional problem falls back to a feasible engine instead of
  failing. Covered by a dedicated input-validation test suite.
