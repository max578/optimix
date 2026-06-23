# optimix 0.0.0.9000

## New features

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
