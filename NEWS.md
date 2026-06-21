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
