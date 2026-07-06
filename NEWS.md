# optimix 0.1.0

## Bug fixes

* **The proxymix paths never report an infeasible optimum.** A fitted
  mixture's component means can drift outside the design box; `optimix_map()`
  now drops out-of-bounds modes (falling back to the native multi-start path
  when none survive, with a warning), and the `proxymix_map` engine reports
  the best in-bounds mode or fails cleanly rather than returning an
  out-of-box `par`.
* **An explicit `max_evals` is now a hard budget.** Previously `base_optim`
  interpreted the budget as L-BFGS-B *iterations* (each costing roughly
  `1 + 2d` evaluations), the population engines applied generation floors that
  could multiply a tight budget many-fold (`deoptim` at `d = 10` spent 2100
  evaluations against `max_evals = 50`), and the tier-3 race charged its
  landscape sample to the count without deducting it from the final
  allocation. Every engine now fits its run to an explicit budget (shrinking
  the population before the run length, within at most one generation's
  overshoot) or refuses with an actionable message stating its minimum; the
  race subtracts both the trials and the sample from the final run's share.
  When `max_evals` is left unset, engine defaults are unchanged. A dedicated
  budget-honesty test file pins `counts[["function"]]` to a ground-truth
  counter for every engine family.
* **`cmaes_ipop` removed after honest re-measurement.** The engine promised
  IPOP restarts with a doubled population, but the doubled `lambda` was
  computed and never passed to `cmaes::cma_es()`, and its restart loop
  swallowed every error -- including errors raised by the user's objective --
  and could return the start point as a "converged" result. The
  implementation was fixed (control list carrying `lambda`, verified against
  the installed package's documentation and source; objective errors
  re-thrown; failed restarts reported truthfully) and the CMA family slot
  re-measured on the bake-off suite: the corrected engine is
  indistinguishable from plain `cmaes` on every function and dimension
  (success 0.57 vs 0.57 over 180 instances), because under an evaluation
  budget the first run consumes the whole budget and a restart almost never
  fires -- the engine's original bake-off advantage was an artifact of the
  broken lambda arithmetic accidentally splitting the budget across random
  restarts. Per the zoo's evidence-based curation rule the engine is dropped
  rather than shipped without measured value; the `auto` race now fields
  plain `cmaes` in its place, and a stagnation-triggered true IPOP remains
  an open candidate for a future release.
* **`optimix_map()` no longer garbles one-dimensional multimodal results.**
  The native clustering path collapsed `k` distinct optima of a
  one-dimensional problem into a single row (a `t(vapply())` shape trap);
  a d = 1 double well now returns both optima. The native path is also now
  exercised directly in the tests (previously every "native" test silently
  took the proxymix branch when proxymix was installed).
* **A wrong `delta_fn` is caught instead of silently corrupting `perm_sa`.**
  The incremental contract is verified against a full re-evaluation on one
  random swap before it is trusted, the accumulated value is re-synchronised
  periodically, and the reported `value` is always a fresh evaluation at the
  returned permutation. `counts[["function"]]` now reports true objective
  evaluations, with delta evaluations reported separately in the
  diagnostics; a `warm_start` that is not a permutation of `1:n` is
  rejected.
* **A problem `seed` no longer disturbs the caller's random-number stream.**
  The session's `.Random.seed` is saved and restored around a seeded solve,
  so simulation studies that interleave optimix calls with their own draws
  keep their reproducibility.
* **Selection and reporting honesty.** The landscape sample now respects
  `maximise = TRUE` (race trials previously warm-started from the *worst*
  sampled point on maximisation problems); one failing engine no longer
  aborts the whole race while healthy candidates remain (the failure is
  recorded in the provenance); a `bayesopt` run whose surrogate fit fails
  reports that truthfully instead of claiming a completed run; the proxymix
  mapper honours the problem seed, reports real evaluation counts, and
  states that it cannot honour `max_evals`; solving a problem with pinned
  dimensions is recorded in the provenance; `provenance$tier` is populated
  uniformly across routes.
* **Contract validation hardening.** `warm_start` must lie within its box
  bounds (previously an out-of-bounds start failed deep inside the backend
  with an opaque message); `max_evals` must be a whole number in
  `[1, .Machine$integer.max]`; `optim_engine()` validates its metadata
  (name, dimension range, accepted spaces); replacing a *built-in* engine
  via `register_optimiser()` now warns; the never-populated `archive` field
  was removed from the result contract, and results are documented as
  session objects with `as_optim()` as the durable form.

## New features

* **Release infrastructure.** A README with the positioning and related-work
  statement, three new vignettes (*The auto selector and racing*,
  *Combinatorial optimisation and delta evaluation*, *Mapping optima and the
  orchestra manifest*) alongside *Getting started with optimix*, a pkgdown
  configuration, a citation file, and continuous integration with a
  cross-platform check matrix plus a zero-suggests job that proves the
  package runs with no optional engine installed.
* **Dependency floor.** The one external import (`digest`) is gone: the
  manifest hash now uses `tools::sha256sum()`, so the package imports only
  `S7` beyond base R, and the minimum R version rises to 4.5 accordingly.
* **Capability routing in `auto`.** `optimix(method = "auto")` now honours a
  `goal` argument. `goal = "map"` routes to a map-emitting engine (proxymix's
  `from_objective` mixture) for the full set of optima + a posterior over the
  optimum; `goal = "optimum"` (the default) keeps the cost-efficient
  single-best-point selection, with the mixture engine deliberately kept out of
  that race. The route is data-driven off the registry's `emits_map` metadata and
  falls back honestly -- the single best optimum, with a stated reason -- when no
  map engine fits (proxymix absent, or `p > 10`). Grounded in the
  proxymix-vs-optimix `from_objective` routing study (2026-06-24): the mixture
  engine uniquely wins find-all-modes / uncertainty problems but loses
  single-point jobs on cost, so it is auto-selected only for `goal = "map"`.
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

## Earlier development fixes

* **Degenerate (pinned) box dimensions now solve.** A `space_box()` with a
  pinned coordinate (`lower == upper`) is a legal design space the validator
  admits, but the local engines stepped the pinned variable out of its bound
  (`stats::optim()` L-BFGS-B failed with "non-finite finite-difference value";
  `GenSA`, the `auto` route for a box, rejected it with "bounds are not
  consistent"). `optimix()` now reduces a box to its free coordinates before
  engine dispatch -- holding the pinned ones fixed, a pure restriction of the
  same objective that is a no-op on non-degenerate problems -- and reconstructs
  the full result vector, so every engine handles a pinned dimension uniformly;
  an all-pinned box returns its single feasible point. Surfaced by the
  validation study's two-sided conformance sweep.
* Facade and contract inputs surfaced by a corner-to-corner conformance audit are
  now validated up front rather than failing silently or opaquely downstream: a
  non-logical `maximise=` (e.g. `"yes"`) is rejected instead of being coerced to
  `FALSE`; an engine that cannot handle a permutation space errors with an
  actionable message instead of an opaque crash; and `method = "auto"` on an
  expensive, high-dimensional problem falls back to a feasible engine instead of
  failing. Covered by a dedicated input-validation test suite.
