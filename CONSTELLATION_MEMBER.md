# Orchestra membership — optimix

> **This project (optimix) is a MEMBER of the Orchestra** (one coordination
> structure; reconciled 2026-06-03). Joined 2026-06-24 (manifest emitter + roster
> + conductoR tool + the proxymix-vs-optimix routing study). The leader-node is
> **ORCHESTRA_dev** (governance, roster, contracts, TACI, publication); the
> technical inference hub is **flexyBayes** (the dependency-DAG sink). This file is
> optimix's back-pointer to that charter and a map of my siblings.

- **My role:** **optimisation meta-layer** — a unified problem contract
  (`optim_problem`/`space_box`/`space_permutation`/`objective_*`) + an engine
  registry + an `auto`/`race` selector that routes a problem to the best efficient
  optimiser and returns a `stats::optim()`-compatible result. I am the federation's
  single optimisation entry point; I optimise raw objectives (and, downstream, the
  objectives other members pose — calibration misfit, a profit/utility surface).
- **Contracts I emit:** the canonical **`orchestra_manifest`**
  (`inferential_target = "parameters"` — an optimum *is* the best parameter
  vector). The optimum rides in `params`; the engine that ran, convergence, the
  **true** evaluation count, and — carefully provisioned — **whether an uncertainty
  quantification is available** ride in the typed `summary` + `metadata`. A point
  engine sets `metadata$uncertainty_available = FALSE`; the proxymix mixture engine
  additionally carries the queryable mixture (a posterior over *all* optima) in
  `metadata$solution_map`. A consumer therefore always knows point vs posterior.
- **My engines (registry):** gradient (`base_optim` L-BFGS, `nloptr_bobyqa`);
  global metaheuristics (`deoptim`, `gensa`, `cmaes`, `nloptr_directl`);
  Bayesian-optimisation for expensive objectives (`bayesopt`, DiceKriging);
  **combinatorial / permutation (`perm_sa`)**; and **`proxymix_map` — proxymix's
  `from_objective` mixture engine registered as the premium uncertainty-aware
  mode-mapper.** proxymix is one routed-to engine, not a rival.
- **My edge:** optimix → proxymix (Suggests-only, the mixture engine, acyclic);
  optimix → decideR / grainPlan / any member, downstream, via the manifest (an
  optimum feeds a decision). I take no hard dependency on another member.
- **proxymix vs optimix (the routing map):** a leader-owned capability-comparison
  study (`ORCHESTRA_dev/comparisons/from_objective/`, 2026-06-24, oracle = the
  analytic optima of standard benchmark functions, independent of both) maps
  **when each engine wins** — proxymix for multimodal / find-all-modes /
  uncertainty-required problems; classical engines for smooth / ill-conditioned /
  high-dim; `perm_sa` for combinatorial (outside proxymix's continuous-mixture
  competence). The study informs `auto`'s routing and the orchestra's
  leader-directed adoption — *not* a single global winner.
- **What binds me (charter invariants):**
  - *Standalone-functional* — the base solver runs with no suggested package
    installed; every additional engine (incl. proxymix) is a `Suggests:` adapter
    loaded lazily, so `R CMD check` is clean with none installed.
  - *Grounding-first* — each wrapped optimiser is gated by a reproduce-source check
    (≡ its source package before it may enter the bake-off); the routing study runs
    a **fair hard evaluation cap** with a true-eval counter (no budget-cheating).
  - *Leader-directed adoption* — I align routing to the leader-node's published
    comparison verdicts; I may propose innovations (a `constellation` cairn), the
    leader arbitrates.
- **Governance:** optimix is a **Max-owned personal package** (`max578`, MIT-track);
  the AAGI-AUS canon does not apply.

## My siblings (the full roster — so I am informed about the others)

| Member | Role | Class |
|---|---|---|
| flexyBayes | inference hub (owns C1/C4/C5/C7) | open |
| PESTO | calibration + manifest source (C2) | open |
| kernR | validation + TACI/ACI engine | open |
| proxymix | KL-optimal proxy compression; my `proxymix_map` mixture engine | open |
| gretaR | engine — torch MCMC | open |
| koine | synthesis — fourth opinion | open |
| terroir | data collector (C6) | open (MIT) |
| kalmix | state-space / change-point | open (MIT) |
| masque | data sovereignty (clones) | open |
| apsimR | external engine — APSIM Next Gen | open (MIT-src) |
| flexyBayesOrchestra | composition layer (surrogates, koine backend) | open |
| decideR | decision layer (loss-optimal closer; consumes the manifest) | open |
| gpfield | spatial — change-of-support GP; emits `orchestra_manifest` | open (MIT) |
| grainPlan | grain decision-orchestration (on decideR) | open (MIT) |
| optimix | **optimisation meta-layer (this package); emits `orchestra_manifest`** | open (MIT) |

Planned: genoR. **Canonical charter:** `ORCHESTRA_dev/ORCHESTRA.md` (mirrored in
the MaxAIbase brain, open tier). **Contract + dependency DAG:**
`ORCHESTRA_dev/integration/orchestra_manifest.R`.
