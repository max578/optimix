# auto.R -- The per-instance selector: tier-1 rules and tier-3 racing.
#
# Escalation: tier 1 is a transparent rule over installed engines (no extra
# evaluations), grounded in the 2026-06-21 bake-off (GenSA the best all-round
# global). When the objective is deterministic and the budget is ample, auto
# escalates to tier 3 -- racing the installed global engines on a small fraction
# of the budget and committing the remainder to the leader.
#
# A feature-based tier-2 ELA shortcut (route confidently-smooth problems
# straight to a cheap local solver) was prototyped and REJECTED. Held-out
# validation (benchmarks/tier2_validate.R) showed a single meta-model quadratic
# R-squared cannot route safely: bohachevsky is multimodal yet has quad
# R-squared = 1.0 (a quadratic bowl hiding a cosine ripple the coarse sample
# never sees), so the shortcut would send it to a local solver that fails, while
# zakharov is unimodal yet scores a low R-squared. No threshold separates them.
# Racing is feature-free -- it resolves these by actually trying the engines --
# so it is the robust default. The ELA features are still computed for the
# result's provenance and to seed the race from the best sampled point.
#
# Capability routing (the 2026-06-24 proxymix-vs-optimix from_objective study,
# ORCHESTRA_dev/comparisons/from_objective/). The study mapped when each engine
# wins against the analytic optima of standard benchmarks under a fair eval-cap:
#   - combinatorial / permutation -> perm_sa (proxymix's mixture is continuous-only);
#   - expensive -> bayesopt; noisy -> a noise-tolerant global;
#   - a single best point on a smooth box -> race the classical engines; the
#     proxymix mixture is DELIBERATELY EXCLUDED from this race -- it loses
#     single-point jobs on both accuracy and cost (3x-1524x in the study);
#   - the FULL set of optima / a posterior over the optimum (goal = "map") ->
#     the map-emitting engine (proxymix's from_objective), where it UNIQUELY wins.
# So this is intent + structural routing (space type, declared expense/noise,
# declared goal) -- never the refuted landscape-feature heuristic.

# Engine-preference helpers --------------------------------------------------

#' First installed engine from a preference order
#'
#' @param order A character vector of engine names, best first.
#' @param installed A character vector of installed engine names.
#' @returns The first preferred engine that is installed, or `NA`.
#' @noRd
#' @keywords internal
.prefer_engine <- function(order, installed) {
  hit <- order[order %in% installed]
  if (length(hit) == 0L) NA_character_ else hit[1]
}

#' Tier-1 rule-based pick (no extra evaluations)
#'
#' @param noisy Whether the objective is noisy.
#' @param installed Installed engine names.
#' @returns A list with `engine` and `why`.
#' @noRd
#' @keywords internal
.auto_tier1 <- function(noisy, installed) {
  if (noisy) {
    pick <- .prefer_engine(c("gensa", "deoptimr", "deoptim"), installed)
    if (!is.na(pick)) {
      return(list(engine = pick, why = sprintf(
        "noisy objective: %s (noise-tolerant global)", pick
      )))
    }
  }
  pick <- .prefer_engine(
    c("gensa", "deoptimr", "deoptim", "nloptr_directl"), installed
  )
  if (!is.na(pick)) {
    return(list(engine = pick, why = sprintf(
      "global box search: %s (best available global in the 2026-06-21 bake-off)",
      pick
    )))
  }
  list(
    engine = "base_optim",
    why = paste(
      "no global engine installed: base L-BFGS-B (local).",
      "Install 'GenSA' for the bake-off's strongest global optimiser."
    )
  )
}

#' The engines the race considers
#'
#' A small, diverse set: a global annealer, a restart CMA-ES (for ill-
#' conditioned landscapes), and a cheap local solver (which wins quickly on
#' smooth landscapes).
#'
#' @param installed Installed engine names.
#' @returns A character vector of engine names to race.
#' @noRd
#' @keywords internal
.race_candidates <- function(installed) {
  intersect(c("gensa", "cmaes_ipop", "nloptr_bobyqa"), installed)
}

#' Installed map-emitting engines that fit the problem dimension
#'
#' The `goal = "map"` route needs an engine that returns a mixture-valued
#' solution map (a posterior over all optima). This is read data-driven from the
#' registry's `emits_map` metadata -- so a future map-emitting engine routes here
#' with no change -- filtered to those whose dimension range admits `d`. The
#' proxymix mixture engine is preferred when several qualify.
#'
#' @param installed Installed engine names.
#' @param d The problem dimension (or `NA`).
#' @returns A character vector of map-emitting engine names, preferred first.
#' @noRd
#' @keywords internal
.map_engines <- function(installed, d) {
  cand <- Filter(function(n) {
    e <- .get_engine(n)
    isTRUE(e@emits_map) &&
      (is.na(d) || (d >= e@dim_min && d <= e@dim_max))
  }, installed)
  c(intersect("proxymix_map", cand), setdiff(cand, "proxymix_map"))
}

# The selector ----------------------------------------------------------------

#' Choose how to solve a problem
#'
#' @param problem An [optim_problem].
#' @param goal `"optimum"` (the single best point) or `"map"` (the full set of
#'   optima with a posterior over the optimum). `"map"` routes to a map-emitting
#'   engine per the 2026-06-24 from_objective study.
#' @returns A plan list carrying `tier` (1 or 3) and the tier-specific fields.
#' @noRd
#' @keywords internal
.auto_select <- function(problem, goal = "optimum") {
  installed <- .installed_engine_names()

  # Combinatorial (permutation) spaces go to the permutation annealer. A
  # continuous mixture map is not defined over permutations, so goal = "map"
  # cannot be honoured here -- noted in the provenance rather than faked.
  if (S7::S7_inherits(problem@space, space_permutation)) {
    return(list(
      tier = 1L, engine = "perm_sa",
      why = paste0("permutation design space: simulated annealing over swaps",
                   if (identical(goal, "map")) {
                     " (goal = map not available: a continuous mixture map is undefined over permutations)"
                   } else "")
    ))
  }

  # goal = "map": the user wants the FULL set of optima / a posterior over the
  # optimum, not a single best point. Route to a map-emitting engine (proxymix's
  # from_objective mixture), where the from_objective study shows it uniquely
  # wins. It is reached ONLY here -- never in the single-point race, where it
  # loses on cost.
  if (identical(goal, "map")) {
    d <- .space_dim(problem@space)
    map_set <- .map_engines(installed, d)
    if (length(map_set) > 0L) {
      return(list(
        tier = 1L, engine = map_set[1L],
        why = sprintf(paste0("goal = map: %s returns the full set of optima + a ",
                             "posterior over the optimum (from_objective study, 2026-06-24)"),
                      map_set[1L])
      ))
    }
    # No map-emitting engine fits (e.g. proxymix absent, or d above its range):
    # the need is outside the installed engines, so fall back to the single best
    # optimum and SAY SO -- never fabricate a map.
    fb <- .auto_tier1(identical(problem@objective@kind, "noisy"), installed)
    fb$why <- paste0(
      "goal = map requested but no map-emitting engine fits this problem ",
      "(install 'proxymix' for the posterior over all optima, p <= 10); ",
      "returning the single best optimum via ", fb$engine
    )
    return(c(fb, list(tier = 1L)))
  }

  # Expensive objectives go to the surrogate engine (few true evaluations).
  if (identical(problem@objective@kind, "expensive")) {
    d <- .space_dim(problem@space)
    cands <- c("bayesopt", "gensa", "deoptimr", "deoptim")
    # The GP surrogate is for low dimension: drop bayesopt when the problem is
    # outside its supported range so auto falls back rather than failing the fit
    # check (auto must only ever pick an engine that fits the problem).
    too_big <- !is.na(d) && d > .get_engine("bayesopt")@dim_max
    if (too_big) cands <- setdiff(cands, "bayesopt")
    pick <- .prefer_engine(cands, installed)
    if (is.na(pick)) pick <- "base_optim"
    why <- if (identical(pick, "bayesopt")) {
      "expensive objective: Bayesian optimisation (GP surrogate + EI)"
    } else if (too_big) {
      sprintf(
        "expensive objective at d = %d, above the surrogate range: %s (global fallback)",
        d, pick
      )
    } else {
      sprintf("expensive objective: %s (no surrogate engine installed)", pick)
    }
    return(list(tier = 1L, engine = pick, why = why))
  }

  noisy <- identical(problem@objective@kind, "noisy")
  d <- .space_dim(problem@space)
  total <- .budget(problem, 1000L * max(1L, d))
  n_ela <- .ela_size(d)
  race_set <- .race_candidates(installed)

  if (noisy || is.na(d) || total < 6L * n_ela || length(race_set) < 2L) {
    return(c(.auto_tier1(noisy, installed), list(tier = 1L)))
  }

  samp <- .ela_sample(problem, n_ela)
  feats <- .ela_features(samp$X, samp$y)
  list(
    tier = 3L, race = race_set, features = feats, ela = samp, n_ela = n_ela,
    why = sprintf(
      "race %d engines (ELA quad R2 = %.2f)", length(race_set), feats[["quad_r2"]]
    )
  )
}

# The tier-3 race --------------------------------------------------------------

#' Run the tier-3 race: short trials, then commit to the leader
#'
#' Races each candidate on a small fraction of the budget from a shared warm
#' start, then commits the remainder to the leader, warm-started from its own
#' racing best. The evaluation budget is split honestly: the race trials and the
#' ELA sample are both charged to the result's count.
#'
#' @param problem An [optim_problem].
#' @param plan A plan from `.auto_select()` (or an explicit race plan).
#' @returns An `optimix_result`.
#' @noRd
#' @keywords internal
.run_race <- function(problem, plan) {
  d <- .space_dim(problem@space)
  total <- .budget(problem, 1000L * max(1L, d))
  cands <- plan$race
  per <- max(50L, floor(total * 0.15 / length(cands)))
  warm <- plan$ela$best

  trials <- lapply(cands, function(e) {
    sub <- problem
    sub@max_evals <- per
    sub@warm_start <- warm
    .get_engine(e)@run(sub)
  })
  vals <- vapply(trials, function(r) r$value, numeric(1))
  leader_i <- if (isTRUE(problem@maximise)) which.max(vals) else which.min(vals)
  leader <- cands[leader_i]
  race_evals <- sum(vapply(trials, function(r) r$counts[["function"]], numeric(1)))

  sub2 <- problem
  sub2@max_evals <- max(per, total - race_evals)
  sub2@warm_start <- trials[[leader_i]]$par
  final <- .get_engine(leader)@run(sub2)
  final$counts["function"] <-
    final$counts[["function"]] + race_evals + plan$n_ela
  final$provenance$engine <- leader
  final$provenance$tier <- 3L
  final$provenance$why <- sprintf("%s; %s led the race", plan$why, leader)
  final$provenance$race <- stats::setNames(vals, cands)
  final$provenance$features <- plan$features
  final
}
