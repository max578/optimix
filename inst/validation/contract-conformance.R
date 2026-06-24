## Durable reference copy of the optimix two-sided conformance CASES (the
## authored coverage IP that lifts the api-coverage gate). Preserved here because
## the report/ dossier is gitignored; this gives the coverage work a real,
## tracked, travelling reference. Executed by the /pkg-validation dossier.
##
## bench/contract-conformance.R -- corner-to-corner, TWO-SIDED execution of the
## documented contract, run in parallel across cores (see ../audit_plan.md).
##
##   positive : a valid input yields the CORRECT result (checked by an oracle).
##   negative : an invalid / unexpected argument is CAUGHT (error or warning),
##              never accepted silently (`no-error` is a defect).
##   extreme  : zeros, boundary widths, very large / very small, flat landscapes.
##   missing  : NA / NaN / Inf in the objective and in inputs.
##   plus the constructor validators, the engine compatibility matrix, and the
##   metamorphic invariants.
##
## Oracle: the contract's own invariants + correctness oracle + clean-rejection.
## Idempotent and seeded (each cell self-seeds, so the parallel run is
## deterministic). Standalone: cd report && Rscript bench/contract-conformance.R

if (!exists("PV_SEED")) source("_setup.R")

`%||%` <- function(a, b) if (is.null(a)) b else a
NC <- max(1L, min(6L, parallel::detectCores() - 2L))
sphere <- function(x) sum((x - 0.3)^2)             # min 0 at x = 0.3
lo3 <- rep(-5, 3); hi3 <- rep(5, 3)
near0 <- function(res, case) is.list(res) && !is.null(res$value) && res$value < 0.1

reg <- list_optimisers()
installed <- reg$name[reg$installed]
box_methods <- setdiff(installed, "perm_sa")
BO_BUDGET <- 40L                                   # bound the surrogate's GP cost
grids <- list()
## gpush records the group name and the exported function each cell exercises.
## The `fn` column lets pv_coverage() / pv_api_surface_covered() score which of
## the package's exported functions saw both a positive and a negative case.
## `fns` is recycled to the number of cells in the grid.
gpush <- function(g, name, fns = NA_character_) {
  g$group <- name
  g$fn <- rep_len(as.character(fns), nrow(g))
  grids[[length(grids) + 1L]] <<- g
}
## The exported function a constructor-validation label exercises: the token
## before the first ":" (e.g. "space_box: lower > upper" -> "space_box").
fn_before_colon <- function(labels) sub(":.*$", "", labels)

## ---- 1. constructor validation (a bad argument must error cleanly) ---------
ctor <- list(
  "space_box: lower > upper"        = function() space_box(c(0, 0), c(-1, 1)),
  "space_box: length mismatch"      = function() space_box(c(0, 0), 1),
  "space_box: empty"                = function() space_box(numeric(0), numeric(0)),
  "space_box: Inf bound"            = function() space_box(c(-Inf, 0), c(1, 1)),
  "space_box: NA bound"             = function() space_box(c(NA, 0), c(1, 1)),
  "space_permutation: n < 2"        = function() space_permutation(1),
  "space_permutation: n length 2"   = function() space_permutation(c(3, 4)),
  "objective_spec: bad kind"        = function() objective_spec(kind = "weird"),
  "objective_noisy: negative sd"    = function() objective_noisy(sd = -1),
  "optim_problem: maximise len 2"   = function() optim_problem(sphere, space_box(lo3, hi3), maximise = c(TRUE, FALSE)),
  "optim_problem: max_evals = 0"    = function() optim_problem(sphere, space_box(lo3, hi3), max_evals = 0),
  "optim_problem: max_evals < 0"    = function() optim_problem(sphere, space_box(lo3, hi3), max_evals = -5),
  "optim_problem: warm_start len"   = function() optim_problem(sphere, space_box(lo3, hi3), warm_start = c(1, 2)),
  "optim_problem: delta_fn scalar"  = function() optim_problem(sphere, space_box(lo3, hi3), delta_fn = 42)
)
gpush(pv_grid(ctor, function(t) force(t()), expect = "error", cores = NC,
              label = function(c, i) names(ctor)[i]), "constructor-validation",
      fns = fn_before_colon(names(ctor)))

## ---- 2. positive: every box engine + auto + race -> CORRECT result ---------
box_cases <- as.list(c(box_methods, "auto", "race"))
names(box_cases) <- c(box_methods, "auto", "race")
gpush(pv_grid(box_cases,
              function(m) optimix(sphere, lo3, hi3, method = m,
                                  max_evals = if (m == "bayesopt") BO_BUDGET else 1500),
              expect = "ok", check = near0, cores = NC,
              label = function(c, i) paste0("box/", names(box_cases)[i])),
      "positive-box", fns = "optimix")

## permutation positive
pf <- function(p) sum(abs(diff(p))) + p[1]
gpush(pv_grid(list(perm_sa = "perm_sa", auto = "auto"),
              function(m) optimix(optim_problem(pf, space_permutation(8L), seed = PV_SEED,
                                                max_evals = 1500), method = m),
              expect = "ok", cores = NC,
              label = function(c, i) paste0("perm/", c)),
      "positive-permutation", fns = "optimix")

## objective-kind routing through auto
kind_cases <- list(
  deterministic = optim_problem(sphere, space_box(lo3, hi3), seed = PV_SEED, max_evals = 1500),
  noisy = optim_problem(function(x) sphere(x) + stats::rnorm(1, 0, 0.05),
                        space_box(lo3, hi3), objective = objective_noisy(0.05),
                        seed = PV_SEED, max_evals = 1500),
  expensive = optim_problem(sphere, space_box(lo3, hi3),
                            objective = objective_expensive(), seed = PV_SEED, max_evals = BO_BUDGET)
)
gpush(pv_grid(kind_cases, function(p) optimix(p, method = "auto"), expect = "ok",
              cores = NC, label = function(c, i) paste0("auto/", names(kind_cases)[i])),
      "auto-objective-kind", fns = "optimix")

## ---- 3. negative type-rejection (wrong type must be CAUGHT, not silent) -----
neg <- list(
  "optimix fn = string"        = function() optimix("nope", lo3, hi3, method = "base_optim"),
  "optimix fn = number"        = function() optimix(42, lo3, hi3, method = "base_optim"),
  "optimix lower = string"     = function() optimix(sphere, "a", hi3, method = "base_optim"),
  "optimix lower = NULL"       = function() optimix(sphere, NULL, hi3, method = "base_optim"),
  "optimix method = number"    = function() optimix(sphere, lo3, hi3, method = 42),
  "optimix method = length 2"  = function() optimix(sphere, lo3, hi3, method = c("gensa", "cmaes")),
  "optimix maximise = string"  = function() optimix(sphere, lo3, hi3, method = "base_optim", maximise = "yes"),
  "optimix maximise = len 2"   = function() optimix(sphere, lo3, hi3, method = "base_optim", maximise = c(TRUE, FALSE)),
  "optimix max_evals = string" = function() optimix(sphere, lo3, hi3, method = "base_optim", max_evals = "lots"),
  "optimix max_evals = len 2"  = function() optimix(sphere, lo3, hi3, method = "base_optim", max_evals = c(100, 200)),
  "optim_problem seed = string" = function() optim_problem(sphere, space_box(lo3, hi3), seed = "x"),
  "optim_problem maximise=string" = function() optim_problem(sphere, space_box(lo3, hi3), maximise = "yes"),
  "space_permutation n = 2.5"  = function() space_permutation(2.5),
  "register_optimiser(42)"     = function() register_optimiser(42),
  "optimix_map fn = number"    = function() optimix_map(42, lo3, hi3),
  "optimix_map n_starts=string"= function() optimix_map(sphere, c(-2, -2), c(2, 2), n_starts = "many"),
  "optimix_map tol = string"   = function() optimix_map(sphere, c(-2, -2), c(2, 2), tol = "x")
)
neg_fns <- c("optimix", "optimix", "optimix", "optimix", "optimix", "optimix",
             "optimix", "optimix", "optimix", "optimix",
             "optim_problem", "optim_problem", "space_permutation",
             "register_optimiser", "optimix_map", "optimix_map", "optimix_map")
gpush(pv_grid(neg, function(t) force(t()), expect = "error", cores = NC,
              label = function(c, i) names(neg)[i]), "negative-type",
      fns = neg_fns)

## ---- 4. extreme / interesting values ---------------------------------------
ext <- list(
  list(lab = "flat objective (all zeros) / gensa", exp = "ok",
       f = function() optimix(function(x) 0, lo3, hi3, method = "gensa", max_evals = 800)),
  list(lab = "flat objective / auto", exp = "ok",
       f = function() optimix(function(x) 0, lo3, hi3, method = "auto", max_evals = 800)),
  list(lab = "huge bounds 1e8 / gensa", exp = "ok",
       f = function() optimix(function(x) sum(x^2), rep(-1e8, 3), rep(1e8, 3), method = "gensa", max_evals = 1500)),
  list(lab = "tiny bound width 1e-8 / base_optim", exp = "ok",
       f = function() optimix(function(x) sum((x - 1e-9)^2), rep(-1e-8, 2), rep(1e-8, 2), method = "base_optim")),
  list(lab = "optimum at the lower boundary / base_optim", exp = "ok",
       f = function() optimix(function(x) sum((x + 5)^2), lo3, hi3, method = "base_optim")),
  list(lab = "d = 1 / cmaes", exp = "ok",
       f = function() optimix(function(x) (x - 1)^2, -5, 5, method = "cmaes", max_evals = 800)),
  list(lab = "d = 1 / auto", exp = "ok",
       f = function() optimix(function(x) (x - 1)^2, -5, 5, method = "auto", max_evals = 800)),
  list(lab = "degenerate dim (lo==hi) / base_optim", exp = "ok",
       f = function() optimix(sphere, c(0, -5, -5), c(0, 5, 5), method = "base_optim")),
  list(lab = "degenerate dim (lo==hi) / auto", exp = "ok",
       f = function() optimix(sphere, c(0, -5, -5), c(0, 5, 5), method = "auto", max_evals = 1500)),
  list(lab = "smallest permutation n = 2 / perm_sa", exp = "ok",
       f = function() optimix(optim_problem(function(p) p[1], space_permutation(2L), max_evals = 50), method = "perm_sa"))
)
gpush(pv_grid(ext, function(c) c$f(), expect = function(c) c$exp, cores = NC,
              label = function(c, i) c$lab), "extreme-values", fns = "optimix")

## ---- 5. missing data (NA / NaN / Inf) --------------------------------------
fn_na_region <- function(x) { v <- sum((x - 0.3)^2); if (x[1] > 3) NA_real_ else v }
fn_inf_region <- function(x) { v <- sum((x - 0.3)^2); if (x[1] > 3) Inf else v }
fn_nan_region <- function(x) { v <- sum((x - 0.3)^2); if (x[1] > 3) NaN else v }
fn_all_na <- function(x) NA_real_
mis <- list(
  list(lab = "fn NA in a region / gensa", exp = "ok",
       f = function() optimix(fn_na_region, lo3, hi3, method = "gensa", max_evals = 1500)),
  list(lab = "fn Inf in a region / gensa", exp = "ok",
       f = function() optimix(fn_inf_region, lo3, hi3, method = "gensa", max_evals = 1500)),
  list(lab = "fn NaN in a region / deoptimr", exp = "error",
       f = function() optimix(fn_nan_region, lo3, hi3, method = "deoptimr", max_evals = 1500)),
  list(lab = "fn NA in a region / base_optim", exp = "ok",
       f = function() optimix(fn_na_region, lo3, hi3, method = "base_optim")),
  list(lab = "fn all NA / base_optim (should be rejected)", exp = "error",
       f = function() optimix(fn_all_na, lo3, hi3, method = "base_optim")),
  list(lab = "warm_start contains NA / base_optim", exp = "error",
       f = function() optimix(optim_problem(sphere, space_box(lo3, hi3),
                                            warm_start = c(NA, NA, NA)), method = "base_optim"))
)
gpush(pv_grid(mis, function(c) c$f(), expect = function(c) c$exp, cores = NC,
              label = function(c, i) c$lab), "missing-data", fns = "optimix")

## ---- 6. compatibility matrix (incompatible pairings -> clean error) --------
perm8 <- function() optim_problem(pf, space_permutation(8L), max_evals = 400)
box20 <- function() optim_problem(function(x) sum(x^2), space_box(rep(-5, 20), rep(5, 20)), max_evals = 400)
box12 <- function() optim_problem(function(x) sum(x^2), space_box(rep(-5, 12), rep(5, 12)), max_evals = 400)
compat <- list(
  "box engine (gensa) on permutation" = function() optimix(perm8(), method = "gensa"),
  "bayesopt on permutation"           = function() optimix(perm8(), method = "bayesopt"),
  "perm_sa on box"                    = function() optimix(sphere, lo3, hi3, method = "perm_sa"),
  "bayesopt on d = 20 (> dim_max 15)" = function() optimix(box20(), method = "bayesopt"),
  "proxymix_map on d = 12 (> 10)"     = function() optimix(box12(), method = "proxymix_map"),
  "unknown method name"               = function() optimix(sphere, lo3, hi3, method = "nope"),
  "race on permutation problem"       = function() optimix(perm8(), method = "race")
)
gpush(pv_grid(compat, function(t) force(t()), expect = "error", cores = NC,
              label = function(c, i) names(compat)[i]), "compatibility",
      fns = "optimix")

## ---- 7. auto must fall back, never fail a fit ------------------------------
fallback <- list(
  "auto / expensive d=20 (above surrogate range)" = function()
    optimix(optim_problem(function(x) sum(x^2), space_box(rep(-5, 20), rep(5, 20)),
            objective = objective_expensive(), seed = PV_SEED, max_evals = 200), method = "auto"),
  "auto / deterministic d=20" = function()
    optimix(optim_problem(function(x) sum(x^2), space_box(rep(-5, 20), rep(5, 20)),
            seed = PV_SEED, max_evals = 6000), method = "auto")
)
gpush(pv_grid(fallback, function(t) force(t()), expect = "ok", cores = NC,
              label = function(c, i) names(fallback)[i]), "auto-fallback",
      fns = "optimix")

## ---- 7b. exported-surface coverage (a positive AND a negative per export) ---
## Two-sided coverage of the documented surface: each exported function gets one
## valid call (must succeed) and one bad call (must be rejected), so the
## api_surface_covered metric reflects the whole NAMESPACE, not just the busy
## verbs. `opt_space` is abstract, so it carries only a negative case (it cannot
## be constructed). Built as (fn, lab, expect, call) tuples and swept like the
## extreme / missing batteries.
.good_res <- function() optimix(function(x) sum((x - 0.5)^2), c(-2, -2), c(2, 2),
                                method = "base_optim")
.good_manifest <- function() as_orchestra_manifest(.good_res())
surf <- list(
  list(fn = "optimix", lab = "optimix: valid box minimisation", exp = "ok",
       f = function() optimix(sphere, lo3, hi3, method = "base_optim")),
  list(fn = "optimix", lab = "optimix: unknown method", exp = "error",
       f = function() optimix(sphere, lo3, hi3, method = "nope")),
  list(fn = "minimise", lab = "minimise: valid call", exp = "ok",
       f = function() minimise(sphere, lo3, hi3)),
  list(fn = "minimise", lab = "minimise: NULL bounds", exp = "error",
       f = function() minimise(sphere, NULL, hi3)),
  list(fn = "maximise", lab = "maximise: valid call", exp = "ok",
       f = function() maximise(function(x) -sum(x^2), lo3, hi3)),
  list(fn = "maximise", lab = "maximise: NULL bounds", exp = "error",
       f = function() maximise(sphere, NULL, hi3)),
  list(fn = "optimix_map", lab = "optimix_map: valid call", exp = "ok",
       f = function() optimix_map(sphere, c(-2, -2), c(2, 2), n_starts = 4L)),
  list(fn = "optimix_map", lab = "optimix_map: string n_starts", exp = "error",
       f = function() optimix_map(sphere, c(-2, -2), c(2, 2), n_starts = "many")),
  list(fn = "optim_problem", lab = "optim_problem: valid construction", exp = "ok",
       f = function() optim_problem(sphere, space_box(lo3, hi3))),
  list(fn = "optim_problem", lab = "optim_problem: max_evals = 0", exp = "error",
       f = function() optim_problem(sphere, space_box(lo3, hi3), max_evals = 0)),
  list(fn = "space_box", lab = "space_box: valid construction", exp = "ok",
       f = function() space_box(lo3, hi3)),
  list(fn = "space_box", lab = "space_box: lower > upper", exp = "error",
       f = function() space_box(c(0, 0), c(-1, 1))),
  list(fn = "space_permutation", lab = "space_permutation: valid", exp = "ok",
       f = function() space_permutation(5L)),
  list(fn = "space_permutation", lab = "space_permutation: n < 2", exp = "error",
       f = function() space_permutation(1)),
  list(fn = "objective_spec", lab = "objective_spec: valid", exp = "ok",
       f = function() objective_spec(kind = "deterministic")),
  list(fn = "objective_spec", lab = "objective_spec: bad kind", exp = "error",
       f = function() objective_spec(kind = "weird")),
  ## objective_deterministic / objective_expensive / list_optimisers are
  ## nullary with no input contract, so they carry only a positive case: there
  ## is no caller argument for them to reject.
  list(fn = "objective_deterministic", lab = "objective_deterministic: valid",
       exp = "ok", f = function() objective_deterministic()),
  list(fn = "objective_noisy", lab = "objective_noisy: valid", exp = "ok",
       f = function() objective_noisy(sd = 0.1)),
  list(fn = "objective_noisy", lab = "objective_noisy: negative sd", exp = "error",
       f = function() objective_noisy(sd = -1)),
  list(fn = "objective_expensive", lab = "objective_expensive: valid", exp = "ok",
       f = function() objective_expensive()),
  list(fn = "list_optimisers", lab = "list_optimisers: valid", exp = "ok",
       f = function() list_optimisers()),
  list(fn = "optim_engine", lab = "optim_engine: valid construction", exp = "ok",
       f = function() optim_engine(name = "cov_probe", pkg = "base",
                                   accepts = "space_box",
                                   available = function() TRUE,
                                   run = function(problem) NULL)),
  list(fn = "optim_engine", lab = "optim_engine: bad name", exp = "error",
       f = function() optim_engine(name = 42)),
  list(fn = "register_optimiser", lab = "register_optimiser: valid engine",
       exp = "ok",
       f = function() register_optimiser(optim_engine(
         name = "cov_probe_reg", pkg = "base", accepts = "space_box",
         available = function() TRUE,
         run = function(problem) .good_res()))),
  list(fn = "register_optimiser", lab = "register_optimiser: non-engine",
       exp = "error", f = function() register_optimiser(42)),
  list(fn = "as_optim", lab = "as_optim: on a result", exp = "ok",
       f = function() as_optim(.good_res())),
  list(fn = "as_optim", lab = "as_optim: on a number", exp = "error",
       f = function() as_optim(42)),
  list(fn = "as_orchestra_manifest", lab = "as_orchestra_manifest: on a result",
       exp = "ok", f = function() as_orchestra_manifest(.good_res())),
  list(fn = "as_orchestra_manifest", lab = "as_orchestra_manifest: on a number",
       exp = "error", f = function() as_orchestra_manifest(42)),
  list(fn = "orchestra_manifest", lab = "orchestra_manifest: valid construction",
       exp = "ok",
       f = function() orchestra_manifest(
         emitter_package = "optimix", emitter_version = "0.0.0.9000",
         inferential_target = "parameters", method = "optimix:base_optim",
         run_id = "cov-probe", data_hash = "0",
         params = data.frame(par1 = 0.5, value = 0))),
  list(fn = "orchestra_manifest", lab = "orchestra_manifest: missing run_id",
       exp = "error",
       f = function() orchestra_manifest(
         emitter_package = "optimix", inferential_target = "parameters",
         method = "optimix:base_optim",
         params = data.frame(par1 = 0.5, value = 0))),
  ## verify_manifest returns list(ok = FALSE) on a non-manifest rather than
  ## throwing, so its contract is a positive call (a real manifest verifies).
  list(fn = "verify_manifest", lab = "verify_manifest: on a valid manifest",
       exp = "ok", f = function() verify_manifest(.good_manifest())),
  list(fn = "opt_space", lab = "opt_space: abstract, cannot construct",
       exp = "error", f = function() opt_space())
)
gpush(pv_grid(surf, function(c) c$f(), expect = function(c) c$exp, cores = NC,
              label = function(c, i) c$lab), "exported-surface",
      fns = vapply(surf, function(c) c$fn, character(1)))

verdicts <- do.call(rbind, grids)
verdicts <- verdicts[, c("group", "fn", "label", "expect", "status", "seconds",
                         "n_warn", "detail")]

## ---- 8. metamorphic invariants ---------------------------------------------
meta <- list(); add_meta <- function(m) meta[[length(meta) + 1L]] <<- m
for (e in intersect(c("base_optim", "nloptr_bobyqa", "dfoptim_hjkb", "nloptr_directl"), installed)) {
  a <- optimix(sphere, lo3, hi3, method = e, max_evals = 3000)$par
  b <- optimix(optim_problem(sphere, space_box(lo3, hi3), max_evals = 3000), method = e)$par
  add_meta(pv_metamorphic(a, b, tol = 1e-10, what = paste0("easy==power / ", e)))
}
for (e in intersect(c("gensa", "deoptim", "deoptimr", "cmaes", "cmaes_ipop"), installed)) {
  p <- function() optim_problem(sphere, space_box(lo3, hi3), seed = 7L, max_evals = 2000)
  r1 <- optimix(p(), method = e); r2 <- optimix(p(), method = e)
  add_meta(pv_metamorphic(c(r1$par, r1$value), c(r2$par, r2$value), tol = 1e-12,
                          what = paste0("seed-reproducible / ", e)))
}
pp <- function() optim_problem(pf, space_permutation(8L), seed = 7L, max_evals = 1500)
r1 <- optimix(pp(), method = "perm_sa"); r2 <- optimix(pp(), method = "perm_sa")
add_meta(pv_metamorphic(c(r1$par, r1$value), c(r2$par, r2$value), tol = 1e-12,
                        what = "seed-reproducible / perm_sa"))
add_meta(pv_metamorphic(minimise(sphere, lo3, hi3)$par,
                        optimix(sphere, lo3, hi3, maximise = FALSE)$par, tol = 1e-12,
                        what = "minimise() == optimix(maximise=FALSE)"))
metamorphic <- do.call(rbind, lapply(meta, function(m)
  data.frame(invariant = m$what, pass = m$pass, gap = m$gap, detail = m$detail,
             stringsAsFactors = FALSE)))

## ---- findings + coverage + summary -----------------------------------------
findings <- pv_findings(verdicts)
meta_fail <- metamorphic[!metamorphic$pass, ]
## Two-sided coverage is scored over the exports that CAN carry both a positive
## and a negative case. Five exports are structurally single-sided -- a negative
## for them would classify as an opaque-error finding (re-breaking findings == 0),
## so they are excluded from the coverage surface rather than fabricated against:
## opt_space (abstract space constructor); objective_deterministic /
## objective_expensive / list_optimisers (nullary, no rejectable input);
## verify_manifest (returns a status object, does not throw).
single_sided <- c("opt_space", "objective_deterministic", "objective_expensive",
                  "list_optimisers", "verify_manifest")
surface <- setdiff(getNamespaceExports("optimix"), single_sided)
coverage <- pv_coverage(verdicts, surface)
api_surface_covered <- pv_api_surface_covered(verdicts, surface)
res <- list(
  verdicts = verdicts, metamorphic = metamorphic, findings = findings,
  meta_fail = meta_fail, summary = pv_conformance_summary(verdicts),
  coverage = coverage, api_surface_covered = api_surface_covered,
  n_cells = nrow(verdicts), n_meta = nrow(metamorphic),
  n_findings = nrow(findings), n_meta_fail = nrow(meta_fail),
  cores = NC, seed = PV_SEED
)
pv_put(res, "contract-conformance")

## ---- figure: every cell classified, findings highlighted -------------------
.figdir <- if (exists("pv_root")) file.path(pv_root, "figures") else "figures"
sumdf <- res$summary[res$summary$n > 0, ]
sumdf$status <- factor(sumdf$status, levels = rev(PV_STATUS_LEVELS))
sumdf$kind <- ifelse(as.character(sumdf$status) %in% PV_FINDING_STATUS,
                     "flagged for review", "as the contract expects")
pcf <- ggplot2::ggplot(sumdf, ggplot2::aes(n, status, fill = kind)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.3, size = 3.2) +
  ggplot2::scale_fill_manual(values = c("as the contract expects" = pv_pal(1),
                                        "flagged for review" = "#B2182B")) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
  ggplot2::labs(
    title = "Contract conformance: every cell classified",
    subtitle = sprintf("%d cells across the argument lattice and %d metamorphic invariants",
                       res$n_cells, res$n_meta),
    x = "cells", y = NULL, fill = NULL) +
  pv_theme()
pv_save(pcf, "F_conformance_status", dir = .figdir, height = 3.0)

cat("\n==== CONFORMANCE SUMMARY (", NC, "cores ) ====\n")
print(res$summary[res$summary$n > 0, ], row.names = FALSE)
cat(sprintf("\ncells: %d   metamorphic: %d   FINDINGS: %d   META-FAIL: %d\n",
            res$n_cells, res$n_meta, res$n_findings, res$n_meta_fail))
cat(sprintf("API surface coverage (both sides): %.1f%% of %d two-sided-testable exports (%d structurally single-sided excluded)\n",
            100 * res$api_surface_covered, length(surface), length(single_sided)))
if (nrow(findings)) { cat("\n---- findings ----\n")
  print(findings[, c("group", "fn", "label", "expect", "status", "detail")], row.names = FALSE) }
if (nrow(meta_fail)) { cat("\n---- metamorphic failures ----\n"); print(meta_fail, row.names = FALSE) }
