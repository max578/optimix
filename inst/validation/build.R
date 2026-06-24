## Durable reference copy of the optimix validation-dossier build driver (the
## authored coverage-gate wiring). Preserved here because report/ is gitignored.
## The live driver runs from report/; this is the durable record.
##
## report/build.R -- full build driver for the optimix validation dossier.
##
## Usage (from the report directory):
##   Rscript build.R all       # benches -> gates -> PDFs + explorer (default)
##   Rscript build.R bench     # run the benchmark battery only (cache + figures)
##   Rscript build.R gate      # run the pre-render gates only
##   Rscript build.R render    # build PDFs + the HTML explorer only

args <- commandArgs(trailingOnly = TRUE)
what <- if (length(args)) args[[1]] else "all"

REPORT <- normalizePath(".")
PV_ENG <- path.expand("~/.claude/skills/pkg-validation/engine")
source(file.path(REPORT, "_setup.R"))
for (.f in c("assemble.R", "audit_docs.R", "build_explorer.R", "gates.R")) {
  source(file.path(PV_ENG, .f))
}

TITLE <- "optimix: a validation and benchmark study"

if (what %in% c("bench", "all")) {
  message("== running benchmark battery ==")
  source(file.path(REPORT, "bench", "run_all.R"), local = new.env())
}
if (what %in% c("gate", "all")) {
  message("== pre-render gates ==")
  pv_gates(REPORT)
}
if (what %in% c("render", "all")) {
  message("== building PDFs (one per recipe) ==")
  pv_build_pdfs(REPORT, title = TITLE)
  message("== building the HTML explorer ==")
  pv_build_explorer(REPORT, title = TITLE)
}
if (what %in% c("docset", "render", "all")) {
  ## The external-auditor document set: the comprehensive all-in-one PDF, the
  ## crisp introductory primer, the audit-plan PDF, and the AI-auditor dossier
  ## (markdown + evidence.json). Content-once: each is a view over the same cards.
  message("== building the all-in-one + crisp PDFs ==")
  pv_build_allinone(REPORT, "pdf", title = TITLE, depth = "comprehensive")
  pv_build_allinone(REPORT, "pdf", title = TITLE, depth = "crisp")
  message("== rendering the audit-plan PDF ==")
  pv_render_plan(file.path(REPORT, "audit_plan.md"),
                 file.path(REPORT, "pdf", "audit_plan.pdf"),
                 title = "optimix validation: computational audit plan")
  message("== building the AI-auditor dossier ==")
  # Two-sided conformance coverage is scored over the exports that CAN carry both
  # a positive and a negative case. Five exports are STRUCTURALLY single-sided and
  # are excluded from the coverage surface -- not fabricated against, since a
  # negative for a nullary / abstract / status function classifies as an
  # opaque-error finding (re-breaking findings == 0):
  #   opt_space            -- abstract space constructor (no standalone positive)
  #   objective_deterministic / objective_expensive / list_optimisers
  #                        -- nullary: no rejectable input
  #   verify_manifest      -- returns list(ok = FALSE) rather than throwing
  # Coverage is therefore reported over the 14 two-sided-testable exports.
  .single_sided <- c("opt_space", "objective_deterministic", "objective_expensive",
                     "list_optimisers", "verify_manifest")
  pv_build_ai_dossier(REPORT, title = TITLE,
                      surface = setdiff(getNamespaceExports("optimix"),
                                        .single_sided))
}
message("build.R: done (", what, ")")
