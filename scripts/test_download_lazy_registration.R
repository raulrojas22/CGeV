#!/usr/bin/env Rscript
# Phase 4B.4B: lazy download registration lifecycle test.
#
# Requires the preloaded human annotation/genome under CGV_DATA_ROOT.
# Validates that homologous download handlers follow the DOM/hydrated card
# lifecycle instead of being registered for every active transcript id.
#
# Usage:
#   CGV_DATA_ROOT=/path/to/data Rscript scripts/test_download_lazy_registration.R [server.R]

Sys.setenv(
  APP_PERF_TIMING = "0",
  APP_DEBUG_LOGS = "0",
  APP_FUTURE_MODE = "sequential",
  APP_GENE_PLOT_RENDERER_PREWARM = "0",
  CGV_DATA_ROOT = Sys.getenv("CGV_DATA_ROOT", getwd()),
  CGV_CACHE_DIR = Sys.getenv("CGV_CACHE_DIR", file.path(getwd(), "cache"))
)
source("global.R")
args <- commandArgs(trailingOnly = TRUE)
server_fun <- source(if (length(args)) args[[1]] else "server.R", local = TRUE)$value

assert <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

shiny::testServer(server_fun, {
  session$setInputs(
    navtabs = "homologous", homo_data_mode = "preloaded",
    homo_preloaded_species = "homo_sapiens_gcf_000001405_40_grch38_p14_genomic",
    homo_visual_mode = "compact", homo_sort_mode = "load",
    app_theme = "light", colorblind_mode = FALSE
  )
  for (i in 1:10) {
    later::run_now(0.1)
    session$flushReact()
  }
  cat("registry rows", nrow(preloadedRegistry()), "\n")

  session$setInputs(filter1 = "BRCA1", generate1 = 1)
  session$flushReact()
  for (i in 1:5) {
    later::run_now(0.05)
    session$flushReact()
  }

  active_ids <- as.character(activePlotIdsHomologous())
  inserted_ids <- as.character(homoInsertedCardIds())
  bound_ids <- as.character(homoDownloadOutputsBound())
  cat(
    "after search: active=", length(active_ids),
    " inserted=", paste(inserted_ids, collapse = ","),
    " bound=", length(bound_ids), "\n",
    sep = ""
  )

  assert(length(active_ids) > 300L, "expected many active BRCA1 transcript ids")
  assert(identical(inserted_ids, "1"), "only the canonical primary card should be admitted")
  assert(identical(bound_ids, "1"), "download handlers must be bound only for admitted cards")
  assert(!("2" %in% bound_ids), "hidden isoform must not have a download handler")

  iso_ids <- setdiff(active_ids, inserted_ids)
  assert(length(iso_ids) > 0L, "expected hidden isoform ids")
  iso_first <- iso_ids[[1L]]
  # Use the real UI toggle key so pending progressive batches stop after clear.
  process_isoform_expand_request(
    list(context = "homologous", ids = iso_first),
    toggle_key = "homologous::1"
  )
  for (i in 1:20) {
    later::run_now(0.2)
    session$flushReact()
  }

  hydrated_ids <- as.character(homoHydratedIsoformIds())
  bound_after_expand <- as.character(homoDownloadOutputsBound())
  cat(
    "after expand: hydrated=", length(hydrated_ids),
    " bound=", length(bound_after_expand), "\n",
    sep = ""
  )
  assert(iso_first %in% hydrated_ids, "requested isoform should be hydrated")
  assert(iso_first %in% bound_after_expand, "hydrated isoform must get a download handler")
  assert(
    length(bound_after_expand) < 40L,
    "bound download set must stay small after one expansion"
  )
  hidden_remaining <- setdiff(active_ids, c(inserted_ids, hydrated_ids))
  assert(
    !any(hidden_remaining %in% bound_after_expand),
    "non-hydrated isoforms must not have download handlers"
  )

  run_clear_selected_visualizations(active_panel = "homologous", include_other = FALSE)
  session$flushReact()
  for (i in 1:5) {
    later::run_now(0.05)
    session$flushReact()
  }
  bound_after_clear <- as.character(homoDownloadOutputsBound())
  hydrated_after_clear <- as.character(homoHydratedIsoformIds())
  footer_after_clear <- as.character(homoFooterOutputsBound())
  cat(
    "after clear: active=", length(as.character(activePlotIdsHomologous())),
    " inserted=", length(as.character(homoInsertedCardIds())),
    " hydrated=[", paste(hydrated_after_clear, collapse = ","), "]",
    " download_bound=[", paste(bound_after_clear, collapse = ","), "]",
    " footer_bound=[", paste(footer_after_clear, collapse = ","), "]\n",
    sep = ""
  )
  # Pre-existing clear-path note: clear_homologous_visualizations() removes the
  # card DOM but does not reset homoHydratedIsoformIds(), and the footer
  # observer already keeps hydrated ids bound after clear. The lazy download
  # observer intentionally mirrors that lifecycle: after clear the bound set is
  # a subset of the still-hydrated ids, and no hidden/non-hydrated id is bound.
  assert(
    all(bound_after_clear %in% unique(c(hydrated_after_clear))),
    "bound downloads after clear must be a subset of the hydrated lifecycle set"
  )
  assert(!("1" %in% bound_after_clear), "canonical card download must be unbound after clear")
  assert(
    !any(hidden_remaining %in% bound_after_clear),
    "hidden non-hydrated ids must never be bound after clear"
  )
  assert(
    setequal(bound_after_clear, footer_after_clear),
    "download and footer bound sets must follow the same lifecycle after clear"
  )
})

cat("download-lazy-registration-ok\n")
