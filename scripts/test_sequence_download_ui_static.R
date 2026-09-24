#!/usr/bin/env Rscript

server_txt <- paste(readLines("server.R", warn = FALSE), collapse = "\n")
lifecycle_txt <- paste(readLines(file.path("R", "server_plot_lifecycle_domain.R"), warn = FALSE), collapse = "\n")
utils_txt <- paste(readLines(file.path("R", "utils.R"), warn = FALSE), collapse = "\n")

expect_pattern <- function(txt, pattern, label) {
  if (!grepl(pattern, txt, perl = TRUE)) {
    stop(sprintf("Missing expected sequence download UI behavior: %s", label), call. = FALSE)
  }
}

expect_pattern(
  server_txt,
  'build_sequence_download_controls <- function\\(context = c\\("homo", "ortho"\\), id_chr, default_type = "transcript"',
  "shared dropdown download UI helper"
)
expect_pattern(
  server_txt,
  'class = "btn btn-download sequence-download-button dropdown-toggle"[\\s\\S]*`data-bs-toggle` = "dropdown"',
  "single visible Bootstrap 5 FASTA dropdown button"
)
expect_pattern(
  server_txt,
  'paste0\\(prefix, "_", id_chr, "_", value\\)',
  "dropdown options use dedicated lazy download endpoints"
)
if (grepl('class = "form-control sequence-download-type"', server_txt, fixed = TRUE)) {
  stop("The separate visible sequence type selector should not be rendered", call. = FALSE)
}
expect_pattern(
  server_txt,
  'build_sequence_download_controls\\("homo", id_chr, default_type = "gene"\\)',
  "homologous canonical cards default to gene download"
)
expect_pattern(
  server_txt,
  'build_sequence_download_controls\\("homo", id_chr, default_type = "transcript", compact = TRUE\\)',
  "homologous transcript headers default to transcript download"
)
expect_pattern(
  server_txt,
  'build_sequence_download_controls\\("ortho", id_chr, default_type = "gene"\\)',
  "orthologous canonical cards default to gene download"
)
expect_pattern(
  server_txt,
  'build_sequence_download_controls\\("ortho", id_chr, default_type = "transcript", compact = TRUE\\)',
  "orthologous transcript headers default to transcript download"
)
expect_pattern(
  lifecycle_txt,
  'for \\(sequence_type in c\\("gene", "transcript", "cds", "cds_segments", "introns"\\)\\) local\\(\\{[\\s\\S]*selected_type_local <- sequence_type[\\s\\S]*output\\[\\[paste0\\("download_homo_", id_local, "_", selected_type_local\\)\\]\\]',
  "homologous menu options bind fixed lazy download handlers"
)
expect_pattern(
  lifecycle_txt,
  'output\\[\\[paste0\\("download_ortho_", id_local, "_", selected_type_local\\)\\]\\]',
  "orthologous menu options bind fixed lazy download handlers"
)
expect_pattern(
  utils_txt,
  'build_selected_sequence_fasta_content <- function\\(sequence_type = "transcript"',
  "selected FASTA builder exists"
)
expect_pattern(
  server_txt,
  'cds_segments = "CDS — individual segments"[\\s\\S]*introns = "Introns — individual sequences"',
  "segmented CDS and intron download choices are explicit"
)
expect_pattern(
  utils_txt,
  'build_segmented_sequence_fasta <- function',
  "multi-record segmented FASTA builder exists"
)

# Phase 4B.4B: the homologous download observer must follow the DOM/hydrated
# card lifecycle (same derivation as the footer observer), not all active ids.
dl_start <- regexpr(
  'outputOptions\\(output, "download_ortho_summary_csv", suspendWhenHidden = FALSE\\)',
  server_txt
)
dl_end_matches <- gregexpr('activePlotIdsOrthologous\\(\\)', server_txt)[[1L]]
dl_end_matches <- dl_end_matches[dl_end_matches > dl_start[[1L]]]
dl_end <- if (length(dl_end_matches)) dl_end_matches[[1L]] else -1L
if (dl_start[[1]] < 0 || dl_end < 0) {
  stop("Could not locate the homologous download observer region", call. = FALSE)
}
homolog_download_observer <- substr(
  server_txt,
  dl_start[[1]] + attr(dl_start, "match.length"),
  dl_end - 1L
)
expect_pattern(
  homolog_download_observer,
  'homoInsertedCardIds\\(\\)',
  "homologous download observer follows admitted/DOM-present cards"
)
expect_pattern(
  homolog_download_observer,
  'homoHydratedIsoformIds\\(\\)',
  "homologous download observer follows hydrated isoform cards"
)
expect_pattern(
  homolog_download_observer,
  'get_active_homologous_copy_ids\\(ids_chr\\)',
  "homologous download observer keeps the copy-id lifecycle"
)
if (grepl('activePlotIdsHomologous\\(\\)', homolog_download_observer, perl = TRUE)) {
  stop(
    "Homologous download observer must not register handlers for every active transcript id",
    call. = FALSE
  )
}

cat("sequence-download-ui-static-ok\n")
