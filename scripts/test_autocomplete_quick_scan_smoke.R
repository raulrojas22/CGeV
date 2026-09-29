#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(later)
  library(stringr)
  library(purrr)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
workspace <- if (length(script_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", script_arg[[1]]))))
} else {
  normalizePath(".")
}
source(file.path(workspace, "R", "utils.R"))
source(file.path(workspace, "R", "server_autocomplete_domain.R"))

assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) {
    stop(msg, call. = FALSE)
  }
}

make_state_cell <- function(initial = NULL) {
  value <- initial
  function(value_new = NULL) {
    if (missing(value_new)) {
      return(value)
    }
    value <<- value_new
    invisible(value)
  }
}

make_session_stub <- function() {
  session <- new.env(parent = emptyenv())
  session$isClosed <- function() FALSE
  session$onSessionEnded <- function(callback) function() NULL
  session
}
session_stub <- make_session_stub()
session_stub$messages <- list()
session_stub$sendCustomMessage <- function(type, payload) {
  session_stub$messages[[length(session_stub$messages) + 1L]] <<- list(type = type, payload = payload)
  invisible(NULL)
}

domain <- init_autocomplete_domain(
  geneAutocompleteCache_rv = make_state_cell(list()),
  quickGeneAutocompleteCache_rv = make_state_cell(list()),
  globalGeneSuggestionSources_rv = make_state_cell(list()),
  session = session_stub
)

tmp_gff <- tempfile(fileext = ".gff3")
writeLines(
  c(
    "##gff-version 3",
    paste0("chr1\tsrc\tgene\t1\t100\t.\t+\t.\tID=gene-1;gene_name=GENE_A;Name=GENE_A"),
    paste0("chr1\tsrc\tgene\t200\t300\t.\t+\t.\tID=gene-2;Name=GENE_B"),
    rep("# filler", 5000L),
    paste0("chr1\tsrc\tgene\t400\t500\t.\t+\t.\tID=gene-3;gene_name=GENE_C;Name=GENE_C")
  ),
  tmp_gff,
  useBytes = TRUE
)
on.exit(unlink(tmp_gff, force = TRUE), add = TRUE)

sync_suggestions <- domain$extract_quick_gene_suggestions(
  tmp_gff,
  max_suggestions = 50L,
  max_lines = 20000L,
  max_seconds = 2,
  chunk_lines = 300L
)

state <- domain$init_quick_gene_scan_state(
  tmp_gff,
  max_suggestions = 50L,
  max_lines = 20000L,
  max_seconds = 2,
  chunk_lines = 300L
)
on.exit(domain$close_quick_gene_scan_state(state), add = TRUE)
repeat {
  step <- domain$step_quick_gene_scan_state(state)
  if (isTRUE(step$done)) {
    async_like <- step$suggestions
    break
  }
}

assert_true(identical(sort(sync_suggestions), sort(async_like)),
            "Incremental quick-scan path diverged from the synchronous quick-scan output.")

session_stub$messages <- list()
domain$update_gene_autocomplete(
  "gene_name",
  tmp_gff,
  max_total = 50L,
  allow_build = FALSE,
  allow_quick_scan = TRUE,
  allow_disk_index = FALSE
)

deadline <- Sys.time() + 2
async_update_choices <- character(0)
repeat {
  later::run_now(0.05)
  gene_msgs <- Filter(function(msg) {
    identical(msg$type, "update_gene_autocomplete") &&
      identical(msg$payload$input_id, "gene_name")
  }, session_stub$messages)
  if (length(gene_msgs) > 0) {
    latest <- gene_msgs[[length(gene_msgs)]]
    async_update_choices <- as.character(latest$payload$choices %||% character(0))
  }
  if (length(async_update_choices) > 0 || Sys.time() > deadline) break
  Sys.sleep(0.01)
}

assert_true("GENE_A" %in% async_update_choices,
            "Nonblocking autocomplete update did not publish quick-scan suggestions.")

tmp_gff_2 <- tempfile(fileext = ".gff3")
writeLines(
  c(
    "##gff-version 3",
    paste0("chr1\tsrc\tgene\t1\t100\t.\t+\t.\tID=gene-4;gene_name=GENE_SHARED;Name=GENE_SHARED"),
    paste0("chr1\tsrc\tgene\t200\t300\t.\t+\t.\tID=gene-5;gene_name=GENE_D;Name=GENE_D")
  ),
  tmp_gff_2,
  useBytes = TRUE
)
on.exit(unlink(tmp_gff_2, force = TRUE), add = TRUE)

staged_cache <- list()
staged_cache[[gff_cache_key(tmp_gff)]] <- c("GENE_A", "GENE_B", "GENE_SHARED")
staged_cache[[gff_cache_key(tmp_gff_2)]] <- c("GENE_SHARED", "GENE_D")
staged_session <- make_session_stub()
staged_session$messages <- list()
staged_session$sendCustomMessage <- function(type, payload) {
  staged_session$messages[[length(staged_session$messages) + 1L]] <<- list(type = type, payload = payload)
  invisible(NULL)
}
staged_domain <- init_autocomplete_domain(
  geneAutocompleteCache_rv = make_state_cell(staged_cache),
  quickGeneAutocompleteCache_rv = make_state_cell(list()),
  globalGeneSuggestionSources_rv = make_state_cell(list()),
  session = staged_session
)

staged_union_result <- staged_domain$update_gene_autocomplete(
  "gene_name",
  c(tmp_gff, tmp_gff_2),
  max_total = 50L,
  allow_build = FALSE,
  allow_quick_scan = FALSE,
  allow_disk_index = FALSE,
  min_shared_organisms = 1L
)
staged_gene_msgs <- Filter(function(msg) {
  identical(msg$type, "update_gene_autocomplete") &&
    identical(msg$payload$input_id, "gene_name")
}, staged_session$messages)
assert_true(length(staged_gene_msgs) >= 2L,
            "Autocomplete should publish staged updates as each cached organism contributes suggestions.")
assert_true(all(c("GENE_A", "GENE_D", "GENE_SHARED") %in% as.character(staged_gene_msgs[[length(staged_gene_msgs)]]$payload$choices)),
            "Final staged autocomplete should include suggestions from all cached organisms.")
assert_true(isTRUE(staged_union_result$complete) && length(staged_union_result$unresolved_paths) == 0L,
            "Cached autocomplete paths should be reported as completely resolved.")

staged_session$messages <- list()
staged_shared_result <- staged_domain$update_gene_autocomplete(
  "gene_name",
  c(tmp_gff, tmp_gff_2),
  max_total = 50L,
  allow_build = FALSE,
  allow_quick_scan = FALSE,
  allow_disk_index = FALSE,
  min_shared_organisms = 2L
)
shared_gene_msgs <- Filter(function(msg) {
  identical(msg$type, "update_gene_autocomplete") &&
    identical(msg$payload$input_id, "gene_name")
}, staged_session$messages)
latest_shared <- as.character(shared_gene_msgs[[length(shared_gene_msgs)]]$payload$choices)
assert_true(length(shared_gene_msgs) == 1L,
            "Cross-species shared autocomplete should publish only once after aggregating all organisms.")
assert_true(identical(latest_shared, "GENE_SHARED"),
            "Cross-species autocomplete should only publish the same candidate when it appears in at least two organisms.")
assert_true(isTRUE(staged_shared_result$complete),
            "Cross-species autocomplete should report cached paths as completely resolved.")

missing_session <- make_session_stub()
missing_session$messages <- list()
missing_session$sendCustomMessage <- function(type, payload) {
  missing_session$messages[[length(missing_session$messages) + 1L]] <<- list(type = type, payload = payload)
  invisible(NULL)
}
missing_domain <- init_autocomplete_domain(
  geneAutocompleteCache_rv = make_state_cell(list()),
  quickGeneAutocompleteCache_rv = make_state_cell(list()),
  globalGeneSuggestionSources_rv = make_state_cell(list()),
  session = missing_session
)
missing_result <- missing_domain$update_gene_autocomplete(
  "gene_name",
  tmp_gff,
  max_total = 50L,
  allow_build = FALSE,
  allow_quick_scan = FALSE,
  allow_disk_index = FALSE
)
assert_true(!isTRUE(missing_result$complete) &&
              identical(as.character(missing_result$unresolved_paths), as.character(tmp_gff)),
            "A skipped cache miss must remain unresolved so organism preparation can retry it once.")

shared_cap_paths <- vapply(seq_len(13L), function(i) {
  path <- tempfile(fileext = ".gff3")
  gene <- if (i %in% c(1L, 13L)) "GENE_EDGE_SHARED" else sprintf("GENE_ONLY_%02d", i)
  writeLines(
    c(
      "##gff-version 3",
      sprintf("chr1\tsrc\tgene\t1\t100\t.\t+\t.\tID=gene-%02d;gene_name=%s;Name=%s", i, gene, gene)
    ),
    path,
    useBytes = TRUE
  )
  path
}, character(1))
on.exit(unlink(shared_cap_paths, force = TRUE), add = TRUE)
shared_cap_cache <- list()
for (i in seq_along(shared_cap_paths)) {
  shared_cap_cache[[gff_cache_key(shared_cap_paths[[i]])]] <-
    if (i %in% c(1L, 13L)) "GENE_EDGE_SHARED" else sprintf("GENE_ONLY_%02d", i)
}
shared_cap_session <- make_session_stub()
shared_cap_session$messages <- list()
shared_cap_session$sendCustomMessage <- function(type, payload) {
  shared_cap_session$messages[[length(shared_cap_session$messages) + 1L]] <<- list(type = type, payload = payload)
  invisible(NULL)
}
shared_cap_domain <- init_autocomplete_domain(
  geneAutocompleteCache_rv = make_state_cell(shared_cap_cache),
  quickGeneAutocompleteCache_rv = make_state_cell(list()),
  globalGeneSuggestionSources_rv = make_state_cell(list()),
  session = shared_cap_session
)
shared_cap_result <- shared_cap_domain$update_gene_autocomplete(
  "gene_name",
  shared_cap_paths,
  max_total = 50L,
  allow_build = FALSE,
  allow_quick_scan = FALSE,
  allow_disk_index = FALSE,
  min_shared_organisms = 2L
)
assert_true("GENE_EDGE_SHARED" %in% as.character(shared_cap_result$choices),
            "Shared autocomplete must include the thirteenth organism instead of silently capping at twelve.")

tmp_gff_3 <- tempfile(fileext = ".gff3")
writeLines(
  c(
    "##gff-version 3",
    paste0("chr1\tsrc\tgene\t1\t100\t.\t+\t.\tID=gene-6;gene_name=GENE_SHARED;Name=GENE_SHARED"),
    paste0("chr1\tsrc\tgene\t200\t300\t.\t+\t.\tID=gene-7;gene_name=GENE_E;Name=GENE_E")
  ),
  tmp_gff_3,
  useBytes = TRUE
)
on.exit(unlink(tmp_gff_3, force = TRUE), add = TRUE)

quick_shared_session <- make_session_stub()
quick_shared_session$messages <- list()
quick_shared_session$sendCustomMessage <- function(type, payload) {
  quick_shared_session$messages[[length(quick_shared_session$messages) + 1L]] <<- list(type = type, payload = payload)
  invisible(NULL)
}
quick_shared_domain <- init_autocomplete_domain(
  geneAutocompleteCache_rv = make_state_cell(list()),
  quickGeneAutocompleteCache_rv = make_state_cell(list()),
  globalGeneSuggestionSources_rv = make_state_cell(list()),
  session = quick_shared_session
)

quick_shared_domain$update_gene_autocomplete(
  "gene_name",
  c(tmp_gff, tmp_gff_2, tmp_gff_3),
  max_total = 50L,
  allow_build = FALSE,
  allow_quick_scan = TRUE,
  allow_disk_index = FALSE,
  min_shared_organisms = 2L
)

deadline <- Sys.time() + 3
quick_shared_choices <- character(0)
repeat {
  later::run_now(0.05)
  shared_msgs <- Filter(function(msg) {
    identical(msg$type, "update_gene_autocomplete") &&
      identical(msg$payload$input_id, "gene_name")
  }, quick_shared_session$messages)
  if (length(shared_msgs) > 0) {
    latest <- shared_msgs[[length(shared_msgs)]]
    quick_shared_choices <- as.character(latest$payload$choices %||% character(0))
  }
  if ("GENE_SHARED" %in% quick_shared_choices || Sys.time() > deadline) break
  Sys.sleep(0.01)
}

assert_true("GENE_SHARED" %in% quick_shared_choices,
            "Cross-species quick autocomplete should publish genes shared by at least two organisms even when another organism has no match.")

cat("autocomplete-quick-scan-smoke-ok\n")
