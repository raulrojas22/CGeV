#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(stringr)
  library(purrr)
})

root <- normalizePath(Sys.getenv("CGV_TEST_ROOT", "."), winslash = "/", mustWork = TRUE)
source(file.path(root, "R", "utils.R"))
source(file.path(root, "R", "server_autocomplete_domain.R"))

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

make_state_cell <- function(initial = NULL) {
  value <- initial
  function(next_value = NULL) {
    if (missing(next_value)) return(value)
    value <<- next_value
    invisible(value)
  }
}

session_stub <- new.env(parent = emptyenv())
session_stub$sendCustomMessage <- function(type, payload) invisible(NULL)

domain <- init_autocomplete_domain(
  geneAutocompleteCache_rv = make_state_cell(list()),
  quickGeneAutocompleteCache_rv = make_state_cell(list()),
  globalGeneSuggestionSources_rv = make_state_cell(list()),
  session = session_stub
)

registry <- read.delim(
  file.path(root, "annotations", "registry.tsv"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
labels <- c(
  "Saccharomyces cerevisiae",
  "Drosophila melanogaster",
  "Oryza sativa ssp. japonica",
  "Homo sapiens",
  "Caenorhabditis elegans"
)
available <- labels[labels %in% registry$label]
assert_true(length(available) >= 3L, "Expected at least three reference annotations in the registry.")

for (label in available) {
  path <- file.path(root, registry$annotation[match(label, registry$label)])
  idx <- load_gff_index_from_disk(path, cache_kind = "gene_light", base_dir = root)
  assert_true(is.list(idx) && is.data.frame(idx$genes_df), paste("Missing gene_light index for", label))

  attrs <- as.character(idx$genes_df$attributes %||% rep("", nrow(idx$genes_df)))
  legacy <- sanitize_autocomplete_choices(extract_partial_gene_display_names(attrs), max_total = 20000L)
  sidecar <- ensure_gff_autocomplete_cache(path, idx, base_dir = root)

  assert_true(is.list(sidecar), paste("Sidecar generation failed for", label))
  assert_true(identical(legacy, sidecar$display), paste("Suggestion parity failed for", label))
  assert_true(identical(as.character(normalize_partial_gene_query(legacy)), sidecar$keys),
              paste("Normalized-key parity failed for", label))

  slim <- slim_gff_gene_light_index(idx)
  assert_true(
    identical(names(slim), intersect(c("genes_df", "gene_rows", "norm_map", "comp_map", "comp_order"), names(idx))),
    paste("Unexpected slim index shape for", label)
  )
  assert_true(all(c("genes_df", "gene_rows", "norm_map", "comp_map") %in% names(slim)),
              paste("Slim index lacks required fields for", label))
}

fixture_label <- available[[1L]]
fixture_path <- file.path(root, registry$annotation[match(fixture_label, registry$label)])
fixture_idx <- load_gff_index_from_disk(fixture_path, cache_kind = "gene_light", base_dir = root)
tmp_cache_root <- tempfile("autocomplete-sidecar-cache-")
dir.create(tmp_cache_root, recursive = TRUE)
on.exit(unlink(tmp_cache_root, recursive = TRUE, force = TRUE), add = TRUE)
old_cache_dir <- Sys.getenv("APP_ANNOTATION_DISK_CACHE_DIR", unset = "")
Sys.setenv(APP_ANNOTATION_DISK_CACHE_DIR = tmp_cache_root)
on.exit(Sys.setenv(APP_ANNOTATION_DISK_CACHE_DIR = old_cache_dir), add = TRUE)
rm(list = ls(.gff_autocomplete_cache_validation, all.names = TRUE),
   envir = .gff_autocomplete_cache_validation)

fixture_sidecar <- ensure_gff_autocomplete_cache(fixture_path, fixture_idx, base_dir = root)
assert_true(is.list(fixture_sidecar), "Temporary sidecar generation failed.")
fixture_sidecar_path <- get_gff_disk_index_path(fixture_path, cache_kind = "autocomplete", base_dir = root)
writeBin(charToRaw("corrupt"), fixture_sidecar_path)
rm(list = ls(.gff_autocomplete_cache_validation, all.names = TRUE),
   envir = .gff_autocomplete_cache_validation)
assert_true(is.null(load_gff_autocomplete_cache(fixture_path, base_dir = root)),
            "A corrupt sidecar must be rejected.")

obsolete <- fixture_sidecar
obsolete$version <- obsolete$version + 1L
assert_true(atomic_save_rds(obsolete, fixture_sidecar_path), "Could not write obsolete sidecar fixture.")
rm(list = ls(.gff_autocomplete_cache_validation, all.names = TRUE),
   envir = .gff_autocomplete_cache_validation)
assert_true(is.null(load_gff_autocomplete_cache(fixture_path, base_dir = root)),
            "An obsolete sidecar version must be rejected.")

fat_fixture <- fixture_idx
fat_fixture$norm_list <- list("unused")
fat_fixture$comp_list <- list("unused")
fat_fixture$all_norm_tokens <- "unused"
assert_true(isTRUE(save_gff_index_to_disk(fixture_path, fat_fixture, cache_kind = "gene_light", base_dir = root)),
            "Could not create legacy gene_light fixture.")
assert_true(isTRUE(slim_gff_gene_light_index_file(fixture_path, base_dir = root)),
            "Legacy gene_light migration failed.")
slim_fixture <- load_gff_index_from_disk(fixture_path, cache_kind = "gene_light", base_dir = root)
assert_true(
  !any(c("norm_list", "comp_list", "all_norm_tokens") %in% names(slim_fixture)),
  "Legacy-only fields survived the slim migration."
)

Sys.setenv(APP_ANNOTATION_DISK_CACHE_DIR = old_cache_dir)
rm(list = ls(.gff_autocomplete_cache_validation, all.names = TRUE),
   envir = .gff_autocomplete_cache_validation)

cross_labels <- c("Homo sapiens", "Canis lupus familiaris", "Equus caballus", "Pan troglodytes")
cross_labels <- cross_labels[cross_labels %in% registry$label]
assert_true(length(cross_labels) == 4L, "Cross-species parity fixtures are incomplete.")

entries <- lapply(cross_labels, function(label) {
  path <- file.path(root, registry$annotation[match(label, registry$label)])
  idx <- load_gff_index_from_disk(path, cache_kind = "gene_light", base_dir = root)
  ensure_gff_autocomplete_cache(path, idx, base_dir = root)
})
suggestions <- lapply(entries, `[[`, "display")
keys <- lapply(entries, `[[`, "keys")

legacy_aggregate <- function(values, min_shared = 2L, max_total = 20000L) {
  rows <- lapply(seq_along(values), function(i) {
    vals <- sanitize_autocomplete_choices(values[[i]], max_total = max_total)
    key <- vapply(vals, normalize_partial_gene_query, character(1))
    keep <- !is.na(key) & nzchar(key)
    data.frame(gene_name = vals[keep], gene_key = key[keep], path_idx = i, stringsAsFactors = FALSE)
  })
  all_rows <- do.call(rbind, rows)
  grouped <- lapply(split(all_rows, all_rows$gene_key), function(df) {
    count <- length(unique(df$path_idx))
    if (count < min_shared) return(NULL)
    name_tab <- sort(table(df$gene_name), decreasing = TRUE)
    data.frame(gene_name = names(name_tab)[[1L]], source_count = count, stringsAsFactors = FALSE)
  })
  grouped <- Filter(Negate(is.null), grouped)
  out <- do.call(rbind, grouped)
  out <- out[order(-out$source_count, nchar(out$gene_name), tolower(out$gene_name)), , drop = FALSE]
  sanitize_autocomplete_choices(out$gene_name, max_total = max_total)
}

expected <- legacy_aggregate(suggestions)
actual <- domain$aggregate_shared_gene_suggestions(
  suggestions,
  keys_by_path = keys,
  min_shared_organisms = 2L,
  max_total = 20000L
)
assert_true(identical(expected, actual), "Vectorized Cross-Species aggregation changed visible output.")

cat(sprintf(
  "autocomplete-sidecar-ok organisms=%d cross_shared=%d\n",
  length(available),
  length(actual)
))
