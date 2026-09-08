#!/usr/bin/env Rscript
# Compare the actual cached-search helpers, keeping datasets and ranking fixed.
# Run from the app root; this does not launch Shiny or measure card rendering.
suppressPackageStartupMessages(library(stringr))
source_root <- Sys.getenv("CGV_BENCH_SOURCE_ROOT", ".")
source(file.path(source_root, "R", "utils.R"))
source(file.path(source_root, "R", "server_autocomplete_domain.R"))

cell <- function(initial) {
    value <- initial
    function(next_value) {
        if (missing(next_value)) return(value)
        value <<- next_value
    }
}
registry <- read.delim("annotations/registry.tsv", stringsAsFactors = FALSE)
labels <- c("Homo sapiens", "Mus musculus", "Arabidopsis thaliana", "Oryza sativa ssp. japonica")
stopifnot(all(labels %in% registry$label))
paths <- registry$annotation[match(labels, registry$label)]
entries <- lapply(paths, load_gff_autocomplete_cache)
stopifnot(all(vapply(entries, is.list, logical(1))))
choices <- lapply(entries, `[[`, "display")
cache <- setNames(choices, vapply(paths, gff_cache_key, character(1)))
cat(sprintf("R=%s platform=%s repetitions=3\n", getRversion(), R.version$platform))
for (i in seq_along(paths)) cat(sprintf("%s names=%d\n", labels[i], length(choices[[i]])))

wanted <- c("find_partial_gene_suggestions_from_autocomplete_cache",
            "has_fast_exact_gene_match", "count_fast_exact_gene_matches")
make_search <- function(legacy = FALSE) {
    env <- new.env(parent = globalenv())
    env$geneAutocompleteCache <- cell(cache)
    env$domain <- init_autocomplete_domain(env$geneAutocompleteCache, cell(list()), cell(list()), list())
    env$autocomplete_keys_for_choices <- env$domain$autocomplete_keys_for_choices
    if (legacy) env$autocomplete_keys_for_choices <- function(path, choices, cache_key = NULL) {
        as.character(vapply(choices, normalize_partial_gene_query, character(1)))
    }
    walk <- function(expr) {
        if (missing(expr) || !is.call(expr)) return(invisible(NULL))
        if (identical(expr[[1L]], as.name("<-")) && is.symbol(expr[[2L]]) &&
            as.character(expr[[2L]]) %in% wanted) return(invisible(eval(expr, env)))
        invisible(lapply(as.list(expr)[-1L], walk))
    }
    for (expr in parse(file.path(source_root, "server.R"))) walk(expr)
    stopifnot(all(vapply(wanted, exists, logical(1), envir = env, inherits = FALSE)))
    env
}
legacy <- make_search(TRUE)
candidate <- make_search()
clear_keys <- function() {
    env <- get("normalized_choices_cache", environment(candidate$autocomplete_keys_for_choices))
    rm(list = ls(env, all.names = TRUE), envir = env)
}
measure <- function(fn) {
    started <- proc.time()[["elapsed"]]
    value <- fn()
    list(ms = (proc.time()[["elapsed"]] - started) * 1000, value = value)
}
cases <- list(
    list(name = "human_partial_FOX", paths = paths[1], query = "FOX", kind = "partial"),
    list(name = "cross_partial_FOX", paths = paths[1:2], query = "FOX", kind = "partial"),
    list(name = "four_species_partial_HKT", paths = paths, query = "HKT", kind = "partial"),
    list(name = "human_exact_AMY1A", paths = paths[1], query = "AMY1A", kind = "exact"),
    list(name = "cross_exact_TP53", paths = paths[1:2], query = "TP53", kind = "count")
)
run_case <- function(env, case) {
    switch(case$kind,
        partial = env$find_partial_gene_suggestions_from_autocomplete_cache(case$paths, case$query, max_total = 20000L),
        exact = env$has_fast_exact_gene_match(case$paths, case$query, time_budget_sec = 30),
        count = env$count_fast_exact_gene_matches(case$paths, case$query, min_count = 2L, time_budget_sec = 30)
    )
}
cat("case\tlegacy_ms\tfirst_keys_ms\treused_keys_ms\tidentical\n")
for (case in cases) {
    # Warm R's function compilation and disk/OS caches equally. 'first_keys'
    # means a new derived-key cache, not a cold container or cold filesystem.
    stopifnot(identical(run_case(legacy, case), run_case(candidate, case)))
    times <- matrix(NA_real_, nrow = 3, ncol = 3)
    for (i in 1:3) {
        clear_keys()
        old <- measure(function() run_case(legacy, case))
        fresh <- measure(function() run_case(candidate, case))
        reused <- measure(function() run_case(candidate, case))
        stopifnot(identical(old$value, fresh$value), identical(old$value, reused$value))
        times[i, ] <- c(old$ms, fresh$ms, reused$ms)
    }
    med <- apply(times, 2, median)
    cat(sprintf("%s\t%.1f\t%.1f\t%.1f\tTRUE\n", case$name, med[1], med[2], med[3]))
}
