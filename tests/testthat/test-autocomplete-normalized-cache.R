library(testthat)

normalization_root <- if (file.exists(file.path("R", "utils.R"))) "." else file.path("..", "..")

make_normalization_fixture <- function() {
    env <- new.env(parent = globalenv())
    sys.source(file.path(normalization_root, "R", "utils.R"), env)
    sys.source(file.path(normalization_root, "R", "server_autocomplete_domain.R"), env)
    env$sidecar_reads <- 0L
    env$normalizations <- 0L
    normalizer <- env$normalize_partial_gene_choices
    env$normalize_partial_gene_choices <- function(x) {
        env$normalizations <- env$normalizations + 1L
        normalizer(x)
    }
    env$load_gff_autocomplete_cache <- function(...) {
        env$sidecar_reads <- env$sidecar_reads + 1L
        NULL
    }
    cell <- function(initial) {
        value <- initial
        function(next_value) {
            if (missing(next_value)) return(value)
            value <<- next_value
        }
    }
    env$geneAutocompleteCache <- cell(list())
    env$domain <- env$init_autocomplete_domain(
        env$geneAutocompleteCache, cell(list()), cell(list()),
        list(sendCustomMessage = function(...) NULL)
    )
    env$autocomplete_keys_for_choices <- env$domain$autocomplete_keys_for_choices
    env
}

test_that("batched normalization preserves scalar values and names, including bad escapes", {
    env <- make_normalization_fixture()
    cases <- c("TP53", "Trp53", "HKT1;5", "LOC_Os01g01010", "foo%3Bbar",
               "a%00b", "a%invalid", "a%", NA_character_, "", " X-Y ", "éGENE")
    for (x in list(character(0), cases, setNames(cases, paste0("id", seq_along(cases))))) {
        expect_identical(env$normalize_partial_gene_choices(x),
                         vapply(x, env$normalize_partial_gene_query, character(1)))
    }
    invalid <- c("TP53", "a%FFb")
    legacy <- tryCatch(vapply(invalid, env$normalize_partial_gene_query, character(1)), error = identity)
    candidate <- tryCatch(env$normalize_partial_gene_choices(invalid), error = identity)
    expect_identical(class(candidate), class(legacy))
    if (inherits(legacy, "error")) expect_identical(conditionMessage(candidate), conditionMessage(legacy))
    else expect_identical(candidate, legacy)
})

test_that("unchanged choices avoid normalization and disk reads; revisions invalidate", {
    env <- make_normalization_fixture()
    path <- tempfile(fileext = ".gff3")
    writeLines("initial annotation", path)
    on.exit(unlink(path), add = TRUE)
    choices <- c("TP53", "Trp53", "HKT1;5")
    expect_identical(env$autocomplete_keys_for_choices(path, choices), c("tp53", "trp53", "hkt15"))
    expect_identical(env$autocomplete_keys_for_choices(path, choices), c("tp53", "trp53", "hkt15"))
    expect_identical(env$normalizations, 1L)
    expect_identical(env$sidecar_reads, 1L)
    expect_identical(env$autocomplete_keys_for_choices(path, rev(choices)), c("hkt15", "trp53", "tp53"))
    expect_identical(env$normalizations, 2L)
    expect_identical(env$autocomplete_keys_for_choices(path, c(choices, "BRCA1")),
                     c("tp53", "trp53", "hkt15", "brca1"))
    expect_identical(env$normalizations, 3L)
    writeLines("changed annotation with a different size", path)
    env$autocomplete_keys_for_choices(path, c(choices, "BRCA1"))
    expect_identical(env$normalizations, 4L)
    second_session <- env$init_autocomplete_domain(function() list(), function() list(),
                                                  function() list(), list())
    second_session$autocomplete_keys_for_choices(path, choices)
    expect_identical(env$normalizations, 5L)
})

test_that("valid sidecar keys are reused and mismatched choices fall back safely", {
    env <- make_normalization_fixture()
    env$load_gff_autocomplete_cache <- function(...) {
        list(display = c("TP53", "HKT1;5"), keys = c("tp53", "hkt15"))
    }
    expect_identical(env$autocomplete_keys_for_choices("fixture", "TP53", "revision1"), "tp53")
    expect_identical(env$normalizations, 0L)
    expect_identical(env$autocomplete_keys_for_choices("fixture", c("HKT1;5", "TP53"), "revision1"),
                     c("hkt15", "tp53"))
    expect_identical(env$normalizations, 1L)
    env$load_gff_autocomplete_cache <- function(...) list(display = "TP53", keys = character())
    expect_identical(env$autocomplete_keys_for_choices("fixture", "TP53", "revision2"), "tp53")
    expect_identical(env$normalizations, 2L)
})

test_that("derived key cache is bounded without losing results after eviction", {
    env <- make_normalization_fixture()
    for (i in 1:25) env$autocomplete_keys_for_choices("fixture", "TP53", paste0("revision", i))
    before <- env$normalizations
    expect_identical(env$autocomplete_keys_for_choices("fixture", "TP53", "revision1"), "tp53")
    expect_identical(env$normalizations, before + 1L)
    large <- sprintf("GENE%07d", 1:90000)
    keys <- env$autocomplete_keys_for_choices("fixture", large, "large")
    expect_identical(keys, as.character(env$normalize_partial_gene_query(large)))
    cache <- get("normalized_choices_cache", environment(env$domain$autocomplete_keys_for_choices))
    expect_lte(length(env$cache_env_entry_keys(cache)), 24L)
    expect_lte(env$cache_env_usage_bytes(cache), 8 * 1024^2)
})

# Read the actual server helpers without starting an unrelated Shiny session.
load_normalized_search_helpers <- function(env) {
    wanted <- c("find_partial_gene_suggestions_from_autocomplete_cache",
                "has_fast_exact_gene_match", "count_fast_exact_gene_matches")
    walk <- function(expr) {
        if (missing(expr)) return(invisible(NULL))
        if (!is.call(expr)) return(invisible(NULL))
        if (identical(expr[[1L]], as.name("<-")) && is.symbol(expr[[2L]]) &&
            as.character(expr[[2L]]) %in% wanted) {
            eval(expr, env)
            return(invisible(NULL))
        }
        invisible(lapply(as.list(expr)[-1L], walk))
    }
    for (expr in parse(file.path(normalization_root, "server.R"))) walk(expr)
    stopifnot(all(vapply(wanted, exists, logical(1), envir = env, inherits = FALSE)))
}

test_that("multi-gene and cross-species cached searches retain all rows and ranking", {
    env <- make_normalization_fixture()
    legacy <- make_normalization_fixture()
    load_normalized_search_helpers(env)
    load_normalized_search_helpers(legacy)
    legacy$autocomplete_keys_for_choices <- function(path, choices, cache_key = NULL) {
        as.character(vapply(choices, legacy$normalize_partial_gene_query, character(1)))
    }
    paths <- c(tempfile(fileext = ".gff3"), tempfile(fileext = ".gff3"))
    for (p in paths) writeLines("fixture annotation", p)
    on.exit(unlink(paths), add = TRUE)
    choices <- list(c("FOX", "FOXP1", sprintf("FOX%03d", 1:55), "XFOX", "TP53", "a%00b", NA, ""),
                    c("Fox", "Foxp1", "Trp53", "HKT1;5", "HKT1.5", "  TP53  ", "TP53"))
    cache <- setNames(choices, vapply(paths, env$gff_cache_key, character(1)))
    for (e in list(env, legacy)) {
        e$geneAutocompleteCache(cache)
        e$load_gff_gene_light_index_if_available <- function(...) NULL
    }
    for (selected in list(paths[1], paths)) {
        for (query in c("FOX", "FOXP", "TP53", "HKT1", "a%00b", "absent", "")) {
            for (limit in c(3L, 100L)) {
                expect_identical(env$find_partial_gene_suggestions_from_autocomplete_cache(selected, query, max_total = limit),
                                 legacy$find_partial_gene_suggestions_from_autocomplete_cache(selected, query, max_total = limit))
            }
            expect_identical(env$has_fast_exact_gene_match(selected, query),
                             legacy$has_fast_exact_gene_match(selected, query))
            expect_identical(env$count_fast_exact_gene_matches(selected, query, min_count = 2L),
                             legacy$count_fast_exact_gene_matches(selected, query, min_count = 2L))
        }
    }
    all_fox <- env$find_partial_gene_suggestions_from_autocomplete_cache(paths, "FOX", max_total = 100L)
    expect_gt(nrow(all_fox), 20L)
    expect_equal(all_fox$source_count[all_fox$gene_name == "FOX"], 2L)
})
