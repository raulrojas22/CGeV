library(testthat)
root <- if (file.exists("R/modules.R")) "." else "../.."
girafe_identity_env <- new.env(parent = globalenv())
girafe_identity_env$`%||%` <- function(a, b) if (!is.null(a)) a else b
sys.source(file.path(root, "R/modules.R"), girafe_identity_env)

test_that("shared final-widget identity separates display, payloads and file versions", {
    e <- girafe_identity_env
    annotation <- tempfile()
    on.exit(unlink(annotation))
    writeLines("annotation", annotation)
    inputs <- list(annotation_file_path = annotation, gene_display_name = "TP53",
                   plot_id = "1", organism_label = "Human", neighbor_context = list(),
                   precomputed_genomic_span = "ACGT", df = data.frame(start = 10))
    key <- function(x = inputs, sig = "annotation||TP53||tx||chr1||10||20")
        e$make_girafe_plot_cache_key("homologous", sig, fallback_id = "1", render_inputs = x)
    original <- key()
    expect_identical(original, key())
    for (change in list(list(gene_display_name = "P53 -> TP53"), list(plot_id = "2"),
                        list(organism_label = "Mouse"), list(neighbor_context = list(name = "OTHER")),
                        list(precomputed_genomic_span = "AAAA"), list(df = data.frame(start = 11)))) {
        altered <- inputs
        altered[names(change)] <- change
        expect_false(identical(original, key(altered)))
    }
    # Equal-length replacement with only a fractional-second metadata change.
    stamp <- file.info(annotation)$mtime
    Sys.setFileTime(annotation, stamp + 0.125)
    expect_false(identical(original, key()))
    changed_time <- key()
    writeLines("longer annotation", annotation)
    Sys.setFileTime(annotation, stamp + 0.125)
    expect_false(identical(changed_time, key()))
    unlink(annotation)
    expect_identical(key(), "")
})

test_that("fallback renders cannot populate the global widget cache", {
    e <- girafe_identity_env
    before <- ls(e$.cgv_girafe_plot_cache)
    for (sig in list(NULL, "", NA_character_, "plot_id:1")) {
        k <- e$make_girafe_plot_cache_key("homologous", sig, fallback_id = "1")
        expect_identical(k, "")
        expect_null(e$get_shared_girafe_plot_cache(k))
        expect_false(e$set_shared_girafe_plot_cache(k, list(svg = "request-specific")))
    }
    expect_identical(ls(e$.cgv_girafe_plot_cache), before)
})

test_that("safe entries retain LRU eviction and disabled-cache behavior", {
    e <- girafe_identity_env
    withr::local_envvar(APP_GIRAFE_PLOT_CACHE_MAX_ENTRIES = "2")
    e$set_shared_girafe_plot_cache("a", list(svg = "a"))
    e$set_shared_girafe_plot_cache("b", list(svg = "b"))
    expect_identical(e$get_shared_girafe_plot_cache("a"), list(svg = "a"))
    e$set_shared_girafe_plot_cache("c", list(svg = "c"))
    expect_null(e$get_shared_girafe_plot_cache("b"))
    expect_length(ls(e$.cgv_girafe_plot_cache), 2L)
    withr::local_envvar(APP_GIRAFE_PLOT_CACHE_MAX_ENTRIES = "0")
    expect_null(e$get_shared_girafe_plot_cache("a"))
    expect_false(e$set_shared_girafe_plot_cache("d", list(svg = "d")))
})
