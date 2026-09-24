library(testthat)
library(shiny)

`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L) y else x
}

resolve_project_file <- function(...) {
    direct <- file.path(...)
    if (file.exists(direct)) direct else file.path("..", "..", ...)
}

lifecycle_path <- resolve_project_file("R", "server_plot_lifecycle_domain.R")
sys.source(lifecycle_path, envir = environment())

make_homologous_lifecycle_fixture <- function(with_hydrated = TRUE, hydrated = character(0)) {
    rv_list <- function(value = list()) shiny::reactiveVal(value)
    rv_num <- function(value) shiny::reactiveVal(value)
    hydrated_rv <- if (isTRUE(with_hydrated)) rv_list(as.character(hydrated)) else NULL
    args <- list(
        input = list(),
        output = new.env(parent = emptyenv()),
        session = shiny::MockShinySession$new(),
        preferredSearchWorkflow_fn = function() "homologous",
        preloadedRegistry_rv = rv_list(),
        searchStatusHomologous_rv = rv_list(character()),
        searchStatusOrthologous_rv = rv_list(character()),
        activePlotIdsHomologous_rv = rv_list(integer()),
        titlesHomologous_rv = rv_list(),
        existingPlotsHomologous_rv = rv_list(),
        fileDataHomologous_rv = rv_list(),
        chrNamesHomologous_rv = rv_list(),
        genSequencesHomologous_rv = rv_list(),
        plotSignaturesHomologous_rv = rv_list(),
        annotationPathsHomologous_rv = rv_list(),
        genomePathsHomologous_rv = rv_list(),
        organismInfoHomologous_rv = rv_list(),
        plotMetricsHomologous_rv = rv_list(),
        plotGeneMetaHomologous_rv = rv_list(),
        closeObserversBoundHomologous_rv = rv_list(character()),
        activePlotIdsOrthologous_rv = rv_list(integer()),
        titlesOrthologous_rv = rv_list(),
        existingPlotsOrthologous_rv = rv_list(),
        fileDataOrthologous_rv = rv_list(),
        chrNamesOrthologous_rv = rv_list(),
        genSequencesOrthologous_rv = rv_list(),
        plotSignaturesOrthologous_rv = rv_list(),
        annotationPathsOrthologous_rv = rv_list(),
        genomePathsOrthologous_rv = rv_list(),
        organismInfoOrthologous_rv = rv_list(),
        plotMetricsOrthologous_rv = rv_list(),
        plotGeneMetaOrthologous_rv = rv_list(),
        closeObserversBoundOrthologous_rv = rv_list(character()),
        homoRenderedPlotIds_rv = rv_list(character()),
        homoInsertedCardIds_rv = rv_list(character()),
        homoFooterOutputsBound_rv = rv_list(character()),
        homoDownloadOutputsBound_rv = rv_list(character()),
        homoPlotTimingTracker_rv = rv_list(),
        orthoRenderedPlotIds_rv = rv_list(character()),
        orthoInsertedCardIds_rv = rv_list(character()),
        orthoFooterOutputsBound_rv = rv_list(character()),
        orthoDownloadOutputsBound_rv = rv_list(character()),
        orthoPlotTimingTracker_rv = rv_list(),
        homoVisibleCount_rv = rv_num(1L),
        homoInitialVisibleCount = 1L,
        orthoVisibleCount_rv = rv_num(1L),
        orthoInitialVisibleCount = 1L,
        max_gene_length_homo_rv = rv_num(0),
        min_gene_coord_homo_rv = rv_num(Inf),
        max_gene_coord_homo_rv = rv_num(-Inf),
        max_gene_length_ortho_rv = rv_num(0),
        min_gene_coord_ortho_rv = rv_num(Inf),
        max_gene_coord_ortho_rv = rv_num(-Inf),
        orthoAlignedRenderCache_rv = rv_list(),
        orthoAlignedTrackCache_env = new.env(parent = emptyenv()),
        orthoAlignedSceneCache_env = new.env(parent = emptyenv()),
        orthoAlignedPlotCache_env = new.env(parent = emptyenv()),
        orthoAlignedSeqCache_env = new.env(parent = emptyenv()),
        orthoAlignedGcCache_env = new.env(parent = emptyenv()),
        orthoHomologyCache_env = new.env(parent = emptyenv()),
        clear_summary_cache_scope_fn = function(...) invisible(NULL),
        clear_analytics_cache_scope_fn = function(...) invisible(NULL),
        empty_plot_timing_tracker_fn = function() list(),
        drop_plot_timing_id_fn = function(...) invisible(NULL),
        mark_plot_ready_timing_fn = function(...) invisible(NULL),
        append_status_fn = function(...) invisible(NULL),
        emit_popup_status_fn = function(...) invisible(NULL)
    )
    if (isTRUE(with_hydrated)) {
        args$homoHydratedIsoformIds_rv <- hydrated_rv
    }
    list(
        domain = do.call(init_plot_lifecycle_domain, args),
        args = args,
        session = args$session,
        hydrated = hydrated_rv
    )
}

seed_two_gene_group <- function(fixture) {
    args <- fixture$args
    args$activePlotIdsHomologous_rv(c(1L, 2L, 3L, 4L))
    args$fileDataHomologous_rv(list(
        "1" = data.frame(V3 = "transcript", V4 = 101, V5 = 200),
        "2" = data.frame(V3 = "transcript", V4 = 201, V5 = 400),
        "3" = data.frame(V3 = "transcript", V4 = 210, V5 = 380),
        "4" = data.frame(V3 = "transcript", V4 = 120, V5 = 190)
    ))
    args$annotationPathsHomologous_rv(list(
        "1" = "annotation-a.gff", "2" = "annotation-a.gff",
        "3" = "annotation-a.gff", "4" = "annotation-a.gff"
    ))
    args$organismInfoHomologous_rv(list(
        "1" = list(name = "Species A"), "2" = list(name = "Species A"),
        "3" = list(name = "Species A"), "4" = list(name = "Species A")
    ))
    args$plotGeneMetaHomologous_rv(list(
        "1" = list(is_canonical = TRUE, matched_gene_id = "gene-short"),
        "2" = list(is_canonical = TRUE, matched_gene_id = "gene-long"),
        "3" = list(is_canonical = FALSE, matched_gene_id = "gene-long"),
        "4" = list(is_canonical = FALSE, matched_gene_id = "gene-short"),
        "2_c" = list(is_canonical = FALSE, is_canonical_copy = TRUE, matched_gene_id = "gene-long")
    ))
    invisible(NULL)
}

test_that("destroying one homologous plot prunes only that hydrated id", {
    fixture <- make_homologous_lifecycle_fixture(hydrated = c("2", "3", "2_c", "4"))
    shiny::withReactiveDomain(
        fixture$session,
        shiny::isolate(fixture$domain$destroy_homologous_plot_runtime("2"))
    )
    expect_identical(shiny::isolate(fixture$hydrated()), c("3", "2_c", "4"))

    shiny::withReactiveDomain(
        fixture$session,
        shiny::isolate(fixture$domain$destroy_homologous_plot_runtime("2"))
    )
    expect_identical(shiny::isolate(fixture$hydrated()), c("3", "2_c", "4"))
})

test_that("destroying a canonical copy prunes only the copy lifecycle id", {
    fixture <- make_homologous_lifecycle_fixture(hydrated = c("2", "2_c", "4"))
    shiny::withReactiveDomain(
        fixture$session,
        shiny::isolate(fixture$domain$destroy_homologous_plot_runtime("2_c"))
    )
    expect_identical(shiny::isolate(fixture$hydrated()), c("2", "4"))
})

test_that("group removal prunes the removed gene ids, copies, and keeps unrelated hydrated ids", {
    fixture <- make_homologous_lifecycle_fixture(hydrated = c("3", "2_c", "4"))
    seed_two_gene_group(fixture)
    shiny::withReactiveDomain(
        fixture$session,
        shiny::isolate(fixture$domain$remove_homologous_plot("2", announce = FALSE))
    )
    expect_identical(shiny::isolate(fixture$hydrated()), "4")
    expect_identical(shiny::isolate(fixture$args$activePlotIdsHomologous_rv()), c(1L, 4L))
})

test_that("whole-panel clear resets the hydrated lifecycle", {
    fixture <- make_homologous_lifecycle_fixture(hydrated = c("2", "3", "2_c", "4"))
    seed_two_gene_group(fixture)
    shiny::withReactiveDomain(
        fixture$session,
        shiny::isolate(fixture$domain$clear_homologous_visualizations())
    )
    expect_identical(shiny::isolate(fixture$hydrated()), character(0))
    expect_identical(shiny::isolate(fixture$args$activePlotIdsHomologous_rv()), integer())
    expect_identical(shiny::isolate(fixture$args$homoInsertedCardIds_rv()), character(0))
})

test_that("clear unblocks re-hydration after ID reuse (A to B to A proxy)", {
    fixture <- make_homologous_lifecycle_fixture(hydrated = c("2", "3", "1_c"))
    seed_two_gene_group(fixture)
    shiny::withReactiveDomain(
        fixture$session,
        shiny::isolate(fixture$domain$clear_homologous_visualizations())
    )
    expanded_ids <- c("2", "3", "1_c")
    expect_identical(setdiff(expanded_ids, shiny::isolate(fixture$hydrated())), expanded_ids)
})

test_that("repeated expand/clear cycles do not accumulate hydrated ids", {
    fixture <- make_homologous_lifecycle_fixture(hydrated = c("2", "1_c"))
    for (cycle in seq_len(3L)) {
        shiny::isolate(fixture$hydrated(c("2", "1_c")))
        seed_two_gene_group(fixture)
        shiny::withReactiveDomain(
            fixture$session,
            shiny::isolate(fixture$domain$clear_homologous_visualizations())
        )
        expect_identical(shiny::isolate(fixture$hydrated()), character(0))
    }
})

test_that("lifecycle domain remains backward compatible without the hydrated rv", {
    fixture <- make_homologous_lifecycle_fixture(with_hydrated = FALSE)
    expect_silent(
        shiny::withReactiveDomain(
            fixture$session,
            shiny::isolate(fixture$domain$destroy_homologous_plot_runtime("1"))
        )
    )
    expect_silent(
        shiny::withReactiveDomain(
            fixture$session,
            shiny::isolate(fixture$domain$clear_homologous_visualizations())
        )
    )
})

test_that("server.R wires the hydrated lifecycle into the plot lifecycle domain", {
    server_txt <- paste(readLines(resolve_project_file("server.R"), warn = FALSE), collapse = "\n")
    lifecycle_txt <- paste(readLines(lifecycle_path, warn = FALSE), collapse = "\n")

    expect_match(server_txt, "homoHydratedIsoformIds_rv = homoHydratedIsoformIds", fixed = TRUE)
    expect_match(lifecycle_txt, "homoHydratedIsoformIds_rv = NULL", fixed = TRUE)
    expect_match(lifecycle_txt, "hydrated_keep <- setdiff(hydrated_now, id_chr)", fixed = TRUE)
    expect_match(lifecycle_txt, "homoHydratedIsoformIds_rv(character())", fixed = TRUE)
    expect_match(
        server_txt,
        "hydrated_remove <- unique(c(ids_chr, paste0(ids_chr, \"_c\")))",
        fixed = TRUE
    )
})
