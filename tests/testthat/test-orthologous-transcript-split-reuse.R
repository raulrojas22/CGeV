library(testthat)

utils_path <- file.path("R", "utils.R")
server_path <- "server.R"
if (!file.exists(utils_path)) {
    utils_path <- file.path("..", "..", "R", "utils.R")
    server_path <- file.path("..", "..", "server.R")
}

utils_env <- new.env(parent = globalenv())
sys.source(utils_path, envir = utils_env)

make_transcript_fixture <- function(gene_id) {
    data.frame(
        V1 = rep("chr1", 7L),
        V2 = rep("fixture", 7L),
        V3 = c("gene", "mRNA", "exon", "CDS", "mRNA", "exon", "CDS"),
        V4 = c(1L, 1L, 1L, 50L, 1L, 1L, 300L),
        V5 = c(500L, 200L, 100L, 150L, 500L, 120L, 500L),
        V6 = rep(".", 7L),
        V7 = rep("+", 7L),
        V8 = rep(".", 7L),
        V9 = c(
            sprintf("ID=gene-%s;Name=%s", gene_id, gene_id),
            sprintf("ID=rna-%s-1;Parent=gene-%s", gene_id, gene_id),
            sprintf("ID=exon-%s-1;Parent=rna-%s-1", gene_id, gene_id),
            sprintf("ID=cds-%s-1;Parent=rna-%s-1", gene_id, gene_id),
            sprintf("ID=rna-%s-2;Parent=gene-%s", gene_id, gene_id),
            sprintf("ID=exon-%s-2;Parent=rna-%s-2", gene_id, gene_id),
            sprintf("ID=cds-%s-2;Parent=rna-%s-2", gene_id, gene_id)
        ),
        stringsAsFactors = FALSE
    )
}

test_that("orthologous transcript splits run once per found result and remain identical", {
    data_a <- make_transcript_fixture("GENEA")
    data_b <- make_transcript_fixture("GENEB")
    results <- list(
        list(found = TRUE, data = data_a),
        list(found = FALSE, data = NULL),
        list(found = TRUE, data = data_b)
    )
    found_idx <- c(1L, 3L)
    expected <- lapply(found_idx, function(idx) {
        utils_env$split_gene_data_by_transcript(results[[idx]]$data)
    })

    counter <- new.env(parent = emptyenv())
    counter$n <- 0L
    counting_split <- function(data) {
        counter$n <- counter$n + 1L
        utils_env$split_gene_data_by_transcript(data)
    }
    prepared <- utils_env$prepare_orthologous_transcript_splits_once(
        results,
        split_fun = counting_split
    )

    expect_identical(counter$n, length(found_idx))
    expect_null(prepared[[2L]])
    expect_true(all(vapply(prepared[found_idx], function(item) isTRUE(item$reusable), logical(1))))
    actual <- lapply(found_idx, function(idx) prepared[[idx]]$blocks)
    expect_identical(actual, expected)
})

test_that("orthologous split prepass preserves empty and error fallbacks", {
    data <- make_transcript_fixture("FALLBACK")
    results <- list(list(found = TRUE, data = data))

    empty_split <- utils_env$prepare_orthologous_transcript_splits_once(
        results,
        split_fun = function(data) list()
    )[[1L]]
    expect_true(empty_split$reusable)
    expect_identical(empty_split$blocks, list(data))

    failed_split <- utils_env$prepare_orthologous_transcript_splits_once(
        results,
        split_fun = function(data) stop("fixture split failure")
    )[[1L]]
    expect_false(failed_split$reusable)
    expect_identical(failed_split$blocks, list(data))
})

test_that("transcript splitting follows indexed multi-level parent relationships", {
    transcript_n <- 60L
    children_per_transcript <- 10L
    transcript_ids <- sprintf("rna-TX%03d", seq_len(transcript_n))
    exon_ids <- sprintf("exon-%05d", seq_len(transcript_n * children_per_transcript))
    exon_transcripts <- rep(transcript_ids, each = children_per_transcript)

    gene <- data.frame(
        V1 = "chr1", V2 = "fixture", V3 = "gene", V4 = 1L, V5 = 999999L,
        V6 = ".", V7 = "+", V8 = ".", V9 = "ID=gene-STRESS;Name=STRESS"
    )
    transcripts <- data.frame(
        V1 = "chr1", V2 = "fixture", V3 = "mRNA",
        V4 = seq_len(transcript_n) * 100L,
        V5 = seq_len(transcript_n) * 100L + 99L,
        V6 = ".", V7 = "+", V8 = ".",
        V9 = paste0("ID=", transcript_ids, ";Parent=gene-STRESS")
    )
    exons <- data.frame(
        V1 = "chr1", V2 = "fixture", V3 = "exon",
        V4 = seq_along(exon_ids), V5 = seq_along(exon_ids) + 10L,
        V6 = ".", V7 = "+", V8 = ".",
        V9 = paste0("ID=", exon_ids, ";Parent=", exon_transcripts)
    )
    cds <- data.frame(
        V1 = "chr1", V2 = "fixture", V3 = "CDS",
        V4 = seq_along(exon_ids), V5 = seq_along(exon_ids) + 5L,
        V6 = ".", V7 = "+", V8 = "0",
        V9 = paste0("ID=cds-", seq_along(exon_ids), ";Parent=", exon_ids)
    )

    blocks <- utils_env$split_gene_data_by_transcript(rbind(gene, transcripts, exons, cds))

    expect_length(blocks, transcript_n)
    expect_true(all(vapply(
        blocks,
        function(block) nrow(block) == 2L + 2L * children_per_transcript,
        logical(1)
    )))
})

test_that("transcript splitting preserves encoded IDs and multiple Parent fields", {
    fixture <- data.frame(
        V1 = rep("chr1", 6L), V2 = rep("fixture", 6L),
        V3 = c("gene", "mRNA", "exon", "CDS", "mRNA", "exon"),
        V4 = seq_len(6L), V5 = seq_len(6L) + 10L,
        V6 = ".", V7 = "+", V8 = ".",
        V9 = c(
            "ID=gene-MULTI",
            "ID=rna-A%2E1;Parent=gene-MULTI",
            "ID=exon-shared;Parent=rna-A%2E1,rna-B.1",
            "ID=cds-shared;Parent=rna-A%2E1;Parent=rna-B.1",
            "ID=rna-B.1;Parent=gene-MULTI",
            "ID=exon-B;Parent=rna-B.1"
        ),
        stringsAsFactors = FALSE
    )

    blocks <- utils_env$split_gene_data_by_transcript(fixture)

    expect_length(blocks, 2L)
    expect_setequal(names(blocks), c("rna-A.1", "rna-B.1"))
    expect_equal(nrow(blocks[["rna-A.1"]]), 4L)
    expect_equal(nrow(blocks[["rna-B.1"]]), 5L)
    meta_a <- attr(blocks[["rna-A.1"]], "cgev_transcript_meta", exact = TRUE)
    expect_identical(meta_a$transcript, "rna-A.1")
    expect_identical(meta_a$chromosome, "chr1")
    expect_equal(unname(meta_a$span), c(2, 12))
    expect_equal(nrow(meta_a$exon_ranges), 1L)
    expect_equal(nrow(meta_a$cds_ranges), 1L)
    expect_identical(
        utils_env$extract_plot_labels(blocks[["rna-A.1"]]),
        list(transcript = "rna-A.1", chromosome = "chr1")
    )
})

test_that("the orthologous processor reuses successful prepass blocks", {
    server_txt <- paste(readLines(server_path, warn = FALSE), collapse = "\n")
    start <- regexpr("process_orthologous_lookup_results <- function", server_txt, fixed = TRUE)[[1L]]
    expect_gt(start, 0L)
    tail_txt <- substring(server_txt, start)
    finish_offset <- regexpr("finish_orthologous_local_phase <- function", tail_txt, fixed = TRUE)[[1L]]
    expect_gt(finish_offset, 0L)
    processor_txt <- substring(tail_txt, 1L, finish_offset - 1L)

    expect_match(
        processor_txt,
        "phase_transcript_splits <- prepare_orthologous_transcript_splits_once(",
        fixed = TRUE
    )
    expect_match(
        processor_txt,
        "is.list(prepared_split) && isTRUE(prepared_split$reusable)",
        fixed = TRUE
    )
    expect_match(
        processor_txt,
        "phase_transcript_splits[res_idx] <- list(NULL)",
        fixed = TRUE
    )
    direct_calls <- gregexpr(
        "prepared_split <- shared_gene_split(",
        processor_txt,
        fixed = TRUE
    )[[1L]]
    expect_identical(sum(direct_calls > 0L), 1L)
})
