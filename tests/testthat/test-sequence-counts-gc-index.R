library(testthat)
sequence_root <- if (file.exists("R/utils.R")) "." else "../.."
sequence_env <- new.env(parent = globalenv())
sys.source(file.path(sequence_root, "R/utils.R"), sequence_env)

legacy_counts <- function(sequence) {
    stats::setNames(as.integer(vapply(c("A", "T", "C", "G"), function(base) {
        nchar(gsub(paste0("[^", base, "]"), "", sequence))
    }, numeric(1))), c("A", "T", "C", "G"))
}
legacy_gc <- function(sequence) {
    sequence <- toupper(gsub("\\s+", "", sequence))
    known <- gsub("[^ATCG]", "", sequence)
    if (!nchar(known)) return(NA_real_)
    round(100 * nchar(gsub("[^GC]", "", known)) / nchar(known), 2)
}

test_that("base counts preserve ambiguity, whitespace and missing values", {
    set.seed(271)
    sequences <- c("", NA_character_, "NNRY-?", "ATCG", "aTcg\n NRY", "éATGCß",
                   paste(sample(c("A", "T", "C", "G", "N", "R", "Y", "-", " "), 10000, TRUE), collapse = ""))
    for (sequence in toupper(sequences)) {
        expect_identical(sequence_env$count_sequence_bases(sequence), legacy_counts(sequence))
    }
    # The public helper uses total bytes, including ambiguous bases/whitespace;
    # the transcript cache uses canonical bases only. Never exchange them.
    expect_identical(sequence_env$calculate_sequence_composition("ACNN")$composition,
                     "Sequence Composition: A = 25.00%\tT = 0.00%  C = 25.00%  G = 0.00%")
    expect_identical(sequence_env$calculate_sequence_composition("a cN")$length, 4L)
    expect_equal(sequence_env$calculate_sequence_composition(NA_character_)$length, 0)
})

test_that("prefix GC preserves coordinates, canonical denominators and rounding", {
    set.seed(907)
    sequence <- paste(sample(c("A", "t", "C", "g", "N", "R", "-", " ", "\n"), 3000, TRUE), collapse = "")
    index <- sequence_env$make_genomic_gc_index(sequence)
    starts <- c(1L, 3000L, sample(1:2900, 150))
    ends <- c(3000L, 3000L, pmin(3000L, starts[-(1:2)] + sample(0:100, 150, TRUE)))
    expected <- vapply(substring(sequence, starts, ends), legacy_gc, numeric(1))
    expect_identical(sequence_env$gc_percent_from_index(index, starts, ends), unname(expected))
    expect_identical(sequence_env$gc_percent_from_index(index, c(0, NA, 5, 1), c(1, 4, 2, 3001)), rep(NA_real_, 4))
    ambiguous <- sequence_env$make_genomic_gc_index("NN--")
    expect_identical(sequence_env$gc_percent_from_index(ambiguous, 1, 4), NA_real_)
    expect_identical(sequence_env$gc_percent_from_index(sequence_env$make_genomic_gc_index("ACNN"), 1, 4), 50)
    expect_null(sequence_env$make_genomic_gc_index(""))
    expect_null(sequence_env$make_genomic_gc_index("AéGC"))
    expect_null(sequence_env$make_genomic_gc_index("ACGT", max_bases = 3L))
    expect_lte(as.numeric(object.size(index)), 8 * (nchar(sequence) + 1L) + 1024)
})

test_that("transcript cache preserves known-base denominator and invalidates by source", {
    env <- new.env(parent = globalenv())
    sys.source(file.path(sequence_root, "R/utils.R"), env)
    sequence <- "a cNN\n"
    reads <- 0L
    env$extract_spliced_exon_sequence <- function(...) { reads <<- reads + 1L; sequence }
    path <- tempfile()
    writeLines("source version 1", path)
    on.exit(unlink(path), add = TRUE)
    exons <- data.frame(start = 1L, end = 6L)
    first <- env$get_transcript_composition_cached(path, "chr1", exons)
    second <- env$get_transcript_composition_cached(path, "chr1", exons)
    expect_identical(first, second)
    expect_identical(reads, 1L)
    expect_identical(first$known_total, 2L)
    expect_identical(first$length, 4L)
    expect_identical(first$counts, c(A = 1L, T = 0L, C = 1L, G = 0L))
    expect_identical(first$composition, "Sequence Composition: A = 50.00%\tT = 0.00%  C = 50.00%  G = 0.00%")
    sequence <- "GGNT"
    writeLines("source version 2, new size", path)
    changed <- env$get_transcript_composition_cached(path, "chr1", exons)
    expect_identical(reads, 2L)
    expect_identical(changed$counts, c(A = 0L, T = 1L, C = 0L, G = 2L))
    expect_identical(env$parse_sequence_composition_blob(env$make_sequence_composition_blob(changed))$counts, changed$counts)
})

test_that("sequence fetch reuses the transcript without conflating genomic fallback", {
    env <- new.env(parent = globalenv())
    sys.source(file.path(sequence_root, "R/utils.R"), env)
    path <- tempfile()
    writeLines("genome", path)
    on.exit(unlink(path), add = TRUE)
    sequence <- "ACNN"
    reads <- 0L
    env$extract_spliced_exon_sequence <- function(...) { reads <<- reads + 1L; sequence }
    env$extract_sequence_from_fasta <- function(...) "TTGG"
    exons <- data.frame(start = 1L, end = 4L)
    info <- env$fetch_gene_data_sync("chr1", list(start = 1, end = 4), path, "tx1", exons)
    expect_identical(reads, 1L)
    expect_identical(info$sequence, "ACNN")
    expect_identical(info$composition$known_total, 2L)
    expect_identical(info$file_content, ">tx1 | chr=chr1 | start=1 | end=4\nACNN")
    sequence <- ""
    info <- env$fetch_gene_data_sync("chr2", list(start = 1, end = 4), path, "tx2", exons)
    expect_identical(reads, 2L)
    expect_identical(info$sequence, "TTGG")
    expect_identical(info$composition$known_total, 0L)
    expect_identical(info$composition$length, 0L)
    expect_identical(info$composition$composition, "Sequence Composition: N/A (Sequence empty)")
    info <- env$fetch_gene_data_sync("chr3", list(start = 1, end = 4), path, "tx3", exon_ranges = NULL)
    expect_identical(info$composition$counts, c(A = 0L, T = 2L, C = 0L, G = 2L))
    expect_identical(info$composition$known_total, 4L)
})
