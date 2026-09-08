#!/usr/bin/env Rscript
# Real sequences and all annotated exon/CDS intervals for each gene. These
# timings isolate counting; they do not measure a Shiny session or card paint.
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(purrr); library(vroom) })
source("R/alias_resolution.R")
source("R/utils.R")

legacy_counts <- function(sequence) {
    stats::setNames(as.integer(vapply(c("A", "T", "C", "G"), function(base) {
        nchar(gsub(paste0("[^", base, "]"), "", sequence))
    }, numeric(1))), c("A", "T", "C", "G"))
}
legacy_gc <- function(sequence, starts, ends) {
    seqs <- toupper(gsub("\\s+", "", substring(sequence, starts, ends)))
    known <- gsub("[^ATCG]", "", seqs)
    denominator <- nchar(known)
    gc <- nchar(gsub("[^GC]", "", known))
    ifelse(denominator > 0L, round(100 * gc / denominator, 2), NA_real_)
}
measure <- function(fun, repetitions = 10L) {
    samples <- replicate(3L, {
        started <- proc.time()[["elapsed"]]
        for (i in seq_len(repetitions)) fun()
        (proc.time()[["elapsed"]] - started) * 1000 / repetitions
    })
    median(samples)
}
registry <- read.delim("annotations/registry.tsv", stringsAsFactors = FALSE)
cases <- list(c("Homo sapiens", "AMY1A"), c("Homo sapiens", "AMY2A"),
              c("Homo sapiens", "TP53"), c("Mus musculus", "Trp53"))
cat(sprintf("R=%s platform=%s median_of=3 batches repetitions_per_batch=10\n", getRversion(), R.version$platform))
cat("case\tbases\tintervals\told_counts_ms\tnew_counts_ms\told_gc_ms\tindex_build_and_gc_ms\tidentical\n")
for (case in cases) {
    row <- registry[match(case[1], registry$label), ]
    result <- search_gene_in_file(row$annotation, case[2], show_diagnostics = FALSE,
                                  match_mode = "exact", return_meta = TRUE)
    df <- result$data
    stopifnot(is.data.frame(df), nrow(df) > 0)
    df <- df[tolower(df$V3) %in% c("exon", "cds"), ]
    lo <- min(df$V4); hi <- max(df$V5)
    sequence <- extract_sequence_from_fasta(row$genome_2bit, df$V1[1], lo, hi)
    stopifnot(nzchar(sequence), nchar(sequence) == hi - lo + 1)
    starts <- df$V4 - lo + 1L; ends <- df$V5 - lo + 1L
    old_counts <- function() legacy_counts(toupper(sequence))
    new_counts <- function() count_sequence_bases(toupper(sequence))
    old_gc <- function() legacy_gc(sequence, starts, ends)
    new_gc <- function() gc_percent_from_index(make_genomic_gc_index(sequence), starts, ends)
    stopifnot(identical(old_counts(), new_counts()), identical(old_gc(), new_gc()))
    times <- vapply(list(old_counts, new_counts, old_gc, new_gc), measure, numeric(1))
    cat(sprintf("%s/%s\t%d\t%d\t%.3f\t%.3f\t%.3f\t%.3f\tTRUE\n",
                case[1], case[2], nchar(sequence), nrow(df), times[1], times[2], times[3], times[4]))
}
