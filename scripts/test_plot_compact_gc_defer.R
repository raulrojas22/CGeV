#!/usr/bin/env Rscript

`%||%` <- function(a, b) if (!is.null(a)) a else b

script_file <- NULL
for (arg in commandArgs(trailingOnly = FALSE)) {
    if (startsWith(arg, "--file=")) {
        script_file <- sub("^--file=", "", arg)
        break
    }
}

root_dir <- if (!is.null(script_file)) {
    normalizePath(file.path(dirname(script_file), ".."), winslash = "/", mustWork = FALSE)
} else {
    getwd()
}
if (!file.exists(file.path(root_dir, "global.R"))) {
    root_dir <- getwd()
}
if (!file.exists(file.path(root_dir, "global.R"))) {
    stop("Could not locate project root (global.R). Run from project root or scripts/.")
}
setwd(root_dir)

source(file.path(root_dir, "global.R"), local = .GlobalEnv)
app_env <- as.environment("app_libraries")

required <- c("create_gene_plot", "extract_sequence_from_fasta")
missing <- required[!vapply(required, exists, logical(1), envir = app_env, inherits = FALSE)]
if (length(missing) > 0L) {
    stop("Missing app function(s): ", paste(missing, collapse = ", "))
}

extract_calls <- 0L
mock_extract_sequence <- function(...) {
    extract_calls <<- extract_calls + 1L
    paste(rep("ATGC", 100), collapse = "")
}

tmp_fasta <- tempfile(fileext = ".fa")
writeLines(c(">chr1", paste(rep("ATGC", 100), collapse = "")), tmp_fasta)
on.exit(unlink(tmp_fasta), add = TRUE)

df <- data.frame(
    y = c(1, 1),
    xstart = c(110, 150),
    xend = c(130, 170),
    group = factor(c("exon", "cds")),
    text = c("ID=exon1", "ID=cds1"),
    feature_type = c("exon", "cds"),
    seqid = c("chr1", "chr1"),
    source = c("test", "test"),
    feature_raw = c("exon", "CDS"),
    score = c(".", "."),
    strand = c("+", "+"),
    phase = c(".", "0"),
    attributes_raw = c("ID=exon1;Parent=tx1", "ID=cds1;Parent=tx1"),
    largo = c(21, 21),
    stringsAsFactors = FALSE
)

df_gene <- data.frame(
    V1 = "chr1",
    V2 = "test",
    V3 = "gene",
    V4 = 100,
    V5 = 200,
    V6 = ".",
    V7 = "+",
    V8 = ".",
    V9 = "ID=gene1;Name=GENE1",
    largo = 101,
    stringsAsFactors = FALSE
)

df_tx <- data.frame(
    V1 = "chr1",
    V2 = "test",
    V3 = "mRNA",
    V4 = 100,
    V5 = 200,
    V6 = ".",
    V7 = "+",
    V8 = ".",
    V9 = "ID=tx1;Parent=gene1",
    stringsAsFactors = FALSE
)

create_gene_plot <- get("create_gene_plot", envir = app_env, inherits = FALSE)
patch_envs <- list(app_env, environment(create_gene_plot))
patch_envs <- patch_envs[!vapply(patch_envs, is.null, logical(1))]
patch_envs <- patch_envs[!duplicated(vapply(patch_envs, function(env) {
    sprintf("%s:%s", environmentName(env), capture.output(print(env))[1])
}, character(1)))]
original_extracts <- lapply(patch_envs, function(env) {
    if (exists("extract_sequence_from_fasta", envir = env, inherits = FALSE)) {
        get("extract_sequence_from_fasta", envir = env, inherits = FALSE)
    } else {
        NULL
    }
})
for (env in patch_envs) {
    assign("extract_sequence_from_fasta", mock_extract_sequence, envir = env)
}
on.exit({
    for (i in seq_along(patch_envs)) {
        original <- original_extracts[[i]]
        if (is.null(original)) {
            if (exists("extract_sequence_from_fasta", envir = patch_envs[[i]], inherits = FALSE)) {
                rm("extract_sequence_from_fasta", envir = patch_envs[[i]])
            }
        } else {
            assign("extract_sequence_from_fasta", original, envir = patch_envs[[i]])
        }
    }
}, add = TRUE)
invisible(create_gene_plot(
    df = df,
    df_gene = df_gene,
    df_transcript = df_tx,
    current_transcript_length = 101,
    length_difference = 0,
    composicion_secuencia = "Sequence Composition: N/A",
    gene_length_label = "Gene Length: 101 pb",
    transcript_length_label = "Transcript Length: 101 pb",
    visual_mode = "compact",
    genome_fasta_path = tmp_fasta,
    plot_id = "compact_gc_defer",
    plot_context = "test"
))

if (extract_calls != 1L) {
    stop(sprintf("compact plot should extract the genomic span once for feature GC; got %d call(s)", extract_calls))
}

cat("plot-compact-gc-present-ok\n")

# Compare the actual SVG and tooltip payload against the original substring GC
# path. Duplicate/overlapping exon and CDS ranges exercise reuse in both views.
df_compare <- df[c(1, 2, 1, 2), ]
df_compare$xstart <- c(110, 115, 160, 163)
df_compare$xend <- c(130, 128, 180, 175)
df_compare$largo <- df_compare$xend - df_compare$xstart + 1
original_index <- get("make_genomic_gc_index", envir = app_env)
normalize_svg <- function(widget) {
    svg <- widget$x$html
    stopifnot(is.character(svg), length(svg) == 1L, nzchar(svg))
    gsub("svg_[[:alnum:]]+", "svg_TEST", svg)
}
for (mode in c("compact", "detailed")) {
    for (sequence in c(paste(rep("ACgtNN-- \n", 30), collapse = ""), paste(rep("NRY-", 75), collapse = ""))) {
        render_comparison <- function() create_gene_plot(
            df = df_compare, df_gene = df_gene, df_transcript = df_tx,
            current_transcript_length = 101, length_difference = 0,
            composicion_secuencia = stop("The legacy composition argument must remain unused"),
            gene_length_label = "Gene Length: 101 pb", transcript_length_label = "Transcript Length: 101 pb",
            visual_mode = mode, genome_fasta_path = tmp_fasta,
            precomputed_genomic_span = sequence, plot_id = "gc_parity", plot_context = "test"
        )
        for (env in patch_envs) assign("make_genomic_gc_index", original_index, envir = env)
        candidate <- normalize_svg(render_comparison())
        for (env in patch_envs) assign("make_genomic_gc_index", function(...) NULL, envir = env)
        reference <- normalize_svg(render_comparison())
        stopifnot(identical(candidate, reference))
    }
}
for (env in patch_envs) assign("make_genomic_gc_index", original_index, envir = env)
cat("plot-gc-svg-tooltip-parity-ok compact+detailed overlapping+ambiguous\n")
