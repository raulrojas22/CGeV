#!/usr/bin/env Rscript
# Run from repository root. --baseline records the unsafe pre-fix behavior.
git_binary <- Sys.which("git")
source("global.R")
e <- as.environment("app_libraries")
if ("--baseline" %in% commandArgs(TRUE)) {
    old_file <- tempfile()
    base::system2(git_binary, c("show", "d0a31682096f198278a11c0684251d2b8d205e97:R/modules.R"), stdout = old_file)
    old <- readLines(old_file)
    unlink(old_file)
    for (expr in parse(text = old)) {
        if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
            as.character(expr[[2]])[1] %in% c("normalize_girafe_plot_signature", "make_girafe_plot_cache_key")) eval(expr, e)
    }
}
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


annotation <- tempfile(fileext = ".gff")
writeLines("fixture annotation", annotation)
on.exit(unlink(annotation), add = TRUE)
sig <- paste(annotation, "TP53", "tx1", "chr1", 100, 200, sep = "||")
args <- function(label, id = "1") list(
    df = df, df_gene = df_gene, df_transcript = df_tx,
    current_transcript_length = 61, length_difference = 0,
    composicion_secuencia = NULL, gene_length_label = "Gene Length: 101 pb",
    transcript_length_label = "Transcript Length: 61 pb",
    gene_display_name = label, plot_id = id, plot_context = "homologous",
    annotation_file_path = annotation, organism_label = "Human",
    neighbor_context = list(upstream = list(dist_bp = 20, neighbor_start = 50, neighbor_end = 80, neighbor_id = "other", neighbor_name = "OTHER", neighbor_strand = "+")), visual_mode = "compact", genome_fasta_path = "")
key <- function(a, signature = sig) {
    extra <- if ("render_inputs" %in% names(formals(e$make_girafe_plot_cache_key)))
        list(render_inputs = a) else list()
    do.call(e$make_girafe_plot_cache_key, c(list(plot_context = "homologous",
        plot_signature = signature, fallback_id = a$plot_id), extra))
}
a <- args("P53 -> TP53", "1")
b <- args("TP53", "2")
wa <- do.call(e$create_gene_plot, a)
wb <- do.call(e$create_gene_plot, b)
stopifnot(!identical(wa$x$html, wb$x$html),
    grepl("P53", wa$x$html, fixed = TRUE),
    !grepl("P53 -", wb$x$html, fixed = TRUE),
    grepl(utils::URLencode("P53 -> TP53", reserved = TRUE), wa$x$html, fixed = TRUE),
    !grepl(utils::URLencode("P53 -> TP53", reserved = TRUE), wb$x$html, fixed = TRUE),
    grepl("plot_id=1", wa$x$html, fixed = TRUE),
    grepl("plot_id=2", wb$x$html, fixed = TRUE))
e$set_shared_girafe_plot_cache(key(a), wa)
if ("--baseline" %in% commandArgs(TRUE)) {
    stopifnot(identical(key(a), key(b)),
        identical(e$get_shared_girafe_plot_cache(key(b)), wa))
    cat("BASELINE: collision confirmed; B receives A SVG and plot_id=1\n")
    quit(status = 0)
}
stopifnot(key(a) != key(b), key(a) != key(args("TP53", "1")),
    key(b) != key(args("TP53", "1")), is.null(e$get_shared_girafe_plot_cache(key(b))))
e$set_shared_girafe_plot_cache(key(b), wb)
stopifnot(identical(e$get_shared_girafe_plot_cache(key(b)), wb))
cat("PASS: display and plot identity isolated; identical inputs share real widgets\n")
old <- key(b)
writeLines("a different annotation size", annotation)
stopifnot(key(b) != old, is.null(e$get_shared_girafe_plot_cache(key(b))))
old <- key(b)
t <- file.info(annotation)$mtime
Sys.setFileTime(annotation, t + 0.125)
stopifnot(as.numeric(file.info(annotation)$mtime) != as.numeric(t), key(b) != old)
cat("PASS: size and fractional mtime independently invalidate\n")
n <- length(ls(e$.cgv_girafe_plot_cache))
for (i in 1:3) {
    k <- key(b, NULL)
    stopifnot(identical(k, ""), is.null(e$get_shared_girafe_plot_cache(k)),
        identical(e$set_shared_girafe_plot_cache(k, wb), FALSE))
}
stopifnot(length(ls(e$.cgv_girafe_plot_cache)) == n)
cat("PASS: repeated fallback bypasses global cache\n")
render_count <- 0L
render <- function(a) {
    k <- key(a)
    hit <- e$get_shared_girafe_plot_cache(k)
    if (!is.null(hit)) return(e$refresh_girafe_widget_uid(hit))
    render_count <<- render_count + 1L
    w <- do.call(e$create_gene_plot, a)
    e$set_shared_girafe_plot_cache(k, w)
    w
}
# Independent reactive domains in the same R process; actual Girafe SVGs.
s1 <- shiny::MockShinySession$new(); s2 <- shiny::MockShinySession$new()
pid <- Sys.getpid()
w1 <- shiny::withReactiveDomain(s1, render(a))
w2 <- shiny::withReactiveDomain(s2, render(b))
w3 <- shiny::withReactiveDomain(s2, render(a))
stopifnot(render_count == 2L, Sys.getpid() == pid,
    grepl("plot_id=1", w1$x$html, fixed = TRUE),
    grepl("plot_id=2", w2$x$html, fixed = TRUE),
    !grepl("P53 -", w2$x$html, fixed = TRUE))
s1$close()
stopifnot(!s2$isClosed())
invisible(shiny::withReactiveDomain(s2, render(b)))
s2$close()
cat("PASS: two mock Shiny sessions, same PID, correct SVGs; identical request avoids render\n")
key_ms <- system.time(for (i in 1:1000) key(b))[["elapsed"]]
render_ms <- 1000 * system.time(do.call(e$create_gene_plot, b))[["elapsed"]]
cat(sprintf("TIMING: key %.4f ms/call (1000 calls); render %.1f ms\n", key_ms, render_ms))

raw <- rbind(df_gene[, paste0('V', 1:9)], df_tx[, paste0('V', 1:9)],
    data.frame(V1='chr1',V2='test',V3=c('exon','CDS'),V4=c(110,150),V5=c(130,170),
        V6='.',V7='+',V8=c('.','0'),V9=c('ID=exon1;Parent=tx1','ID=cds1;Parent=tx1')))
run_module <- function(label, id, signature=sig, kind='homologous') {
    session <- shiny::MockShinySession$new()
    fn <- if(kind=='homologous') e$plotServerHomologous else e$plotServerOrtologous
    shiny::withReactiveDomain(session, fn('card', data=raw,
        max_gene_length=shiny::reactiveVal(101), min_gene_coord=shiny::reactiveVal(100),
        max_gene_coord=shiny::reactiveVal(200), genSequences=shiny::reactiveVal(list()),
        plotIndex=id, gene_name=label, annotation_file_path=annotation,
        genome_fasta_path='', prefetch_sequence=FALSE, precomputed_neighbor_context=a$neighbor_context,
        organism_name='Human', plot_signature=signature))
    session$flushReact()
    result <- session$getOutput('card-plot')
    list(session=session, result=result)
}
local({
    render_env <- environment(e$plotServerHomologous)
    stopifnot(identical(render_env, environment(e$plotServerOrtologous)))
    original <- render_env$create_gene_plot
    on.exit(assign("create_gene_plot", original, envir = render_env))
    builds <- 0L
    render_env$create_gene_plot <- function(...) {
        builds <<- builds + 1L
        original(...)
    }
    html <- function(m) jsonlite::fromJSON(m$result)$x$html
    for (kind in c("homologous", "orthologous")) {
        sessions <- list()
        launch <- function(...) {
            m <- run_module(..., kind = kind)
            sessions[[length(sessions) + 1L]] <<- m$session
            m
        }
        m1 <- launch("P53 -> TP53", "1")
        m2 <- launch("TP53", "2")
        stopifnot(grepl("P53 -", html(m1), fixed = TRUE),
                  !grepl("P53 -", html(m2), fixed = TRUE),
                  grepl("plot_id=1", html(m1), fixed = TRUE),
                  grepl("plot_id=2", html(m2), fixed = TRUE))
        same_id <- launch("TP53", "1")
        stopifnot(!grepl("P53 -", html(same_id), fixed = TRUE),
                  !grepl(utils::URLencode("P53 -> TP53", reserved = TRUE), html(m2), fixed = TRUE),
                  !grepl(utils::URLencode("P53 -> TP53", reserved = TRUE), html(same_id), fixed = TRUE))
        before <- builds
        m3 <- launch("TP53", "2")
        stopifnot(builds == before, !identical(html(m2), html(m3))) # UID refreshed
        n <- length(ls(e$.cgv_girafe_plot_cache))
        invisible(launch("TP53", "2", signature = NULL))
        invisible(launch("TP53", "2", signature = NULL))
        stopifnot(builds >= before + 2L, length(ls(e$.cgv_girafe_plot_cache)) == n)
        before <- builds
        Sys.setFileTime(annotation, file.info(annotation)$mtime + 0.125)
        invisible(launch("TP53", "2"))
        stopifnot(builds == before + 1L)
        m1$session$close()
        stopifnot(!m2$session$isClosed(), Sys.getpid() == pid)
        m2$session$flushReact()
        for (session in sessions) session$close()
        cat("PASS:", kind, "production module sessions: isolated payloads, true hit with fresh UID, fallback bypass, annotation invalidation\n")
    }
})

large_inputs <- b
large_inputs$precomputed_genomic_span <- strrep("ACGT", 25000)
large_key_ms <- 10 * system.time(for (i in 1:100) key(large_inputs))[["elapsed"]]
cat(sprintf("TIMING: key with 100 kb span %.4f ms/call (100 calls)\n", large_key_ms))
