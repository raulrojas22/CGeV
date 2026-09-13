#!/usr/bin/env Rscript
# Run from the app root. Args: baseline modules.R, output.csv, captured arguments.rds...
# Captures can be produced by scripts/test_gene_plot_model_reuse.R.
Sys.setenv(APP_PERF_TIMING = '0', APP_FUTURE_MODE = 'sequential', APP_GENE_PLOT_RENDERER_PREWARM = '0')
source('global.R')
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 3L)
inputs <- unlist(lapply(args[-c(1, 2)], readRDS), recursive = FALSE)
reference_env <- new.env(parent = environment(create_gene_plot))
sys.source(args[[1]], reference_env)
normalize_svg <- function(widget) gsub(widget$x$uid, 'SVG_UID', widget$x$html, fixed = TRUE)
compare <- function(a, order = 1:2) {
    a$caller_started_at <- NULL
    widgets <- vector('list', 2L)
    elapsed <- numeric(2L)
    for (k in order) {
        start <- proc.time()[['elapsed']]
        widgets[[k]] <- do.call(if (k == 1L) reference_env$create_gene_plot else create_gene_plot, a)
        elapsed[[k]] <- 1000 * (proc.time()[['elapsed']] - start)
    }
    stopifnot(identical(normalize_svg(widgets[[1]]), normalize_svg(widgets[[2]])),
              identical(widgets[[1]]$x$settings, widgets[[2]]$x$settings),
              identical(widgets[[1]]$x$ratio, widgets[[2]]$x$ratio),
              identical(widgets[[1]]$x$js, widgets[[2]]$x$js))
    elapsed
}
# Warm both constructors. Alternate order in timed rounds; include allocation/GC cost.
for (a in inputs) invisible(compare(a))
rows <- list()
for (round in 1:3) {
    for (i in seq_along(inputs)) {
        a <- inputs[[i]]
        ms <- compare(a, if (round %% 2) 2:1 else 1:2)
        rows[[length(rows) + 1L]] <- data.frame(round = round, input = i,
            gene = a$gene_display_name, mode = a$visual_mode,
            base_ms = ms[[1]], new_ms = ms[[2]])
    }
    cat('ROUND_OK', round, '\n')
}
r <- do.call(rbind, rows)
write.csv(r, args[[2]], row.names = FALSE)
print(aggregate(cbind(base_ms, new_ms) ~ gene + mode, r, median), row.names = FALSE)
print(aggregate(cbind(base_ms, new_ms) ~ round, r, sum), row.names = FALSE)
cat('SVG_PARITY_OK comparisons=', 4L * length(inputs), '\n')
# Additional parity checks: no context, larger shared scale, single feature,
# opposite strand, and different SVG size. No timings claimed for these variants.
count <- 0L
for (mode in c('compact', 'detailed')) for (orientation in c('genomic', 'transcription')) {
    for (variant in c('no_context', 'wide_scale', 'single_feature', 'positive_strand', 'svg_size')) {
        a <- inputs[[1]]
        a$visual_mode <- mode
        a$orientation_mode <- orientation
        if (variant == 'no_context') a$neighbor_context <- NULL
        if (variant == 'wide_scale') a[[5]] <- 20 * a[[4]]
        if (variant == 'single_feature') a[[1]] <- a[[1]][1, , drop = FALSE]
        if (variant == 'positive_strand') {
            a[[1]]$strand <- '+'; a[[2]]$V7 <- '+'; a[[3]]$V7 <- '+'
        }
        if (variant == 'svg_size') { a$width_svg <- 12; a$height_svg <- 2.5 }
        # These variants alter immutable inputs outside a module; derive their own key.
        a$model_cache_key <- make_gene_plot_model_data_key(a[[1]], a[[2]], a[[3]])
        invisible(compare(a))
        count <- count + 1L
    }
}
cat('SVG_VARIANTS_OK comparisons=', count, '\n')
