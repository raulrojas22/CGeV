#!/usr/bin/env Rscript
# Args: captured create_gene_plot arguments, pre-change modules.R, candidate SVG records.
Sys.setenv(APP_PERF_TIMING='0',APP_FUTURE_MODE='sequential',APP_GENE_PLOT_RENDERER_PREWARM='0')
source('global.R')
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==3L)
inputs <- readRDS(args[[1]])
records <- readRDS(args[[3]])
reference_env <- new.env(parent=environment(create_gene_plot))
sys.source(args[[2]],reference_env)
normalize_svg <- function(html) {
    uid <- xml2::xml_attr(xml2::read_xml(html),'id')
    gsub(uid,'SVG_UID',html,fixed=TRUE)
}
stopifnot(length(inputs)==length(records))
for(i in seq_along(inputs)) {
    a <- inputs[[i]]
    a$model_cache_key <- NULL
    reference <- do.call(reference_env$create_gene_plot,a)
    stopifnot(identical(normalize_svg(reference$x$html),normalize_svg(records[[i]]$html)))
}
cat('SVG_PARITY_OK renders=',length(inputs),'\n')
# Isolate model preparation/cache costs. These are NOT whole-card timings.
measure <- function(fn,n=1000L) {
    invisible(fn())
    median(replicate(3,{gc();start<-proc.time()[['elapsed']]
        for(i in seq_len(n)) invisible(fn())
        1000*(proc.time()[['elapsed']]-start)/n}))
}
seen <- character(0)
for (a in inputs) {
    label <- paste(a$gene_display_name,a$visual_mode)
    if(label %in% seen) next
    seen <- c(seen,label)
    data_key <- make_gene_plot_model_data_key(a[[1]],a[[2]],a[[3]])
    key <- make_gene_plot_model_cache_key(data_key,a$visual_mode,TRUE)
    prepare <- function() prepare_gene_plot_model(a[[1]],a[[2]],a[[3]],visual_mode=a$visual_mode)
    expected <- prepare()
    set_gene_plot_model_cache(key,expected)
    stopifnot(identical(get_gene_plot_model_cache(key),expected))
    hash_ms <- measure(function() make_gene_plot_model_data_key(a[[1]],a[[2]],a[[3]]))
    prepare_ms <- measure(prepare)
    reuse_ms <- measure(function() get_gene_plot_model_cache(make_gene_plot_model_cache_key(data_key,a$visual_mode,TRUE)))
    cat(sprintf('%s: hash_once_ms=%.4f prepare_ms=%.4f reuse_ms=%.4f\n',label,hash_ms,prepare_ms,reuse_ms))
}
