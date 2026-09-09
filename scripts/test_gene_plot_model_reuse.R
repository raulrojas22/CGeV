#!/usr/bin/env Rscript
# Requires the preloaded human annotation/genome under CGV_DATA_ROOT.
Sys.setenv(APP_PERF_TIMING='1', APP_DEBUG_LOGS='0', APP_FUTURE_MODE='sequential', APP_GENE_PLOT_RENDERER_PREWARM='0', CGV_DATA_ROOT=Sys.getenv('CGV_DATA_ROOT',getwd()), CGV_CACHE_DIR=Sys.getenv('CGV_CACHE_DIR',file.path(getwd(),'cache')))
source('global.R')
args <- commandArgs(trailingOnly=TRUE)
server_fun <- source(if(length(args)) args[[1]] else 'server.R', local=TRUE)$value
app_env <- environment(plotServerHomologous)
original_create <- app_env$create_gene_plot
calls <- list()
plot_arguments <- list()
model_preparations <- 0L
original_prepare <- app_env$prepare_gene_plot_model
app_env$prepare_gene_plot_model <- function(...) {
    model_preparations <<- model_preparations + 1L
    original_prepare(...)
}
app_env$create_gene_plot <- function(...) {
    a <- list(...)
    calls[[length(calls)+1L]] <<- list(id=a$plot_id, scale=a[[4]]+a[[5]])
    plot_arguments[[length(plot_arguments)+1L]] <<- a
    before <- model_preparations
    obj <- original_create(...)
    calls[[length(calls)]]$model_preparations <<- model_preparations-before
    calls[[length(calls)]]$html <<- obj$x$html
    obj
}
on.exit(assign('create_gene_plot',original_create,envir=app_env),add=TRUE)
shiny::testServer(server_fun, {
    session$setInputs(navtabs='homologous', homo_data_mode='preloaded',
        homo_preloaded_species='homo_sapiens_gcf_000001405_40_grch38_p14_genomic',
        homo_visual_mode='compact', homo_sort_mode='load', app_theme='light', colorblind_mode=FALSE)
    for (i in 1:10) { later::run_now(0.1); session$flushReact() }
    cat('REGISTRY READY',nrow(preloadedRegistry()),'\n')
    batch_t0 <- proc.time()[['elapsed']]
    set_batch_search_state('homologous',2L)
    batchSearchQueue('AMY2A')
    pendingHomoSearchGene('AMY1A'); pendingHomoSearchOrigin('global_batch')
    session$setInputs(filter1='AMY1A',generate1=1)
    cat('AFTER FIRST ids=',paste(primaryPlotIdsHomologous(),collapse=','),' inserted=',paste(homoInsertedCardIds(),collapse=','),' remaining=',batchSearchRemaining(),'\n')
    # A complete first SVG must exist while another batch gene is pending.
    stopifnot(identical(homoInsertedCardIds(), '1'), batchSearchRemaining() == 1L,
              length(calls) == 1L, nzchar(calls[[1]]$html), calls[[1]]$scale == 9033)
    pendingHomoSearchGene('AMY2A'); pendingHomoSearchOrigin('global_batch')
    session$setInputs(filter1='AMY2A',generate1=2)
    session$flushReact()
    homoVisibleCount(2L); session$flushReact()
    cat('FINAL',paste(primaryPlotIdsHomologous(),collapse=','),'inserted=',paste(homoInsertedCardIds(),collapse=','),'scale=',max_gene_length_homo(),'\n')
    cat('BATCH_COMPUTE_SECONDS=',proc.time()[['elapsed']]-batch_t0,'\n')
    print(lapply(calls,function(x) x[c('id','scale')]))
    stopifnot(model_preparations == 2L)
    session$setInputs(app_theme='dark')
    stopifnot(model_preparations == 2L)
    session$setInputs(homo_orientation_pick='transcription',colorblind_mode=TRUE)
    stopifnot(model_preparations == 2L)
    session$setInputs(homo_visual_mode='detailed')
    stopifnot(model_preparations == 4L)
    session$setInputs(app_theme='light',colorblind_mode=FALSE)
    stopifnot(model_preparations == 4L)
    saveRDS(calls, if(length(args)>1) args[[2]] else file.path(tempdir(),'cgev-progressive-cards.rds'))
    stopifnot(length(primaryPlotIdsHomologous())==2L,length(homoInsertedCardIds())==2L, max_gene_length_homo()==9130)
})

saveRDS(plot_arguments, if(length(args)>2) args[[3]] else file.path(tempdir(),'cgev-model-real-arguments.rds'))
cat('MODEL_PREPARATIONS=',model_preparations,'\n')
stopifnot(identical(vapply(calls[1:3],function(x)x$model_preparations,integer(1)),c(1L,0L,1L)),model_preparations==4L)
