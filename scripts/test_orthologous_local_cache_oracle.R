#!/usr/bin/env Rscript
# Integration oracle: run with read-only preloaded annotations and a private cache.
Sys.setenv(APP_FUTURE_MODE = 'sequential', APP_GENE_PLOT_RENDERER_PREWARM = '0')
source('global.R')
reg <- get_preloaded_species_registry()
human <- reg[grepl('^homo_sapiens_', reg$species_id), , drop=FALSE]
mouse <- reg[grepl('^mus_musculus_', reg$species_id), , drop=FALSE]
stopifnot(nrow(human)==1L, nrow(mouse)==1L,
          file.exists(human$annotation_path), file.exists(mouse$annotation_path))
cache <- get('.orthologous_local_lookup_cache', envir=environment(run_orthologous_lookup_job_pure))
clear <- function() rm(list=ls(cache,all.names=TRUE),envir=cache)
science <- function(x) {
    x$lookup$lookup_elapsed_ms <- NULL
    x
}
job <- list(file_idx=1L, file_path=human$annotation_path,
            file_label='Homo sapiens', gene_name='TP53',
            forced_genome=human$genome_path, enabled_external_sources=character(),
            allow_partial_suggestions=FALSE)
lookup <- run_orthologous_lookup_job_pure
clear(); first <- lookup(job)
stopifnot(isTRUE(first$found), identical(lookup(job), first))
variants <- list(
    modifyList(job,list(forced_genome=mouse$genome_path)),
    modifyList(job,list(file_label='Mus musculus')),
    modifyList(job,list(det=list(organism='Mus musculus',taxid=10090L))),
    modifyList(job,list(file_path=mouse$annotation_path,file_label='Mus musculus',
                       forced_genome=mouse$genome_path,gene_name='Trp53')))
for(i in seq_along(variants)) {
    clear(); lookup(job)
    actual <- lookup(variants[[i]])
    stopifnot(identical(lookup(variants[[i]]),actual),
              identical(actual$forced_genome,variants[[i]]$forced_genome),
              identical(actual$file_label,variants[[i]]$file_label))
    clear(); oracle <- lookup(variants[[i]])
    stopifnot(identical(science(actual),science(oracle)),isTRUE(actual$found))
    cat('real annotation cache oracle',i,'PASS rows=',nrow(actual$data),'\n')
}
clear()
cat('orthologous-local-cache-real-oracle-ok\n')
