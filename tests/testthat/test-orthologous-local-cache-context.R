library(testthat)
root <- if (file.exists('R/utils.R')) '.' else '../..'

test_that('shared orthologous cache agrees with an uncached context oracle', {
    e <- new.env(parent=globalenv()); sys.source(file.path(root,'R/utils.R'),e)
    annotation <- tempfile(fileext='.gff'); other <- tempfile(fileext='.gff')
    writeLines('human annotation',annotation); writeLines('mouse annotation',other)
    on.exit(unlink(c(annotation,other)),add=TRUE)
    calls <- 0L
    # Deliberately return distinguishable science for label/detection/gene/file.
    # The worker wrapper and actual cache operations are not mocked.
    e$detect_organism_from_gff <- function(file,label) list(organism=label)
    e$run_lookup_pipeline_pure <- function(file_path,input_gene,det_info,file_label,...) {
        calls <<- calls+1L
        list(data=data.frame(gene=input_gene, species=det_info$organism,
                            label=file_label, annotation=readLines(file_path)),
             det_resolved=det_info,matched_gene_id=input_gene)
    }
    e$compute_neighbor_context_from_plot_data <- function(...) list(neighbor='oracle')
    job <- list(file_idx=1L,file_path=annotation,file_label='Homo sapiens',
                gene_name='TP53',forced_genome='human.fa',
                enabled_external_sources=character(),allow_partial_suggestions=FALSE)
    lookup <- e$run_orthologous_lookup_job_pure
    first <- lookup(job); expect_identical(calls,1L)
    expect_identical(lookup(job),first); expect_identical(calls,1L)
    moved <- job; moved$file_idx <- 7L
    got <- lookup(moved); expect_identical(got$file_idx,7L)
    expect_identical(got$data,first$data); expect_identical(calls,1L)
    variants <- list(
        modifyList(job,list(forced_genome='mouse.fa')),
        modifyList(job,list(file_label='Mus musculus')),
        modifyList(job,list(det=list(organism='Mus musculus',taxid='10090'))),
        modifyList(job,list(file_path=other)),
        modifyList(job,list(gene_name='TP-53')),
        modifyList(job,list(gene_name='tp53')),
        modifyList(job,list(file_label='human|TP53',forced_genome='a|b.fa')))
    for (v in variants) {
        before <- calls; actual <- lookup(v)
        expect_identical(calls,before+1L)
        expect_identical(lookup(v),actual); expect_identical(calls,before+1L)
        # Clear only this private test cache to obtain a fresh oracle result.
        rm(list=ls(e$.orthologous_local_lookup_cache,all.names=TRUE),envir=e$.orthologous_local_lookup_cache)
        expect_identical(lookup(v),actual)
        lookup(job)
    }
    before <- calls
    writeLines('changed annotation size',annotation)
    expect_identical(lookup(job)$data$annotation,'changed annotation size')
    expect_identical(calls,before+1L)
    before <- calls
    old_time <- file.info(annotation)$mtime
    writeLines('changed annotation SIZE',annotation) # equal size, changed version
    Sys.setFileTime(annotation,old_time+2)
    expect_identical(lookup(job)$data$annotation,'changed annotation SIZE')
    expect_identical(calls,before+1L)
    # Fractional mtime must not be rounded away by the local lookup key.
    before <- calls; Sys.setFileTime(annotation,old_time+2.25)
    lookup(job); expect_identical(calls,before+1L)
})
