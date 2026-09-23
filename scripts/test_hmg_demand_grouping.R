#!/usr/bin/env Rscript
# Production-expression regression; no app startup, workers or external datasets.
library(shiny)
source('R/utils.R')
find_assignment <- function(tree, name) {
    if (missing(tree)) return(NULL)
    if (is.call(tree) && identical(tree[[1]], as.name('<-')) &&
        identical(tree[[2]], as.name(name))) return(tree)
    if (is.recursive(tree)) for (item in as.list(tree)) {
        found <- find_assignment(item, name)
        if (!is.null(found)) return(found)
    }
    NULL
}
base_sha <- 'a5ec791f9270de0b4f2ef202c98d473d6f744a3e'
baseline <- parse(text=system2('git', c('show', paste0(base_sha, ':server.R')), stdout=TRUE))
current <- parse('server.R')
base_hmg <- find_assignment(baseline, 'homoMultiTranscriptGeneGroups')
new_hmg <- find_assignment(current, 'homoMultiTranscriptGeneGroups')
compute <- find_assignment(current, 'computeHomoMultiTranscriptGeneGroups')
# The entire calculation from norm_key_local to the final groups is unchanged.
base_body <- as.list(base_hmg[[3]][[2]])
new_body <- as.list(compute[[3]][[3]])
start_at <- function(x) which(vapply(x, function(e) is.call(e) &&
    identical(e[[1]], as.name('<-')) && identical(e[[2]], as.name('norm_key_local')), logical(1)))[1]
stopifnot(identical(base_body[start_at(base_body):length(base_body)],
                    new_body[start_at(new_body):length(new_body)]))
fixture <- function(gene='TP53', n=26L, org='Human', ann='/human.gff') {
    ids <- as.character(seq_len(n))
    list(ids=ids,
         meta=setNames(lapply(seq_len(n), function(i) list(total_transcripts=n,
             query_gene=gene, display_gene_name=gene, matched_gene_name=gene,
             matched_gene_id=paste0('gene-',gene), is_canonical=i==1L)),ids),
         org=setNames(rep(list(list(name=org)),n),ids),
         ann=setNames(as.list(rep(ann,n)),ids),
         titles=setNames(as.list(sprintf('Gene: %s | Transcript: tx%s',gene,ids)),ids))
}
results <- list()
run_case <- function(initial, label) shiny::testServer(function(input, output, session) {
    ids_rv <- reactiveVal(initial$ids)
    plotGeneMetaHomologous <- reactiveVal(initial$meta)
    organismInfoHomologous <- reactiveVal(initial$org)
    annotationPathsHomologous <- reactiveVal(initial$ann)
    titlesHomologous <- reactiveVal(initial$titles)
    sortedPlotIdsHomologous <- reactive({ input$sort_roundtrip; ids_rv() })
    for (name in c('extract_title_field','canonical_gene_group_key','resolve_isoform_expand_request'))
        eval(find_assignment(current, name))
    baseline_stats <- new.env(); baseline_stats$calls <- 0L; baseline_stats$ms <- 0
    instrumented_base <- base_hmg
    instrumented_base[[3]][[2]] <- as.call(c(list(as.name('{')),
        quote(baseline_stats$calls <- baseline_stats$calls+1L),
        quote(.t0 <- proc.time()[['elapsed']]),
        quote(on.exit(baseline_stats$ms <- baseline_stats$ms+1000*(proc.time()[['elapsed']]-.t0))),
        as.list(base_hmg[[3]][[2]])[-1]))
    eval(instrumented_base)
    oracle <- homoMultiTranscriptGeneGroups
    eval(compute)
    stats <- new.env(); stats$calls <- 0L; stats$ms <- 0
    original_compute <- computeHomoMultiTranscriptGeneGroups
    computeHomoMultiTranscriptGeneGroups <- function(...) {
        stats$calls <- stats$calls+1L
        t0 <- proc.time()[['elapsed']]
        on.exit(stats$ms <- stats$ms+1000*(proc.time()[['elapsed']]-t0))
        original_compute(...)
    }
    eval(new_hmg)
    for (name in c('homoAlignedSelectedGroupKey','homoAlignedPlotIds'))
        eval(find_assignment(current,name))
    apply_state <- function(x) {
        ids_rv(x$ids); plotGeneMetaHomologous(x$meta); organismInfoHomologous(x$org)
        annotationPathsHomologous(x$ann); titlesHomologous(x$titles)
    }
    check <- function() {
        actual <- isolate(homoMultiTranscriptGeneGroups())
        stopifnot(identical(actual,isolate(oracle())))
        actual
    }
}, {
    # Nothing computes until a real reader asks, even across invalidation/flushes.
    session$flushReact(); stopifnot(stats$calls==0L)
    groups <- check(); stopifnot(stats$calls==1L, length(groups)==1L)
    for (i in 1:4) check()
    stopifnot(stats$calls==1L)
    # Canonical-copy writes + equivalent sort round trips invalidate baseline.
    for (i in 1:3) {
        for (rv in list(plotGeneMetaHomologous,organismInfoHomologous,
                        annotationPathsHomologous,titlesHomologous)) {
            x <- isolate(rv()); x[['1_c']] <- x[['1']]; x[[paste0('unused',i)]] <- i; rv(x)
        }
        session$setInputs(sort_roundtrip=i); check()
    }
    stopifnot(stats$calls==1L,baseline_stats$calls==4L)
    baseline_stable_ms <- baseline_stats$ms
    # Actual production expansion resolution: order, canonical anchor, copy last.
    m <- isolate(plotGeneMetaHomologous()); m[['1_c']]$is_canonical_copy <- TRUE
    plotGeneMetaHomologous(m)
    request <- resolve_isoform_expand_request(list(context='homologous',ids='1'))
    stopifnot(identical(request$anchor_id,'1'),
              identical(request$expanded_ids,c(initial$ids[-1],'1_c')),
              stats$calls==1L)
    # Alignment selection consumes the same ordered IDs; no extra calculation.
    stopifnot(identical(isolate(homoAlignedSelectedGroupKey()),names(groups)[[1]]),
              identical(isolate(homoAlignedPlotIds()),groups[[1]]$ids),stats$calls==1L)
    stable_ms <- stats$ms
    # Same-gene repeat preserves identity. Gene change, organism/path and labels invalidate.
    apply_state(initial); check(); stopifnot(stats$calls==1L)
    next_state <- fixture('BRCA1',368L); apply_state(next_state); check()
    before <- stats$calls
    x <- isolate(organismInfoHomologous()); x[['1']]$name <- 'Mouse'; organismInfoHomologous(x)
    check(); stopifnot(stats$calls==before+1L)
    x <- isolate(annotationPathsHomologous()); x[['1']] <- '/mouse.gff'; annotationPathsHomologous(x)
    before <- stats$calls; check(); stopifnot(stats$calls==before+1L)
    x <- isolate(titlesHomologous()); x[['1']] <- 'Gene: Changed | Transcript: other'; titlesHomologous(x)
    before <- stats$calls; check(); stopifnot(stats$calls==before+1L)
    x <- isolate(plotGeneMetaHomologous()); x[['1']]$display_gene_name <- 'Changed'; plotGeneMetaHomologous(x)
    before <- stats$calls; check(); stopifnot(stats$calls==before+1L)
    # Multiple genes/organisms, singleton exclusion, exact sorted group order.
    a <- fixture('TP53',26L); b <- fixture('BRCA1',3L,org='Mouse',ann='/mouse.gff')
    b$ids <- paste0('b',b$ids)
    for (field in c('meta','org','ann','titles')) names(b[[field]]) <- b$ids
    mixed <- lapply(names(a),function(field) c(a[[field]],b[[field]])); names(mixed) <- names(a)
    apply_state(mixed); mixed_groups <- check(); stopifnot(length(mixed_groups)==2L)
    apply_state(next_state)
    ids_rv(rev(next_state$ids)); check()
    ids_rv(next_state$ids[-1]); check() # card removal
    ids_rv(next_state$ids); check() # recreation
    ids_rv(character()); stopifnot(length(check())==0L)
    # A -> B -> C before demand must only compute C; no scheduled stale work.
    before <- stats$calls
    apply_state(fixture('TP53')); apply_state(fixture('BRCA1',368L)); apply_state(fixture('FINAL',3L))
    session$flushReact(); stopifnot(stats$calls==before)
    final <- check(); stopifnot(stats$calls==before+1L, final[[1]]$gene_label=='FINAL')
    # No-Synteny state must clear both the selected key and alignment IDs.
    apply_state(fixture('SINGLE',1L)); stopifnot(length(check())==0L,
        identical(isolate(homoAlignedSelectedGroupKey()),''),
        identical(isolate(homoAlignedPlotIds()),character()))
    results[[label]] <<- list(before_calls=4L, after_calls=1L, before_ms=baseline_stable_ms, after_ms=stable_ms, assertions='passed')
    session$close()
})
run_case(fixture(), 'TP53-shaped')
run_case(fixture('BRCA1',368L), 'BRCA1-shaped')
# Separate sessions, even for identical plot IDs: each must compute its own result.
run_case(fixture('SECOND_SESSION',26L,org='Mouse',ann='/mouse.gff'), 'session-isolation')
fixture_dir <- Sys.getenv('CGEV_PHASE4A_FIXTURES','')
if (nzchar(fixture_dir)) for (gene in c('TP53','BRCA1')) {
    path <- file.path(fixture_dir,paste0(gene,'-state.rds'))
    if (file.exists(path)) run_case(readRDS(path),paste0(gene,'-real'))
}
print(results)
cat('PASS: unchanged scientific body; exact groups/order/IDs/labels; lazy demand; stable count; invalidation; expansion; session isolation/closure.\n')
