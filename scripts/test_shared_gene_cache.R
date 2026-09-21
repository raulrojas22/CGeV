#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(stringr); library(dplyr); library(purrr)})
source('R/utils.R')
source('R/shared_gene_cache.R')
# Evaluate only function definitions, never the Shiny server or session setup.
server_defs <- parse('server.R')[[1L]][[3L]][[3L]]
e <- new.env(parent = globalenv())
for (expr in as.list(server_defs)[-1L]) {
    if (is.call(expr) && identical(expr[[1L]], as.name('<-')) &&
        is.symbol(expr[[2L]]) && is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name('function'))) {
        assign(as.character(expr[[2L]]), eval(expr[[3L]], e), e)
    }
}
source('R/server_popup_status_domain.R')
e$format_org_name <- init_popup_status_domain(NULL)$format_org_name
# Accidental session reads must fail. Nothing from this environment is persisted.
makeActiveBinding('session', function(...) stop('session read'), e)
makeActiveBinding('input', function(...) stop('input read'), e)
root <- tempfile('shared-gene-tests-')
dir.create(root)
on.exit_cleanup <- function() unlink(root, recursive = TRUE)
Sys.setenv(CGV_CACHE_DIR = file.path(root, 'cache'))
assert <- function(x, label) { if (!isTRUE(x)) stop(label); cat('PASS:', label, '\n') }
fixture <- function(n=2L, gene='g', strand='+') {
    row <- function(type,start,end,attr) data.frame(V1='chr1', V2='fixture', V3=type,
        V4=start, V5=end, V6='.', V7=strand, V8='.', V9=attr)
    do.call(rbind,c(list(row('gene',1L,10000L,paste0('ID=',gene,';Name=',gene))),
        lapply(seq_len(n),function(i) rbind(
            row('mRNA',i,1000L+i,paste0('ID=',gene,'.t',i,';Parent=',gene, if(i==2) ';tag=MANE Select' else '')),
            row('exon',i,100L+i,paste0('Parent=',gene,'.t',i)),
            row('CDS',i,50L+i,paste0('Parent=',gene,'.t',i))))))
}
annotation <- file.path(root, 'annotation.gff')
writeLines('fixture annotation', annotation)
for (n in c(2L,60L)) for (strand in c('+','-')) {
    d <- fixture(n, strand=strand)
    original_blocks <- split_gene_data_by_transcript(d)
    expected <- list(blocks=original_blocks, canonical=e$compute_canonical_block_idx(original_blocks))
    calls <- 0L
    canonical <- function(b) { calls <<- calls+1L; e$compute_canonical_block_idx(b) }
    miss <- shared_gene_split(d,canonical,annotation,'Fixture')
    hit <- shared_gene_split(d,canonical,annotation,'Fixture')
    assert(identical(miss,expected) && identical(hit,expected) && calls==1L,
        paste('split/canonical original == MISS == HIT', n,strand))
    b <- miss$blocks
    expected_metrics <- e$build_transcript_metrics_payloads(do.call(rbind,b),b,'g','Fixture',annotation)
    calls <- 0L
    compute <- function() { calls <<- calls+1L; e$build_transcript_metrics_payloads(do.call(rbind,b),b,'g','Fixture',annotation) }
    get_metrics <- function(name='g',report='',map=FALSE) shared_gene_metrics(b,name,'Fixture',annotation,map,report,compute)
    mm <- get_metrics(); mh <- get_metrics()
    assert(identical(mm,expected_metrics) && identical(mh,expected_metrics) && calls==1L,
        paste('metrics original == MISS == HIT',n,strand))
    assert(shared_gene_plain(miss) && shared_gene_plain(mm), 'persisted types are plain data')
    assert(identical(unserialize(serialize(miss,NULL)),expected) &&
        identical(unserialize(serialize(mm,NULL)),expected_metrics), 'serialization exact parity')
    for (value in list(split=miss,metrics=mm)) {
        p <- tempfile(tmpdir=root); saveRDS(value,p,compress='gzip',version=2)
        cat('SIZE fixture=',n,' strand=',strand,' raw=',length(serialize(value,NULL,version=2)),
            ' gzip=',file.info(p)$size,'\n',sep='')
    }
    if(n==2L && strand=='+') {
        get_metrics(name='alias'); get_metrics(report='different-report'); get_metrics(map=TRUE)
        assert(calls==4L,'metrics parameter isolation')
    }
}
d <- fixture(); key <- shared_gene_cache_key('split',d,annotation)
assert(!is.null(key), 'cache key enabled')
assert(!identical(key,shared_gene_cache_key('split',fixture(gene='other'),annotation)), 'different genes isolated')
assert(!identical(key,shared_gene_cache_key('split',d,paste0(annotation,'.other'))), 'annotation path isolated')
oldtime <- file.info(annotation)$mtime
writeLines('different annotation content size',annotation)
assert(!identical(key,shared_gene_cache_key('split',d,annotation)), 'annotation metadata invalidation')
key <- shared_gene_cache_key('split',d,annotation)
d2 <- d; d2$V4[3] <- d2$V4[3]+1L
assert(!identical(key,shared_gene_cache_key('split',d2,annotation)), 'coordinate content invalidation')
key <- shared_gene_cache_key('split',d,annotation)
assert(!identical(key,shared_gene_cache_key('split',d,annotation,schema=2L)), 'schema isolation')
assert(!identical(key,shared_gene_cache_key('split',d,annotation,algorithm='next')), 'algorithm isolation')
assert(is.null(shared_gene_cache_key('split',list(server=function() NULL))), 'closure input bypass')
assert(!shared_gene_plain(list(x=new.env())) && !shared_gene_plain(list(x=quote(a+b))), 'environment/language rejected')
x <- d; attr(x,'hidden') <- new.env()
assert(!shared_gene_plain(x), 'unsafe nested attributes rejected')

for (kind in c('split','metrics')) {
    calls <- 0L
    compute <- function() {calls <<- calls+1L; list(science=c(1L,2L))}
    fetch <- function(...) shared_gene_cache_get(kind,key,compute,...)
    expected <- fetch(); assert(identical(fetch(),expected) && calls==1L,paste(kind,'generic hit'))
    path <- shared_gene_cache_path(kind,key)
    writeLines('corrupt',path)
    assert(identical(fetch(),expected) && calls==2L,paste(kind,'corrupt recalculation'))
    obj <- readRDS(path); obj$value <- list(science=99); saveRDS(obj,path)
    assert(identical(fetch(),expected) && calls==3L,paste(kind,'checksum mismatch recalculation'))
    obj <- readRDS(path); obj$schema <- 999L; saveRDS(obj,path)
    assert(identical(fetch(),expected) && calls==4L,paste(kind,'schema mismatch recalculation'))
    Sys.setFileTime(path,Sys.time()-8*86400)
    assert(identical(fetch(),expected) && calls==5L,paste(kind,'TTL recalculation'))
    unlink(path); dir.create(paste0(path,'.lock'))
    assert(identical(fetch(wait_seconds=0),expected) && calls==6L && !file.exists(path),paste(kind,'orphan lock bounded fail open, no write'))
    unlink(paste0(path,'.lock'),recursive=TRUE)
    dir.create(path)
    suppressWarnings(assert(identical(fetch(),expected),paste(kind,'rename failure preserves computed value')))
    unlink(path,recursive=TRUE)
    assert(!any(grepl('\\.part$|\\.lock$',list.files(dirname(path),all.files=TRUE))),paste(kind,'staging and lock cleanup'))
    tryCatch(shared_gene_cache_get(kind,key,function() stop('scientific failure')),error=function(e) NULL)
    assert(!dir.exists(paste0(path,'.lock')),paste(kind,'compute error releases lock'))
}
# Failure fallback remains session-local: do not persist a transient canonical error.
bad_calls <- 0L
bad_canonical <- function(b) { bad_calls <<- bad_calls+1L; stop('transient') }
bad_data <- fixture(gene='canonical-error')
bad_a <- shared_gene_split(bad_data,bad_canonical,annotation)
bad_b <- shared_gene_split(bad_data,bad_canonical,annotation)
assert(bad_calls==2L && identical(bad_a$canonical,1L) && identical(bad_a,bad_b),
    'canonical error fallback is never persisted')
# Content isolation even when file metadata is unchanged, for both products.
for (kind in c('split','metrics')) {
    original_key <- shared_gene_cache_key(kind,d,annotation)
    changed <- d; changed$V5[3L] <- changed$V5[3L]+1L
    assert(!identical(original_key,shared_gene_cache_key(kind,changed,annotation)),paste(kind,'same-file content isolation'))
    assert(!identical(original_key,shared_gene_cache_key(kind,d[nrow(d):1L,],annotation)),paste(kind,'row order isolation'))
    assert(!identical(original_key,shared_gene_cache_key(kind,d,annotation,parameters=list(organism='different'))),paste(kind,'organism isolation'))
}
# GTF/no-CDS and shared-parent cases exercise original semantics without edits.
variants <- list(no_cds=d[d$V3!='CDS',], no_transcript=d[d$V3!='mRNA',], shared_parent=d)
variants$shared_parent$V9[3] <- 'ID=shared;Parent=g.t1,g.t2'
variants$no_cds$V9[variants$no_cds$V3=='mRNA'] <- c('gene_id "g"; transcript_id "a";', 'gene_id "g"; transcript_id "b";')
for (name in names(variants)) {
    v <- variants[[name]]; original <- split_gene_data_by_transcript(v)
    expected <- list(blocks=original,canonical=e$compute_canonical_block_idx(original))
    miss <- shared_gene_split(v,e$compute_canonical_block_idx,annotation)
    hit <- shared_gene_split(v,e$compute_canonical_block_idx,annotation)
    assert(identical(miss,expected) && identical(hit,expected),paste(name,'split parity'))
    # Metrics may run after canonical-first reorder; order is part of its key.
    b <- rev(original)
    expected <- e$build_transcript_metrics_payloads(do.call(rbind,b),b)
    compute_variant <- function() e$build_transcript_metrics_payloads(do.call(rbind,b),b)
    miss <- shared_gene_metrics(b,'','',annotation,FALSE,'',compute_variant)
    hit <- shared_gene_metrics(b,'','',annotation,FALSE,'',compute_variant)
    assert(identical(miss,expected) && identical(hit,expected),paste(name,'reordered metrics parity'))
}
# Two OS processes, same key: the second re-checks after the first publishes.
if (.Platform$OS.type=='unix') {
    counter <- file.path(root,'counter')
    work <- function(i) shared_gene_cache_get('concurrency','same-key',function() {
        cat('compute\n',file=counter,append=TRUE); Sys.sleep(0.3); list(value=42L)
    })
    jobs <- lapply(1:2,function(i) parallel::mcparallel(work(i)))
    values <- parallel::mccollect(jobs)
    assert(all(vapply(values,identical,logical(1),list(value=42L))) && length(readLines(counter))==1L,
        'two processes: one compute, identical results')
    keys <- parallel::mclapply(1:2,function(i) shared_gene_cache_key('split',d,annotation),mc.cores=2L)
    assert(identical(keys[[1]],keys[[2]]) && identical(keys[[1]],key),'cross-process key stability')
}
# A successful orthologous prepass carries canonical along with its exact blocks.
prepared <- prepare_orthologous_transcript_splits_once(list(list(found=TRUE,data=d)),
    prepare_fun=function(result) shared_gene_split(result$data,e$compute_canonical_block_idx,annotation))[[1]]
assert(isTRUE(prepared$reusable) && identical(prepared$canonical,e$compute_canonical_block_idx(prepared$blocks)),
    'orthologous prepass canonical reuse')
# Actual metrics group boundary installs the same payload under new session IDs.
stored <- NULL
e$set_context_metrics_payloads <- function(...) stored <<- list(...)
first <- e$build_plot_metrics_group_payloads(plot_ids=c('session-a-1','session-a-2'),
    transcript_blocks=prepared$blocks,annotation_path=annotation)
second <- e$build_plot_metrics_group_payloads(plot_ids=c('session-b-1','session-b-2'),
    transcript_blocks=prepared$blocks,annotation_path=annotation)
assert(identical(first,second) && identical(stored$plot_ids,c('session-b-1','session-b-2')),
    'metrics group rebinds session IDs outside disk payload')
on.exit_cleanup()
cat('shared-gene-cache: PASS\n')
