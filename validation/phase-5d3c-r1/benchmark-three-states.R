# Execute the committed 5D.3C harnesses. Preserve their workloads and sample
# sizes; replace only the timing reporter with five-batch median reporting.
# R1 alone explicitly threads its new context through the logical request.
sources<-c(baseline=tempfile(fileext='.R'),parent=tempfile(fileext='.R'),R1='R/utils.R')
refs<-c(baseline='31debadccd9505f5ca7683445806b5632d9e5b13',parent='ba5d9002d953502b97112c310bb44836aab808b0')
for(state in names(refs))stopifnot(system2('git',c('show',paste0(refs[[state]],':R/utils.R')),stdout=sources[[state]])==0L)
rows<-list();counts_rows<-list()
add_context<-function(x){
    if(!is.call(x))return(x)
    for(i in seq_along(x))if(!identical(x[[i]],quote(expr=)))x[[i]]<-add_context(x[[i]])
    if(is.symbol(x[[1L]]) && as.character(x[[1L]])%in%c('extract_sequence_from_fasta','extract_spliced_exon_sequence','get_transcript_composition_cached'))x[['.sequence_context']]<-as.name('ctx')
    x
}
wrap_request<-function(x){
    if(!is.call(x))return(x)
    if(identical(x[[1]],as.name('<-')) && identical(x[[2]],as.name('request'))){
        body<-add_context(x[[3]][[3]])
        x[[3]][[3]]<-substitute({with_sequence_file_identity(p,function(ctx) BODY)},list(BODY=body))
        return(x)
    }
    for(i in seq_along(x))if(!identical(x[[i]],quote(expr=)))x[[i]]<-wrap_request(x[[i]])
    x
}
run_harness<-function(state,harness,scoped=FALSE,count_only=FALSE){
    e<-new.env(parent=globalenv())
    e$source<-function(file,...)sys.source(sources[[state]],e)
    expressions<-parse(harness)
    for(expr in expressions){
        if(is.call(expr) && identical(expr[[1]],as.name('<-')) && identical(expr[[2]],as.name('bench'))){
            is_request<-grepl('request-benchmark',harness,fixed=TRUE)
            e$bench<-function(label,fn,n=if(is_request)500L else 2000L){
                if(Sys.getenv('CGV_R1_REQUIRED_ONLY')=='1' && !is_request &&
                   !label %in% c('interval-hit','splice-hit','composition-hit')) return(invisible(NULL))
                for(i in seq_len(if(is_request)10L else 20L))fn()
                if(count_only){
                    tracked<-c('sequence_file_identity','sequence_cache_validate','sequence_cache_finish','file.info','normalizePath')
                    counts<-setNames(integer(length(tracked)),tracked);originals<-list()
                    for(name in tracked)if(exists(name,e,inherits=TRUE)){
                        originals[[name]]<-get(name,e,inherits=TRUE)
                        e[[name]]<-local({nm<-name;old<-originals[[name]];function(...){counts[[nm]]<<-counts[[nm]]+1L;old(...)}})
                    }
                    fn()
                    for(name in names(originals))e[[name]]<-originals[[name]]
                    counts_rows[[length(counts_rows)+1L]]<<-data.frame(state=state,scoped=scoped,operation=label,t(counts),check.names=FALSE)
                    return(invisible(NULL))
                }
                samples<-vapply(1:5,function(j){gc();system.time(for(i in seq_len(n))fn())[['elapsed']]/n*1e6},numeric(1))
                rows[[length(rows)+1L]]<<-data.frame(state=state,operation=label,scoped=scoped,n=n,median_us=median(samples),min_us=min(samples),max_us=max(samples),samples_us=paste(samples,collapse=','))
                cat(state,label,'median_us',median(samples),'range',range(samples),'\n');flush.console()
            }
        }else eval(if(scoped)wrap_request(expr) else expr,e)
    }
}
for(state in strsplit(Sys.getenv('CGV_R1_BENCH_STATES',paste(names(sources),collapse=',')),',',fixed=TRUE)[[1L]]){
    run_harness(state,'validation/phase-5d3c/benchmark.R')
    run_harness(state,'validation/phase-5d3c/request-benchmark.R',scoped=state=='R1')
    run_harness(state,'validation/phase-5d3c/request-benchmark.R',scoped=state=='R1',count_only=TRUE)
    # Explicitly distinguish backend paths from an exact memo hit.
    for(fallback in c(FALSE,TRUE)){
        e<-new.env(parent=baseenv());sys.source(sources[[state]],e)
        if(fallback)e$requireNamespace<-function(package,...)if(package=='Rsamtools')FALSE else base::requireNamespace(package,...)
        p<-tempfile(fileext='.fa');writeLines(c('>chr1',strrep('ACGT',250)),p)
        fn<-function(){
            if(fallback)e$cache_env_drop(e$.seq_extract_cache,e$get_seq_extract_cache_key(p,'chr1',1,900))
            stopifnot(identical(e$extract_sequence_from_fasta(p,'chr1',1,900),strrep('ACGT',225)))
        }
        for(i in 1:20)fn()
        samples<-vapply(1:5,function(j){gc();system.time(for(i in 1:500)fn())[['elapsed']]/500*1e6},numeric(1))
        label<-if(fallback)'fallback-full-record-hit-exact-evicted' else 'indexed-interval-scan'
        rows[[length(rows)+1L]]<-data.frame(state=state,operation=label,scoped=FALSE,n=500L,median_us=median(samples),min_us=min(samples),max_us=max(samples),samples_us=paste(samples,collapse=','))
        cat(state,label,'median_us',median(samples),'\n');flush.console();unlink(c(p,paste0(p,'.fai')))
    }
}
# Public independent calls intentionally keep per-call validation.
run_harness('R1','validation/phase-5d3c/request-benchmark.R',count_only=TRUE)
write.table(do.call(rbind,rows),file.path('validation/phase-5d3c-r1',paste0(Sys.getenv('CGV_R1_PERF_PREFIX','performance'),'.tsv')),sep='\t',row.names=FALSE)
write.table(do.call(rbind,counts_rows),file.path('validation/phase-5d3c-r1',paste0(Sys.getenv('CGV_R1_COUNT_PREFIX','counts'),'.tsv')),sep='\t',row.names=FALSE)
