e<-new.env(parent=baseenv());sys.source('R/utils.R',e)
e$requireNamespace<-function(package,...)if(package=='Rsamtools')FALSE else base::requireNamespace(package,...)
p<-tempfile();ex<-data.frame(start=c(1L,9L),end=c(4L,12L))
caches<-c('.seq_extract_cache','.spliced_seq_cache','.fasta_fallback_seq_cache','.transcript_composition_cache','.sequence_file_state')
run<-function()e$with_sequence_file_identity(p,function(ctx){
    e$extract_spliced_exon_sequence(p,'chr1',ex,.sequence_context=ctx)
    e$get_transcript_composition_cached(p,'chr1',ex,.sequence_context=ctx)
    stopifnot(ctx$active);ctx
})
contains_env<-function(x){if(is.environment(x))return(TRUE);if(is.list(x))return(any(vapply(x,contains_env,logical(1))));FALSE}
for(cycle in 1:12){
    bases<-if(cycle%%2L)'AAAATTTTAAAATTTT' else 'CCCCGGGGCCCCGGGG'
    q<-paste0(p,'.next');writeLines(c('>chr1',bases),q);stopifnot(file.rename(q,p))
    for(i in 1:100){ctx<-run();stopifnot(!ctx$active,is.null(ctx$owner),is.null(ctx$runtime))}
    vals<-lapply(caches,function(n)as.list(e[[n]],all.names=TRUE))
    stopifnot(!any(vapply(vals,contains_env,logical(1))))
    cat(cycle,'counts',paste(vapply(caches,function(n)length(e$cache_env_entry_keys(e[[n]])),integer(1)),collapse=','),
        'bytes',sum(vapply(vals,function(v)sum(vapply(v,function(x)as.numeric(object.size(x)),numeric(1))),numeric(1))),
        'live contexts stored',0,'\n')
}
unlink(p)
