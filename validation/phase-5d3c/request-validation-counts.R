# Count one request after three warmups; no timings or implementation changes.
for (spec in list(list(name='TP53-like',span=20000L,n=11L),
                  list(name='BRCA1-like',span=80000L,n=23L))) {
    e<-new.env(parent=baseenv());sys.source('R/utils.R',e)
    p<-tempfile(fileext='.fa');writeLines(c('>chr1',strrep('ACGT',spec$span/4)),p)
    starts<-as.integer(seq(1,spec$span-100,length.out=spec$n))
    ex<-data.frame(start=starts,end=starts+99L)
    request<-function(){
        e$extract_sequence_from_fasta(p,'chr1',1,spec$span)
        e$extract_spliced_exon_sequence(p,'chr1',ex)
        e$get_transcript_composition_cached(p,'chr1',ex)
        for(i in seq_len(nrow(ex))) e$extract_sequence_from_fasta(p,'chr1',ex$start[i],ex$end[i])
        invisible(NULL)
    }
    for(i in 1:3)request()
    counts<-new.env(parent=emptyenv())
    tracked<-c('sequence_file_identity','sequence_cache_validate','sequence_cache_finish','get_cached_fafile','file.info')
    for(n in tracked) {
        counts[[n]]<-0L
        old<-get(n,envir=e,inherits=TRUE)
        e[[n]]<-local({name<-n;fn<-old;function(...){counts[[name]]<-counts[[name]]+1L;fn(...)}})
    }
    request()
    cat(spec$name, paste(vapply(tracked,function(n)paste0(n,'=',counts[[n]]),character(1)),collapse=' '),'\n')
    cat('full-span memo present:',exists(e$get_seq_extract_cache_key(p,'chr1',1,spec$span),e$.seq_extract_cache,inherits=FALSE),'\n')
    unlink(c(p,paste0(p,'.fai')))
}
