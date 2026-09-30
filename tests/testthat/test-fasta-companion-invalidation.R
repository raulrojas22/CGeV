companion_root <- normalizePath(if(file.exists('R/utils.R')) '.' else '../..')
companion_env <- function() {
    e <- new.env(parent=globalenv())
    sys.source(Sys.getenv('CGV_COMPANION_SOURCE',file.path(companion_root,'R/utils.R')),e)
    e
}
companion_fixture <- function(dir, tag, bases, width, header='chr1') {
    plain <- file.path(dir,paste0(tag,'.fa')); p <- paste0(plain,'.gz')
    starts <- seq.int(1L,nchar(bases),by=width)
    writeLines(c(paste0('>',header),substring(bases,starts,pmin(starts+width-1L,nchar(bases)))),plain)
    Rsamtools::bgzip(plain,dest=p,overwrite=TRUE); Rsamtools::indexFa(p);p
}
companion_data <- function() {
    set.seed(541L)
    list(a=paste(sample(c('A','C','G','T'),150000L,TRUE),collapse=''),
         b=paste(sample(c('A','C','G','T'),150000L,TRUE),collapse=''))
}
companion_publish <- function(src,dst,parts=c('','.fai','.gzi')) {
    for(s in parts){stopifnot(file.copy(paste0(src,s),paste0(dst,s,'.next'),overwrite=TRUE));stopifnot(file.rename(paste0(dst,s,'.next'),paste0(dst,s)))}
}
companion_oracle <- function(e,p,b) {
    ex<-data.frame(start=c(70001L,100001L),end=c(70010L,100010L))
    want<-paste0(substr(b,70001,70010),substr(b,100001,100010))
    for(i in 1:2){
        testthat::expect_identical(e$extract_sequence_from_fasta(p,'chr1',70001,70028),substr(b,70001,70028))
        testthat::expect_identical(e$extract_spliced_exon_sequence(p,'chr1',ex),want)
        counts<-setNames(vapply(c('A','T','C','G'),function(x)sum(strsplit(want,'')[[1]]==x),integer(1)),c('A','T','C','G'))
        testthat::expect_identical(e$get_transcript_composition_cached(p,'chr1',ex)$counts,counts)
    }
}
for(case in c('gzi-only','fai-only','neither','both','K','same-size','preserved-mtime')) local({
    mode<-case
    testthat::test_that(paste('compressed companion replacement',mode),{
        testthat::skip_if_not_installed('Rsamtools')
        d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE));x<-companion_data()
        a<-companion_fixture(d,'a',x$a,60L)
        # Same-source bytes gives equal compressed size while changing layout
        # via a same-width header; other cases differ in wrapping and blocks.
        b<-if(mode=='same-size') companion_fixture(d,'b',x$a,60L,'chr2') else companion_fixture(d,'b',x$b,73L)
        p<-file.path(d,'live.fa.gz');companion_publish(a,p);e<-companion_env()
        companion_oracle(e,p,x$a);old<-file.info(p)
        parts<-switch(mode,'gzi-only'=c('','.gzi'),'fai-only'=c('','.fai'),'neither'='',c('','.fai','.gzi'))
        companion_publish(b,p,parts)
        if(mode=='K') {file.copy(paste0(a,'.fai'),paste0(p,'.fai'),overwrite=TRUE);Sys.setFileTime(paste0(p,'.fai'),Sys.time()+1)}
        if(mode=='preserved-mtime') {Sys.setFileTime(p,old$mtime);testthat::expect_identical(file.info(p)$mtime,old$mtime)}
        e$sequence_cache_validate(p)
        testthat::expect_identical(e$sequence_fasta_index_usable(normalizePath(p)),mode%in%c('both','same-size'))
        if(mode=='same-size') {
            testthat::expect_equal(file.info(p)$size,old$size)
            testthat::expect_identical(e$extract_sequence_from_fasta(p,'chr2',70001,70028),substr(x$a,70001,70028))
        } else companion_oracle(e,p,x$b)
        if(mode=='K') {fresh<-companion_env();companion_oracle(fresh,p,x$b);testthat::expect_false(fresh$sequence_fasta_index_usable(normalizePath(p)))}
    })
})

testthat::test_that('companion ages and changes are independent without optional dependencies',{
    for(which in c(2L,3L)) {
        e<-companion_env();id<-list(path='/synthetic/genome.fa.gz',size=c(100,20,30),mtime=c(10,11,11),ctime=c(10,11,11),valid=TRUE)
        e$sequence_file_identity<-function(path)id
        probes<-0L;e$sequence_fasta_companions_compatible<-function(...) {probes<<-probes+1L;TRUE}
        e$sequence_cache_validate(id$path)
        testthat::expect_true(e$sequence_fasta_index_usable(id$path))
        id$mtime[1]<-id$ctime[1]<-12
        other<-if(which==2L)3L else 2L
        id$mtime[other]<-id$ctime[other]<-13
        e$sequence_cache_validate(id$path)
        testthat::expect_false(e$sequence_fasta_index_usable(id$path))
        testthat::expect_identical(probes,1L)
        # Changed source with an unchanged companion whose timestamps are
        # in the future must still be rejected independently of age.
        e<-companion_env();id$mtime[]<-id$ctime[]<-c(10,20,20)
        e$sequence_file_identity<-function(path)id
        e$sequence_fasta_companions_compatible<-function(...)TRUE
        e$sequence_cache_validate(id$path);id$mtime[1]<-id$ctime[1]<-11
        id$mtime[other]<-id$ctime[other]<-21
        e$sequence_cache_validate(id$path)
        testthat::expect_false(e$sequence_fasta_index_usable(id$path))
    }
})

testthat::test_that('compatibility result is bounded per identity and fails closed',{
    e<-companion_env();id<-list(path='/synthetic/genome.fa.gz',size=c(100,20,30),mtime=c(10,11,11),ctime=c(10,11,11),valid=TRUE)
    e$sequence_file_identity<-function(path)id;probes<-0L
    e$sequence_fasta_companions_compatible<-function(...) {probes<<-probes+1L;FALSE}
    for(i in 1:20)e$sequence_cache_validate(id$path)
    testthat::expect_identical(probes,1L)
    testthat::expect_false(e$sequence_fasta_index_usable(id$path))
    testthat::expect_length(ls(e$.sequence_file_state),1L)
    id$ctime[2]<-12;e$sequence_cache_validate(id$path)
    testthat::expect_identical(probes,2L)
})

testthat::test_that('fresh-metadata stale gzi and later-record fai mismatch fail closed',{
    testthat::skip_if_not_installed('Rsamtools')
    d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE));x<-companion_data()
    a<-companion_fixture(d,'a',x$a,60L);b<-companion_fixture(d,'b',x$b,73L)
    p<-file.path(d,'live.fa.gz');companion_publish(b,p)
    file.copy(paste0(a,'.gzi'),paste0(p,'.gzi'),overwrite=TRUE)
    Sys.setFileTime(paste0(p,'.gzi'),Sys.time()+1)
    e<-companion_env();companion_oracle(e,p,x$b)
    testthat::expect_false(e$sequence_fasta_index_usable(normalizePath(p)))
    # First record is identical. A first-record-only layout check would miss
    # this modified later-record offset.
    plain<-file.path(d,'multi.fa');writeLines(c('>first','ACGT','>chr1',x$b),plain)
    Rsamtools::bgzip(plain,dest=p,overwrite=TRUE);unlink(paste0(p,c('.fai','.gzi')));Rsamtools::indexFa(p)
    fai<-readLines(paste0(p,'.fai'));fields<-strsplit(fai[2],'\t')[[1]]
    fields[3]<-as.character(as.numeric(fields[3])+2L);fai[2]<-paste(fields,collapse='\t');writeLines(fai,paste0(p,'.fai'))
    e<-companion_env();companion_oracle(e,p,x$b)
    testthat::expect_false(e$sequence_fasta_index_usable(normalizePath(p)))
})

testthat::test_that('failed indexed handle attempt proceeds to safe streaming',{
    testthat::skip_if_not_installed('Rsamtools')
    d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE));x<-companion_data()
    p<-companion_fixture(d,'b',x$b,73L);e<-companion_env()
    e$get_cached_fafile<-function(...)NULL
    companion_oracle(e,p,x$b)
})

testthat::test_that('same persistent worker rejects asymmetric and copied indexes',{
    testthat::skip_if_not_installed('Rsamtools')
    testthat::skip_if(Sys.getenv('CGV_SEQUENCE_WORKER_TEST')!='1','requires local worker sockets')
    future::plan(future::multisession,workers=I(1));on.exit(future::plan(future::sequential),add=TRUE)
    d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE),add=TRUE);x<-companion_data()
    a<-companion_fixture(d,'a',x$a,60L);b<-companion_fixture(d,'b',x$b,73L);p<-file.path(d,'live.fa.gz');pids<-integer()
    for(i in 1:4){
        src<-if(i%%2L)a else b;companion_publish(src,p)
        if(i==2L) {file.copy(paste0(a,'.gzi'),paste0(p,'.gzi'),overwrite=TRUE);Sys.setFileTime(paste0(p,'.gzi'),Sys.time()+1)}
        if(i==4L) {file.copy(paste0(a,'.fai'),paste0(p,'.fai'),overwrite=TRUE);Sys.setFileTime(paste0(p,'.fai'),Sys.time()+1)}
        result<-future::value(future::future({
            e<-getOption('cgev.companion.worker')
            if(is.null(e)){e<-new.env(parent=baseenv());sys.source(runtime,e);options(cgev.companion.worker=e)}
            ex<-data.frame(start=c(70001L,100001L),end=c(70010L,100010L))
            list(pid=Sys.getpid(),sequence=e$extract_sequence_from_fasta(p,'chr1',70001,70028),
                 splice=e$extract_spliced_exon_sequence(p,'chr1',ex),comp=e$get_transcript_composition_cached(p,'chr1',ex)$counts)
        },globals=list(p=p,runtime=file.path(companion_root,'R/utils.R'))))
        bases<-if(i%%2L)x$a else x$b;want<-paste0(substr(bases,70001,70010),substr(bases,100001,100010))
        testthat::expect_identical(result$sequence,substr(bases,70001,70028))
        testthat::expect_identical(result$splice,want)
        testthat::expect_identical(result$comp,setNames(vapply(c('A','T','C','G'),function(z)sum(strsplit(want,'')[[1]]==z),integer(1)),c('A','T','C','G')))
        pids<-c(pids,result$pid)
    }
    testthat::expect_length(unique(pids),1L);cat('\nCompanion worker PID:',unique(pids),'\n')
})
