r1_root <- normalizePath(if(file.exists('R/utils.R')) '.' else '../..')
r1_env <- function(fallback=TRUE) {
    e<-new.env(parent=baseenv())
    sys.source(Sys.getenv('CGV_R1_TEST_SOURCE',file.path(r1_root,'R/utils.R')),e)
    if(fallback) e$requireNamespace<-function(package,...) if(package=='Rsamtools') FALSE else base::requireNamespace(package,...)
    if(Sys.getenv('CGV_R1_MUTANT')=='1') {
        e$sequence_cache_validate<-function(path) e$sequence_file_identity(path)
        e$sequence_cache_finish<-function(identity) invisible(NULL)
        e$sequence_fasta_index_usable<-function(path) TRUE
    }
    e
}
r1_a<-'AAAATTTTAAAATTTT';r1_b<-'CCCCGGGGCCCCGGGG'
r1_ex<-data.frame(start=c(1L,9L),end=c(4L,12L))
r1_put<-function(p,bases){q<-paste0(p,'.next');writeLines(c('>chr1',bases),q);stopifnot(file.rename(q,p))}
r1_cache_count<-function(e)sum(vapply(c('.seq_extract_cache','.spliced_seq_cache','.fasta_fallback_seq_cache','.transcript_composition_cache'),function(n)length(e$cache_env_entry_keys(e[[n]])),integer(1)))
r1_next<-function(e,p){
    testthat::expect_identical(e$extract_sequence_from_fasta(p,'chr1',1,16),r1_b)
    testthat::expect_identical(e$extract_spliced_exon_sequence(p,'chr1',r1_ex),'CCCCCCCC')
    testthat::expect_identical(e$get_transcript_composition_cached(p,'chr1',r1_ex)$counts,c(A=0L,T=0L,C=8L,G=0L))
}

testthat::test_that('snapshot then replacement purges old warm values on outer exit',{
    e<-r1_env();p<-tempfile();on.exit(unlink(p));r1_put(p,r1_a)
    e$extract_spliced_exon_sequence(p,'chr1',r1_ex)
    during<-NULL
    result<-e$with_sequence_file_identity(p,function(ctx){
        r1_put(p,r1_b)
        value<-e$extract_spliced_exon_sequence(p,'chr1',r1_ex,.sequence_context=ctx)
        during<<-r1_cache_count(e);value
    })
    testthat::expect_identical(result,'AAAAAAAA')
    testthat::expect_gt(during,0L)
    testthat::expect_identical(r1_cache_count(e),0L)
    r1_next(e,p)
})

testthat::test_that('replacement between exon reads may mix in-flight bases but retains no generation',{
    e<-r1_env();e$annotation_memory_cache_limits$fasta_fallback_seq_max_bp<-1L
    p<-tempfile();on.exit(unlink(p));r1_put(p,r1_a);during<-NULL
    result<-e$with_sequence_file_identity(p,function(ctx){
        first<-e$extract_sequence_from_fasta(p,'chr1',1,4,.sequence_context=ctx)
        r1_put(p,r1_b)
        second<-e$extract_sequence_from_fasta(p,'chr1',9,12,.sequence_context=ctx)
        sequence<-paste0(first,second)
        comp<-e$get_transcript_composition_cached(p,'chr1',r1_ex,
            spliced_sequence=sequence,spliced_identity=ctx$identity,.sequence_context=ctx)
        during<<-c(interval=length(e$cache_env_entry_keys(e$.seq_extract_cache)),
                   composition=length(e$cache_env_entry_keys(e$.transcript_composition_cache)))
        list(sequence=sequence,counts=comp$counts)
    })
    testthat::expect_identical(result$sequence,'AAAACCCC')
    testthat::expect_identical(result$counts,c(A=4L,T=0L,C=4L,G=0L))
    testthat::expect_identical(during,c(interval=2L,composition=0L))
    testthat::expect_identical(r1_cache_count(e),0L)
    r1_next(e,p)
})

testthat::test_that('replacement after extraction before composition insertion cannot label A as B',{
    e<-r1_env();p<-tempfile();on.exit(unlink(p));r1_put(p,r1_a)
    original<-e$count_sequence_bases
    e$count_sequence_bases<-function(sequence){r1_put(p,r1_b);original(sequence)}
    during<-NULL
    result<-e$with_sequence_file_identity(p,function(ctx){
        value<-e$get_transcript_composition_cached(p,'chr1',r1_ex,.sequence_context=ctx)
        during<<-length(e$cache_env_entry_keys(e$.transcript_composition_cache));value
    })
    testthat::expect_identical(result$counts,c(A=8L,T=0L,C=0L,G=0L))
    testthat::expect_identical(during,0L)
    testthat::expect_identical(r1_cache_count(e),0L)
    e$count_sequence_bases<-original;r1_next(e,p)
})

testthat::test_that('replacement immediately before outer exit purges entries already inserted',{
    e<-r1_env();p<-tempfile();on.exit(unlink(p));r1_put(p,r1_a)
    finish<-e$sequence_cache_finish;before_exit<-NULL;after_exit<-NULL
    e$sequence_cache_finish<-function(identity){
        before_exit<<-r1_cache_count(e);r1_put(p,r1_b)
        finish(identity);after_exit<<-r1_cache_count(e)
    }
    result<-e$with_sequence_file_identity(p,function(ctx){
        e$get_transcript_composition_cached(p,'chr1',r1_ex,.sequence_context=ctx)
    })
    testthat::expect_identical(result$counts,c(A=8L,T=0L,C=0L,G=0L))
    testthat::expect_gt(before_exit,0L);testthat::expect_identical(after_exit,0L)
    e$sequence_cache_finish<-finish;r1_next(e,p)
})

testthat::test_that('expired and serialized capabilities cannot bypass fresh validation',{
    e<-r1_env();p<-tempfile();on.exit(unlink(p));r1_put(p,r1_a)
    ctx<-e$with_sequence_file_identity(p,function(ctx){
        e$extract_spliced_exon_sequence(p,'chr1',r1_ex,.sequence_context=ctx);ctx
    })
    testthat::expect_false(ctx$active);testthat::expect_null(ctx$owner);testthat::expect_null(ctx$runtime)
    r1_put(p,r1_b)
    testthat::expect_identical(e$extract_spliced_exon_sequence(p,'chr1',r1_ex,.sequence_context=ctx),'CCCCCCCC')
    r1_put(p,r1_a)
    e$with_sequence_file_identity(p,function(ctx){
        e$extract_spliced_exon_sequence(p,'chr1',r1_ex,.sequence_context=ctx)
        copied<-unserialize(serialize(ctx,NULL));r1_put(p,r1_b)
        testthat::expect_false(e$sequence_context_matches(copied,p))
        testthat::expect_identical(e$extract_spliced_exon_sequence(p,'chr1',r1_ex,.sequence_context=copied),'CCCCCCCC')
    })
    r1_next(e,p)
})

testthat::test_that('default calls and superseded contexts remain independent inside an active scope',{
    e<-r1_env();p<-tempfile();on.exit(unlink(p));r1_put(p,r1_a)
    e$with_sequence_file_identity(p,function(ctx){
        old<-e$extract_spliced_exon_sequence(p,'chr1',r1_ex,.sequence_context=ctx)
        r1_put(p,r1_b)
        testthat::expect_identical(e$extract_spliced_exon_sequence(p,'chr1',r1_ex),'CCCCCCCC')
        testthat::expect_false(e$sequence_context_matches(ctx,p))
        comp<-e$get_transcript_composition_cached(p,'chr1',r1_ex,spliced_sequence=old,
            spliced_identity=ctx$identity,.sequence_context=ctx)
        testthat::expect_identical(comp$counts,c(A=0L,T=0L,C=8L,G=0L))
    })
    r1_next(e,p)
})

testthat::test_that('wrong-path contexts and errors cannot retain a live capability',{
    e<-r1_env();p<-tempfile();q<-tempfile();on.exit(unlink(c(p,q)))
    r1_put(p,r1_a);r1_put(q,r1_b);retained<-NULL
    testthat::expect_error(e$with_sequence_file_identity(p,function(ctx){
        retained<<-ctx
        testthat::expect_identical(e$extract_sequence_from_fasta(q,'chr1',1,16,.sequence_context=ctx),r1_b)
        stop('intentional')
    }),'intentional')
    testthat::expect_false(retained$active);testthat::expect_null(retained$owner)
    r1_put(p,r1_b);r1_next(e,p)
})

testthat::test_that('one logical warm request uses two identity stats regardless of exon count',{
    for(n in c(11L,23L)) {
        e<-r1_env(FALSE);p<-tempfile(fileext='.fa');on.exit(unlink(c(p,paste0(p,'.fai'))),add=TRUE)
        writeLines(c('>chr1',strrep('ACGT',5000)),p)
        st<-as.integer(seq(1,19900,length.out=n));ex<-data.frame(start=st,end=st+99L)
        run<-function()e$with_sequence_file_identity(p,function(ctx){
            e$extract_sequence_from_fasta(p,'chr1',1,20000,.sequence_context=ctx)
            e$extract_spliced_exon_sequence(p,'chr1',ex,.sequence_context=ctx)
            e$get_transcript_composition_cached(p,'chr1',ex,.sequence_context=ctx)
            for(i in seq_len(n))e$extract_sequence_from_fasta(p,'chr1',ex$start[i],ex$end[i],.sequence_context=ctx)
        })
        for(i in 1:3)run()
        stats<-0L;identities<-0L;enters<-0L;exits<-0L
        info<-base::file.info;identity<-e$sequence_file_identity;enter<-e$sequence_cache_validate;exit<-e$sequence_cache_finish
        e$file.info<-function(...){stats<<-stats+1L;info(...)}
        e$sequence_file_identity<-function(...){identities<<-identities+1L;identity(...)}
        e$sequence_cache_validate<-function(...){enters<<-enters+1L;enter(...)}
        e$sequence_cache_finish<-function(...){exits<<-exits+1L;exit(...)}
        run()
        testthat::expect_identical(c(identities,stats,enters,exits),c(2L,2L,1L,1L))
    }
})

testthat::test_that('wide transcript per-exon loop shares its snapshot and purges mixed reads',{
    e<-r1_env();e$annotation_memory_cache_limits$fasta_fallback_seq_max_bp<-1L
    p<-tempfile();on.exit(unlink(p));a<-paste0('AAAA',strrep('T',399996),'AAAA');b<-paste0('CCCC',strrep('G',399996),'CCCC')
    ex<-data.frame(start=c(1L,400001L),end=c(4L,400004L));r1_put(p,a)
    read<-e$extract_sequence_from_fasta;calls<-0L
    e$extract_sequence_from_fasta<-function(...){value<-read(...);calls<<-calls+1L;if(calls==1L)r1_put(p,b);value}
    testthat::expect_identical(e$extract_spliced_exon_sequence(p,'chr1',ex),'AAAACCCC')
    testthat::expect_identical(calls,2L);testthat::expect_identical(r1_cache_count(e),0L)
    e$extract_sequence_from_fasta<-read
    testthat::expect_identical(e$extract_spliced_exon_sequence(p,'chr1',ex),'CCCCCCCC')
    testthat::expect_identical(e$get_transcript_composition_cached(p,'chr1',ex)$counts,c(A=0L,T=0L,C=8L,G=0L))
})

testthat::test_that('scoped persistent future worker self-heals across repeated replacements',{
    testthat::skip_if(Sys.getenv('CGV_SEQUENCE_WORKER_TEST')!='1','requires local worker sockets')
    future::plan(future::multisession,workers=I(1));on.exit(future::plan(future::sequential),add=TRUE)
    p<-tempfile();on.exit(unlink(c(p,paste0(p,'.fai'))),add=TRUE);pids<-integer()
    for(i in 1:4){
        r1_put(p,if(i%%2L)r1_a else r1_b)
        result<-future::value(future::future({
            e<-getOption('cgev.r1.worker')
            if(is.null(e)){e<-new.env(parent=baseenv());sys.source(src,e);options(cgev.r1.worker=e)}
            e$with_sequence_file_identity(p,function(ctx){
                list(pid=Sys.getpid(),sequence=e$extract_spliced_exon_sequence(p,'chr1',ex,.sequence_context=ctx),
                     counts=e$get_transcript_composition_cached(p,'chr1',ex,.sequence_context=ctx)$counts)
            })
        },globals=list(p=p,ex=r1_ex,src=file.path(r1_root,'R/utils.R'))))
        pids<-c(pids,result$pid)
        testthat::expect_identical(result$sequence,if(i%%2L)'AAAAAAAA' else 'CCCCCCCC')
        testthat::expect_identical(result$counts,if(i%%2L)c(A=8L,T=0L,C=0L,G=0L) else c(A=0L,T=0L,C=8L,G=0L))
    }
    testthat::expect_length(unique(pids),1L);cat('\nR1 scoped worker PID:',unique(pids),'\n')
})

testthat::test_that('forked workers cannot borrow a live parent-process snapshot',{
    testthat::skip_on_os('windows')
    testthat::skip_if(Sys.getenv('CGV_SEQUENCE_WORKER_TEST')!='1','requires local worker processes')
    e<-r1_env();p<-tempfile();on.exit(unlink(p));r1_put(p,r1_a)
    parent_pid<-Sys.getpid()
    e$with_sequence_file_identity(p,function(ctx){
        e$extract_spliced_exon_sequence(p,'chr1',r1_ex,.sequence_context=ctx)
        r1_put(p,r1_b)
        child<-parallel::mccollect(parallel::mcparallel({
            list(pid=Sys.getpid(),matches=e$sequence_context_matches(ctx,p),
                 sequence=e$extract_spliced_exon_sequence(p,'chr1',r1_ex,.sequence_context=ctx))
        }))[[1L]]
        testthat::expect_false(identical(child$pid,parent_pid))
        testthat::expect_false(child$matches)
        testthat::expect_identical(child$sequence,'CCCCCCCC')
    })
    r1_next(e,p)
})
