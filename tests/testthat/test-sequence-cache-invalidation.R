sequence_identity_root <- normalizePath(if (file.exists('R/utils.R')) '.' else '../..')
sequence_identity_source <- Sys.getenv('CGV_SEQUENCE_TEST_SOURCE', file.path(sequence_identity_root, 'R/utils.R'))
seq_identity_env <- function(fallback = FALSE) {
    e <- new.env(parent = globalenv()); sys.source(sequence_identity_source, e)
    if (fallback) e$requireNamespace <- function(package, ...) {
        if (package == 'Rsamtools') FALSE else base::requireNamespace(package, ...)
    }
    if (Sys.getenv('CGV_SEQUENCE_MUTANT') == '1') {
        e$sequence_cache_validate <- function(path) e$sequence_file_identity(path)
        e$sequence_cache_finish <- function(identity) invisible(NULL)
        e$sequence_fasta_index_usable <- function(path) TRUE
    }
    e
}
seq_a <- 'AAAATTTTAAAATTTT'
seq_b <- 'CCCCGGGGCCCCGGGG'
seq_exons <- data.frame(start = c(1L, 9L), end = c(4L, 12L))
seq_write <- function(path, bases, name = 'chr1', mode = 'atomic') {
    before <- file.info(path)
    target <- if (mode == 'rewrite') path else paste0(path, '.next')
    writeLines(c(paste0('>', name, ' chromosome 1'), bases), target)
    if (mode == 'preserved-mtime') Sys.setFileTime(target, before$mtime)
    if (target != path) stopifnot(file.rename(target, path))
    invisible(path)
}
seq_oracle <- function(e, p, bases) {
    testthat::expect_identical(e$extract_sequence_from_fasta(p, 'chr1', 1L, 16L), bases)
    # This subregion was never requested before the replacement; an old broad
    # span must not promote its old bases into a fresh exact entry.
    testthat::expect_identical(e$extract_sequence_from_fasta(p, 'chr1', 5L, 8L), substr(bases, 5L, 8L))
    splice <- paste0(substr(bases, 1L, 4L), substr(bases, 9L, 12L))
    testthat::expect_identical(e$extract_spliced_exon_sequence(p, 'chr1', seq_exons), splice)
    reverse <- paste(rev(strsplit(chartr('ACGT', 'TGCA', splice), '')[[1L]]), collapse = '')
    testthat::expect_identical(e$extract_spliced_exon_sequence(p, 'chr1', seq_exons, '-'), reverse)
    expected <- setNames(vapply(c('A','T','C','G'), function(x) sum(strsplit(splice, '')[[1L]] == x), integer(1)), c('A','T','C','G'))
    comp <- e$get_transcript_composition_cached(p, 'chr1', seq_exons)
    testthat::expect_identical(comp$counts, expected)
    testthat::expect_identical(comp$length, 8L)
}
for (fallback in c(FALSE, TRUE)) for (replacement in c('atomic','rewrite','same-size','preserved-mtime')) local({
    forced <- fallback; mode <- replacement
    testthat::test_that(paste('bases and composition follow', mode, if(forced) 'fallback' else 'indexed'), {
        e <- seq_identity_env(forced); p <- tempfile(fileext = '.fa')
        on.exit(unlink(c(p, paste0(p, '.fai'))), add = TRUE)
        seq_write(p, seq_a)
        e$extract_sequence_from_fasta(p, 'chr1', 1L, 16L)
        e$extract_spliced_exon_sequence(p, 'chr1', seq_exons)
        e$get_transcript_composition_cached(p, 'chr1', seq_exons)
        before <- file.info(p); Sys.sleep(1.1)
        seq_write(p, seq_b, mode = mode)
        testthat::expect_identical(file.info(p)$size, before$size)
        if (mode == 'preserved-mtime') {
            testthat::expect_identical(file.info(p)$mtime, before$mtime)
            testthat::expect_false(identical(file.info(p)$ctime, before$ctime))
        }
        seq_oracle(e, p, seq_b)
    })
})

testthat::test_that('unchanged inputs reuse values and unrelated genomes stay warm', {
    e <- seq_identity_env(TRUE); p <- tempfile(); q <- tempfile()
    on.exit(unlink(c(p,q)), add=TRUE); seq_write(p,seq_a);seq_write(q,seq_b)
    seq_oracle(e,p,seq_a);seq_oracle(e,q,seq_b)
    qkey <- e$get_seq_extract_cache_key(q,'chr1',1,16)
    old <- get(qkey,e$.seq_extract_cache)
    # Reader sentinel: hits must never open the FASTA.
    e$file <- function(...) stop('unexpected cold read')
    seq_oracle(e,p,seq_a);seq_oracle(e,q,seq_b)
    rm('file',envir=e);Sys.sleep(0.01);seq_write(p,seq_b);seq_oracle(e,p,seq_b)
    testthat::expect_identical(get(qkey,e$.seq_extract_cache),old)
    e$file <- function(...) stop('unrelated path was evicted');seq_oracle(e,q,seq_b)
})

testthat::test_that('deletion fails closed and recreation observes new bases', {
    e<-seq_identity_env(TRUE);p<-tempfile();seq_write(p,seq_a);seq_oracle(e,p,seq_a)
    unlink(p)
    testthat::expect_identical(e$extract_sequence_from_fasta(p,'chr1',1,16),'')
    testthat::expect_identical(e$extract_spliced_exon_sequence(p,'chr1',seq_exons),'')
    testthat::expect_identical(e$get_transcript_composition_cached(p,'chr1',seq_exons)$known_total,0L)
    seq_write(p,seq_b);on.exit(unlink(p));seq_oracle(e,p,seq_b)
})

testthat::test_that('header, seqname and resolution follow source and index replacements', {
    testthat::skip_if_not_installed('Rsamtools')
    e<-seq_identity_env(FALSE);p<-tempfile();seq_write(p,seq_a,'old')
    on.exit(unlink(c(p,paste0(p,'.fai'))))
    Rsamtools::indexFa(p)
    testthat::expect_identical(e$resolve_seqname_in_fasta(p,'1'),'old')
    testthat::expect_identical(e$get_fasta_index_seqnames(p),'old')
    Sys.sleep(0.01);seq_write(p,seq_b,'new')
    testthat::expect_identical(names(e$get_fasta_header_map(p)$seqname_to_header),'new')
    testthat::expect_identical(e$resolve_seqname_in_fasta(p,'1'),'new')
    testthat::expect_identical(e$extract_sequence_from_fasta(p,'1',1,16),seq_b)
    unlink(paste0(p,'.fai'));Rsamtools::indexFa(p)
    testthat::expect_identical(e$get_fasta_index_seqnames(p),'new')
    writeLines('indexOnly\t16\t25\t16\t17',paste0(p,'.fai'))
    # A replacement index with names/layout absent from the source is unsafe.
    testthat::expect_identical(e$get_fasta_index_seqnames(p),character(0))
    testthat::expect_false(e$sequence_fasta_index_usable(normalizePath(p)))
    testthat::expect_identical(e$extract_sequence_from_fasta(p,'1',1,16),seq_b)
})

testthat::test_that('composition rejects supplied sequence without matching provenance', {
    e<-seq_identity_env(TRUE);p<-tempfile();seq_write(p,seq_a)
    on.exit(unlink(p));old<-e$extract_spliced_exon_sequence(p,'chr1',seq_exons)
    id<-if(exists('sequence_file_identity',e)) e$sequence_file_identity(p) else NULL
    Sys.sleep(0.01);seq_write(p,seq_b)
    result<-e$get_transcript_composition_cached(p,'chr1',seq_exons,spliced_sequence=old)
    testthat::expect_identical(result$counts,c(A=0L,T=0L,C=8L,G=0L))
    if (!is.null(id)) {
        e$sequence_cache_purge(normalizePath(p))
        result<-e$get_transcript_composition_cached(p,'chr1',seq_exons,spliced_sequence=old,spliced_identity=id)
        testthat::expect_identical(result$counts,c(A=0L,T=0L,C=8L,G=0L))
    }
})

testthat::test_that('repeated replacements retain one bounded generation', {
    e<-seq_identity_env(TRUE);p<-tempfile();on.exit(unlink(p));sizes<-numeric();counts<-list()
    caches<-c('.seq_extract_cache','.spliced_seq_cache','.fasta_fallback_seq_cache','.fasta_header_cache',
              '.fasta_seqnames_cache','.fasta_resolved_seqname_cache','.transcript_composition_cache')
    for(i in 1:24) {
        bases<-if(i%%2L)seq_a else seq_b;Sys.sleep(0.002);seq_write(p,bases)
        seq_oracle(e,p,bases)
        counts[[i]]<-vapply(caches,function(n)length(e$cache_env_entry_keys(e[[n]])),integer(1))
        sizes[i]<-sum(vapply(caches,function(n) sum(vapply(as.list(e[[n]],all.names=TRUE),function(x)as.numeric(object.size(x)),numeric(1))),numeric(1)))
    }
    testthat::expect_true(all(vapply(counts[-1L],function(x)identical(x,counts[[2]]),logical(1))))
    testthat::expect_lte(max(sizes),min(sizes)+2048)
    cat('\nSequence cache cycles: counts=',paste(counts[[24]],collapse=','),' retained bytes=',paste(range(sizes),collapse='..'),'\n')
})

testthat::test_that('relative paths and retargeted symlinks return current bases', {
    e<-seq_identity_env(TRUE);root<-tempfile();dir.create(root);on.exit(unlink(root,recursive=TRUE))
    withr::local_dir(root);seq_write('a.fa',seq_a);seq_write('b.fa',seq_b)
    testthat::expect_identical(e$extract_sequence_from_fasta('a.fa','chr1',1,16),seq_a)
    testthat::expect_true(file.symlink('a.fa','link.fa'));seq_oracle(e,'link.fa',seq_a)
    unlink('link.fa');testthat::expect_true(file.symlink('b.fa','link.fa'));seq_oracle(e,'link.fa',seq_b)
    unlink('a.fa');testthat::expect_identical(e$extract_sequence_from_fasta('a.fa','chr1',1,16),'')
})

testthat::test_that('truncated or malformed replacement cannot return old bases', {
    e<-seq_identity_env(TRUE);p<-tempfile();on.exit(unlink(p));seq_write(p,seq_a);seq_oracle(e,p,seq_a)
    seq_write(p,'CC');testthat::expect_identical(e$extract_sequence_from_fasta(p,'chr1',1,16),'')
    writeLines('not a fasta',p);testthat::expect_identical(e$extract_sequence_from_fasta(p,'chr1',1,16),'')
    testthat::expect_identical(e$extract_spliced_exon_sequence(p,'chr1',seq_exons),'')
})

testthat::test_that('persistent multisession worker follows multiple replacements in one PID', {
    testthat::skip_if(Sys.getenv('CGV_SEQUENCE_WORKER_TEST') != '1', 'requires local worker sockets')
    testthat::skip_if_not_installed('future')
    future::plan(future::multisession,workers=I(1));on.exit(future::plan(future::sequential),add=TRUE)
    p<-tempfile();on.exit(unlink(c(p,paste0(p,'.fai'))),add=TRUE);pids<-integer()
    for(i in 1:4) {
        bases<-if(i%%2L)seq_a else seq_b;seq_write(p,bases);Sys.sleep(0.01)
        result<-future::value(future::future({
            e<-getOption('cgev.sequence.identity.test')
            if(is.null(e)) {e<-new.env(parent=baseenv());sys.source(src,e);options(cgev.sequence.identity.test=e)}
            list(pid=Sys.getpid(),seq=e$extract_spliced_exon_sequence(p,'chr1',ex),
                 counts=e$get_transcript_composition_cached(p,'chr1',ex)$counts)
        },globals=list(src=sequence_identity_source,p=p,ex=seq_exons)))
        pids<-c(pids,result$pid)
        testthat::expect_identical(result$seq,if(i%%2L)'AAAAAAAA' else 'CCCCCCCC')
        testthat::expect_identical(result$counts,if(i%%2L)c(A=8L,T=0L,C=0L,G=0L) else c(A=0L,T=0L,C=8L,G=0L))
    }
    testthat::expect_length(unique(pids),1L);cat('\nPersistent worker PID:',unique(pids),'\n')
})

testthat::test_that('2bit metadata, offsets and sidecars reject preserved-mtime replacement', {
    testthat::skip_if_not_installed('rtracklayer')
    e<-seq_identity_env();root<-tempfile();dir.create(root);on.exit(unlink(root,recursive=TRUE))
    withr::local_envvar(CGV_CACHE_DIR=file.path(root,'cache'))
    p<-file.path(root,'tiny.2bit')
    rtracklayer::export(Biostrings::DNAStringSet(c(old=seq_a)),p)
    testthat::expect_identical(e$get_twobit_seqnames(p),'old')
    testthat::expect_identical(e$extract_sequence_from_fasta(p,'old',1,16),seq_a)
    before<-file.info(p);Sys.sleep(1.1)
    q<-file.path(root,'next.2bit');rtracklayer::export(Biostrings::DNAStringSet(c(new=seq_b)),q)
    Sys.setFileTime(q,before$mtime);stopifnot(file.rename(q,p))
    testthat::expect_identical(file.info(p)$size,before$size)
    testthat::expect_null(e$read_twobit_seqnames_sidecar(p))
    testthat::expect_identical(e$get_twobit_seqnames(p),'new')
    testthat::expect_identical(e$extract_sequence_from_fasta(p,'new',1,16),seq_b)
    testthat::expect_identical(e$extract_sequence_from_2bit_native(p,'new',5,8),'GGGG')
    testthat::expect_identical(e$extract_spliced_exon_sequence(p,'new',seq_exons),'CCCCCCCC')
    testthat::expect_identical(e$get_transcript_composition_cached(p,'new',seq_exons)$counts,c(A=0L,T=0L,C=8L,G=0L))
})

testthat::test_that('copied sequence memo state is checked against the worker filesystem', {
    e<-seq_identity_env(TRUE);p<-tempfile();on.exit(unlink(p));seq_write(p,seq_a);seq_oracle(e,p,seq_a)
    copied<-unserialize(serialize(e$.spliced_seq_cache,NULL))
    state<-if(exists('.sequence_file_state',e))unserialize(serialize(e$.sequence_file_state,NULL)) else NULL
    seq_write(p,seq_b)
    worker<-seq_identity_env(TRUE);worker$.spliced_seq_cache<-copied
    if(!is.null(state))worker$.sequence_file_state<-state
    testthat::expect_identical(worker$extract_spliced_exon_sequence(p,'chr1',seq_exons),'CCCCCCCC')
})

testthat::test_that('changed FASTA layout cannot reuse an old on-disk index', {
    testthat::skip_if_not_installed('Rsamtools')
    e<-seq_identity_env();p<-tempfile(fileext='.fa');on.exit(unlink(c(p,paste0(p,'.fai'))))
    seq_write(p,seq_a);Rsamtools::indexFa(p);seq_oracle(e,p,seq_a)
    Sys.sleep(0.01)
    writeLines(c('>chr1 longer replacement header chromosome 1','CCCC','GGGG','CCCC','GGGG'),p)
    seq_oracle(e,p,seq_b)
    # Forgetting the small provenance map must not rehabilitate the old index.
    cold<-seq_identity_env();seq_oracle(cold,p,seq_b)
    Rsamtools::indexFa(p)
    testthat::expect_false(is.null(e$get_cached_fafile(p)))
    seq_oracle(e,p,seq_b)
})

testthat::test_that('a replacement during composition extraction cannot poison the next version', {
    e<-seq_identity_env(TRUE);p<-tempfile();on.exit(unlink(p));seq_write(p,seq_a)
    original<-e$extract_spliced_exon_sequence
    e$extract_spliced_exon_sequence<-function(...) {
        old<-original(...);seq_write(p,seq_b);old
    }
    # The in-flight result may belong to A; it must not be stored under B.
    e$get_transcript_composition_cached(p,'chr1',seq_exons)
    e$extract_spliced_exon_sequence<-original
    testthat::expect_identical(e$get_transcript_composition_cached(p,'chr1',seq_exons)$counts,c(A=0L,T=0L,C=8L,G=0L))
})

testthat::test_that('2bit sidecar writes reject seqnames from an older source identity', {
    e<-seq_identity_env();p<-tempfile(fileext='.2bit');on.exit(unlink(p))
    writeBin(charToRaw('version-a'),p)
    if(!exists('sequence_file_identity',e)) testthat::skip('baseline has no provenance API')
    id<-e$sequence_file_identity(p);Sys.sleep(0.01);writeBin(charToRaw('version-b'),p)
    testthat::expect_false(e$write_twobit_seqnames_sidecar(p,'old',source_identity=id))
})
