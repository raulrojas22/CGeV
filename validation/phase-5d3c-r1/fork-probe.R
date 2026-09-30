# Supplemental final-tree probe: inherit a live scope and instrument child stats.
for (expr in parse('tests/testthat/test-sequence-operation-identity.R')) {
    if (is.call(expr) && identical(expr[[1]], quote(testthat::test_that))) break
    eval(expr)
}
stopifnot(.Platform$OS.type != 'windows')
e <- r1_env(); p <- tempfile(); r1_put(p,r1_a)
parent <- Sys.getpid()
child <- e$with_sequence_file_identity(p,function(ctx) {
    stopifnot(e$extract_spliced_exon_sequence(p,'chr1',r1_ex,.sequence_context=ctx)=='AAAAAAAA')
    r1_put(p,r1_b)
    parallel::mccollect(parallel::mcparallel({
        ids <- stats <- 0L
        old <- e$sequence_file_identity
        e$sequence_file_identity <- function(...) {ids <<- ids+1L; old(...)}
        e$file.info <- function(...) {stats <<- stats+1L; base::file.info(...)}
        matches <- e$sequence_context_matches(ctx,p)
        value <- e$extract_spliced_exon_sequence(p,'chr1',r1_ex,.sequence_context=ctx)
        checks <- c(identity=ids,file.info=stats)
        cached <- unlist(as.list(e$.spliced_seq_cache),use.names=FALSE)
        stopifnot(!matches, value=='CCCCCCCC', ids>=2L,stats>=2L,
                  !any(cached=='AAAAAAAA'))
        r1_next(e,p)
        list(pid=Sys.getpid(),inherited_active=ctx$active,owner_pid=ctx$pid,
             matches=matches,value=value,checks=checks,next_correct=TRUE)
    }))[[1L]]
})
stopifnot(child$pid!=parent,r1_cache_count(e)==0L)
r1_next(e,p)
cat('Parent PID:',parent,'\n'); print(child)
cat('Parent exit cache entries before next request: 0\nParent next independent request: B, C=8\n')
unlink(p)
