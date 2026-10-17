# Full server, real scientific producers and publication; private queued workers
# avoid changing the application/global future plan. Each case is a fresh process.
restoration_root <- normalizePath(if (file.exists('server.R')) '.' else '../..')
for (scenario in c('restore-queued','restore-worker-error','restore-new-search','restore-repeat',
    'restore-batch','restore-malformed','restore-read-error','restore-closed','restore-closed-error','restore-schema1',
    'restore-pending-h','restore-pending-o','restore-external-queued','restore-view-callbacks','restore-isolation','restore-stale-source-ack','restore-external-settle','restore-external-error','restore-report')) {
    local({
        case <- scenario
        testthat::test_that(paste('complete server work restoration:', case), {
            log <- tempfile(fileext = '.log'); on.exit(unlink(log), add = TRUE)
            status <- system2(file.path(R.home('bin'),'Rscript'),
                c(shQuote(file.path(restoration_root,'tests/fixtures/search_cancellation_integration.R')),
                  shQuote(restoration_root),case), stdout = log, stderr = log)
            output <- readLines(log,warn=FALSE)
            testthat::expect_equal(status,0L,info=paste(output,collapse='\n'))
            testthat::expect_true(paste0('PASS ',case) %in% output,info=paste(tail(output,15),collapse='\n'))
        })
    })
}
