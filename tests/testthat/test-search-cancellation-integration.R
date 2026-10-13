# Each scenario loads the full server in a fresh process, avoiding global cache,
# future-plan and package-attachment interference with the rest of testthat.
cancellation_root <- normalizePath(if (file.exists('server.R')) '.' else '../..')
for (scenario in c('rapid-selection', 'stale-delete', 'valid-delete', 'old-source-owner',
                   'owned-delete-cancellation', 'repeat-and-replace', 'cross-mode', 'confirmation-generation',
                   'ncbi-source-replacement', 'source-repeated', 'source-ncbi-same-labels', 'source-mixed',
                   'source-upload', 'source-upload-genome', 'source-upload-delete', 'source-rapid',
                   'source-new-owner', 'source-return', 'source-teardown', 'source-downloaded', 'source-partial', 'source-draft')) {
    local({
        case <- scenario
        testthat::test_that(paste('complete server cancellation ownership:', case), {
            script <- file.path(cancellation_root, 'tests/fixtures/search_cancellation_integration.R')
            log <- tempfile('cgv-cancellation-case-', fileext = '.log')
            on.exit(unlink(log), add = TRUE)
            status <- system2(file.path(R.home('bin'), 'Rscript'),
                c(shQuote(script), shQuote(cancellation_root), case), stdout = log, stderr = log)
            output <- readLines(log, warn = FALSE)
            testthat::expect_equal(status, 0L, info = paste(output, collapse = '\n'))
            testthat::expect_true(paste0('PASS ', case) %in% output, info = paste(tail(output, 12L), collapse = '\n'))
        })
    })
}
