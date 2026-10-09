# Read-only baseline comparison using synthetic Shiny sessions, no search workload.
# Run from repository root: Rscript tests/manual/benchmark_search_operation_ownership.R
source('tests/testthat/helper-search-ownership.R')
benchmark <- function() {
    baseline_ref <- '7558c186629c2021bb21dad483e9e6fa4cd6d2df'
    baseline_file <- tempfile(fileext = '.R')
    on.exit(unlink(baseline_file), add = TRUE)
    stopifnot(system2('git', c('show', paste0(baseline_ref, ':R/server_state_helpers_domain.R')),
        stdout = baseline_file) == 0L)
    old_env <- new.env(parent = globalenv())
    old_env$`%||%` <- function(a, b) if (is.null(a)) b else a
    sys.source(baseline_file, old_env)
    old <- ownership_harness(FALSE); new <- ownership_harness(FALSE)
    on.exit({old$close(); new$close()}, add = TRUE)
    old_helpers <- old_env$init_server_state_helpers_domain(old$e$autocompleteBuildEpochs, old$e$searchRunState)
    baseline_text <- system2('git', c('show', paste0(baseline_ref, ':server.R')), stdout = TRUE)
    baseline_body <- parse(text = baseline_text)[[1L]][[3L]][[3L]]
    cleanup_def <- Filter(function(x) is.call(x) && identical(x[[1L]], quote(`<-`)) &&
        identical(x[[2L]], quote(finalize_search_cleanup)), as.list(baseline_body))[[1L]]
    old_cleanup <- eval(cleanup_def[[3L]], old$e)
    count <- 0
    baseline_pair <- function() {
        old_helpers$begin_search_run('homologous', 'gene::G', 0)
        old_cleanup('homologous')
        old_helpers$finish_search_run('homologous')
    }
    candidate_pair <- function() {
        count <<- count + 1
        id <- new$e$take_search_event('homologous', count, NULL)
        a <- new$e$begin_search_run('homologous', 'gene::G', 0, id)
        batch <- new$e$bind_search_batch_owner('homologous', a$id, 'local')
        new$e$finalize_search_cleanup('homologous', a$id, batch)
        new$e$release_search_admission('homologous', a$id)
    }
    n <- 1000L; baseline <- candidate <- numeric(7L)
    shiny::isolate(for (i in 1:100) {baseline_pair(); candidate_pair()})
    for (round in 1:7) {
        gc()
        baseline[round] <- system.time(shiny::isolate(for (i in seq_len(n)) baseline_pair()))[['elapsed']] * 1e6 / n
        candidate[round] <- system.time(shiny::isolate(for (i in seq_len(n)) candidate_pair()))[['elapsed']] * 1e6 / n
    }
    cat('Shiny', as.character(packageVersion('shiny')), R.version.string, '\n')
    cat('Seven rounds of', n, 'admission/cleanup/completion pairs, microseconds per pair:\n')
    print(data.frame(baseline, candidate))
    cat('Median baseline', median(baseline), 'candidate', median(candidate),
        'increment', median(candidate) - median(baseline), 'us/pair\n')
    cat('Excludes DOM, browser transport, scientific computation and rendering.\n')
    cat('Includes actual session guards and batch binding/cleanup; synthetic reactive data.\n')
}
benchmark()
