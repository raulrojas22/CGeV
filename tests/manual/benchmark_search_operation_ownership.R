# Read-only baseline/reviewed/corrected comparison, no scientific search workload.
# Run from repository root: Rscript tests/manual/benchmark_search_operation_ownership.R
source('tests/testthat/helper-search-ownership.R')
benchmark <- function() {
    baseline_ref <- '7558c186629c2021bb21dad483e9e6fa4cd6d2df'
    reviewed_ref <- '5440da9f3d973f9f1bb4b7469af59a015165c1b5'
    baseline_file <- tempfile(fileext = '.R')
    on.exit(unlink(baseline_file), add = TRUE)
    stopifnot(system2('git', c('show', paste0(baseline_ref, ':R/server_state_helpers_domain.R')),
        stdout = baseline_file) == 0L)
    old_env <- new.env(parent = globalenv())
    old_env$`%||%` <- function(a, b) if (is.null(a)) b else a
    sys.source(baseline_file, old_env)
    old <- ownership_harness(FALSE); new <- ownership_harness(FALSE)
    on.exit({old$close(); new$close()}, add = TRUE)
    reviewed_root <- tempfile('reviewed-lifecycle-'); dir.create(file.path(reviewed_root, 'R'), recursive = TRUE)
    on.exit(unlink(reviewed_root, recursive = TRUE), add = TRUE)
    for (path in c('server.R', 'R/utils.R', 'R/server_state_helpers_domain.R', 'R/server_popup_status_domain.R')) {
        stopifnot(system2('git', c('show', paste0(reviewed_ref, ':', path)),
            stdout = file.path(reviewed_root, path)) == 0L)
    }
    reviewed_env <- new.env(parent = globalenv())
    reviewed_helper <- system2('git', c('show', paste0(reviewed_ref, ':tests/testthat/helper-search-ownership.R')), stdout = TRUE)
    withr::with_envvar(c(CGV_SEARCH_TEST_ROOT = reviewed_root),
        eval(parse(text = reviewed_helper), reviewed_env))
    reviewed <- reviewed_env$ownership_harness(FALSE)
    on.exit(reviewed$close(), add = TRUE)
    old_helpers <- old_env$init_server_state_helpers_domain(old$e$autocompleteBuildEpochs, old$e$searchRunState)
    baseline_text <- system2('git', c('show', paste0(baseline_ref, ':server.R')), stdout = TRUE)
    baseline_body <- parse(text = baseline_text)[[1L]][[3L]][[3L]]
    cleanup_def <- Filter(function(x) is.call(x) && identical(x[[1L]], quote(`<-`)) &&
        identical(x[[2L]], quote(finalize_search_cleanup)), as.list(baseline_body))[[1L]]
    old_cleanup <- eval(cleanup_def[[3L]], old$e)
    count <- previous_count <- 0
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
        new$e$finalize_search_request('homologous', id)
    }
    reviewed_pair <- function() {
        previous_count <<- previous_count + 1
        id <- reviewed$e$take_search_event('homologous', previous_count, NULL)
        a <- reviewed$e$begin_search_run('homologous', 'gene::G', 0, id)
        batch <- reviewed$e$bind_search_batch_owner('homologous', a$id, 'local')
        reviewed$e$finalize_search_cleanup('homologous', a$id, batch)
        reviewed$e$release_search_admission('homologous', a$id)
        reviewed$e$stateHelpersDomain$cancel_search_request('homologous', id)
    }
    n <- 1000L; baseline <- previous <- candidate <- numeric(7L)
    shiny::isolate(for (i in 1:100) {baseline_pair(); reviewed_pair(); candidate_pair()})
    for (round in 1:7) {
        gc()
        baseline[round] <- system.time(shiny::isolate(for (i in seq_len(n)) baseline_pair()))[['elapsed']] * 1e6 / n
        previous[round] <- system.time(shiny::isolate(for (i in seq_len(n)) reviewed_pair()))[['elapsed']] * 1e6 / n
        candidate[round] <- system.time(shiny::isolate(for (i in seq_len(n)) candidate_pair()))[['elapsed']] * 1e6 / n
    }
    cat('Shiny', as.character(packageVersion('shiny')), R.version.string, '\n')
    cat('Seven rounds of', n, 'admission/cleanup/completion pairs, microseconds per pair:\n')
    print(data.frame(baseline, reviewed = previous, corrected = candidate))
    cat('Median baseline', median(baseline), 'reviewed', median(previous), 'corrected', median(candidate),
        'increment vs baseline', median(candidate) - median(baseline),
        'increment vs reviewed', median(candidate) - median(previous), 'us/pair\n')
    cat('Excludes DOM, browser transport, scientific computation and rendering.\n')
    cat('Includes actual session guards and batch binding/cleanup; synthetic reactive data.\n')
}
benchmark()
