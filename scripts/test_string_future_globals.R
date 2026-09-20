#!/usr/bin/env Rscript
# Small offline transport regression; no app startup or external requests.
e <- new.env(parent = baseenv())
sys.source('R/utils.R', e)
sys.source('R/string_cache.R', e)
sys.source('R/string_worker.R', e)
e$`%>%` <- magrittr::`%>%`
e$unused_heavy_cache <- raw(2 * 1024^2)
stopifnot(identical(environment(e$string_future_worker), baseenv()))
response <- function(status, body = '') httr2::response(
    status_code = status, body = charToRaw(body),
    headers = list(`content-type` = 'text/tab-separated-values'))
snapshot <- list(taxid = 9606L, id_candidates = 'target', screen_variants = character(),
                 required_score = 600L, add_nodes = 8L)
local({
    tmp <- tempfile('string-future-'); dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    globals <- e$string_future_globals(snapshot)
    stopifnot(identical(names(globals), c('string_future_worker', 'query_payload', 'cache_snapshot')),
              identical(parent.env(globals$cache_snapshot$resolution), emptyenv()),
              identical(parent.env(globals$cache_snapshot$network), emptyenv()),
              length(serialize(globals, NULL)) < 20000L)
    invoke <- function(s = snapshot) e$string_future_worker(s, globals$cache_snapshot, tmp)
    httr2::with_mocked_responses(list(
        response(200, '0\t9606.TARGET\t9606\tHomo sapiens\tTARGET\tannotation\n'),
        response(200, paste0('stringId_A\tstringId_B\tpreferredName_A\tpreferredName_B\tscore\n',
                             '9606.TARGET\t9606.OTHER\tTARGET\tOTHER\t0.9\n'))
    ), result <- invoke())
    stopifnot(isTRUE(result$ok), identical(result$cache_hit, FALSE),
              identical(result$payload$nodes$role, c('target', 'neighbor')),
              identical(result$payload$edges$score, 0.9))
    e$string_resolution_cache_set(9606L, 'other_alias',
        list(found = TRUE, string_id = '9606.OTHER', preferred_name = 'OTHER'), tmp)
    plotted <- snapshot; plotted$screen_variants <- 'other_alias'
    stopifnot(identical(invoke(plotted)$payload$nodes$role, c('target', 'plotted')))
    original <- e$string_resolve_and_fetch(snapshot, tmp)
    warm <- invoke()
    original$payload$role_applied_at <- warm$payload$role_applied_at <- NULL
    stopifnot(identical(original, warm), isTRUE(warm$cache_hit))
    missing <- snapshot; missing$id_candidates <- 'missing'
    httr2::with_mocked_responses(list(response(404)), {
        stopifnot(identical(invoke(missing), list(ok = FALSE, reason = 'not_found')))
    })
    for (status in c(400L, 401L, 500L, 503L)) {
        bad <- snapshot; bad$id_candidates <- paste0('bad', status)
        httr2::with_mocked_responses(list(response(status), response(status)), {
            err <- tryCatch(invoke(bad), error = identity)
            stopifnot(inherits(err, paste0('httr2_http_', status)))
        })
    }
    e$string_resolution_cache_set(9606L, 'network-error',
        list(found = TRUE, string_id = '9606.ERROR', preferred_name = 'ERROR'), tmp)
    bad <- snapshot; bad$id_candidates <- 'network-error'
    httr2::with_mocked_responses(list(response(404)), {
        stopifnot(inherits(tryCatch(invoke(bad), error = identity), 'httr2_http_404'))
    })
    # A separate R process must resolve everything from the explicit globals.
    future::plan(future::multisession, workers = I(1L))
    on.exit(future::plan(future::sequential), add = TRUE)
    globals$base_dir <- tmp
    f <- promises::future_promise(string_future_worker(query_payload, cache_snapshot, base_dir),
                        globals = globals, packages = c('httr2', 'magrittr'), seed = TRUE)
    remote <- NULL; failure <- NULL; done <- FALSE
    promises::then(f, onFulfilled = function(x) { remote <<- x; done <<- TRUE },
                   onRejected = function(err) { failure <<- err; done <<- TRUE })
    deadline <- Sys.time() + 20
    while (!done && Sys.time() < deadline) later::run_now(0.05)
    stopifnot(done)
    if (!is.null(failure)) stop(failure)
    remote$payload$role_applied_at <- NULL
    stopifnot(identical(remote, original))
    cat('STRING explicit globals:', length(globals) - 1L,
        'serialized bytes (fixture):', length(serialize(globals[-length(globals)], NULL)), '\n')
})
cat('string-future-globals-ok\n')
