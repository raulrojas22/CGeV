#!/usr/bin/env Rscript
# Offline Phase 2A regression and microbenchmark. Run from repository root.
# Optional: CGEV_PHASE2A_FIXTURES directory containing ACTB/TP53/BRCA1.gff.
# Uses real multisession workers; no live STRING requests or app cache writes.
suppressPackageStartupMessages({ library(shiny); library(promises); library(visNetwork) })
source('R/utils.R'); source('R/string_cache.R'); source('R/string_worker.R')
source('R/string_annotation.R')
main_screen_calls <- 0L
original_screen <- string_prepare_screen_rows
string_prepare_screen_rows <- function(attrs) {
    if (length(attrs)) main_screen_calls <<- main_screen_calls + 1L
    original_screen(attrs)
}
Sys.setenv(APP_PERF_TIMING = '0', APP_DEBUG_LOGS = '0')
lines <- readLines('server.R')
block <- function(start, end) {
    a <- grep(start, lines, fixed = TRUE); b <- grep(end, lines, fixed = TRUE)
    stopifnot(length(a) == 1L, length(b) == 1L, a < b)
    parse(text = lines[a:(b - 1L)])
}
builder <- block('    stringAnnotationState <- new_string_annotation_state()',
                 '    build_string_network_widget_async <- function')
a <- grep('    observeEvent(input$open_string_network, {', lines, fixed = TRUE)
b <- grep('    observeEvent(input$app_theme, {', lines, fixed = TRUE)
b <- b[b > a][1L]
observers <- parse(text = lines[a:(b - 1L)])
widget <- block('    build_string_network_widget_from_data <- function',
                '    observeEvent(input$open_string_network, {')
frame <- function(attrs) data.frame(V1 = 'chr1', V3 = c('gene', rep('exon', length(attrs) - 1L)),
                                    V4 = seq_along(attrs), V5 = seq_along(attrs) + 100L, V9 = attrs)
genes <- c('ACTB', 'TP53', 'BRCA1')
fixtures <- setNames(lapply(genes, function(gene) {
    path <- file.path(Sys.getenv('CGEV_PHASE2A_FIXTURES'), paste0(gene, '.gff'))
    if (nzchar(Sys.getenv('CGEV_PHASE2A_FIXTURES')) && file.exists(path)) {
        d <- utils::read.delim(path, header = FALSE, quote = '', comment.char = '', stringsAsFactors = FALSE)
        cat(gene, 'real GFF rows:', nrow(d), '\n'); d
    } else frame(c(paste0('ID=gene:', gene, ';gene=', gene, ';Dbxref=GeneID:1'),
                   paste0('ID=cds:', gene, ';protein_id=NP_', gene)))
}), genes)
edge_attrs <- c(NA_character_, '', '.', 'ID=%00;Name=%FF', '%4Eame=encoded',
                'gene_id "gtf"; Name=one=two', 'ID=é;Name=%C3%A9',
                'ID=A;ID=B;Dbxref=GeneID:1,Ref:part:tail', 'malformed;=;Name=;ID==')
make_env <- function(reactive = FALSE) {
    e <- new.env(parent = globalenv())
    e$hfiles <- setNames(unname(fixtures), as.character(seq_along(fixtures)))
    e$hfiles[['4']] <- frame(edge_attrs)
    e$horg <- setNames(rep(list(list(name = 'Human', taxid = 9606L)), 4L), names(e$hfiles))
    e$hpaths <- setNames(rep(list('fixture.gff'), 4L), names(e$hfiles))
    e$htitles <- setNames(as.list(c(genes, 'Edge')), names(e$hfiles))
    e$ofiles <- list('1' = frame('ID=mouse;Name=Mouse'))
    e$oorg <- list('1' = list(name = 'Mouse', taxid = 10090L))
    e$opaths <- list('1' = 'mouse.gff'); e$otitles <- list('1' = 'Mouse')
    eval(quote({
        fileDataHomologous <- function() hfiles
        fileDataOrthologous <- function() ofiles
        organismInfoHomologous <- function() horg
        organismInfoOrthologous <- function() oorg
        annotationPathsHomologous <- function() hpaths
        annotationPathsOrthologous <- function() opaths
        titlesHomologous <- function() htitles
        titlesOrthologous <- function() otitles
        activePlotIdsHomologous <- function() names(fileDataHomologous())
        activePlotIdsOrthologous <- function() names(fileDataOrthologous())
        get_chart_plot_data <- function(pid, ctx) {
            if (ctx == 'homo') list(file_data = fileDataHomologous()[[pid]], org_info = organismInfoHomologous()[[pid]])
            else list(file_data = fileDataOrthologous()[[pid]], org_info = organismInfoOrthologous()[[pid]])
        }
        extract_title_field <- function(title, field) title
        resolve_taxid_for_go_lookup <- function(...) 9606L
        build_string_error_widget <- function(label, ...) list(error = label)
        build_string_info_widget <- function(...) list(loading = TRUE)
        input <- list(app_theme = 'light')
        observe <- function(...) NULL
    }), e)
    if (reactive) {
        e$fileDataHomologous <- reactiveVal(e$hfiles)
        e$fileDataOrthologous <- reactiveVal(e$ofiles)
        e$titlesHomologous <- reactiveVal(e$htitles)
        e$observe <- shiny::observe
    }
    eval(builder, e)
    e
}
await <- function(p, timeout = 30) {
    done <- FALSE; result <- failure <- NULL
    then(p, function(x) { result <<- x; done <<- TRUE; NULL },
         function(e) { failure <<- e; done <<- TRUE; NULL })
    deadline <- Sys.time() + timeout
    while (!done && Sys.time() < deadline) later::run_now(0.01)
    stopifnot(done)
    if (!is.null(failure)) stop(failure)
    result
}
submit <- function(batches, timing = TRUE) {
    future_promise(string_screen_future_worker(screen_batches, timing),
                   globals = string_screen_future_globals(batches, timing), packages = 'stringr', seed = TRUE)
}
future::plan(future::multisession, workers = I(1L))
warm <- await(submit(list('ID=warm')))
stopifnot(warm$timing$pid != Sys.getpid())

# Inspect the actual explicit FutureGlobals, including a deliberately heavy lib env.
lib_env <- new.env(parent = baseenv())
sys.source('R/string_annotation.R', lib_env)
lib_env$unrelated_cache <- raw(10 * 1024^2)
batches <- list(unique(unlist(lapply(fixtures, function(d) as.character(d$V9)))), edge_attrs)
globals <- lib_env$string_screen_future_globals(batches, TRUE)
inspection <- future::getGlobalsAndPackages(
    quote(string_screen_future_worker(screen_batches, timing)),
    globals = globals, envir = lib_env)
stopifnot(identical(names(inspection$globals), c('string_screen_future_worker', 'screen_batches', 'timing')),
          identical(environment(globals$string_screen_future_worker), baseenv()),
          !any(vapply(globals, is.environment, logical(1))),
          length(serialize(globals, NULL)) - length(serialize(batches, NULL)) < 10000L)
cat('FutureGlobals totalSize:', attr(inspection$globals, 'total_size'),
    'serialized globals:', length(serialize(globals, NULL)),
    'input:', length(serialize(batches, NULL)), '\n')
remote <- await(submit(batches))
stopifnot(identical(remote$rows, lapply(batches, string_prepare_screen_rows)))
cat('main PID:', Sys.getpid(), 'worker PID:', remote$timing$pid,
    'worker compute ms:', remote$timing$compute_ms,
    'serialized output:', length(serialize(remote$rows, NULL)), '\n')

# Frozen baseline is optional locally; frozen pre-PR3 payload remains an independent oracle.
for (pid in as.character(seq_along(genes))) {
    e <- make_env()
    ref <- new.env(parent = e)
    sys.source('tests/fixtures/string_query_payload_pre_pr3.R', ref)
    before <- ref$build_string_query_payload(pid, 'homo')
    plans <- e$capture_string_screen_plans(pid, 'homo')
    result <- await(submit(lapply(plans, function(p) p$attrs)))
    for (i in seq_along(plans)) e$stringAnnotationState$screen_merge(plans[[i]], result$rows[[i]])
    # A production continuation must perform no screen computation in main.
    main_screen_calls <- 0L
    after <- e$build_string_query_payload(pid, 'homo')
    stopifnot(identical(before, after), main_screen_calls == 0L)
    warm_plans <- e$capture_string_screen_plans(pid, 'homo')
    stopifnot(all(lengths(lapply(warm_plans, function(p) p$attrs)) == 0L))
    cat(genes[as.integer(pid)], 'complete query payload/order parity: PASS;',
        length(after$id_candidates), 'candidate IDs;', length(after$screen_variants), 'screen IDs\n')
}

# A delayed real worker gives the observer time to invalidate its request.
# Only transport is wrapped; production capture/merge/continuation code runs.
run_observer_case <- function(mode) shiny::testServer(function(input, output, session) {
    env <- make_env(TRUE)
    env$input <- input; env$session <- session
    env$selectedChartPlotId <- reactiveVal(NULL); env$selectedChartContext <- reactiveVal('homo')
    env$stringNetworkWidget <- reactiveVal(NULL); env$stringNetworkData <- reactiveVal(NULL)
    env$stringNetworkRequestState <- new.env(parent = emptyenv()); env$stringNetworkRequestState$id <- ''
    env$pendingStringPromises <- new.env(parent = emptyenv())
    env$normalize_plot_context <- identity; env$chart_info_tip <- function(...) NULL
    env$showModal <- function(...) NULL
    env$widget_inputs <- list(); env$requests <- list(); env$merges <- 0L
    merge <- env$stringAnnotationState$screen_merge
    env$stringAnnotationState$screen_merge <- function(...) { env$merges <- env$merges + 1L; merge(...) }
    eval(widget, env)
    env$build_string_network_widget_from_data_actual <- env$build_string_network_widget_from_data
    env$build_string_network_widget_from_data <- function(payload, ...) {
        env$widget_inputs[[length(env$widget_inputs) + 1L]] <- payload
        env$build_string_network_widget_from_data_actual(payload, ...)
    }
    env$string_try_cached_payload <- function(snapshot, ...) {
        env$requests[[length(env$requests) + 1L]] <- snapshot
        if (startsWith(mode, 'http_')) return(NULL)
        list(resolved_id = paste0('9606.', snapshot$gene_name), taxid = 9606L,
             nodes = data.frame(id = paste0('9606.', c(snapshot$gene_name, 'NEIGHBOR')),
                                label = c(snapshot$gene_name, 'NEIGHBOR')),
             edges = data.frame(from = paste0('9606.', snapshot$gene_name), to = '9606.NEIGHBOR', score = .9))
    }
    # Role mapping uses the existing function with a cache-miss-only resolver to
    # keep this observer test offline. Full HTTP/cache semantics have separate tests.
    roles_env <- new.env(parent = globalenv())
    roles_env$string_network_display_terms <- function(taxid, screen_candidates, ...) string_role_variants(screen_candidates)
    env$string_apply_display_roles <- string_apply_display_roles
    environment(env$string_apply_display_roles) <- roles_env
    env$string_future_globals <- function(snapshot) {
        if (mode == 'http_launch_error') stop('injected HTTP launch failure')
        if (mode == 'http_error') snapshot$gene_name <- '__FAIL__'
        worker <- function(query_payload, cache_snapshot, base_dir) {
            Sys.sleep(.2)
            gene <- query_payload$gene_name
            if (gene == '__FAIL__') stop('injected HTTP worker failure')
            list(ok = TRUE, cache_hit = FALSE, payload = list(
                resolved_id = paste0('9606.', gene), taxid = 9606L,
                nodes = data.frame(id = paste0('9606.', c(gene, 'NEIGHBOR')), label = c(gene, 'NEIGHBOR')),
                edges = data.frame(from = paste0('9606.', gene), to = '9606.NEIGHBOR', score = .9)))
        }
        environment(worker) <- baseenv()
        list(string_future_worker = worker, query_payload = snapshot, cache_snapshot = list())
    }
    env$jobs <- list()
    env$string_screen_future_globals <- function(screen_batches, timing) {
        i <- length(env$jobs) + 1L
        env$jobs[[i]] <- list(batches = screen_batches)
        if (mode == 'launch_error') stop('injected launch failure')
        list(string_screen_future_worker = structure(function(screen_batches, timing) {
            Sys.sleep(0.2)
            if (screen_batches[[1L]][1L] == '__FAIL__') stop('injected worker failure')
            worker_env <- new.env(parent = baseenv())
            sys.source('R/utils.R', worker_env); sys.source('R/string_annotation.R', worker_env)
            worker_env$string_screen_future_worker(screen_batches, timing)
        }, class = 'function'), screen_batches = if (mode == 'worker_error') list('__FAIL__') else screen_batches, timing = timing)
    }
    # Bind injected transport to baseenv just like production, never test closures.
    orig_globals <- env$string_screen_future_globals
    env$string_screen_future_globals <- function(...) {
        g <- orig_globals(...); environment(g$string_screen_future_worker) <- baseenv(); g
    }
    eval(observers, env)
}, {
    session$setInputs(app_theme = 'light')
    calls_before <- main_screen_calls
    session$setInputs(open_string_network = list(id = '1', context = 'homo'))
    stopifnot(length(env$jobs) == 1L)
    if (mode == 'closed') session$setInputs(string_network_closed = 1)
    if (mode == 'changed') {
        changed <- env$fileDataHomologous(); changed[['2']]$V9[1] <- 'ID=replaced;Name=replaced'
        env$fileDataHomologous(changed); session$flushReact()
    }
    if (mode == 'navigated') env$selectedChartPlotId('2')
    if (mode %in% c('changed', 'navigated')) env$stringNetworkWidget(list(newer = TRUE))
    if (mode == 'superseded') session$setInputs(open_string_network = list(id = '2', context = 'homo'))
    if (mode == 'session_closed') session$close()
    if (mode == 'http_closed') {
        until <- Sys.time() + 15
        while (!length(env$requests) && Sys.time() < until) later::run_now(.01)
        stopifnot(length(env$requests) == 1L, !length(env$widget_inputs))
        session$setInputs(string_network_closed = 1)
    }
    deadline <- Sys.time() + 15
    while (Sys.time() < deadline) {
        later::run_now(.02)
        if (mode %in% c('success', 'superseded', 'http_success') && length(env$widget_inputs)) break
        if (mode %in% c('worker_error', 'launch_error', 'http_error', 'http_launch_error') && !is.null(env$stringNetworkWidget()$error)) break
        if (mode %in% c('closed', 'changed', 'navigated', 'session_closed', 'http_closed') && Sys.time() > deadline - 13) break
    }
    if (mode %in% c('success', 'superseded', 'http_success')) {
        stopifnot(length(env$requests) == 1L, length(env$widget_inputs) == 1L, env$merges == 2L,
                  identical(env$requests[[1]]$gene_name, if (mode == 'superseded') 'TP53' else 'ACTB'),
                  inherits(env$stringNetworkWidget(), 'visNetwork'),
                  identical(env$widget_inputs[[1]]$nodes$role, c('target', 'neighbor')))
    } else if (startsWith(mode, 'http_')) {
        stopifnot(length(env$requests) == 1L, env$merges == 2L,
                  !length(env$widget_inputs), length(ls(env$pendingStringPromises)) == 0L)
        if (mode == 'http_closed') stopifnot(is.null(env$stringNetworkWidget()))
        else stopifnot(grepl('STRING lookup failed', env$stringNetworkWidget()$error))
    } else {
        stopifnot(length(env$requests) == 0L, env$merges == 0L)
        if (mode %in% c('changed', 'navigated')) stopifnot(identical(env$stringNetworkWidget(), list(newer = TRUE)))
        if (mode %in% c('worker_error', 'launch_error')) {
            stopifnot(grepl('STRING lookup failed', env$stringNetworkWidget()$error))
            plans <- env$capture_string_screen_plans('1', 'homo')
            stopifnot(any(lengths(lapply(plans, function(p) p$attrs)) > 0L))
        }
    }
    stopifnot(main_screen_calls == calls_before)
    cat('Production observer:', mode, 'PASS\n')
})
for (mode in c('success', 'closed', 'changed', 'navigated', 'superseded', 'worker_error', 'launch_error', 'session_closed', 'http_success', 'http_error', 'http_launch_error', 'http_closed')) {
    run_observer_case(mode)
}
future::plan(future::sequential)
cat('string-screen-async-ok\n')
