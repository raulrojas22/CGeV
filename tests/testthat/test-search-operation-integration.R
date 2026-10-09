library(testthat)
if (!exists('ownership_harness')) source(if (file.exists('tests/testthat/helper-search-ownership.R'))
    'tests/testthat/helper-search-ownership.R' else 'helper-search-ownership.R')

test_that('actual Shiny observers preserve normal local searches and duplicate admission', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    f$session$setInputs(filter1 = 'TP53', generate1 = 1)
    expect_identical(f$state('homologous')$status, 'completed')
    expect_identical(f$e$lookups, 1L)
    expect_match(shiny::isolate(f$e$searchStatusHomologous()), 'not found')
    a <- f$state('homologous')$id
    f$session$setInputs(generate1 = 2)
    expect_identical(f$state('homologous')$id, a)
    expect_identical(f$e$lookups, 1L)
    expect_null(f$e$stateHelpersDomain$get_search_request_id('homologous'))
    f$session$setInputs(gene_name = 'TP53', search_gene = 1)
    expect_identical(f$state('orthologous')$status, 'completed')
    expect_identical(f$e$lookups, 3L)
})

test_that('definitive preflight rejection retires only its own pending request', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    for (mode in c('homologous', 'orthologous')) {
        a <- f$e$begin_search_run(mode, 'a')
        f$e$start_search_child(mode, a$id)
        f$e$release_search_admission(mode, a$id)
        execute <- if (mode == 'homologous') f$e$execute_homologous_search else f$e$execute_orthologous_search
        execute('')
        if (mode == 'homologous') f$session$setInputs(cgv_homo_search_dispatch = f$ticket(mode))
        else f$session$setInputs(cgv_ortho_search_dispatch = f$ticket(mode))
        expect_null(f$e$stateHelpersDomain$get_search_request_id(mode))
        expect_true(f$e$owns_search_run(mode, a$id))
        f$e$cancel_search_operation(mode, a$id)
    }
})

test_that('confirmation close acknowledgements retire pending intents but never accepted operations', {
    f <- ownership_harness(observers = FALSE); on.exit(f$close(), add = TRUE)
    for (mode in c('homologous', 'orthologous')) {
        a <- f$e$request_search_run(mode)
        b <- f$e$request_search_run(mode)
        f$session$setInputs(cgv_search_confirmation_closed = list(mode = mode, request_id = a$id))
        expect_true(f$e$search_request_is_current(mode, b$id))
        accepted <- f$e$begin_search_run(mode, 'b', request_id = b$id)
        f$session$setInputs(cgv_search_confirmation_closed = list(mode = mode, request_id = b$id))
        expect_true(f$e$owns_search_run(mode, accepted$id))
        f$e$release_search_admission(mode, accepted$id)
        c <- f$e$request_search_run(mode)
        f$session$setInputs(cgv_search_confirmation_closed = list(mode = mode, request_id = c$id))
        expect_false(f$e$search_request_is_current(mode, c$id))
        expect_identical(f$state(mode)$status, 'completed')
    }
})

test_that('both actual observers reject busy events before any accepted-operation reset', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    for (mode in c('homologous', 'orthologous')) {
        a <- f$e$begin_search_run(mode, 'gene::A')
        before <- f$state(mode); clears <- f$e$clears
        rescue <- f$e$orthoExternalRescueState$id
        if (mode == 'homologous') f$session$setInputs(filter1 = 'B', generate1 = 1)
        else f$session$setInputs(gene_name = 'B', search_gene = 1)
        expect_identical(f$state(mode), before)
        expect_identical(f$e$clears, clears)
        expect_identical(f$e$orthoExternalRescueState$id, rescue)
        f$e$cancel_search_operation(mode, a$id)
    }
})

test_that('busy global batch and external-alias handlers preserve accepted ownership and status', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    f$e$preferredSearchWorkflow <- function() 'homologous'
    f$e$parse_batch_gene_query <- function(...) list(genes = c('B', 'C'), truncated = FALSE)
    f$e$parse_batch_gene_payload <- function(...) list(genes = character(), truncated = FALSE)
    f$session$setInputs(navtabs = 'homologous')
    for (mode in c('homologous', 'orthologous')) {
        a <- f$e$begin_search_run(mode, 'a')
        status <- if (mode == 'homologous') f$e$searchStatusHomologous else f$e$searchStatusOrthologous
        status('A running')
        before <- f$state(mode); generation <- f$e$searchBatchOwner$generation
        shiny::isolate(f$e$run_global_search('B,C'))
        f$session$setInputs(partial_gene_external_alias_search = list(mode = mode, gene = 'B'))
        expect_identical(f$state(mode), before)
        expect_identical(shiny::isolate(status()), 'A running')
        expect_identical(f$e$searchBatchOwner$generation, generation)
        expect_null(f$e$stateHelpersDomain$get_search_request_id(mode))
        f$e$cancel_search_operation(mode, a$id)
    }
})

test_that('external-alias intent belongs to the request and cannot leak to a superseding normal search', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    allowed <- list()
    family_checks <- 0L
    f$e$is_low_specific_gene_family_query <- function(...) {family_checks <<- family_checks + 1L; FALSE}
    f$e$run_lookup_pipeline <- function(..., allow_partial_suggestions) {
        allowed[[length(allowed) + 1L]] <<- allow_partial_suggestions
        list(data = NULL)
    }
    f$e$run_orthologous_lookup_job_pure <- function(job) {
        allowed[[length(allowed) + 1L]] <<- job$allow_partial_suggestions
        list(file_idx = job$file_idx, file_label = job$file_label, found = FALSE, lookup = list())
    }
    for (mode in c('homologous', 'orthologous')) {
        f$session$setInputs(partial_gene_external_alias_search = list(mode = mode, gene = 'same'))
        stale <- f$ticket(mode)
        if (mode == 'homologous') f$session$setInputs(filter1 = 'same', generate1 = 1)
        else f$session$setInputs(gene_name = 'same', search_gene = 1)
        if (mode == 'homologous') expect_true(all(unlist(allowed)))
        expect_gt(family_checks, 0L)
        allowed <- list(); family_checks <- 0L
        if (mode == 'homologous') f$session$setInputs(cgv_homo_search_dispatch = stale)
        else f$session$setInputs(cgv_ortho_search_dispatch = stale)
        expect_length(allowed, 0L)
        f$session$setInputs(partial_gene_external_alias_search = list(mode = mode, gene = 'explicit'))
        live <- f$ticket(mode)
        if (mode == 'homologous') f$session$setInputs(cgv_homo_search_dispatch = live)
        else f$session$setInputs(cgv_ortho_search_dispatch = live)
        expect_gt(length(allowed), 0L)
        expect_true(all(!unlist(allowed)))
        expect_identical(family_checks, 0L); allowed <- list()
    }
})

test_that('valid organism/gene confirmation preserves owned external-alias intent', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    f$e$build_app_confirmation_modal <- function(...) NULL
    f$e$run_clear_selected_visualizations <- function(active_panel, ...) {
        if (active_panel == 'homologous') f$e$fileDataHomologous(list())
        else f$e$fileDataOrthologous(list())
    }
    f$e$fileDataHomologous(list(old = list()))
    f$e$lastConfirmedHomoDataMode('upload')
    f$e$fileDataOrthologous(list(old = list()))
    f$e$lastOrthoSearchedGene('old')
    for (mode in c('homologous', 'orthologous')) {
        f$session$setInputs(partial_gene_external_alias_search = list(mode = mode, gene = 'explicit'))
        before <- f$ticket(mode)
        if (mode == 'homologous') f$session$setInputs(cgv_homo_search_dispatch = before)
        else f$session$setInputs(cgv_ortho_search_dispatch = before)
        expect_true(f$e$search_request_is_current(mode, before$request_id))
        if (mode == 'homologous') f$session$setInputs(confirm_homo_organism_change_btn = 1)
        else f$session$setInputs(confirm_ortho_gene_change_btn = 1)
        after <- f$ticket(mode)
        expect_false(identical(after$request_id, before$request_id))
        expect_identical(after$origin, 'external_alias')
        f$session$setInputs(cgv_search_confirmation_closed = list(mode = mode, request_id = before$request_id))
        expect_true(f$e$search_request_is_current(mode, after$request_id))
        if (mode == 'homologous') f$session$setInputs(cgv_homo_search_dispatch = after)
        else f$session$setInputs(cgv_ortho_search_dispatch = after)
        expect_identical(f$state(mode)$status, 'completed')
    }
})

test_that('delayed browser acknowledgements after supersession or cancellation cannot launch', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    for (mode in c('homologous', 'orthologous')) {
        execute <- if (mode == 'homologous') f$e$execute_homologous_search else f$e$execute_orthologous_search
        execute('A'); a <- f$ticket(mode)
        execute('B'); b <- f$ticket(mode)
        send <- function(ticket) {
            if (mode == 'homologous') f$session$setInputs(cgv_homo_search_dispatch = ticket)
            else f$session$setInputs(cgv_ortho_search_dispatch = ticket)
        }
        before <- f$e$lookups; send(a)
        expect_identical(f$e$lookups, before)
        expect_true(f$e$search_request_is_current(mode, b$request_id))
        f$e$cancel_search_operation(mode, b$request_id); send(b)
        expect_identical(f$e$lookups, before)
        execute('A'); send(f$ticket(mode))
        expect_gt(f$e$lookups, before)
    }
})

test_that('cancel during actual preparation suppresses readiness and admits the same gene', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    for (mode in c('homologous', 'orthologous')) {
        ready <- if (mode == 'homologous') f$e$searchPreparationReadyHomo else f$e$searchPreparationReadyOrtho
        execute <- if (mode == 'homologous') f$e$execute_homologous_search else f$e$execute_orthologous_search
        send <- function(ticket) {
            if (mode == 'homologous') f$session$setInputs(cgv_homo_search_dispatch = ticket)
            else f$session$setInputs(cgv_ortho_search_dispatch = ticket)
        }
        ready(FALSE); execute('same'); a <- f$ticket(mode); send(a)
        pending <- shiny::isolate(f$e$preparation_state_for_mode(mode)$pending())
        expect_identical(pending$request_id, a$request_id)
        before <- length(f$e$messages)
        expect_true(f$e$cancel_search_operation(mode, a$request_id))
        ready(TRUE); f$session$flushReact()
        expect_length(f$e$messages, before)
        execute('same'); send(f$ticket(mode))
        expect_identical(f$state(mode)$status, 'completed')
    }
})

test_that('old cleanup and deferred batch dispatch cannot consume a newer batch', {
    f <- ownership_harness(observers = FALSE); on.exit(f$close(), add = TRUE)
    shiny::isolate({
        f$e$set_batch_search_state('homologous', 2L)
        old_batch <- f$e$searchBatchOwner$generation
        a <- f$e$begin_search_run('homologous', 'a')
        f$e$batchSearchQueue(c('a-next'))
        f$e$dispatch_next_batch_search('homologous', 0.01, old_batch, a$id)
        f$e$release_search_admission('homologous', a$id)
        f$e$set_batch_search_state('homologous', 2L)
        f$e$batchSearchQueue(c('b-next'))
        b <- f$e$begin_search_run('homologous', 'b')
        expect_false(f$e$finalize_search_cleanup('homologous', a$id, old_batch))
        later::run_now(0.02)
        expect_identical(f$e$batchSearchQueue(), 'b-next')
        expect_identical(f$e$batchSearchRemaining(), 2L)
        expect_identical(f$e$clears, 0L)
        expect_true(f$e$owns_search_run('homologous', b$id))
    })
})

test_that('batch generation protects B even while released A still owns rescue', {
    f <- ownership_harness(observers = FALSE); on.exit(f$close(), add = TRUE)
    shiny::isolate({
        f$e$set_batch_search_state('orthologous', 2L)
        old_batch <- f$e$searchBatchOwner$generation
        a <- f$e$begin_search_run('orthologous', 'a')
        f$e$start_search_child('orthologous', a$id)
        f$e$release_search_admission('orthologous', a$id)
        f$e$set_batch_search_state('homologous', 2L)
        f$e$batchSearchQueue('b-next')
        expect_true(f$e$owns_search_run('orthologous', a$id))
        expect_false(f$e$finalize_search_cleanup('orthologous', a$id, old_batch))
        expect_identical(f$e$batchSearchQueue(), 'b-next')
        expect_identical(f$e$clears, 0L)
    })
})

test_that('stale confirmation and catalog acknowledgements cannot clear or retrigger', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    f$e$run_clear_selected_visualizations <- function(...) stop('stale confirmation cleared results')
    f$e$removeModal <- function(...) NULL
    for (mode in c('homologous', 'orthologous')) {
        a <- f$e$request_search_run(mode)
        pending <- list(gene = 'a', request_id = a$id)
        if (mode == 'homologous') f$e$pendingHomoOrganismChange(pending) else f$e$pendingOrthoGeneChange(pending)
        b <- f$e$request_search_run(mode)
        if (mode == 'homologous') {
            f$session$setInputs(confirm_homo_organism_change_btn = 1,
                catalog_multigene_ready = list(species_id = 'a', gene = 'a', request_id = a$id))
        } else {
            f$session$setInputs(confirm_ortho_gene_change_btn = 1,
                catalog_cross_ready = list(species_ids = c('a', 'b'), gene = 'a', request_id = a$id))
        }
        expect_true(f$e$search_request_is_current(mode, b$id))
        expect_identical(f$e$lookups, 0L)
    }
})

test_that('two actual session orchestration instances keep cancellation isolated', {
    a <- ownership_harness(observers = FALSE); b <- ownership_harness(observers = FALSE)
    on.exit({a$close(); b$close()}, add = TRUE)
    x <- a$e$begin_search_run('homologous', 'same'); y <- b$e$begin_search_run('homologous', 'same')
    touched <- character()
    ca <- a$e$search_operation_guard('homologous', x$id, function() touched <<- c(touched, 'A'))
    cb <- b$e$search_operation_guard('homologous', y$id, function() touched <<- c(touched, 'B'))
    a$e$session_later(ca); b$e$session_later(cb)
    a$e$cancel_search_operation('homologous', x$id)
    later::run_now(0)
    expect_identical(touched, 'B')
    expect_true(b$e$owns_search_run('homologous', y$id))
})

test_that('deferred server dispatch is suppressed after supersession and teardown', {
    f <- ownership_harness(observers = FALSE); on.exit(f$close(), add = TRUE)
    launched <- 0L
    f$e$schedule_search_dispatch('homologous', function() launched <<- launched + 1L, 0.01)
    f$e$request_search_run('homologous')
    later::run_now(0.02); expect_identical(launched, 0L)
    f$e$schedule_search_dispatch('homologous', function() stop('closed session entered'), 0.01)
    f$session$close(); expect_no_error(later::run_now(0.02))
})

test_that('actual external rescue settles after cancellation without publishing or ending B', {
    for (reject in c(FALSE, TRUE)) {
        f <- ownership_harness(); on.exit(f$close(), add = TRUE)
        f$e$get_enabled_external_alias_sources <- function() 'fixture-source'
        settlements <- list()
        f$e$search_future_promise <- function(...) {
            f$e$rescue_submissions <- f$e$rescue_submissions + 1L
            promises::promise(function(resolve, reject_fn) {
                settlements[[length(settlements) + 1L]] <<- if (reject) reject_fn else resolve
            })
        }
        f$session$setInputs(gene_name = 'A', search_gene = 1)
        a <- f$state('orthologous')$id
        expect_identical(f$state('orthologous')$status, 'running')
        expect_false(f$state('orthologous')$active)
        later::run_now(0.08)
        expect_gt(length(settlements), 0L)
        f$e$cancel_search_operation('orthologous', a)
        b <- f$e$begin_search_run('orthologous', 'B')
        before <- f$state('orthologous'); status <- shiny::isolate(f$e$searchStatusOrthologous())
        for (settle in settlements) settle(if (reject) simpleError('late error') else list())
        for (i in 1:8) later::run_now(0.01)
        expect_identical(f$state('orthologous'), before)
        expect_identical(shiny::isolate(f$e$searchStatusOrthologous()), status)
        expect_true(f$e$owns_search_run('orthologous', b$id))
        f$close()
    }
})

test_that('cancellation before the actual rescue timer prevents worker submission', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    f$e$get_enabled_external_alias_sources <- function() 'fixture-source'
    f$e$search_future_promise <- function(...) stop('unsubmitted rescue launched')
    f$session$setInputs(gene_name = 'A', search_gene = 1)
    expect_identical(f$state('orthologous')$status, 'running')
    expect_true(f$e$cancel_search_operation('orthologous', f$state('orthologous')$id))
    expect_no_error(later::run_now(0.08))
    expect_identical(f$e$rescue_submissions, 0L)
})

test_that('live external rescue success and error reach the correct terminal state', {
    for (reject in c(FALSE, TRUE)) {
        f <- ownership_harness(); on.exit(f$close(), add = TRUE)
        f$e$get_enabled_external_alias_sources <- function() 'fixture-source'
        settlements <- list()
        f$e$search_future_promise <- function(...) {
            promises::promise(function(resolve, reject_fn) {
                settlements[[length(settlements) + 1L]] <<- if (reject) reject_fn else resolve
            })
        }
        f$session$setInputs(gene_name = 'A', search_gene = 1)
        later::run_now(0.08)
        expect_gt(length(settlements), 0L)
        for (settle in settlements) settle(if (reject) simpleError('fixture error') else list())
        for (i in 1:8) later::run_now(0.01)
        expect_identical(f$state('orthologous')$status, if (reject) 'failed' else 'completed')
        expect_identical(f$state('orthologous')$children, 0L)
        expect_match(shiny::isolate(f$e$searchStatusOrthologous()), if (reject) 'fixture error' else 'not found')
        f$close()
    }
})

test_that('actual batch searches still drain once and emit their summary', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    shiny::isolate({
        f$e$set_batch_search_state('homologous', 2L)
        f$e$queue_global_batch_search(c('A', 'B'), 'homologous')
    })
    later::run_now(0.1)
    f$session$setInputs(cgv_homo_search_dispatch = f$ticket('homologous'))
    expect_identical(f$state('homologous')$status, 'completed')
    expect_identical(shiny::isolate(f$e$batchSearchRemaining()), 1L)
    later::run_now(0.12)
    f$session$setInputs(cgv_homo_search_dispatch = f$ticket('homologous'))
    expect_identical(f$e$lookups, 2L)
    expect_identical(shiny::isolate(f$e$batchSearchQueue()), character())
    expect_identical(shiny::isolate(f$e$batchSearchRemaining()), 0L)
    expect_identical(f$e$clears, 1L)
    expect_match(shiny::isolate(f$e$searchStatusHomologous()), 'Multi-gene batch completed')
})

test_that('a failed rescue chunk cannot suppress a still-running sibling', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    f$e$get_enabled_external_alias_sources <- function() 'fixture-source'
    settlements <- list(); processed <- integer()
    f$e$set_ortho_organism_lookup_status <- function(file_idx = NULL, status = NULL, ...) {
        if (identical(status, 'not_found')) processed <<- c(processed, file_idx)
    }
    local_mocked_bindings(nbrOfWorkers = function(...) 2L, .package = 'future')
    f$e$search_future_promise <- function(...) promises::promise(function(resolve, reject) {
        settlements[[length(settlements) + 1L]] <<- list(resolve = resolve, reject = reject)
    })
    f$session$setInputs(gene_name = 'A', search_gene = 1)
    later::run_now(0.08)
    expect_length(settlements, 2L)
    settlements[[1L]]$reject(simpleError('chunk one failed'))
    for (i in 1:8) later::run_now(0.01)
    expect_identical(f$state('orthologous')$status, 'running')
    expect_identical(f$state('orthologous')$children, 1L)
    settlements[[2L]]$resolve(list(list(found = FALSE, file_idx = 2L, lookup = list())))
    for (i in 1:8) later::run_now(0.01)
    expect_true(2L %in% processed)
    expect_identical(f$state('orthologous')$status, 'failed')
    expect_identical(f$state('orthologous')$children, 0L)
})

test_that('owned cleanup is idempotent while rescue remains running', {
    f <- ownership_harness(observers = FALSE); on.exit(f$close(), add = TRUE)
    shiny::isolate({
        f$e$set_batch_search_state('homologous', 2L)
        a <- f$e$begin_search_run('homologous', 'a')
        batch <- f$e$bind_search_batch_owner('homologous', a$id, 'global_batch')
        f$e$start_search_child('homologous', a$id)
        f$e$batchSearchQueue('b')
        f$e$finalize_search_cleanup('homologous', a$id, batch)
        expect_identical(f$e$batchSearchRemaining(), 1L)
        expect_false(f$e$finalize_search_cleanup('homologous', a$id, batch))
        expect_identical(f$e$batchSearchRemaining(), 1L)
    })
})

test_that('preparation retries preserve the exact reference-locus payload', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    anchor <- list(local_gene_id = 'gene-LOC42', organism_id = 'a', organism_name = 'A', query = 'alias')
    f$e$pendingOrthoReferenceAnchor(anchor)
    f$e$searchPreparationReadyOrtho(FALSE)
    f$e$execute_orthologous_search('alias', 'orthology_reference')
    ticket <- f$ticket('orthologous')
    f$session$setInputs(cgv_ortho_search_dispatch = ticket)
    f$e$searchPreparationReadyOrtho(TRUE); f$session$flushReact()
    expect_identical(f$ticket('orthologous')$request_id, ticket$request_id)
    expect_identical(f$e$searchRequestPayload$orthologous$reference_anchor, anchor)
    f$e$cancel_search_operation('orthologous', ticket$request_id)
    f$e$execute_orthologous_search('alias')
    expect_null(f$e$searchRequestPayload$orthologous$reference_anchor)
})

test_that('internal cancellation preserves already committed session results', {
    f <- ownership_harness(observers = FALSE); on.exit(f$close(), add = TRUE)
    results <- list('1' = list(gene_id = 'gene-G', coordinates = c(10L, 42L), fasta = '>G\nACGT'))
    f$e$fileDataHomologous(results)
    a <- f$e$begin_search_run('homologous', 'a')
    expect_true(f$e$cancel_search_operation('homologous', a$id))
    expect_false(f$e$cancel_search_operation('homologous', a$id))
    b <- f$e$begin_search_run('homologous', 'a')
    expect_true(b$ok)
    expect_identical(shiny::isolate(f$e$fileDataHomologous()), results)
})

test_that('lazy futures queued behind busy workers are skipped after cancellation or replacement', {
    for (scenario in c('cancelled', 'superseded', 'closed')) {
        f <- ownership_harness(observers = FALSE); on.exit(f$close(), add = TRUE)
        queued <- list(); marker <- tempfile(); on.exit(unlink(marker), add = TRUE)
        queue <- list(schedule_work = function(fn) queued[[length(queued) + 1L]] <<- fn)
        a <- f$e$begin_search_run('orthologous', 'a')
        value <- 'pending'; error <- NULL
        p <- f$e$search_future_promise('orthologous', a$id, {writeLines('ran', marker); 42L}, queue = queue, seed = FALSE)
        promises::then(p, onFulfilled = function(x) value <<- x, onRejected = function(e) error <<- e)
        expect_length(queued, 1L); expect_false(file.exists(marker))
        if (scenario == 'closed') f$session$close()
        else {
            f$e$cancel_search_operation('orthologous', a$id)
            if (scenario == 'superseded') f$e$begin_search_run('orthologous', 'b')
        }
        # This is the same schedule_work callback that the shared queue invokes
        # when capacity becomes available; the captured Future itself is real.
        expect_no_error(queued[[1L]]())
        for (i in 1:8) later::run_now(0.01)
        expect_false(file.exists(marker)); expect_null(value); expect_null(error)
        f$close()
    }
})

test_that('the actual shared future queue still submits live work normally', {
    f <- ownership_harness(observers = FALSE); on.exit(f$close(), add = TRUE)
    a <- f$e$begin_search_run('orthologous', 'a'); value <- NULL; error <- NULL
    p <- f$e$search_future_promise('orthologous', a$id, 42L, seed = FALSE)
    promises::then(p, onFulfilled = function(x) value <<- x, onRejected = function(e) error <<- e)
    for (i in 1:20) later::run_now(0.01)
    expect_identical(value, 42L); expect_null(error)
    expect_true(f$e$owns_search_run('orthologous', a$id))
})

test_that('skipping A through the real shared queue leaves session B runnable', {
    a <- ownership_harness(observers = FALSE); b <- ownership_harness(observers = FALSE)
    on.exit({a$close(); b$close()}, add = TRUE)
    x <- a$e$begin_search_run('orthologous', 'a'); y <- b$e$begin_search_run('orthologous', 'b')
    queue <- list(schedule_work = function(fn) {
        a$e$cancel_search_operation('orthologous', x$id)
        promises::future_promise_queue()$schedule_work(fn)
    })
    av <- bv <- 'pending'; errors <- list()
    pa <- a$e$search_future_promise('orthologous', x$id, stop('cancelled shared job launched'), queue = queue, seed = FALSE)
    pb <- b$e$search_future_promise('orthologous', y$id, 42L, seed = FALSE)
    promises::then(pa, onFulfilled = function(x) av <<- x, onRejected = function(e) errors[[length(errors)+1L]] <<- e)
    promises::then(pb, onFulfilled = function(x) bv <<- x, onRejected = function(e) errors[[length(errors)+1L]] <<- e)
    for (i in 1:20) later::run_now(0.01)
    expect_null(av); expect_identical(bv, 42L); expect_length(errors, 0L)
    expect_true(b$e$owns_search_run('orthologous', y$id))
})

test_that('initial queued batch dispatch cannot consume a newer batch without an operation yet', {
    f <- ownership_harness(observers = FALSE); on.exit(f$close(), add = TRUE)
    shiny::isolate({
        f$e$set_batch_search_state('homologous', 2L)
        f$e$queue_global_batch_search(c('a', 'a2'), 'homologous')
        f$e$set_batch_search_state('homologous', 2L)
        f$e$batchSearchQueue('b')
    })
    later::run_now(0.1)
    expect_identical(shiny::isolate(f$e$batchSearchQueue()), 'b')
    expect_length(f$e$messages, 0L)
})

test_that('existing progressive local promises complete, fail and suppress cancelled continuations', {
    withr::local_envvar(c(APP_ORTHO_LOCAL_PROGRESSIVE = '1'))
    for (scenario in c('completed', 'failed', 'cancelled', 'setup_failure')) {
        f <- ownership_harness(); on.exit(f$close(), add = TRUE)
        settlements <- list(); call_count <- 0L
        f$e$search_future_promise <- function(...) {
            call_count <<- call_count + 1L
            if (scenario == 'setup_failure' && call_count == 2L) stop('fixture lazy-capture failure')
            promises::promise(function(resolve, reject) {
                settlements[[length(settlements) + 1L]] <<- list(resolve = resolve, reject = reject)
            })
        }
        if (scenario == 'setup_failure') {
            expect_warning(f$session$setInputs(gene_name = 'A', search_gene = 1), 'fixture lazy-capture failure')
            expect_identical(f$state('orthologous')$status, 'failed')
        } else {
            f$session$setInputs(gene_name = 'A', search_gene = 1)
            expect_true(f$state('orthologous')$active)
            expect_length(settlements, 2L)
            if (scenario == 'cancelled') f$e$cancel_search_operation('orthologous', f$state('orthologous')$id)
        }
        before <- shiny::isolate(f$e$searchStatusOrthologous())
        for (j in seq_along(settlements)) {
            if (scenario == 'failed' && j == 1L) settlements[[j]]$reject(simpleError('fixture local error'))
            else settlements[[j]]$resolve(list(file_idx = j, found = FALSE, lookup = list()))
        }
        for (i in 1:8) later::run_now(0.01)
        expect_identical(f$state('orthologous')$status, if (scenario == 'setup_failure') 'failed' else scenario)
        expect_false(f$state('orthologous')$active)
        if (scenario %in% c('cancelled', 'setup_failure')) {
            expect_identical(shiny::isolate(f$e$searchStatusOrthologous()), before)
        }
        f$close()
    }
})

test_that('cancelling A invalidates its already-dispatched batch handoff before admission or readiness', {
    for (prepared in c(FALSE, TRUE)) {
        f <- ownership_harness(); on.exit(f$close(), add = TRUE)
        shiny::isolate({
            f$e$set_batch_search_state('orthologous', 2L)
            a <- f$e$begin_search_run('orthologous', 'a')
            batch <- f$e$bind_search_batch_owner('orthologous', a$id, 'global_batch')
            f$e$start_search_child('orthologous', a$id)
            f$e$batchSearchQueue('next')
            f$e$finalize_search_cleanup('orthologous', a$id, batch)
            f$e$release_search_admission('orthologous', a$id)
        })
        later::run_now(0.12)
        ticket <- f$ticket('orthologous')
        if (prepared) {
            f$e$searchPreparationReadyOrtho(FALSE)
            f$session$setInputs(cgv_ortho_search_dispatch = ticket)
        }
        f$e$cancel_search_operation('orthologous', a$id)
        if (prepared) {f$e$searchPreparationReadyOrtho(TRUE); f$session$flushReact()}
        else f$session$setInputs(cgv_ortho_search_dispatch = ticket)
        expect_identical(f$state('orthologous')$id, a$id)
        expect_identical(f$state('orthologous')$status, 'cancelled')
        expect_identical(f$e$lookups, 0L)
        expect_false(f$e$search_request_is_current('orthologous', ticket$request_id))
        f$e$execute_orthologous_search('next', 'local')
        f$session$setInputs(cgv_ortho_search_dispatch = f$ticket('orthologous'))
        expect_identical(f$state('orthologous')$status, 'completed')
        f$close()
    }
})

test_that('an old automatic batch stage waits for a newer independent pending request', {
    for (admit in c(FALSE, TRUE)) {
        f <- ownership_harness(); on.exit(f$close(), add = TRUE)
        shiny::isolate({
            f$e$set_batch_search_state('homologous', 2L)
            a <- f$e$begin_search_run('homologous', 'a')
            batch <- f$e$bind_search_batch_owner('homologous', a$id, 'global_batch')
            f$e$start_search_child('homologous', a$id)
            f$e$batchSearchQueue('a-next')
            f$e$finalize_search_cleanup('homologous', a$id, batch)
            f$e$release_search_admission('homologous', a$id)
        })
        f$e$execute_homologous_search('b', 'local')
        b <- f$ticket('homologous')
        later::run_now(0.12)
        expect_true(f$e$search_request_is_current('homologous', b$request_id))
        expect_identical(shiny::isolate(f$e$batchSearchQueue()), 'a-next')
        expect_identical(f$ticket('homologous')$request_id, b$request_id)
        if (admit) {
            f$session$setInputs(cgv_homo_search_dispatch = b)
            later::run_now(0.03)
            expect_identical(f$ticket('homologous')$request_id, b$request_id)
            expect_identical(f$state('homologous')$id, b$request_id)
        } else {
            f$e$cancel_search_operation('homologous', b$request_id)
            later::run_now(0.03)
            expect_identical(f$e$searchRequestPayload$homologous$gene, 'a-next')
            expect_true(f$e$owns_search_run('homologous', a$id))
        }
        f$close()
    }
})
