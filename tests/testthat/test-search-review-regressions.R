library(testthat)

# The observers, dispatch and confirmation are actual server definitions. Only
# rendering and external providers are replaced; GFF resolution is real.
review_gff_fixture <- function(f) {
    cache_dir <- withr::local_tempdir(.local_envir = parent.frame())
    withr::local_envvar(c(APP_ANNOTATION_DISK_CACHE_DIR = cache_dir), .local_envir = parent.frame())
    paths <- f$e$registry$annotation_path
    writeLines(c('##gff-version 3',
        'chr1\tfixture\tgene\t31\t380\t.\t+\t.\tID=gene-LOC_SELECTED;Name=LOC_SELECTED',
        'chr1\tfixture\tmRNA\t31\t380\t.\t+\t.\tID=tx-selected;Parent=gene-LOC_SELECTED',
        'chr1\tfixture\texon\t31\t380\t.\t+\t.\tID=ex-selected;Parent=tx-selected',
        'chr1\tfixture\tgene\t1001\t1500\t.\t-\t.\tID=gene-BROAD;Name=BROAD',
        'chr1\tfixture\tmRNA\t1001\t1500\t.\t-\t.\tID=tx-broad;Parent=gene-BROAD',
        'chr1\tfixture\texon\t1001\t1500\t.\t-\t.\tID=ex-broad;Parent=tx-broad',
        'chr1\tfixture\tgene\t2001\t2400\t.\t+\t.\tID=gene-ABC1;Name=ABC1',
        'chr1\tfixture\tmRNA\t2001\t2400\t.\t+\t.\tID=tx-abc;Parent=gene-ABC1',
        'chr1\tfixture\texon\t2001\t2400\t.\t+\t.\tID=ex-abc;Parent=tx-abc',
        'chr1\tfixture\tgene\t3001\t3400\t.\t+\t.\tID=gene-TP53;Name=TP53',
        'chr1\tfixture\tmRNA\t3001\t3400\t.\t+\t.\tID=tx-tp53;Parent=gene-TP53',
        'chr1\tfixture\texon\t3001\t3400\t.\t+\t.\tID=ex-tp53;Parent=tx-tp53'), paths[[1L]])
    file.copy(paths[[1L]], paths[[2L]], overwrite = TRUE)
    invisible(lapply(paths, f$e$build_gff_gene_light_index))
    # Source genuine scientific functions into a private cache environment.
    science <- new.env(parent = globalenv())
    sys.source(file.path(ownership_root, 'R/utils.R'), science)
    sys.source(file.path(ownership_root, 'R/gene_search_lib.R'), science)
    f$e$is_low_specific_gene_family_query <- science$is_low_specific_gene_family_query
    f$e$build_app_confirmation_modal <- function(...) NULL
    f$e$run_clear_selected_visualizations <- function(...) f$e$fileDataOrthologous(list())
    results <- list(); jobs <- list()
    f$e$run_orthologous_lookup_job_pure <- function(job) {
        result <- science$run_orthologous_lookup_job_pure(job)
        jobs[[length(jobs) + 1L]] <<- job
        results[[length(results) + 1L]] <<- result
        # Observe real scientific resolution without invoking plotting modules.
        list(file_idx = job$file_idx, file_label = job$file_label, found = FALSE, lookup = list())
    }
    list(science = science, results = function() results, jobs = function() jobs)
}

review_real_suggestions <- function(f) {
    # Actual broad-query classifier and deterministic collector, including GFF
    # index lookup. UI construction is the only substituted suggestion boundary.
    f$e$geneAutocompleteCache <- shiny::reactiveVal(list())
    for (name in c('count_fast_exact_gene_matches', 'maybe_show_partial_gene_suggestions')) {
        eval(ownership_assignment(name), f$e)
    }
    found <- list()
    f$e$show_partial_gene_suggestions_df <- function(mode, query, suggestions, ...) {
        found[[length(found) + 1L]] <<- list(query = query, suggestions = suggestions)
        TRUE
    }
    function() found
}

test_that('confirmed reference selection preserves real GFF resolution and the other organism query', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    fixture <- review_gff_fixture(f)
    anchor <- list(local_gene_id = 'LOC_SELECTED', organism_id = 'a', organism_name = 'A', query = 'BROAD')
    f$e$fileDataOrthologous(list(old = list()))
    f$e$lastOrthoSearchedGene('OLD')
    f$e$pendingOrthoReferenceAnchor(anchor)
    f$e$execute_orthologous_search('BROAD', 'orthology_reference')
    original <- f$ticket('orthologous')
    f$session$setInputs(cgv_ortho_search_dispatch = original)
    expect_length(fixture$jobs(), 0L)
    expect_identical(shiny::isolate(f$e$pendingOrthoGeneChange())$reference_anchor, anchor)
    f$session$setInputs(cgv_search_confirmation_accepted = list(mode = 'orthologous', request_id = original$request_id))
    replacement <- f$ticket('orthologous')
    expect_false(identical(original$request_id, replacement$request_id))
    expect_identical(f$e$searchRequestPayload$orthologous$reference_anchor, anchor)
    f$session$setInputs(cgv_search_confirmation_closed = list(mode = 'orthologous', request_id = original$request_id))
    expect_true(f$e$search_request_is_current('orthologous', replacement$request_id))
    f$session$setInputs(cgv_ortho_search_dispatch = replacement)
    expect_identical(vapply(fixture$jobs(), `[[`, character(1), 'gene_name'), c('LOC_SELECTED', 'BROAD'))
    reference <- fixture$results()[[1L]]
    other <- fixture$results()[[2L]]
    expect_true(reference$found)
    expect_identical(reference$lookup$matched_gene_id, 'gene-LOC_SELECTED')
    expect_equal(range(as.numeric(reference$data$V4), as.numeric(reference$data$V5)), c(31, 380))
    expect_identical(unique(as.character(reference$data$V7)), '+')
    expect_identical(other$lookup$matched_gene_id, 'gene-BROAD')
    expect_equal(range(as.numeric(other$data$V4), as.numeric(other$data$V5)), c(1001, 1500))
    expect_identical(unique(as.character(other$data$V7)), '-')
})

test_that('confirmed reference payload survives replacement preparation and stale confirmation messages', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    fixture <- review_gff_fixture(f)
    anchor <- list(local_gene_id = 'LOC_SELECTED', organism_id = 'a', organism_name = 'A',
        query = 'BROAD', evidence = list(identifier = 'selected-locus', coordinates = c(31L, 380L), strand = '+'))
    f$e$fileDataOrthologous(list(old = list())); f$e$lastOrthoSearchedGene('OLD')
    f$e$execute_orthologous_search('BROAD', 'orthology_reference', reference_anchor = anchor)
    a <- f$ticket('orthologous'); f$session$setInputs(cgv_ortho_search_dispatch = a)
    f$session$setInputs(cgv_search_confirmation_accepted = list(mode = 'orthologous', request_id = a$request_id))
    b <- f$ticket('orthologous')
    f$e$searchPreparationReadyOrtho(FALSE)
    f$session$setInputs(cgv_ortho_search_dispatch = b)
    f$session$setInputs(cgv_search_confirmation_accepted = list(mode = 'orthologous', request_id = a$request_id),
        cgv_search_confirmation_closed = list(mode = 'orthologous', request_id = a$request_id),
        cgv_ortho_search_dispatch = a)
    expect_true(f$e$search_request_is_current('orthologous', b$request_id))
    expect_false(f$e$cancel_search_operation('orthologous', a$request_id))
    expect_length(fixture$jobs(), 0L)
    f$e$searchPreparationReadyOrtho(TRUE); f$session$flushReact()
    retry <- f$ticket('orthologous')
    expect_identical(retry$request_id, b$request_id)
    expect_identical(f$e$searchRequestPayload$orthologous$reference_anchor, anchor)
    f$session$setInputs(cgv_ortho_search_dispatch = retry)
    expect_identical(fixture$results()[[1]]$lookup$matched_gene_id, 'gene-LOC_SELECTED')
    expect_identical(f$state('orthologous')$status, 'completed')
})

test_that('preflight batch cleanup is idempotent and cannot advance a cancelled or replacement batch', {
    for (scenario in c('cancel', 'replace')) {
        f <- ownership_harness(); on.exit(f$close(), add = TRUE)
        review_gff_fixture(f); review_real_suggestions(f)
        collect <- f$e$maybe_show_partial_gene_suggestions
        a <- NULL
        f$e$maybe_show_partial_gene_suggestions <- function(...) {
            shown <- collect(...)
            if (scenario == 'cancel') f$e$cancel_search_operation('orthologous', a$request_id)
            else shiny::isolate({
                f$e$set_batch_search_state('orthologous', 2L)
                f$e$queue_global_batch_search(c('TP53', 'BROAD'), 'orthologous')
            })
            shown
        }
        shiny::isolate({f$e$set_batch_search_state('orthologous', 2L)
            f$e$queue_global_batch_search(c('ABC', 'TP53'), 'orthologous')})
        later::run_now(0.1); a <- f$ticket('orthologous')
        f$session$setInputs(cgv_ortho_search_dispatch = a)
        expect_false(f$e$finalize_search_request('orthologous', a$request_id))
        later::run_now(0.12)
        if (scenario == 'cancel') {
            expect_identical(f$ticket('orthologous')$request_id, a$request_id)
            expect_identical(shiny::isolate(f$e$batchSearchRemaining()), 0L)
        } else {
            b <- f$ticket('orthologous')
            expect_true(f$e$search_request_is_current('orthologous', b$request_id))
            expect_identical(shiny::isolate(f$e$batchSearchRemaining()), 2L)
            expect_identical(shiny::isolate(f$e$batchSearchQueue()), 'BROAD')
            expect_false(f$e$cancel_search_operation('orthologous', a$request_id))
        }
        f$close()
    }
})

test_that('batch preparation retains the request without advancing before preflight runs', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    review_gff_fixture(f); suggestions <- review_real_suggestions(f)
    f$e$searchPreparationReadyOrtho(FALSE)
    shiny::isolate({f$e$set_batch_search_state('orthologous', 2L)
        f$e$queue_global_batch_search(c('ABC', 'TP53'), 'orthologous')})
    later::run_now(0.1); a <- f$ticket('orthologous')
    f$session$setInputs(cgv_ortho_search_dispatch = a)
    expect_length(suggestions(), 0L)
    expect_identical(shiny::isolate(f$e$batchSearchRemaining()), 2L)
    expect_true(f$e$search_request_is_current('orthologous', a$request_id))
    f$e$searchPreparationReadyOrtho(TRUE); f$session$flushReact()
    f$session$setInputs(cgv_ortho_search_dispatch = f$ticket('orthologous'))
    expect_identical(shiny::isolate(f$e$batchSearchRemaining()), 1L)
    expect_false(f$e$finalize_search_request('orthologous', a$request_id))
    expect_identical(shiny::isolate(f$e$batchSearchRemaining()), 1L)
})

test_that('cross-mode cleanup preserves inputs owned by pending or running searches', {
    for (owner in c('homologous', 'orthologous')) {
        for (other_status in c('pending', 'running', 'rescue')) {
            f <- ownership_harness(FALSE); on.exit(f$close(), add = TRUE)
            other <- setdiff(c('homologous', 'orthologous'), owner)
            a <- f$e$begin_search_run(owner, 'a')
            batch <- f$e$bind_search_batch_owner(owner, a$id, 'local')
            if (other_status == 'pending') f$e$request_search_run(other)
            else {
                b <- f$e$begin_search_run(other, 'b')
                if (other_status == 'rescue') {
                    f$e$start_search_child(other, b$id)
                    f$e$release_search_admission(other, b$id)
                }
            }
            shiny::isolate(f$e$finalize_search_cleanup(owner, a$id, batch))
            expect_identical(f$e$clears, 0L)
            expect_true(f$e$owns_search_run(owner, a$id))
            f$close()
        }
    }
})

test_that('orthologous batch advances after a real broad-query preflight rejection', {
    f <- ownership_harness(); on.exit(f$close(), add = TRUE)
    fixture <- review_gff_fixture(f)
    suggestions <- review_real_suggestions(f)
    expect_true(fixture$science$is_low_specific_gene_family_query('ABC'))
    shiny::isolate({
        f$e$set_batch_search_state('orthologous', 2L)
        f$e$queue_global_batch_search(c('ABC', 'TP53'), 'orthologous')
    })
    later::run_now(0.1)
    first <- f$ticket('orthologous')
    f$session$setInputs(cgv_ortho_search_dispatch = first)
    expect_identical(suggestions()[[1L]]$query, 'ABC')
    expect_true('ABC1' %in% suggestions()[[1L]]$suggestions$gene_name)
    expect_null(f$e$stateHelpersDomain$get_search_request_id('orthologous'))
    expect_length(fixture$jobs(), 0L)
    later::run_now(0.12)
    next_ticket <- f$ticket('orthologous')
    expect_false(identical(next_ticket$request_id, first$request_id))
    f$session$setInputs(cgv_ortho_search_dispatch = next_ticket)
    expect_identical(vapply(fixture$jobs(), `[[`, character(1), 'gene_name'), rep('TP53', 2L))
    expect_true(all(vapply(fixture$results(), `[[`, logical(1), 'found')))
    expect_identical(shiny::isolate(f$e$batchSearchRemaining()), 0L)
    expect_identical(shiny::isolate(f$e$batchSearchQueue()), character())
})


test_that('new request and batch cleanup guards do not access destroyed session reactives', {
    f <- ownership_harness(FALSE)
    request <- f$e$request_search_run('orthologous')
    cancellation_owner <- f$e$capture_orthologous_cancellation_owner()
    generation <- f$e$searchBatchOwner$generation
    f$close()
    destroyed <- function(...) stop('destroyed reactive accessed')
    for (name in c('searchRunState', 'batchSearchMode', 'batchSearchRemaining', 'batchSearchQueue')) f$e[[name]] <- destroyed
    f$e$input <- destroyed
    expect_false(f$e$finalize_search_request('orthologous', request$id))
    expect_false(f$e$clear_search_inputs_if_unowned('homologous'))
    expect_false(f$e$advance_search_batch('orthologous', generation))
    expect_null(f$e$capture_orthologous_cancellation_owner())
    expect_false(f$e$cancel_orthologous_background_rescue('closed', cancellation_owner))
    expect_identical(f$e$begin_search_run('orthologous')$reason, 'closed')
    expect_null(f$e$orthologous_search_source_key())
    expect_null(f$e$search_event_payload('orthologous', request$id, 'TP53'))
    expect_false(f$e$execute_orthologous_search('TP53', request_id = request$id))
})

test_that('tagged confirmation acceptance cannot authorize another mode or newer pending intent', {
    f <- ownership_harness(FALSE); on.exit(f$close(), add = TRUE)
    f$e$run_clear_selected_visualizations <- function(...) stop('stale acceptance cleared science')
    for (mode in c('homologous', 'orthologous')) {
        a <- f$e$request_search_run(mode)
        b <- f$e$request_search_run(mode)
        pending <- list(gene = 'GENEB', request_id = b$id)
        if (mode == 'homologous') f$e$pendingHomoOrganismChange(pending) else f$e$pendingOrthoGeneChange(pending)
        f$session$setInputs(cgv_search_confirmation_accepted = list(mode = mode, request_id = a$id))
        f$session$setInputs(cgv_search_confirmation_accepted = list(
            mode = setdiff(c('homologous', 'orthologous'), mode), request_id = b$id))
        expect_true(f$e$search_request_is_current(mode, b$id))
        expect_identical(f$e$lookups, 0L)
    }
})
