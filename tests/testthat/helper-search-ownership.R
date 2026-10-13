# Load actual orchestration definitions without launching the application or
# reading production datasets. Scientific producers/renderers are test doubles.
ownership_root <- Sys.getenv('CGV_SEARCH_TEST_ROOT', if (file.exists('server.R')) '.' else '../..')
ownership_tree <- parse(file.path(ownership_root, 'server.R'))
ownership_nodes <- new.env(parent = emptyenv())
ownership_assignment <- function(name) {
    if (exists(name, ownership_nodes, inherits = FALSE)) return(ownership_nodes[[name]])
    found <- NULL
    walk <- function(x) {
        if (missing(x) || !is.call(x) || !is.null(found)) return()
        if (identical(x[[1L]], quote(`<-`)) && identical(x[[2L]], as.name(name))) {
            found <<- x; return()
        }
        for (child in as.list(x)[-1L]) walk(child)
    }
    walk(ownership_tree[[1L]])
    stopifnot(!is.null(found)); ownership_nodes[[name]] <- found; found
}
ownership_observers <- function(symbol) {
    found <- list()
    walk <- function(x) {
        if (missing(x) || !is.call(x)) return()
        if (identical(x[[1L]], quote(observeEvent)) &&
            grepl(symbol, paste(deparse(x[[2L]]), collapse = ''), fixed = TRUE)) {
            found[[length(found) + 1L]] <<- x
            return()
        }
        for (child in as.list(x)[-1L]) walk(child)
    }
    walk(ownership_tree[[1L]]); found
}
ownership_harness <- function(observers = TRUE) {
    session <- shiny::MockShinySession$new()
    e <- new.env(parent = globalenv())
    sys.source(file.path(ownership_root, 'R/utils.R'), e)
    sys.source(file.path(ownership_root, 'R/server_state_helpers_domain.R'), e)
    sys.source(file.path(ownership_root, 'R/server_popup_status_domain.R'), e)
    e$session <- session; e$input <- session$input; e$output <- session$output
    e$reactiveVal <- shiny::reactiveVal; e$isolate <- shiny::isolate
    e$observeEvent <- shiny::observeEvent; e$req <- shiny::req
    e$updateTextInput <- shiny::updateTextInput
    e$`%...>%` <- promises::`%...>%`; e$`%...!%` <- promises::`%...!%`
    e$i <- 1L
    e$session_deferred <- e$make_session_deferred(session)
    e$session_later <- e$session_deferred$later
    e$session_guard <- e$session_deferred$guard
    e$session_alive <- e$session_guard(function() TRUE)
    e$popupStatusDomain <- e$init_popup_status_domain(session)
    for (name in c('batchSearchMode', 'batchSearchRemaining', 'batchSearchQueue',
        'searchBatchOwner', 'searchOperationBatch', 'homoBatchSummary',
        'autocompleteBuildEpochs', 'searchRunState', 'searchDispatchContext', 'searchRequestPayload',
        'pendingHomoSearchGene', 'pendingOrthoSearchGene', 'pendingHomoSearchOrigin', 'pendingOrthoSearchOrigin',
        'searchPreparationReadyHomo', 'searchPreparationReadyOrtho',
        'searchPreparationTokenHomo', 'searchPreparationTokenOrtho',
        'pendingPreparedSearchHomo', 'pendingPreparedSearchOrtho',
        'lastConfirmedHomoSpeciesId', 'lastConfirmedHomoDataMode', 'lastOrthoSearchedGene',
        'orthoPreparedNcbiSourceIds',
        'pendingHomoOrganismChange', 'pendingOrthoGeneChange', 'orthoExternalRescueState')) {
        eval(ownership_assignment(name), e)
    }
    e$searchBatchOwner$generation <- 0
    e$orthoExternalRescueState$id <- ''
    e$stateHelpersDomain <- e$init_server_state_helpers_domain(e$autocompleteBuildEpochs, e$searchRunState,
        function() isTRUE(e$session_alive()))
    for (name in c('get_search_run_mode_state', 'owns_search_run',
        'release_search_admission', 'start_search_child', 'cancel_autocomplete_background', 'parse_positive_int_env')) {
        e[[name]] <- e$stateHelpersDomain[[name]]
    }
    e$search_request_is_current <- e$stateHelpersDomain$search_request_is_current
    for (name in c('orthologous_search_source_key', 'begin_search_run',
        'capture_orthologous_cancellation_owner', 'request_search_run', 'take_search_event', 'search_operation_guard', 'search_future_promise', 'search_event_payload',
        'bind_search_batch_owner', 'schedule_search_dispatch', 'cancel_search_operation',
        'preparation_state_for_mode', 'invalidate_search_preparation', 'check_search_preparation_gate',
        'set_batch_search_state', 'take_next_batch_gene', 'dispatch_next_batch_search',
        'queue_global_batch_search', 'finalize_search_cleanup', 'finalize_search_request',
        'advance_search_batch', 'clear_search_inputs_if_unowned', 'execute_homologous_search',
        'execute_orthologous_search', 'reset_homo_batch_summary', 'start_homo_batch_summary',
        'record_homo_batch_result', 'emit_homo_batch_summary_if_ready', 'run_global_search',
        'cancel_orthologous_background_rescue', 'summarize_label_list', 'append_status')) eval(ownership_assignment(name), e)
    e$messages <- list(); e$clears <- 0L; e$lookups <- 0L; e$rescue_submissions <- 0L
    session$sendCustomMessage <- function(type, message) e$messages[[length(e$messages) + 1L]] <- list(type = type, payload = message)
    e$clear_all_gene_search_inputs <- function() e$clears <- e$clears + 1L
    for (name in c('searchStatusHomologous', 'searchStatusOrthologous')) e[[name]] <- shiny::reactiveVal('')
    for (name in c('emit_popup_status', 'set_popup_loading', 'close_popup_status', 'showModal', 'removeModal',
        'app_perf_mark', 'app_perf_mark_ms', 'start_plot_timing_tracker', 'reset_ortho_first_paint_gate',
        'set_ortho_organism_lookup_status', 'reset_ortho_organism_lookup_status',
        'cancel_ortho_auto_render_queue', 'set_plot_timing_expected', 'set_panel_plots_for_mode')) {
        e[[name]] <- function(...) NULL
    }
    e$fast_organism_sync_enabled <- function() TRUE
    e$app_perf_new_run <- function(...) list(id = 'fixture', t0 = Sys.time())
    e$ms_since_run_start <- function(...) 0
    e$homoInitialVisibleCount <- e$orthoInitialVisibleCount <- 1L
    e$secondary_work_retry_delay_sec <- 0.01
    for (name in c('homoVisibleCount', 'orthoVisibleCount')) e[[name]] <- shiny::reactiveVal(1L)
    for (name in c('fileDataHomologous', 'fileDataOrthologous', 'plotSignaturesOrthologous',
        'plotGeneMetaOrthologous')) e[[name]] <- shiny::reactiveVal(list())
    e$orthoPlotTimingTracker <- e$homoPlotTimingTracker <- shiny::reactiveVal(list())
    e$plotCounterOrthologous <- shiny::reactiveVal(0L)
    for (name in c('max_gene_length_ortho', 'min_gene_coord_ortho', 'max_gene_coord_ortho')) e[[name]] <- shiny::reactiveVal(0)
    e$primaryPlotIdsOrthologous <- shiny::reactiveVal(character())
    e$pendingOrthoReferenceAnchor <- shiny::reactiveVal(NULL)
    annotations <- c(tempfile(fileext = '.gff'), tempfile(fileext = '.gff'))
    writeLines('fixture', annotations[1]); writeLines('fixture', annotations[2])
    e$registry <- data.frame(species_id = c('a', 'b'), label = c('A', 'B'), organism = c('A', 'B'),
        ready = TRUE, annotation_path = annotations, genome_path = '', stringsAsFactors = FALSE)
    e$preloadedRegistry <- function() e$registry
    e$get_preloaded_entry <- function(id) e$registry[e$registry$species_id == id, , drop = FALSE]
    e$build_det_from_preloaded <- function(entry) list(organism = entry$organism[[1L]])
    e$format_org_name <- function(name, ...) name
    e$is_low_specific_gene_family_query <- function(...) FALSE
    e$maybe_show_partial_gene_suggestions <- function(...) FALSE
    e$maybe_show_alias_index_ambiguity <- function(...) NULL
    e$initial_plot_timing_ids <- function(...) character()
    e$cross_species_requires_verified_orthology <- function() FALSE
    e$annotation_is_warmed <- function(...) TRUE
    e$get_enabled_external_alias_sources <- function() character()
    e$run_lookup_pipeline <- function(...) {e$lookups <- e$lookups + 1L; list(data = NULL)}
    e$run_orthologous_lookup_job_pure <- function(job) {
        e$lookups <- e$lookups + 1L
        list(file_idx = job$file_idx, file_label = job$file_label, found = FALSE, lookup = list())
    }
    e$select_cross_species_results_by_orthology_policy <- function(...) list(approved_positions = integer())
    e$ortho_timing_search_start <- Sys.time()
    installed <- list()
    shiny::withReactiveDomain(session, {
        if (observers) {
            installed[[1L]] <- eval(ownership_assignment('homo_search_obs'), e)
            installed[[2L]] <- eval(ownership_assignment('ortho_search_obs'), e)
        }
        for (symbol in c('searchPreparationReadyHomo()', 'searchPreparationReadyOrtho()',
            'cgv_search_confirmation_accepted',
            'cgv_search_confirmation_closed', 'partial_gene_external_alias_search',
            'catalog_multigene_ready', 'catalog_cross_ready')) {
            for (node in ownership_observers(symbol)) installed[[length(installed) + 1L]] <- eval(node, e)
        }
    })
    session$setInputs(generate1 = 0, search_gene = 0, homo_data_mode = 'preloaded',
        homo_preloaded_species = 'a', ortho_data_mode = 'preloaded', ortho_preloaded_species = c('a', 'b'))
    list(e = e, session = session, close = function() {
        for (observer in installed) observer$destroy()
        session$close(); unlink(annotations)
    }, ticket = function(mode) {
        type <- if (mode == 'homologous') 'cgv_trigger_homo_search' else 'cgv_trigger_ortho_search'
        hits <- Filter(function(x) identical(x$type, type), e$messages)
        tail(hits, 1)[[1L]]$payload
    }, state = function(mode) shiny::isolate(e$searchRunState())[[mode]])
}
