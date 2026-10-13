# Complete production server in a fresh process, with real GFF/FASTA data.
# Browser acknowledgements and the pre-settlement admission boundary are driven
# explicitly; scientific producers, preparation and plot modules are unchanged.
args <- commandArgs(TRUE)
root <- normalizePath(args[[1L]])
scenario <- args[[2L]]
main <- function() {
    data_root <- tempfile('cgv-cancellation-data-')
    dir.create(file.path(data_root, 'annotations'), recursive = TRUE)
    dir.create(file.path(data_root, 'genomes'))
    on.exit(unlink(data_root, recursive = TRUE), add = TRUE)
    gff <- c('##gff-version 3',
        'chr1\tfixture\tgene\t10\t99\t.\t+\t.\tID=gene-TP53;Name=TP53',
        'chr1\tfixture\tmRNA\t10\t99\t.\t+\t.\tID=tx1;Parent=gene-TP53',
        'chr1\tfixture\texon\t10\t29\t.\t+\t.\tID=ex1;Parent=tx1',
        'chr1\tfixture\texon\t70\t99\t.\t+\t.\tID=ex2;Parent=tx1',
        'chr1\tfixture\tgene\t120\t180\t.\t-\t.\tID=gene-GENE2;Name=GENE2',
        'chr1\tfixture\tmRNA\t120\t180\t.\t-\t.\tID=tx2;Parent=gene-GENE2',
        'chr1\tfixture\texon\t120\t180\t.\t-\t.\tID=ex3;Parent=tx2',
        'chr1\tfixture\tgene\t210\t240\t.\t+\t.\tID=gene-GENE3;Name=GENE3',
        'chr1\tfixture\tmRNA\t210\t240\t.\t+\t.\tID=tx3;Parent=gene-GENE3',
        'chr1\tfixture\texon\t210\t240\t.\t+\t.\tID=ex4;Parent=tx3')
    for (id in c('a', 'b', 'c')) {
        writeLines(c(gff[1L], paste('##organism', c(a = 'Review alpha', b = 'Review beta', c = 'Review gamma')[[id]]), gff[-1L]),
            file.path(data_root, 'annotations', paste0(id, '.gff3')))
        writeLines(c('>chr1', strrep('ACGT', 100)), file.path(data_root, 'genomes', paste0(id, '.fa')))
    }
    registry <- data.frame(species_id = c('a', 'b', 'c'),
        label = c('Review alpha', 'Review beta', 'Review gamma'),
        organism = c('Review alpha', 'Review beta', 'Review gamma'), taxid = 0,
        annotation = paste0('annotations/', c('a', 'b', 'c'), '.gff3'),
        annotation_tabix = '', annotation_index = '',
        genome = paste0('genomes/', c('a', 'b', 'c'), '.fa'), genome_2bit = '',
        aliases = 'fixture', icon = '/icons/DNA.ico', kingdom = 'Other')
    if (scenario == 'source-ncbi-same-labels') {
        registry$label[3L] <- registry$organism[3L] <- 'Review beta'
    }
    write.table(registry, file.path(data_root, 'annotations/registry.tsv'),
        sep = '\t', quote = FALSE, row.names = FALSE)
    Sys.setenv(APP_FUTURE_MODE = 'sequential', APP_COMPILED_RUNTIME = '0',
        APP_GENE_CATALOG_ENABLED = '0', CGV_DATA_ROOT = data_root,
        CGV_CACHE_DIR = file.path(data_root, 'cache'), APP_ORTHO_SELECTION_DEBOUNCE_MS = '500')
    options(sass.cache = FALSE)
    setwd(root)
    source('global.R', local = .GlobalEnv)
    source('server.R', local = .GlobalEnv)
    if (scenario %in% c('source-downloaded', 'source-partial')) {
        # Fixed provider response; actual download preparation, mapping, lookup
        # and plot publication run normally. Never contact NCBI in this fixture.
        .GlobalEnv$ncbi_full_download_pipeline <- function(accession, ...) {
            if (accession == 'failed') stop('controlled download failure')
            id <- sub('download_', '', accession, fixed = TRUE)
            list(annotation_tabix = file.path(data_root, 'annotations', paste0(id, '.gff3')),
                genome = file.path(data_root, 'genomes', paste0(id, '.fa')), go_annotation = '')
        }
    }
    shiny::testServer(.cgv_server_definition, {
        messages <- list()
        send <- session$sendCustomMessage
        session$sendCustomMessage <- function(type, message) {
            messages[[length(messages) + 1L]] <<- list(type = type, payload = message)
            send(type, message)
        }
        session$setInputs(navtabs = 'orthologous', homo_data_mode = 'upload', ortho_data_mode = 'preloaded',
            ortho_preloaded_species = c('a', 'b'), search_gene = 0, generate1 = 0,
            confirm_delete_plot_btn = 0)
        action_count <- 0
        search <- function(gene = 'TP53') {
            action_count <<- action_count + 1
            session$setInputs(gene_name = gene, search_gene = action_count)
        }
        wait_ready <- function() {
            deadline <- Sys.time() + 10
            while (!isTRUE(searchPreparationReadyOrtho()) && Sys.time() < deadline) {
                later::run_now(.05); session$flushReact()
            }
            stopifnot(isTRUE(searchPreparationReadyOrtho()))
        }
        acknowledge <- function() {
            # Let the existing successful-query duplicate window expire. Mock
            # session time advances timers, not the actual Sys.time() clock.
            remaining <- 1.21 - (as.numeric(Sys.time()) - get_search_run_mode_state('orthologous')$mode_state$last_completed)
            if (is.finite(remaining) && remaining > 0) Sys.sleep(remaining)
            hits <- Filter(function(x) identical(x$type, 'cgv_trigger_ortho_search'), messages)
            stopifnot(length(hits) > 0L)
            ticket <- tail(hits, 1)[[1L]]$payload
            if (!is.null(ticket$request_id)) session$setInputs(cgv_ortho_search_dispatch = ticket)
            else search()
        }
        cards <- function(labels = c('Review alpha', 'Review beta'), gene = 'TP53',
                          coords = c(10, 99), strand = '+') {
            ids <- as.character(activePlotIdsOrthologous())
            stopifnot(length(ids) == length(labels))
            values <- fileDataOrthologous()[ids]
            stopifnot(all(vapply(values, function(df) {
                identical(range(as.numeric(df$V4), as.numeric(df$V5)), coords) &&
                    identical(unique(as.character(df$V7)), strand) &&
                    any(grepl(paste0('ID=gene-', gene, ';'), df$V9, fixed = TRUE))
            }, logical(1))))
            info <- organismInfoOrthologous()[ids]
            names <- vapply(info, function(x) as.character(x$name), character(1))
            stopifnot(setequal(names, labels))
            for (path in unique(unlist(genomePathsOrthologous()[ids]))) {
                stopifnot(identical(extract_sequence_from_fasta(path, 'chr1', 10, 99),
                    substr(strrep('ACGT', 100), 10, 99)))
            }
            invisible(values)
        }
        delete_request <- function(id) {
            session$setInputs(plot_close_request = list(panel = 'orthologous', id = as.character(id)))
            if (exists('pendingPlotDeleteOwner')) pendingPlotDeleteOwner() else NULL
        }
        delete_confirm <- function(owner, count = 1) {
            # The obsolete untagged event is included so the same oracle detects
            # F2 on 8be1934. Corrected code acts only on the tagged transport.
            session$setInputs(confirm_delete_plot_btn = count,
                cgv_plot_delete_confirmed = list(intent_id = owner$intent_id %||% 'original-delete'))
        }
        session$elapse(600); wait_ready()
        search(); cards()

        prepare_count <- 0L
        prepare_ncbi <- function(ids, mode = 'ncbi') {
            if (!identical(input$ortho_data_mode, mode)) {
                session$setInputs(ortho_data_mode = mode)
                session$elapse(600); wait_ready()
            }
            while (length(ncbiQueueOrtho())) session$setInputs(ortho_ncbi_remove_queue = 1)
            for (id in ids) {
                if (scenario %in% c('source-downloaded', 'source-partial') && id != 'a') {
                    accession <- paste0('download_', id)
                    ncbiSearchResultsOrtho(data.frame(accession = accession,
                        organism = registry$organism[match(id, registry$species_id)], taxid = 0L,
                        genome_size_mb = 0, has_annotation = TRUE))
                    session$setInputs(ortho_ncbi_add_organism = accession)
                } else session$setInputs(ortho_ncbi_add_preloaded = id)
            }
            if (scenario == 'source-partial') {
                ncbiSearchResultsOrtho(data.frame(accession = 'failed', organism = 'Failed organism',
                    taxid = 0L, genome_size_mb = 0, has_annotation = TRUE))
                session$setInputs(ortho_ncbi_add_organism = 'failed')
            }
            prepare_count <<- prepare_count + 1L
            session$setInputs(ortho_ncbi_download_all_btn = prepare_count)
            wait_ready()
        }
        upload_sources <- function(ids) {
            make_input <- function(kind) {
                paths <- file.path(data_root, if (kind == 'ann') 'annotations' else 'genomes',
                    paste0(ids, if (kind == 'ann') '.gff3' else '.fa'))
                data.frame(name = basename(paths), size = file.info(paths)$size, type = '', datapath = paths)
            }
            session$setInputs(ortho_data_mode = 'upload', files = make_input('ann'),
                genome_fasta_ortho = make_input('gen'))
            session$elapse(600); wait_ready()
        }
        owned_source <- function(label) {
            a <- begin_search_run('orthologous', label, duplicate_window_sec = 0)
            stopifnot(isTRUE(a$ok))
            outcome <- new.env(parent = emptyenv()); outcome$ran <- FALSE; outcome$cleaned <- 0L
            science <- fileDataOrthologous()
            late <- search_operation_guard('orthologous', a$id, function() {
                outcome$ran <- TRUE; fileDataOrthologous(science)
            }, cleanup = function() outcome$cleaned <- outcome$cleaned + 1L)
            settle <- start_search_child('orthologous', a$id)
            owner <- capture_orthologous_cancellation_owner()
            release_search_admission('orthologous', a$id)
            list(id = a$id, late = late, settle = settle, outcome = outcome, owner = owner,
                key = get_search_run_mode_state('orthologous')$mode_state$source_key)
        }
        stale_source <- function(a) {
            before <- fileDataOrthologous(); state <- get_search_run_mode_state('orthologous')$mode_state
            a$late(); a$settle(); a$settle()
            stopifnot(!a$outcome$ran, a$outcome$cleaned == 1L,
                !owns_search_run('orthologous', a$id), identical(fileDataOrthologous(), before),
                identical(get_search_run_mode_state('orthologous')$mode_state, state))
        }
        if (startsWith(scenario, 'source-')) {
            if (scenario %in% c('source-upload', 'source-upload-genome', 'source-upload-delete')) {
                upload_sources(c('a', 'b')); search(); cards()
                a <- owned_source('uploaded-A')
                if (scenario == 'source-upload') {
                    upload_sources(c('a', 'c')); search(); cards(c('Review alpha', 'Review gamma'))
                } else if (scenario == 'source-upload-genome') {
                    paths <- file.path(data_root, 'genomes', paste0(c('a', 'c'), '.fa'))
                    session$setInputs(genome_fasta_ortho = data.frame(name = basename(paths),
                        size = file.info(paths)$size, type = '', datapath = paths))
                    stopifnot(identical(get_search_run_mode_state('orthologous')$mode_state$status, 'cancelled'))
                } else {
                    session$setInputs(files = NULL)
                    stopifnot(identical(get_search_run_mode_state('orthologous')$mode_state$status, 'cancelled'))
                }
                stale_source(a)
            } else {
                prepare_ncbi(c('a', 'b')); search(); cards()
                a <- owned_source('prepared-A')
                if (scenario == 'source-draft') {
                    session$setInputs(ortho_ncbi_remove_queue = 2)
                    session$setInputs(ortho_ncbi_add_preloaded = 'c')
                    published <- FALSE
                    live <- search_operation_guard('orthologous', a$id, function() {published <<- TRUE})
                    live(); stopifnot(published, owns_search_run('orthologous', a$id))
                    cards() # Editing the draft queue does not replace prepared scientific sources.
                }
                if (scenario == 'source-teardown') {
                    session$setInputs(ortho_ncbi_remove_queue = 2, ortho_ncbi_add_preloaded = 'c')
                    key <- orthologous_search_source_key(ncbi_ids = c('a', 'c'))
                    session$close()
                    stopifnot(is.null(orthologous_search_source_key()),
                        identical(handle_ortho_source_change(key), FALSE))
                    a$late(); a$settle(); a$settle()
                    stopifnot(!a$outcome$ran, a$outcome$cleaned == 1L)
                } else if (scenario == 'source-mixed') {
                    session$setInputs(ortho_preloaded_species = 'a')
                    prepare_ncbi('b', mode = 'mixed'); search(); cards()
                    stale_source(a)
                    m <- owned_source('mixed-A')
                    prepare_ncbi('c', mode = 'mixed'); search(); cards(c('Review alpha', 'Review gamma'))
                    stale_source(m)
                } else {
                    prepare_ncbi(c('a', 'c'))
                    stopifnot(identical(get_search_run_mode_state('orthologous')$mode_state$status, 'cancelled'))
                    if (scenario == 'source-ncbi-same-labels') {
                        search(); cards()
                        stopifnot(any(grepl('/c.gff3$', unlist(annotationPathsOrthologous()))))
                    } else if (scenario == 'source-rapid') {
                        execute_orthologous_search('TP53'); request <- stateHelpersDomain$get_search_request_id('orthologous')
                        prepare_ncbi(c('b', 'c'))
                        stopifnot(!search_request_is_current('orthologous', request))
                        search(); cards(c('Review beta', 'Review gamma'))
                    } else {
                        search(); cards(c('Review alpha', 'Review gamma'))
                    }
                    if (scenario == 'source-new-owner') {
                        b <- begin_search_run('orthologous', 'new-owner-B', duplicate_window_sec = 0)
                        before <- get_search_run_mode_state('orthologous')$mode_state
                        cancel_orthologous_background_rescue('late_old_source',
                            capture_orthologous_cancellation_owner(source_key = a$key))
                        cancel_orthologous_background_rescue('late_captured_owner', a$owner)
                        stopifnot(owns_search_run('orthologous', b$id), identical(before,
                            get_search_run_mode_state('orthologous')$mode_state))
                    }
                    stale_source(a)
                    if (scenario == 'source-repeated') {
                        for (ids in list(c('b', 'c'), c('a', 'b'), c('a', 'c'))) {
                            previous <- owned_source(paste(ids, collapse = '-'))
                            prepare_ncbi(ids)
                            search(); cards(registry$organism[match(ids, registry$species_id)])
                            stale_source(previous)
                        }
                    } else if (scenario == 'source-return') {
                        prepare_ncbi(c('a', 'b'))
                        cards() # Restored valid source-history science, without a new search.
                        stopifnot(!owns_search_run('orthologous', a$id))
                        a$late(); stopifnot(!a$outcome$ran); cards()
                        apply_app_state_from_snapshot(list(ortho_data_mode = 'ncbi',
                            homo_data_mode = 'upload', ortho_preloaded_species = character(0)))
                        session$setInputs(cgv_session_source_restored = list(input_id = 'ortho_preloaded_species'))
                        stopifnot(identical(consume_session_source_restore('orthologous', orthologous_search_source_key()), 'none'))
                    }
                }
            }
            cat('SOURCE OWNERSHIP AND REAL SCIENTIFIC PUBLICATION VERIFIED ', scenario, '\n', sep = '')
        } else if (scenario == 'ncbi-source-replacement') {
            session$setInputs(ortho_data_mode = 'ncbi')
            session$elapse(600); wait_ready()
            session$setInputs(ortho_ncbi_add_preloaded = 'a')
            session$setInputs(ortho_ncbi_add_preloaded = 'b')
            session$setInputs(ortho_ncbi_download_all_btn = 1)
            wait_ready()
            search(); cards()
            a <- begin_search_run('orthologous', 'ncbi-A', duplicate_window_sec = 0)
            settle <- start_search_child('orthologous', a$id)
            release_search_admission('orthologous', a$id)
            stored <- lastObservedOrthoSourceKey()
            operation <- get_search_run_mode_state('orthologous')$mode_state$source_key
            old_ran <- FALSE
            late <- search_operation_guard('orthologous', a$id, function() {old_ran <<- TRUE})
            session$setInputs(ortho_ncbi_remove_queue = 2)
            session$setInputs(ortho_ncbi_add_preloaded = 'c')
            session$setInputs(ortho_ncbi_download_all_btn = 2)
            late()
            cat('stored_source =', stored, 'operation_source =', operation,
                'operation_status =', get_search_run_mode_state('orthologous')$mode_state$status,
                'old_continuation_executed =', old_ran, '\n')
            stopifnot(identical(get_search_run_mode_state('orthologous')$mode_state$status, 'cancelled'), !old_ran)
            settle(); search(); cards(c('Review alpha', 'Review gamma'))
        } else if (scenario == 'rapid-selection') {
            session$setInputs(ortho_preloaded_species = c('a', 'c'))
            search()
            queued <- pendingPreparedSearchOrtho()
            stopifnot(is.list(queued), identical(queued$gene, 'TP53'))
            session$elapse(600); wait_ready()
            # Fails on the reviewed candidate before any browser acknowledgement.
            stopifnot(identical(pendingOrthoSearchGene(), 'TP53'))
            if (exists('search_request_is_current')) stopifnot(search_request_is_current('orthologous', queued$request_id))
            acknowledge()
            cards(c('Review alpha', 'Review gamma'))
            cat('QUEUED REQUEST REDISPATCHED; real replacement TP53 cards 10:99 +\n')
        } else if (scenario == 'stale-delete') {
            owner <- delete_request(activePlotIdsOrthologous()[[1L]])
            session$setInputs(ortho_preloaded_species = c('a', 'c'))
            session$elapse(600); wait_ready()
            stopifnot(length(activePlotIdsOrthologous()) == 0L)
            b <- begin_search_run('orthologous', 'replacement-B', duplicate_window_sec = 0)
            before <- get_search_run_mode_state('orthologous')$mode_state
            stopifnot(isTRUE(b$ok), isTRUE(before$active))
            delete_confirm(owner)
            stopifnot(identical(get_search_run_mode_state('orthologous')$mode_state, before))
            if (!is.null(b$id)) {
                publish <- search_operation_guard('orthologous', b$id, function() searchStatusOrthologous('B published'))
                publish(); stopifnot(identical(searchStatusOrthologous(), 'B published'))
                cancel_search_operation('orthologous', b$id)
            } else finish_search_run('orthologous')
            search(); cards(c('Review alpha', 'Review gamma'))
            d <- begin_search_run('orthologous', 'replacement-D', duplicate_window_sec = 0)
            before <- get_search_run_mode_state('orthologous')$mode_state
            science_before <- fileDataOrthologous()
            delete_confirm(owner, 2)
            stopifnot(identical(get_search_run_mode_state('orthologous')$mode_state, before),
                identical(fileDataOrthologous(), science_before))
            if (!is.null(d$id)) cancel_search_operation('orthologous', d$id) else finish_search_run('orthologous')
            cat('STALE DELETE INERT; replacement B publishes and real replacement cards resolve\n')
        } else if (scenario == 'valid-delete') {
            ids <- activePlotIdsOrthologous()
            before <- fileDataOrthologous()
            owner <- delete_request(ids[[1L]])
            replacement_owner <- delete_request(ids[[2L]])
            delete_confirm(owner)
            stopifnot(identical(fileDataOrthologous(), before),
                identical(pendingPlotDeleteOwner(), replacement_owner))
            other <- before[as.character(ids[[1L]])]
            delete_confirm(replacement_owner, 2)
            stopifnot(!as.character(ids[[2L]]) %in% as.character(activePlotIdsOrthologous()),
                identical(fileDataOrthologous()[names(other)], other), length(activePlotIdsOrthologous()) == 1L)
            delete_confirm(replacement_owner, 3)
            stopifnot(identical(fileDataOrthologous()[names(other)], other))
            cat('VALID DELETE removes only its original card; duplicate confirmation is inert\n')
        } else if (scenario == 'old-source-owner') {
            a <- begin_search_run('orthologous', 'old-source', duplicate_window_sec = 0)
            stopifnot(isTRUE(a$ok))
            settle <- start_search_child('orthologous', a$id)
            release_search_admission('orthologous', a$id)
            owner <- capture_orthologous_cancellation_owner()
            late <- search_operation_guard('orthologous', a$id, function() stop('obsolete scientific publication'))
            session$setInputs(ortho_preloaded_species = c('a', 'c'))
            search(); queued <- pendingPreparedSearchOrtho()
            session$elapse(600); wait_ready()
            stopifnot(!owns_search_run('orthologous', a$id), search_request_is_current('orthologous', queued$request_id))
            late(); settle(); settle()
            acknowledge(); before <- fileDataOrthologous()
            cancel_orthologous_background_rescue('late_source_cleanup', owner)
            stopifnot(identical(fileDataOrthologous(), before))
            cards(c('Review alpha', 'Review gamma'))
            cat('OLD SOURCE retires only A; repeated late cleanup and settlement preserve B science\n')
        } else if (scenario == 'owned-delete-cancellation') {
            a <- begin_search_run('orthologous', 'rescue-A', duplicate_window_sec = 0)
            settle <- start_search_child('orthologous', a$id)
            release_search_admission('orthologous', a$id)
            late <- search_operation_guard('orthologous', a$id, function() stop('deleted scientific result resurrected'))
            ids <- activePlotIdsOrthologous(); other <- fileDataOrthologous()[as.character(ids[-1L])]
            owner <- delete_request(ids[[1L]])
            delete_confirm(owner)
            stopifnot(!owns_search_run('orthologous', a$id),
                identical(get_search_run_mode_state('orthologous')$mode_state$status, 'cancelled'),
                identical(fileDataOrthologous()[names(other)], other))
            late(); settle(); settle()
            cat('VALID DELETE cancels its admission-released owner and suppresses late resurrection\n')
        } else if (scenario == 'repeat-and-replace') {
            execute_orthologous_search('GENE2')
            stale <- tail(Filter(function(x) identical(x$type, 'cgv_trigger_ortho_search'), messages), 1)[[1L]]$payload
            before <- fileDataOrthologous()
            session$setInputs(ortho_preloaded_species = c('a', 'c'))
            session$setInputs(cgv_ortho_search_dispatch = stale)
            stopifnot(!search_request_is_current('orthologous', stale$request_id),
                is.null(pendingPreparedSearchOrtho()), identical(fileDataOrthologous(), before))
            search(); first <- pendingPreparedSearchOrtho()
            owner <- capture_orthologous_cancellation_owner()
            cancel_orthologous_background_rescue('explicit_preparation_cancel', owner)
            stopifnot(!search_request_is_current('orthologous', first$request_id), is.null(pendingPreparedSearchOrtho()))
            search(); second <- pendingPreparedSearchOrtho()
            stopifnot(!identical(first$request_id, second$request_id))
            session$setInputs(ortho_preloaded_species = c('b', 'c'))
            search(); third <- pendingPreparedSearchOrtho()
            stopifnot(!search_request_is_current('orthologous', second$request_id))
            cancel_orthologous_background_rescue('stale_cancel', owner)
            session$elapse(600); wait_ready()
            stopifnot(search_request_is_current('orthologous', third$request_id))
            acknowledge(); cards(c('Review beta', 'Review gamma'))
            before <- fileDataOrthologous()
            session$setInputs(cgv_ortho_search_dispatch = list(request_id = first$request_id, dispatch_id = 1),
                cgv_search_confirmation_closed = list(mode = 'orthologous', request_id = second$request_id))
            stopifnot(identical(fileDataOrthologous(), before))
            cat('REPEATED selections, cancelled preparation and stale browser events preserve final science\n')
        } else if (scenario == 'cross-mode') {
            session$setInputs(homo_data_mode = 'preloaded', homo_preloaded_species = 'a')
            deadline <- Sys.time() + 10
            while (!isTRUE(searchPreparationReadyHomo()) && Sys.time() < deadline) {
                later::run_now(.05); session$flushReact()
            }
            stopifnot(isTRUE(searchPreparationReadyHomo()))
            session$setInputs(filter1 = 'GENE2', generate1 = 1)
            homo <- fileDataHomologous(); stopifnot(length(homo) > 0L)
            h <- begin_search_run('homologous', 'homo-owned', duplicate_window_sec = 0)
            h_before <- get_search_run_mode_state('homologous')$mode_state
            owner <- delete_request(activePlotIdsOrthologous()[[1L]])
            session$setInputs(ortho_preloaded_species = c('a', 'c')); search()
            session$elapse(600); wait_ready(); acknowledge()
            delete_confirm(owner)
            stopifnot(owns_search_run('homologous', h$id), identical(fileDataHomologous(), homo),
                identical(get_search_run_mode_state('homologous')$mode_state, h_before))
            cards(c('Review alpha', 'Review gamma'))
            cat('CROSS MODE retains homologous committed science and running ownership\n')
        } else if (scenario == 'confirmation-generation') {
            before <- fileDataOrthologous()
            search('GENE2'); a <- pendingOrthoGeneChange()
            search('GENE3'); b <- pendingOrthoGeneChange()
            stopifnot(is.list(a), is.list(b), !identical(a$request_id, b$request_id))
            session$setInputs(confirm_ortho_gene_change_btn = 1,
                cgv_search_confirmation_accepted = list(mode = 'orthologous', request_id = a$request_id))
            stopifnot(identical(fileDataOrthologous(), before), search_request_is_current('orthologous', b$request_id))
            session$setInputs(cgv_search_confirmation_accepted = list(mode = 'orthologous', request_id = b$request_id))
            acknowledge(); cards(gene = 'GENE3', coords = c(210, 240))
            before <- fileDataOrthologous()
            search('GENE2'); c <- pendingOrthoGeneChange()
            session$setInputs(ortho_preloaded_species = c('a', 'c'))
            session$setInputs(ortho_preloaded_species = c('a', 'b'))
            session$setInputs(cgv_search_confirmation_accepted = list(mode = 'orthologous', request_id = c$request_id))
            stopifnot(identical(fileDataOrthologous(), before))
            session$setInputs(ortho_preloaded_species = c('a', 'c'))
            search('GENE2'); session$elapse(600); wait_ready(); acknowledge()
            cards(c('Review alpha', 'Review gamma'), gene = 'GENE2', coords = c(120, 180), strand = '-')
            cat('CONFIRMATION identity and source replacement preserve accepted gene/coordinates/strand\n')
        } else stop('unknown scenario')
    })
}
main()
cat('PASS ', scenario, '\n', sep = '')
