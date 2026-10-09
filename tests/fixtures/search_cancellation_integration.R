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
        writeLines(gff, file.path(data_root, 'annotations', paste0(id, '.gff3')))
        writeLines(c('>chr1', strrep('ACGT', 100)), file.path(data_root, 'genomes', paste0(id, '.fa')))
    }
    registry <- data.frame(species_id = c('a', 'b', 'c'),
        label = c('Review alpha', 'Review beta', 'Review gamma'),
        organism = c('Review alpha', 'Review beta', 'Review gamma'), taxid = 0,
        annotation = paste0('annotations/', c('a', 'b', 'c'), '.gff3'),
        annotation_tabix = '', annotation_index = '',
        genome = paste0('genomes/', c('a', 'b', 'c'), '.fa'), genome_2bit = '',
        aliases = 'fixture', icon = '/icons/DNA.ico', kingdom = 'Other')
    write.table(registry, file.path(data_root, 'annotations/registry.tsv'),
        sep = '\t', quote = FALSE, row.names = FALSE)
    Sys.setenv(APP_FUTURE_MODE = 'sequential', APP_COMPILED_RUNTIME = '0',
        APP_GENE_CATALOG_ENABLED = '0', CGV_DATA_ROOT = data_root,
        CGV_CACHE_DIR = file.path(data_root, 'cache'), APP_ORTHO_SELECTION_DEBOUNCE_MS = '500')
    options(sass.cache = FALSE)
    setwd(root)
    source('global.R', local = .GlobalEnv)
    source('server.R', local = .GlobalEnv)
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

        if (scenario == 'rapid-selection') {
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
