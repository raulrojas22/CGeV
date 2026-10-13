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
    if (scenario == 'restore-report') {
        .GlobalEnv$.audit_original_shared <- get('init_shared_analysis_domain',mode='function')
        .GlobalEnv$init_shared_analysis_domain <- function(...) {
            args<-list(...);.GlobalEnv$.audit_report_lastz<-args$run_lastz_fn
            .GlobalEnv$.audit_original_shared(...)
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
        if (scenario %in% c('restore-queued','restore-worker-error','restore-new-search','restore-repeat','restore-batch','restore-malformed','restore-read-error','restore-closed','restore-schema1')) {
            session$setInputs(ortho_preloaded_species = c('a', 'c')); session$elapse(600); wait_ready()
            search('GENE2'); cards(c('Review alpha','Review gamma'), 'GENE2', c(120,180), '-')
            saved <- build_work_session_snapshot(); path <- file.path(data_root, 'saved.rds')
            if (scenario == 'restore-schema1') saved$schema_version <- 1L
            saveRDS(saved, path)
            session$setInputs(ortho_preloaded_species = c('a', 'b')); session$elapse(600); wait_ready()
            workers <- list(); released <- file.path(data_root, 'release')
            on.exit({writeLines('release', released); for(w in workers) w$wait(10000)}, add=TRUE)
            target <- environment(search_future_promise); original_future <- search_future_promise
            assign('search_future_promise', function(mode, id, expr, ..., envir=parent.frame()) {
                expression <- substitute(expr); job <- get('job', envir=envir, inherits=TRUE)
                index <- length(workers)+1L; spec <- file.path(data_root,paste0('job',index,'.rds'))
                result <- file.path(data_root,paste0('result',index,'.rds'))
                saveRDS(list(expr=expression,job=job,fail=scenario=='restore-worker-error'),spec)
                workers[[index]] <<- processx::process$new(file.path(R.home('bin'),'Rscript'),
                    c(file.path(root, 'tests/fixtures/search_restoration_worker.R'),root,spec,released,result),
                    stdout=file.path(data_root,paste0('worker',index,'.log')),stderr='2>&1')
                promises::promise(function(resolve,reject) {
                    poll <- function() {
                        if(file.exists(result)) {
                            x <- readRDS(result); stopifnot(x$pid != Sys.getpid())
                            if(inherits(x$value,'error')) reject(x$value) else resolve(x$value)
                        } else later::later(poll,.01)
                    }; poll()
                })
            },envir=target)
            Sys.setenv(APP_ORTHO_LOCAL_PROGRESSIVE='1')
            clear_orthologous_visualizations(); lastOrthoSearchedGene('')
            if (scenario == 'restore-batch') {
                set_batch_search_state('orthologous',2L); queue_global_batch_search(c('GENE3','TP53'),'orthologous')
                deadline<-Sys.time()+3
                while(!length(Filter(function(x)x$type=='cgv_trigger_ortho_search',messages))&&Sys.time()<deadline) {later::run_now(.05);session$flushReact()}
                acknowledge()
            } else search('GENE3')
            a <- get_search_run_mode_state('orthologous')$mode_state$id
            stopifnot(length(workers)==2L,owns_search_run('orthologous',a))
            if (scenario == 'restore-malformed') {
                invalid <- rep(list(saved),6L)
                invalid[[1]]$app <- 'invalid'
                invalid[[2]]$app$preferred_workflow <- c('homologous','orthologous')
                invalid[[3]]$app$alignment_parameters$lastz$min_identity <- c(20,90)
                invalid[[4]]$orthologous$plots[[1]]$plot_data <- 'invalid'
                invalid[[5]]$orthologous$plot_counter <- c(1,2)
                invalid[[6]]$app$preferred_workflow <- NA_character_
                for (bad in invalid) {
                    saveRDS(bad,path)
                    session$setInputs(upload_work_session=data.frame(datapath=path),load_work_session_btn=sample.int(100000,1))
                    stopifnot(owns_search_run('orthologous',a),length(activePlotIdsOrthologous())==0L)
                }
            }
            if (scenario == 'restore-read-error') writeLines('not an RDS file',path)
            if (scenario == 'restore-closed') {
                session$close()
                stopped <- tryCatch({apply_work_session_snapshot(saved);FALSE},error=function(e) TRUE)
                stopifnot(stopped)
            } else {
                session$setInputs(upload_work_session=data.frame(datapath=path),load_work_session_btn=1)
                if(scenario=='restore-repeat') session$setInputs(load_work_session_btn=2)
                if (!scenario %in% c('restore-malformed','restore-read-error')) {
                    cards(c('Review alpha','Review gamma'), 'GENE2', c(120,180), '-')
                    stopifnot(!owns_search_run('orthologous',a))
                } else stopifnot(owns_search_run('orthologous',a))
            }
                if(scenario=='restore-new-search') {
                    assign('search_future_promise',original_future,envir=target); Sys.setenv(APP_ORTHO_LOCAL_PROGRESSIVE='0')
                    session$setInputs(ortho_preloaded_species=c('a','c'),
                        cgv_session_source_restored=list(input_id='ortho_preloaded_species'))
                    session$elapse(600);wait_ready()
                    search('GENE3'); pending<-pendingOrthoGeneChange()
                    stopifnot(is.list(pending))
                    session$setInputs(cgv_search_confirmation_accepted=list(mode='orthologous',request_id=pending$request_id))
                    acknowledge(); cards(c('Review alpha','Review gamma'),'GENE3',c(210,240),'+')
                }
            new_owner <- if(scenario=='restore-new-search') get_search_run_mode_state('orthologous')$mode_state else NULL
            before_status <- searchStatusOrthologous()
            cat('BEFORE LATE SETTLEMENT cards=',length(activePlotIdsOrthologous()),' status=',get_search_run_mode_state('orthologous')$mode_state$status,'\n')
            writeLines('release',released)
            for(w in workers) {w$wait(15000);stopifnot(!w$is_alive(),w$get_exit_status()==0L)}
            for(index in seq_along(workers)) {
                result<-readRDS(file.path(data_root,paste0('result',index,'.rds')))
                stopifnot(result$pid!=Sys.getpid())
                if(scenario=='restore-worker-error') stopifnot(inherits(result$value,'error'),identical(conditionMessage(result$value),'controlled late worker failure'))
                else stopifnot(isTRUE(result$value$found),identical(range(as.numeric(result$value$data$V4),as.numeric(result$value$data$V5)),c(210,240)))
            }
            for(i in 1:30) {later::run_now(.02);session$flushReact()}
            cat('AFTER LATE SETTLEMENT cards=',length(activePlotIdsOrthologous()),
                ' genes=',paste(vapply(plotGeneMetaOrthologous()[as.character(activePlotIdsOrthologous())],function(x)x$display_gene_name,character(1)),collapse=','),'\n')
            if (scenario %in% c('restore-malformed','restore-read-error')) cards(c('Review alpha','Review beta'), 'GENE3', c(210,240), '+')
            else if (scenario != 'restore-closed') {
                if(scenario=='restore-new-search') {
                    cards(c('Review alpha','Review gamma'),'GENE3',c(210,240),'+')
                    stopifnot(identical(new_owner,get_search_run_mode_state('orthologous')$mode_state))
                } else cards(c('Review alpha','Review gamma'), 'GENE2', c(120,180), '-')
                stopifnot(identical(searchStatusOrthologous(),before_status))
                if(scenario=='restore-batch') stopifnot(length(batchSearchQueue())==0L,batchSearchRemaining()==0L)

            }
        } else if (scenario %in% c('restore-pending-h','restore-pending-o','restore-external-queued','restore-view-callbacks','restore-isolation','restore-benchmark','restore-stale-source-ack','restore-external-settle','restore-external-error','restore-report')) {
            session$setInputs(ortho_preloaded_species=c('a','c'));session$elapse(600);wait_ready()
            search('GENE2');cards(c('Review alpha','Review gamma'),'GENE2',c(120,180),'-')
            saved <- build_work_session_snapshot();path<-file.path(data_root,'restore.rds');saveRDS(saved,path)
            load <- function() session$setInputs(upload_work_session=data.frame(datapath=path),load_work_session_btn=1)
            if (scenario %in% c('restore-pending-h','restore-pending-o')) {
                mode<-if(scenario=='restore-pending-h') 'homologous' else 'orthologous'
                if(mode=='homologous') {
                    session$setInputs(homo_data_mode='preloaded',homo_preloaded_species='b')
                    deadline<-Sys.time()+10
                    while(!searchPreparationReadyHomo()&&Sys.time()<deadline){later::run_now(.05);session$flushReact()}
                    stopifnot(searchPreparationReadyHomo())
                }
                invalidate_search_preparation(mode,ready=FALSE)
                if(mode=='homologous')execute_homologous_search('GENE3') else execute_orthologous_search('GENE3')
                type<-if(mode=='homologous')'cgv_trigger_homo_search' else 'cgv_trigger_ortho_search'
                ticket<-tail(Filter(function(x)x$type==type,messages),1)[[1]]$payload
                if(mode=='homologous')session$setInputs(cgv_homo_search_dispatch=ticket) else session$setInputs(cgv_ortho_search_dispatch=ticket)
                stopifnot(search_request_is_current(mode,ticket$request_id))
                load();stopifnot(!search_request_is_current(mode,ticket$request_id))
                if(mode=='homologous') searchPreparationReadyHomo(TRUE)
                if(mode=='homologous')session$setInputs(cgv_homo_search_dispatch=ticket) else session$setInputs(cgv_ortho_search_dispatch=ticket)
                later::run_now(.2);session$flushReact()
                cards(c('Review alpha','Review gamma'),'GENE2',c(120,180),'-')
                stopifnot(length(activePlotIdsHomologous())==0L,length(fileDataHomologous())==0L)
            } else if(scenario=='restore-external-queued') {
                clear_orthologous_visualizations();lastOrthoSearchedGene('')
                submissions<-0L; target<-environment(search_future_promise)
                assign('search_future_promise',function(...) {submissions<<-submissions+1L;stop('obsolete rescue submitted')},envir=target)
                search('MISSING'); a<-get_search_run_mode_state('orthologous')$mode_state$id
                stopifnot(owns_search_run('orthologous',a))
                load();later::run_now(1.5);session$flushReact()
                stopifnot(submissions==0L,!owns_search_run('orthologous',a))
                cards(c('Review alpha','Review gamma'),'GENE2',c(120,180),'-')
            } else if(scenario=='restore-report') {
                session$setInputs(ortho_preloaded_species=c('a','b'));session$elapse(600);wait_ready()
                clear_orthologous_visualizations();lastOrthoSearchedGene('');search('GENE3')
                cards(c('Review alpha','Review beta'),'GENE3',c(210,240),'+')
                report_lastz <- .GlobalEnv$.audit_report_lastz
                stopifnot(is.function(report_lastz))
                holds<-list()
                assign('run_canonical_lastz_contexts_async',function(...) promises::promise(function(resolve,reject) holds[[length(holds)+1L]]<<-list(resolve=resolve,reject=reject)),envir=environment(report_lastz))
                pending <- report_lastz('ortho');stopifnot(length(holds)>0L)
                sharedAnalysisDomain$pending <- list(request_id='outgoing-report',phase='lastz')
                sharedAnalysisDomain$busy <- TRUE
                sharedAnalysisDomain$alignment_request <- list(workflow='ortho',mode='blocks')
                sharedAnalysisDomain$result <- list(token='existing-valid-report')
                load();before<-pipLocalAlignmentStateOrthologous();before_multi<-multipipLocalAlignmentStateOrthologous()
                for(hold in holds) hold$resolve(list(status='done',runs=list(old_worker=list(status='ok',blocks=data.frame())),stamp='GENE3-worker'))
                for(i in 1:30){later::run_now(.02);session$flushReact()}
                cat('REPORT_AFTER_RESTORE pending=',!is.null(sharedAnalysisDomain$pending),' old_alignment_published=',!identical(before,pipLocalAlignmentStateOrthologous()),'\n')
                stopifnot(is.null(sharedAnalysisDomain$pending),is.null(sharedAnalysisDomain$alignment_request),!sharedAnalysisDomain$busy,identical(before,pipLocalAlignmentStateOrthologous()),identical(before_multi,multipipLocalAlignmentStateOrthologous()))
                session$setInputs(cgv_analysis_assets=list(request_id='outgoing-report',assets=list(),missing=character()))
                stopifnot(identical(sharedAnalysisDomain$result,list(token='existing-valid-report')))
                cards(c('Review alpha','Review gamma'),'GENE2',c(120,180),'-')
            } else if(scenario %in% c('restore-external-settle','restore-external-error')) {
                session$setInputs(ortho_preloaded_species=c('a','b'));session$elapse(600);wait_ready()
                clear_orthologous_visualizations();lastOrthoSearchedGene('')
                holds<-list();target<-environment(search_future_promise)
                assign('search_future_promise',function(mode,id,expr,...,envir=parent.frame()) {
                    jobs<-get('chunk_jobs',envir=envir,inherits=TRUE)
                    # Fixed alias-provider outcome; actual GFF/sequence producers
                    # supply the scientific payload accepted by the real rescue.
                    results<-lapply(jobs,function(job) {
                        job$gene_name<-'GENE3';job$enabled_external_sources<-character();job$allow_partial_suggestions<-FALSE
                        run_orthologous_lookup_job_pure(job)
                    })
                    stopifnot(all(vapply(results,function(x)isTRUE(x$found),logical(1))))
                    promises::promise(function(resolve,reject) holds[[length(holds)+1L]]<<-list(resolve=resolve,reject=reject,results=results))
                },envir=target)
                search('MISSING');deadline<-Sys.time()+5
                while(!length(holds)&&Sys.time()<deadline){later::run_now(.05);session$flushReact()}
                a<-get_search_run_mode_state('orthologous')$mode_state$id
                stopifnot(length(holds)>0L,owns_search_run('orthologous',a))
                load();before<-fileDataOrthologous();status<-searchStatusOrthologous()
                for(hold in holds) if(scenario=='restore-external-error') hold$reject(simpleError('late rescue error')) else hold$resolve(hold$results)
                for(i in 1:30){later::run_now(.02);session$flushReact()}
                stopifnot(!owns_search_run('orthologous',a),identical(before,fileDataOrthologous()),identical(status,searchStatusOrthologous()))
                cards(c('Review alpha','Review gamma'),'GENE2',c(120,180),'-')
            } else if(scenario=='restore-stale-source-ack') {
                load()
                session$setInputs(ortho_preloaded_species=c('b','c'))
                stopifnot(!searchPreparationReadyOrtho())
                session$setInputs(cgv_session_source_restored=list(input_id='ortho_preloaded_species'))
                stopifnot(!searchPreparationReadyOrtho())
                session$elapse(600);wait_ready()
                search('GENE3')
                pending<-pendingOrthoGeneChange()
                if(is.list(pending)) {
                    session$setInputs(cgv_search_confirmation_accepted=list(mode='orthologous',request_id=pending$request_id))
                    acknowledge()
                }
                cards(c('Review beta','Review gamma'),'GENE3',c(210,240),'+')
            } else if(scenario=='restore-view-callbacks') {
                clear_orthologous_visualizations();lastOrthoSearchedGene('');search('GENE3')
                cards(c('Review alpha','Review gamma'),'GENE3',c(210,240),'+')
                published<-FALSE; cleaned<-0L
                late<-work_view_guard(function() {published<<-TRUE; fileDataOrthologous(list())},cleanup=function()cleaned<<-cleaned+1L)
                old<-fileDataOrthologous()[[1]];ids<-vapply(saved$orthologous$plots,function(x)x$id,character(1))
                expected_old<-build_plot_metrics_group_payloads('ortho',ids,rep(list(old),length(ids)),representative_name='old gene',annotation_path=file.path(data_root,'annotations/a.gff3'))
                stopifnot(length(expected_old)>0L)
                schedule_plot_metrics_payload_build('ortho',ids,rep(list(old),length(ids)),representative_name='old gene',annotation_path=file.path(data_root,'annotations/a.gff3'))
                schedule_plot_metrics_payload_build('ortho',as.character(1:30),rep(list(old),30),representative_name='old gene',perf_run=list(id='old-metrics'),annotation_path=file.path(data_root,'annotations/a.gff3'))
                apply_work_session_snapshot(saved);metric_before<-plotMetricsOrthologous()
                stopifnot(!dispatch_pending_metrics_build('old-metrics'))
                late();deadline<-Sys.time()+.3
                while(Sys.time()<deadline){later::run_now(.02);session$flushReact()}
                stopifnot(!published,cleaned==1L,identical(metric_before,plotMetricsOrthologous()))
                cards(c('Review alpha','Review gamma'),'GENE2',c(120,180),'-')
            } else if (scenario=='restore-benchmark') {
                size_before<-as.numeric(object.size(searchRunState()))
                elapsed<-replicate(7,system.time(for(i in 1:100)invalidate_work_session_ownership())[['elapsed']]*10)
                size_after<-as.numeric(object.size(searchRunState()))
                stopifnot(size_after<=size_before,length(ls(pendingMetricsBuilds))==0L,length(ls(pendingMetricsPlotIds))==0L)
                cat('RETIREMENT ms_per_call median=',median(elapsed),' range=',paste(range(elapsed),collapse='..'),' state_bytes_before=',size_before,' after=',size_after,' calls=700\n')
            } else {
                restoring_session<-session
                peer <- shiny::MockShinySession$new()
                peer_definition <- .cgv_server_definition
                body(peer_definition) <- as.call(c(as.list(body(peer_definition)),list(quote(environment()))))
                peer_env <- shiny::withReactiveDomain(peer,peer_definition(peer$input,peer$output,peer))
                on.exit(peer$close(),add=TRUE)
                shiny::withReactiveDomain(peer,shiny::isolate({
                    peer$setInputs(homo_data_mode='preloaded',homo_preloaded_species='b',ortho_data_mode='upload',generate1=0,search_gene=0)
                    deadline<-Sys.time()+10
                    while(!peer_env$searchPreparationReadyHomo()&&Sys.time()<deadline){later::run_now(.05);peer$flushReact()}
                    peer$setInputs(filter1='GENE2',generate1=1)
                    science<-peer_env$fileDataHomologous();stopifnot(length(science)>0L)
                    b<-peer_env$begin_search_run('homologous','other-session-B',duplicate_window_sec=0)
                    before<-peer_env$get_search_run_mode_state('homologous')$mode_state
                    restoring_session$setInputs(upload_work_session=data.frame(datapath=path),load_work_session_btn=1)
                    stopifnot(peer_env$owns_search_run('homologous',b$id),identical(before,peer_env$get_search_run_mode_state('homologous')$mode_state),identical(science,peer_env$fileDataHomologous()))
                    did_publish<-FALSE
                    publish<-peer_env$search_operation_guard('homologous',b$id,function(){did_publish<<-TRUE;peer_env$fileDataHomologous(science)})
                    publish();stopifnot(did_publish,identical(science,peer_env$fileDataHomologous()))
                }))
                cards(c('Review alpha','Review gamma'),'GENE2',c(120,180),'-')
            }
        } else if (startsWith(scenario, 'source-')) {
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
