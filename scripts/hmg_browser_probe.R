    # Benchmark-only session instrumentation, injected by benchmark_hmg_browser.R.
    .bench_lag <- numeric()
    .bench_due <- proc.time()[['elapsed']]+0.05
    .bench_tick <- function() {
        if (session$isClosed()) return(invisible(NULL))
        now <- proc.time()[['elapsed']]
        .bench_lag <<- c(tail(.bench_lag,999L), max(0,1000*(now-.bench_due)))
        .bench_due <<- now+0.05
        later::later(.bench_tick,0.05)
    }
    later::later(.bench_tick,0.05)
    observeEvent(input$generate1, {
        .bench_lag <<- numeric()
        cat('[BENCH_SEARCH]',as.character(input$filter1),'\n')
    },priority=10000,ignoreInit=TRUE)
    observeEvent(input$hmg_bench_browser, {
        cat('[BENCH_BROWSER]',jsonlite::toJSON(input$hmg_bench_browser,auto_unbox=TRUE),'\n')
        cat('[BENCH_LAG] max_ms=',max(c(0,.bench_lag)),' p95_ms=',
            if(length(.bench_lag)) unname(quantile(.bench_lag,.95)) else NA_real_,'\n',sep='')
        cat('[BENCH_MEMORY]',session_memory_metric_fields(),'\n')
        # Capture actual state and complete group outputs for the independent A/B oracle.
        gene <- gsub('[^A-Za-z0-9_-]','',as.character(isolate(plotGeneMetaHomologous())[[isolate(sortedPlotIdsHomologous())[[1]]]]$display_gene_name %||% 'unknown'))
        saveRDS(list(ids=isolate(sortedPlotIdsHomologous()),meta=isolate(plotGeneMetaHomologous()),
            org=isolate(organismInfoHomologous()),ann=isolate(annotationPathsHomologous()),
            titles=isolate(titlesHomologous())),file.path(out,paste0(gene,'-state.rds')))
        saveRDS(isolate(homoMultiTranscriptGeneGroups()),file.path(out,paste0(gene,'-groups.rds')))
    },ignoreInit=TRUE)
