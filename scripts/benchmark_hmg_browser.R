#!/usr/bin/env Rscript
# Temporary instrumentation only. Run from this worktree, then use the normal UI.
# CGV_DATA_ROOT must contain the preloaded annotation/genome. Never use a live cache.
# Rscript scripts/benchmark_hmg_browser.R before|after OUT_DIR PORT
args <- commandArgs(TRUE)
stopifnot(length(args)==3L,args[[1]] %in% c('before','after'))
variant <- args[[1]]
out <- normalizePath(args[[2]],mustWork=FALSE); dir.create(out,recursive=TRUE,showWarnings=FALSE)
Sys.setenv(APP_PERF_TIMING='1',APP_SESSION_METRICS='1',CGV_CACHE_DIR=file.path(out,'cache'))
source('global.R')
text <- if(variant=='before') system2('git',c('show','a5ec791f9270de0b4f2ef202c98d473d6f744a3e:server.R'),stdout=TRUE) else readLines('server.R')
text <- paste(text,collapse='\n')
marker <- '        norm_key_local <- function(x)'
insert <- paste0('        .hmg_t0 <- proc.time()[["elapsed"]]\n',
    '        on.exit(cat("[HMG_COMPUTE] ids=",length(ids)," ms=",1000*(proc.time()[["elapsed"]]-.hmg_t0),"\\n",sep=""))\n',marker)
stopifnot(grepl(marker,text,fixed=TRUE))
text <- sub(marker,insert,text,fixed=TRUE)
# A temporary session probe records demand results, lag and browser timestamps.
probe <- paste0('\n',paste(readLines('scripts/hmg_browser_probe.R'),collapse='\n'),'\n')
text <- sub('    session_t0 <- app_perf_now()',paste0('    session_t0 <- app_perf_now()',probe),text,fixed=TRUE)
path <- file.path(out,paste0(variant,'-instrumented-server.R')); writeLines(text,path)
server_fn <- source(path,local=TRUE)$value
ui_fn <- source('ui.R',local=TRUE)$value
bench_js <- "$(function(){var start=0,first=null;$(document).on('shiny:inputchanged.hmgbench',function(e){if(e.name==='generate1'&&e.value){start=performance.now();first=null;}if(e.name==='cgv_card_complete'&&start){Shiny.setInputValue('hmg_bench_browser',{first_message_ms:first,complete_ms:performance.now()-start,card:e.value,nonce:Date.now()},{priority:'event'});}});$(document).on('shiny:message.hmgbench',function(){if(start&&first===null)first=performance.now()-start;});});"
ui_bench <- htmltools::tagList(ui_fn,shiny::tags$script(shiny::HTML(bench_js)))
cat('[BENCH_PID]',Sys.getpid(),'variant',variant,'\n')
app <- shiny::shinyApp(ui=ui_bench,server=server_fn)
app$staticPaths <- shiny::shinyAppDir(getwd())$staticPaths
shiny::runApp(app,host='127.0.0.1',port=as.integer(args[[3]]),launch.browser=FALSE)
