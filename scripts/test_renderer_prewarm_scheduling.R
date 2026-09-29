#!/usr/bin/env Rscript

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

server_text <- read_text("server.R")
env_text <- read_text(".env.example")
cache_warm_text <- read_text(file.path("R", "server_cache_warm.R"))

warm_start <- regexpr("warm_gene_plot_renderer_once <- function", server_text, fixed = TRUE)[[1L]]
warm_end <- regexpr("sanitize_girafe_tooltip_text <- function", server_text, fixed = TRUE)[[1L]]
stopifnot(warm_start > 0L, warm_end > warm_start)
warm_block <- substr(server_text, warm_start, warm_end - 1L)

stopifnot(
  grepl('session$onFlushed(function() {', warm_block, fixed = TRUE),
  grepl('once = TRUE', warm_block, fixed = TRUE),
  grepl('APP_GENE_PLOT_RENDERER_PREWARM_DELAY_MS', warm_block, fixed = TRUE),
  grepl('run_renderer_prewarm <- function()', warm_block, fixed = TRUE),
  grepl('if (isTRUE(secondary_work_should_yield()))', warm_block, fixed = TRUE),
  grepl('session_later(run_renderer_prewarm, delay = secondary_work_retry_delay_sec)', warm_block, fixed = TRUE),
  grepl('delay = max(renderer_prewarm_delay_ms / 1000, secondary_work_initial_delay_sec)', warm_block, fixed = TRUE),
  grepl('warm_gene_plot_renderer_once()', warm_block, fixed = TRUE),
  !grepl('}, delay = 0.15)', warm_block, fixed = TRUE),
  grepl('APP_GENE_PLOT_RENDERER_PREWARM=1', env_text, fixed = TRUE),
  grepl('APP_GENE_PLOT_RENDERER_PREWARM_DELAY_MS=1000', env_text, fixed = TRUE),
  grepl('APP_SECONDARY_WORK_INITIAL_DELAY_MS=2500', env_text, fixed = TRUE),
  grepl('APP_SECONDARY_WORK_RETRY_MS=750', env_text, fixed = TRUE),
  grepl('APP_TABIX_PROBE_ON_WARM=0', env_text, fixed = TRUE),
  grepl('Sys.getenv("APP_TABIX_PROBE_ON_WARM", "0")', cache_warm_text, fixed = TRUE),
  grepl('if (!isTRUE(tabix_probe_on_warm()))', cache_warm_text, fixed = TRUE)
)

secondary_start <- regexpr("secondary_work_initial_delay_sec <-", server_text, fixed = TRUE)[[1L]]
secondary_end <- regexpr("begin_session_source_restore <-", server_text, fixed = TRUE)[[1L]]
stopifnot(secondary_start > 0L, secondary_end > secondary_start)
secondary_block <- substr(server_text, secondary_start, secondary_end - 1L)
stopifnot(
  grepl('APP_SECONDARY_WORK_INITIAL_DELAY_MS', secondary_block, fixed = TRUE),
  grepl('APP_SECONDARY_WORK_RETRY_MS', secondary_block, fixed = TRUE),
  grepl('isolate(searchRunState())', secondary_block, fixed = TRUE),
  grepl('function(mode_state) isTRUE((mode_state %||% list())$active)', secondary_block, fixed = TRUE),
  grepl('first_paint_gate_rv <- get0(', secondary_block, fixed = TRUE),
  grepl('"orthoFirstPaintGate"', secondary_block, fixed = TRUE),
  grepl('isTRUE((gate %||% list())$enabled) && !isTRUE((gate %||% list())$released)', secondary_block, fixed = TRUE)
)

string_start <- regexpr("schedule_string_worker_prewarm <- function", server_text, fixed = TRUE)[[1L]]
string_end <- regexpr("schedule_string_worker_prewarm()", server_text, fixed = TRUE)[[1L]]
cache_start <- regexpr("schedule_cache_warm_build <- function", server_text, fixed = TRUE)[[1L]]
lookup_start <- regexpr("schedule_lookup_worker_prewarm <- function", server_text, fixed = TRUE)[[1L]]
fast_start <- regexpr("schedule_fast_search_preparation <- function", server_text, fixed = TRUE)[[1L]]
fast_end <- regexpr("check_search_preparation_gate <- function", server_text, fixed = TRUE)[[1L]]
stopifnot(all(c(string_start, string_end, cache_start, lookup_start, fast_start, fast_end) > 0L))

string_block <- substr(server_text, string_start, string_end - 1L)
cache_block <- substr(server_text, cache_start, lookup_start - 1L)
lookup_block <- substr(server_text, lookup_start, fast_start - 1L)
fast_block <- substr(server_text, fast_start, fast_end - 1L)
stopifnot(
  grepl('session_later(launch_string_worker_prewarm, delay = secondary_work_retry_delay_sec)', string_block, fixed = TRUE),
  grepl('delay = max(delay_val, secondary_work_initial_delay_sec)', string_block, fixed = TRUE),
  grepl('session_later(run_next, delay = secondary_work_retry_delay_sec)', cache_block, fixed = TRUE),
  grepl('session_later(finish_this_warm, delay = secondary_work_retry_delay_sec)', cache_block, fixed = TRUE),
  grepl('complete_annotation_future <- function(idx = NULL, err = NULL)', cache_block, fixed = TRUE),
  grepl('annotation callback yield: user search active', cache_block, fixed = TRUE),
  grepl('function() complete_annotation_future(idx = idx, err = err)', cache_block, fixed = TRUE),
  grepl('app_env_flag("APP_TABIX_PROBE_ON_WARM", FALSE)', cache_block, fixed = TRUE),
  grepl('session_later(launch, delay = secondary_work_retry_delay_sec)', lookup_block, fixed = TRUE),
  grepl('session_later(run_followup_queue, delay = secondary_work_retry_delay_sec)', fast_block, fixed = TRUE),
  grepl('session_later(start_followup_queue, delay = secondary_work_initial_delay_sec)', fast_block, fixed = TRUE),
  grepl('app_env_flag("APP_TABIX_PROBE_ON_WARM", FALSE)', fast_block, fixed = TRUE)
)

search_active_fixture <- function(state) {
  is.list(state) && length(state) > 0L && any(vapply(
    state,
    function(mode_state) isTRUE(mode_state$active),
    logical(1)
  ))
}
secondary_should_yield_fixture <- function(state, gate = list()) {
  search_active_fixture(state) ||
    (isTRUE(gate$enabled) && !isTRUE(gate$released))
}
stopifnot(
  !search_active_fixture(list(homologous = list(active = FALSE))),
  search_active_fixture(list(homologous = list(active = TRUE), orthologous = list(active = FALSE))),
  !search_active_fixture(list(homologous = list(active = FALSE), orthologous = list(active = FALSE))),
  secondary_should_yield_fixture(
    list(orthologous = list(active = FALSE)),
    list(enabled = TRUE, released = FALSE)
  ),
  !secondary_should_yield_fixture(
    list(orthologous = list(active = FALSE)),
    list(enabled = TRUE, released = TRUE)
  ),
  !secondary_should_yield_fixture(
    list(orthologous = list(active = FALSE)),
    list(enabled = FALSE, released = FALSE)
  )
)

secondary_callback_fixture <- function(active_states) {
  pending <- TRUE
  runs <- 0L
  for (active in active_states) {
    if (!pending) break
    if (isTRUE(active)) next
    runs <- runs + 1L
    pending <- FALSE
  }
  list(pending = pending, runs = runs)
}
stopifnot(
  identical(secondary_callback_fixture(c(TRUE, TRUE, FALSE)), list(pending = FALSE, runs = 1L)),
  identical(secondary_callback_fixture(c(TRUE, TRUE)), list(pending = TRUE, runs = 0L))
)

message("secondary-prewarm-yield-scheduling-ok")
