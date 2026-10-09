# Preserve the existing whole-map getter/setter while allowing a card to subscribe
# to one entry. Updates are synchronous, so no extra observer or flush is needed.
make_keyed_reactive_map <- function(value = list()) {
    whole <- shiny::reactiveVal(value)
    entries <- new.env(parent = emptyenv())
    entry_value <- function(value, key) {
        tryCatch(value[[key]], error = function(e) NULL)
    }
    accessor <- function(x) {
        if (missing(x)) return(whole())
        result <- whole(x)
        for (key in ls(entries, all.names = TRUE)) {
            entries[[key]](entry_value(x, key))
        }
        invisible(result)
    }
    attr(accessor, "read_key") <- function(key) {
        stopifnot(is.character(key), length(key) == 1L, !is.na(key), nzchar(key))
        if (!exists(key, envir = entries, inherits = FALSE)) {
            entries[[key]] <- shiny::reactiveVal(entry_value(shiny::isolate(whole()), key))
        }
        entries[[key]]()
    }
    accessor
}

read_reactive_map_key <- function(map, key) {
    read_key <- attr(map, "read_key", exact = TRUE)
    if (is.function(read_key)) read_key(key) else map()[[key]]
}

init_server_state_helpers_domain <- function(
    autocompleteBuildEpochs_rv,
    searchRunState_rv,
    session_alive = function() TRUE
) {
    pending_session_source_restore <- shiny::reactiveVal(list(
        homologous = "",
        orthologous = ""
    ))

    parse_positive_int_env <- function(name, default_value) {
        raw <- trimws(as.character(Sys.getenv(name, as.character(default_value)) %||% ""))
        val <- suppressWarnings(as.integer(raw))
        if (!is.finite(val) || is.na(val) || val < 1L) {
            as.integer(default_value)
        } else {
            as.integer(val)
        }
    }

    get_autocomplete_epoch <- function(input_id) {
        key <- as.character(input_id %||% "")
        ep <- autocompleteBuildEpochs_rv()
        if (!is.list(ep)) ep <- list()
        val <- suppressWarnings(as.integer(ep[[key]] %||% 0L))
        if (!is.finite(val) || is.na(val)) {
            val <- 0L
        }
        as.integer(val)
    }

    bump_autocomplete_epoch <- function(input_id) {
        key <- as.character(input_id %||% "")
        if (!nzchar(key)) {
            return(invisible(0L))
        }
        ep <- autocompleteBuildEpochs_rv()
        if (!is.list(ep)) ep <- list()
        current <- suppressWarnings(as.integer(ep[[key]] %||% 0L))
        if (!is.finite(current) || is.na(current)) {
            current <- 0L
        }
        next_val <- as.integer(current + 1L)
        ep[[key]] <- next_val
        autocompleteBuildEpochs_rv(ep)
        next_val
    }

    cancel_autocomplete_background <- function(input_ids = c("filter1", "gene_name")) {
        ids <- unique(as.character(input_ids %||% character(0)))
        ids <- ids[nzchar(ids)]
        invisible(lapply(ids, bump_autocomplete_epoch))
    }

    compose_autocomplete_still_valid <- function(input_id, epoch_token = NULL, extra_check = NULL) {
        key <- as.character(input_id %||% "")
        epoch_target <- suppressWarnings(as.integer(epoch_token %||% get_autocomplete_epoch(key)))
        if (!is.finite(epoch_target) || is.na(epoch_target)) epoch_target <- 0L
        function() {
            same_epoch <- identical(get_autocomplete_epoch(key), as.integer(epoch_target))
            extra_ok <- TRUE
            if (is.function(extra_check)) {
                extra_ok <- tryCatch(isTRUE(extra_check()), error = function(e) FALSE)
            }
            isTRUE(same_epoch && extra_ok)
        }
    }

    # IDs are session-local counters, never fingerprints or scientific cache keys.
    next_search_id <- 0
    pending_search_requests <- new.env(parent = emptyenv())
    search_dispatches <- new.env(parent = emptyenv())
    search_action_counts <- new.env(parent = emptyenv())
    next_dispatch_id <- 0
    search_mode <- function(mode) {
        key <- tolower(trimws(as.character(mode %||% "")))
        if (key %in% c("homologous", "orthologous")) key else "homologous"
    }
    new_search_id <- function() {
        next_search_id <<- next_search_id + 1
        paste0("search_", next_search_id)
    }
    get_search_run_mode_state <- function(mode) {
        mode_key <- tolower(trimws(as.character(mode %||% "")))
        if (!mode_key %in% c("homologous", "orthologous")) {
            mode_key <- "homologous"
        }
        # Liveness must precede even a read of a destroyed session reactive.
        st <- if (isTRUE(session_alive())) shiny::isolate(searchRunState_rv()) else list()
        if (!is.list(st)) st <- list()
        mode_st <- st[[mode_key]]
        if (!is.list(mode_st)) {
            mode_st <- list(active = FALSE, last_fingerprint = "", last_completed = as.numeric(0))
        }
        mode_st$active <- isTRUE(mode_st$active)
        mode_st$last_fingerprint <- as.character(mode_st$last_fingerprint %||% "")
        mode_st$last_completed <- suppressWarnings(as.numeric(mode_st$last_completed %||% 0))
        if (!is.finite(mode_st$last_completed)) mode_st$last_completed <- 0
        mode_st$id <- as.character(mode_st$id %||% "")
        mode_st$status <- as.character(mode_st$status %||% "idle")
        mode_st$children <- as.integer(mode_st$children %||% 0L)
        list(mode_key = mode_key, state = st, mode_state = mode_st)
    }

    request_search_run <- function(mode) {
        if (!isTRUE(session_alive())) return(list(ok = FALSE, reason = "closed"))
        key <- search_mode(mode)
        if (isTRUE(get_search_run_mode_state(key)$mode_state$active)) {
            return(list(ok = FALSE, reason = "active", mode = key))
        }
        id <- new_search_id()
        pending_search_requests[[key]] <- id
        list(ok = TRUE, id = id, mode = key)
    }
    search_request_is_current <- function(mode, id) {
        isTRUE(session_alive()) && nzchar(as.character(id %||% "")) &&
            identical(pending_search_requests[[search_mode(mode)]], id)
    }
    cancel_search_request <- function(mode, id) {
        if (!search_request_is_current(mode, id)) return(invisible(FALSE))
        pending_search_requests[[search_mode(mode)]] <- NULL
        invisible(TRUE)
    }
    get_search_request_id <- function(mode) {
        if (!isTRUE(session_alive())) return(NULL)
        pending_search_requests[[search_mode(mode)]]
    }
    prepare_search_dispatch <- function(mode, id) {
        if (!search_request_is_current(mode, id)) return(NULL)
        next_dispatch_id <<- next_dispatch_id + 1
        ticket <- list(request_id = id, dispatch_id = next_dispatch_id)
        search_dispatches[[search_mode(mode)]] <- ticket
        ticket
    }
    take_search_event <- function(mode, action_count = 0, dispatch = NULL) {
        if (!isTRUE(session_alive())) return(NULL)
        key <- search_mode(mode)
        previous <- search_action_counts[[key]] %||% 0
        search_action_counts[[key]] <- action_count %||% 0
        if (!identical(as.numeric(action_count %||% 0), as.numeric(previous)) &&
            as.numeric(action_count %||% 0) > 0) {
            request <- request_search_run(key)
            return(if (isTRUE(request$ok)) request$id else NULL)
        }
        ticket <- search_dispatches[[key]]
        if (!is.list(dispatch) || !is.list(ticket) ||
            !identical(dispatch$request_id, ticket$request_id) ||
            !identical(as.numeric(dispatch$dispatch_id), as.numeric(ticket$dispatch_id)) ||
            !search_request_is_current(key, ticket$request_id)) return(NULL)
        search_dispatches[[key]] <- NULL
        ticket$request_id
    }
    owns_search_run <- function(mode, id, terminal = FALSE) {
        if (!isTRUE(session_alive()) || !nzchar(as.character(id %||% ""))) return(FALSE)
        current <- get_search_run_mode_state(mode)$mode_state
        identical(current$id, id) && (identical(current$status, "running") ||
            (isTRUE(terminal) && current$status %in% c("completed", "failed")))
    }
    begin_search_run <- function(mode, fingerprint = "", duplicate_window_sec = 1.2, request_id = NULL,
                                 source_key = "") {
        if (!isTRUE(session_alive())) return(list(ok = FALSE, reason = "closed"))
        state_info <- get_search_run_mode_state(mode)
        mode_key <- state_info$mode_key
        st <- state_info$state
        mode_st <- state_info$mode_state
        fp <- tolower(trimws(as.character(fingerprint %||% "")))
        now_num <- as.numeric(Sys.time())
        if (isTRUE(mode_st$active)) {
            return(list(ok = FALSE, reason = "active", mode = mode_key))
        }
        if (!is.null(request_id) && !search_request_is_current(mode_key, request_id)) {
            return(list(ok = FALSE, reason = "stale", mode = mode_key))
        }
        if (nzchar(fp) && identical(fp, mode_st$last_fingerprint)) {
            dt <- now_num - as.numeric(mode_st$last_completed %||% 0)
            if (is.finite(dt) && dt >= 0 && dt < as.numeric(duplicate_window_sec %||% 1.2)) {
                return(list(ok = FALSE, reason = "duplicate", mode = mode_key))
            }
        }
        mode_st$active <- TRUE
        mode_st$id <- if (is.null(request_id)) new_search_id() else request_id
        mode_st$status <- "running"
        # Lifecycle metadata only; this never enters a scientific fingerprint.
        mode_st$source_key <- source_key
        mode_st$fingerprint <- fp
        mode_st$children <- 0L
        mode_st$local_outcome <- NULL
        mode_st$admission_completed_at <- NULL
        pending_search_requests[[mode_key]] <- NULL
        st[[mode_key]] <- mode_st
        searchRunState_rv(st)
        list(ok = TRUE, reason = "started", mode = mode_key, id = mode_st$id)
    }

    finish_search_run <- function(mode, id, outcome = "completed") {
        stopifnot(outcome %in% c("completed", "cancelled", "failed"))
        if (!owns_search_run(mode, id)) return(invisible(FALSE))
        state_info <- get_search_run_mode_state(mode)
        mode_key <- state_info$mode_key
        st <- state_info$state
        mode_st <- state_info$mode_state
        if (identical(outcome, "completed") && mode_st$children > 0L) return(invisible(FALSE))
        mode_st$active <- FALSE
        mode_st$status <- outcome
        if (identical(outcome, "completed")) {
            mode_st$last_fingerprint <- mode_st$fingerprint
            mode_st$last_completed <- mode_st$admission_completed_at %||% as.numeric(Sys.time())
        }
        if (identical(outcome, "cancelled")) {
            # A cancelled retry must not inherit duplicate suppression for its query.
            if (identical(mode_st$last_fingerprint, mode_st$fingerprint)) {
                mode_st$last_fingerprint <- ""
                mode_st$last_completed <- 0
            }
        }
        st[[mode_key]] <- mode_st
        searchRunState_rv(st)
        invisible(TRUE)
    }
    cancel_search_run <- function(mode, id) {
        cancelled_request <- cancel_search_request(mode, id)
        cancelled_run <- finish_search_run(mode, id, "cancelled")
        invisible(isTRUE(cancelled_request) || isTRUE(cancelled_run))
    }
    # Local execution releases admission; an owned rescue can still be running.
    release_search_admission <- function(mode, id, outcome = "completed") {
        stopifnot(outcome %in% c("completed", "failed"))
        if (!owns_search_run(mode, id)) return(invisible(FALSE))
        info <- get_search_run_mode_state(mode)
        current <- info$mode_state
        if (!isTRUE(current$active)) return(invisible(FALSE))
        if (identical(current$local_outcome, "failed")) outcome <- "failed"
        if (current$children == 0L) return(finish_search_run(mode, id, outcome))
        current$active <- FALSE
        current$local_outcome <- outcome
        if (identical(outcome, "completed")) {
            # Preserve the existing duplicate window at local completion, even
            # while rescue owns the still-running operation. This is admission
            # bookkeeping, not a terminal success transition.
            current$admission_completed_at <- as.numeric(Sys.time())
            current$last_fingerprint <- current$fingerprint
            current$last_completed <- current$admission_completed_at
        }
        info$state[[info$mode_key]] <- current
        searchRunState_rv(info$state)
        invisible(TRUE)
    }
    start_search_child <- function(mode, id) {
        force(mode); force(id)
        if (!owns_search_run(mode, id)) return(NULL)
        info <- get_search_run_mode_state(mode)
        info$mode_state$children <- info$mode_state$children + 1L
        info$state[[info$mode_key]] <- info$mode_state
        searchRunState_rv(info$state)
        settled <- FALSE
        function(outcome = "completed") {
            if (settled) return(invisible(FALSE))
            settled <<- TRUE
            settle_search_child(mode, id, outcome)
        }
    }
    settle_search_child <- function(mode, id, outcome = "completed") {
        stopifnot(outcome %in% c("completed", "failed"))
        if (!owns_search_run(mode, id)) return(invisible(FALSE))
        info <- get_search_run_mode_state(mode)
        current <- info$mode_state
        if (current$children < 1L) return(invisible(FALSE))
        current$children <- current$children - 1L
        if (identical(outcome, "failed")) current$local_outcome <- "failed"
        info$state[[info$mode_key]] <- current
        searchRunState_rv(info$state)
        if (current$children == 0L && !isTRUE(current$active)) {
            finish_search_run(mode, id, current$local_outcome %||% "completed")
        }
        invisible(TRUE)
    }
    guard_search_continuation <- function(mode, id, callback, cleanup = function() NULL, terminal = FALSE) {
        force(mode); force(id); force(callback); force(cleanup); force(terminal)
        function(...) {
            # Cleanup here must own only nonreactive resources. It runs even when
            # the session is closed or the publishing callback is suppressed.
            on.exit(cleanup(), add = TRUE)
            if (!owns_search_run(mode, id, terminal = terminal)) return(invisible(NULL))
            callback(...)
        }
    }

    begin_session_source_restore <- function(panel, expected_key) {
        panel_key <- tolower(trimws(as.character(panel %||% "")))
        if (!panel_key %in% c("homologous", "orthologous")) {
            return(invisible(FALSE))
        }
        key <- trimws(as.character(expected_key %||% ""))
        pending <- pending_session_source_restore()
        pending[[panel_key]] <- key
        pending_session_source_restore(pending)
        invisible(nzchar(key))
    }

    consume_session_source_restore <- function(panel, observed_key) {
        panel_key <- tolower(trimws(as.character(panel %||% "")))
        if (!panel_key %in% c("homologous", "orthologous")) {
            return("none")
        }
        pending <- pending_session_source_restore()
        expected <- trimws(as.character(pending[[panel_key]] %||% ""))
        if (!nzchar(expected)) {
            return("none")
        }
        observed <- trimws(as.character(observed_key %||% ""))
        if (identical(observed, expected)) {
            pending[[panel_key]] <- ""
            pending_session_source_restore(pending)
            return("complete")
        }
        "pending"
    }

    finish_session_source_restore <- function(panel) {
        panel_key <- tolower(trimws(as.character(panel %||% "")))
        if (!panel_key %in% c("homologous", "orthologous")) {
            return(invisible(FALSE))
        }
        pending <- pending_session_source_restore()
        was_pending <- nzchar(trimws(as.character(pending[[panel_key]] %||% "")))
        pending[[panel_key]] <- ""
        pending_session_source_restore(pending)
        invisible(was_pending)
    }

    new_ortho_first_paint_gate <- function(run_id = "", enabled = TRUE, previous_token = 0L) {
        run_key <- trimws(as.character(run_id %||% ""))
        token <- suppressWarnings(as.integer(previous_token %||% 0L))
        if (!is.finite(token) || is.na(token) || token < 0L) token <- 0L
        token <- if (token >= .Machine$integer.max) 1L else as.integer(token + 1L)
        gate_enabled <- isTRUE(enabled) && nzchar(run_key)
        list(
            run_id = run_key,
            token = token,
            enabled = gate_enabled,
            armed = FALSE,
            released = !gate_enabled,
            release_reason = if (gate_enabled) "" else "disabled",
            output_id = ""
        )
    }

    arm_ortho_first_paint_gate <- function(gate, run_id) {
        current <- gate %||% list()
        run_key <- trimws(as.character(run_id %||% ""))
        can_arm <- isTRUE(current$enabled) &&
            !isTRUE(current$released) &&
            !isTRUE(current$armed) &&
            nzchar(run_key) &&
            identical(run_key, trimws(as.character(current$run_id %||% "")))
        if (!isTRUE(can_arm)) {
            return(list(changed = FALSE, state = current))
        }
        current$armed <- TRUE
        list(changed = TRUE, state = current)
    }

    release_ortho_first_paint_gate <- function(gate, run_id, reason, output_id = "") {
        current <- gate %||% list()
        run_key <- trimws(as.character(run_id %||% ""))
        can_release <- isTRUE(current$enabled) &&
            !isTRUE(current$released) &&
            nzchar(run_key) &&
            identical(run_key, trimws(as.character(current$run_id %||% "")))
        if (!isTRUE(can_release)) {
            return(list(changed = FALSE, state = current))
        }
        current$released <- TRUE
        current$release_reason <- trimws(as.character(reason %||% "released"))
        current$output_id <- trimws(as.character(output_id %||% ""))
        list(changed = TRUE, state = current)
    }

    list(
        parse_positive_int_env = parse_positive_int_env,
        get_autocomplete_epoch = get_autocomplete_epoch,
        bump_autocomplete_epoch = bump_autocomplete_epoch,
        cancel_autocomplete_background = cancel_autocomplete_background,
        compose_autocomplete_still_valid = compose_autocomplete_still_valid,
        get_search_run_mode_state = get_search_run_mode_state,
        begin_search_run = begin_search_run,
        finish_search_run = finish_search_run,
        request_search_run = request_search_run,
        search_request_is_current = search_request_is_current,
        cancel_search_request = cancel_search_request,
        get_search_request_id = get_search_request_id,
        prepare_search_dispatch = prepare_search_dispatch,
        take_search_event = take_search_event,
        owns_search_run = owns_search_run,
        cancel_search_run = cancel_search_run,
        release_search_admission = release_search_admission,
        start_search_child = start_search_child,
        settle_search_child = settle_search_child,
        guard_search_continuation = guard_search_continuation,
        begin_session_source_restore = begin_session_source_restore,
        consume_session_source_restore = consume_session_source_restore,
        finish_session_source_restore = finish_session_source_restore,
        new_ortho_first_paint_gate = new_ortho_first_paint_gate,
        arm_ortho_first_paint_gate = arm_ortho_first_paint_gate,
        release_ortho_first_paint_gate = release_ortho_first_paint_gate
    )
}
