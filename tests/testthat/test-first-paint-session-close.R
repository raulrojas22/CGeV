# Exercise the actual timeout callbacks without waiting for their wall-clock delay.
paint_close_root <- if (file.exists('server.R')) '.' else '../..'
paint_close_body <- parse(file.path(paint_close_root, 'server.R'))[[1L]][[3L]][[3L]]
paint_timeout_callback <- function(helper) {
    definition <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name('<-')) &&
        identical(x[[2L]], as.name(helper)), as.list(paint_close_body))[[1L]]
    callbacks <- list()
    walk <- function(x) {
        if (missing(x) || !is.call(x)) return(invisible(NULL))
        if (identical(x[[1L]], quote(later::later))) callbacks[[length(callbacks) + 1L]] <<- x[[2L]]
        for (child in as.list(x)[-1L]) walk(child)
    }
    walk(definition)
    stopifnot(length(callbacks) == 1L)
    callbacks[[1L]]
}

testthat::test_that('paint timeouts never read reactive state after the session closes', {
    for (helper in c('activate_ortho_first_paint_gate', 'arm_ortho_first_paint_timeout')) {
        for (compiled in c(FALSE, TRUE)) {
            e <- new.env(parent = environment())
            e$session <- list(isClosed = function() TRUE)
            e$isolate <- shiny::isolate
            e$orthoFirstPaintGate <- function() stop('Reactive state has been destroyed')
            callback <- eval(paint_timeout_callback(helper), e)
            if (compiled) callback <- compiler::cmpfun(callback)
            testthat::expect_no_error(callback())
        }
    }
})

testthat::test_that('live-session timeouts still release the current gate only', {
    for (helper in c('activate_ortho_first_paint_gate', 'arm_ortho_first_paint_timeout')) {
        e <- new.env(parent = environment())
        e$session <- list(isClosed = function() FALSE)
        e$isolate <- shiny::isolate
        e$`%||%` <- function(x, y) if (is.null(x)) y else x
        e$run_id <- e$run_key <- 'current'
        e$gate_token <- e$activation_token <- 2L
        gate <- list(run_id = 'current', token = 2L, enabled = TRUE, armed = TRUE, released = FALSE)
        e$orthoFirstPaintGate <- function() gate
        releases <- list()
        e$release_ortho_first_paint_gate <- function(run_id, reason) {
            releases[[length(releases) + 1L]] <<- list(run_id = run_id, reason = reason)
        }
        callback <- eval(paint_timeout_callback(helper), e)
        callback()
        testthat::expect_identical(releases, list(list(run_id = 'current', reason =
            if (helper == 'activate_ortho_first_paint_gate') 'activation_timeout' else 'timeout')))
        gate$token <- 3L
        callback()
        testthat::expect_length(releases, 1L)
        gate$token <- 2L
        gate$released <- TRUE
        callback()
        testthat::expect_length(releases, 1L)
    }
})
