library(testthat)
root <- if (file.exists('R/utils.R')) '.' else '../..'
env <- new.env(parent = globalenv())
sys.source(file.path(root, 'R/utils.R'), env)

test_that('ended A cancels queued work and leaves B functional', {
    a <- shiny::MockShinySession$new(); b <- shiny::MockShinySession$new()
    on.exit({a$close(); b$close()}, add=TRUE)
    da <- env$make_session_deferred(a); db <- env$make_session_deferred(b)
    loop <- later::create_loop(); on.exit(later::destroy_loop(loop), add=TRUE)
    touched <- character()
    da$later(function() stop('destroyed A reactive accessed'), loop=loop)
    db$later(function() touched <<- c(touched, 'B'), loop=loop)
    expect_identical(da$pending_count(), 1L)
    a$close()
    expect_identical(da$pending_count(), 0L)
    later::run_now(0, loop=loop)
    expect_identical(touched, 'B')
    expect_identical(db$pending_count(), 0L)
    da$later(function() stop('scheduled after close'), loop=loop)
    expect_identical(da$pending_count(), 0L)
})

test_that('live callbacks keep their domain, values, and legitimate errors', {
    s <- shiny::MockShinySession$new(); on.exit(s$close(), add=TRUE)
    d <- env$make_session_deferred(s)
    expect_identical(d$guard(function() shiny::getDefaultReactiveDomain())(), s)
    expect_identical(d$guard(function(x) x + 1L)(2L), 3L)
    expect_error(d$guard(function() stop('real failure'))(), 'real failure')
    loop <- later::create_loop(); on.exit(later::destroy_loop(loop), add=TRUE)
    d$later(function() stop('timer failure'), loop=loop)
    expect_error(later::run_now(0, loop=loop), 'timer failure')
    expect_identical(d$pending_count(), 0L)
})

test_that('promise fulfillment and rejection never enter ended session callbacks', {
    for (reject in c(FALSE, TRUE)) {
        s <- shiny::MockShinySession$new()
        d <- env$make_session_deferred(s)
        settle <- NULL; calls <- 0L
        p <- promises::promise(function(resolve, reject_fn) {
            settle <<- if (reject) reject_fn else resolve
        })
        p <- promises::then(p,
            onFulfilled=d$guard(function(x) {calls <<- calls+1L; x}),
            onRejected=d$guard(function(e) {calls <<- calls+1L; stop(e)}))
        s$close()
        settle(if(reject) simpleError('worker failed') else 42L)
        for(i in 1:10) later::run_now(0.01)
        expect_identical(calls, 0L)
    }
})

test_that('recursive scheduling and explicit cancellation release pending entries', {
    s <- shiny::MockShinySession$new(); on.exit(s$close(), add=TRUE)
    d <- env$make_session_deferred(s)
    loop <- later::create_loop(); on.exit(later::destroy_loop(loop), add=TRUE)
    n <- 0L
    tick <- function() {n <<- n+1L; d$later(tick, delay=0.01, loop=loop)}
    d$later(tick, loop=loop); later::run_now(0, loop=loop)
    expect_identical(n, 1L); expect_identical(d$pending_count(), 1L)
    s$close(); later::run_now(0.02, loop=loop)
    expect_identical(n, 1L); expect_identical(d$pending_count(), 0L)
    s2 <- shiny::MockShinySession$new(); on.exit(s2$close(), add=TRUE)
    d2 <- env$make_session_deferred(s2)
    cancel <- d2$later(function() stop('cancelled'), loop=loop)
    expect_true(cancel()); expect_identical(d2$pending_count(), 0L)
    expect_false(cancel())
})

test_that('guarded promise pipe syntax forwards live results and errors', {
    `%...>%` <- promises::`%...>%`; `%...!%` <- promises::`%...!%`
    s <- shiny::MockShinySession$new(); on.exit(s$close(),add=TRUE)
    session_guard <- env$make_session_deferred(s)$guard
    value <- NULL; error <- NULL
    promises::promise_resolve(41L) %...>% (session_guard(function(x) {
        value <<- x+1L
        stop('live async failure')
    })) %...!% (session_guard(function(e) {error <<- conditionMessage(e)}))
    for(i in 1:10) later::run_now(0.01)
    expect_identical(value,42L)
    expect_identical(error,'live async failure')
})

test_that('cancelled long-delay callbacks release their captured payloads', {
    released <- new.env(parent=emptyenv()); released$n <- 0L
    loop <- later::create_loop(); on.exit(later::destroy_loop(loop),add=TRUE)
    for(i in 1:20) {
        s <- shiny::MockShinySession$new()
        d <- env$make_session_deferred(s)
        callback <- local({
            payload <- new.env(parent=emptyenv())
            payload$data <- raw(1024*1024)
            reg.finalizer(payload,function(e) released$n <- released$n+1L)
            function() payload$data
        })
        d$later(callback,delay=3600,loop=loop)
        rm(callback)
        s$close()
        expect_identical(d$pending_count(),0L)
    }
    gc(); gc()
    expect_identical(released$n,20L)
})
