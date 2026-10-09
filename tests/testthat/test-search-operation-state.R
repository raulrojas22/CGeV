library(testthat)
root <- Sys.getenv('CGV_SEARCH_TEST_ROOT', if (file.exists('R/server_state_helpers_domain.R')) '.' else '../..')
e <- new.env(parent = globalenv())
e$`%||%` <- function(a, b) if (is.null(a)) b else a
sys.source(file.path(root, 'R/server_state_helpers_domain.R'), e)
operation_fixture <- function() {
    alive <- TRUE
    reads <- 0L
    value <- list()
    rv <- function(x) {
        if (!alive) stop('destroyed reactive accessed')
        reads <<- reads + 1L
        if (missing(x)) value else {value <<- x; invisible(NULL)}
    }
    h <- e$init_server_state_helpers_domain(function() list(), rv, function() alive)
    list(h = h, state = function(mode = 'homologous') value[[mode]],
         close = function() alive <<- FALSE, reads = function() reads)
}

test_that('accepted searches have unique session-local IDs across both modes', {
    f <- operation_fixture(); h <- f$h
    a <- h$begin_search_run('homologous', 'a')
    b <- h$begin_search_run('orthologous', 'a')
    expect_true(a$ok); expect_true(b$ok); expect_false(identical(a$id, b$id))
    expect_identical(f$state()$status, 'running')
    expect_false(h$finish_search_run('homologous', b$id))
    expect_true(f$state()$active)
})

test_that('cancel A, start B, late finish/cancel/failure A leaves B untouched', {
    f <- operation_fixture(); h <- f$h
    a <- h$begin_search_run('homologous', 'a')
    expect_true(h$cancel_search_run('homologous', a$id))
    b <- h$begin_search_run('homologous', 'b')
    before <- f$state()
    expect_false(h$finish_search_run('homologous', a$id))
    expect_false(h$cancel_search_run('homologous', a$id))
    expect_false(h$finish_search_run('homologous', a$id, 'failed'))
    expect_identical(f$state(), before)
    expect_true(h$owns_search_run('homologous', b$id))
})

test_that('cancellation is idempotent and admits the same gene', {
    f <- operation_fixture(); h <- f$h
    a <- h$begin_search_run('homologous', 'gene::A')
    expect_true(h$cancel_search_run('homologous', a$id))
    expect_false(h$cancel_search_run('homologous', a$id))
    expect_identical(f$state()$status, 'cancelled')
    expect_identical(f$state()$last_completed, 0)
    expect_true(h$begin_search_run('homologous', 'gene::A')$ok)
})

test_that('busy and duplicate rejection cannot replace accepted ownership', {
    f <- operation_fixture(); h <- f$h
    a <- h$begin_search_run('homologous', 'gene::A')
    before <- f$state()
    expect_identical(h$request_search_run('homologous')$reason, 'active')
    expect_identical(h$begin_search_run('homologous', 'gene::B')$reason, 'active')
    expect_identical(f$state(), before)
    expect_true(h$finish_search_run('homologous', a$id))
    expect_identical(h$begin_search_run('homologous', 'GENE::a', 3600)$reason, 'duplicate')
    expect_false(h$cancel_search_run('homologous', a$id))
    expect_identical(f$state()$status, 'completed')
    expect_true(h$begin_search_run('homologous', 'gene::B')$ok)
})

test_that('cancelled retry clears only its inherited successful suppression', {
    f <- operation_fixture(); h <- f$h
    a <- h$begin_search_run('homologous', 'same', 0)
    h$finish_search_run('homologous', a$id)
    b <- h$begin_search_run('homologous', 'same', 0)
    h$cancel_search_run('homologous', b$id)
    expect_true(h$begin_search_run('homologous', 'same', 3600)$ok)
})

test_that('failed operations do not enter successful duplicate suppression', {
    f <- operation_fixture(); h <- f$h
    a <- h$begin_search_run('homologous', 'a')
    expect_true(h$finish_search_run('homologous', a$id, 'failed'))
    expect_identical(f$state()$status, 'failed')
    expect_true(h$begin_search_run('homologous', 'a')$ok)
})

test_that('pending preparation/browser requests can be cancelled or superseded', {
    f <- operation_fixture(); h <- f$h
    a <- h$request_search_run('homologous')
    expect_true(h$search_request_is_current('homologous', a$id))
    expect_true(h$cancel_search_run('homologous', a$id))
    expect_false(h$cancel_search_run('homologous', a$id))
    b <- h$request_search_run('homologous')
    expect_false(h$search_request_is_current('homologous', a$id))
    expect_identical(h$begin_search_run('homologous', 'a', request_id = a$id)$reason, 'stale')
    expect_true(h$begin_search_run('homologous', 'a', request_id = b$id)$ok)
})

test_that('local admission release retains ownership until rescue settles', {
    f <- operation_fixture(); h <- f$h
    a <- h$begin_search_run('orthologous', 'a')
    settle <- h$start_search_child('orthologous', a$id)
    expect_true(h$release_search_admission('orthologous', a$id))
    expect_false(f$state('orthologous')$active)
    expect_identical(f$state('orthologous')$status, 'running')
    expect_false(h$finish_search_run('orthologous', a$id))
    timestamp <- f$state('orthologous')$last_completed
    expect_identical(h$begin_search_run('orthologous', 'a', 3600)$reason, 'duplicate')
    expect_true(settle())
    expect_false(settle())
    expect_identical(f$state('orthologous')$status, 'completed')
    expect_identical(f$state('orthologous')$last_completed, timestamp)
})

test_that('late child success/error cannot mutate a replacement operation', {
    for (outcome in c('completed', 'failed')) {
        f <- operation_fixture(); h <- f$h
        a <- h$begin_search_run('orthologous', 'a')
        settle <- h$start_search_child('orthologous', a$id)
        h$release_search_admission('orthologous', a$id)
        b <- h$begin_search_run('orthologous', 'b')
        before <- f$state('orthologous')
        expect_false(settle(outcome))
        expect_identical(f$state('orthologous'), before)
        expect_true(h$owns_search_run('orthologous', b$id))
    }
})

test_that('completion/cancellation races have one terminal winner', {
    for (first in c('completed', 'cancelled')) {
        f <- operation_fixture(); h <- f$h
        a <- h$begin_search_run('homologous', 'a')
        expect_true(h$finish_search_run('homologous', a$id, first))
        expect_false(h$finish_search_run('homologous', a$id, if (first == 'completed') 'cancelled' else 'completed'))
        expect_identical(f$state()$status, first)
    }
})

test_that('two sessions remain isolated even when their local counters match', {
    a <- operation_fixture(); b <- operation_fixture()
    x <- a$h$begin_search_run('homologous', 'same')
    y <- b$h$begin_search_run('homologous', 'same')
    expect_identical(x$id, y$id)
    a$h$cancel_search_run('homologous', x$id)
    expect_true(b$h$owns_search_run('homologous', y$id))
})

test_that('late callbacks run owned resource cleanup without destroyed reactive access', {
    f <- operation_fixture(); h <- f$h
    a <- h$begin_search_run('homologous', 'a'); cleaned <- 0L; published <- 0L
    callback <- h$guard_search_continuation('homologous', a$id,
        function() published <<- published + 1L,
        function() cleaned <<- cleaned + 1L)
    h$cancel_search_run('homologous', a$id); callback()
    expect_identical(published, 0L); expect_identical(cleaned, 1L)
    f$close(); before <- f$reads()
    expect_false(h$cancel_search_run('homologous', a$id))
    expect_false(h$finish_search_run('homologous', a$id))
    expect_false(h$request_search_run('homologous')$ok)
    callback()
    expect_identical(f$reads(), before); expect_identical(cleaned, 2L)
})
