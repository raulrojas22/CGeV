library(testthat)
state_root <- if (file.exists('R/server_state_helpers_domain.R')) '.' else '../..'
state_env <- new.env(parent = globalenv())
sys.source(file.path(state_root, 'R/server_state_helpers_domain.R'), state_env)

test_that('key reads invalidate only for their own content, including absent entries', {
    shiny::testServer(function(input, output, session) {
        state <- state_env$make_keyed_reactive_map(list(a = 'AA', b = 'BB'))
        counter <- new.env(parent=emptyenv())
        counter$runs <- c(a = 0L, b = 0L, missing = 0L, whole = 0L)
        counter$seen <- list()
        for (key in c('a', 'b', 'missing')) local({
            k <- key
            shiny::observe({
                counter$seen[[k]] <- state_env$read_reactive_map_key(state, k)
                counter$runs[[k]] <- counter$runs[[k]] + 1L
            })
        })
        shiny::observe({ state(); counter$runs[['whole']] <- counter$runs[['whole']] + 1L })
    }, {
        session$flushReact()
        expect_identical(counter$runs, c(a=1L,b=1L,missing=1L,whole=1L))
        state(list(a='AA',b='BC')) # Same-sized changed value must still invalidate b.
        session$flushReact()
        expect_identical(counter$runs, c(a=1L,b=2L,missing=1L,whole=2L))
        expect_identical(counter$seen$b, 'BC')
        state(list(b='BC',a='AA',missing='new'))
        expect_identical(shiny::isolate(state_env$read_reactive_map_key(state,'missing')), 'new')
        session$flushReact()
        expect_identical(counter$runs, c(a=1L,b=2L,missing=2L,whole=3L))
        expect_identical(names(shiny::isolate(state())), c('b','a','missing'))
        state(list(b='BC',a='AA',missing='new'))
        session$flushReact()
        expect_identical(counter$runs, c(a=1L,b=2L,missing=2L,whole=3L))
        state(list()) # Reset/removal must clear every tracked entry.
        session$flushReact()
        expect_identical(counter$runs, c(a=2L,b=3L,missing=3L,whole=4L))
        expect_null(shiny::isolate(state_env$read_reactive_map_key(state,'a')))
        state(list(a='restored',b='BB')) # Restore/reuse an ID with an existing subscriber.
        session$flushReact()
        expect_identical(counter$runs, c(a=3L,b=4L,missing=3L,whole=5L))
        expect_identical(counter$seen$a, 'restored')
        expect_identical(shiny::isolate(state()), list(a='restored',b='BB'))
    })
})

test_that('nested content, copy-on-write and panel isolation are preserved', {
    shiny::testServer(function(input, output, session) {
        first <- state_env$make_keyed_reactive_map(list('1'=list(df=data.frame(x=1:3))))
        second <- state_env$make_keyed_reactive_map(list('1'='other panel'))
        counter <- new.env(parent=emptyenv())
        counter$runs <- 0L
        shiny::observe({state_env$read_reactive_map_key(first,'1'); counter$runs <- counter$runs+1L})
    }, {
        session$flushReact()
        value <- shiny::isolate(first())
        value[['1']]$df$x[2] <- 9L
        expect_identical(shiny::isolate(state_env$read_reactive_map_key(first,'1'))$df$x, 1:3)
        first(value)
        session$flushReact()
        expect_identical(counter$runs, 2L)
        expect_identical(shiny::isolate(state_env$read_reactive_map_key(first,'1'))$df$x, c(1L,9L,3L))
        expect_identical(shiny::isolate(state_env$read_reactive_map_key(second,'1')), 'other panel')
        second(list('1'='changed other panel'))
        session$flushReact()
        expect_identical(counter$runs, 2L)
        ordinary <- shiny::reactiveVal(list(a=3))
        expect_identical(shiny::isolate(state_env$read_reactive_map_key(ordinary,'a')),3)
    })
})
