# MockShinySession ignores outputOptions, so exercise the real Shiny method.
collapse_root <- if (file.exists('server.R')) '.' else '../..'
server_ast <- parse(file.path(collapse_root, 'server.R'))[[1L]][[3L]]
collapse_definition <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name('<-')) &&
    identical(x[[2L]], as.name('hide_isoform_cards')), as.list(server_ast[[3L]]))[[1L]]

collapse_fixture <- function(configured) {
    private <- new.env(parent=emptyenv())
    private$.outputs <- setNames(rep(list(NULL),length(configured)),configured)
    private$.outputOptions <- setNames(rep(list(list(suspendWhenHidden=FALSE)),length(configured)),configured)
    method <- shiny:::ShinySession$public_methods$outputOptions
    method_env <- new.env(parent=environment(method))
    method_env$private <- private
    method_env$self <- list(manageHiddenOutputs=function(name) invisible(NULL))
    environment(method) <- method_env
    output <- structure(list(ns=identity,impl=list(outputOptions=method)),class='shinyoutput')
    e <- new.env(parent=environment())
    e$output <- output
    e$outputOptions <- shiny::outputOptions
    e$`%||%` <- function(x,y) if(is.null(x)) y else x
    e$set_isoform_toggle_label <- function(...) invisible(NULL)
    eval(collapse_definition,e)
    list(env=e,private=private,output=output)
}

testthat::test_that('the real session rejects options for an unregistered output', {
    f <- collapse_fixture('ortho_footer_1')
    testthat::expect_error(shiny::outputOptions(f$output,'ortho_footer_2',suspendWhenHidden=TRUE),
                          'not in list of output objects')
})

testthat::test_that('closing a partly loaded group hides cards without configuring absent outputs', {
    for (context in c('homologous','orthologous')) for (compiled in c(FALSE,TRUE)) {
        prefix <- if(context=='orthologous') 'ortho_footer_' else 'homo_footer_'
        f <- collapse_fixture(paste0(prefix,c('1','3','unrelated')))
        scripts <- character()
        testthat::local_mocked_bindings(runjs=function(code) {scripts <<- c(scripts,code);invisible(NULL)},.package='shinyjs')
        hide <- f$env$hide_isoform_cards
        if(compiled) hide <- compiler::cmpfun(hide)
        testthat::expect_no_error(hide(c('1','2','3'),context))
        testthat::expect_true(f$private$.outputOptions[[paste0(prefix,'1')]]$suspendWhenHidden)
        testthat::expect_true(f$private$.outputOptions[[paste0(prefix,'3')]]$suspendWhenHidden)
        testthat::expect_false(f$private$.outputOptions[[paste0(prefix,'unrelated')]]$suspendWhenHidden)
        testthat::expect_length(scripts,1L)
        testthat::expect_match(scripts,'["1","2","3"]',fixed=TRUE)
    }
})

testthat::test_that('closing before any footer is configured still sends the hide operation', {
    f <- collapse_fixture(character())
    scripts <- character()
    testthat::local_mocked_bindings(runjs=function(code) {scripts <<- c(scripts,code);invisible(NULL)},.package='shinyjs')
    testthat::expect_no_error(f$env$hide_isoform_cards('1','orthologous'))
    testthat::expect_length(scripts,1L)
})
