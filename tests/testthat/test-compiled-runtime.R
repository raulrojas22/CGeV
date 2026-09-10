compiled_root <- if (file.exists('R/compiled_runtime.R')) '.' else '../..'
source(file.path(compiled_root, 'R', 'compiled_runtime.R'), local = TRUE)

compiled_fixture <- function(text = 'state <- new.env(); state$n <- 0L; next_value <- function(x) {state$n <- state$n + x; state$n}') {
    root <- tempfile('compiled-fixture-'); dir.create(root); dir.create(file.path(root,'R'))
    dir.create(file.path(root,'.cgv-compiled'))
    writeLines(text,file.path(root,'R','example.R'))
    codes <- lapply(parse(text=text), compiler::compile)
    artifact <- file.path(root,'.cgv-compiled','example.rds')
    saveRDS(codes,artifact)
    old <- setwd(root); on.exit(setwd(old))
    manifest <- list(schema=1L,r_version=R.version.string,platform=R.version$platform,packages=list(),
        sources=tools::md5sum('R/example.R'),files=list())
    manifest$files[['R/example.R']] <- list(source_md5=unname(tools::md5sum('R/example.R')),
        artifact='example.rds',artifact_md5=unname(tools::md5sum(artifact)),expressions=length(codes))
    saveRDS(manifest,'.cgv-compiled/manifest.rds')
    root
}

testthat::test_that('compiled expressions create independent runtime environments', {
    root <- compiled_fixture(); withr::local_dir(root)
    a <- new.env(); b <- new.env()
    testthat::expect_true(length(cgv_compiled_expressions('R/example.R'))>0)
    cgv_source_runtime('R/example.R',a);cgv_source_runtime('R/example.R',b)
    testthat::expect_identical(a$next_value(2L),2L)
    testthat::expect_identical(a$next_value(3L),5L)
    testthat::expect_identical(b$next_value(4L),4L)
    testthat::expect_false(identical(a$state,b$state))
    testthat::expect_identical(environment(a$next_value),a)
})

testthat::test_that('missing, stale and disabled bytecode use source', {
    root <- compiled_fixture(); withr::local_dir(root)
    a <- new.env()
    withr::with_envvar(c(APP_COMPILED_RUNTIME='0'), {
        testthat::expect_null(cgv_compiled_expressions('R/example.R'))
        cgv_source_runtime('R/example.R',a)
    })
    testthat::expect_identical(a$next_value(7L),7L)
    writeLines('next_value <- function(x) x + 10L','R/example.R')
    testthat::expect_null(cgv_compiled_expressions('R/example.R'))
    cgv_source_runtime('R/example.R',a)
    testthat::expect_identical(a$next_value(2L),12L)
    unlink('.cgv-compiled',recursive=TRUE)
    testthat::expect_null(cgv_compiled_expressions('R/example.R'))
})

testthat::test_that('incompatible or damaged artifacts fall back before evaluation', {
    for (failure in c('version','package','artifact')) {
        root <- compiled_fixture(); old <- setwd(root)
        tryCatch({
            m<-readRDS('.cgv-compiled/manifest.rds')
            if(failure=='version')m$r_version<-'incompatible'
            if(failure=='package')m$packages<-list(base='0.0.0')
            if(failure=='artifact')saveRDS(list(),'.cgv-compiled/example.rds')
            saveRDS(m,'.cgv-compiled/manifest.rds')
            testthat::expect_null(cgv_compiled_expressions('R/example.R'))
            e<-new.env();cgv_source_runtime('R/example.R',e)
            testthat::expect_identical(e$next_value(3L),3L)
        },finally=setwd(old))
    }
})

testthat::test_that('runtime evaluation errors never trigger a second evaluation', {
    root<-compiled_fixture('count$n <- count$n + 1L; stop("deliberate failure")');withr::local_dir(root)
    e<-new.env();e$count<-new.env();e$count$n<-0L
    testthat::expect_error(cgv_source_runtime('R/example.R',e),'deliberate failure')
    testthat::expect_identical(e$count$n,1L)
})

testthat::test_that('the server loader returns the compiled function in the live environment', {
    root<-compiled_fixture('function(input, output, session) offset + input');withr::local_dir(root)
    file.rename('R/example.R','server.R')
    m<-readRDS('.cgv-compiled/manifest.rds');m$sources<-tools::md5sum('server.R')
    m$files[['server.R']]<-m$files[['R/example.R']];m$files[['server.R']]$source_md5<-unname(tools::md5sum('server.R'))
    saveRDS(m,'.cgv-compiled/manifest.rds')
    e<-new.env();e$offset<-8L
    f<-eval(quote(function(input,output,session) -1L),e)
    compiled<-cgv_runtime_server(f)
    testthat::expect_true(is.function(compiled))
    testthat::expect_identical(environment(compiled),e)
    testthat::expect_identical(compiled(2L,NULL,NULL),10L)
    testthat::expect_identical(formals(compiled),formals(f))
})
