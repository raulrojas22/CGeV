# Set CGV_ALIAS_TEST_SOURCE to run these result oracles against an unmodified
# historical alias_resolution.R in a separate process.
alias_test_root <- normalizePath(if (file.exists('R/alias_resolution.R')) '.' else '../..')
alias_test_source <- Sys.getenv('CGV_ALIAS_TEST_SOURCE', file.path(alias_test_root, 'R/alias_resolution.R'))
alias_test_env <- function() {
    e <- new.env(parent = globalenv())
    sys.source(file.path(alias_test_root, 'R/utils.R'), e)
    sys.source(alias_test_source, e)
    e
}
alias_fixture <- function(e, gene) e$normalize_alias_index_df(data.frame(
    organism_id = 'fixture', query_term_original = 'ALIAS_X',
    query_term_upper = 'ALIAS_X', query_term_clean_basic = 'ALIAS_X',
    query_term_clean_strict = 'ALIASX', term_type = 'alias',
    local_gene_id = gene, local_feature_id = gene, confidence = 'HIGH',
    source_db = 'NCBI', stringsAsFactors = FALSE
))
alias_fixture_root <- function() {
    root <- tempfile('alias-identity-')
    dir.create(file.path(root, 'data', 'alias_index'), recursive = TRUE)
    root
}
alias_result <- function(e, root) {
    index <- e$load_alias_index('fixture', base_dir = root, allow_gff_fallback = FALSE)
    e$search_alias_index('ALIAS_X', index, organism_id = 'fixture')$matches$local_gene_id
}
alias_write_db <- function(e, root, gene, path = e$alias_sqlite_path('fixture', root)) {
    e$write_alias_sqlite_compact(alias_fixture(e, gene), path)
    invisible(path)
}

testthat::test_that('SQLite unchanged lookup reuses the handle and scientific result', {
    testthat::skip_if_not_installed('RSQLite')
    e <- alias_test_env(); root <- alias_fixture_root()
    on.exit({e$close_all_alias_sqlite_connections(); unlink(root, recursive = TRUE)})
    alias_write_db(e, root, 'GENE_A')
    old <- e$load_alias_index_sqlite('fixture', root)
    testthat::expect_identical(alias_result(e, root), 'GENE_A')
    for (i in 1:5) testthat::expect_identical(e$load_alias_index_sqlite('fixture', root), old)
    testthat::expect_identical(alias_result(e, root), 'GENE_A')
})

for (replacement in c('atomic', 'truncate', 'same-size', 'preserved-mtime')) {
    local({
        mode <- replacement
        testthat::test_that(paste('SQLite result follows', mode, 'replacement'), {
            testthat::skip_if_not_installed('RSQLite')
            e <- alias_test_env(); root <- alias_fixture_root()
            on.exit({e$close_all_alias_sqlite_connections(); unlink(root, recursive = TRUE)})
            path <- alias_write_db(e, root, 'GENE_A')
            old <- e$load_alias_index_sqlite('fixture', root)
            testthat::expect_identical(alias_result(e, root), 'GENE_A')
            before <- file.info(path)
            # Cross coarse filesystem timestamp boundaries for portable ctime tests.
            Sys.sleep(1.1)
            next_path <- paste0(path, '.next')
            alias_write_db(e, root, 'GENE_B', next_path)
            testthat::expect_equal(file.info(next_path)$size, before$size)
            if (mode == 'preserved-mtime') Sys.setFileTime(next_path, before$mtime)
            if (mode == 'truncate') {
                bytes <- readBin(next_path, 'raw', n = file.info(next_path)$size)
                # Opening wb truncates the existing inode; it does not unlink it.
                out <- file(path, 'wb'); writeBin(bytes, out); close(out)
            } else {
                testthat::skip_on_os('windows') # Windows may prohibit renaming open databases.
                testthat::expect_true(file.rename(next_path, path))
            }
            if (mode == 'preserved-mtime') {
                after <- file.info(path)
                testthat::expect_equal(as.numeric(after$mtime), as.numeric(before$mtime), tolerance = 0)
                if (is.na(after$ctime) || identical(after$ctime, before$ctime)) {
                    testthat::skip('filesystem ctime cannot distinguish this replacement')
                }
            }
            testthat::expect_identical(alias_result(e, root), 'GENE_B')
            testthat::expect_false(DBI::dbIsValid(old))
            testthat::expect_false(identical(old, e$load_alias_index_sqlite('fixture', root)))
        })
    })
}

testthat::test_that('SQLite deletion purges old inode and recreation returns new result', {
    testthat::skip_if_not_installed('RSQLite'); testthat::skip_on_os('windows')
    e <- alias_test_env(); root <- alias_fixture_root()
    on.exit({e$close_all_alias_sqlite_connections(); unlink(root, recursive = TRUE)})
    path <- alias_write_db(e, root, 'GENE_A')
    old <- e$load_alias_index_sqlite('fixture', root)
    testthat::expect_identical(alias_result(e, root), 'GENE_A')
    unlink(path)
    testthat::expect_null(e$load_alias_index_sqlite('fixture', root))
    testthat::expect_length(alias_result(e, root), 0L)
    testthat::expect_false(DBI::dbIsValid(old))
    testthat::expect_length(ls(e$.alias_sqlite_connection_identity), 0L)
    alias_write_db(e, root, 'GENE_B')
    testthat::expect_identical(alias_result(e, root), 'GENE_B')
})

testthat::test_that('identity state follows LRU, purge and close-all lifecycle', {
    testthat::skip_if_not_installed('RSQLite')
    withr::local_envvar(APP_ALIAS_SQLITE_MAX_CONNECTIONS = '2')
    e <- alias_test_env(); root <- alias_fixture_root()
    on.exit({e$close_all_alias_sqlite_connections(); unlink(root, recursive = TRUE)})
    for (org in c('a', 'b', 'c')) alias_write_db(e, root, 'GENE_A', e$alias_sqlite_path(org, root))
    a <- e$load_alias_index_sqlite('a', root); b <- e$load_alias_index_sqlite('b', root)
    testthat::expect_identical(e$load_alias_index_sqlite('a', root), a)
    c <- e$load_alias_index_sqlite('c', root)
    testthat::expect_false(DBI::dbIsValid(b))
    testthat::expect_true(DBI::dbIsValid(a))
    testthat::expect_identical(ls(e$.alias_sqlite_connection_identity), ls(e$.alias_sqlite_connection_cache))
    testthat::expect_identical(e$search_alias_index('ALIAS_X', a)$matches$local_gene_id, 'GENE_A')
    e$purge_alias_sqlite_connection('a', root)
    testthat::expect_false(DBI::dbIsValid(a))
    testthat::expect_identical(ls(e$.alias_sqlite_connection_identity), ls(e$.alias_sqlite_connection_cache))
    e$close_all_alias_sqlite_connections()
    testthat::expect_false(DBI::dbIsValid(c))
    testthat::expect_length(ls(e$.alias_sqlite_connection_identity), 0L)
})

testthat::test_that('legacy TSV cache hit avoids reparsing and rewrite changes result', {
    e <- alias_test_env(); root <- alias_fixture_root()
    on.exit(unlink(root, recursive = TRUE))
    path <- e$write_alias_index_tsv(alias_fixture(e, 'GENE_A'), 'fixture', root)
    old <- e$load_alias_index('fixture', base_dir = root)
    testthat::expect_identical(alias_result(e, root), 'GENE_A')
    # A cached hit must not invoke the parser/normalizer again.
    normalize <- e$normalize_alias_index_df
    e$normalize_alias_index_df <- function(...) stop('unexpected reparse')
    testthat::expect_equal(e$load_alias_index('fixture', base_dir = root)$local_gene_id, old$local_gene_id)
    e$normalize_alias_index_df <- normalize
    before <- file.info(path)$mtime
    e$write_alias_index_tsv(alias_fixture(e, 'GENE_B'), 'fixture', root)
    Sys.setFileTime(path, before + 2)
    testthat::expect_identical(alias_result(e, root), 'GENE_B')
    testthat::expect_length(ls(e$.alias_index_memory_cache), 1L)
    unlink(path)
    testthat::expect_length(alias_result(e, root), 0L)
    e$write_alias_index_tsv(alias_fixture(e, 'GENE_C'), 'fixture', root)
    testthat::expect_identical(alias_result(e, root), 'GENE_C')
})

testthat::test_that('legacy same-size preserved-mtime TSV uses ctime', {
    e <- alias_test_env(); root <- alias_fixture_root()
    on.exit(unlink(root, recursive = TRUE))
    # Plain TSV is readable at the legacy path and guarantees equal fixture sizes.
    path <- e$alias_index_path('fixture', root)
    write <- function(gene) utils::write.table(alias_fixture(e, gene), path,
        sep = '\t', quote = FALSE, row.names = FALSE)
    write('GENE_A'); before <- file.info(path)
    testthat::expect_identical(alias_result(e, root), 'GENE_A')
    Sys.sleep(1.1); write('GENE_B'); Sys.setFileTime(path, before$mtime)
    after <- file.info(path)
    testthat::expect_equal(after$size, before$size)
    testthat::expect_equal(as.numeric(after$mtime), as.numeric(before$mtime), tolerance = 0)
    if (is.na(after$ctime) || identical(after$ctime, before$ctime)) testthat::skip('ctime unavailable')
    testthat::expect_identical(alias_result(e, root), 'GENE_B')
})

testthat::test_that('identity keeps fractional timestamps and deterministic absent ctime', {
    e <- alias_test_env()
    calls <- 0L
    e$file.info <- function(..., extra_cols) {
        calls <<- calls + 1L
        testthat::expect_false(extra_cols)
        data.frame(size = 42, isdir = FALSE,
        mtime = as.POSIXct(1000.123456, origin = '1970-01-01'),
        ctime = as.POSIXct(1000.654321, origin = '1970-01-01'))
    }
    a <- e$alias_file_identity('fixture')
    testthat::expect_identical(calls, 1L)
    testthat::expect_identical(a$mtime, 1000.123456)
    testthat::expect_identical(a$ctime, 1000.654321)
    e$file.info <- function(...) data.frame(size = 42, isdir = FALSE, mtime = 1000.123456)
    testthat::expect_identical(e$alias_file_identity('fixture')$ctime, NA_real_)
    testthat::expect_identical(e$alias_file_identity('fixture'), e$alias_file_identity('fixture'))
})

testthat::test_that('persistent multisession worker independently refreshes alias result', {
    testthat::skip_if_not_installed('future'); testthat::skip_if_not_installed('RSQLite')
    testthat::skip_on_os('windows')
    e <- alias_test_env(); root <- alias_fixture_root()
    old_plan <- future::plan()
    on.exit({future::plan(old_plan); e$close_all_alias_sqlite_connections(); unlink(root, recursive = TRUE)})
    future::plan(future::multisession, workers = I(1))
    path <- alias_write_db(e, root, 'GENE_A')
    run_worker <- function() future::value(future::future({
        if (!exists('.alias_invalidation_test_runtime', .GlobalEnv, inherits = FALSE)) {
            runtime <- new.env(parent = globalenv())
            sys.source(utils_path, runtime); sys.source(alias_path, runtime)
            assign('.alias_invalidation_test_runtime', runtime, .GlobalEnv)
        }
        runtime <- get('.alias_invalidation_test_runtime', .GlobalEnv)
        index <- runtime$load_alias_index('fixture', base_dir = fixture_root, allow_gff_fallback = FALSE)
        list(pid = Sys.getpid(), gene = runtime$search_alias_index('ALIAS_X', index)$matches$local_gene_id)
    }, globals = list(utils_path = file.path(alias_test_root, 'R/utils.R'),
                      alias_path = alias_test_source, fixture_root = root)))
    a <- run_worker()
    testthat::expect_false(identical(a$pid, Sys.getpid()))
    testthat::expect_identical(a$gene, 'GENE_A')
    testthat::expect_identical(alias_result(e, root), 'GENE_A')
    next_path <- paste0(path, '.next'); alias_write_db(e, root, 'GENE_B', next_path)
    testthat::expect_true(file.rename(next_path, path))
    testthat::expect_identical(alias_result(e, root), 'GENE_B')
    b <- run_worker()
    testthat::expect_identical(b$pid, a$pid)
    testthat::expect_identical(b$gene, 'GENE_B')
})
