library(testthat)
compact_root <- if (file.exists('R/utils.R')) '.' else '../..'
compact_env <- new.env(parent = globalenv())
sys.source(file.path(compact_root, 'R/utils.R'), compact_env)

legacy_index <- function() list(
    genes_df = data.frame(attributes = c('ID=a;Name=A', 'ID=b;Name=B', 'ID=c;Name=C')),
    gene_rows = c(10L, 20L, 30L),
    norm_map = list(a = 1L, 'a-b' = 2L, collision = c(1L, 2L), empty = integer(0), onlynormal = 3L),
    comp_map = list(collision = c(2L, 3L), a = 1L, ab = 2L, empty = integer(0))
)

test_that('flat ranges preserve all alias rows, collisions, absences and order', {
    e <- compact_env
    old <- legacy_index()
    new <- e$compact_gff_gene_light_index(old)
    expect_identical(new$genes_df, old$genes_df)
    expect_identical(new$gene_rows, old$gene_rows)
    expect_identical(e$compact_gff_gene_light_index(new), new)
    expect_identical(e$slim_gff_gene_light_index(new), new)
    for (kind in c('norm', 'comp')) {
        tokens <- names(old[[paste0(kind, '_map')]])
        expect_identical(e$gene_lookup_tokens(new, kind), tokens)
        for (query in c(as.list(tokens), list(rev(tokens), c('missing', tokens, tokens, NA_character_), character(0), 'onlynormal'))) {
            expect_identical(e$gene_lookup_hits(new, query, kind), e$gene_lookup_hits(old, query, kind))
        }
    }
    for (mode in c('exact', 'flex')) {
        for (query in list('a', 'A-B', 'collision', 'onlynormal', 'missing', c('a', 'collision'))) {
            expect_identical(e$search_gene_rows_with_index(new, query, mode), e$search_gene_rows_with_index(old, query, mode))
        }
    }
})

test_that('empty and entirely shared maps survive serialization', {
    e <- compact_env
    for (maps in list(list(norm_map = list(), comp_map = list()),
                      list(norm_map = list(a = 1L), comp_map = list(a = 1L)),
                      list(norm_map = list(), comp_map = list(a = 1L)))) {
        old <- c(list(gene_rows = 1L), maps)
        new <- unserialize(serialize(e$compact_gff_gene_light_index(old), NULL))
        for (kind in c('norm', 'comp')) {
            expect_identical(e$gene_lookup_tokens(new, kind), e$gene_lookup_tokens(old, kind))
            expect_identical(e$gene_lookup_hits(new, c('a', 'missing'), kind), e$gene_lookup_hits(old, c('a', 'missing'), kind))
        }
    }
})

test_that('compact disk caches retain the legacy file for rollback and invalidate on revision', {
    e <- compact_env
    root <- tempfile('compact-index-'); dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    old_dir <- Sys.getenv('APP_ANNOTATION_DISK_CACHE_DIR', unset = NA_character_)
    on.exit(if (is.na(old_dir)) Sys.unsetenv('APP_ANNOTATION_DISK_CACHE_DIR') else Sys.setenv(APP_ANNOTATION_DISK_CACHE_DIR = old_dir), add = TRUE)
    Sys.setenv(APP_ANNOTATION_DISK_CACHE_DIR = file.path(root, 'cache'))
    p <- file.path(root, 'fixture.gff'); writeLines('annotation', p)
    old <- legacy_index()
    legacy_path <- e$get_gff_disk_index_path(p, base_dir = root)
    dir.create(dirname(legacy_path)); saveRDS(old, legacy_path)
    expect_identical(e$load_gff_index_from_disk(p, base_dir = root), e$compact_gff_gene_light_index(old))
    expect_true(e$save_gff_index_to_disk(p, old, base_dir = root))
    compact_path <- e$get_gff_disk_index_path(p, 'gene_light_compact_v1', base_dir = root)
    expect_true(file.exists(compact_path))
    expect_identical(readRDS(legacy_path), old)
    expect_identical(e$load_gff_index_from_disk(p, base_dir = root), readRDS(compact_path))
    unlink(legacy_path)
    writeLines('annotation with a new size', p)
    expect_null(e$load_gff_index_from_disk(p, base_dir = root))
})
