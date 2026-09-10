#!/usr/bin/env Rscript
# Build-only: no session, genome, annotation, or computed result is serialized.
args <- commandArgs(TRUE)
out <- if (length(args)) args[[1L]] else '.cgv-compiled'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
global_expressions <- parse('global.R', keep.source = FALSE)
libraries <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name('library')), as.list(global_expressions))
for (expr in libraries) eval(expr, .GlobalEnv)
paths <- unique(unlist(lapply(global_expressions, function(expr) {
    if (is.call(expr) && identical(expr[[1L]], as.name('cgv_source_runtime'))) as.character(expr[[2L]]) else NULL
}), use.names = FALSE))
# Conditional optional sources are explicit so the fallback branches remain
# ordinary source evaluation when a legacy source isn't part of this image.
paths <- unique(c('R/alias_resolution.R', paths, 'R/gene_search_lib.R'))
paths <- paths[file.exists(paths)]
compile_env <- new.env(parent = parent.env(.GlobalEnv))
for (p in paths) sys.source(p, envir = compile_env)
manifest <- list(schema = 1L, r_version = R.version.string, platform = R.version$platform,
    packages = setNames(lapply(libraries, function(expr) as.character(utils::packageVersion(as.character(expr[[2L]])))),
                        vapply(libraries, function(expr) as.character(expr[[2L]]), character(1))), files = list())
for (p in c(paths, 'server.R')) {
    start <- proc.time()[['elapsed']]
    expressions <- parse(p, keep.source = FALSE)
    if (identical(p, 'server.R')) {
        stopifnot(is.call(expressions[[1L]]), identical(expressions[[1L]][[1L]], as.name('<-')),
                  identical(expressions[[1L]][[2L]], as.name('.cgv_server_definition')))
        expressions <- as.expression(list(expressions[[1L]][[3L]]))
    }
    codes <- lapply(expressions, compiler::compile, env = compile_env)
    artifact <- paste0(gsub('/', '_', p, fixed = TRUE), '.rds')
    saveRDS(codes, file.path(out, artifact), compress = FALSE)
    manifest$files[[p]] <- list(source_md5 = unname(tools::md5sum(p)), artifact = artifact,
                              artifact_md5 = unname(tools::md5sum(file.path(out, artifact))), expressions = length(codes))
    cat(p, 'compiled in', proc.time()[['elapsed']] - start, 's\n')
}
manifest$sources <- tools::md5sum(c(paths, 'server.R', 'global.R', 'R/compiled_runtime.R'))
saveRDS(manifest, file.path(out, 'manifest.rds'))
cat('Compiled', length(manifest$files), 'source files; JIT remains enabled at runtime.\n')
