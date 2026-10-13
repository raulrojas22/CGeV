# Actual source-key definitions; temporary inputs only, no scientific workload.
# Rscript tests/manual/benchmark_search_source_identity.R
source('tests/testthat/helper-search-ownership.R')
benchmark_source_identity <- function() {
    reviewed <- system2('git', c('show', '8fd5f34bec06125e38bc6bffe31458e8c942dfd3:server.R'), stdout = TRUE)
    old_tree <- parse(text = reviewed)[[1L]][[3L]][[3L]]
    old_definition <- Filter(function(x) is.call(x) && identical(x[[1L]], quote(`<-`)) &&
        identical(x[[2L]], quote(orthologous_search_source_key)), as.list(old_tree))[[1L]][[3L]]
    new_definition <- ownership_assignment('orthologous_search_source_key')[[3L]]
    cat(R.version.string, 'Shiny', as.character(packageVersion('shiny')), '\n')
    for (n in c(2L, 20L, 100L)) {
        ids <- paste0('GCF_', seq_len(n)); labels <- paste('Organism', seq_len(n))
        prepared <- setNames(rep(list(list(organism = 'fixture', annotation_path = '/fixture/annotation',
            genome_path = '/fixture/genome')), n), labels)
        e <- new.env(parent = globalenv()); e$isolate <- shiny::isolate; e$session_alive <- function() TRUE
        e$orthoPreparedNcbiOrganisms <- shiny::reactiveVal(prepared)
        e$orthoPreparedNcbiSourceIds <- shiny::reactiveVal(ids)
        before <- eval(old_definition, e); after <- eval(new_definition, e)
        for (mode in c('preloaded', 'ncbi', 'upload', 'mixed')) {
            e$input <- list(ortho_data_mode = mode, ortho_preloaded_species = ids,
                files = list(datapath = paste0('/fixture/ann/', seq_len(n))),
                genome_fasta_ortho = list(datapath = paste0('/fixture/genome/', seq_len(n))))
            for (i in 1:50) {before(); after()}
            old <- current <- numeric(5L)
            for (round in 1:5) {
                gc()
                old[round] <- system.time(for (i in 1:1000) before())[['elapsed']] * 1000
                current[round] <- system.time(for (i in 1:1000) after())[['elapsed']] * 1000
            }
            memory <- function(fn) {
                path <- tempfile(); on.exit(unlink(path))
                Rprofmem(path); for (i in 1:1000) fn(); Rprofmem(NULL)
                lines <- readLines(path); bytes <- suppressWarnings(as.numeric(sub(' .*', '', lines)))
                sum(bytes, na.rm = TRUE) / 1000
            }
            cat('n=', n, 'mode=', mode, 'reviewed_us=', median(old), 'corrected_us=', median(current),
                'delta_us=', median(current) - median(old), 'allocated_bytes/call_reviewed=', memory(before),
                'corrected=', memory(after), '\n', sep = '')
        }
        cat('retained prepared IDs bytes n=', n, ': ', as.numeric(object.size(ids)), '\n', sep = '')
    }
    cat('NCBI baseline producer had no operation source-key helper: queue ID composition only.\n')
    queue <- lapply(1:20, function(i) list(accession = paste0('GCF_', i), preloaded_id = ''))
    baseline <- function() paste('ncbi', paste(vapply(queue, function(q)
        as.character(q$accession %||% q$preloaded_id %||% ''), character(1)), collapse = '|'), sep = '::')
    times <- replicate(5L, system.time(for (i in 1:10000) baseline())[['elapsed']] * 100)
    cat('Baseline original queue-key expression, 20 sources:', median(times), 'us/call\n')
    cat('No file I/O, genome metadata construction or scientific-object hashing is timed.\n')
}
benchmark_source_identity()
