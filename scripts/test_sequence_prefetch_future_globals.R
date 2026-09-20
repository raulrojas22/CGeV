#!/usr/bin/env Rscript
# Exercise the actual module bodies, including strand, exon and neighbor paths.
e <- new.env(parent = baseenv())
sys.source('R/utils.R', e)
sys.source('R/modules.R', e)
find_assignment <- function(x, name) {
    if (!is.call(x) && !is.expression(x)) return(NULL)
    if (is.call(x) && identical(x[[1L]], as.name('<-')) &&
        identical(x[[2L]], as.name(name))) return(x[[3L]])
    for (i in seq_along(x)) {
        if (identical(x[[i]], quote(expr = ))) next
        hit <- find_assignment(x[[i]], name)
        if (!is.null(hit)) return(hit)
    }
    NULL
}
modules <- parse('R/modules.R')
defs <- list(homo = find_assignment(modules, 'run_sequence_prefetch'),
             ortho = find_assignment(modules, 'run_prefetch_payload'))
stopifnot(all(vapply(defs, is.call, logical(1))),
          identical(environment(e$sequence_prefetch_future_worker), baseenv()))
strip_timing <- function(x) x[!grepl('_ms$', names(x))]
local({
    tmp <- tempfile('sequence-future-'); dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    fasta <- file.path(tmp, 'tiny.fa')
    writeLines(c('>chr1', 'AACCGGTTACGTACGT'), fasta)
    twobit <- file.path(tmp, 'tiny.2bit')
    rtracklayer::export(Biostrings::DNAStringSet(c(chr1 = 'AACCGGTTACGTACGT')), twobit)
    annotation <- file.path(tmp, 'tiny.gff3')
    writeLines(c('##gff-version 3',
        'chr1\ttest\tgene\t1\t2\t.\t+\t.\tID=left;Name=LEFT',
        'chr1\ttest\tgene\t4\t10\t.\t-\t.\tID=target;Name=TARGET',
        'chr1\ttest\tgene\t12\t16\t.\t+\t.\tID=right;Name=RIGHT'), annotation)
    jobs <- list(); expected <- list()
    for (source_path in c(fasta, twobit)) for (kind in c('homo', 'ortho')) for (strand in c('+', '-')) {
        locals <- list(
            local_genome = source_path, local_gs_seqid = 'chr1', local_gs_start = 1,
            local_gs_end = 16, local_gs_ok = TRUE, local_chr = 'chr1',
            local_tx_coords = list(start = 4, end = 10), local_tx_label = 'target ID',
            local_exon_ranges = data.frame(start = c(4, 8), end = c(6, 10)),
            local_tx_strand = strand, local_need_sequence = TRUE, local_need_gc_span = TRUE,
            local_annotation = annotation, local_need_neighbor = kind == 'ortho',
            local_target_gene = data.frame(gene_id = 'target', chr = 'chr1', start = 4,
                                          end = 10, strand = '-'))
        scope <- list2env(locals, parent = e)
        scope$unused_session_state <- raw(2 * 1024^2)
        scope$fn_extract_seq <- e$extract_sequence_from_fasta
        scope$fn_fetch_gene <- e$fetch_gene_data_sync
        scope$fn_get_neighbor <- e$get_neighbor_context_for_target
        run <- eval(defs[[kind]], scope)
        baseline <- strip_timing(run())
        stopifnot(identical(baseline$gs_seq, 'AACCGGTTACGTACGT'),
                  identical(baseline$seq_result$sequence, if (strand == '+') 'CGGTAC' else 'GTACCG'),
                  identical(baseline$seq_result$fasta_id, 'target ID'))
        if (kind == 'ortho') stopifnot(!is.null(baseline$ctx))
        globals <- e$sequence_prefetch_future_globals(run, kind)
        unused_format_caches <- if (source_path == twobit)
            c('.fasta_fallback_seq_cache', '.fasta_header_cache', '.fasta_seqnames_cache',
              '.fasta_resolved_seqname_cache') else c('.twobit_seqinfo_cache', '.twobit_native_index_cache')
        stopifnot(!any(unused_format_caches %in% names(globals$prefetch_state)))
        stopifnot(!any(c('.gff_cache', '.orthologous_local_lookup_cache',
                        '.fafile_handle_cache', '.twobit_handle_cache') %in% names(globals$prefetch_state)),
                  !is.function(globals$prefetch_code), is.language(globals$prefetch_code),
                  length(serialize(globals, NULL)) < 200000L)
        clone <- unserialize(serialize(globals, NULL))
        actual <- do.call(clone$sequence_prefetch_future_worker, clone[-1L], quote = TRUE)
        stopifnot(identical(strip_timing(actual), baseline))
        jobs[[length(jobs) + 1L]] <- globals
        expected[[length(expected) + 1L]] <- baseline
        # Cold worker must exercise the extraction code, not only warm memo hits.
        cold <- globals
        for (name in names(cold$prefetch_state)) {
            if (is.environment(cold$prefetch_state[[name]]))
                cold$prefetch_state[[name]] <- new.env(parent = emptyenv())
        }
        jobs[[length(jobs) + 1L]] <- cold
        expected[[length(expected) + 1L]] <- baseline
        cat(basename(source_path), kind, strand, 'globals:', length(globals), 'serialized bytes:',
            length(serialize(globals, NULL)), '\n')
        # Disabled/missing source follows the original empty-result behavior.
        scope$local_genome <- file.path(tmp, 'absent.fa')
        scope$local_gs_ok <- FALSE
        scope$local_need_sequence <- FALSE
        scope$local_need_neighbor <- FALSE
        disabled <- e$sequence_prefetch_future_globals(run, kind)
        stopifnot(identical(names(disabled$prefetch_state),
                            c('annotation_memory_cache_limits', '.cache_access_counter')),
                  identical(strip_timing(do.call(disabled$sequence_prefetch_future_worker, disabled[-1L], quote = TRUE)),
                            strip_timing(run())))
    }
    # Explicit globals in a genuine multisession child, not sequential emulation.
    future::plan(future::multisession, workers = I(1L))
    on.exit(future::plan(future::sequential), add = TRUE)
    for (i in seq_along(jobs)) {
        f <- future::future(sequence_prefetch_future_worker(prefetch_args, prefetch_code, prefetch_state),
                            globals = jobs[[i]], packages = character(), seed = FALSE)
        stopifnot(identical(strip_timing(future::value(f)), expected[[i]]))
    }
})
cat('sequence-prefetch-future-globals-ok\n')
