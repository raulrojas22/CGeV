# Disk L2 for deterministic gene products only. Bump algorithm when any of the
# scientific/formatting helpers used by split, canonical or metrics changes.
.shared_gene_cache_schema <- 1L
.shared_gene_cache_algorithm <- "split-canonical-metrics-1"

shared_gene_plain <- function(x) {
    if (!(is.null(x) || is.atomic(x) || is.list(x)) || isS4(x)) return(FALSE)
    if (is.object(x) && !identical(class(x), "data.frame")) return(FALSE)
    if (is.list(x) && !all(vapply(x, shared_gene_plain, logical(1)))) return(FALSE)
    a <- attributes(x)
    is.null(a) || all(vapply(a, shared_gene_plain, logical(1)))
}

shared_gene_file_identity <- function(path) {
    path <- as.character(path %||% "")
    if (length(path) != 1L || is.na(path) || !nzchar(path)) return(list(path = ""))
    p <- normalizePath(path, winslash = "/", mustWork = FALSE)
    info <- file.info(p)
    list(path = p, size = as.numeric(info$size), mtime = as.numeric(info$mtime))
}

shared_gene_cache_key <- function(kind, inputs, annotation_path = "", parameters = list(),
                                  schema = .shared_gene_cache_schema,
                                  algorithm = .shared_gene_cache_algorithm) {
    tryCatch({
        if (!shared_gene_plain(inputs) || !shared_gene_plain(parameters) ||
            !requireNamespace("digest", quietly = TRUE)) return(NULL)
        # Exact gene rows (including IDs, order, coordinates and attributes), or
        # exact ordered blocks for metrics, supply the content fingerprint. A
        # same-size/same-mtime replacement cannot reuse different gene content.
        digest::digest(list(kind = kind, schema = schema, algorithm = algorithm,
            annotation = shared_gene_file_identity(annotation_path),
            inputs = inputs, parameters = parameters,
            runtime = list(R = R.version.string, locale = Sys.getlocale(),
                options = options()[c("digits", "scipen", "OutDec")],
                packages = lapply(c("stringr", "stringi", "htmltools", "dplyr", "purrr"), function(p)
                    as.character(utils::packageVersion(p))))),
            algo = "sha256", serializeVersion = 2)
    }, error = function(e) NULL)
}

shared_gene_cache_path <- function(kind, key, base_dir = ".") {
    file.path(get_cgv_cache_root(base_dir), "shared_gene", kind, paste0(key, ".rds"))
}

shared_gene_cache_read <- function(path, key, schema) {
    tryCatch({
        if (!file.exists(path) || as.numeric(Sys.time()) - as.numeric(file.info(path)$mtime) > 7 * 86400) return(NULL)
        x <- readRDS(path)
        if (!is.list(x) || !identical(x$schema, schema) || !identical(x$key, key) ||
            !shared_gene_plain(x$value) ||
            !identical(x$checksum, digest::digest(x$value, algo = "sha256", serializeVersion = 2))) return(NULL)
        x
    }, error = function(e) NULL)
}

shared_gene_cache_prune <- function(root, max_files = 128L) {
    # Same per-kind bound as STRING; only completed files, never locks/staging.
    files <- list.files(root, pattern = "\\.rds$", full.names = TRUE)
    if (length(files) <= max_files) return(invisible(NULL))
    oldest <- order(as.numeric(file.info(files)$mtime), na.last = TRUE)
    unlink(files[oldest[seq_len(length(files) - max_files)]], force = TRUE)
    invisible(NULL)
}

shared_gene_cache_get <- function(kind, key, compute, base_dir = ".",
                                  schema = .shared_gene_cache_schema,
                                  wait_seconds = 15, should_store = function(value) TRUE, perf_run = NULL, perf_context = "APP") {
    t0 <- app_perf_now()
    mark <- function(state) {
        app_perf_mark(perf_run, paste0("shared_gene_cache kind=", kind, " state=", state), perf_context)
        app_perf_mark_ms(perf_run, paste0("shared_gene_", kind, "_lookup_ms"), app_perf_elapsed_ms(t0), perf_context)
    }
    if (is.null(key)) { mark("bypass"); return(compute()) }
    path <- shared_gene_cache_path(kind, key, base_dir)
    cached <- shared_gene_cache_read(path, key, schema)
    if (!is.null(cached)) { mark("hit"); return(cached$value) }
    root <- dirname(path)
    ready <- tryCatch({
        dir.create(root, recursive = TRUE, showWarnings = FALSE)
        dir.exists(root) && file.access(root, 2L) == 0L
    }, error = function(e) FALSE)
    if (!ready) { mark("unwritable"); return(compute()) }
    lock <- paste0(path, ".lock")
    deadline <- proc.time()[["elapsed"]] + max(0, wait_seconds)
    repeat {
        acquired <- tryCatch(dir.create(lock, showWarnings = FALSE), error = function(e) FALSE)
        if (isTRUE(acquired)) break
        cached <- shared_gene_cache_read(path, key, schema)
        if (!is.null(cached)) { mark("wait_hit"); return(cached$value) }
        if (proc.time()[["elapsed"]] >= deadline) {
            # Never steal a possibly live cross-host lock. Orphan locks cause
            # bounded bypass (no write), never deadlock or unsafe lock deletion.
            mark("lock_timeout"); return(compute())
        }
        Sys.sleep(0.05)
    }
    on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
    cached <- shared_gene_cache_read(path, key, schema)
    if (!is.null(cached)) { mark("recheck_hit"); return(cached$value) }
    mark("miss")
    compute_t0 <- app_perf_now()
    value <- compute() # Scientific errors propagate; cache errors alone fail open.
    app_perf_mark_ms(perf_run, paste0("shared_gene_", kind, "_compute_ms"), app_perf_elapsed_ms(compute_t0), perf_context)
    if (!shared_gene_plain(value) || !isTRUE(should_store(value))) return(value)
    tmp <- tempfile(pattern = ".shared-gene-", tmpdir = root, fileext = ".part")
    on.exit(unlink(tmp, force = TRUE), add = TRUE)
    tryCatch({
        payload <- list(schema = schema, key = key, value = value,
            checksum = digest::digest(value, algo = "sha256", serializeVersion = 2))
        saveRDS(payload, tmp, compress = "gzip", version = 2)
        # Same-directory rename: no copy fallback, no reader sees partial bytes.
        if (file.rename(tmp, path)) shared_gene_cache_prune(root)
    }, error = function(e) NULL)
    value
}

shared_gene_split <- function(data, canonical_fun, annotation_path = "", organism = "",
                              base_dir = ".", perf_run = NULL, perf_context = "APP") {
    key <- shared_gene_cache_key("split", as.data.frame(data), annotation_path, list(organism = organism))
    canonical_ok <- TRUE
    shared_gene_cache_get("split", key, function() {
        t0 <- app_perf_now()
        blocks <- split_gene_data_by_transcript(data)
        if (length(blocks) == 0L) blocks <- list(data)
        app_perf_mark_ms(perf_run, "split_blocks_only_ms", app_perf_elapsed_ms(t0), perf_context)
        t0 <- app_perf_now()
        canonical <- tryCatch(canonical_fun(blocks), error = function(e) { canonical_ok <<- FALSE; 1L })
        app_perf_mark_ms(perf_run, "canonical_select_ms", app_perf_elapsed_ms(t0), perf_context)
        list(blocks = blocks, canonical = canonical)
    }, base_dir = base_dir, should_store = function(value) canonical_ok,
        perf_run = perf_run, perf_context = perf_context)
}

shared_gene_metrics <- function(blocks, representative_name, organism_name, annotation_path,
                                use_report_map, report_path, compute, base_dir = ".",
                                perf_run = NULL, perf_context = "APP") {
    # Include effective auxiliary labels, not just report filenames: the existing
    # chromosome helper has process caches. These labels are currently internal
    # only, but representing their effective values avoids hidden dependencies.
    key <- tryCatch({
        chrs <- unique(vapply(blocks, function(b) as.character(extract_plot_labels(b)$chromosome), character(1)))
        labels <- lapply(chrs, function(chr) tryCatch(get_short_chromosome_name(chr,
            annotation_path, use_report_map, report_path), error = function(e) chr))
        resolved_report <- report_path
        if (isTRUE(use_report_map) && !nzchar(resolved_report))
            resolved_report <- get_assembly_report_path_for_annotation(annotation_path)
        shared_gene_cache_key("metrics", blocks, annotation_path,
            list(representative_name = representative_name, organism_name = organism_name,
                use_report_map = isTRUE(use_report_map), report_path = report_path,
                report_identity = shared_gene_file_identity(resolved_report), chromosome_labels = labels))
    }, error = function(e) NULL)
    shared_gene_cache_get("metrics", key, compute, base_dir = base_dir,
        perf_run = perf_run, perf_context = perf_context)
}
