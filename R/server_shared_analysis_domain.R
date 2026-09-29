# Portable CGeV analysis bundles and static read-only reports.
#
# This domain deliberately depends only on base R plus jsonlite/digest.  It is
# sourced into app_libraries so the Web and Desktop runtimes use the exact same
# schema and package writer.

cgv_analysis_schema_version <- 1L
cgv_session_schema_version <- 2L

cgv_safe_scalar <- function(x, fallback = "") {
    value <- tryCatch(as.character(x %||% fallback), error = function(e) fallback)
    value <- value[!is.na(value)]
    if (!length(value)) fallback else value[[1L]]
}

cgv_first_nonempty <- function(...) {
    values <- as.character(unlist(list(...), recursive = TRUE, use.names = FALSE))
    values <- trimws(values[!is.na(values)])
    values <- values[nzchar(values)]
    if (length(values)) values[[1L]] else ""
}

cgv_safe_filename <- function(x, fallback = "analysis") {
    value <- trimws(cgv_safe_scalar(x, fallback))
    value <- gsub("[^A-Za-z0-9._-]+", "_", value)
    value <- gsub("^_+|_+$", "", value)
    if (!nzchar(value)) fallback else substr(value, 1L, 120L)
}

cgv_random_secret <- function(bytes = 32L) {
    bytes <- max(16L, min(64L, suppressWarnings(as.integer(bytes %||% 32L))))
    raw_value <- tryCatch({
        con <- file("/dev/urandom", open = "rb", raw = TRUE)
        on.exit(close(con), add = TRUE)
        readBin(con, what = "raw", n = bytes)
    }, error = function(e) raw(0))
    if (length(raw_value) != bytes) {
        raw_value <- as.raw(sample.int(256L, bytes, replace = TRUE) - 1L)
    }
    paste(sprintf("%02x", as.integer(raw_value)), collapse = "")
}

cgv_sha256_text <- function(text) {
    if (requireNamespace("digest", quietly = TRUE)) {
        return(digest::digest(enc2utf8(cgv_safe_scalar(text)), algo = "sha256", serialize = FALSE))
    }
    stop("The digest package is required for SHA-256 support.")
}

cgv_sha256_file <- function(path) {
    if (!file.exists(path)) return("")
    if (requireNamespace("digest", quietly = TRUE)) {
        return(digest::digest(file = path, algo = "sha256", serialize = FALSE))
    }
    stop("The digest package is required for SHA-256 support.")
}

cgv_is_absolute_path <- function(path) {
    value <- cgv_safe_scalar(path)
    nzchar(value) && (
        startsWith(value, "/") ||
        grepl("^[A-Za-z]:[/\\\\]", value) ||
        startsWith(value, "\\\\")
    )
}

cgv_manifest_path_field <- function(name) {
    key <- if (length(name) && !is.na(name[[1L]])) {
        tolower(trimws(as.character(name[[1L]])))
    } else {
        ""
    }
    nzchar(key) && (
        key %in% c("path", "paths", "internal_path", "binary_path", "working_directory") ||
        endsWith(key, "_path") ||
        endsWith(key, "_paths")
    )
}

cgv_redact_absolute_path_fields <- function(value, field_name = "") {
    if (is.list(value)) {
        out <- value
        keys <- names(out)
        if (is.null(keys)) keys <- rep("", length(out))
        for (i in seq_along(out)) {
            out[i] <- list(cgv_redact_absolute_path_fields(out[[i]], keys[[i]]))
        }
        return(out)
    }
    if (!is.atomic(value) || !cgv_manifest_path_field(field_name)) return(value)
    out <- value
    text <- as.character(out)
    redact <- vapply(text, cgv_is_absolute_path, logical(1))
    if (!any(redact)) return(out)
    if (is.character(out)) {
        out[redact] <- ""
        return(out)
    }
    ""
}

cgv_collect_absolute_manifest_paths <- function(value, field_name = "", location = "analysis") {
    found <- character(0)
    walk <- function(item, item_field, item_location) {
        if (is.list(item)) {
            keys <- names(item)
            if (is.null(keys)) keys <- rep("", length(item))
            for (i in seq_along(item)) {
                key <- keys[[i]]
                child_location <- if (nzchar(key)) {
                    paste(item_location, key, sep = ".")
                } else {
                    paste0(item_location, "[", i, "]")
                }
                walk(item[[i]], key, child_location)
            }
            return(invisible(NULL))
        }
        if (!is.atomic(item) || !cgv_manifest_path_field(item_field)) {
            return(invisible(NULL))
        }
        text <- as.character(item)
        if (any(vapply(text, cgv_is_absolute_path, logical(1)))) {
            found <<- c(found, item_location)
        }
        invisible(NULL)
    }
    walk(value, field_name, location)
    found
}

cgv_local_asset_data_uri <- function(path, base_dir = ".", max_bytes = 512L * 1024L) {
    ref <- trimws(cgv_safe_scalar(path))
    if (!nzchar(ref) || grepl("^(?:https?:)?//", ref, ignore.case = TRUE)) return("")
    web_root <- normalizePath(file.path(base_dir, "www"), winslash = "/", mustWork = FALSE)
    candidate <- if (startsWith(ref, "/")) {
        file.path(web_root, sub("^/+", "", ref))
    } else if (cgv_is_absolute_path(ref)) {
        ref
    } else {
        file.path(web_root, ref)
    }
    candidate <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    if (!startsWith(candidate, paste0(sub("/+$", "", web_root), "/")) ||
        !file.exists(candidate) ||
        isTRUE(file.info(candidate)$isdir) ||
        !is.finite(file.info(candidate)$size) ||
        file.info(candidate)$size > max_bytes) {
        return("")
    }
    extension <- tolower(tools::file_ext(candidate))
    mime <- switch(
        extension,
        png = "image/png",
        jpg = "image/jpeg",
        jpeg = "image/jpeg",
        svg = "image/svg+xml",
        webp = "image/webp",
        ico = "image/x-icon",
        gif = "image/gif",
        ""
    )
    if (!nzchar(mime)) return("")
    bytes <- readBin(candidate, what = "raw", n = file.info(candidate)$size)
    paste0("data:", mime, ";base64,", jsonlite::base64_enc(bytes))
}

cgv_organism_icon_data_uri <- function(name, icon, base_dir = ".") {
    encoded <- cgv_local_asset_data_uri(icon, base_dir)
    if (nzchar(encoded)) return(encoded)
    fallback <- file.path("icons", paste0(cgv_safe_scalar(name), ".ico"))
    encoded <- cgv_local_asset_data_uri(fallback, base_dir)
    if (nzchar(encoded)) encoded else cgv_local_asset_data_uri("icons/DNA.ico", base_dir)
}

cgv_cached_sha256_file <- function(path, base_dir = ".") {
    if (!file.exists(path) || isTRUE(file.info(path)$isdir)) return("")
    info <- file.info(path)
    cache_root <- if (exists("get_cgv_cache_root", mode = "function")) {
        get_cgv_cache_root(base_dir)
    } else {
        file.path(base_dir, "cache")
    }
    checksum_root <- file.path(cache_root, "reference_checksums")
    dir.create(checksum_root, recursive = TRUE, showWarnings = FALSE)
    key <- cgv_sha256_text(paste(
        normalizePath(path, winslash = "/", mustWork = FALSE),
        as.numeric(info$size),
        as.numeric(info$mtime),
        sep = "|"
    ))
    cache_path <- file.path(checksum_root, paste0(key, ".sha256"))
    if (file.exists(cache_path)) {
        cached <- trimws(cgv_safe_scalar(readLines(cache_path, warn = FALSE)))
        if (grepl("^[a-f0-9]{64}$", cached)) return(cached)
    }
    checksum <- cgv_sha256_file(path)
    staging <- tempfile(paste0(".", key, "-"), tmpdir = checksum_root)
    writeLines(checksum, staging, useBytes = TRUE)
    if (!file.rename(staging, cache_path)) unlink(staging, force = TRUE)
    checksum
}

cgv_portable_source_ref <- function(path, base_dir = ".", include_checksum = FALSE) {
    value <- trimws(cgv_safe_scalar(path))
    if (!nzchar(value)) {
        return(list(name = "", relative_path = "", available = FALSE, checksum = ""))
    }
    normalized <- normalizePath(value, winslash = "/", mustWork = FALSE)
    root <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
    prefix <- paste0(sub("/+$", "", root), "/")
    relative <- if (startsWith(normalized, prefix)) {
        substring(normalized, nchar(prefix) + 1L)
    } else if (!cgv_is_absolute_path(value)) {
        gsub("^\\./", "", value)
    } else {
        ""
    }
    list(
        name = basename(value),
        relative_path = relative,
        available = isTRUE(file.exists(value)),
        checksum = if (isTRUE(include_checksum) && file.exists(value)) {
            paste0("sha256:", cgv_cached_sha256_file(value, base_dir))
        } else {
            ""
        }
    )
}

cgv_resolve_source_ref <- function(ref, base_dir = ".") {
    if (is.character(ref)) {
        value <- cgv_safe_scalar(ref)
        if (!nzchar(value)) return("")
        if (cgv_is_absolute_path(value)) return(value)
        candidate <- normalizePath(file.path(base_dir, value), winslash = "/", mustWork = FALSE)
        return(candidate)
    }
    if (!is.list(ref)) return("")
    relative <- cgv_safe_scalar(ref$relative_path)
    if (!nzchar(relative)) return("")
    normalizePath(file.path(base_dir, relative), winslash = "/", mustWork = FALSE)
}

cgv_redact_session_snapshot <- function(snapshot, include_private = FALSE, base_dir = ".") {
    out <- snapshot %||% list()
    if (!is.list(out)) stop("Invalid CGeV session snapshot.")
    out$schema_version <- cgv_session_schema_version
    out$privacy <- list(
        private_sequences_included = isTRUE(include_private),
        absolute_paths_removed = TRUE
    )

    redact_panel <- function(panel) {
        if (!is.list(panel)) return(panel)
        panel$annotation_source <- cgv_portable_source_ref(panel$annotation_path %||% "", base_dir)
        panel$genome_source <- cgv_portable_source_ref(panel$genome_path %||% "", base_dir)
        panel$annotation_path <- panel$annotation_source$relative_path
        panel$genome_path <- panel$genome_source$relative_path
        if (!isTRUE(include_private)) {
            panel$sequence_blob <- ""
        }
        panel
    }

    for (key in c("homologous", "orthologous")) {
        section <- out[[key]] %||% list()
        plots <- section$plots %||% list()
        section$plots <- lapply(plots, redact_panel)
        out[[key]] <- section
    }
    out
}

cgv_df_records <- function(value) {
    if (is.null(value)) return(list())
    df <- tryCatch(as.data.frame(value, stringsAsFactors = FALSE), error = function(e) data.frame())
    if (!nrow(df)) return(list())
    df[] <- lapply(df, function(column) {
        if (inherits(column, "POSIXt")) return(format(column, "%Y-%m-%dT%H:%M:%S%z"))
        if (is.factor(column)) return(as.character(column))
        if (is.list(column)) {
            return(vapply(column, function(item) {
                paste(as.character(unlist(item, recursive = TRUE, use.names = FALSE)), collapse = "; ")
            }, character(1)))
        }
        column
    })
    unname(lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE])))
}

cgv_collect_plot_manifest <- function(section, workflow, base_dir = ".") {
    plots <- section$plots %||% list()
    lapply(plots, function(plot) {
        if (!is.list(plot)) return(NULL)
        data <- plot$plot_data
        columns <- if (is.data.frame(data)) names(data) else character(0)
        info <- plot$organism_info %||% list()
        gene_meta <- plot$plot_gene_meta %||% list()
        metrics <- plot$plot_metrics %||% list()
        transcript_columns <- intersect(
            c("transcript_id", "transcript", "transcript_name", "transcript_label"),
            columns
        )
        transcripts <- unique(Filter(nzchar, unlist(lapply(transcript_columns, function(column) {
            as.character(data[[column]] %||% character(0))
        }), use.names = FALSE)))
        if (!length(transcripts)) {
            title_match <- regmatches(
                cgv_safe_scalar(plot$title),
                regexec("Transcript\\s*:\\s*([^|]+)", cgv_safe_scalar(plot$title), ignore.case = TRUE)
            )[[1L]]
            if (length(title_match) >= 2L) transcripts <- trimws(title_match[[2L]])
        }
        list(
            id = cgv_safe_scalar(plot$id),
            workflow = workflow,
            title = cgv_safe_scalar(plot$title),
            chromosome = cgv_safe_scalar(plot$chr_name),
            organism = list(
                id = cgv_safe_scalar(info$id %||% info$species_id),
                name = cgv_safe_scalar(info$name %||% info$organism),
                taxid = cgv_safe_scalar(info$taxid %||% info$taxonomy_id),
                assembly = cgv_safe_scalar(info$assembly %||% info$assembly_accession),
                annotation = cgv_safe_scalar(info$annotation %||% info$annotation_release),
                icon_data_uri = cgv_organism_icon_data_uri(
                    info$name %||% info$organism,
                    info$icon %||% "",
                    base_dir
                )
            ),
            source = list(
                annotation = plot$annotation_source %||% cgv_portable_source_ref(plot$annotation_path %||% ""),
                genome = plot$genome_source %||% cgv_portable_source_ref(plot$genome_path %||% "")
            ),
            gene = cgv_redact_absolute_path_fields(gene_meta),
            metrics = cgv_redact_absolute_path_fields(metrics),
            selected_transcripts = I(transcripts),
            plot_signature = cgv_safe_scalar(plot$plot_signature),
            data_rows = if (is.data.frame(data)) nrow(data) else 0L,
            data_columns = I(columns)
        )
    })
}

cgv_compact_structural_records <- function(plots) {
    lapply(plots %||% list(), function(plot) {
        record <- plot %||% list()
        organism <- record$organism %||% list()
        organism$icon_data_uri <- NULL
        record$organism <- organism
        gene <- record$gene %||% list()
        for (key in c(
            "lookup_alias_candidates",
            "lookup_alias_used",
            "local_alias_terms",
            "precomputed_neighbor_context"
        )) {
            gene[[key]] <- NULL
        }
        record$gene <- gene
        record
    })
}

cgv_alias_decision_manifest <- function(plots) {
    decisions <- list()
    decision_index <- new.env(parent = emptyenv(), hash = TRUE)
    for (plot in plots %||% list()) {
        record <- plot %||% list()
        gene <- record$gene %||% list()
        organism <- record$organism %||% list()
        key <- paste(
            cgv_first_nonempty(organism$id, organism$name),
            cgv_first_nonempty(gene$query_gene_input, gene$query_gene),
            cgv_first_nonempty(gene$matched_gene_id, gene$matched_gene_name),
            sep = "|"
        )
        if (!nzchar(gsub("\\|", "", key, fixed = FALSE))) next
        if (exists(key, envir = decision_index, inherits = FALSE)) {
            idx <- get(key, envir = decision_index, inherits = FALSE)
            decisions[[idx]]$record_ids <- unique(c(
                as.character(decisions[[idx]]$record_ids %||% character(0)),
                cgv_safe_scalar(record$id)
            ))
            next
        }
        decisions[[length(decisions) + 1L]] <- list(
            record_ids = I(Filter(nzchar, c(cgv_safe_scalar(record$id)))),
            organism_id = cgv_first_nonempty(organism$id, organism$name),
            query_gene = cgv_safe_scalar(gene$query_gene),
            query_gene_input = cgv_safe_scalar(gene$query_gene_input),
            matched_gene_name = cgv_safe_scalar(gene$matched_gene_name),
            display_gene_name = cgv_safe_scalar(gene$display_gene_name),
            matched_gene_id = cgv_safe_scalar(gene$matched_gene_id),
            lookup_alias_candidates = gene$lookup_alias_candidates %||% list(),
            lookup_alias_used = gene$lookup_alias_used %||% list(),
            local_alias_terms = gene$local_alias_terms %||% list(),
            precomputed_neighbor_context = gene$precomputed_neighbor_context %||% list()
        )
        assign(key, length(decisions), envir = decision_index)
    }
    unname(decisions)
}

cgv_reference_accession <- function(...) {
    values <- paste(
        Filter(nzchar, as.character(unlist(list(...), use.names = FALSE))),
        collapse = " "
    )
    match <- regmatches(
        values,
        regexpr("(?:GCF|GCA)_[0-9]+(?:\\.[0-9]+)?", values, perl = TRUE)
    )
    if (!length(match) || !nzchar(match[[1L]])) "" else match[[1L]]
}

cgv_reference_version <- function(accession, fallback = "") {
    match <- regmatches(
        cgv_safe_scalar(accession),
        regexpr("\\.[0-9]+$", cgv_safe_scalar(accession), perl = TRUE)
    )
    if (length(match) && nzchar(match[[1L]])) {
        substring(match[[1L]], 2L)
    } else {
        cgv_safe_scalar(fallback)
    }
}

cgv_analysis_reference_manifest <- function(plots, app_state, base_dir = ".") {
    data_root <- tryCatch(
        get_cgv_data_root(base_dir),
        error = function(e) normalizePath(base_dir, winslash = "/", mustWork = FALSE)
    )
    refs <- lapply(plots, function(plot) {
        workflow <- cgv_safe_scalar(plot$workflow)
        mode <- if (identical(workflow, "multi_gene")) {
            cgv_safe_scalar(app_state$homo_data_mode)
        } else {
            cgv_safe_scalar(app_state$ortho_data_mode)
        }
        source_label <- switch(
            tolower(mode),
            preloaded = "CGeV preloaded reference",
            ncbi = "NCBI Datasets",
            upload = "Author upload",
            if (nzchar(mode)) mode else "Not recorded"
        )
        raw_source <- plot$source %||% list()
        source_with_checksum <- function(ref) {
            ref <- ref %||% list()
            path <- cgv_resolve_source_ref(ref, data_root)
            portable <- cgv_portable_source_ref(path, data_root, include_checksum = TRUE)
            if (!nzchar(portable$name)) portable$name <- cgv_safe_scalar(ref$name)
            portable
        }
        annotation_source <- source_with_checksum(raw_source$annotation)
        genome_source <- source_with_checksum(raw_source$genome)
        organism <- plot$organism %||% list()
        accession <- cgv_reference_accession(
            organism$assembly,
            genome_source$name,
            genome_source$relative_path,
            annotation_source$name,
            annotation_source$relative_path
        )
        list(
            organism_id = cgv_safe_scalar(organism$id %||% organism$name),
            organism_name = cgv_safe_scalar(organism$name),
            assembly = list(
                accession = accession,
                version = cgv_reference_version(accession, organism$assembly_version),
                source = source_label,
                checksum = cgv_safe_scalar(genome_source$checksum),
                file_name = cgv_safe_scalar(genome_source$name)
            ),
            annotation = list(
                accession = cgv_safe_scalar(organism$annotation_accession, accession),
                version = cgv_safe_scalar(organism$annotation %||% organism$annotation_version),
                source = source_label,
                checksum = cgv_safe_scalar(annotation_source$checksum),
                file_name = cgv_safe_scalar(annotation_source$name)
            )
        )
    })
    keys <- vapply(refs, function(item) {
        paste(
            cgv_safe_scalar(item$organism_id),
            cgv_safe_scalar(item$assembly$accession),
            cgv_safe_scalar(item$assembly$checksum),
            sep = "|"
        )
    }, character(1))
    unname(refs[!duplicated(keys)])
}

cgv_sanitize_svg <- function(svg, max_bytes = 4L * 1024L * 1024L) {
    value <- cgv_safe_scalar(svg)
    if (!nzchar(value)) return("")
    if (nchar(value, type = "bytes") > max_bytes) {
        stop("A captured SVG exceeds the configured per-figure limit.")
    }
    value <- gsub("<(?:script|foreignObject|iframe|object|embed)\\b[^>]*>[\\s\\S]*?</(?:script|foreignObject|iframe|object|embed)\\s*>", "", value, ignore.case = TRUE, perl = TRUE)
    value <- gsub("<(?:script|iframe|object|embed)\\b[^>]*/\\s*>", "", value, ignore.case = TRUE, perl = TRUE)
    value <- gsub("\\s+on[a-zA-Z]+\\s*=\\s*(?:\"[^\"]*\"|'[^']*'|[^\\s>]+)", "", value, ignore.case = TRUE, perl = TRUE)
    value <- gsub("\\s+(?:href|xlink:href)\\s*=\\s*(?:\"\\s*(?:javascript:|https?:|//)[^\"]*\"|'\\s*(?:javascript:|https?:|//)[^']*'|(?:javascript:|https?:|//)[^\\s>]+)", "", value, ignore.case = TRUE, perl = TRUE)
    value <- gsub("@import\\s+(?:url\\s*\\()?\\s*['\"]?(?:https?:|//)[^;})]+[;})]?", "", value, ignore.case = TRUE, perl = TRUE)
    value <- gsub("url\\s*\\(\\s*['\"]?(?:https?:|//)[^)]+\\)", "none", value, ignore.case = TRUE, perl = TRUE)
    value <- gsub("javascript\\s*:", "", value, ignore.case = TRUE, perl = TRUE)
    value
}

cgv_normalize_client_assets <- function(payload, max_total_bytes = 24L * 1024L * 1024L) {
    raw_assets <- payload$assets %||% list()
    if (!is.list(raw_assets)) raw_assets <- list()
    total <- 0
    assets <- list()
    used_ids <- character(0)
    for (i in seq_along(raw_assets)) {
        item <- raw_assets[[i]]
        if (!is.list(item)) next
        svg <- cgv_sanitize_svg(item$svg %||% "")
        if (!nzchar(svg)) next
        total <- total + nchar(svg, type = "bytes")
        if (total > max_total_bytes) {
            stop("Captured figures exceed the 24 MB report limit.")
        }
        raw_id <- cgv_safe_filename(item$id %||% paste0("figure_", i), paste0("figure_", i))
        id <- raw_id
        suffix <- 1L
        while (id %in% used_ids) {
            suffix <- suffix + 1L
            id <- paste0(raw_id, "_", suffix)
        }
        used_ids <- c(used_ids, id)
        assets[[length(assets) + 1L]] <- list(
            id = id,
            title = substr(cgv_safe_scalar(item$title, paste("Figure", i)), 1L, 180L),
            group = tolower(cgv_safe_scalar(item$group, "visualization")),
            context = tolower(cgv_safe_scalar(item$context, "analysis")),
            source_id = substr(cgv_safe_scalar(item$source_id), 1L, 180L),
            record_id = substr(cgv_safe_scalar(item$record_id), 1L, 120L),
            gene = substr(cgv_safe_scalar(item$gene), 1L, 180L),
            comparison_gene = substr(cgv_safe_scalar(item$comparison_gene), 1L, 180L),
            transcript = substr(cgv_safe_scalar(item$transcript), 1L, 180L),
            organism = substr(cgv_safe_scalar(item$organism), 1L, 180L),
            svg = svg
        )
    }
    assets
}

cgv_scope_shared_snapshot <- function(snapshot,
                                      include_multi_gene = TRUE,
                                      include_cross_species = TRUE) {
    scoped <- snapshot %||% list()
    include_multi_gene <- isTRUE(include_multi_gene)
    include_cross_species <- isTRUE(include_cross_species)
    if (!include_multi_gene && !include_cross_species) {
        stop("Select at least one analysis workflow for the report.")
    }

    empty_section <- function(section) {
        section <- section %||% list()
        section$plot_counter <- 0L
        section$plots <- list()
        section
    }
    app_state <- scoped$app %||% list()
    if (!include_multi_gene) {
        scoped$homologous <- empty_section(scoped$homologous)
        for (key in c(
            "homo_data_mode", "homo_preloaded_species", "homo_visual_mode",
            "homo_sort_mode", "homo_summary_visible", "search_status_homo",
            "genome_source_homo", "current_organism_homo", "filter1"
        )) app_state[[key]] <- NULL
        if (is.list(app_state$alignment_parameters)) {
            app_state$alignment_parameters$multi_gene <- NULL
        }
    }
    if (!include_cross_species) {
        scoped$orthologous <- empty_section(scoped$orthologous)
        for (key in c(
            "ortho_data_mode", "ortho_preloaded_species", "ortho_visual_mode",
            "ortho_sort_mode", "ortho_summary_visible", "search_status_ortho",
            "genome_source_ortho", "current_organism_ortho", "gene_name"
        )) app_state[[key]] <- NULL
        if (is.list(app_state$alignment_parameters)) {
            app_state$alignment_parameters$aligned <- NULL
            app_state$alignment_parameters$lastz <- NULL
            app_state$alignment_parameters$multipip <- NULL
        }
    }
    # Figure Studio can combine panels from both workflows. Until panel-level
    # provenance is portable, omit the composition from a single-workflow
    # report instead of silently leaking results from the excluded workflow.
    if (xor(include_multi_gene, include_cross_species)) {
        app_state$figure_studio_state <- "{\"version\":2,\"panels\":[]}"
        included_plots <- c(
            if (include_multi_gene) (scoped$homologous %||% list())$plots %||% list() else list(),
            if (include_cross_species) (scoped$orthologous %||% list())$plots %||% list() else list()
        )
        scoped_genes <- unique(Filter(nzchar, vapply(included_plots, function(plot) {
            meta <- (plot %||% list())$plot_gene_meta %||% list()
            gene <- cgv_first_nonempty(
                meta$display_gene_name,
                meta$matched_gene_name,
                meta$query_gene,
                meta$input
            )
            if (nzchar(gene)) return(gene)
            title <- cgv_safe_scalar((plot %||% list())$title)
            match <- regmatches(title, regexec("Gene\\s*:\\s*([^|]+)", title, ignore.case = TRUE))[[1L]]
            if (length(match) >= 2L) trimws(match[[2L]]) else ""
        }, character(1))))
        app_state$global_search_query <- paste(scoped_genes, collapse = "; ")
        app_state$global_search_query_collapsed <- app_state$global_search_query
        app_state$global_search_chip_payload <- ""
    }
    app_state$preferred_workflow <- if (include_multi_gene && !include_cross_species) {
        "homologous"
    } else if (include_cross_species && !include_multi_gene) {
        "orthologous"
    } else {
        app_state$preferred_workflow
    }
    scoped$app <- app_state
    scoped
}

cgv_scope_client_payload <- function(payload,
                                     include_multi_gene = TRUE,
                                     include_cross_species = TRUE) {
    scoped <- payload %||% list()
    allowed <- c(
        if (isTRUE(include_multi_gene)) "multi_gene",
        if (isTRUE(include_cross_species)) "cross_species"
    )
    include_global <- length(allowed) == 2L
    raw_assets <- scoped$assets %||% list()
    if (!is.list(raw_assets)) raw_assets <- list()
    scoped$assets <- Filter(function(item) {
        if (!is.list(item)) return(FALSE)
        context <- tolower(cgv_safe_scalar(item$context, "analysis"))
        if (context %in% c("homo", "homologous")) context <- "multi_gene"
        if (context %in% c("ortho", "orthologous")) context <- "cross_species"
        context %in% allowed || (include_global && context %in% c("analysis", "figure_studio"))
    }, raw_assets)
    if (!include_global) scoped$external_results <- list()
    scoped
}

cgv_scope_alignment_runs <- function(runs,
                                     include_multi_gene = TRUE,
                                     include_cross_species = TRUE) {
    runs <- runs %||% list()
    if (!is.list(runs) || !length(runs)) return(list())
    if (isTRUE(include_multi_gene) && isTRUE(include_cross_species)) return(runs)
    prefixes <- c(
        if (isTRUE(include_multi_gene)) "multi_gene_",
        if (isTRUE(include_cross_species)) "cross_species_"
    )
    run_names <- names(runs)
    if (is.null(run_names)) return(list())
    keep <- vapply(run_names, function(id) {
        any(startsWith(tolower(as.character(id)), prefixes))
    }, logical(1))
    runs[keep]
}

cgv_alignment_completed <- function(run) {
    status <- tolower(cgv_safe_scalar((run %||% list())$status))
    status %in% c("ok", "completed", "complete", "success", "succeeded")
}

cgv_completed_alignment_runs <- function(runs) {
    runs <- runs %||% list()
    if (!is.list(runs)) return(list())
    Filter(cgv_alignment_completed, runs)
}

cgv_alignment_manifest <- function(runs, mode) {
    runs <- cgv_completed_alignment_runs(runs)
    lapply(names(runs), function(id) {
        run <- runs[[id]] %||% list()
        table <- if (identical(mode, "multipip")) run$segments else run$blocks
        job <- run$job %||% list()
        parameters <- run$parameters %||% list(
            format = cgv_safe_scalar(job$format),
            extra_args = as.character(job$extra_args %||% character(0))
        )
        list(
            id = cgv_safe_scalar(id),
            mode = mode,
            algorithm = cgv_safe_scalar(run$engine %||% job$engine, "lastz"),
            status = cgv_safe_scalar(run$status),
            reference_width = suppressWarnings(as.integer(run$reference_width %||% NA_integer_)),
            query_width = suppressWarnings(as.integer(run$query_width %||% NA_integer_)),
            parameters = parameters,
            result_rows = if (is.data.frame(table)) nrow(table) else 0L,
            stderr = substr(paste(as.character(run$stderr %||% ""), collapse = " "), 1L, 500L)
        )
    })
}

cgv_build_analysis_manifest <- function(snapshot,
                                        homo_summary = NULL,
                                        ortho_summary = NULL,
                                        pip_runs = list(),
                                        multipip_runs = list(),
                                        client_payload = list(),
                                        include_private = FALSE,
                                        allow_downloads = FALSE,
                                        ttl_days = 7L,
                                        app_version = "unknown",
                                        base_dir = ".") {
    safe_snapshot <- cgv_redact_session_snapshot(snapshot, include_private = include_private, base_dir = base_dir)
    assets <- cgv_normalize_client_assets(client_payload %||% list())
    app_state <- safe_snapshot$app %||% list()
    homo_plots <- Filter(Negate(is.null), cgv_collect_plot_manifest(
        safe_snapshot$homologous %||% list(),
        "multi_gene",
        base_dir
    ))
    ortho_plots <- Filter(Negate(is.null), cgv_collect_plot_manifest(
        safe_snapshot$orthologous %||% list(),
        "cross_species",
        base_dir
    ))
    structural_records <- cgv_compact_structural_records(c(homo_plots, ortho_plots))
    capture_mode <- tolower(cgv_safe_scalar(client_payload$capture_mode, "complete"))
    if (!capture_mode %in% c("complete", "fast")) capture_mode <- "complete"
    workflows <- character(0)
    if (length(homo_plots)) workflows <- c(workflows, "multi_gene")
    if (length(ortho_plots)) workflows <- c(workflows, "cross_species")
    created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ttl <- suppressWarnings(as.integer(ttl_days))
    if (!ttl %in% c(7L, 14L, 30L)) ttl <- 7L

    plot_gene_terms <- unique(Filter(nzchar, unlist(lapply(c(homo_plots, ortho_plots), function(plot) {
            meta <- plot$gene %||% list()
            from_meta <- cgv_first_nonempty(
                meta$gene,
                meta$symbol,
                meta$display_gene_name,
                meta$matched_gene_name,
                meta$query_gene,
                meta$input_gene,
                meta$input
            )
            if (nzchar(from_meta)) return(from_meta)
            title <- cgv_safe_scalar(plot$title)
            match <- regmatches(
                title,
                regexec("Gene\\s*:\\s*([^|]+)", title, ignore.case = TRUE)
            )[[1L]]
            if (length(match) >= 2L) trimws(match[[2L]]) else ""
        }), use.names = FALSE)))
    gene_terms <- if (length(plot_gene_terms)) {
        plot_gene_terms
    } else {
        unique(Filter(nzchar, c(
            cgv_safe_scalar(app_state$filter1),
            cgv_safe_scalar(app_state$gene_name),
            cgv_safe_scalar(app_state$global_search_query)
        )))
    }
    organisms <- unname(Filter(function(item) nzchar(cgv_safe_scalar(item$name)) || nzchar(cgv_safe_scalar(item$id)),
        lapply(c(homo_plots, ortho_plots), function(plot) plot$organism %||% list())
    ))
    organism_keys <- vapply(organisms, function(item) {
        tolower(cgv_first_nonempty(item$id, item$name, item$taxid))
    }, character(1))
    organisms <- unname(organisms[!duplicated(organism_keys)])
    selected_transcripts <- unique(Filter(nzchar, unlist(lapply(c(homo_plots, ortho_plots), function(plot) {
        as.character(plot$selected_transcripts %||% character(0))
    }), use.names = FALSE)))
    selection_ids <- function(value) {
        if (is.list(value) && !is.data.frame(value)) {
            return(unique(Filter(nzchar, unlist(lapply(value, function(item) {
                if (is.list(item)) {
                    cgv_safe_scalar(item$id %||% item$species_id %||% item$name)
                } else {
                    cgv_safe_scalar(item)
                }
            }), use.names = FALSE))))
        }
        unique(Filter(nzchar, as.character(value %||% character(0))))
    }
    result_ids <- unique(tolower(Filter(nzchar, unlist(lapply(organisms, function(item) {
        c(cgv_safe_scalar(item$id), cgv_safe_scalar(item$name))
    }), use.names = FALSE))))
    unresolved_records <- function(ids, workflow, reason) {
        ids <- unique(Filter(nzchar, ids))
        ids <- ids[!tolower(ids) %in% result_ids]
        lapply(ids, function(id) list(
            organism = id,
            workflow = workflow,
            reason = if (nzchar(reason)) reason else "No result was produced for this organism."
        ))
    }
    unresolved_organisms <- c(
        unresolved_records(
            unique(c(
                selection_ids(app_state$homo_preloaded_species),
                selection_ids(app_state$current_organism_homo)
            )),
            "multi_gene",
            cgv_safe_scalar(app_state$search_status_homo)
        ),
        unresolved_records(
            unique(c(
                selection_ids(app_state$ortho_preloaded_species),
                selection_ids(app_state$current_organism_ortho)
            )),
            "cross_species",
            cgv_safe_scalar(app_state$search_status_ortho)
        )
    )

    manifest <- list(
        schema_version = cgv_analysis_schema_version,
        analysis_id = cgv_random_secret(16L),
        created_at = created,
        expires_at = format(Sys.time() + ttl * 86400, "%Y-%m-%dT%H:%M:%S%z"),
        generator = list(name = "CGeV", version = cgv_safe_scalar(app_version, "unknown")),
        branding = list(
            name = "Comparative Gene Viewer",
            short_name = "CGeV",
            logo_data_uri = cgv_local_asset_data_uri("favicon2.ico", base_dir),
            primary_color = "#2C3E50",
            accent_color = "#18BC9C"
        ),
        workflows = I(unique(workflows)),
        query = list(
            genes = I(gene_terms),
            selected_transcripts = I(selected_transcripts),
            preferred_workflow = cgv_safe_scalar(app_state$preferred_workflow),
            data_modes = list(
                multi_gene = cgv_safe_scalar(app_state$homo_data_mode),
                cross_species = cgv_safe_scalar(app_state$ortho_data_mode)
            )
        ),
        organisms = organisms,
        references = cgv_analysis_reference_manifest(
            c(homo_plots, ortho_plots),
            app_state,
            base_dir
        ),
        parameters = list(
            visual_modes = list(
                multi_gene = cgv_safe_scalar(app_state$homo_visual_mode),
                cross_species = cgv_safe_scalar(app_state$ortho_visual_mode)
            ),
            sorting = list(
                multi_gene = cgv_safe_scalar(app_state$homo_sort_mode),
                cross_species = cgv_safe_scalar(app_state$ortho_sort_mode)
            ),
            alignments = cgv_redact_absolute_path_fields(app_state$alignment_parameters %||% list()),
            external_alias_sources = I(as.character(Filter(nzchar, c(
                if (isTRUE(app_state$ext_alias_source_mygene)) "MyGene",
                if (isTRUE(app_state$ext_alias_source_ncbi)) "NCBI",
                if (isTRUE(app_state$ext_alias_source_uniprot)) "UniProt",
                if (isTRUE(app_state$ext_alias_source_ensembl)) "Ensembl"
            ))))
        ),
        provenance = list(
            alias_decisions = cgv_alias_decision_manifest(c(homo_plots, ortho_plots)),
            sources = unname(lapply(c(homo_plots, ortho_plots), function(plot) plot$source %||% list())),
            organisms_without_result = unresolved_organisms,
            no_result = list(
                multi_gene_status = cgv_safe_scalar(app_state$search_status_homo),
                cross_species_status = cgv_safe_scalar(app_state$search_status_ortho),
                selected_multi_gene_organism = app_state$current_organism_homo %||% list(),
                selected_cross_species_organisms = I(as.character(app_state$current_organism_ortho %||% character(0))),
                selected_preloaded_ids = list(
                    multi_gene = cgv_safe_scalar(app_state$homo_preloaded_species),
                    cross_species = I(as.character(app_state$ortho_preloaded_species %||% character(0)))
                ),
                organisms_with_results = I(unique(vapply(organisms, function(item) {
                    cgv_safe_scalar(item$id %||% item$name)
                }, character(1))))
            )
        ),
        results = list(
            structural = structural_records,
            alignments = list(
                lastz = cgv_alignment_manifest(pip_runs, "lastz"),
                multipip = cgv_alignment_manifest(multipip_runs, "multipip")
            ),
            external = cgv_redact_absolute_path_fields(client_payload$external_results %||% list())
        ),
        tables = list(
            multi_gene = cgv_df_records(homo_summary),
            cross_species = cgv_df_records(ortho_summary)
        ),
        figures = assets,
        figure_studio = list(
            state = tryCatch(
                cgv_redact_absolute_path_fields(jsonlite::fromJSON(
                    cgv_safe_scalar(app_state$figure_studio_state),
                    simplifyVector = FALSE
                )),
                error = function(e) list()
            ),
            included = any(vapply(assets, function(item) identical(item$group, "figure_studio"), logical(1)))
        ),
        privacy = list(
            access = "secret_link",
            private_data_included = isTRUE(include_private),
            public_downloads = isTRUE(allow_downloads),
            ttl_days = ttl,
            warning = "Anyone with the secret URL can view and copy information visible in this report."
        ),
        capture = list(
            mode = capture_mode,
            missing = I(as.character(client_payload$missing %||% character(0))),
            omitted = I(as.character(client_payload$omitted %||% character(0))),
            captured_figure_count = length(assets)
        )
    )
    cgv_redact_absolute_path_fields(manifest)
}

cgv_validate_analysis_manifest <- function(analysis) {
    if (!is.list(analysis) || !identical(as.integer(analysis$schema_version %||% 0L), cgv_analysis_schema_version)) {
        stop("Unsupported or invalid analysis.json schema.")
    }
    required <- c(
        "analysis_id", "created_at", "generator", "branding", "query", "organisms",
        "references", "parameters", "provenance", "results", "tables",
        "figures", "privacy", "capture"
    )
    missing <- required[!vapply(required, function(key) !is.null(analysis[[key]]), logical(1))]
    if (length(missing)) {
        stop("analysis.json is missing required fields: ", paste(missing, collapse = ", "))
    }
    if (!is.list(analysis$privacy) ||
        !is.logical(analysis$privacy$private_data_included) ||
        !is.logical(analysis$privacy$public_downloads)) {
        stop("analysis.json contains an invalid privacy policy.")
    }
    invalid_reference <- any(vapply(analysis$references %||% list(), function(ref) {
        !is.list(ref$assembly) ||
            !is.list(ref$annotation) ||
            is.null(ref$assembly$source) ||
            is.null(ref$assembly$checksum) ||
            is.null(ref$annotation$source) ||
            is.null(ref$annotation$checksum)
    }, logical(1)))
    if (invalid_reference) stop("analysis.json contains an invalid assembly/annotation reference.")

    absolute_paths <- unique(cgv_collect_absolute_manifest_paths(analysis))
    if (length(absolute_paths)) {
        stop("analysis.json contains an absolute internal path.")
    }
    invisible(TRUE)
}

cgv_report_css <- function() {
    paste(c(
        ":root{color-scheme:light;--ink:#263d4f;--muted:#667d8d;--line:#dfe8ed;--paper:#fff;--bg:#f3f6f8;--navy:#20384b;--navy2:#2c5368;--accent:#18bc9c;--accent-dark:#128f76;--soft:#f7fafb}",
        "*{box-sizing:border-box}html{scroll-behavior:smooth}body{margin:0;background:var(--bg);color:var(--ink);font:14px/1.52 Roboto,\"Helvetica Neue\",Arial,sans-serif}",
        ".report-hero{position:relative;overflow:hidden;background:linear-gradient(135deg,var(--navy),var(--navy2));color:#fff;box-shadow:0 12px 34px rgba(22,48,67,.2)}.report-hero:after{content:'';position:absolute;width:420px;height:420px;right:-110px;top:-260px;border-radius:50%;background:rgba(24,188,156,.17)}.hero-inner{position:relative;z-index:1;max-width:1320px;margin:auto;padding:25px 24px 28px}.brand-lockup{display:flex;align-items:center;gap:10px;margin-bottom:23px;color:#dff8f2;font-size:12px;font-weight:800;letter-spacing:.08em;text-transform:uppercase}.brand-logo{width:34px;height:34px;padding:6px;border:1px solid rgba(255,255,255,.26);border-radius:10px;background:rgba(255,255,255,.1);object-fit:contain}.hero-copy{display:flex;align-items:flex-end;justify-content:space-between;gap:28px}.hero-copy h1{max-width:900px;margin:0 0 6px;font-size:clamp(27px,4vw,43px);line-height:1.1;letter-spacing:-.035em}.hero-copy p{margin:0;color:#d7e6ed}.badges{display:flex;flex-wrap:wrap;gap:7px;margin-top:15px}.badge{display:inline-flex;align-items:center;border:1px solid rgba(255,255,255,.23);border-radius:999px;padding:5px 9px;background:rgba(255,255,255,.07);color:#f4fbfd;font-size:11px;text-decoration:none}.badge-download{border-color:rgba(133,235,212,.55);background:rgba(24,188,156,.17)}",
        "main{max-width:1320px;margin:22px auto;padding:0 22px 62px}.privacy-note{display:flex;align-items:flex-start;gap:10px;margin-bottom:15px;padding:10px 13px;border:1px solid #edda9c;border-radius:11px;background:#fff9e8;color:#705818;font-size:12px}.privacy-icon,.section-icon{display:grid;place-items:center;flex:0 0 auto}.privacy-icon{width:24px;height:24px;border-radius:7px;background:#f3df9e;font-weight:850}",
        ".report-section,details.report-details{margin:13px 0;border:1px solid var(--line);border-radius:15px;background:var(--paper);box-shadow:0 7px 24px rgba(26,57,76,.055);overflow:hidden}.section-heading{display:flex;align-items:center;justify-content:space-between;gap:18px;padding:16px 18px;border-bottom:1px solid var(--line);background:linear-gradient(180deg,#fff,#fbfcfd)}details.report-section>summary.section-heading{cursor:pointer;list-style:none;border-bottom:0}details.report-section>summary.section-heading::-webkit-details-marker{display:none}details.report-section>summary.section-heading:after{content:'＋';flex:0 0 auto;color:var(--accent-dark);font-size:19px;font-weight:500}details.report-section[open]>summary.section-heading{border-bottom:1px solid var(--line)}details.report-section[open]>summary.section-heading:after{content:'−'}.section-title{display:flex;align-items:center;gap:11px;min-width:0;flex:1}.section-icon{width:31px;height:31px;border-radius:9px;background:rgba(24,188,156,.11);color:#128f76;font-size:11px;font-weight:850;letter-spacing:-.02em}.section-title h2{margin:0;font-size:17px;letter-spacing:-.015em}.section-title p{margin:2px 0 0;color:var(--muted);font-size:11px}.section-count{margin-left:auto;padding:3px 8px;border-radius:999px;background:#eef6f5;color:#347565;font-size:10px;font-weight:800}.section-body{padding:16px 18px 19px}",
        ".overview-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:10px}.overview-card{min-width:0;padding:12px 13px;border:1px solid var(--line);border-radius:11px;background:var(--soft)}.overview-card b{display:block;margin-bottom:4px;color:var(--muted);font-size:10px;letter-spacing:.075em;text-transform:uppercase}.overview-card span{display:block;overflow:hidden;color:var(--ink);font-weight:700;text-overflow:ellipsis}.organism-row{display:flex;flex-wrap:wrap;gap:7px;margin-top:12px}.organism-chip{display:inline-flex;align-items:center;gap:7px;min-height:33px;padding:5px 9px 5px 6px;border:1px solid #dce7eb;border-radius:10px;background:#fff}.organism-chip img{width:22px;height:22px;border-radius:6px;object-fit:contain}.organism-chip span{font-size:11px;font-weight:700}",
        ".locus-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(245px,1fr));gap:9px;margin-bottom:14px}.locus-card{padding:10px 12px;border:1px solid var(--line);border-radius:11px;background:#fafcfd}.locus-top{display:flex;align-items:center;justify-content:space-between;gap:8px;margin-bottom:8px}.locus-name{display:flex;align-items:center;gap:7px;min-width:0;font-size:11px;font-weight:750}.locus-name img{width:20px;height:20px;border-radius:5px;object-fit:contain}.locus-chr{color:var(--muted);font-size:10px;white-space:nowrap}.locus-chrmap{margin:2px 1px 0}.locus-chrmap svg{display:block;width:100%;height:16px}.locus-coords{margin-top:5px;color:#758995;font-size:9px;text-align:center}",
        ".flow-stack{display:grid;gap:13px}.flow-group{border:1px solid #dce7eb;border-radius:12px;background:#fbfcfd;overflow:hidden}.flow-group>summary,.analysis-group>summary{display:flex;align-items:center;justify-content:space-between;gap:12px;list-style:none;cursor:pointer}.flow-group>summary::-webkit-details-marker,.analysis-group>summary::-webkit-details-marker{display:none}.flow-group>summary{padding:12px 14px;background:#f1f7f7}.flow-group>summary:after,.analysis-group>summary:after{content:'＋';color:var(--accent-dark);font-size:16px}.flow-group[open]>summary:after,.analysis-group[open]>summary:after{content:'−'}.flow-heading{display:flex;align-items:center;gap:9px;min-width:0}.flow-badge{padding:3px 7px;border-radius:999px;background:#dff3ee;color:#147d68;font-size:9px;font-weight:850;text-transform:uppercase}.flow-heading strong{font-size:13px}.flow-summary{overflow:hidden;color:var(--muted);font-size:10px;text-overflow:ellipsis;white-space:nowrap}.flow-content{display:grid;gap:11px;padding:12px}.analysis-group{border:1px solid var(--line);border-radius:10px;background:#fff;overflow:hidden}.analysis-group>summary{padding:10px 12px;background:#f8fafb}.analysis-summary-main{display:flex;align-items:center;gap:8px;min-width:0}.analysis-summary-main strong{overflow:hidden;font-size:12px;text-overflow:ellipsis;white-space:nowrap}.analysis-summary-meta{color:var(--muted);font-size:10px;white-space:nowrap}.analysis-content{display:grid;gap:10px;padding:11px}.transcript-filter{display:flex;align-items:center;justify-content:flex-end;gap:8px;padding:8px 10px;border:1px solid var(--line);border-radius:9px;background:var(--soft)}.transcript-filter label{font-size:10px;font-weight:750}.transcript-filter select{max-width:min(100%,390px);padding:5px 8px;border:1px solid #cbd9e0;border-radius:7px;background:#fff;color:var(--ink);font:inherit;font-size:11px}.figure-list{display:grid;gap:11px}.figure-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:11px}.figure-card{min-width:0;border:1px solid var(--line);border-radius:12px;background:#fff;overflow:hidden}.figure-card[hidden]{display:none}.figure-card-primary,.figure-card-wide{grid-column:1/-1}.figure-card-header{display:flex;align-items:center;justify-content:space-between;gap:12px;padding:8px 11px;border-bottom:1px solid var(--line);background:#f8fafb}.figure-card-header h3{min-width:0;margin:0;overflow:hidden;font-size:12px;text-overflow:ellipsis;white-space:nowrap}.figure-tag{flex:0 0 auto;padding:3px 7px;border-radius:999px;background:rgba(24,188,156,.1);color:#11836d;font-size:9px;font-weight:800;text-transform:uppercase}.figure-view{position:relative;min-height:0;padding:8px;overflow:auto;background:#fff}.figure-card-primary .figure-view{min-height:0;padding:10px 12px}.figure-card-compact .figure-view{min-height:0;max-height:130px}.figure-card-compact .figure-view svg{max-height:105px}.figure-view svg{display:block;width:100%;height:auto;margin:auto}.figure-tools{display:flex;justify-content:flex-end;gap:5px;padding:6px 9px;border-top:1px solid var(--line);background:#fbfcfd}.figure-tools button,.figure-tools a{padding:4px 8px;border:1px solid #cbd9e0;border-radius:7px;background:#fff;color:#466173;font:inherit;font-size:10px;cursor:pointer;text-decoration:none}.figure-tools button:hover,.figure-tools a:hover{border-color:var(--accent);color:var(--accent-dark)}",
        ".table-tools{display:flex;gap:10px;margin-bottom:10px}.table-tools input{width:min(360px,100%);padding:7px 9px;border:1px solid #cbd9e0;border-radius:8px;font:inherit}.table-wrap{max-height:560px;overflow:auto;border:1px solid var(--line);border-radius:10px}table{width:100%;border-collapse:collapse;font-size:11px}th,td{padding:8px 9px;border-bottom:1px solid var(--line);text-align:left;white-space:nowrap}th{position:sticky;z-index:1;top:0;background:#edf4f6;color:#3f5969;cursor:pointer}tr:hover td{background:#f6fbfa}",
        "details.report-details summary{display:flex;align-items:center;justify-content:space-between;gap:12px;padding:14px 17px;list-style:none;font-size:13px;font-weight:780;cursor:pointer}details.report-details summary::-webkit-details-marker{display:none}details.report-details summary:after{content:'＋';color:var(--accent-dark);font-size:17px}details.report-details[open] summary:after{content:'−'}details.report-details .section-body{border-top:1px solid var(--line)}pre{max-height:420px;margin:0;padding:13px;overflow:auto;border-radius:9px;background:#173248;color:#e7f2f6;font:10px/1.55 \"SFMono-Regular\",Consolas,monospace;white-space:pre-wrap;word-break:break-word}.empty{padding:13px;border:1px dashed #cbd8df;border-radius:10px;color:var(--muted);font-size:11px;font-style:italic}.tooltip{position:fixed;z-index:100;max-width:340px;max-height:min(420px,70vh);padding:9px 11px;overflow:auto;border-radius:8px;background:#173248;color:#fff;font-size:11px;line-height:1.45;white-space:pre-line;pointer-events:none;box-shadow:0 5px 16px #0004}.footer{margin-top:25px;color:var(--muted);font-size:10px;text-align:center}",
        "@media(max-width:840px){.overview-grid{grid-template-columns:repeat(2,minmax(0,1fr))}.figure-grid{grid-template-columns:1fr}.hero-copy{align-items:flex-start;flex-direction:column}.figure-filter{align-items:flex-start;flex-direction:column}}@media(max-width:560px){.hero-inner{padding:20px 16px 23px}main{padding:0 10px 42px}.section-heading,.section-body{padding-left:12px;padding-right:12px}.overview-grid{grid-template-columns:1fr}.locus-grid{grid-template-columns:1fr}.brand-lockup{margin-bottom:17px}}"
    ), collapse = "\n")
}

cgv_report_js <- function() {
    paste(c(
        "(function(){'use strict';",
        "var node=document.getElementById('cgv-analysis-data');if(!node)return;var a=JSON.parse(node.textContent||'{}');",
        "function el(tag,cls,text){var n=document.createElement(tag);if(cls)n.className=cls;if(text!==undefined)n.textContent=String(text);return n}",
        "function add(parent,child){if(child)parent.appendChild(child);return child}",
        "function value(x){return x===null||x===undefined||x===''?'—':String(x)}",
        "function array(x){return Array.isArray(x)?x:(x===null||x===undefined||x==='')?[]:[x]}",
        "function text(x){return String(x===null||x===undefined?'':x)}",
        "function section(id,visible){var n=document.getElementById(id);if(n)n.hidden=!visible;return n}",
        "var genes=array(a.query&&a.query.genes);document.getElementById('report-title').textContent=(genes.length?'CGeV analysis: '+genes.join(', '):'CGeV interactive analysis');",
        "document.getElementById('report-subtitle').textContent='Read-only snapshot created '+value(a.created_at);",
        "var logo=a.branding&&a.branding.logo_data_uri;if(logo){document.getElementById('report-logo').src=logo}else{document.getElementById('report-logo').hidden=true}",
        "var badges=document.getElementById('report-badges');var captureMode=text(a.capture&&a.capture.mode||'complete');[value(a.generator&&a.generator.version),array(a.workflows).map(function(w){return w==='multi_gene'?'Multi-Gene':w==='cross_species'?'Cross-Species':w}).join(' + '),(captureMode==='fast'?'Fast capture':'Complete capture'),'Expires '+value(a.expires_at)].forEach(function(x){add(badges,el('span','badge',x))});",
        "if(a.privacy&&a.privacy.public_downloads){var dl=add(badges,el('a','badge badge-download','Download reproducibility ZIP'));dl.href='downloads/cgv_reproducibility.zip';dl.download='cgv_reproducibility.zip'}",
        "var organisms=array(a.organisms);var overview=document.getElementById('overview-cards');var cards=[['Genes',genes.join(', ')],['Organisms',organisms.length],['Transcripts',array(a.query&&a.query.selected_transcripts).length],['Captured views',array(a.figures).length]];cards.forEach(function(c){var d=add(overview,el('div','overview-card'));add(d,el('b','',c[0]));add(d,el('span','',value(c[1])))});",
        "var orgRoot=document.getElementById('organism-row');var seenOrg={};organisms.forEach(function(o){var key=text(o.id||o.name);if(!key||seenOrg[key])return;seenOrg[key]=true;var chip=add(orgRoot,el('div','organism-chip'));if(o.icon_data_uri){var img=add(chip,el('img'));img.src=o.icon_data_uri;img.alt=''}add(chip,el('span','',o.name||o.id))});if(!orgRoot.children.length)orgRoot.hidden=true;",
        "function numberFrom(obj,keys){for(var i=0;i<keys.length;i++){var n=Number(obj&&obj[keys[i]]);if(isFinite(n))return n}return NaN}",
        "function normalizedContext(value){value=text(value).toLowerCase();if(value==='homo'||value==='homologous')return 'multi_gene';if(value==='ortho'||value==='orthologous')return 'cross_species';return value||'analysis'}",
        "function workflowLabel(context){context=normalizedContext(context);return context==='multi_gene'?'Multi-Gene':context==='cross_species'?'Cross-Species':'Analysis'}",
        "function organismFor(record){var id=text(record&&record.organism&&(record.organism.id||record.organism.name));for(var i=0;i<organisms.length;i++){if(text(organisms[i].id||organisms[i].name)===id)return organisms[i]}return record&&record.organism||{}}",
        "var structuralRecords=array(a.results&&a.results.structural);function recordGene(record){var gene=record&&record.gene||{};return text(gene.display_gene_name||gene.matched_gene_name||gene.symbol||gene.gene||gene.input||gene.query_gene||'')}",
        "function recordTranscript(record){var selected=array(record&&record.selected_transcripts);if(selected.length)return text(selected[0]);var match=text(record&&record.title).match(/Transcript\\s*:\\s*([^|]+)/i);return match?match[1].trim():''}",
        "function recordOrganism(record){var org=organismFor(record);return text(org.name||org.id)}",
        "function workflowRecords(context){context=normalizedContext(context);return structuralRecords.filter(function(record){return normalizedContext(record.workflow)===context})}",
        "function recordForFigure(figure){var context=normalizedContext(figure&&figure.context);var records=workflowRecords(context);var id=text(figure&&figure.record_id).replace(/_c$/,'');if(!id){var idMatch=text(figure&&figure.source_id).match(/^(?:homo|ortho)-card-(.+)$/);if(idMatch)id=idMatch[1].replace(/_c$/,'')}if(id){for(var i=0;i<records.length;i++){if(text(records[i].id)===id)return records[i]}}var tx=text(figure&&figure.transcript);if(tx){for(var j=0;j<records.length;j++){if(recordTranscript(records[j])===tx)return records[j]}}var gene=text(figure&&figure.gene).toLowerCase();var org=text(figure&&figure.organism).toLowerCase();if(!gene&&!org)return null;for(var k=0;k<records.length;k++){if((!gene||recordGene(records[k]).toLowerCase()===gene)&&(!org||recordOrganism(records[k]).toLowerCase()===org))return records[k]}return null}",
        "function uniqueValues(values){var seen={};return values.filter(function(value){var key=text(value);if(!key||seen[key])return false;seen[key]=true;return true})}",
        "function uniqueFigures(list){var seen={};return list.filter(function(figure){var record=recordForFigure(figure);var key=[normalizedContext(figure.context),text(figure.group),text(figure.source_id||figure.id),text(figure.comparison_gene),text(figure.transcript||recordTranscript(record)),text(figure.gene||recordGene(record)),text(figure.organism||recordOrganism(record))].join('|').toLowerCase();if(seen[key])return false;seen[key]=true;return true})}",
        "function workflowSummary(context){var records=workflowRecords(context);var genes=uniqueValues(records.map(recordGene));var orgs=uniqueValues(records.map(recordOrganism));if(normalizedContext(context)==='multi_gene')return (genes.slice(0,4).join(', ')||'Gene analysis')+(genes.length>4?' +'+(genes.length-4):'');return (genes.slice(0,2).join(', ')||'Cross-species analysis')+' · '+orgs.length+' organism'+(orgs.length===1?'':'s')}",
        "function createFlowGroup(root,context,count,open,headingText,summaryText){var details=add(root,el('details','flow-group'));details.open=open!==false;var summary=add(details,el('summary'));var heading=add(summary,el('div','flow-heading'));add(heading,el('span','flow-badge',workflowLabel(context)));var copy=add(heading,el('div'));add(copy,el('strong','',headingText||workflowSummary(context)));add(copy,el('div','flow-summary',summaryText||count+' captured view'+(count===1?'':'s')));var content=add(details,el('div','flow-content'));return content}",
        "function setSectionCount(sectionId,count){var sectionNode=document.getElementById(sectionId);var countNode=sectionNode&&sectionNode.querySelector('.section-count');if(countNode)countNode.textContent=count+' item'+(count===1?'':'s')}",
        "function summaryRowsFor(context){return array(a.tables&&a.tables[normalizedContext(context)])}",
        "function makeChromosomeMap(parent,start,end,chrLen){if(!isFinite(start)||!isFinite(chrLen)||chrLen<=0)return;var sFrac=Math.max(0,Math.min(1,(start-1)/chrLen));var eFrac=isFinite(end)?Math.max(sFrac,Math.min(1,end/chrLen)):sFrac;var markerX=Math.max(1,Math.min(138*sFrac,135));var markerW=Math.max(2,138*(eFrac-sFrac));if(markerX+markerW>137)markerW=Math.max(2,137-markerX);var ns='http://www.w3.org/2000/svg';var wrap=add(parent,el('div','locus-chrmap'));var svg=document.createElementNS(ns,'svg');svg.setAttribute('viewBox','0 0 138 16');svg.setAttribute('preserveAspectRatio','none');var path=document.createElementNS(ns,'path');path.setAttribute('d','M8,1 H58 C61,1 63,2.6 63,4 H75 C75,2.6 77,1 80,1 H130 C134,1 137,4 137,8 C137,12 134,15 130,15 H80 C77,15 75,13.4 75,12 H63 C63,13.4 61,15 58,15 H8 C4,15 1,12 1,8 C1,4 4,1 8,1 Z');path.setAttribute('fill','#e2ebf3');path.setAttribute('stroke','#7f96ab');path.setAttribute('stroke-width','1');svg.appendChild(path);var marker=document.createElementNS(ns,'rect');marker.setAttribute('x',markerX.toFixed(2));marker.setAttribute('y','2');marker.setAttribute('width',markerW.toFixed(2));marker.setAttribute('height','12');marker.setAttribute('rx','2');marker.setAttribute('fill','#eb5e5e');marker.setAttribute('stroke','#b53f3f');svg.appendChild(marker);wrap.appendChild(svg)}",
        "function renderLocusCard(root,record,summary){var org=organismFor(record);var gene=record.gene||{};var metrics=record.metrics||{};var geneLabel=recordGene(record);var start=numberFrom(gene,['start','gene_start','tx_start','genomic_start','Gene_Start_bp']);if(!isFinite(start))start=numberFrom(metrics,['start','gene_start','tx_start','Gene_Start_bp']);if(!isFinite(start))start=numberFrom(summary,['Gene_Start_bp','Transcript_Start_bp','start']);var end=numberFrom(gene,['end','gene_end','tx_end','genomic_end','Gene_End_bp']);if(!isFinite(end))end=numberFrom(metrics,['end','gene_end','tx_end','Gene_End_bp']);if(!isFinite(end))end=numberFrom(summary,['Gene_End_bp','Transcript_End_bp','end']);var chrLen=numberFrom(gene,['chromosome_length','chr_length','seq_length']);if(!isFinite(chrLen))chrLen=numberFrom(metrics,['chromosome_length','chr_length']);var card=add(root,el('article','locus-card'));var top=add(card,el('div','locus-top'));var name=add(top,el('div','locus-name'));if(org.icon_data_uri){var img=add(name,el('img'));img.src=org.icon_data_uri;img.alt=''}add(name,el('span','',geneLabel||org.name||org.id||record.title||'Gene'));add(top,el('span','locus-chr',record.chromosome||summary.Chromosome||'Chromosome'));makeChromosomeMap(card,start,end,chrLen);add(card,el('div','locus-coords',isFinite(start)&&isFinite(end)?start.toLocaleString()+' – '+end.toLocaleString()+' bp':'Gene position in chromosome'))}",
        "function renderLoci(){var root=document.getElementById('locus-grid');var total=0;['multi_gene','cross_species'].forEach(function(context){var records=workflowRecords(context);var seen={};var unique=[];records.forEach(function(record){var key=[recordOrganism(record),text(record.chromosome),recordGene(record)].join('|').toLowerCase();if(seen[key])return;seen[key]=true;unique.push(record)});if(!unique.length)return;total+=unique.length;var content=createFlowGroup(root,context,unique.length,true);var grid=add(content,el('div','locus-grid'));var rows=summaryRowsFor(context);unique.forEach(function(record){var tx=recordTranscript(record);var summary=rows.filter(function(row){return !tx||text(row.Transcript||row.transcript)===tx})[0]||{};renderLocusCard(grid,record,summary)})});setSectionCount('section-locus',total);section('section-locus',total>0)}",
        "var analyticsTitles={arch:'Gene architecture',exon:'Transcript isoforms',seq:'Nucleotide composition',context:'Genomic context',exon_dist:'Exon length distribution',intron_dist:'Intron length distribution',scatter:'Gene and transcript lengths',heatmap:'Feature heatmap',radar:'Comparative feature profile',corr:'Feature correlation'}",
        "function humanFigureTitle(figure,record){var group=text(figure.group);if(group==='structural'){var gene=text(figure.gene||recordGene(record)||'Gene');var tx=text(figure.transcript||recordTranscript(record));return gene+(tx?' · '+tx:'')}if(group==='synteny'){var comparison=text(figure.comparison_gene);return workflowLabel(figure.context)+' aligned synteny'+(comparison?' · '+comparison:' · '+workflowSummary(figure.context))}if(group==='analytics'){var id=text(figure.source_id||figure.id).toLowerCase().replace(/^(homo|ortho)_/,'').replace(/_chart_export.*$/,'');return analyticsTitles[id]||text(figure.title||figure.id).replace(/[_-]+/g,' ')}if(group==='alignment')return workflowLabel(figure.context)+' · '+text(figure.title||figure.id).replace(/[_-]+/g,' ');return text(figure.title||figure.id||'CGeV visualization').replace(/[_-]+/g,' ')}",
        "function figureCard(figure,variant,record){var card=el('article','figure-card '+variant);card.dataset.transcript=text(figure.transcript||recordTranscript(record));var head=add(card,el('div','figure-card-header'));add(head,el('h3','',humanFigureTitle(figure,record)));add(head,el('span','figure-tag',text(figure.group||'view').replace(/_/g,' ')));var view=add(card,el('div','figure-view'));view.innerHTML=figure.svg||'';var tools=add(card,el('div','figure-tools'));var zoom=1;[['−',-.15],['＋',.15],['Reset',0]].forEach(function(def){var button=add(tools,el('button','',def[0]));button.type='button';button.addEventListener('click',function(){zoom=def[1]===0?1:Math.max(.35,Math.min(3,zoom+def[1]));var svg=view.querySelector('svg');if(svg){svg.style.width=(zoom*100)+'%';svg.style.maxWidth='none'}})});if(a.privacy&&a.privacy.public_downloads){var blob=new Blob([figure.svg||''],{type:'image/svg+xml'});var link=add(tools,el('a','','SVG'));link.href=URL.createObjectURL(blob);link.download=(figure.id||'figure')+'.svg'}return card}",
        "function renderStructural(root,list){var entries=list.map(function(figure){return {figure:figure,record:recordForFigure(figure)}});var contexts=uniqueValues(entries.map(function(entry){return normalizedContext(entry.figure.context)}));contexts.sort(function(left,right){return ['multi_gene','cross_species','analysis'].indexOf(left)-['multi_gene','cross_species','analysis'].indexOf(right)});contexts.forEach(function(context){var selected=entries.filter(function(entry){return normalizedContext(entry.figure.context)===context});var flow=createFlowGroup(root,context,selected.length,true);var grouped={};var order=[];selected.forEach(function(entry){var record=entry.record;var key=[text(entry.figure.gene||recordGene(record)||'Unassigned gene'),text(entry.figure.organism||recordOrganism(record)||'')].join('|').toLowerCase();if(!grouped[key]){grouped[key]=[];order.push(key)}grouped[key].push(entry)});order.forEach(function(key){var group=grouped[key];var first=group[0];var gene=text(first.figure.gene||recordGene(first.record)||'Gene');var organism=text(first.figure.organism||recordOrganism(first.record));var details=add(flow,el('details','analysis-group'));var summary=add(details,el('summary'));var main=add(summary,el('div','analysis-summary-main'));add(main,el('strong','',gene));add(main,el('span','analysis-summary-meta',(organism?organism+' · ':'')+group.length+' transcript'+(group.length===1?'':'s')));var content=add(details,el('div','analysis-content'));var cards=add(content,el('div','figure-list'));var cardNodes=[];group.forEach(function(entry,index){var card=figureCard(entry.figure,'figure-card-primary',entry.record);card.hidden=index!==0;cards.appendChild(card);cardNodes.push(card)});if(group.length>1){var filter=content.insertBefore(el('div','transcript-filter'),cards);add(filter,el('label','','View'));var selector=add(filter,el('select'));var primary=el('option','','Primary transcript · '+value(cardNodes[0].dataset.transcript));primary.value='__primary__';selector.appendChild(primary);var all=el('option','','All transcripts ('+group.length+')');all.value='__all__';selector.appendChild(all);uniqueValues(cardNodes.map(function(card){return card.dataset.transcript})).forEach(function(tx){var option=el('option','',tx);option.value=tx;selector.appendChild(option)});selector.addEventListener('change',function(){cardNodes.forEach(function(card,index){card.hidden=selector.value==='__all__'?false:selector.value==='__primary__'?index!==0:card.dataset.transcript!==selector.value})})}})})}",
        "function renderFigureGroup(group,rootId,sectionId,variant){var root=document.getElementById(rootId);var list=uniqueFigures(array(a.figures).filter(function(figure){return text(figure.group||'structural')===group}));if(group==='structural'){renderStructural(root,list)}else{var contexts=uniqueValues(list.map(function(figure){return normalizedContext(figure.context)}));contexts.sort(function(left,right){return ['multi_gene','cross_species','analysis','figure_studio'].indexOf(left)-['multi_gene','cross_species','analysis','figure_studio'].indexOf(right)});contexts.forEach(function(context){var figures=list.filter(function(figure){return normalizedContext(figure.context)===context});var comparisonGenes=uniqueValues(figures.map(function(figure){return text(figure.comparison_gene)}));var flowHeading=group==='synteny'&&context==='multi_gene'&&comparisonGenes.length?comparisonGenes.join(', '):workflowSummary(context);var flowSummary=group==='synteny'&&context==='multi_gene'?figures.length+' gene comparison'+(figures.length===1?'':'s'):figures.length+' captured view'+(figures.length===1?'':'s');var target=context==='analysis'||context==='figure_studio'?root:createFlowGroup(root,context,figures.length,true,flowHeading,flowSummary);var grid=add(target,el('div',group==='analytics'?'figure-grid':'figure-list'));figures.forEach(function(figure){grid.appendChild(figureCard(figure,variant,recordForFigure(figure)))})})}setSectionCount(sectionId,list.length);section(sectionId,list.length>0)}",
        "function renderFigures(){renderFigureGroup('structural','figures-structural','section-structural','figure-card-primary');renderFigureGroup('synteny','figures-synteny','section-synteny','figure-card-wide');renderFigureGroup('alignment','figures-alignment','section-alignment','figure-card-wide');renderFigureGroup('analytics','figures-analytics','section-analytics','');renderFigureGroup('figure_studio','figures-studio','section-studio','figure-card-wide')}",
        "function table(rootId,rows){var root=document.getElementById(rootId);rows=Array.isArray(rows)?rows:[];if(!rows.length)return false;var keys=Object.keys(rows[0]||{});var tools=add(root,el('div','table-tools'));var search=add(tools,el('input'));search.type='search';search.placeholder='Filter rows';var wrap=add(root,el('div','table-wrap'));var t=add(wrap,el('table'));var head=add(t,el('thead'));var hr=add(head,el('tr'));var body=add(t,el('tbody'));var orderKey='',asc=true;keys.forEach(function(k){var th=add(hr,el('th','',k));th.addEventListener('click',function(){asc=orderKey===k?!asc:true;orderKey=k;draw()})});function draw(){var q=(search.value||'').toLowerCase();var data=rows.filter(function(r){return !q||keys.some(function(k){return value(r[k]).toLowerCase().indexOf(q)>=0})});if(orderKey)data.sort(function(x,y){return value(x[orderKey]).localeCompare(value(y[orderKey]),undefined,{numeric:true})*(asc?1:-1)});body.textContent='';data.forEach(function(r){var tr=add(body,el('tr'));keys.forEach(function(k){add(tr,el('td','',value(r[k])))})})}search.addEventListener('input',draw);draw();return true}",
        "renderLoci();renderFigures();document.getElementById('details-table-multi').hidden=!table('table-multi',a.tables&&a.tables.multi_gene);document.getElementById('details-table-cross').hidden=!table('table-cross',a.tables&&a.tables.cross_species);",
        "document.getElementById('provenance').textContent=JSON.stringify(a.provenance||{},null,2);document.getElementById('parameters').textContent=JSON.stringify(a.parameters||{},null,2);document.getElementById('external-results').textContent=JSON.stringify(a.results&&a.results.external||[],null,2);",
        "var missing=array(a.capture&&a.capture.missing);var omitted=array(a.capture&&a.capture.omitted);var notes=[];if(omitted.length)notes.push('Intentionally omitted in fast mode:\\n- '+omitted.join('\\n- '));if(missing.length)notes.push('Items that could not be captured:\\n- '+missing.join('\\n- '));document.getElementById('missing').textContent=notes.length?notes.join('\\n\\n'):'No missing or intentionally omitted capture items were reported.';",
        "function decodeTooltipEntities(raw){var decoder=document.createElement('textarea');var current=text(raw);for(var i=0;i<3;i++){decoder.innerHTML=current;var next=decoder.value;if(next===current)break;current=next}return current}",
        "function plainTooltip(raw){var decoded=decodeTooltipEntities(raw).replace(/<br\\s*\\/?>/gi,'\\n').replace(/<hr\\s*\\/?>/gi,'\\n').replace(/<\\/(?:div|p|li|h[1-6])>/gi,'\\n');var doc=new DOMParser().parseFromString('<div>'+decoded+'</div>','text/html');return text(doc.body&&doc.body.textContent).replace(/\\u00a0/g,' ').replace(/[ \\t]+\\n/g,'\\n').replace(/\\n{3,}/g,'\\n\\n').trim()}",
        "var tip=el('div','tooltip');tip.hidden=true;document.body.appendChild(tip);document.addEventListener('mouseover',function(e){var n=e.target.closest&&e.target.closest('[data-tooltip],[title]');if(!n)return;var raw=n.getAttribute('data-tooltip')||n.getAttribute('title');var copy=plainTooltip(raw);if(!copy)return;tip.textContent=copy;tip.hidden=false});document.addEventListener('mousemove',function(e){if(!tip.hidden){tip.style.left=Math.max(8,Math.min(innerWidth-356,e.clientX+14))+'px';tip.style.top=Math.max(8,Math.min(innerHeight-tip.offsetHeight-12,e.clientY+14))+'px'}});document.addEventListener('mouseout',function(e){if(e.target.closest&&e.target.closest('[data-tooltip],[title]'))tip.hidden=true});",
        "})();"
    ), collapse = "\n")
}

cgv_report_script_csp_hash <- function() {
    "sha256-EXkIJnQvs/u+7Sw8WFMz21f46Zk6qxolUQbZIaQ9yfg="
}

cgv_render_report_html <- function(analysis, validated = FALSE, json = NULL) {
    if (!isTRUE(validated)) cgv_validate_analysis_manifest(analysis)
    if (is.null(json)) {
        json <- jsonlite::toJSON(
            analysis,
            auto_unbox = TRUE,
            null = "null",
            na = "null",
            digits = NA
        )
    }
    json <- gsub("</script", "<\\\\/script", json, ignore.case = TRUE, fixed = FALSE)
    paste0(
        "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">",
        "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
        "<meta name=\"robots\" content=\"noindex,nofollow,noarchive\">",
        "<meta name=\"referrer\" content=\"no-referrer\">",
        "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'none'; img-src data: blob:; style-src 'unsafe-inline'; script-src '", cgv_report_script_csp_hash(), "'; connect-src 'none'; font-src 'none'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'\">",
        "<title>CGeV interactive analysis</title><style>", cgv_report_css(), "</style></head><body>",
        "<header class=\"report-hero\"><div class=\"hero-inner\"><div class=\"brand-lockup\"><img id=\"report-logo\" class=\"brand-logo\" alt=\"CGeV\"><span>Comparative Gene Viewer</span></div><div class=\"hero-copy\"><div><h1 id=\"report-title\">CGeV interactive analysis</h1><p id=\"report-subtitle\"></p><div id=\"report-badges\" class=\"badges\"></div></div></div></div></header>",
        "<main><div class=\"privacy-note\"><span class=\"privacy-icon\">i</span><span>This is an immutable read-only snapshot. Anyone with this secret URL can view and copy information shown here.</span></div>",
        "<section class=\"report-section\"><div class=\"section-heading\"><div class=\"section-title\"><span class=\"section-icon\">CGeV</span><div><h2>Analysis overview</h2><p>Query, organisms and captured scope</p></div></div></div><div class=\"section-body\"><div id=\"overview-cards\" class=\"overview-grid\"></div><div id=\"organism-row\" class=\"organism-row\"></div></div></section>",
        "<details id=\"section-locus\" class=\"report-section\" hidden><summary class=\"section-heading\"><div class=\"section-title\"><span class=\"section-icon\">CHR</span><div><h2>Genomic location</h2><p>Chromosome context separated by analysis workflow</p></div></div><span class=\"section-count\"></span></summary><div class=\"section-body\"><div id=\"locus-grid\" class=\"flow-stack\"></div></div></details>",
        "<details id=\"section-structural\" class=\"report-section\" hidden><summary class=\"section-heading\"><div class=\"section-title\"><span class=\"section-icon\">DNA</span><div><h2>Gene structure</h2><p>Choose one transcript or reveal all transcripts within each gene</p></div></div><span class=\"section-count\"></span></summary><div class=\"section-body\"><div id=\"figures-structural\" class=\"flow-stack\"></div></div></details>",
        "<details id=\"section-synteny\" class=\"report-section\" hidden><summary class=\"section-heading\"><div class=\"section-title\"><span class=\"section-icon\">SYN</span><div><h2>Aligned synteny</h2><p>Comparisons identified and separated by analysis workflow</p></div></div><span class=\"section-count\"></span></summary><div class=\"section-body\"><div id=\"figures-synteny\" class=\"flow-stack\"></div></div></details>",
        "<details id=\"section-alignment\" class=\"report-section\" hidden><summary class=\"section-heading\"><div class=\"section-title\"><span class=\"section-icon\">LZ</span><div><h2>LASTZ and MultiPIP</h2><p>Completed local alignment results and parameters</p></div></div><span class=\"section-count\"></span></summary><div class=\"section-body\"><div id=\"figures-alignment\" class=\"flow-stack\"></div></div></details>",
        "<details id=\"section-analytics\" class=\"report-section\" hidden><summary class=\"section-heading\"><div class=\"section-title\"><span class=\"section-icon\">STAT</span><div><h2>Analytics</h2><p>Derived charts captured whether or not Analytics was opened</p></div></div><span class=\"section-count\"></span></summary><div class=\"section-body\"><div id=\"figures-analytics\" class=\"flow-stack\"></div></div></details>",
        "<details id=\"section-studio\" class=\"report-section\" hidden><summary class=\"section-heading\"><div class=\"section-title\"><span class=\"section-icon\">FIG</span><div><h2>Figure Studio</h2><p>The saved SVG composition from the analysis</p></div></div><span class=\"section-count\"></span></summary><div class=\"section-body\"><div id=\"figures-studio\" class=\"figure-list\"></div></div></details>",
        "<details id=\"details-table-multi\" class=\"report-details\"><summary>Multi-Gene data table</summary><div class=\"section-body\" id=\"table-multi\"></div></details>",
        "<details id=\"details-table-cross\" class=\"report-details\"><summary>Cross-Species data table</summary><div class=\"section-body\" id=\"table-cross\"></div></details>",
        "<details class=\"report-details\"><summary>Captured external results</summary><div class=\"section-body\"><pre id=\"external-results\"></pre></div></details>",
        "<details class=\"report-details\"><summary>Methods and parameters</summary><div class=\"section-body\"><pre id=\"parameters\"></pre></div></details>",
        "<details class=\"report-details\"><summary>Provenance, aliases and unresolved results</summary><div class=\"section-body\"><pre id=\"provenance\"></pre></div></details>",
        "<details class=\"report-details\"><summary>Capture notes</summary><div class=\"section-body\"><pre id=\"missing\"></pre></div></details>",
        "<p class=\"footer\">Generated by CGeV. No live database requests or analysis jobs run from this report.</p></main>",
        "<script id=\"cgv-analysis-data\" type=\"application/json\">", json, "</script><script>", cgv_report_js(), "</script></body></html>"
    )
}

cgv_prepare_report_artifacts <- function(analysis) {
    cgv_validate_analysis_manifest(analysis)
    json <- jsonlite::toJSON(
        analysis,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        digits = NA
    )
    structure(list(
        analysis = analysis,
        json = as.character(json),
        html = cgv_render_report_html(
            analysis,
            validated = TRUE,
            json = json
        )
    ), class = "cgv_report_artifacts")
}

cgv_is_prepared_report_artifacts <- function(value) {
    inherits(value, "cgv_report_artifacts") &&
        is.list(value$analysis) &&
        is.character(value$json) &&
        length(value$json) == 1L &&
        is.character(value$html) &&
        length(value$html) == 1L
}

cgv_write_alignment_tables <- function(runs, mode, directory) {
    runs <- cgv_completed_alignment_runs(runs)
    if (!length(runs)) return(character(0))
    written <- character(0)
    for (id in names(runs)) {
        run <- runs[[id]] %||% list()
        table <- if (identical(mode, "multipip")) run$segments else run$blocks
        if (!is.data.frame(table) || !nrow(table)) next
        path <- file.path(directory, paste0(cgv_safe_filename(paste(mode, id, sep = "_")), ".tsv"))
        utils::write.table(table, path, sep = "\t", row.names = FALSE, quote = TRUE, na = "")
        written <- c(written, path)
    }
    written
}

cgv_write_sequence_files <- function(snapshot, directory) {
    written <- character(0)
    for (section_name in c("homologous", "orthologous")) {
        plots <- (snapshot[[section_name]] %||% list())$plots %||% list()
        for (plot in plots) {
            sequence <- cgv_safe_scalar((plot %||% list())$sequence_blob)
            if (!nzchar(trimws(sequence))) next
            id <- cgv_safe_filename((plot %||% list())$id, "sequence")
            path <- file.path(directory, paste0(section_name, "_", id, ".fasta"))
            if (!startsWith(trimws(sequence), ">")) {
                sequence <- paste0(">", section_name, "_", id, "\n", gsub("\\s+", "", sequence))
            }
            writeLines(sequence, path, useBytes = TRUE)
            written <- c(written, path)
        }
    }
    written
}

cgv_bundle_readme <- function(analysis) {
    genes <- paste(analysis$query$genes %||% character(0), collapse = ", ")
    organisms <- paste(vapply(analysis$organisms %||% list(), function(item) {
        cgv_safe_scalar(item$name %||% item$id)
    }, character(1)), collapse = ", ")
    paste(c(
        "# CGeV reproducibility package",
        "",
        sprintf("- CGeV version: %s", cgv_safe_scalar(analysis$generator$version)),
        sprintf("- Created: %s", cgv_safe_scalar(analysis$created_at)),
        sprintf("- Analysis schema: %s", cgv_safe_scalar(analysis$schema_version)),
        sprintf("- Genes: %s", if (nzchar(genes)) genes else "not recorded"),
        sprintf("- Organisms: %s", if (nzchar(organisms)) organisms else "not recorded"),
        "",
        "## Contents",
        "",
        "- `analysis.json`: portable machine-readable manifest.",
        "- `session/cgv_session.rds`: CGeV schema-v2 session snapshot.",
        "- `tables/`: summary data used by the report.",
        "- `sequences/`: included only when private sequence inclusion was explicitly enabled.",
        "- `alignments/`: completed LASTZ/MultiPIP tabular results.",
        "- `figures/`: sanitized SVG snapshots, including Figure Studio when available.",
        "- `CHECKSUMS.sha256`: integrity hashes for package files.",
        "",
        "## Reproduction notes",
        "",
        "Reference assemblies and annotations are identified by provenance metadata; complete reference genomes are not bundled.",
        "The interactive report is immutable and performs no live external queries.",
        if (isTRUE(analysis$privacy$private_data_included)) {
            "Private sequence inclusion was enabled by the author."
        } else {
            "Private sequences and uploaded source files were excluded. Restored sequence downloads may therefore be unavailable."
        }
    ), collapse = "\n")
}

cgv_dir_size <- function(path) {
    files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (!length(files)) return(0)
    sum(file.info(files)$size, na.rm = TRUE)
}

cgv_reports_root <- function(base_dir = ".") {
    root <- if (exists("get_cgv_cache_root", mode = "function")) {
        get_cgv_cache_root(base_dir)
    } else {
        file.path(base_dir, "cache")
    }
    file.path(root, "shared_reports")
}

cgv_packages_root <- function(base_dir = ".") {
    root <- if (exists("get_cgv_cache_root", mode = "function")) {
        get_cgv_cache_root(base_dir)
    } else {
        file.path(base_dir, "cache")
    }
    file.path(root, "reproducibility_packages")
}

cgv_reproducibility_staging_path <- function(package_root) {
    tempfile(
        pattern = ".cgv-reproducibility-",
        tmpdir = package_root,
        fileext = ".zip"
    )
}

cgv_zip_directory <- function(zipfile, directory, files = NULL) {
    if (!requireNamespace("processx", quietly = TRUE)) {
        stop("Creating a reproducibility package requires the 'processx' package.")
    }
    if (is.null(files)) {
        files <- list.files(directory, recursive = TRUE, all.files = FALSE, no.. = TRUE)
    }
    result <- tryCatch(
        processx::run(
            Sys.getenv("R_ZIPCMD", "zip"),
            args = c("-q", zipfile, files),
            wd = directory,
            error_on_status = FALSE
        ),
        error = function(e) structure(conditionMessage(e), class = "cgv_zip_error")
    )
    if (inherits(result, "cgv_zip_error")) {
        stop("Could not create the reproducibility package archive: ", as.character(result))
    }
    status <- suppressWarnings(as.integer(result$status))
    if (!is.finite(status) || status != 0L) {
        stop("Could not create the reproducibility package archive.")
    }
    invisible(status)
}

cgv_report_meta_root <- function(base_dir = ".") {
    root <- if (exists("get_cgv_cache_root", mode = "function")) {
        get_cgv_cache_root(base_dir)
    } else {
        file.path(base_dir, "cache")
    }
    file.path(root, "shared_report_metadata")
}

cgv_with_shared_storage_lock <- function(base_dir = ".", fn, timeout_seconds = 10) {
    cache_root <- if (exists("get_cgv_cache_root", mode = "function")) {
        get_cgv_cache_root(base_dir)
    } else {
        file.path(base_dir, "cache")
    }
    dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
    lock_path <- file.path(cache_root, ".shared-analysis-storage.lock")
    deadline <- Sys.time() + max(1, as.numeric(timeout_seconds))
    acquired <- FALSE
    repeat {
        acquired <- isTRUE(dir.create(lock_path, showWarnings = FALSE))
        if (acquired) break
        info <- file.info(lock_path)
        if (dir.exists(lock_path) &&
            !is.na(info$mtime) &&
            as.numeric(difftime(Sys.time(), info$mtime, units = "secs")) > 120) {
            unlink(lock_path, recursive = TRUE, force = TRUE)
        }
        if (Sys.time() >= deadline) {
            stop("Shared-analysis storage is busy; try the publication again.")
        }
        Sys.sleep(0.05)
    }
    on.exit(if (acquired) unlink(lock_path, recursive = TRUE, force = TRUE), add = TRUE)
    fn()
}

cgv_report_limits <- function() {
    per_report_mb <- suppressWarnings(as.numeric(Sys.getenv("APP_SHARED_REPORT_MAX_MB", "100")))
    total_gb <- suppressWarnings(as.numeric(Sys.getenv("APP_SHARED_REPORT_STORAGE_GB", "5")))
    if (!is.finite(per_report_mb) || per_report_mb < 1) per_report_mb <- 100
    if (!is.finite(total_gb) || total_gb < 0.1) total_gb <- 5
    list(per_report_bytes = per_report_mb * 1024^2, total_bytes = total_gb * 1024^3)
}

cgv_shared_storage_size <- function(base_dir = ".") {
    cgv_dir_size(cgv_reports_root(base_dir)) + cgv_dir_size(cgv_packages_root(base_dir))
}

cgv_write_reproducibility_package <- function(analysis,
                                              session_snapshot,
                                              homo_summary = NULL,
                                              ortho_summary = NULL,
                                              pip_runs = list(),
                                              multipip_runs = list(),
                                              include_private = FALSE,
                                              base_dir = ".",
                                              artifacts = NULL) {
    prepared <- cgv_is_prepared_report_artifacts(artifacts)
    if (prepared) {
        analysis <- artifacts$analysis
    } else {
        cgv_validate_analysis_manifest(analysis)
    }
    package_root <- cgv_packages_root(base_dir)
    dir.create(package_root, recursive = TRUE, showWarnings = FALSE)
    work_dir <- tempfile("cgv-package-")
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
    dirs <- file.path(work_dir, c("session", "tables", "sequences", "alignments", "figures"))
    invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

    safe_snapshot <- cgv_redact_session_snapshot(session_snapshot, include_private, base_dir)
    if (prepared) {
        writeLines(artifacts$json, file.path(work_dir, "analysis.json"), useBytes = TRUE)
    } else {
        jsonlite::write_json(
            analysis,
            file.path(work_dir, "analysis.json"),
            auto_unbox = TRUE,
            pretty = FALSE,
            null = "null",
            na = "null",
            digits = NA
        )
    }
    writeLines(cgv_bundle_readme(analysis), file.path(work_dir, "README.md"), useBytes = TRUE)
    saveRDS(safe_snapshot, file.path(work_dir, "session", "cgv_session.rds"), compress = "gzip")
    if (is.data.frame(homo_summary) && nrow(homo_summary)) {
        utils::write.csv(homo_summary, file.path(work_dir, "tables", "multi_gene_summary.csv"), row.names = FALSE)
    }
    if (is.data.frame(ortho_summary) && nrow(ortho_summary)) {
        utils::write.csv(ortho_summary, file.path(work_dir, "tables", "cross_species_summary.csv"), row.names = FALSE)
    }
    if (isTRUE(include_private)) {
        cgv_write_sequence_files(safe_snapshot, file.path(work_dir, "sequences"))
    }
    cgv_write_alignment_tables(pip_runs, "lastz", file.path(work_dir, "alignments"))
    cgv_write_alignment_tables(multipip_runs, "multipip", file.path(work_dir, "alignments"))
    for (figure in analysis$figures %||% list()) {
        path <- file.path(work_dir, "figures", paste0(cgv_safe_filename(figure$id, "figure"), ".svg"))
        writeLines(cgv_sanitize_svg(figure$svg %||% ""), path, useBytes = TRUE)
    }

    checksum_files <- list.files(work_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
    checksum_files <- checksum_files[file.info(checksum_files)$isdir %in% FALSE]
    checksum_lines <- vapply(checksum_files, function(path) {
        sprintf("%s  %s", cgv_sha256_file(path), substring(path, nchar(work_dir) + 2L))
    }, character(1))
    writeLines(checksum_lines, file.path(work_dir, "CHECKSUMS.sha256"), useBytes = TRUE)

    limits <- cgv_report_limits()
    uncompressed <- cgv_dir_size(work_dir)
    if (uncompressed > limits$per_report_bytes) {
        stop(sprintf(
            "The reproducibility package is %.1f MB; the configured per-report limit is %.1f MB.",
            uncompressed / 1024^2,
            limits$per_report_bytes / 1024^2
        ))
    }

    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    zip_path <- file.path(package_root, paste0("cgv_reproducibility_", stamp, "_", cgv_random_secret(6L), ".zip"))
    # Atomic rename requires staging and destination to be on the same
    # filesystem. In production /tmp and the persistent /app/cache mount are
    # different filesystems, so stage directly in the package directory.
    zip_staging <- cgv_reproducibility_staging_path(package_root)
    on.exit(if (file.exists(zip_staging)) unlink(zip_staging, force = TRUE), add = TRUE)
    files <- list.files(work_dir, recursive = TRUE, all.files = FALSE, no.. = TRUE)
    cgv_zip_directory(zipfile = zip_staging, directory = work_dir, files = files)
    cgv_with_shared_storage_lock(base_dir, function() {
        staging_size <- file.info(zip_staging)$size
        existing_size <- max(0, cgv_shared_storage_size(base_dir) - staging_size)
        if (existing_size + staging_size > limits$total_bytes) {
            stop("Creating this package would exceed shared-analysis storage. No active report was removed.")
        }
        if (!file.rename(zip_staging, zip_path)) {
            stop("Could not atomically store the reproducibility package.")
        }
    })
    normalizePath(zip_path, winslash = "/", mustWork = TRUE)
}

cgv_publish_static_report <- function(analysis,
                                      package_path = "",
                                      allow_downloads = FALSE,
                                      base_dir = ".",
                                      artifacts = NULL) {
    if (!cgv_is_prepared_report_artifacts(artifacts)) {
        artifacts <- cgv_prepare_report_artifacts(analysis)
    }
    analysis <- artifacts$analysis
    report_root <- cgv_reports_root(base_dir)
    meta_root <- cgv_report_meta_root(base_dir)
    dir.create(report_root, recursive = TRUE, showWarnings = FALSE)
    dir.create(meta_root, recursive = TRUE, showWarnings = FALSE)
    limits <- cgv_report_limits()
    if (cgv_shared_storage_size(base_dir) >= limits$total_bytes) {
        stop("Shared-report storage is full. No active report was removed.")
    }

    public_token <- cgv_random_secret(32L)
    revoke_secret <- cgv_random_secret(32L)
    staging <- file.path(report_root, paste0(".", public_token, ".tmp"))
    final <- file.path(report_root, public_token)
    if (dir.exists(staging)) unlink(staging, recursive = TRUE, force = TRUE)
    dir.create(staging, recursive = TRUE, showWarnings = FALSE)
    on.exit(if (dir.exists(staging)) unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)

    writeLines(artifacts$html, file.path(staging, "index.html"), useBytes = TRUE)
    if (isTRUE(allow_downloads) && nzchar(package_path) && file.exists(package_path)) {
        dir.create(file.path(staging, "downloads"), showWarnings = FALSE)
        file.copy(package_path, file.path(staging, "downloads", "cgv_reproducibility.zip"), overwrite = TRUE)
    }
    metadata <- list(
        token_hash = cgv_sha256_text(public_token),
        revoke_hash = cgv_sha256_text(revoke_secret),
        created_at = analysis$created_at,
        expires_at = analysis$expires_at,
        allow_downloads = isTRUE(allow_downloads)
    )
    report_size <- cgv_dir_size(staging)
    if (report_size > limits$per_report_bytes) {
        stop(sprintf(
            "The shared report is %.1f MB; the configured limit is %.1f MB.",
            report_size / 1024^2,
            limits$per_report_bytes / 1024^2
        ))
    }
    meta_path <- file.path(meta_root, paste0(public_token, ".rds"))
    cgv_with_shared_storage_lock(base_dir, function() {
        existing_size <- max(0, cgv_shared_storage_size(base_dir) - cgv_dir_size(staging))
        if (existing_size + report_size > limits$total_bytes) {
            stop("Publishing this report would exceed shared-report storage. No active report was removed.")
        }
        if (!file.rename(staging, final)) {
            stop("Could not atomically publish the shared report.")
        }
        tryCatch(
            saveRDS(metadata, meta_path, compress = "gzip"),
            error = function(e) {
                unlink(final, recursive = TRUE, force = TRUE)
                stop("Could not persist private report revocation metadata.")
            }
        )
    })
    list(
        token = public_token,
        revoke_secret = revoke_secret,
        path = final,
        expires_at = analysis$expires_at,
        size_bytes = report_size
    )
}

cgv_revoke_static_report <- function(token, revoke_secret, base_dir = ".") {
    token <- tolower(trimws(cgv_safe_scalar(token)))
    revoke_secret <- tolower(trimws(cgv_safe_scalar(revoke_secret)))
    if (!grepl("^[a-f0-9]{64}$", token) || !grepl("^[a-f0-9]{64}$", revoke_secret)) {
        return(FALSE)
    }
    cgv_with_shared_storage_lock(base_dir, function() {
        target <- file.path(cgv_reports_root(base_dir), token)
        meta_path <- file.path(cgv_report_meta_root(base_dir), paste0(token, ".rds"))
        if (!file.exists(meta_path)) return(FALSE)
        metadata <- tryCatch(readRDS(meta_path), error = function(e) NULL)
        if (!is.list(metadata) || !identical(metadata$revoke_hash, cgv_sha256_text(revoke_secret))) {
            return(FALSE)
        }
        unlink(target, recursive = TRUE, force = TRUE)
        if (!dir.exists(target)) unlink(meta_path, force = TRUE)
        !dir.exists(target)
    })
}

cgv_cleanup_shared_reports <- function(base_dir = ".", now = Sys.time()) {
    cgv_with_shared_storage_lock(base_dir, function() {
        root <- cgv_reports_root(base_dir)
        if (!dir.exists(root)) return(invisible(0L))
        dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
        removed <- 0L
        for (path in dirs) {
            if (startsWith(basename(path), ".")) next
            meta_path <- file.path(cgv_report_meta_root(base_dir), paste0(basename(path), ".rds"))
            meta <- tryCatch(readRDS(meta_path), error = function(e) NULL)
            expires <- if (is.list(meta)) suppressWarnings(as.POSIXct(meta$expires_at, format = "%Y-%m-%dT%H:%M:%S%z")) else as.POSIXct(NA)
            if (is.na(expires) || expires <= now) {
                unlink(path, recursive = TRUE, force = TRUE)
                if (!dir.exists(path)) {
                    unlink(meta_path, force = TRUE)
                    removed <- removed + 1L
                }
            }
        }
        invisible(removed)
    })
}

cgv_cleanup_reproducibility_packages <- function(base_dir = ".", now = Sys.time(), max_age_hours = 24) {
    cgv_with_shared_storage_lock(base_dir, function() {
        root <- cgv_packages_root(base_dir)
        if (!dir.exists(root)) return(invisible(0L))
        files <- list.files(root, pattern = "\\.zip$", full.names = TRUE)
        if (!length(files)) return(invisible(0L))
        info <- file.info(files)
        cutoff <- now - max(1, as.numeric(max_age_hours)) * 3600
        stale <- files[!is.na(info$mtime) & info$mtime < cutoff]
        if (length(stale)) unlink(stale, force = TRUE)
        invisible(sum(!file.exists(stale)))
    })
}

cgv_public_base_url <- function(session) {
    configured <- sub("/+$", "", trimws(Sys.getenv("CGV_PUBLIC_BASE_URL", "")))
    if (nzchar(configured)) return(configured)
    protocol <- cgv_safe_scalar(session$clientData$url_protocol, "http:")
    hostname <- cgv_safe_scalar(session$clientData$url_hostname, "127.0.0.1")
    port <- cgv_safe_scalar(session$clientData$url_port)
    if (nzchar(port) && !port %in% c("80", "443")) {
        paste0(protocol, "//", hostname, ":", port)
    } else {
        paste0(protocol, "//", hostname)
    }
}

cgv_runtime_is_desktop <- function() {
    nzchar(trimws(Sys.getenv("CGV_DESKTOP_DATA_ROOT", ""))) ||
        nzchar(trimws(Sys.getenv("CGV_DESKTOP_RUNTIME_ROOT", ""))) ||
        identical(tolower(trimws(Sys.getenv("CGV_RUNTIME", ""))), "desktop")
}

init_shared_analysis_domain <- function(input,
                                        output,
                                        session,
                                        build_session_snapshot_fn,
                                        homo_summary_fn,
                                        ortho_summary_fn,
                                        pip_runs_fn,
                                        multipip_runs_fn,
                                        run_lastz_fn = NULL,
                                        active_homo_ids_rv,
                                        active_ortho_ids_rv,
                                        homo_synteny_available_fn = NULL,
                                        homo_synteny_groups_fn = NULL,
                                        plot_chr_length_fn = NULL,
                                        enqueue_background_report_fn = NULL,
                                        background_report_ready_fn = NULL,
                                        background_report_failed_fn = NULL,
                                        background_bootstrap = FALSE,
                                        app_version = "unknown",
                                        base_dir = ".") {
    session_deferred <- make_session_deferred(session)
    session_later <- session_deferred$later
    session_guard <- session_deferred$guard

    state <- shiny::reactiveValues(
        pending = NULL,
        package_path = "",
        package_context = NULL,
        desktop_html_path = "",
        artifacts = NULL,
        result = NULL,
        error = "",
        missing = character(0),
        preview = list(),
        busy = FALSE,
        busy_message = "",
        alignment_request = NULL,
        alignment_busy = FALSE,
        alignment_error = "",
        alignment_result = NULL,
        background_submissions = numeric(0)
    )

    has_results <- shiny::reactive({
        length(active_homo_ids_rv() %||% integer(0)) + length(active_ortho_ids_rv() %||% integer(0)) > 0L
    })

    report_perf_mark <- function(pending, step) {
        if (exists("app_perf_mark", mode = "function")) {
            try(app_perf_mark(
                (pending %||% list())$perf_run,
                step,
                "SHARED_REPORT"
            ), silent = TRUE)
        }
        invisible(NULL)
    }

    shiny::observe({
        shinyjs::toggle(
            "open_share_analysis_homo",
            condition = length(active_homo_ids_rv() %||% integer(0)) > 0L
        )
        shinyjs::toggle(
            "open_share_analysis_ortho",
            condition = length(active_ortho_ids_rv() %||% integer(0)) > 0L
        )
    })

    output$share_analysis_result_ui <- shiny::renderUI({
        if (isTRUE(state$busy)) {
            return(shiny::div(
                class = "cgv-share-progress",
                shiny::icon("spinner", class = "fa-spin"),
                shiny::span(cgv_safe_scalar(
                    state$busy_message,
                    "Capturing the current analysis and building the reproducibility package…"
                ))
            ))
        }
        if (length(state$missing %||% character(0))) {
            return(shiny::div(
                class = "cgv-share-callout cgv-share-callout-warning",
                shiny::strong("Some items could not be captured."),
                shiny::p("Review them before publishing. Continuing will explicitly exclude the listed items from the immutable report:"),
                shiny::tags$ul(lapply(state$missing, shiny::tags$li)),
                shiny::div(
                    class = "cgv-share-result-actions",
                    shiny::actionButton(
                        "continue_shared_analysis_without_missing",
                        "Publish without these items",
                        icon = shiny::icon("check")
                    ),
                    shiny::actionButton(
                        "cancel_shared_analysis_capture",
                        "Cancel",
                        icon = shiny::icon("xmark"),
                        class = "btn btn-default"
                    )
                )
            ))
        }
        if (nzchar(state$error %||% "")) {
            return(shiny::div(
                class = "cgv-share-callout cgv-share-callout-error",
                shiny::icon("triangle-exclamation"),
                shiny::span(state$error)
            ))
        }
        result <- state$result
        if (!is.list(result)) return(NULL)
        if (isTRUE(result$background)) {
            return(shiny::div(
                class = "cgv-share-result",
                shiny::div(
                    class = "cgv-share-callout cgv-share-callout-success",
                    shiny::icon("envelope-circle-check"),
                    shiny::strong("Report queued for email delivery.")
                ),
                shiny::p(
                    "CGeV saved an immutable copy of this analysis. You can keep working or close this page; the queued report will not change."
                ),
                shiny::p(class = "help-block", paste("Delivery:", result$email)),
                shiny::p(class = "help-block", paste("Reference:", result$job_id))
            ))
        }
        if (isTRUE(result$desktop)) {
            return(shiny::div(
                class = "cgv-share-callout cgv-share-callout-success",
                shiny::strong("Local report ready."),
                shiny::p("Download the self-contained interactive HTML and reproducibility ZIP below.")
            ))
        }
        shiny::div(
            class = "cgv-share-result",
            shiny::div(
                class = "cgv-share-callout cgv-share-callout-success",
                shiny::icon("circle-check"),
                shiny::strong("Secret report link created.")
            ),
            shiny::tags$label(`for` = "cgv-share-result-url", "Read-only URL"),
            shiny::tags$input(
                id = "cgv-share-result-url",
                class = "form-control",
                readonly = "readonly",
                value = result$url
            ),
            shiny::div(
                class = "cgv-share-result-actions",
                shiny::tags$button(
                    type = "button",
                    class = "btn btn-sm btn-primary",
                    onclick = "window.CGVSharedAnalysis && window.CGVSharedAnalysis.copyLatestUrl();",
                    shiny::icon("copy"),
                    "Copy link"
                ),
                shiny::tags$a(
                    href = result$url,
                    target = "_blank",
                    rel = "noopener noreferrer",
                    class = "btn btn-sm btn-default",
                    shiny::icon("arrow-up-right-from-square"),
                    "Open report"
                )
            ),
            shiny::p(class = "help-block", paste("Expires:", result$expires_at))
        )
    })

    output$share_analysis_preview_ui <- shiny::renderUI({
        preview <- state$preview %||% list()
        if (!length(preview)) return(NULL)
        capture_mode <- tolower(cgv_safe_scalar(input$share_capture_mode, "complete"))
        complete <- !identical(capture_mode, "fast")
        shiny::div(
            class = "cgv-share-preview",
            shiny::tags$h4("Content preview"),
            shiny::tags$ul(
                shiny::tags$li(sprintf(
                    "%d structural result(s) with included transcript selection",
                    as.integer(preview$structural %||% 0L)
                )),
                shiny::tags$li(if (complete) {
                    "Derived statistics and summary tables, including charts not opened yet"
                } else {
                    "Summary tables plus Analytics charts that are already rendered"
                }),
                shiny::tags$li(if (complete && isTRUE(preview$synteny_possible)) {
                    "Aligned synteny comparison, rendered even if it has not been opened yet"
                } else if (!complete && isTRUE(preview$synteny_possible)) {
                    "Aligned synteny only when its SVG is already available"
                } else {
                    "Aligned synteny is not available for the current result set"
                }),
                shiny::tags$li(sprintf(
                    "%d completed LASTZ and %d completed MultiPIP result(s)",
                    as.integer(preview$lastz %||% 0L),
                    as.integer(preview$multipip %||% 0L)
                )),
                shiny::tags$li(if (isTRUE(preview$figure_studio)) {
                    "Current Figure Studio composition"
                } else {
                    "Figure Studio is empty and will be omitted"
                }),
                shiny::tags$li("Recorded parameters, aliases, provenance, external results already opened, and organisms without a result")
            )
        )
    })

    ensure_reproducibility_package <- function() {
        existing <- shiny::isolate(state$package_path)
        if (nzchar(existing) && file.exists(existing)) return(existing)
        context <- shiny::isolate(state$package_context)
        if (!is.list(context)) {
            stop("The report package context is no longer available. Recreate the report and try again.")
        }
        package_path <- cgv_write_reproducibility_package(
            analysis = context$analysis,
            session_snapshot = context$snapshot,
            homo_summary = context$homo_summary,
            ortho_summary = context$ortho_summary,
            pip_runs = context$pip_runs,
            multipip_runs = context$multipip_runs,
            include_private = isTRUE(context$include_private),
            base_dir = base_dir,
            artifacts = context$artifacts
        )
        state$package_path <- package_path
        package_path
    }

    output$download_reproducibility_package <- shiny::downloadHandler(
        filename = function() {
            if (nzchar(state$package_path) && file.exists(state$package_path)) {
                basename(state$package_path)
            } else {
                "cgv_reproducibility.zip"
            }
        },
        content = function(file) {
            package_path <- ensure_reproducibility_package()
            if (!file.copy(package_path, file, overwrite = TRUE)) {
                stop("Could not prepare the ZIP download.")
            }
        },
        contentType = "application/zip"
    )

    output$download_interactive_report <- shiny::downloadHandler(
        filename = function() {
            paste0("cgv_interactive_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
        },
        content = function(file) {
            shiny::req(nzchar(state$desktop_html_path), file.exists(state$desktop_html_path))
            file.copy(state$desktop_html_path, file, overwrite = TRUE)
        },
        contentType = "text/html"
    )

    enqueue_background_snapshot <- function(email,
                                            include_multi_gene,
                                            include_cross_species,
                                            ttl_days = 7L,
                                            run_lastz = FALSE,
                                            include_private = FALSE,
                                            allow_downloads = FALSE,
                                            request_kind = "report",
                                            alignment_mode = "") {
        if (!cgv_background_reports_enabled()) {
            stop("Background report delivery is not enabled on this deployment.")
        }
        email <- trimws(cgv_safe_scalar(email))
        if (!feedback_is_valid_email(email)) {
            stop("Enter a valid email address for report delivery.")
        }
        now <- as.numeric(Sys.time())
        recent <- as.numeric(state$background_submissions %||% numeric(0))
        recent <- recent[is.finite(recent) & recent > now - 3600]
        if (length(recent) && now - max(recent) < 20) {
            stop("Please wait a few seconds before queueing another report.")
        }
        if (length(recent) >= 6L) {
            stop("This session has reached the background report limit. Please try again in one hour.")
        }
        snapshot <- cgv_scope_shared_snapshot(
            build_session_snapshot_fn(),
            include_multi_gene = isTRUE(include_multi_gene),
            include_cross_species = isTRUE(include_cross_species)
        )
        options <- list(
            capture_mode = "complete",
            include_multi_gene = isTRUE(include_multi_gene),
            include_cross_species = isTRUE(include_cross_species),
            include_private = isTRUE(include_private),
            allow_downloads = isTRUE(allow_downloads),
            ttl_days = suppressWarnings(as.integer(ttl_days %||% 7L)),
            run_lastz = isTRUE(run_lastz),
            request_kind = cgv_safe_scalar(request_kind, "report"),
            alignment_mode = cgv_safe_scalar(alignment_mode)
        )
        queued <- if (is.function(enqueue_background_report_fn)) {
            enqueue_background_report_fn(snapshot, email, options)
        } else {
            cgv_enqueue_background_report(snapshot, email, options, base_dir = base_dir)
        }
        state$background_submissions <- c(recent, now)
        queued
    }

    output$alignment_email_status_ui <- shiny::renderUI({
        if (isTRUE(state$alignment_busy)) {
            return(shiny::div(
                class = "cgv-share-progress",
                shiny::icon("spinner", class = "fa-spin"),
                shiny::span("Freezing the current analysis and placing it in the report queue…")
            ))
        }
        if (nzchar(state$alignment_error %||% "")) {
            return(shiny::div(
                class = "cgv-share-callout cgv-share-callout-error",
                shiny::icon("triangle-exclamation"),
                shiny::span(state$alignment_error)
            ))
        }
        result <- state$alignment_result
        if (!is.list(result)) return(NULL)
        shiny::div(
            class = "cgv-share-callout cgv-share-callout-success",
            shiny::icon("envelope-circle-check"),
            shiny::div(
                shiny::strong("Alignment report queued."),
                shiny::p("You can close this page. CGeV will email the complete interactive report when LASTZ and MultiPIP finish."),
                shiny::p(class = "help-block", paste("Delivery:", result$email)),
                shiny::p(class = "help-block", paste("Reference:", result$job_id))
            )
        )
    })

    show_alignment_email_dialog <- function(workflow = c("homo", "ortho"), mode = c("blocks", "multipip")) {
        workflow <- match.arg(workflow)
        mode <- match.arg(mode)
        if (cgv_runtime_is_desktop()) {
            shiny::showNotification(
                "Background email delivery is available in the CGeV web version. The Desktop app keeps analysis data on this computer.",
                type = "warning",
                duration = 10
            )
            return(invisible(NULL))
        }
        count <- if (identical(workflow, "homo")) {
            length(active_homo_ids_rv() %||% integer(0))
        } else {
            length(active_ortho_ids_rv() %||% integer(0))
        }
        if (count < 2L) {
            shiny::showNotification("At least two compatible results are required for an alignment report.", type = "warning")
            return(invisible(NULL))
        }
        state$alignment_request <- list(workflow = workflow, mode = mode)
        state$alignment_busy <- FALSE
        state$alignment_error <- ""
        state$alignment_result <- NULL
        workflow_label <- if (identical(workflow, "homo")) "Multi-Gene" else "Cross-Species"
        mode_label <- if (identical(mode, "multipip")) "MultiPIP" else "LASTZ blocks"
        shiny::showModal(shiny::modalDialog(
            title = shiny::div(
                class = "cgv-share-modal-title",
                shiny::icon("envelope-circle-check"),
                shiny::div(
                    shiny::span(class = "cgv-share-modal-kicker", "BACKGROUND DELIVERY"),
                    shiny::span("Email the complete alignment report")
                )
            ),
            size = "m",
            easyClose = TRUE,
            shiny::div(
                id = "cgv-share-modal-shell",
                class = "cgv-share-modal-shell cgv-alignment-email-modal",
                shiny::div(
                    class = "cgv-share-intro",
                    shiny::p(
                        "CGeV will freeze the current ", shiny::strong(workflow_label),
                        " analysis, run LASTZ and MultiPIP in a separate worker, and create the full interactive report."
                    ),
                    shiny::p(
                        class = "help-block",
                        paste0("Requested from the ", mode_label, " view. You can keep working or close this page after it enters the queue.")
                    )
                ),
                shiny::textInput(
                    "alignment_delivery_email",
                    "Email address",
                    placeholder = "name@example.org",
                    width = "100%"
                ),
                shiny::div(
                    class = "cgv-share-privacy-note",
                    shiny::icon("shield-halved"),
                    shiny::span("The email contains a private secret link valid for 7 days. Background delivery supports preloaded CGeV datasets only.")
                ),
                shiny::uiOutput("alignment_email_status_ui")
            ),
            footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton(
                    "queue_alignment_report_email",
                    "Email full report",
                    icon = shiny::icon("paper-plane"),
                    class = "btn btn-primary"
                )
            )
        ))
        invisible(NULL)
    }

    shiny::observeEvent(input$email_homo_pip_report, {
        show_alignment_email_dialog("homo", "blocks")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$email_homo_multipip_report, {
        show_alignment_email_dialog("homo", "multipip")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$email_ortho_pip_report, {
        show_alignment_email_dialog("ortho", "blocks")
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$email_ortho_multipip_report, {
        show_alignment_email_dialog("ortho", "multipip")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$queue_alignment_report_email, {
        if (isTRUE(state$alignment_busy) || is.list(state$alignment_result)) return(invisible(NULL))
        request <- shiny::isolate(state$alignment_request)
        if (!is.list(request)) return(invisible(NULL))
        email <- trimws(cgv_safe_scalar(input$alignment_delivery_email))
        state$alignment_busy <- TRUE
        state$alignment_error <- ""
        shinyjs::disable("queue_alignment_report_email")
        queued <- tryCatch(
            enqueue_background_snapshot(
                email = email,
                include_multi_gene = identical(request$workflow, "homo"),
                include_cross_species = identical(request$workflow, "ortho"),
                ttl_days = 7L,
                run_lastz = TRUE,
                include_private = FALSE,
                allow_downloads = FALSE,
                request_kind = "alignment",
                alignment_mode = request$mode
            ),
            error = function(e) e
        )
        state$alignment_busy <- FALSE
        if (inherits(queued, "error")) {
            state$alignment_error <- paste0("Could not queue the alignment report: ", conditionMessage(queued))
            shinyjs::enable("queue_alignment_report_email")
            return(invisible(NULL))
        }
        state$alignment_result <- list(
            job_id = cgv_safe_scalar(queued$id),
            email = cgv_safe_scalar(queued$email, email)
        )
        shiny::showNotification(
            "Alignment report queued. You may close CGeV; the interactive link will arrive by email.",
            type = "message",
            duration = 10
        )
        invisible(NULL)
    }, ignoreInit = TRUE)

    show_share_analysis_dialog <- function() {
        if (!isTRUE(has_results())) return(invisible(NULL))
        state$result <- NULL
        state$error <- ""
        state$missing <- character(0)
        state$pending <- NULL
        state$package_path <- ""
        state$package_context <- NULL
        state$desktop_html_path <- ""
        state$artifacts <- NULL
        state$busy_message <- ""
        preview_snapshot <- tryCatch(build_session_snapshot_fn(), error = function(e) list())
        preview_state <- preview_snapshot$app %||% list()
        studio <- tryCatch(
            jsonlite::fromJSON(cgv_safe_scalar(preview_state$figure_studio_state), simplifyVector = FALSE),
            error = function(e) list()
        )
        completed_count <- function(runs) {
            runs <- tryCatch(runs(), error = function(e) list())
            length(cgv_completed_alignment_runs(runs))
        }
        homo_synteny_available <- isTRUE(tryCatch(
            if (is.function(homo_synteny_available_fn)) homo_synteny_available_fn() else FALSE,
            error = function(e) FALSE
        ))
        # Uploaded/private data lives outside the portable data root, so its
        # source refs keep a file name but lose the relative path.
        plot_has_uploaded_source <- function(plot) {
            if (!is.list(plot)) return(FALSE)
            if (cgv_is_absolute_path(plot$annotation_path %||% "") ||
                cgv_is_absolute_path(plot$genome_path %||% "")) {
                return(TRUE)
            }
            refs <- list(plot$annotation_source, plot$genome_source)
            any(vapply(refs, function(ref) {
                is.list(ref) &&
                    nzchar(cgv_safe_scalar(ref$name)) &&
                    !nzchar(cgv_safe_scalar(ref$relative_path))
            }, logical(1)))
        }
        snapshot_plots <- c(
            (preview_snapshot$homologous %||% list())$plots %||% list(),
            (preview_snapshot$orthologous %||% list())$plots %||% list()
        )
        state$preview <- list(
            multi_gene = length(active_homo_ids_rv() %||% integer(0)),
            cross_species = length(active_ortho_ids_rv() %||% integer(0)),
            structural = length(active_homo_ids_rv() %||% integer(0)) +
                length(active_ortho_ids_rv() %||% integer(0)),
            lastz = completed_count(pip_runs_fn),
            multipip = completed_count(multipip_runs_fn),
            figure_studio = length(studio$panels %||% list()) > 0L,
            synteny_possible = length(active_ortho_ids_rv() %||% integer(0)) > 1L ||
                homo_synteny_available,
            lastz_eligible = length(active_ortho_ids_rv() %||% integer(0)) > 1L,
            has_private_uploads = any(vapply(snapshot_plots, plot_has_uploaded_source, logical(1)))
        )
        desktop <- cgv_runtime_is_desktop()
        background_available <- !desktop && cgv_background_reports_enabled()
        light_logo_src <- versioned_asset_path("favicon2.ico?v=2")
        dark_logo_src <- versioned_asset_path("favicon.ico?v=2")
        modal_logo_src <- if (identical(
            tolower(cgv_safe_scalar(input$app_theme, "light")),
            "dark"
        )) dark_logo_src else light_logo_src
        shiny::showModal(shiny::modalDialog(
            title = shiny::div(
                class = "cgv-share-modal-title",
                shiny::tags$img(
                    src = modal_logo_src,
                    `data-light-src` = light_logo_src,
                    `data-dark-src` = dark_logo_src,
                    alt = "CGeV logo",
                    class = "cgv-share-modal-logo"
                ),
                shiny::div(
                    shiny::span(class = "cgv-share-modal-kicker", if (desktop) "LOCAL EXPORT" else "READ-ONLY SNAPSHOT"),
                    shiny::span(if (desktop) "Export interactive analysis" else "Share analysis")
                )
            ),
            size = "l",
            easyClose = !isTRUE(state$busy),
            shiny::div(
                id = "cgv-share-modal-shell",
                class = "cgv-share-modal-shell",
                shiny::div(
                    class = "cgv-share-intro",
                    shiny::p(if (desktop) {
                        "Create a self-contained interactive report and reproducibility package. Nothing leaves this computer."
                    } else {
                        "Create an immutable, read-only view of the complete analysis at a private secret URL."
                    }),
                    shiny::div(
                        class = "cgv-share-counts",
                        shiny::span(shiny::icon("layer-group"), shiny::strong(length(active_homo_ids_rv() %||% integer(0))), " Multi-Gene"),
                        shiny::span(shiny::icon("earth-americas"), shiny::strong(length(active_ortho_ids_rv() %||% integer(0))), " Cross-Species")
                    )
                ),
                shiny::uiOutput("share_analysis_result_ui"),
                shiny::div(
                    class = "cgv-share-callout cgv-share-callout-warning",
                    shiny::icon("clock"),
                    shiny::strong("Report generation can take several minutes."),
                    shiny::p(
                        if (background_available) {
                            "Generate it in this session and keep the page open, or choose email delivery below to freeze the current analysis and continue working."
                        } else {
                            "This is one of CGeV's most intensive processes. Keep CGeV and this window open, and please wait until it finishes."
                        }
                    )
                ),
                if (background_available) shiny::div(
                    class = "cgv-share-options cgv-share-delivery-mode",
                    shiny::radioButtons(
                        "share_delivery_mode",
                        "Delivery",
                        choiceNames = list(
                            shiny::HTML("Generate here &mdash; keep this page open"),
                            shiny::HTML("Email me &mdash; continue working or close this page")
                        ),
                        choiceValues = c("session", "email"),
                        selected = "session"
                    ),
                    shiny::conditionalPanel(
                        condition = "input.share_delivery_mode == 'email'",
                        shiny::textInput(
                            "share_delivery_email",
                            "Email address",
                            placeholder = "name@example.org",
                            width = "100%"
                        ),
                        shiny::p(
                            class = "help-block",
                            "CGeV will freeze the current analysis, generate the complete interactive report in a separate worker, and email the secret link. Background delivery currently supports preloaded CGeV datasets."
                        )
                    )
                ) else NULL,
                shiny::div(
                    class = "cgv-share-options cgv-share-capture-mode",
                    shiny::radioButtons(
                        "share_capture_mode",
                        "Report detail",
                        choiceNames = list(
                            shiny::HTML("Complete &mdash; generate every available view (recommended)"),
                            shiny::HTML("Fast &mdash; include only views already rendered")
                        ),
                        choiceValues = c("complete", "fast"),
                        selected = "complete"
                    ),
                    shiny::conditionalPanel(
                        condition = "input.share_capture_mode == 'complete'",
                        shiny::p(
                            class = "help-block",
                            "CGeV will generate views that have not been opened. Depending on the number of results, this can take several minutes."
                        )
                    ),
                    shiny::conditionalPanel(
                        condition = "input.share_capture_mode == 'fast'",
                        shiny::p(
                            class = "help-block",
                            "CGeV will not activate hidden Analytics, structures, alignments or synteny views."
                        )
                    )
                ),
                if (isTRUE(state$preview$multi_gene > 0L) &&
                    isTRUE(state$preview$cross_species > 0L)) shiny::div(
                    class = "cgv-share-scope",
                    shiny::div(
                        class = "cgv-share-scope-heading",
                        shiny::strong("Results to include"),
                        shiny::span("Choose one workflow or keep both.")
                    ),
                    shiny::div(
                        class = "cgv-share-scope-options",
                        shiny::div(
                            class = "cgv-share-scope-option",
                            shiny::checkboxInput(
                                "share_include_multi_gene",
                                sprintf("Multi-Gene (%d results)", state$preview$multi_gene),
                                value = TRUE
                            )
                        ),
                        shiny::div(
                            class = "cgv-share-scope-option",
                            shiny::checkboxInput(
                                "share_include_cross_species",
                                sprintf("Cross-Species (%d results)", state$preview$cross_species),
                                value = TRUE
                            )
                        )
                    )
                ) else NULL,
                shiny::uiOutput("share_analysis_preview_ui"),
                shiny::div(
                    class = "cgv-share-options",
                    if (isTRUE(state$preview$has_private_uploads)) shiny::div(
                        class = "cgv-share-option",
                        shiny::checkboxInput(
                            "share_include_private",
                            "Include private or uploaded sequences",
                            value = FALSE
                        ),
                        shiny::p("Off by default. Assembly and annotation provenance remains available without complete genomes.")
                    ) else NULL,
                    if (!desktop) shiny::div(
                        class = "cgv-share-option",
                        shiny::checkboxInput(
                            "share_allow_downloads",
                            "Allow readers to download the reproducibility ZIP",
                            value = FALSE
                        ),
                        shiny::p("CSV, FASTA and the ZIP are not published while this option is off.")
                    ) else NULL,
                    if (isTRUE(state$preview$lastz_eligible)) {
                        lastz_option <- shiny::div(
                        class = "cgv-share-option cgv-share-option-lastz",
                        shiny::checkboxInput(
                            "share_run_lastz",
                            "Run LASTZ and MultiPIP before creating the report",
                            value = FALSE
                        ),
                        shiny::p(
                            "Optional and computationally intensive. CGeV will run the compatible selected Cross-Species comparison, which can add several minutes, and include the completed LASTZ and MultiPIP views."
                        )
                        )
                        if (isTRUE(state$preview$multi_gene > 0L) &&
                            isTRUE(state$preview$cross_species > 0L)) {
                            shiny::conditionalPanel(
                                condition = "input.share_capture_mode == 'complete' && input.share_include_cross_species",
                                lastz_option
                            )
                        } else {
                            shiny::conditionalPanel(
                                condition = "input.share_capture_mode == 'complete'",
                                lastz_option
                            )
                        }
                    } else NULL
                ),
                if (!desktop) shiny::div(
                    class = "cgv-share-expiry",
                    shiny::tags$label("Secret link expiry"),
                    shiny::radioButtons(
                        "share_ttl_days",
                        label = NULL,
                        choices = c("7 days" = 7L, "14 days" = 14L, "30 days" = 30L),
                        selected = 7L,
                        inline = TRUE
                    )
                ) else NULL,
                shiny::div(
                    class = "cgv-share-privacy-note",
                    shiny::icon("shield-halved"),
                    shiny::div(
                        shiny::strong("Privacy reminder"),
                        shiny::span(if (desktop) {
                            "Anyone who receives the exported HTML can copy information visible in it."
                        } else {
                            "Anyone with the secret URL can view and copy visible information until the link expires or is revoked."
                        })
                    )
                )
            ),
            footer = shiny::tagList(
                shiny::modalButton("Close"),
                shiny::conditionalPanel(
                    condition = "output.share_analysis_has_package",
                    shiny::downloadButton(
                        "download_reproducibility_package",
                        "Generate / download ZIP",
                        class = "btn btn-default",
                        onclick = "window.CGVSharedAnalysis && window.CGVSharedAnalysis.notifyZipStart();"
                    )
                ),
                shiny::conditionalPanel(
                    condition = "output.share_analysis_has_html",
                    shiny::downloadButton(
                        "download_interactive_report",
                        "Download HTML",
                        class = "btn btn-default"
                    )
                ),
                shiny::actionButton(
                    "publish_shared_analysis",
                    shiny::span(
                        class = "cgv-share-submit-label",
                        if (desktop) "Build files" else "Create secret link"
                    ),
                    icon = shiny::icon(if (desktop) "file-export" else "share-nodes"),
                    class = "btn btn-primary"
                )
            )
        ))
        invisible(NULL)
    }

    shiny::observeEvent(input$open_share_analysis_homo, {
        show_share_analysis_dialog()
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$open_share_analysis_ortho, {
        show_share_analysis_dialog()
    }, ignoreInit = TRUE)

    shiny::observe({
        homo_count <- length(active_homo_ids_rv() %||% integer(0))
        ortho_count <- length(active_ortho_ids_rv() %||% integer(0))
        if (homo_count > 0L && ortho_count > 0L &&
            !is.null(input$share_include_multi_gene) &&
            !is.null(input$share_include_cross_species)) {
            selected <- isTRUE(input$share_include_multi_gene) ||
                isTRUE(input$share_include_cross_species)
            if (selected) {
                shinyjs::enable("publish_shared_analysis")
            } else {
                shinyjs::disable("publish_shared_analysis")
            }
        }
    })

    output$share_analysis_has_package <- shiny::reactive({
        (nzchar(state$package_path) && file.exists(state$package_path)) ||
            is.list(state$package_context)
    })
    shiny::outputOptions(output, "share_analysis_has_package", suspendWhenHidden = FALSE)
    output$share_analysis_has_html <- shiny::reactive({
        nzchar(state$desktop_html_path) && file.exists(state$desktop_html_path)
    })
    shiny::outputOptions(output, "share_analysis_has_html", suspendWhenHidden = FALSE)

    arm_report_stage_timeout <- function(request_id, phase, seconds, message) {
        if (!requireNamespace("later", quietly = TRUE)) return(invisible(NULL))
        session_later(function() {
            current <- shiny::isolate(state$pending)
            if (isTRUE(shiny::isolate(state$busy)) &&
                is.list(current) &&
                identical(current$request_id, request_id) &&
                identical(current$phase, phase)) {
                session$sendCustomMessage("cgv:restore-report-modes", list(request_id = request_id))
                state$busy <- FALSE
                state$pending <- NULL
                state$error <- message
                if (isTRUE(background_bootstrap) && is.function(background_report_failed_fn)) {
                    try(background_report_failed_fn(message), silent = TRUE)
                }
            }
        }, delay = seconds)
        invisible(NULL)
    }

    request_browser_capture <- function(pending) {
        pending$phase <- "capture"
        state$pending <- pending
        report_perf_mark(pending, "browser_capture_requested")
        state$busy_message <- if (identical(pending$capture_mode, "fast")) {
            "Capturing the views that are already available…"
        } else {
            "Preparing structures and Analytics for the complete report…"
        }
        session$sendCustomMessage("cgv:capture-analysis-assets", list(
            request_id = pending$request_id,
            capture_mode = pending$capture_mode %||% "complete",
            max_total_bytes = 24L * 1024L * 1024L,
            analytics_contexts = pending$analytics_contexts %||% character(0),
            structural_targets = pending$structural_targets %||% list(),
            capture_contexts = pending$capture_contexts %||% character(0),
            include_global_assets = isTRUE(pending$include_global_assets)
        ))
        arm_report_stage_timeout(
            pending$request_id,
            "capture",
            55,
            "The browser did not finish capturing the report within 55 seconds. Nothing was published; try again after the current visualizations finish rendering."
        )
        invisible(NULL)
    }

    start_lastz_phase <- function(pending) {
        pending$phase <- "lastz"
        state$pending <- pending
        report_perf_mark(pending, "lastz_phase_started")
        state$busy_message <- "Running and capturing LASTZ and MultiPIP. This can take several minutes; keep CGeV open and please wait…"
        arm_report_stage_timeout(
            pending$request_id,
            "lastz",
            370,
            "LASTZ or MultiPIP did not finish within 6 minutes. Nothing was published; you can retry without running alignments or complete them from the analysis view first."
        )
        finish_lastz_phase <- function(lastz_result) {
            current_pending <- shiny::isolate(state$pending)
            if (is.null(current_pending) || !identical(
                cgv_safe_scalar(current_pending$request_id),
                cgv_safe_scalar(pending$request_id)
            )) {
                return(invisible(NULL))
            }
            pending_after_lastz <- current_pending
            pending_after_lastz$pre_capture_missing <- unique(as.character(
                unlist(lastz_result$missing %||% character(0), use.names = FALSE)
            ))
            pending_after_lastz$lastz_runs <- if (is.list(lastz_result$pip_runs)) {
                lastz_result$pip_runs
            } else {
                list()
            }
            state$pending <- pending_after_lastz
            session$sendCustomMessage("cgv:prepare-lastz-for-report", list(
                request_id = pending_after_lastz$request_id,
                contexts = pending_after_lastz$lastz_contexts,
                max_total_bytes = 24L * 1024L * 1024L,
                skip_run = is.function(run_lastz_fn),
                run_multipip = !isTRUE(lastz_result$multipip_prepared),
                capture_contexts = pending_after_lastz$capture_contexts %||% character(0)
            ))
            arm_report_stage_timeout(
                pending_after_lastz$request_id,
                "lastz",
                370,
                "LASTZ or MultiPIP did not finish within 6 minutes. Nothing was published; you can retry without running alignments or complete them from the analysis view first."
            )
            invisible(NULL)
        }
        fail_lastz_phase <- function(err) {
            finish_lastz_phase(list(
                missing = paste0("LASTZ: ", conditionMessage(err)),
                pip_runs = list(),
                multipip_prepared = FALSE
            ))
        }
        lastz_result <- if (is.function(run_lastz_fn)) {
            tryCatch(
                run_lastz_fn(pending$lastz_contexts),
                error = function(e) list(
                    missing = paste0("LASTZ: ", conditionMessage(e))
                )
            )
        } else {
            list()
        }
        if (promises::is.promise(lastz_result)) {
            lastz_result %...>% (session_guard(finish_lastz_phase)) %...!% (session_guard(fail_lastz_phase))
        } else {
            finish_lastz_phase(lastz_result)
        }
        invisible(NULL)
    }

    start_synteny_phase <- function(pending) {
        pending$phase <- "synteny"
        state$pending <- pending
        report_perf_mark(pending, "synteny_phase_started")
        synteny_views <- length(pending$homo_synteny_groups %||% list()) +
            as.integer("ortho" %in% (pending$synteny_contexts %||% character(0)))
        state$busy_message <- sprintf(
            "Rendering %d aligned synteny view(s) for the complete report. This can take several minutes; please wait…",
            synteny_views
        )
        session$sendCustomMessage("cgv:prepare-synteny-for-report", list(
            request_id = pending$request_id,
            contexts = pending$synteny_contexts,
            max_total_bytes = 24L * 1024L * 1024L,
            analytics_contexts = pending$analytics_contexts %||% character(0),
            structural_targets = pending$structural_targets %||% list(),
            capture_contexts = pending$capture_contexts %||% character(0),
            include_global_assets = isTRUE(pending$include_global_assets),
            homo_groups = pending$homo_synteny_groups %||% list(),
            homo_selected_group = pending$homo_selected_group %||% "",
            per_view_timeout_ms = 30000L,
            capture_after_synteny = TRUE
        ))
        arm_report_stage_timeout(
            pending$request_id,
            "synteny",
            max(100, 60 + 35 * synteny_views),
            "One or more aligned synteny views did not finish before the report timeout. Nothing was published; try again after the current analysis finishes updating."
        )
        invisible(NULL)
    }

    shiny::observeEvent(input$publish_shared_analysis, {
        if (isTRUE(state$busy)) return(invisible(NULL))
        desktop <- cgv_runtime_is_desktop()
        background_delivery <- !desktop && identical(
            tolower(cgv_safe_scalar(input$share_delivery_mode, "session")),
            "email"
        )
        capture_mode <- tolower(cgv_safe_scalar(input$share_capture_mode, "complete"))
        if (!capture_mode %in% c("complete", "fast")) capture_mode <- "complete"
        if (background_delivery) capture_mode <- "complete"
        complete_capture <- identical(capture_mode, "complete")
        ttl <- if (desktop) 7L else suppressWarnings(as.integer(input$share_ttl_days %||% 7L))
        homo_count <- length(active_homo_ids_rv() %||% integer(0))
        ortho_count <- length(active_ortho_ids_rv() %||% integer(0))
        has_both <- homo_count > 0L && ortho_count > 0L
        include_multi_gene <- homo_count > 0L &&
            (!has_both || isTRUE(input$share_include_multi_gene))
        include_cross_species <- ortho_count > 0L &&
            (!has_both || isTRUE(input$share_include_cross_species))
        if (!include_multi_gene && !include_cross_species) {
            state$error <- "Select at least one analysis workflow for the report."
            return(invisible(NULL))
        }
        if (background_delivery) {
            email <- trimws(cgv_safe_scalar(input$share_delivery_email))
            state$busy <- TRUE
            state$error <- ""
            state$result <- NULL
            state$busy_message <- "Saving an immutable analysis snapshot in the background queue…"
            queued <- tryCatch(
                enqueue_background_snapshot(
                    email = email,
                    include_multi_gene = include_multi_gene,
                    include_cross_species = include_cross_species,
                    include_private = isTRUE(input$share_include_private),
                    allow_downloads = isTRUE(input$share_allow_downloads),
                    ttl_days = ttl,
                    run_lastz = isTRUE(input$share_run_lastz),
                    request_kind = "report"
                ),
                error = function(e) e
            )
            state$busy <- FALSE
            if (inherits(queued, "error")) {
                state$error <- paste0("Could not queue the background report: ", conditionMessage(queued))
                return(invisible(NULL))
            }
            state$result <- list(
                background = TRUE,
                job_id = cgv_safe_scalar(queued$id),
                email = cgv_safe_scalar(queued$email, email)
            )
            shiny::showNotification(
                "Report queued. You can keep working or close CGeV; the interactive link will arrive by email.",
                type = "message",
                duration = 10
            )
            return(invisible(NULL))
        }
        lastz_contexts <- character(0)
        if (include_cross_species && ortho_count > 1L) lastz_contexts <- c(lastz_contexts, "ortho")
        homo_synteny_available <- complete_capture && isTRUE(tryCatch(
            if (is.function(homo_synteny_available_fn)) homo_synteny_available_fn() else FALSE,
            error = function(e) FALSE
        ))
        synteny_contexts <- character(0)
        homo_synteny_groups <- if (include_multi_gene && homo_synteny_available) {
            tryCatch(
                if (is.function(homo_synteny_groups_fn)) homo_synteny_groups_fn() else list(),
                error = function(e) list()
            )
        } else {
            list()
        }
        if (include_multi_gene && length(homo_synteny_groups)) {
            synteny_contexts <- c(synteny_contexts, "homo")
        }
        if (complete_capture && include_cross_species && ortho_count > 1L) {
            synteny_contexts <- c(synteny_contexts, "ortho")
        }
        capture_contexts <- c(
            if (include_multi_gene) "multi_gene",
            if (include_cross_species) "cross_species"
        )
        include_global_assets <- include_multi_gene && include_cross_species
        if (include_global_assets) {
            capture_contexts <- c(capture_contexts, "analysis", "figure_studio")
        }
        state$pending <- list(
            request_id = cgv_random_secret(12L),
            perf_run = if (exists("app_perf_new_run", mode = "function")) {
                app_perf_new_run("SHARED_REPORT")
            } else {
                NULL
            },
            include_multi_gene = include_multi_gene,
            include_cross_species = include_cross_species,
            include_private = isTRUE(input$share_include_private),
            allow_downloads = if (desktop) FALSE else isTRUE(input$share_allow_downloads),
            ttl_days = ttl,
            desktop = desktop,
            capture_mode = capture_mode,
            accept_missing = isTRUE(background_bootstrap),
            run_lastz = complete_capture &&
                isTRUE(input$share_run_lastz) &&
                length(lastz_contexts) > 0L,
            lastz_contexts = lastz_contexts,
            synteny_contexts = synteny_contexts,
            homo_synteny_groups = unname(homo_synteny_groups),
            homo_selected_group = cgv_safe_scalar(input$homo_aligned_gene_group),
            capture_contexts = capture_contexts,
            include_global_assets = include_global_assets,
            analytics_contexts = if (complete_capture) c(
                if (include_multi_gene) "homo",
                if (include_cross_species) "ortho"
            ) else character(0),
            structural_targets = list(
                homo = I(if (complete_capture && include_multi_gene) as.character(active_homo_ids_rv() %||% character(0)) else character(0)),
                ortho = I(if (complete_capture && include_cross_species) as.character(active_ortho_ids_rv() %||% character(0)) else character(0))
            ),
            pre_capture_missing = character(0),
            lastz_runs = list(),
            phase = "ready"
        )
        state$busy <- TRUE
        report_perf_mark(
            state$pending,
            sprintf(
                "publication_started mode=%s homo=%d ortho=%d public_zip=%d",
                capture_mode,
                homo_count,
                ortho_count,
                as.integer(isTRUE(state$pending$allow_downloads))
            )
        )
        state$error <- ""
        state$missing <- character(0)
        state$result <- NULL
        state$package_path <- ""
        state$package_context <- NULL
        state$artifacts <- NULL
        state$busy_message <- if (complete_capture) {
            "Preparing a complete report. This intensive process can take several minutes; keep CGeV open and please wait…"
        } else {
            "Building the report. This can take several minutes; keep CGeV open and please wait…"
        }
        if (isTRUE(state$pending$run_lastz)) {
            start_lastz_phase(state$pending)
        } else if (length(synteny_contexts) > 0L) {
            start_synteny_phase(state$pending)
        } else {
            request_browser_capture(state$pending)
        }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cgv_report_synteny_ready, {
        pending <- shiny::isolate(state$pending)
        payload <- input$cgv_report_synteny_ready %||% list()
        if (!is.list(pending) ||
            !identical(pending$phase, "synteny") ||
            !identical(cgv_safe_scalar(payload$request_id), pending$request_id)) {
            return(invisible(NULL))
        }
        synteny_missing <- unique(as.character(unlist(payload$missing %||% character(0), use.names = FALSE)))
        pending$pre_capture_missing <- unique(c(
            as.character(pending$pre_capture_missing %||% character(0)),
            synteny_missing[nzchar(synteny_missing)]
        ))
        state$pending <- pending
        if (isTRUE(pending$run_lastz)) {
            start_lastz_phase(pending)
        } else {
            request_browser_capture(pending)
        }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cgv_report_progress, {
        pending <- shiny::isolate(state$pending)
        progress <- input$cgv_report_progress %||% list()
        if (!is.list(pending) ||
            !identical(cgv_safe_scalar(progress$request_id), pending$request_id)) {
            return(invisible(NULL))
        }
        message <- substr(cgv_safe_scalar(progress$message), 1L, 240L)
        if (nzchar(message)) state$busy_message <- message
        invisible(NULL)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cgv_report_lastz_ready, {
        pending <- shiny::isolate(state$pending)
        payload <- input$cgv_report_lastz_ready %||% list()
        if (!is.list(pending) ||
            !identical(pending$phase, "lastz") ||
            !identical(cgv_safe_scalar(payload$request_id), pending$request_id)) {
            return(invisible(NULL))
        }
        lastz_missing <- unique(as.character(unlist(payload$missing %||% character(0), use.names = FALSE)))
        pending$pre_capture_missing <- unique(c(
            as.character(pending$pre_capture_missing %||% character(0)),
            lastz_missing[nzchar(lastz_missing)]
        ))
        state$pending <- pending
        if (length(pending$synteny_contexts %||% character(0)) > 0L) {
            start_synteny_phase(pending)
        } else {
            request_browser_capture(pending)
        }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cgv_analysis_assets, {
        pending <- shiny::isolate(state$pending)
        payload <- input$cgv_analysis_assets %||% list()
        if (!is.list(pending) || !identical(cgv_safe_scalar(payload$request_id), pending$request_id)) {
            return(invisible(NULL))
        }
        payload$missing <- unique(c(
            as.character(pending$pre_capture_missing %||% character(0)),
            as.character(unlist(payload$missing %||% character(0), use.names = FALSE))
        ))
        payload$capture_mode <- pending$capture_mode %||% "complete"
        report_perf_mark(
            pending,
            sprintf(
                "browser_capture_received figures=%d bytes=%.0f client_capture_ms=%.0f",
                length(payload$assets %||% list()),
                suppressWarnings(as.numeric(payload$captured_bytes %||% 0)),
                suppressWarnings(as.numeric((payload$timings %||% list())$client_capture_ms %||% 0))
            )
        )
        missing <- unique(as.character(unlist(payload$missing %||% character(0), use.names = FALSE)))
        missing <- missing[nzchar(missing)]
        if (length(missing) && !isTRUE(pending$accept_missing)) {
            state$busy <- FALSE
            state$missing <- missing
            return(invisible(NULL))
        }
        on.exit({
            session$sendCustomMessage("cgv:restore-report-modes", list(request_id = pending$request_id))
            state$busy <- FALSE
            state$pending <- NULL
            state$missing <- character(0)
        }, add = TRUE)
        tryCatch({
            state$busy_message <- "Building and validating the report…"
            snapshot <- cgv_scope_shared_snapshot(
                build_session_snapshot_fn(),
                include_multi_gene = pending$include_multi_gene,
                include_cross_species = pending$include_cross_species
            )
            payload <- cgv_scope_client_payload(
                payload,
                include_multi_gene = pending$include_multi_gene,
                include_cross_species = pending$include_cross_species
            )
            if (is.function(plot_chr_length_fn)) {
                for (section_name in c("homologous", "orthologous")) {
                    workflow <- if (identical(section_name, "homologous")) "multi_gene" else "cross_species"
                    plots <- (snapshot[[section_name]] %||% list())$plots %||% list()
                    snapshot[[section_name]]$plots <- lapply(plots, function(plot) {
                        if (!is.list(plot)) return(plot)
                        meta <- plot$plot_gene_meta %||% list()
                        existing <- suppressWarnings(as.numeric(meta$chromosome_length %||% NA_real_))
                        if (is.finite(existing) && existing > 0) return(plot)
                        len <- tryCatch(
                            suppressWarnings(as.numeric(plot_chr_length_fn(
                                workflow,
                                cgv_safe_scalar(plot$id),
                                cgv_safe_scalar(plot$chr_name)
                            ))),
                            error = function(e) NA_real_
                        )
                        if (is.finite(len) && len > 0) {
                            meta$chromosome_length <- len
                            plot$plot_gene_meta <- meta
                        }
                        plot
                    })
                }
            }
            homo_summary <- if (isTRUE(pending$include_multi_gene)) {
                tryCatch(homo_summary_fn(), error = function(e) data.frame())
            } else {
                data.frame()
            }
            ortho_summary <- if (isTRUE(pending$include_cross_species)) {
                tryCatch(ortho_summary_fn(), error = function(e) data.frame())
            } else {
                data.frame()
            }
            pip_runs <- cgv_scope_alignment_runs(
                tryCatch(pip_runs_fn(), error = function(e) list()),
                include_multi_gene = pending$include_multi_gene,
                include_cross_species = pending$include_cross_species
            )
            direct_lastz_runs <- cgv_scope_alignment_runs(
                pending$lastz_runs %||% list(),
                include_multi_gene = pending$include_multi_gene,
                include_cross_species = pending$include_cross_species
            )
            if (length(direct_lastz_runs)) {
                combined_runs <- c(pip_runs, direct_lastz_runs)
                combined_names <- names(combined_runs)
                if (!is.null(combined_names) && length(combined_names) == length(combined_runs)) {
                    combined_runs <- combined_runs[!duplicated(combined_names, fromLast = TRUE)]
                }
                pip_runs <- combined_runs
            }
            multipip_runs <- cgv_scope_alignment_runs(
                tryCatch(multipip_runs_fn(), error = function(e) list()),
                include_multi_gene = pending$include_multi_gene,
                include_cross_species = pending$include_cross_species
            )
            analysis <- cgv_build_analysis_manifest(
                snapshot = snapshot,
                homo_summary = homo_summary,
                ortho_summary = ortho_summary,
                pip_runs = pip_runs,
                multipip_runs = multipip_runs,
                client_payload = payload,
                include_private = pending$include_private,
                allow_downloads = pending$allow_downloads,
                ttl_days = pending$ttl_days,
                app_version = app_version,
                base_dir = base_dir
            )
            report_perf_mark(
                pending,
                sprintf(
                    "manifest_built figures=%d mode=%s",
                    length(analysis$figures %||% list()),
                    cgv_safe_scalar(analysis$capture$mode)
                )
            )
            artifacts <- cgv_prepare_report_artifacts(analysis)
            report_perf_mark(
                pending,
                sprintf(
                    "artifacts_prepared html_bytes=%d json_bytes=%d",
                    nchar(artifacts$html, type = "bytes"),
                    nchar(artifacts$json, type = "bytes")
                )
            )
            state$artifacts <- artifacts
            state$package_context <- list(
                analysis = analysis,
                snapshot = snapshot,
                homo_summary = homo_summary,
                ortho_summary = ortho_summary,
                pip_runs = pip_runs,
                multipip_runs = multipip_runs,
                include_private = pending$include_private,
                artifacts = artifacts
            )
            package_path <- ""
            if (isTRUE(pending$allow_downloads)) {
                state$busy_message <- "Building the public reproducibility ZIP before publication…"
                package_path <- ensure_reproducibility_package()
                report_perf_mark(
                    pending,
                    sprintf("public_zip_built bytes=%.0f", file.info(package_path)$size)
                )
            }

            if (isTRUE(pending$desktop)) {
                html_path <- tempfile("cgv-interactive-report-", fileext = ".html")
                writeLines(artifacts$html, html_path, useBytes = TRUE)
                state$desktop_html_path <- html_path
                state$result <- list(desktop = TRUE, expires_at = analysis$expires_at)
            } else {
                state$busy_message <- "Publishing the immutable report…"
                published <- cgv_publish_static_report(
                    analysis,
                    package_path = package_path,
                    allow_downloads = pending$allow_downloads,
                    base_dir = base_dir,
                    artifacts = artifacts
                )
                url <- paste0(cgv_public_base_url(session), "/share/", published$token, "/index.html")
                state$result <- list(
                    desktop = FALSE,
                    url = url,
                    token = published$token,
                    revoke_secret = published$revoke_secret,
                    expires_at = published$expires_at
                )
                session$sendCustomMessage("cgv:shared-report-created", state$result)
                if (isTRUE(background_bootstrap) && is.function(background_report_ready_fn)) {
                    try(background_report_ready_fn(state$result), silent = TRUE)
                }
            }
            report_perf_mark(pending, "publication_completed")
        }, error = function(e) {
            state$error <- paste0("Could not create the report: ", conditionMessage(e))
            if (isTRUE(background_bootstrap) && is.function(background_report_failed_fn)) {
                try(background_report_failed_fn(conditionMessage(e)), silent = TRUE)
            }
        })
        invisible(NULL)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$continue_shared_analysis_without_missing, {
        pending <- shiny::isolate(state$pending)
        if (!is.list(pending) || isTRUE(state$busy)) return(invisible(NULL))
        pending$accept_missing <- TRUE
        state$pending <- pending
        state$busy <- TRUE
        state$error <- ""
        request_browser_capture(pending)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel_shared_analysis_capture, {
        pending <- shiny::isolate(state$pending)
        if (is.list(pending)) {
            session$sendCustomMessage("cgv:restore-report-modes", list(request_id = pending$request_id))
        }
        state$busy <- FALSE
        state$pending <- NULL
        state$missing <- character(0)
        state$error <- ""
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$revoke_shared_report, {
        request <- input$revoke_shared_report %||% list()
        ok <- tryCatch(
            cgv_revoke_static_report(request$token, request$revoke_secret, base_dir),
            error = function(e) FALSE
        )
        session$sendCustomMessage("cgv:shared-report-revoked", list(
            token = cgv_safe_scalar(request$token),
            ok = isTRUE(ok)
        ))
    }, ignoreInit = TRUE)

    cleanup_loop <- function() {
        try(cgv_cleanup_shared_reports(base_dir), silent = TRUE)
        try(cgv_cleanup_reproducibility_packages(base_dir), silent = TRUE)
        if (!session$isClosed() && requireNamespace("later", quietly = TRUE)) {
            session_later(cleanup_loop, delay = 15 * 60)
        }
    }
    try(cgv_cleanup_shared_reports(base_dir), silent = TRUE)
    try(cgv_cleanup_reproducibility_packages(base_dir), silent = TRUE)
    if (requireNamespace("later", quietly = TRUE)) {
        session_later(cleanup_loop, delay = 15 * 60)
    }

    invisible(state)
}
