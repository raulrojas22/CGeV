# STRING-only projection of one plot's annotation subset. No genome-wide state,
# no changed parser, and no parsed attribute lists retained after preparation.
string_prepare_gff_identifiers <- function(file_data) {
    attrs <- as.character(file_data$V9)
    parsed_rows <- lapply(attrs, function(attr) {
        tryCatch(parse_gff_attributes(attr), error = function(e) list())
    })
    types <- tolower(trimws(as.character(file_data$V3)))
    gene_rows <- which(types == "gene")
    gene_attr <- if (length(gene_rows) > 0) attrs[gene_rows[1]] else ""
    gene_name <- tryCatch(extract_primary_gene_name(gene_attr), error = function(e) "")
    gene_id <- tryCatch(extract_primary_gene_id(gene_attr), error = function(e) "")

    # Keep the two original traversals and their independent error boundaries:
    # ordering and even partial results on malformed fields are significant.
    ids <- c()
    tryCatch({
        for (pa in parsed_rows) {
            for (key in c("gene", "gene_id", "locus_tag", "name", "protein_id", "id")) {
                v <- pa[[key]][1]
                if (!is.null(v) && !is.na(v) && nzchar(trimws(v))) {
                    clean_v <- trimws(v)
                    clean_v2 <- sub("^(gene|rna|mrna|cds|transcript)[:-]", "", clean_v, ignore.case = TRUE)
                    ids <- c(ids, clean_v, clean_v2)
                }
            }
            dbx_vals <- c(pa[["dbxref"]], pa[["db_xref"]])
            for (dbx in dbx_vals) {
                if (is.null(dbx) || is.na(dbx) || !nzchar(dbx)) next
                entries <- trimws(unlist(strsplit(as.character(dbx), ",", fixed = TRUE)))
                for (ent in entries) {
                    if (!nzchar(ent)) next
                    ids <- c(ids, ent)
                    parts <- strsplit(ent, ":", fixed = TRUE)[[1]]
                    if (length(parts) >= 2) {
                        ids <- c(ids, trimws(paste(parts[-1], collapse = ":")))
                    }
                }
            }
        }
    }, error = function(e) NULL)
    screen_ids <- unique(ids[nzchar(ids)])
    id_candidates <- c()
    tryCatch({
        for (parsed in parsed_rows) {
            pid_val <- parsed[["protein_id"]][1]
            if (!is.null(pid_val) && !is.na(pid_val) && nzchar(trimws(pid_val))) {
                id_candidates <- c(id_candidates, trimws(pid_val))
            }
            dbx_vals <- c(parsed[["dbxref"]], parsed[["db_xref"]])
            for (dbx in dbx_vals) {
                if (is.null(dbx) || is.na(dbx) || !nzchar(dbx)) next
                entries <- trimws(unlist(strsplit(as.character(dbx), ",", fixed = TRUE)))
                for (ent in entries) {
                    parts <- strsplit(ent, ":", fixed = TRUE)[[1]]
                    if (length(parts) >= 2) {
                        id_candidates <- c(id_candidates, trimws(paste(parts[-1], collapse = ":")))
                    }
                    id_candidates <- c(id_candidates, trimws(ent))
                }
            }
            syn_vals <- c(parsed[["gene_synonym"]], parsed[["gene_synonyms"]], parsed[["synonym"]])
            for (sv in syn_vals) {
                if (is.null(sv) || is.na(sv) || !nzchar(sv)) next
                syns <- trimws(unlist(strsplit(as.character(sv), ",", fixed = TRUE)))
                id_candidates <- c(id_candidates, syns[nzchar(syns)])
            }
            for (key in c("locus_tag", "name", "id")) {
                v <- parsed[[key]][1]
                if (!is.null(v) && !is.na(v) && nzchar(trimws(v))) {
                    id_candidates <- c(id_candidates, trimws(v))
                }
            }
        }
    }, error = function(e) NULL)

    list(gene_name = gene_name, gene_id = gene_id, screen_ids = screen_ids,
         id_candidates = id_candidates)
}

# Same screen extraction and error boundary as the original builder. A failed
# row terminates that plot's sweep, retaining the original partial result.
string_screen_ids_from_rows <- function(parsed_rows) {
    failed <- FALSE
    ids <- c()
    tryCatch({
        for (pa in parsed_rows) {
            for (key in c("gene", "gene_id", "locus_tag", "name", "protein_id", "id")) {
                v <- pa[[key]][1]
                if (!is.null(v) && !is.na(v) && nzchar(trimws(v))) {
                    clean_v <- trimws(v)
                    clean_v2 <- sub("^(gene|rna|mrna|cds|transcript)[:-]", "", clean_v, ignore.case = TRUE)
                    ids <- c(ids, clean_v, clean_v2)
                }
            }
            dbx_vals <- c(pa[["dbxref"]], pa[["db_xref"]])
            for (dbx in dbx_vals) {
                if (is.null(dbx) || is.na(dbx) || !nzchar(dbx)) next
                entries <- trimws(unlist(strsplit(as.character(dbx), ",", fixed = TRUE)))
                for (ent in entries) {
                    if (!nzchar(ent)) next
                    ids <- c(ids, ent)
                    parts <- strsplit(ent, ":", fixed = TRUE)[[1]]
                    if (length(parts) >= 2) {
                        ids <- c(ids, trimws(paste(parts[-1], collapse = ":")))
                    }
                }
            }
        }
    }, error = function(e) failed <<- TRUE)
    list(ids = unique(ids[nzchar(ids)]), failed = failed)
}

# Batch only the eight fields used by screen candidates. The fast path is
# deliberately restricted to ASCII GFF3 key=value tokens with ASCII URL escapes.
# GTF, encoded keys, extra '=', non-ASCII, NUL/high-byte escapes and malformed
# tokens use the unchanged scalar parser. This preserves its unusual fallbacks
# without paying per-key/per-value URLdecode overhead for ordinary GFF3 rows.
string_prepare_screen_rows <- function(attrs) {
    attrs <- as.character(attrs)
    if (!length(attrs)) return(list())
    fields <- c("gene", "gene_id", "locus_tag", "name", "protein_id", "id", "dbxref", "db_xref")
    fast <- !is.na(attrs) & grepl("^[ -~]*$", attrs) &
        !grepl("%00|%[89A-Fa-f][0-9A-Fa-f]", attrs)
    pieces <- strsplit(ifelse(fast, attrs, ""), ";", fixed = TRUE)
    tokens <- trimws(unlist(pieces, use.names = FALSE))
    rows <- rep.int(seq_along(attrs), lengths(pieces))
    nonempty <- !is.na(tokens) & nzchar(tokens)
    valid <- !is.na(tokens) & (!nonempty | grepl("^[A-Za-z_][A-Za-z_0-9.:-]*=[^=]*$", tokens))
    fast[unique(rows[!valid])] <- FALSE
    keep <- nonempty & fast[rows]
    tokens <- tokens[keep]
    rows <- rows[keep]
    keys <- tolower(sub("=.*$", "", tokens))
    wanted <- keys %in% fields
    keys <- keys[wanted]
    rows <- rows[wanted]
    values <- sub("^[^=]*=", "", tokens[wanted])
    # No malformed/high-byte decode in this batch can change the scalar
    # safe_url_decode error boundary. Empty batches make no decoder call.
    if (length(values)) values <- safe_url_decode(values)
    by_row <- split(seq_along(rows), factor(rows, levels = seq_along(attrs)))
    result <- lapply(seq_along(attrs), function(i) {
        if (fast[i]) {
            idx <- by_row[[i]]
            pa <- if (length(idx)) split(values[idx], keys[idx]) else list()
        } else {
            pa <- tryCatch(parse_gff_attributes(attrs[i]), error = function(e) list())
        }
        string_screen_ids_from_rows(list(pa))
    })
    attr(result, "fallback_rows") <- sum(!fast)
    result
}

# One instance per Shiny session. At most one current projection per active plot.
# Exact source equality avoids file timestamps/hash collisions and notices row
# order, coordinates, malformed values, organism and annotation-source changes.
new_string_annotation_state <- function() {
    entries <- new.env(parent = emptyenv())
    screens <- new.env(parent = emptyenv())
    list(
        get = function(key, source) {
            timing <- isTRUE(app_perf_enabled())
            if (timing) t0 <- app_perf_now()
            entry <- entries[[key]]
            reuse <- !is.null(entry) && identical(entry$source, source)
            if (!reuse) {
                screens[[key]] <- NULL
                entry <- list(source = source,
                              value = string_prepare_gff_identifiers(source$file_data))
                entries[[key]] <- entry
            }
            if (timing) {
                app_perf_mark_ms(NULL, "annotation_prepare_ms", app_perf_elapsed_ms(t0), "STRING")
                app_perf_mark(NULL, if (reuse) "annotation_reuse" else "annotation_materialized", "STRING")
            }
            entry$value
        },
        screen = function(sources) {
            timing <- isTRUE(app_perf_enabled())
            if (timing) t0 <- app_perf_now()
            result <- vector("list", length(sources))
            names(result) <- names(sources)
            missing <- character(0)
            for (key in names(sources)) {
                full <- entries[[key]]
                if (!is.null(full) && !identical(full$source, sources[[key]])) {
                    entries[[key]] <- NULL
                    full <- NULL
                }
                entry <- screens[[key]]
                if (!is.null(full) && identical(full$source, sources[[key]])) {
                    result[key] <- list(full$value$screen_ids)
                } else if (!is.null(entry) && identical(entry$source, sources[[key]])) {
                    result[key] <- list(entry$value)
                } else {
                    missing <- c(missing, key)
                }
            }
            # One request-scoped batch of unique raw rows across changed plots.
            # Shared rows are decoded/extracted once then mapped back in original
            # row order. Only each plot's compact IDs survive this call.
            raw_by_plot <- lapply(sources[missing], function(src) as.character(src$file_data$V9))
            attrs <- unique(unlist(raw_by_plot, use.names = FALSE))
            prepared <- string_prepare_screen_rows(attrs)
            for (key in missing) {
                row_ids <- match(raw_by_plot[[key]], attrs)
                failed <- vapply(prepared[row_ids], function(row) row$failed, logical(1))
                if (any(failed)) row_ids <- row_ids[seq_len(which(failed)[1L])]
                value <- unique(unlist(lapply(prepared[row_ids], function(row) row$ids), use.names = FALSE))
                screens[[key]] <- list(source = sources[[key]], value = value)
                result[key] <- list(value)
            }
            if (timing) {
                app_perf_mark_ms(NULL, "screen_prepare_ms", app_perf_elapsed_ms(t0), "STRING")
                app_perf_mark(NULL, sprintf("screen_materialized=%d screen_reused=%d screen_unique_rows=%d screen_fallback_rows=%d",
                    length(missing), length(sources) - length(missing), length(attrs),
                    attr(prepared, "fallback_rows") %||% 0L), "STRING")
            }
            result
        },
        prune = function(keys) {
            stale <- setdiff(ls(entries, all.names = TRUE), keys)
            if (length(stale)) rm(list = stale, envir = entries)
            stale <- setdiff(ls(screens, all.names = TRUE), keys)
            if (length(stale)) rm(list = stale, envir = screens)
            invisible(NULL)
        }
    )
}
