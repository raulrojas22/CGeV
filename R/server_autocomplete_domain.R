init_autocomplete_domain <- function(
    geneAutocompleteCache_rv,
    quickGeneAutocompleteCache_rv,
    globalGeneSuggestionSources_rv,
    session,
    warm_annotation_cache_fn = NULL,
    normalize_annotation_key_fn = NULL
) {
    quick_scan_tokens <- new.env(parent = emptyenv(), hash = TRUE)
    # Derived, session-local keys: never share uploaded annotation names between
    # sessions. Bound both entry count and bytes, as with the other search caches.
    normalized_choices_cache <- new.env(parent = emptyenv(), hash = TRUE)

    normalize_annotation_key_safe <- function(annotation_path) {
        if (is.function(normalize_annotation_key_fn)) {
            return(normalize_annotation_key_fn(annotation_path))
        }
        p <- as.character(annotation_path %||% "")
        if (!nzchar(p)) {
            return("")
        }
        normalizePath(p, winslash = "/", mustWork = FALSE)
    }

    gene_autocomplete_cache_key <- function(annotation_path) {
        p <- as.character(annotation_path %||% "")
        if (!nzchar(p) || !file.exists(p)) {
            return("")
        }
        tryCatch(gff_cache_key(p), error = function(e) normalize_annotation_key_safe(p))
    }

    trim_autocomplete_cache_map <- function(cache_map, max_entries = 24L) {
        if (!is.list(cache_map)) {
            return(list())
        }
        max_entries <- suppressWarnings(as.integer(max_entries))
        if (!is.finite(max_entries) || is.na(max_entries) || max_entries < 1L) {
            max_entries <- 24L
        }
        if (length(cache_map) <= max_entries) {
            return(cache_map)
        }
        keep <- tail(names(cache_map), max_entries)
        keep <- keep[!is.na(keep) & nzchar(keep)]
        if (length(keep) == 0L) {
            return(list())
        }
        cache_map[keep]
    }

    extract_autocomplete_name_lists <- function(attrs) {
        attrs <- as.character(attrs %||% "")
        gene_name <- stringr::str_match(
            attrs,
            '(?:^|;|\\t)\\s*gene_name\\s*[= ]\\s*"?([^;"\\t]+)'
        )[, 2]
        name_fallback <- stringr::str_match(attrs, "(?:^|;)\\s*Name=([^;]+)")[, 2]
        gene_field <- stringr::str_match(attrs, "(?:^|;)\\s*gene=([^;]+)")[, 2]
        alias_field <- stringr::str_match(attrs, "(?:^|;)\\s*Alias=([^;]+)")[, 2]
        synonym_field <- stringr::str_match(attrs, "(?:^|;)\\s*gene_synonym=([^;]+)")[, 2]

        gene_name <- sanitize_gene_display_name_batch(gene_name)
        name_fallback <- sanitize_gene_display_name_batch(name_fallback)
        gene_field <- sanitize_gene_display_name_batch(gene_field)
        alias_field <- sanitize_gene_display_name_batch(alias_field)
        synonym_field <- sanitize_gene_display_name_batch(synonym_field)

        has_primary <- !is.na(gene_name) & nzchar(trimws(gene_name))
        name_fallback[has_primary] <- NA_character_

        has_name_fb <- !is.na(name_fallback) & nzchar(trimws(name_fallback))
        gene_field[has_primary | has_name_fb] <- NA_character_

        has_gene <- !is.na(gene_field) & nzchar(trimws(gene_field))
        alias_field[has_primary | has_name_fb | has_gene] <- NA_character_

        primary <- gene_name[has_primary]
        fallback <- c(
            name_fallback[!is.na(name_fallback) & nzchar(trimws(name_fallback))],
            gene_field[!is.na(gene_field) & nzchar(trimws(gene_field))],
            alias_field[!is.na(alias_field) & nzchar(trimws(alias_field))]
        )

        has_synonym <- !is.na(synonym_field) & nzchar(trimws(synonym_field))
        if (any(has_synonym)) {
            synonym_vals <- synonym_field[has_synonym]
            synonym_split <- trimws(unlist(strsplit(synonym_vals, ",", fixed = TRUE)))
            synonym_split <- synonym_split[nzchar(synonym_split) & !is.na(synonym_split) & nchar(synonym_split) >= 2L]
            synonym_split <- sanitize_gene_display_name_batch(synonym_split)
            synonym_split <- synonym_split[!is.na(synonym_split)]
            fallback <- c(fallback, synonym_split)
        }

        list(
            primary = as.character(primary %||% character(0)),
            fallback = as.character(fallback %||% character(0))
        )
    }

    next_quick_scan_token <- function(input_id) {
        key <- as.character(input_id %||% "")
        if (!nzchar(key)) {
            return(0L)
        }
        current <- if (exists(key, envir = quick_scan_tokens, inherits = FALSE)) {
            suppressWarnings(as.integer(get(key, envir = quick_scan_tokens, inherits = FALSE)))
        } else {
            0L
        }
        if (!is.finite(current) || is.na(current) || current < 0L) {
            current <- 0L
        }
        next_val <- current + 1L
        assign(key, next_val, envir = quick_scan_tokens)
        next_val
    }

    quick_scan_token_is_current <- function(input_id, token) {
        key <- as.character(input_id %||% "")
        if (!nzchar(key)) {
            return(FALSE)
        }
        current <- if (exists(key, envir = quick_scan_tokens, inherits = FALSE)) {
            suppressWarnings(as.integer(get(key, envir = quick_scan_tokens, inherits = FALSE)))
        } else {
            NA_integer_
        }
        token_int <- suppressWarnings(as.integer(token %||% NA_integer_))
        is.finite(current) && is.finite(token_int) && identical(current, token_int)
    }

    get_gene_suggestions_for_annotation <- function(annotation_path, max_suggestions = 12000L) {
        p <- as.character(annotation_path %||% "")
        if (!nzchar(p) || !file.exists(p)) {
            return(character(0))
        }
        max_suggestions <- suppressWarnings(as.integer(max_suggestions))
        if (!is.finite(max_suggestions) || max_suggestions <= 0L) {
            max_suggestions <- 12000L
        }
        ckey <- gene_autocomplete_cache_key(p)
        if (!nzchar(ckey)) {
            return(character(0))
        }

        cache_map <- geneAutocompleteCache_rv()
        if (!is.null(cache_map[[ckey]])) {
            return(cache_map[[ckey]])
        }

        idx <- tryCatch(build_gff_gene_light_index(p), error = function(e) NULL)
        if (is.null(idx) || !is.list(idx) || is.null(idx$genes_df) || nrow(idx$genes_df) == 0) {
            cache_map[[ckey]] <- character(0)
            geneAutocompleteCache_rv(trim_autocomplete_cache_map(cache_map))
            return(character(0))
        }

        ac <- tryCatch(ensure_gff_autocomplete_cache(p, idx, base_dir = "."), error = function(e) NULL)
        suggestions <- if (is.list(ac)) as.character(ac$display %||% character(0)) else character(0)
        suggestions <- sanitize_autocomplete_choices(suggestions, max_total = max_suggestions)
        if (length(suggestions) > max_suggestions) suggestions <- suggestions[seq_len(max_suggestions)]

        cache_map[[ckey]] <- suggestions
        geneAutocompleteCache_rv(trim_autocomplete_cache_map(cache_map))
        suggestions
    }

    get_gene_suggestions_from_disk_index <- function(annotation_path, max_suggestions = 12000L) {
        p <- as.character(annotation_path %||% "")
        if (!nzchar(p) || !file.exists(p)) {
            return(NULL)
        }
        max_suggestions <- suppressWarnings(as.integer(max_suggestions))
        if (!is.finite(max_suggestions) || max_suggestions <= 0L) {
            max_suggestions <- 12000L
        }
        ckey <- gene_autocomplete_cache_key(p)
        if (!nzchar(ckey)) {
            return(NULL)
        }

        if (!isTRUE(app_env_flag("APP_FAST_ORGANISM_SYNC", TRUE))) {
            idx <- tryCatch(load_gff_index_from_disk(p, cache_kind = "gene_light", base_dir = "."), error = function(e) NULL)
            if (is.null(idx) || !is.list(idx) || is.null(idx$genes_df) || !is.data.frame(idx$genes_df)) {
                return(NULL)
            }
            if (exists("cache_env_set", mode = "function") &&
                exists(".gff_gene_light_index_cache", inherits = TRUE) &&
                exists("annotation_memory_cache_limits", inherits = TRUE)) {
                tryCatch(
                    cache_env_set(
                        .gff_gene_light_index_cache,
                        gff_cache_key(p),
                        idx,
                        max_size = annotation_memory_cache_limits$gene_light_max_entries,
                        max_bytes = annotation_memory_cache_limits$gene_light_max_bytes
                    ),
                    error = function(e) NULL
                )
            }
            attrs <- as.character(idx$genes_df$attributes %||% rep("", nrow(idx$genes_df)))
            suggestions <- extract_partial_gene_display_names(attrs)
            suggestions <- sanitize_autocomplete_choices(suggestions, max_total = max_suggestions)
            cache_map <- geneAutocompleteCache_rv()
            cache_map[[ckey]] <- suggestions
            geneAutocompleteCache_rv(trim_autocomplete_cache_map(cache_map))
            return(suggestions)
        }

        ac <- tryCatch(load_gff_autocomplete_cache(p, base_dir = "."), error = function(e) NULL)
        if (!is.list(ac)) {
            idx <- tryCatch(load_gff_index_from_disk(p, cache_kind = "gene_light", base_dir = "."), error = function(e) NULL)
            if (is.null(idx) || !is.list(idx) || is.null(idx$genes_df) || !is.data.frame(idx$genes_df)) {
                return(NULL)
            }
            ac <- tryCatch(ensure_gff_autocomplete_cache(p, idx, base_dir = "."), error = function(e) NULL)
        }
        if (!is.list(ac)) {
            return(NULL)
        }
        suggestions <- as.character(ac$display %||% character(0))
        suggestions <- sanitize_autocomplete_choices(suggestions, max_total = max_suggestions)
        if (length(suggestions) > max_suggestions) {
            suggestions <- suggestions[seq_len(max_suggestions)]
        }

        cache_map <- geneAutocompleteCache_rv()
        cache_map[[ckey]] <- suggestions
        geneAutocompleteCache_rv(trim_autocomplete_cache_map(cache_map))
        suggestions
    }

    get_cached_quick_gene_suggestions <- function(annotation_path, max_suggestions = 1200L, max_lines = 180000L, max_seconds = 0.45) {
        ckey <- gene_autocomplete_cache_key(annotation_path)
        if (!nzchar(ckey)) {
            return(NULL)
        }
        cache_map <- quickGeneAutocompleteCache_rv()
        entry <- cache_map[[ckey]]
        if (!is.list(entry) || is.null(entry$suggestions)) {
            return(NULL)
        }
        entry_cap <- suppressWarnings(as.integer(entry$max_suggestions %||% 0L))
        entry_lines <- suppressWarnings(as.integer(entry$max_lines %||% 0L))
        entry_seconds <- suppressWarnings(as.numeric(entry$max_seconds %||% 0))
        if (!is.finite(entry_cap) || !is.finite(entry_lines) || !is.finite(entry_seconds)) {
            return(NULL)
        }
        if (entry_cap < as.integer(max_suggestions) || entry_lines < as.integer(max_lines) || entry_seconds + 1e-9 < as.numeric(max_seconds)) {
            return(NULL)
        }
        suggestions <- as.character(entry$suggestions %||% character(0))
        if (length(suggestions) > max_suggestions) {
            suggestions <- suggestions[seq_len(max_suggestions)]
        }
        suggestions
    }

    store_cached_quick_gene_suggestions <- function(annotation_path, suggestions, max_suggestions = 1200L, max_lines = 180000L, max_seconds = 0.45) {
        ckey <- gene_autocomplete_cache_key(annotation_path)
        if (!nzchar(ckey)) {
            return(as.character(suggestions %||% character(0)))
        }
        clean_suggestions <- sanitize_autocomplete_choices(suggestions, max_total = max_suggestions)
        cache_map <- quickGeneAutocompleteCache_rv()
        prev <- cache_map[[ckey]]
        prev_score <- if (is.list(prev)) {
            c(
                suppressWarnings(as.integer(prev$max_suggestions %||% 0L)),
                suppressWarnings(as.integer(prev$max_lines %||% 0L)),
                suppressWarnings(as.numeric(prev$max_seconds %||% 0)),
                length(as.character(prev$suggestions %||% character(0)))
            )
        } else {
            c(0L, 0L, 0, 0L)
        }
        next_score <- c(
            suppressWarnings(as.integer(max_suggestions %||% 0L)),
            suppressWarnings(as.integer(max_lines %||% 0L)),
            suppressWarnings(as.numeric(max_seconds %||% 0)),
            length(clean_suggestions)
        )
        if (!is.list(prev) || any(next_score > prev_score)) {
            cache_map[[ckey]] <- list(
                suggestions = clean_suggestions,
                max_suggestions = as.integer(max_suggestions %||% length(clean_suggestions)),
                max_lines = as.integer(max_lines %||% 0L),
                max_seconds = as.numeric(max_seconds %||% 0),
                updated_at = as.numeric(Sys.time())
            )
            quickGeneAutocompleteCache_rv(trim_autocomplete_cache_map(cache_map, max_entries = 48L))
        }
        clean_suggestions
    }

    autocomplete_keys_for_choices <- function(annotation_path, choices, cache_key = NULL) {
        vals <- as.character(choices %||% character(0))
        if (length(vals) == 0L) {
            return(character(0))
        }
        ckey <- cache_key %||% gene_autocomplete_cache_key(annotation_path)
        cached <- cache_env_get(normalized_choices_cache, ckey, default = NULL)
        # A quick scan can grow or replace choices without changing the source
        # file. Compare contents and order as well as the file-version key.
        if (is.list(cached) && identical(cached$choices, vals)) {
            return(cached$keys)
        }
        ac <- tryCatch(load_gff_autocomplete_cache(annotation_path, base_dir = "."), error = function(e) NULL)
        keys <- if (is.list(ac) && length(ac$display) >= length(vals) &&
            length(ac$keys) >= length(vals) && !any(grepl("%", vals, fixed = TRUE)) &&
            identical(vals, as.character(ac$display[seq_len(length(vals))]))) {
            as.character(ac$keys[seq_len(length(vals))])
        } else {
            as.character(normalize_partial_gene_choices(vals))
        }
        cache_env_set(normalized_choices_cache, ckey, list(choices = vals, keys = keys),
                      max_size = 24L, max_bytes = 8 * 1024^2)
        keys
    }

    aggregate_shared_gene_suggestions <- function(suggestions_by_path, keys_by_path = NULL, min_shared_organisms = 1L, max_total = 20000L) {
        min_shared <- suppressWarnings(as.integer(min_shared_organisms %||% 1L))
        if (!is.finite(min_shared) || is.na(min_shared) || min_shared < 1L) {
            min_shared <- 1L
        }
        if (min_shared <= 1L) {
            return(sanitize_autocomplete_choices(unique(unlist(suggestions_by_path, use.names = FALSE)), max_total = max_total))
        }
        if (!is.list(suggestions_by_path) || length(suggestions_by_path) == 0L) {
            return(character(0))
        }

        prepared <- lapply(seq_along(suggestions_by_path), function(i) {
            vals <- sanitize_autocomplete_choices(suggestions_by_path[[i]], max_total = max_total)
            if (length(vals) == 0L) {
                return(NULL)
            }
            supplied_keys <- if (is.list(keys_by_path) && length(keys_by_path) >= i) {
                as.character(keys_by_path[[i]] %||% character(0))
            } else {
                character(0)
            }
            keys <- if (length(supplied_keys) == length(vals)) {
                supplied_keys
            } else if (exists("normalize_partial_gene_query", mode = "function")) {
                normalize_partial_gene_choices(vals)
            } else if (exists("normalize_gene_compact", mode = "function")) {
                tolower(vapply(vals, normalize_gene_compact, character(1)))
            } else {
                tolower(gsub("[^[:alnum:]]+", "", vals, perl = TRUE))
            }
            keep <- !is.na(keys) & nzchar(keys)
            vals <- vals[keep]
            keys <- keys[keep]
            if (length(vals) == 0L) {
                return(NULL)
            }
            list(names = vals, keys = keys)
        })
        prepared <- Filter(Negate(is.null), prepared)
        if (length(prepared) == 0L) {
            return(character(0))
        }
        key_counts <- table(unlist(lapply(prepared, function(x) unique(x$keys)), use.names = FALSE))
        kept_keys <- names(key_counts)[as.integer(key_counts) >= min_shared]
        if (length(kept_keys) == 0L) {
            return(character(0))
        }
        all_keys <- unlist(lapply(prepared, `[[`, "keys"), use.names = FALSE)
        all_names <- unlist(lapply(prepared, `[[`, "names"), use.names = FALSE)
        keep <- all_keys %in% kept_keys
        names_by_key <- split(
            all_names[keep],
            factor(all_keys[keep], levels = kept_keys)
        )
        display <- vapply(names_by_key, function(values) {
            name_tab <- sort(table(values), decreasing = TRUE)
            names(name_tab)[[1L]]
        }, character(1))
        if (length(display) == 0L) {
            return(character(0))
        }
        source_count <- as.integer(key_counts[names(display)])
        ord <- order(-source_count, nchar(display), tolower(display))
        sanitize_autocomplete_choices(unname(display[ord]), max_total = max_total)
    }

    init_quick_gene_scan_state <- function(annotation_path, max_suggestions = 1200L, max_lines = 180000L, max_seconds = 0.45, chunk_lines = 1000L) {
        p <- as.character(annotation_path %||% "")
        if (!nzchar(p) || !file.exists(p)) {
            return(NULL)
        }
        max_suggestions <- suppressWarnings(as.integer(max_suggestions))
        if (!is.finite(max_suggestions) || max_suggestions <= 0L) {
            max_suggestions <- 1200L
        }
        max_lines <- suppressWarnings(as.integer(max_lines))
        if (!is.finite(max_lines) || max_lines <= 0L) {
            max_lines <- 180000L
        }
        max_seconds <- suppressWarnings(as.numeric(max_seconds))
        if (!is.finite(max_seconds) || max_seconds <= 0) {
            max_seconds <- 0.45
        }
        chunk_lines <- suppressWarnings(as.integer(chunk_lines))
        if (!is.finite(chunk_lines) || chunk_lines <= 0L) {
            chunk_lines <- 1000L
        }

        con <- if (grepl("\\.(gz|bgz)$", p, ignore.case = TRUE)) {
            gzfile(p, open = "rt")
        } else {
            file(p, open = "rt")
        }
        state <- new.env(parent = emptyenv())
        state$annotation_path <- p
        state$con <- con
        state$max_suggestions <- max_suggestions
        state$max_lines <- max_lines
        state$max_seconds <- max_seconds
        state$chunk_lines <- chunk_lines
        state$started_at <- Sys.time()
        state$lines_seen <- 0L
        state$out_primary <- character(0)
        state$out_fallback <- character(0)
        state$done <- FALSE
        state
    }

    close_quick_gene_scan_state <- function(state) {
        if (is.null(state) || !is.environment(state)) {
            return(invisible(FALSE))
        }
        if (!is.null(state$con)) {
            try(close(state$con), silent = TRUE)
            state$con <- NULL
        }
        state$done <- TRUE
        invisible(TRUE)
    }

    quick_gene_scan_state_suggestions <- function(state) {
        if (is.null(state) || !is.environment(state)) {
            return(character(0))
        }
        out <- unique(c(
            as.character(state$out_primary %||% character(0)),
            as.character(state$out_fallback %||% character(0))
        ))
        max_suggestions <- suppressWarnings(as.integer(state$max_suggestions %||% length(out)))
        if (is.finite(max_suggestions) && max_suggestions > 0L && length(out) > max_suggestions) {
            out <- out[seq_len(max_suggestions)]
        }
        out
    }

    step_quick_gene_scan_state <- function(state) {
        if (is.null(state) || !is.environment(state) || isTRUE(state$done) || is.null(state$con)) {
            return(list(done = TRUE, suggestions = quick_gene_scan_state_suggestions(state)))
        }
        chunk <- readLines(state$con, n = as.integer(state$chunk_lines %||% 1000L), warn = FALSE)
        if (length(chunk) == 0) {
            state$done <- TRUE
            return(list(done = TRUE, suggestions = quick_gene_scan_state_suggestions(state)))
        }
        state$lines_seen <- as.integer(state$lines_seen %||% 0L) + length(chunk)
        chunk <- chunk[nzchar(chunk) & !startsWith(chunk, "#")]
        if (length(chunk) > 0) {
            gene_lines <- chunk[grepl("^[^\t]+\t[^\t]*\tgene\t", chunk, perl = TRUE)]
            if (length(gene_lines) > 0) {
                attrs <- sub("^(?:[^\t]*\t){8}", "", gene_lines, perl = TRUE)
                names_lists <- extract_autocomplete_name_lists(attrs)
                if (length(names_lists$primary) > 0) {
                    state$out_primary <- unique(c(state$out_primary, names_lists$primary))
                }
                if (length(names_lists$fallback) > 0) {
                    state$out_fallback <- unique(c(state$out_fallback, names_lists$fallback))
                }
                out_merged <- unique(c(state$out_primary, state$out_fallback))
                max_suggestions <- as.integer(state$max_suggestions %||% length(out_merged))
                if (is.finite(max_suggestions) && max_suggestions > 0L && length(out_merged) >= max_suggestions) {
                    out_merged <- out_merged[seq_len(max_suggestions)]
                    state$out_primary <- out_merged
                    state$out_fallback <- character(0)
                    state$done <- TRUE
                }
            }
        }
        elapsed <- as.numeric(difftime(Sys.time(), state$started_at, units = "secs"))
        if (as.integer(state$lines_seen %||% 0L) >= as.integer(state$max_lines %||% 0L) ||
            elapsed >= as.numeric(state$max_seconds %||% 0)) {
            state$done <- TRUE
        }
        list(done = isTRUE(state$done), suggestions = quick_gene_scan_state_suggestions(state))
    }

    extract_quick_gene_suggestions <- function(annotation_path, max_suggestions = 1200L, max_lines = 180000L, max_seconds = 0.45, chunk_lines = 1000L) {
        state <- init_quick_gene_scan_state(
            annotation_path = annotation_path,
            max_suggestions = max_suggestions,
            max_lines = max_lines,
            max_seconds = max_seconds,
            chunk_lines = chunk_lines
        )
        if (is.null(state)) {
            return(character(0))
        }
        on.exit(close_quick_gene_scan_state(state), add = TRUE)
        repeat {
            step <- step_quick_gene_scan_state(state)
            if (isTRUE(step$done)) {
                break
            }
        }
        quick_gene_scan_state_suggestions(state)
    }

    schedule_quick_gene_autocomplete_scan <- function(input_id, quick_specs, request_token, max_total = 20000L, delay_sec = 0.02,
                                                      min_shared_organisms = 1L,
                                                      initial_suggestions_by_path = NULL,
                                                      initial_keys_by_path = NULL) {
        if (!requireNamespace("later", quietly = TRUE)) {
            return(invisible(FALSE))
        }
        specs <- quick_specs
        if (!is.list(specs) || length(specs) == 0L) {
            return(invisible(FALSE))
        }
        scan_pairs <- lapply(specs, function(spec) {
            state <- init_quick_gene_scan_state(
                annotation_path = spec$path,
                max_suggestions = spec$max_suggestions,
                max_lines = spec$max_lines,
                max_seconds = spec$max_seconds,
                chunk_lines = spec$chunk_lines %||% 1000L
            )
            if (is.null(state)) {
                return(NULL)
            }
            list(state = state, spec = spec)
        })
        scan_pairs <- Filter(Negate(is.null), scan_pairs)
        if (length(scan_pairs) == 0L) {
            return(invisible(FALSE))
        }
        max_total <- suppressWarnings(as.integer(max_total))
        if (!is.finite(max_total) || max_total <= 0L) {
            max_total <- 20000L
        }
        min_shared <- suppressWarnings(as.integer(min_shared_organisms %||% 1L))
        if (!is.finite(min_shared) || is.na(min_shared) || min_shared < 1L) {
            min_shared <- 1L
        }
        delay_val <- suppressWarnings(as.numeric(delay_sec))
        if (!is.finite(delay_val) || delay_val < 0) {
            delay_val <- 0.02
        }
        suggestions_by_path <- if (is.list(initial_suggestions_by_path)) initial_suggestions_by_path else list()
        keys_by_path <- if (is.list(initial_keys_by_path)) initial_keys_by_path else list()
        if (length(keys_by_path) < length(suggestions_by_path)) {
            length(keys_by_path) <- length(suggestions_by_path)
        }
        current_idx <- 1L
        publish_accumulated <- function() {
            choices <- aggregate_shared_gene_suggestions(
                suggestions_by_path,
                keys_by_path = keys_by_path,
                min_shared_organisms = min_shared,
                max_total = max_total
            )
            publish_gene_autocomplete(
                input_id = input_id,
                choices = choices,
                source_id = input_id,
                max_total = max_total
            )
            invisible(NULL)
        }
        run_next <- NULL
        run_next <- function() {
            if (!quick_scan_token_is_current(input_id, request_token)) {
                invisible(lapply(scan_pairs, function(pair) close_quick_gene_scan_state(pair$state)))
                return(invisible(FALSE))
            }
            if (current_idx > length(scan_pairs)) {
                return(invisible(TRUE))
            }
            pair <- scan_pairs[[current_idx]]
            state <- pair$state
            step <- step_quick_gene_scan_state(state)
            if (isTRUE(step$done)) {
                spec <- pair$spec
                suggestions <- store_cached_quick_gene_suggestions(
                    spec$path,
                    step$suggestions,
                    max_suggestions = spec$max_suggestions,
                    max_lines = spec$max_lines,
                    max_seconds = spec$max_seconds
                )
                if (length(suggestions) > 0) {
                    path_key <- normalize_annotation_key_safe(spec$path)
                    list_names <- names(suggestions_by_path)
                    slot <- if (length(list_names) > 0L) match(path_key, list_names) else NA_integer_
                    if (!is.finite(slot) || is.na(slot)) {
                        slot <- length(suggestions_by_path) + 1L
                    }
                    suggestions_by_path[[slot]] <<- suggestions
                    keys_by_path[[slot]] <<- autocomplete_keys_for_choices(spec$path, suggestions)
                    suggestion_names <- names(suggestions_by_path)
                    key_names <- names(keys_by_path)
                    if (length(suggestion_names) < slot) length(suggestion_names) <- slot
                    if (length(key_names) < slot) length(key_names) <- slot
                    suggestion_names[slot] <- path_key
                    key_names[slot] <- path_key
                    names(suggestions_by_path) <<- suggestion_names
                    names(keys_by_path) <<- key_names
                    current_choices <- aggregate_shared_gene_suggestions(
                        suggestions_by_path,
                        keys_by_path = keys_by_path,
                        min_shared_organisms = min_shared,
                        max_total = max_total
                    )
                    if (min_shared <= 1L || length(current_choices) > 0L) {
                        publish_accumulated()
                    }
                }
                close_quick_gene_scan_state(state)
                current_idx <<- current_idx + 1L
            }
            later::later(run_next, delay = delay_val)
            invisible(TRUE)
        }
        later::later(run_next, delay = delay_val)
        invisible(TRUE)
    }

    publish_gene_autocomplete <- function(input_id, choices, source_id = NULL, max_total = 20000L) {
        target_id <- as.character(input_id %||% "")
        if (!nzchar(target_id)) {
            return(invisible(NULL))
        }
        source_key <- as.character(source_id %||% target_id)
        cleaned <- sanitize_autocomplete_choices(choices, max_total = max_total)

        session$sendCustomMessage(
            "update_gene_autocomplete",
            list(input_id = target_id, choices = cleaned)
        )

        src <- globalGeneSuggestionSources_rv()
        if (!is.list(src)) src <- list()
        src[[source_key]] <- cleaned
        globalGeneSuggestionSources_rv(src)

        all_global <- unique(unlist(src, use.names = FALSE))
        all_global <- sanitize_autocomplete_choices(all_global, max_total = 15000L)
        session$sendCustomMessage(
            "update_gene_autocomplete",
            list(input_id = "global_search_query", choices = all_global)
        )
        invisible(cleaned)
    }

    update_gene_autocomplete <- function(input_id, annotation_paths, status_rv = NULL, max_files = Inf, max_total = 20000L, allow_build = TRUE, allow_quick_scan = TRUE, allow_disk_index = TRUE, min_shared_organisms = 1L) {
        auto_perf <- app_perf_new_run(sprintf("AUTO-%s", as.character(input_id %||% "input")))
        min_shared <- suppressWarnings(as.integer(min_shared_organisms %||% 1L))
        if (!is.finite(min_shared) || is.na(min_shared) || min_shared < 1L) {
            min_shared <- 1L
        }
        app_perf_mark(
            auto_perf,
            sprintf(
                "start allow_build=%s allow_quick_scan=%s allow_disk_index=%s",
                as.character(isTRUE(allow_build)),
                as.character(isTRUE(allow_quick_scan)),
                as.character(isTRUE(allow_disk_index))
            ),
            "AUTO"
        )
        paths <- unique(as.character(annotation_paths %||% character(0)))
        paths <- paths[nzchar(paths) & file.exists(paths)]
        app_perf_mark(auto_perf, sprintf("paths normalized n=%d", as.integer(length(paths))), "AUTO")
        max_total <- suppressWarnings(as.integer(max_total))
        if (!is.finite(max_total) || max_total <= 0L) {
            max_total <- 20000L
        }

        if (length(paths) == 0) {
            next_quick_scan_token(input_id)
            publish_gene_autocomplete(
                input_id = input_id,
                choices = character(0),
                source_id = input_id,
                max_total = max_total
            )
            app_perf_mark(auto_perf, "no valid paths -> empty choices", "AUTO")
            return(invisible(list(
                choices = character(0),
                unresolved_paths = character(0),
                complete = TRUE
            )))
        }

        if (is.finite(max_files) && length(paths) > as.integer(max_files)) {
            paths <- paths[seq_len(as.integer(max_files))]
        }
        # A union autocomplete may be sampled for responsiveness, but a shared
        # Cross-Species result must include every selected organism.
        if (!isTRUE(allow_build) && min_shared <= 1L && length(paths) > 12L) {
            paths <- paths[seq_len(12L)]
        }
        app_perf_mark(auto_perf, sprintf("paths after caps n=%d", as.integer(length(paths))), "AUTO")

        request_token <- next_quick_scan_token(input_id)
        cache_map_check <- geneAutocompleteCache_rv()
        all_suggestions <- character(0)
        suggestions_by_path <- list()
        keys_by_path <- list()
        quick_scan_specs <- list()
        unresolved_paths <- character(0)
        last_published_key <- "\001__unset__"
        path_count <- max(length(paths), 1L)
        per_path_cap <- as.integer(max(600L, min(10000L, ceiling(max_total / path_count))))

        publish_current_suggestions <- function(reason = "partial") {
            current_choices <- aggregate_shared_gene_suggestions(
                suggestions_by_path,
                keys_by_path = keys_by_path,
                min_shared_organisms = min_shared,
                max_total = max_total
            )
            publish_key <- paste(current_choices, collapse = "\r")
            if (identical(publish_key, last_published_key)) {
                return(invisible(current_choices))
            }
            last_published_key <<- publish_key
            publish_gene_autocomplete(
                input_id = input_id,
                choices = current_choices,
                source_id = input_id,
                max_total = max_total
            )
            app_perf_mark(auto_perf, sprintf("publish_%s choices=%d", reason, as.integer(length(current_choices))), "AUTO")
            invisible(current_choices)
        }

        for (p in paths) {
            if (min_shared <= 1L && length(all_suggestions) >= max_total) {
                app_perf_mark(auto_perf, "max_total reached before finishing paths", "AUTO")
                break
            }
            ckey <- gene_autocomplete_cache_key(p)
            cached <- if (nzchar(ckey)) cache_map_check[[ckey]] else NULL
            path_resolved <- !is.null(cached)
            app_perf_mark(
                auto_perf,
                sprintf(
                    "path=%s cache=%s",
                    basename(as.character(p %||% "")),
                    if (is.null(cached)) "miss" else "hit"
                ),
                "AUTO"
            )
            remaining <- if (min_shared > 1L) {
                as.integer(max_total)
            } else {
                as.integer(max_total - length(all_suggestions))
            }
            if (remaining <= 0L) {
                break
            }
            if (is.null(cached)) {
                suggestion_cap <- as.integer(max(1L, min(per_path_cap, remaining)))
                if (isTRUE(allow_build)) {
                    app_perf_mark(auto_perf, sprintf("build mode start cap=%d", as.integer(suggestion_cap)), "AUTO")
                    if (is.function(warm_annotation_cache_fn)) {
                        warm_annotation_cache_fn(annotation_path = p, status_rv = NULL, context_label = NULL)
                    }
                    cached <- get_gene_suggestions_for_annotation(
                        p,
                        max_suggestions = suggestion_cap
                    )
                    cache_map_check <- geneAutocompleteCache_rv()
                    path_resolved <- !is.null(cached)
                    app_perf_mark(auto_perf, sprintf("build mode done n=%d", as.integer(length(cached %||% character(0)))), "AUTO")
                } else {
                    if (isTRUE(allow_disk_index)) {
                        app_perf_mark(auto_perf, sprintf("disk index lookup start cap=%d", as.integer(suggestion_cap)), "AUTO")
                        cached <- get_gene_suggestions_from_disk_index(
                            p,
                            max_suggestions = suggestion_cap
                        )
                        cache_map_check <- geneAutocompleteCache_rv()
                        if (!is.null(cached)) {
                            app_perf_mark(auto_perf, sprintf("disk index hit n=%d", as.integer(length(cached %||% character(0)))), "AUTO")
                        }
                        path_resolved <- !is.null(cached)
                    } else {
                        cached <- NULL
                        path_resolved <- FALSE
                        app_perf_mark(auto_perf, "disk index lookup skipped", "AUTO")
                    }
                    if (isTRUE(allow_quick_scan)) {
                        if (is.null(cached)) {
                            quick_cap_base <- if (path_count <= 1L) {
                                7000L
                            } else if (path_count <= 3L) {
                                4200L
                            } else {
                                1800L
                            }
                            quick_cap <- as.integer(max(200L, min(quick_cap_base, suggestion_cap)))
                            quick_lines <- as.integer(if (path_count <= 1L) {
                                1200000L
                            } else if (path_count <= 3L) {
                                700000L
                            } else {
                                320000L
                            })
                            quick_seconds <- if (path_count <= 1L) {
                                1.20
                            } else if (path_count <= 3L) {
                                0.80
                            } else {
                                0.35
                            }
                            cached <- get_cached_quick_gene_suggestions(
                                p,
                                max_suggestions = quick_cap,
                                max_lines = quick_lines,
                                max_seconds = quick_seconds
                            )
                            if (!is.null(cached)) {
                                path_resolved <- TRUE
                                app_perf_mark(auto_perf, sprintf("quick cache hit n=%d", as.integer(length(cached %||% character(0)))), "AUTO")
                            } else {
                                if (requireNamespace("later", quietly = TRUE)) {
                                    quick_scan_specs[[length(quick_scan_specs) + 1L]] <- list(
                                        path = p,
                                        max_suggestions = quick_cap,
                                        max_lines = quick_lines,
                                        max_seconds = quick_seconds,
                                        chunk_lines = 1000L
                                    )
                                    cached <- character(0)
                                    path_resolved <- FALSE
                                    app_perf_mark(
                                        auto_perf,
                                        sprintf("quick scan scheduled cap=%d lines=%d secs=%.2f", as.integer(quick_cap), as.integer(quick_lines), as.numeric(quick_seconds)),
                                        "AUTO"
                                    )
                                } else {
                                    cached <- extract_quick_gene_suggestions(
                                        p,
                                        max_suggestions = quick_cap,
                                        max_lines = quick_lines,
                                        max_seconds = quick_seconds
                                    )
                                    cached <- store_cached_quick_gene_suggestions(
                                        p,
                                        cached,
                                        max_suggestions = quick_cap,
                                        max_lines = quick_lines,
                                        max_seconds = quick_seconds
                                    )
                                    path_resolved <- TRUE
                                    app_perf_mark(
                                        auto_perf,
                                        sprintf(
                                            "quick scan done n=%d cap=%d lines=%d secs=%.2f",
                                            as.integer(length(cached %||% character(0))),
                                            as.integer(quick_cap),
                                            as.integer(quick_lines),
                                            as.numeric(quick_seconds)
                                        ),
                                        "AUTO"
                                    )
                                }
                            }
                        }
                    } else {
                        cached <- cached %||% character(0)
                        app_perf_mark(auto_perf, "cache miss and quick scan disabled", "AUTO")
                    }
                }
            }
            if (!isTRUE(path_resolved)) {
                unresolved_paths <- c(unresolved_paths, p)
            }
            cached <- as.character(cached %||% character(0))
            if (length(cached) > remaining) {
                cached <- cached[seq_len(remaining)]
            }
            if (length(cached) > 0) {
                all_suggestions <- unique(c(all_suggestions, cached))
            }
            path_key <- normalize_annotation_key_safe(p)
            suggestions_by_path[[length(suggestions_by_path) + 1L]] <- cached
            keys_by_path[[length(keys_by_path) + 1L]] <- autocomplete_keys_for_choices(p, cached)
            names(suggestions_by_path)[length(suggestions_by_path)] <- path_key
            names(keys_by_path)[length(keys_by_path)] <- path_key
            if (length(cached) > 0 && min_shared <= 1L) {
                publish_current_suggestions(reason = sprintf("path_%d", as.integer(length(suggestions_by_path))))
            }
        }

        all_suggestions <- publish_current_suggestions(reason = "final")
        app_perf_mark(
            auto_perf,
            sprintf("done choices=%d", as.integer(length(all_suggestions))),
            "AUTO"
        )
        if (length(quick_scan_specs) > 0L) {
            schedule_quick_gene_autocomplete_scan(
                input_id = input_id,
                quick_specs = quick_scan_specs,
                request_token = request_token,
                max_total = max_total,
                delay_sec = 0.02,
                min_shared_organisms = min_shared,
                initial_suggestions_by_path = suggestions_by_path,
                initial_keys_by_path = keys_by_path
            )
            app_perf_mark(auto_perf, sprintf("async quick scan queued paths=%d", as.integer(length(quick_scan_specs))), "AUTO")
        }
        invisible(list(
            choices = as.character(all_suggestions %||% character(0)),
            unresolved_paths = unique(as.character(unresolved_paths)),
            complete = length(unresolved_paths) == 0L
        ))
    }

    schedule_gene_autocomplete_build <- function(input_id, annotation_paths, max_total = 20000L, delay_sec = 0.8, still_valid = NULL, min_shared_organisms = 1L) {
        sched_perf <- app_perf_new_run(sprintf("AUTO_BG-%s", as.character(input_id %||% "input")))
        min_shared <- suppressWarnings(as.integer(min_shared_organisms %||% 1L))
        if (!is.finite(min_shared) || is.na(min_shared) || min_shared < 1L) {
            min_shared <- 1L
        }
        app_perf_mark(sched_perf, "start", "AUTO_BG")
        if (!requireNamespace("later", quietly = TRUE)) {
            app_perf_mark(sched_perf, "later package unavailable", "AUTO_BG")
            return(invisible(FALSE))
        }
        paths <- unique(as.character(annotation_paths %||% character(0)))
        paths <- paths[nzchar(paths) & file.exists(paths)]
        if (length(paths) == 0) {
            app_perf_mark(sched_perf, "no valid paths", "AUTO_BG")
            return(invisible(FALSE))
        }
        max_total <- suppressWarnings(as.integer(max_total))
        if (!is.finite(max_total) || max_total <= 0L) {
            max_total <- 20000L
        }
        delay_val <- suppressWarnings(as.numeric(delay_sec))
        if (!is.finite(delay_val) || delay_val < 0) {
            delay_val <- 0.8
        }
        app_perf_mark(
            sched_perf,
            sprintf("queue paths=%d delay=%.2f", as.integer(length(paths)), as.numeric(delay_val)),
            "AUTO_BG"
        )

        path_count <- max(length(paths), 1L)
        per_path_cap <- as.integer(max(600L, min(10000L, ceiling(max_total / path_count))))
        cache_map_check <- geneAutocompleteCache_rv()
        all_suggestions <- character(0)
        suggestions_by_path <- list()
        keys_by_path <- list()
        next_idx <- 1L
        run_next <- NULL
        extract_suggestions_from_index <- function(idx, max_suggestions) {
            if (is.null(idx) || !is.list(idx) || is.null(idx$genes_df) || nrow(idx$genes_df) == 0) {
                return(character(0))
            }
            attrs <- as.character(idx$genes_df$attributes %||% rep("", nrow(idx$genes_df)))
            suggestions <- if (exists("extract_partial_gene_display_names", mode = "function")) {
                extract_partial_gene_display_names(attrs)
            } else {
                names_lists <- extract_autocomplete_name_lists(attrs)
                c(names_lists$primary, names_lists$fallback)
            }
            suggestions <- sanitize_autocomplete_choices(suggestions, max_total = max_suggestions)
            if (length(suggestions) > max_suggestions) suggestions <- suggestions[seq_len(max_suggestions)]
            suggestions
        }

        last_published_key <- "\001__unset__"
        publish_current_build_suggestions <- function(reason = "partial") {
            current_choices <- aggregate_shared_gene_suggestions(
                suggestions_by_path,
                keys_by_path = keys_by_path,
                min_shared_organisms = min_shared,
                max_total = max_total
            )
            publish_key <- paste(current_choices, collapse = "\r")
            if (identical(publish_key, last_published_key)) {
                return(invisible(current_choices))
            }
            last_published_key <<- publish_key
            publish_gene_autocomplete(
                input_id = input_id,
                choices = current_choices,
                source_id = input_id,
                max_total = max_total
            )
            app_perf_mark(sched_perf, sprintf("publish_%s choices=%d", reason, as.integer(length(current_choices))), "AUTO_BG")
            invisible(current_choices)
        }

        continue_after_build <- function(cached, p, remaining) {
            cached <- sanitize_autocomplete_choices(cached, max_total = remaining)
            path_key <- normalize_annotation_key_safe(p)
            suggestions_by_path[[length(suggestions_by_path) + 1L]] <<- cached
            keys_by_path[[length(keys_by_path) + 1L]] <<- autocomplete_keys_for_choices(p, cached)
            slot <- length(suggestions_by_path)
            suggestion_names <- names(suggestions_by_path)
            key_names <- names(keys_by_path)
            if (length(suggestion_names) < slot) length(suggestion_names) <- slot
            if (length(key_names) < slot) length(key_names) <- slot
            suggestion_names[slot] <- path_key
            key_names[slot] <- path_key
            names(suggestions_by_path) <<- suggestion_names
            names(keys_by_path) <<- key_names
            if (length(cached) > 0) {
                all_suggestions <<- unique(c(all_suggestions, cached))
            }
            # Update autocomplete cache for this path
            ckey <- gene_autocomplete_cache_key(p)
            if (nzchar(ckey) && length(cached) > 0) {
                cm <- geneAutocompleteCache_rv()
                if (is.null(cm[[ckey]])) {
                    cm[[ckey]] <- cached
                    geneAutocompleteCache_rv(trim_autocomplete_cache_map(cm))
                }
                cache_map_check <<- geneAutocompleteCache_rv()
            }
            if (length(cached) > 0 && min_shared <= 1L) {
                publish_current_build_suggestions(reason = sprintf("path_%d", as.integer(length(suggestions_by_path))))
            }
            later::later(run_next, delay = 0.04)
            invisible(TRUE)
        }

        run_next <- function() {
            if (is.function(still_valid)) {
                is_valid <- tryCatch(isTRUE(still_valid()), error = function(e) FALSE)
                if (!is_valid) {
                    app_perf_mark(sched_perf, "stopped: still_valid=FALSE", "AUTO_BG")
                    return(invisible(FALSE))
                }
            }

            if (next_idx > length(paths) || (min_shared <= 1L && length(all_suggestions) >= max_total)) {
                final_choices <- publish_current_build_suggestions(reason = "final")
                app_perf_mark(sched_perf, sprintf("done final_choices=%d", as.integer(length(final_choices))), "AUTO_BG")
                return(invisible(TRUE))
            }

            p <- paths[[next_idx]]
            next_idx <<- next_idx + 1L
            app_perf_mark(sched_perf, sprintf("build %d/%d %s", as.integer(next_idx - 1L), as.integer(length(paths)), basename(as.character(p %||% ""))), "AUTO_BG")
            remaining <- if (min_shared > 1L) {
                as.integer(max_total)
            } else {
                as.integer(max_total - length(all_suggestions))
            }
            if (remaining > 0L) {
                ckey <- gene_autocomplete_cache_key(p)
                cached <- if (nzchar(ckey)) cache_map_check[[ckey]] else NULL
                if (is.null(cached)) {
                    suggestion_cap <- as.integer(max(1L, min(per_path_cap, remaining)))
                    # Offload heavy GFF parsing to a future worker to avoid blocking UI
                    local_p <- p
                    local_cap <- suggestion_cap
                    tryCatch(
                        {
                            promises::future_promise({
                                # Only the heavy GFF parse runs in the worker thread
                                build_gff_gene_light_index(local_p)
                            }, seed = FALSE) %...>% (function(idx) {
                                # Extract suggestions on the main thread (fast, uses local closure fns)
                                result <- extract_suggestions_from_index(idx, local_cap)
                                app_perf_mark(sched_perf, sprintf("async build done n=%d %s", as.integer(length(result %||% character(0))), basename(as.character(local_p %||% ""))), "AUTO_BG")
                                continue_after_build(result, local_p, remaining)
                            }) %...!% (function(err) {
                                app_perf_mark(sched_perf, sprintf("async build error: %s", as.character(err$message %||% "unknown")), "AUTO_BG")
                                continue_after_build(character(0), local_p, remaining)
                            })
                        },
                        error = function(e) {
                            # Fallback to synchronous if future_promise fails to launch
                            app_perf_mark(sched_perf, sprintf("future_promise fallback: %s", as.character(e$message %||% "unknown")), "AUTO_BG")
                            cached_sync <- tryCatch(
                                {
                                    if (is.function(warm_annotation_cache_fn)) {
                                        warm_annotation_cache_fn(annotation_path = p, status_rv = NULL, context_label = NULL)
                                    }
                                    cached_local <- get_gene_suggestions_for_annotation(p, max_suggestions = suggestion_cap)
                                    cache_map_check <<- geneAutocompleteCache_rv()
                                    cached_local
                                },
                                error = function(e2) character(0)
                            )
                            continue_after_build(cached_sync, p, remaining)
                        }
                    )
                    return(invisible(TRUE))
                }
                cached <- sanitize_autocomplete_choices(cached, max_total = remaining)
                if (length(cached) > 0) {
                    all_suggestions <<- unique(c(all_suggestions, cached))
                }
                path_key <- normalize_annotation_key_safe(p)
                suggestions_by_path[[length(suggestions_by_path) + 1L]] <<- cached
                keys_by_path[[length(keys_by_path) + 1L]] <<- autocomplete_keys_for_choices(p, cached)
                slot <- length(suggestions_by_path)
                suggestion_names <- names(suggestions_by_path)
                key_names <- names(keys_by_path)
                if (length(suggestion_names) < slot) length(suggestion_names) <- slot
                if (length(key_names) < slot) length(key_names) <- slot
                suggestion_names[slot] <- path_key
                key_names[slot] <- path_key
                names(suggestions_by_path) <<- suggestion_names
                names(keys_by_path) <<- key_names
                if (length(cached) > 0 && min_shared <= 1L) {
                    publish_current_build_suggestions(reason = sprintf("cache_%d", as.integer(length(suggestions_by_path))))
                }
            }
            later::later(run_next, delay = 0.04)
            invisible(TRUE)
        }
        later::later(run_next, delay = delay_val)
        invisible(TRUE)
    }

    list(
        gene_autocomplete_cache_key = gene_autocomplete_cache_key,
        trim_autocomplete_cache_map = trim_autocomplete_cache_map,
        extract_autocomplete_name_lists = extract_autocomplete_name_lists,
        sanitize_autocomplete_choices = sanitize_autocomplete_choices,
        autocomplete_keys_for_choices = autocomplete_keys_for_choices,
        get_gene_suggestions_for_annotation = get_gene_suggestions_for_annotation,
        get_gene_suggestions_from_disk_index = get_gene_suggestions_from_disk_index,
        get_cached_quick_gene_suggestions = get_cached_quick_gene_suggestions,
        store_cached_quick_gene_suggestions = store_cached_quick_gene_suggestions,
        aggregate_shared_gene_suggestions = aggregate_shared_gene_suggestions,
        init_quick_gene_scan_state = init_quick_gene_scan_state,
        close_quick_gene_scan_state = close_quick_gene_scan_state,
        step_quick_gene_scan_state = step_quick_gene_scan_state,
        extract_quick_gene_suggestions = extract_quick_gene_suggestions,
        schedule_quick_gene_autocomplete_scan = schedule_quick_gene_autocomplete_scan,
        publish_gene_autocomplete = publish_gene_autocomplete,
        update_gene_autocomplete = update_gene_autocomplete,
        schedule_gene_autocomplete_build = schedule_gene_autocomplete_build
    )
}
