# Gene alias resolution helpers.
# These functions are intentionally pure-ish and fail closed: if an alias index
# is unavailable or malformed, callers can continue with the existing lookup flow.
#
# v2: SQLite backend for near-zero memory footprint.  Prefer .alias_index.sqlite
# databases (built by scripts/build_alias_index_sqlite.R) and fall back to
# the legacy .alias_index.tsv.gz files when SQLite is unavailable.

.alias_index_memory_cache <- new.env(parent = emptyenv())
.alias_sqlite_connection_cache <- new.env(parent = emptyenv())
.alias_sqlite_connection_access <- new.env(parent = emptyenv())
.alias_sqlite_access_counter <- 0
.alias_index_mtime_cache <- new.env(parent = emptyenv())
.alias_sqlite_available <- NULL

alias_sqlite_is_available <- function() {
    if (!is.null(.alias_sqlite_available)) return(.alias_sqlite_available)
    .alias_sqlite_available <<- requireNamespace("DBI", quietly = TRUE) &&
        requireNamespace("RSQLite", quietly = TRUE)
    .alias_sqlite_available
}

alias_index_dir <- function(base_dir = ".") {
    root <- if (exists("get_cgv_data_root", mode = "function")) {
        get_cgv_data_root(base_dir)
    } else {
        normalizePath(base_dir, winslash = "/", mustWork = FALSE)
    }
    normalizePath(file.path(root, "data", "alias_index"), winslash = "/", mustWork = FALSE)
}

alias_sqlite_path <- function(organism_id, base_dir = ".") {
    org <- gsub("[^A-Za-z0-9._-]+", "_", trimws(as.character(organism_id %||% "")))
    if (!nzchar(org)) return("")
    file.path(alias_index_dir(base_dir = base_dir), paste0(org, ".alias_index.sqlite"))
}

normalize_gene_query <- function(query) {
    original <- trimws(safe_url_decode(as.character(query %||% "")))
    original <- ifelse(is.na(original), "", original)
    upper <- toupper(original)
    clean_basic <- gsub("[[:space:]]+", " ", upper)
    clean_basic <- gsub("[._;:-]+", "_", clean_basic)
    clean_basic <- gsub("_+", "_", clean_basic)
    clean_basic <- gsub("^_|_$", "", clean_basic)
    clean_strict <- gsub("[^A-Z0-9]+", "", upper)

    variants <- unique(c(
        original,
        upper,
        clean_basic,
        clean_strict,
        gsub(";", ".", original, fixed = TRUE),
        gsub(";", "-", original, fixed = TRUE),
        gsub(";", "_", original, fixed = TRUE),
        gsub(";", "", original, fixed = TRUE),
        gsub("\\.", ";", original),
        gsub("\\.", "-", original),
        gsub("\\.", "_", original),
        gsub("\\.", "", original),
        gsub("-", ";", original, fixed = TRUE),
        gsub("-", ".", original, fixed = TRUE),
        gsub("-", "_", original, fixed = TRUE),
        gsub("-", "", original, fixed = TRUE),
        gsub("_", ";", original, fixed = TRUE),
        gsub("_", ".", original, fixed = TRUE),
        gsub("_", "-", original, fixed = TRUE),
        gsub("_", "", original, fixed = TRUE)
    ))
    variants <- trimws(as.character(variants %||% character(0)))
    variants <- variants[!is.na(variants) & nzchar(variants)]

    list(
        original = original,
        upper = upper,
        clean_basic = clean_basic,
        clean_strict = clean_strict,
        variants = unique(variants)
    )
}

alias_query_keys_df <- function(terms) {
    terms <- unique(trimws(safe_url_decode(as.character(terms %||% character(0)))))
    terms <- terms[!is.na(terms) & nzchar(terms)]
    if (length(terms) == 0L) {
        return(data.frame(
            query_term_original = character(0),
            query_term_upper = character(0),
            query_term_clean_basic = character(0),
            query_term_clean_strict = character(0),
            stringsAsFactors = FALSE
        ))
    }
    normed <- lapply(terms, normalize_gene_query)
    data.frame(
        query_term_original = terms,
        query_term_upper = vapply(normed, `[[`, character(1), "upper"),
        query_term_clean_basic = vapply(normed, `[[`, character(1), "clean_basic"),
        query_term_clean_strict = vapply(normed, `[[`, character(1), "clean_strict"),
        stringsAsFactors = FALSE
    )
}

alias_index_path <- function(organism_id, base_dir = ".") {
    org <- gsub("[^A-Za-z0-9._-]+", "_", trimws(as.character(organism_id %||% "")))
    if (!nzchar(org)) return("")
    file.path(alias_index_dir(base_dir = base_dir), paste0(org, ".alias_index.tsv.gz"))
}

alias_index_metadata_path <- function(organism_id, base_dir = ".") {
    org <- gsub("[^A-Za-z0-9._-]+", "_", trimws(as.character(organism_id %||% "")))
    if (!nzchar(org)) return("")
    file.path(alias_index_dir(base_dir = base_dir), paste0(org, ".metadata.json"))
}

alias_index_empty <- function() {
    data.frame(
        organism_id = character(),
        organism_name = character(),
        taxid = character(),
        query_term_original = character(),
        query_term_upper = character(),
        query_term_clean_basic = character(),
        query_term_clean_strict = character(),
        term_type = character(),
        local_gene_id = character(),
        local_transcript_id = character(),
        local_feature_id = character(),
        local_symbol = character(),
        chromosome = character(),
        start = numeric(),
        end = numeric(),
        strand = character(),
        description = character(),
        source_db = character(),
        source_release = character(),
        confidence = character(),
        evidence_source = character(),
        stringsAsFactors = FALSE
    )
}

alias_index_required_columns <- names(alias_index_empty())

alias_term_confidence <- function(term_type) {
    tt <- tolower(as.character(term_type %||% ""))
    high <- c("id", "local_id", "gene_id", "transcript_id", "protein_id", "ensembl_gene_id", "ensembl_transcript_id")
    medium <- c("name", "gene_name", "gene", "alias", "gene_synonym", "synonym", "dbxref", "locus_tag", "uniprot_id", "refseq_mrna", "refseq_peptide", "entrezgene_id")
    ifelse(tt %in% high, "HIGH", ifelse(tt %in% medium, "MEDIUM", "LOW"))
}

extract_alias_terms_from_attr <- function(attr) {
    attrs <- parse_gff_attributes(attr %||% "")
    if (length(attrs) == 0L) {
        return(data.frame(term = character(0), term_type = character(0), stringsAsFactors = FALSE))
    }
    keys <- c(
        "id", "name", "gene_id", "gene", "gene_name", "gene_synonym", "gene_synonyms",
        "synonym", "alias", "dbxref", "locus", "locus_tag", "transcript_id", "transcript",
        "protein_id", "product", "note", "description"
    )
    rows <- list()
    row_i <- 0L
    for (key in intersect(keys, names(attrs))) {
        vals <- safe_url_decode(as.character(attrs[[key]] %||% character(0)))
        vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
        if (length(vals) == 0L) next
        split_pattern <- if (key %in% c("product", "note", "description")) "[|]" else "[,| ]"
        expanded <- unique(c(vals, unlist(strsplit(vals, split_pattern))))
        if (key %in% c("product", "note", "description")) {
            cleaned <- trimws(sub("\\s*\\[.*\\]\\s*$", "", vals))
            gene_like <- unlist(regmatches(vals, gregexpr("\\b[A-Za-z][A-Za-z0-9]*(?:[;._-][A-Za-z0-9]+)+\\b", vals, perl = TRUE)))
            expanded <- unique(c(vals, cleaned, gene_like))
        }
        expanded <- trimws(as.character(expanded %||% character(0)))
        expanded <- expanded[!is.na(expanded) & nzchar(expanded) & nchar(expanded) <= 180]
        if (length(expanded) == 0L) next
        row_i <- row_i + 1L
        rows[[row_i]] <- data.frame(term = expanded, term_type = key, stringsAsFactors = FALSE)
    }
    if (length(rows) == 0L) {
        return(data.frame(term = character(0), term_type = character(0), stringsAsFactors = FALSE))
    }
    out <- do.call(rbind,  rows)
    out <- out[!duplicated(paste(tolower(out$term), out$term_type, sep = "\r")), , drop = FALSE]
    rownames(out) <- NULL
    out
}

first_attr_value <- function(attrs, keys) {
    for (key in keys) {
        vals <- safe_url_decode(as.character(attrs[[key]] %||% character(0)))
        vals <- trimws(vals[!is.na(vals) & nzchar(vals)])
        if (length(vals) > 0L) return(vals[[1]])
    }
    ""
}

build_alias_index_from_gff <- function(file_path, organism_id = "", organism_name = "", taxid = "",
                                       source_release = "local_annotation", base_dir = ".") {
    p <- as.character(file_path %||% "")
    if (!nzchar(p) || !file.exists(p)) return(alias_index_empty())
    idx <- tryCatch(build_gff_gene_light_index(p), error = function(e) NULL)
    if (is.null(idx) || !is.list(idx) || is.null(idx$genes_df) || !is.data.frame(idx$genes_df) || nrow(idx$genes_df) == 0L) {
        return(alias_index_empty())
    }
    genes_df <- idx$genes_df
    rows <- vector("list", nrow(genes_df))
    out_i <- 0L
    for (i in seq_len(nrow(genes_df))) {
        attr_txt <- as.character(genes_df$attributes[i] %||% "")
        attrs <- parse_gff_attributes(attr_txt)
        terms <- extract_alias_terms_from_attr(attr_txt)
        if (nrow(terms) == 0L) next
        keys_df <- alias_query_keys_df(terms$term)
        if (nrow(keys_df) == 0L) next
        terms <- terms[match(keys_df$query_term_original, terms$term), , drop = FALSE]
        local_gene_id <- first_attr_value(attrs, c("id", "gene_id", "locus_tag", "name"))
        local_symbol <- first_attr_value(attrs, c("gene_name", "gene", "name", "locus_tag", "id"))
        local_tx <- first_attr_value(attrs, c("transcript_id", "transcript", "parent"))
        desc <- first_attr_value(attrs, c("description", "product", "note"))
        out_i <- out_i + 1L
        rows[[out_i]] <- data.frame(
            organism_id = as.character(organism_id %||% ""),
            organism_name = as.character(organism_name %||% ""),
            taxid = as.character(taxid %||% ""),
            keys_df,
            term_type = terms$term_type,
            local_gene_id = local_gene_id,
            local_transcript_id = local_tx,
            local_feature_id = local_gene_id,
            local_symbol = local_symbol,
            chromosome = as.character(genes_df$seqid[i] %||% ""),
            start = suppressWarnings(as.numeric(genes_df$start[i] %||% NA_real_)),
            end = suppressWarnings(as.numeric(genes_df$end[i] %||% NA_real_)),
            strand = as.character(genes_df$strand[i] %||% ""),
            description = desc,
            source_db = "GFF",
            source_release = as.character(source_release %||% "local_annotation"),
            confidence = alias_term_confidence(terms$term_type),
            evidence_source = "local_annotation",
            stringsAsFactors = FALSE
        )
    }
    rows <- rows[seq_len(out_i)]
    if (length(rows) == 0L) return(alias_index_empty())
    out <- do.call(rbind,  rows)
    out <- out[!duplicated(paste(
        out$organism_id, out$query_term_upper, out$query_term_clean_basic,
        out$query_term_clean_strict, out$local_gene_id, out$term_type,
        out$source_db, sep = "\r"
    )), , drop = FALSE]
    rownames(out) <- NULL
    out
}

normalize_alias_index_df <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(alias_index_empty())
    for (nm in alias_index_required_columns) {
        if (!nm %in% names(df)) {
            df[[nm]] <- if (nm %in% c("start", "end")) rep(NA_real_, nrow(df)) else rep("", nrow(df))
        }
    }
    df <- df[, alias_index_required_columns, drop = FALSE]
    char_cols <- setdiff(names(df), c("start", "end"))
    for (nm in char_cols) df[[nm]] <- as.character(df[[nm]] %||% "")
    df$start <- suppressWarnings(as.numeric(df$start))
    df$end <- suppressWarnings(as.numeric(df$end))
    df
}

# ── SQLite connection management ────────────────────────────────────────────

alias_sqlite_cache_size_mb <- function() {
    runtime <- tolower(trimws(as.character(Sys.getenv("CGV_RUNTIME", "") %||% "")))
    default_mb <- if (identical(runtime, "desktop")) 16 else 8
    raw_value <- trimws(as.character(Sys.getenv("APP_ALIAS_SQLITE_CACHE_MB", "") %||% ""))
    if (!nzchar(raw_value)) return(default_mb)

    parsed <- suppressWarnings(as.numeric(raw_value))
    if (length(parsed) == 0L || is.na(parsed[[1L]]) || !is.finite(parsed[[1L]])) {
        return(default_mb)
    }
    min(64, max(1, parsed[[1L]]))
}

alias_sqlite_cache_size_kib <- function() {
    as.integer(round(alias_sqlite_cache_size_mb() * 1024))
}

alias_sqlite_max_connections <- function() {
    runtime <- tolower(trimws(as.character(Sys.getenv("CGV_RUNTIME", "") %||% "")))
    default_max <- if (identical(runtime, "desktop")) 8L else 4L
    raw_value <- trimws(as.character(Sys.getenv("APP_ALIAS_SQLITE_MAX_CONNECTIONS", "") %||% ""))
    if (!nzchar(raw_value)) return(default_max)

    parsed <- suppressWarnings(as.integer(raw_value))
    if (length(parsed) == 0L || is.na(parsed[[1L]]) || !is.finite(parsed[[1L]])) {
        return(default_max)
    }
    as.integer(min(32L, max(1L, parsed[[1L]])))
}

configure_alias_sqlite_read_connection <- function(con) {
    if (is.null(con)) return(invisible(FALSE))
    configured <- tryCatch({
        DBI::dbExecute(con, "PRAGMA query_only = ON")
        DBI::dbExecute(
            con,
            sprintf("PRAGMA cache_size = -%d", alias_sqlite_cache_size_kib())
        )
        TRUE
    }, error = function(e) FALSE)
    invisible(isTRUE(configured))
}

alias_sqlite_connection_is_valid <- function(con) {
    !is.null(con) &&
        requireNamespace("DBI", quietly = TRUE) &&
        isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE))
}

alias_sqlite_touch_connection <- function(conn_key) {
    key <- as.character(conn_key %||% "")
    if (!nzchar(key)) return(invisible(FALSE))
    .alias_sqlite_access_counter <<- .alias_sqlite_access_counter + 1
    if (!is.finite(.alias_sqlite_access_counter)) {
        .alias_sqlite_access_counter <<- 1
    }
    assign(key, .alias_sqlite_access_counter, envir = .alias_sqlite_connection_access)
    invisible(TRUE)
}

alias_sqlite_close_connection_key <- function(conn_key) {
    key <- as.character(conn_key %||% "")
    if (!nzchar(key)) return(invisible(FALSE))
    con <- get0(
        key,
        envir = .alias_sqlite_connection_cache,
        inherits = FALSE,
        ifnotfound = NULL
    )
    if (exists(key, envir = .alias_sqlite_connection_cache, inherits = FALSE)) {
        rm(list = key, envir = .alias_sqlite_connection_cache)
    }
    if (exists(key, envir = .alias_sqlite_connection_access, inherits = FALSE)) {
        rm(list = key, envir = .alias_sqlite_connection_access)
    }
    if (is.null(con)) return(invisible(FALSE))

    if (alias_sqlite_connection_is_valid(con)) {
        disconnected <- tryCatch({
            DBI::dbDisconnect(con)
            !alias_sqlite_connection_is_valid(con)
        }, error = function(e) FALSE)
        return(invisible(isTRUE(disconnected)))
    }
    invisible(TRUE)
}

purge_alias_sqlite_connection <- function(organism_id = "", base_dir = ".") {
    org <- trimws(as.character(organism_id %||% ""))
    if (!nzchar(org)) return(invisible(FALSE))
    alias_sqlite_close_connection_key(.alias_sqlite_conn_key(org, base_dir))
}

close_all_alias_sqlite_connections <- function() {
    keys <- ls(.alias_sqlite_connection_cache, all.names = TRUE)
    if (length(keys) > 0L) {
        invisible(lapply(keys, alias_sqlite_close_connection_key))
    }
    stale_access <- ls(.alias_sqlite_connection_access, all.names = TRUE)
    if (length(stale_access) > 0L) {
        rm(list = stale_access, envir = .alias_sqlite_connection_access)
    }
    invisible(length(keys))
}

prune_alias_sqlite_connections <- function(keep_key = NULL, max_connections = NULL) {
    keep <- as.character(keep_key %||% "")
    if (is.null(max_connections)) max_connections <- alias_sqlite_max_connections()
    max_connections <- suppressWarnings(as.integer(max_connections))
    if (length(max_connections) == 0L || is.na(max_connections[[1L]]) ||
        !is.finite(max_connections[[1L]])) {
        max_connections <- alias_sqlite_max_connections()
    }
    max_connections <- as.integer(min(32L, max(1L, max_connections[[1L]])))

    keys <- ls(.alias_sqlite_connection_cache, all.names = TRUE)
    invalid_keys <- keys[!vapply(keys, function(key) {
        alias_sqlite_connection_is_valid(get0(
            key,
            envir = .alias_sqlite_connection_cache,
            inherits = FALSE,
            ifnotfound = NULL
        ))
    }, logical(1))]
    if (length(invalid_keys) > 0L) {
        invisible(lapply(invalid_keys, alias_sqlite_close_connection_key))
    }

    keys <- ls(.alias_sqlite_connection_cache, all.names = TRUE)
    overflow <- length(keys) - max_connections
    if (overflow <= 0L) return(invisible(character(0)))

    candidates <- setdiff(keys, keep)
    if (length(candidates) == 0L) return(invisible(character(0)))
    access_order <- vapply(candidates, function(key) {
        suppressWarnings(as.numeric(get0(
            key,
            envir = .alias_sqlite_connection_access,
            inherits = FALSE,
            ifnotfound = -Inf
        )))
    }, numeric(1))
    access_order[!is.finite(access_order)] <- -Inf
    eviction_order <- candidates[order(access_order, candidates)]
    evicted <- utils::head(eviction_order, min(overflow, length(eviction_order)))
    invisible(lapply(evicted, alias_sqlite_close_connection_key))
    invisible(evicted)
}

.alias_sqlite_conn_key <- function(organism_id, base_dir) {
    paste0(
        "sqlite|",
        trimws(as.character(organism_id %||% "")),
        "|",
        normalizePath(as.character(base_dir %||% "."), winslash = "/", mustWork = FALSE)
    )
}

load_alias_index_sqlite <- function(organism_id = "", base_dir = ".") {
    if (!alias_sqlite_is_available()) return(NULL)
    org <- trimws(as.character(organism_id %||% ""))
    if (!nzchar(org)) return(NULL)

    conn_key <- .alias_sqlite_conn_key(org, base_dir)
    cached_conn <- get0(conn_key, envir = .alias_sqlite_connection_cache, inherits = FALSE, ifnotfound = NULL)
    if (alias_sqlite_connection_is_valid(cached_conn)) {
        alias_sqlite_touch_connection(conn_key)
        prune_alias_sqlite_connections(keep_key = conn_key)
        return(cached_conn)
    }
    if (!is.null(cached_conn) || exists(conn_key, envir = .alias_sqlite_connection_access, inherits = FALSE)) {
        alias_sqlite_close_connection_key(conn_key)
    }

    sqlite_path <- alias_sqlite_path(org, base_dir = base_dir)
    if (!nzchar(sqlite_path) || !file.exists(sqlite_path)) return(NULL)

    con <- tryCatch(
        DBI::dbConnect(RSQLite::SQLite(), sqlite_path, flags = RSQLite::SQLITE_RO),
        error = function(e) NULL
    )
    if (is.null(con)) return(NULL)

    configure_alias_sqlite_read_connection(con)

    assign(conn_key, con, envir = .alias_sqlite_connection_cache)
    alias_sqlite_touch_connection(conn_key)
    # Repository callers consume the returned handle synchronously within the
    # current R event-loop turn. Do not retain it in callbacks or reactive state.
    prune_alias_sqlite_connections(keep_key = conn_key)
    con
}

load_alias_index <- function(organism_id = "", annotation_path = "", organism_name = "", taxid = "",
                             base_dir = ".", allow_gff_fallback = TRUE) {
    org <- trimws(as.character(organism_id %||% ""))
    ann <- as.character(annotation_path %||% "")

    # ── Try SQLite first (near-zero memory) ──
    sqlite_con <- load_alias_index_sqlite(organism_id = org, base_dir = base_dir)
    if (!is.null(sqlite_con)) {
        attr(sqlite_con, "index_backend") <- "sqlite"
        attr(sqlite_con, "organism_id") <- org
        return(sqlite_con)
    }

    # ── Legacy TSV path (kept for backward compatibility) ──
    disk_path <- alias_index_path(org, base_dir = base_dir)
    disk_mtime <- if (nzchar(disk_path) && file.exists(disk_path)) {
        cached_mtime <- get0(disk_path, envir = .alias_index_mtime_cache, inherits = FALSE, ifnotfound = NULL)
        if (is.null(cached_mtime)) {
            cached_mtime <- as.numeric(file.info(disk_path)$mtime[1])
            assign(disk_path, cached_mtime, envir = .alias_index_mtime_cache)
        }
        cached_mtime
    } else {
        "no_disk"
    }
    cache_key <- paste(
        "alias-index-v1",
        org,
        normalizePath(ann, winslash = "/", mustWork = FALSE),
        disk_mtime,
        sep = "||"
    )
    cached <- get0(cache_key, envir = .alias_index_memory_cache, inherits = FALSE, ifnotfound = NULL)
    if (!is.null(cached)) return(cached)

    out <- NULL
    if (nzchar(disk_path) && file.exists(disk_path)) {
        out <- tryCatch({
            if (requireNamespace("vroom", quietly = TRUE)) {
                as.data.frame(vroom::vroom(disk_path, delim = "\t", show_col_types = FALSE, progress = FALSE))
            } else {
                read.delim(gzfile(disk_path, open = "rt"), sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
            }
        }, error = function(e) NULL)
    }
    if (is.null(out) && isTRUE(allow_gff_fallback) && nzchar(ann) && file.exists(ann)) {
        out <- tryCatch(
            build_alias_index_from_gff(
                file_path = ann, organism_id = org,
                organism_name = organism_name, taxid = taxid, base_dir = base_dir
            ),
            error = function(e) NULL
        )
    }
    out <- normalize_alias_index_df(out)
    assign(cache_key, out, envir = .alias_index_memory_cache)
    attr(out, "index_backend") <- "dataframe"
    out
}

# ── SQLite search helpers ──────────────────────────────────────────────────

alias_index_match_role <- function(term_type) {
    tt <- tolower(trimws(as.character(term_type %||% "")))
    stable <- c(
        "id", "local_id", "gene_id", "transcript_id", "protein_id",
        "ensembl_gene_id", "ensembl_transcript_id", "ensembl_peptide_id",
        "entrezgene_id", "refseq_mrna", "refseq_peptide", "uniprot_id",
        "locus_tag", "dbxref"
    )
    official_symbol <- c("gene_symbol", "gene", "external_gene_name")
    official_name <- c("name", "gene_name")
    synonym <- c("alias", "gene_synonym", "gene_synonyms", "synonym")
    ifelse(tt %in% stable, "stable_id",
        ifelse(tt %in% official_symbol, "official_symbol",
            ifelse(tt %in% official_name, "official_name",
                ifelse(tt %in% synonym, "synonym", "other"))))
}

rank_alias_index_hits <- function(hits, qn) {
    if (is.null(hits) || !is.data.frame(hits) || nrow(hits) == 0L) return(hits)

    hits$match_role <- alias_index_match_role(hits$term_type)
    hits$confidence <- toupper(as.character(hits$confidence %||% ""))
    hits$confidence[!hits$confidence %in% c("HIGH", "MEDIUM", "LOW")] <- "LOW"

    conf_rank <- c(HIGH = 3L, MEDIUM = 2L, LOW = 1L)
    # Preserve the established ranking values for legacy searches.
    # The lower prefix/contains scores are consumed only by the opt-in catalog.
    match_rank <- c(exact = 3L, normalized_basic = 2L, normalized_strict = 1L, prefix = 0L, contains = -1L)
    role_rank <- c(stable_id = 5L, official_symbol = 4L, official_name = 3L, synonym = 2L, other = 1L)
    hits$.rank <- unname(role_rank[hits$match_role]) * 1000L +
        unname(conf_rank[hits$confidence]) * 100L +
        unname(match_rank[hits$match_type]) * 10L
    hits$.rank[is.na(hits$.rank)] <- 0L

    hits <- hits[order(-hits$.rank, hits$match_type, hits$local_gene_id, hits$query_term_original), , drop = FALSE]
    hits <- hits[!duplicated(paste(hits$local_gene_id, hits$local_feature_id, hits$query_term_original, sep = "\r")), , drop = FALSE]
    rownames(hits) <- NULL
    hits$recommended <- FALSE
    if (nrow(hits) > 0L) hits$recommended[1L] <- TRUE
    hits
}

search_alias_index_partial <- function(query, alias_index, organism_id = "", limit = 80L) {
    qn <- normalize_gene_query(query)
    if (nchar(qn$clean_strict) < 3L) {
        return(list(status = "no_match", query = qn, matches = alias_index_empty(), selected_match = NULL))
    }

    org <- trimws(as.character(organism_id %||% ""))
    limit <- max(1L, suppressWarnings(as.integer(limit %||% 80L)))
    like_key <- gsub("!", "!!", qn$upper, fixed = TRUE)
    like_key <- gsub("%", "!%", like_key, fixed = TRUE)
    like_key <- gsub("_", "!_", like_key, fixed = TRUE)
    prefix_pattern <- paste0(like_key, "%")
    contains_pattern <- paste0("%", like_key, "%")

    if (inherits(alias_index, "SQLiteConnection")) {
        org_sql <- if (nzchar(org)) "AND (organism_id = ? OR organism_id = '')" else ""
        sql <- paste0(
            "SELECT * FROM alias_index WHERE ",
            "query_term_upper LIKE ? ESCAPE '!' AND LOWER(term_type) NOT IN ('description', 'product', 'note') ",
            org_sql,
            " UNION ALL SELECT * FROM alias_index WHERE ",
            "query_term_upper LIKE ? ESCAPE '!' AND LOWER(term_type) NOT IN ('description', 'product', 'note', 'gene_name') ",
            org_sql,
            " LIMIT ?"
        )
        params <- if (nzchar(org)) {
            list(prefix_pattern, org, contains_pattern, org, limit)
        } else {
            list(prefix_pattern, contains_pattern, limit)
        }
        hits <- tryCatch(
            DBI::dbGetQuery(alias_index, sql, params = params),
            error = function(e) {
                app_debug_log("[AliasIndex] SQLite partial query failed: ", e$message)
                alias_index_empty()
            }
        )
    } else {
        idx <- normalize_alias_index_df(alias_index)
        if (nzchar(org) && nrow(idx) > 0L) {
            idx <- idx[!nzchar(idx$organism_id) | idx$organism_id == org, , drop = FALSE]
        }
        term_key <- toupper(as.character(idx$query_term_upper %||% idx$query_term_original %||% ""))
        allowed_term <- !tolower(as.character(idx$term_type %||% "")) %in% c("description", "product", "note")
        prefix_hit <- startsWith(term_key, qn$upper)
        contains_allowed <- allowed_term & tolower(as.character(idx$term_type %||% "")) != "gene_name"
        contains_hit <- grepl(qn$upper, term_key, fixed = TRUE)
        hits <- utils::head(idx[(allowed_term & prefix_hit) | (contains_allowed & contains_hit), , drop = FALSE], limit)
    }

    if (is.null(hits) || !is.data.frame(hits) || nrow(hits) == 0L) {
        return(list(status = "no_match", query = qn, matches = alias_index_empty(), selected_match = NULL))
    }
    hits <- normalize_alias_index_df(hits)
    term_key <- toupper(as.character(hits$query_term_upper %||% hits$query_term_original %||% ""))
    has_token_boundary <- vapply(term_key, function(term) {
        positions <- gregexpr(qn$upper, term, fixed = TRUE)[[1]]
        positions <- positions[positions > 0L]
        if (length(positions) == 0L) return(FALSE)
        any(positions == 1L | !grepl("[A-Z0-9]", substr(term, positions - 1L, positions - 1L)))
    }, logical(1))
    hits <- hits[has_token_boundary, , drop = FALSE]
    term_key <- term_key[has_token_boundary]
    if (nrow(hits) == 0L) {
        return(list(status = "no_match", query = qn, matches = alias_index_empty(), selected_match = NULL))
    }
    is_prefix <- startsWith(term_key, qn$upper)
    hits$match_type <- ifelse(is_prefix, "prefix", "contains")
    hits$input_match <- hits$query_term_original
    hits <- rank_alias_index_hits(hits, qn)
    hits <- hits[!duplicated(paste(hits$local_gene_id, hits$local_feature_id, sep = "\r")), , drop = FALSE]
    hits <- utils::head(hits, limit)

    list(status = "partial_matches", query = qn, matches = hits[, setdiff(names(hits), ".rank"), drop = FALSE], selected_match = NULL)
}

search_alias_index_sqlite <- function(query, con, organism_id = "") {
    qn <- normalize_gene_query(query)
    if (!nzchar(qn$original)) {
        return(list(status = "no_match", query = qn, matches = alias_index_empty(), selected_match = NULL))
    }

    org <- trimws(as.character(organism_id %||% ""))
    org_filter <- if (nzchar(org)) sprintf("AND (organism_id = '%s' OR organism_id = '')", gsub("'", "''", org, fixed = TRUE)) else ""

    sql <- sprintf("SELECT * FROM alias_index WHERE 1=1 %s AND (", org_filter)
    params <- list()

    conditions <- character(0)
    if (nzchar(qn$original)) {
        conditions <- c(conditions, "query_term_original = ?1 OR query_term_upper = ?2")
        params <- c(params, list(qn$original, qn$upper))
    }
    if (nzchar(qn$upper)) {
        param_idx <- length(params) + 1
        conditions <- c(conditions, sprintf("query_term_upper = ?%d", param_idx))
        params <- c(params, list(qn$upper))
    }
    if (nzchar(qn$clean_basic)) {
        param_idx <- length(params) + 1
        conditions <- c(conditions, sprintf("query_term_clean_basic = ?%d", param_idx))
        params <- c(params, list(qn$clean_basic))
    }
    if (nzchar(qn$clean_strict)) {
        param_idx <- length(params) + 1
        conditions <- c(conditions, sprintf("query_term_clean_strict = ?%d", param_idx))
        params <- c(params, list(qn$clean_strict))
    }

    if (length(conditions) == 0L) {
        return(list(status = "no_match", query = qn, matches = alias_index_empty(), selected_match = NULL))
    }

    sql <- paste0(sql, paste(conditions, collapse = " OR "), ")")

    hits <- tryCatch(
        DBI::dbGetQuery(con, sql, params = unlist(params, recursive = FALSE, use.names = FALSE)),
        error = function(e) {
            app_debug_log("[AliasIndex] SQLite query failed: ", e$message)
            alias_index_empty()
        }
    )

    if (nrow(hits) == 0L) {
        return(list(status = "no_match", query = qn, matches = alias_index_empty(), selected_match = NULL))
    }

    hits <- normalize_alias_index_df(hits)

    # Determine match type per row
    hits$match_type <- "normalized_strict"
    hits$match_type[hits$query_term_original == qn$original | hits$query_term_upper == qn$upper] <- "exact"
    hits$match_type[hits$match_type != "exact" & hits$query_term_clean_basic == qn$clean_basic] <- "normalized_basic"

    hits$input_match <- hits$query_term_original
    hits <- rank_alias_index_hits(hits, qn)

    unique_targets <- unique(paste(hits$local_gene_id, hits$local_feature_id, sep = "\r"))
    status <- if (length(unique_targets) == 1L && hits$match_type[1] == "exact") {
        "unique_exact"
    } else if (length(unique_targets) == 1L) {
        "unique_normalized"
    } else if (any(hits$match_type == "exact")) {
        "multiple_exact"
    } else {
        "multiple_normalized"
    }

    list(
        status = status,
        query = qn,
        matches = hits[, setdiff(names(hits), ".rank"), drop = FALSE],
        selected_match = if (startsWith(status, "unique")) hits[1, setdiff(names(hits), ".rank"), drop = FALSE] else NULL
    )
}

# ── Public search API ──────────────────────────────────────────────────────

search_alias_index <- function(query, alias_index, organism_id = "") {
    if (inherits(alias_index, "SQLiteConnection")) {
        return(search_alias_index_sqlite(query, alias_index, organism_id = organism_id))
    }

    # Legacy dataframe search
    idx <- normalize_alias_index_df(alias_index)
    qn <- normalize_gene_query(query)
    if (nrow(idx) == 0L || !nzchar(qn$original)) {
        return(list(status = "no_match", query = qn, matches = alias_index_empty(), selected_match = NULL))
    }
    org <- trimws(as.character(organism_id %||% ""))
    if (nzchar(org) && "organism_id" %in% names(idx)) {
        idx <- idx[!nzchar(idx$organism_id) | idx$organism_id == org, , drop = FALSE]
    }
    if (nrow(idx) == 0L) {
        return(list(status = "no_match", query = qn, matches = alias_index_empty(), selected_match = NULL))
    }

    term_upper <- toupper(idx$query_term_original)
    hit_exact <- idx$query_term_original == qn$original | idx$query_term_upper == qn$upper | term_upper == qn$upper
    hit_basic <- idx$query_term_clean_basic == qn$clean_basic
    hit_strict <- nzchar(qn$clean_strict) & idx$query_term_clean_strict == qn$clean_strict
    hits <- idx[hit_exact | hit_basic | hit_strict, , drop = FALSE]
    if (nrow(hits) == 0L) {
        return(list(status = "no_match", query = qn, matches = alias_index_empty(), selected_match = NULL))
    }
    hits$match_type <- ifelse(hit_exact[hit_exact | hit_basic | hit_strict], "exact",
        ifelse(hit_basic[hit_exact | hit_basic | hit_strict], "normalized_basic", "normalized_strict"))
    hits$input_match <- hits$query_term_original
    hits <- rank_alias_index_hits(hits, qn)

    unique_targets <- unique(paste(hits$local_gene_id, hits$local_feature_id, sep = "\r"))
    status <- if (length(unique_targets) == 1L && hits$match_type[1] == "exact") {
        "unique_exact"
    } else if (length(unique_targets) == 1L) {
        "unique_normalized"
    } else if (any(hits$match_type == "exact")) {
        "multiple_exact"
    } else {
        "multiple_normalized"
    }
    list(
        status = status,
        query = qn,
        matches = hits[, setdiff(names(hits), ".rank"), drop = FALSE],
        selected_match = if (startsWith(status, "unique")) hits[1, setdiff(names(hits), ".rank"), drop = FALSE] else NULL
    )
}

search_alias_index_for_context <- function(query, file_path, det_info = NULL, file_label = NULL, base_dir = ".") {
    det <- det_info %||% list()
    org_id <- as.character(det$species_id %||% det$preloaded_id %||% "")
    org_name <- as.character(det$organism %||% file_label %||% "")
    taxid <- as.character(det$taxid %||% "")
    idx <- load_alias_index(
        organism_id = org_id,
        annotation_path = file_path,
        organism_name = org_name,
        taxid = taxid,
        base_dir = base_dir,
        allow_gff_fallback = !nzchar(org_id)
    )
    search_alias_index(query, idx, organism_id = org_id)
}

# ── Installed-organism catalog search ─────────────────────────────────────
#
# A catalog search deliberately consults only indexes bundled with (or installed
# into) CGeV.  It never falls back to parsing the GFF files: that would make a
# simple discovery query unexpectedly expensive and its result dependent on
# transient uploaded files.  The helper remains UI-agnostic so callers can use
# it for the Gene Catalog page, reports, or a future API.
empty_alias_catalog_results <- function() {
    data.frame(
        organism_id = character(),
        organism = character(),
        icon_url = character(),
        kingdom = character(),
        taxid = character(),
        resolved_gene = character(),
        resolved_symbol = character(),
        matched_term = character(),
        term_type = character(),
        match_type = character(),
        match_role = character(),
        confidence = character(),
        chromosome = character(),
        start = numeric(),
        end = numeric(),
        description = character(),
        source_db = character(),
        source_release = character(),
        availability = character(),
        installable = logical(),
        stringsAsFactors = FALSE
    )
}

search_installed_alias_catalog <- function(query, registry, base_dir = ".",
                                           max_per_organism = 25L,
                                           max_total = 400L) {
    query <- trimws(as.character(query %||% ""))
    if (!nzchar(query) || is.null(registry) || !is.data.frame(registry) || nrow(registry) == 0L) {
        return(empty_alias_catalog_results())
    }

    required <- c("species_id", "organism")
    if (!all(required %in% names(registry))) return(empty_alias_catalog_results())
    ready <- if ("ready" %in% names(registry)) as.logical(registry$ready) else rep(TRUE, nrow(registry))
    rows <- registry[!is.na(ready) & ready, , drop = FALSE]
    rows <- rows[!is.na(rows$species_id) & nzchar(trimws(as.character(rows$species_id))), , drop = FALSE]
    if (nrow(rows) == 0L) return(empty_alias_catalog_results())

    max_per_organism <- max(1L, suppressWarnings(as.integer(max_per_organism %||% 25L)))
    max_total <- max(1L, suppressWarnings(as.integer(max_total %||% 400L)))
    result_rows <- lapply(seq_len(nrow(rows)), function(i) {
        entry <- rows[i, , drop = FALSE]
        species_id <- trimws(as.character(entry$species_id[1] %||% ""))
        annotation_path <- as.character(entry$annotation_path[1] %||% entry$annotation[1] %||% "")
        index <- tryCatch(
            load_alias_index(
                organism_id = species_id,
                annotation_path = annotation_path,
                organism_name = as.character(entry$organism[1] %||% entry$label[1] %||% species_id),
                taxid = as.character(entry$taxid[1] %||% ""),
                base_dir = base_dir,
                allow_gff_fallback = FALSE
            ),
            error = function(e) NULL
        )
        if (is.null(index)) return(NULL)
        found <- tryCatch(search_alias_index(query, index, organism_id = species_id), error = function(e) NULL)
        partial <- tryCatch(search_alias_index_partial(query, index, organism_id = species_id, limit = max_per_organism * 4L), error = function(e) NULL)
        hit_parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, list(
            (found %||% list())$matches,
            (partial %||% list())$matches
        ))
        if (length(hit_parts) == 0L) return(NULL)
        hit_names <- unique(unlist(lapply(hit_parts, names), use.names = FALSE))
        hit_parts <- lapply(hit_parts, function(x) {
            for (nm in setdiff(hit_names, names(x))) x[[nm]] <- NA
            x[, hit_names, drop = FALSE]
        })
        hits <- do.call(rbind, hit_parts)
        if (is.null(hits) || !is.data.frame(hits) || nrow(hits) == 0L) return(NULL)
        match_rank <- c(exact = 5L, normalized_basic = 4L, normalized_strict = 3L, prefix = 2L, contains = 1L)
        confidence_rank <- c(HIGH = 3L, MEDIUM = 2L, LOW = 1L)
        hits$.catalog_rank <- unname(match_rank[tolower(hits$match_type)]) * 10L +
            unname(confidence_rank[toupper(hits$confidence)])
        hits$.catalog_rank[is.na(hits$.catalog_rank)] <- 0L
        hits <- hits[order(-hits$.catalog_rank, hits$local_gene_id, hits$query_term_original), , drop = FALSE]
        hits <- hits[!duplicated(paste(hits$local_gene_id, hits$local_feature_id, sep = "\r")), , drop = FALSE]
        hits <- utils::head(hits, max_per_organism)
        data.frame(
            organism_id = species_id,
            organism = as.character(entry$organism[1] %||% entry$label[1] %||% species_id),
            icon_url = as.character(entry$icon_url[1] %||% "/icons/DNA.ico"),
            kingdom = as.character(entry$kingdom[1] %||% "Other"),
            taxid = as.character(entry$taxid[1] %||% ""),
            resolved_gene = as.character(hits$local_gene_id %||% ""),
            resolved_symbol = as.character(hits$local_symbol %||% ""),
            matched_term = as.character(hits$query_term_original %||% ""),
            term_type = as.character(hits$term_type %||% ""),
            match_type = as.character(hits$match_type %||% ""),
            match_role = as.character(hits$match_role %||% ""),
            confidence = as.character(hits$confidence %||% "LOW"),
            chromosome = as.character(hits$chromosome %||% ""),
            start = suppressWarnings(as.numeric(hits$start %||% NA_real_)),
            end = suppressWarnings(as.numeric(hits$end %||% NA_real_)),
            description = as.character(hits$description %||% ""),
            source_db = as.character(hits$source_db %||% ""),
            source_release = as.character(hits$source_release %||% ""),
            availability = "installed",
            installable = FALSE,
            stringsAsFactors = FALSE
        )
    })
    result_rows <- Filter(Negate(is.null), result_rows)
    if (length(result_rows) == 0L) return(empty_alias_catalog_results())
    out <- do.call(rbind, result_rows)
    match_rank <- c(exact = 5L, normalized_basic = 4L, normalized_strict = 3L, prefix = 2L, contains = 1L)
    confidence_rank <- c(HIGH = 3L, MEDIUM = 2L, LOW = 1L)
    out$.rank <- unname(match_rank[tolower(out$match_type)]) * 10L +
        unname(confidence_rank[toupper(out$confidence)])
    out$.rank[is.na(out$.rank)] <- 0L
    out <- out[order(-out$.rank, out$organism, out$resolved_gene, out$matched_term), , drop = FALSE]
    out$.rank <- NULL
    rownames(out) <- NULL
    utils::head(out, max_total)
}

# Reuse a successful alias-index result only within one lookup operation. A
# no-match is deliberately not retained: the SQLite path fails closed to
# no_match after a query error, so retrying it preserves the existing transient
# error recovery semantics.
make_operation_alias_index_lookup <- function(query, file_path, file_label = NULL,
                                              base_dir = ".", lookup_fun = NULL) {
    if (is.null(lookup_fun)) {
        if (!exists("search_alias_index_for_context", mode = "function")) {
            stop("search_alias_index_for_context is unavailable")
        }
        lookup_fun <- get("search_alias_index_for_context", mode = "function")
    }
    if (!is.function(lookup_fun)) {
        stop("lookup_fun must be a function")
    }

    cached_context <- NULL
    cached_result <- NULL
    has_cached_result <- FALSE

    effective_context <- function(det_info) {
        det <- det_info %||% list()
        list(
            organism_id = as.character(det$species_id %||% det$preloaded_id %||% ""),
            organism_name = as.character(det$organism %||% file_label %||% ""),
            taxid = as.character(det$taxid %||% "")
        )
    }

    function(det_info = NULL) {
        context <- effective_context(det_info)
        if (isTRUE(has_cached_result) && identical(context, cached_context)) {
            return(list(result = cached_result, reused = TRUE))
        }

        result <- lookup_fun(
            query = query,
            file_path = file_path,
            det_info = det_info,
            file_label = file_label,
            base_dir = base_dir
        )
        status <- as.character((result %||% list())$status %||% "")
        status <- if (length(status) > 0L) status[[1L]] else ""
        if (nzchar(status) && grepl("^(unique|multiple)", status)) {
            cached_context <<- context
            cached_result <<- result
            has_cached_result <<- TRUE
        }
        list(result = result, reused = FALSE)
    }
}

alias_index_terms_for_gene <- function(local_gene_id = "", local_feature_id = "", local_symbol = "",
                                       organism_id = "", annotation_path = "", organism_name = "",
                                       taxid = "", base_dir = ".", max_aliases = 80L) {
    org_id <- trimws(as.character(organism_id %||% ""))
    idx <- load_alias_index(
        organism_id = org_id,
        annotation_path = annotation_path,
        organism_name = organism_name,
        taxid = taxid,
        base_dir = base_dir,
        allow_gff_fallback = !nzchar(org_id)
    )

    # ── SQLite fast path ──
    if (inherits(idx, "SQLiteConnection")) {
        con <- idx
        local_gene_id <- trimws(as.character(local_gene_id %||% ""))
        local_feature_id <- trimws(as.character(local_feature_id %||% ""))
        local_symbol <- trimws(as.character(local_symbol %||% ""))

        conditions <- character(0)
        params <- list()
        if (nzchar(local_gene_id)) {
            param_idx <- length(params) + 1
            conditions <- c(conditions, sprintf("local_gene_id = ?%d", param_idx))
            params <- c(params, list(local_gene_id))
        }
        if (nzchar(local_feature_id)) {
            param_idx <- length(params) + 1
            conditions <- c(conditions, sprintf("local_feature_id = ?%d", param_idx))
            params <- c(params, list(local_feature_id))
        }
        if (!any(nzchar(c(local_gene_id, local_feature_id))) && nzchar(local_symbol)) {
            param_idx <- length(params) + 1
            conditions <- c(conditions, sprintf("UPPER(local_symbol) = ?%d", param_idx))
            params <- c(params, list(toupper(local_symbol)))
        }
        if (length(conditions) == 0L) return(alias_index_empty())

        alias_types <- c(
            "id", "local_id", "gene_id", "name", "gene", "gene_name", "gene_symbol",
            "alias", "gene_synonym", "gene_synonyms", "synonym", "external_gene_name",
            "dbxref", "locus", "locus_tag", "transcript_id", "transcript",
            "protein_id", "ensembl_gene_id", "ensembl_transcript_id",
            "ensembl_peptide_id", "entrezgene_id", "refseq_mrna", "refseq_peptide",
            "uniprot_id"
        )

        sql <- sprintf(
            "SELECT * FROM alias_index WHERE (%s) AND LOWER(term_type) IN (%s) AND LENGTH(query_term_original) <= 120",
            paste(conditions, collapse = " OR "),
            paste(sprintf("'%s'", alias_types), collapse = ",")
        )
        sql <- paste0(sql, " ORDER BY CASE confidence WHEN 'HIGH' THEN 1 WHEN 'MEDIUM' THEN 2 ELSE 3 END, LENGTH(query_term_original), query_term_original")

        max_n <- suppressWarnings(as.integer(max_aliases %||% 80L))
        if (!is.finite(max_n) || is.na(max_n) || max_n < 1L) max_n <- 80L
        sql <- paste0(sql, sprintf(" LIMIT %d", max_n))

        out <- tryCatch(
            DBI::dbGetQuery(con, sql, params = unlist(params, recursive = FALSE, use.names = FALSE)),
            error = function(e) alias_index_empty()
        )
        return(normalize_alias_index_df(out))
    }

    # ── Legacy dataframe path ──
    idx <- normalize_alias_index_df(idx)
    if (nrow(idx) == 0L) return(alias_index_empty())

    local_gene_id <- trimws(as.character(local_gene_id %||% ""))
    local_feature_id <- trimws(as.character(local_feature_id %||% ""))
    local_symbol <- trimws(as.character(local_symbol %||% ""))

    hit <- rep(FALSE, nrow(idx))
    if (nzchar(local_gene_id)) {
        hit <- hit | trimws(as.character(idx$local_gene_id %||% "")) == local_gene_id
    }
    if (nzchar(local_feature_id)) {
        hit <- hit | trimws(as.character(idx$local_feature_id %||% "")) == local_feature_id
    }
    if (!any(hit) && nzchar(local_symbol)) {
        hit <- hit | toupper(trimws(as.character(idx$local_symbol %||% ""))) == toupper(local_symbol)
    }
    out <- idx[hit, , drop = FALSE]
    if (nrow(out) == 0L) return(alias_index_empty())

    alias_types <- c(
        "id", "local_id", "gene_id", "name", "gene", "gene_name", "gene_symbol",
        "alias", "gene_synonym", "gene_synonyms", "synonym", "external_gene_name",
        "dbxref", "locus", "locus_tag", "transcript_id", "transcript",
        "protein_id", "ensembl_gene_id", "ensembl_transcript_id",
        "ensembl_peptide_id", "entrezgene_id", "refseq_mrna", "refseq_peptide",
        "uniprot_id"
    )
    tt <- tolower(trimws(as.character(out$term_type %||% "")))
    out <- out[tt %in% alias_types, , drop = FALSE]
    out$query_term_original <- trimws(as.character(out$query_term_original %||% ""))
    out <- out[nzchar(out$query_term_original) & nchar(out$query_term_original) <= 120L, , drop = FALSE]
    if (nrow(out) == 0L) return(alias_index_empty())

    priority_type <- match(tolower(out$term_type), alias_types)
    priority_type[is.na(priority_type)] <- length(alias_types) + 1L
    confidence_rank <- c(HIGH = 1L, MEDIUM = 2L, LOW = 3L)
    cr <- unname(confidence_rank[toupper(out$confidence)])
    cr[is.na(cr)] <- 4L
    out <- out[order(cr, priority_type, nchar(out$query_term_original), out$query_term_original), , drop = FALSE]
    out <- out[!duplicated(toupper(out$query_term_original)), , drop = FALSE]
    max_aliases <- suppressWarnings(as.integer(max_aliases %||% 80L))
    if (!is.finite(max_aliases) || is.na(max_aliases) || max_aliases < 1L) max_aliases <- 80L
    if (nrow(out) > max_aliases) out <- out[seq_len(max_aliases), , drop = FALSE]
    rownames(out) <- NULL
    normalize_alias_index_df(out)
}

ensembl_gene_ids_for_alias_locus <- function(local_gene_id = "", local_feature_id = "", local_symbol = "",
                                             organism_id = "", annotation_path = "", organism_name = "",
                                             taxid = "", base_dir = ".") {
    terms <- alias_index_terms_for_gene(
        local_gene_id = local_gene_id,
        local_feature_id = local_feature_id,
        local_symbol = local_symbol,
        organism_id = organism_id,
        annotation_path = annotation_path,
        organism_name = organism_name,
        taxid = taxid,
        base_dir = base_dir,
        max_aliases = 160L
    )
    ids <- character(0)
    if (is.data.frame(terms) && nrow(terms) > 0L) {
        term_type <- tolower(trimws(as.character(terms$term_type %||% "")))
        ids <- trimws(as.character(terms$query_term_original[term_type == "ensembl_gene_id"] %||% character(0)))
    }

    # Some Ensembl Plants identifiers (notably Arabidopsis ATxGxxxxx) are the
    # native GFF locus ID and therefore do not get a separate ensembl_gene_id row.
    local_candidates <- trimws(as.character(c(local_gene_id, local_feature_id)))
    local_candidates <- sub("^(gene|transcript)[:-]", "", local_candidates, ignore.case = TRUE, perl = TRUE)
    ensembl_like <- grepl(
        "^(ENS[A-Z0-9]*G[0-9]+|AT[1-5CM]G[0-9]+|Zm[0-9]+[A-Za-z]+[0-9]+|Os[0-9]+g[0-9]+|TraesCS[A-Za-z0-9._-]+)$",
        local_candidates,
        ignore.case = TRUE,
        perl = TRUE
    )
    ids <- unique(c(ids, local_candidates[ensembl_like]))
    ids <- ids[!is.na(ids) & nzchar(ids)]
    unique(ids)
}

alias_index_match_to_lookup <- function(match_row, file_path, input_gene) {
    if (is.null(match_row) || !is.data.frame(match_row) || nrow(match_row) == 0L) return(NULL)
    row <- match_row[1, , drop = FALSE]

    row_chr <- as.character(row$chromosome[1] %||% "")
    row_start <- suppressWarnings(as.numeric(row$start[1] %||% NA_real_))
    row_end <- suppressWarnings(as.numeric(row$end[1] %||% NA_real_))
    row_gene_id <- trimws(as.character(row$local_gene_id[1] %||% ""))
    row_symbol <- trimws(as.character(row$local_symbol[1] %||% ""))

    if (nzchar(row_chr) && is.finite(row_start) && is.finite(row_end) &&
        exists("is_tabix_annotation_file", mode = "function") &&
        exists("scan_tabix_region_gff", mode = "function") &&
        exists("extract_gene_block_from_df", mode = "function") &&
        isTRUE(is_tabix_annotation_file(file_path))) {
        region_df <- tryCatch(
            scan_tabix_region_gff(file_path, row_chr, row_start, row_end),
            error = function(e) data.frame()
        )
        result_df <- data.frame()
        if (is.data.frame(region_df) && nrow(region_df) > 0L && nzchar(row_gene_id)) {
            result_df <- tryCatch(extract_gene_block_from_df(region_df, row_gene_id), error = function(e) data.frame())
        }
        if (!is.data.frame(result_df) || nrow(result_df) == 0L) {
            gene_like <- region_df
            if (is.data.frame(gene_like) && nrow(gene_like) > 0L && "V3" %in% names(gene_like)) {
                gene_like <- gene_like[tolower(as.character(gene_like$V3 %||% "")) %in% c("gene", "pseudogene"), , drop = FALSE]
            }
            if (is.data.frame(gene_like) && nrow(gene_like) > 0L) {
                result_df <- gene_like[1, , drop = FALSE]
            }
        }
        if (is.data.frame(result_df) && nrow(result_df) > 0L) {
            return(list(
                data = result_df,
                matched_gene_id = if (nzchar(row_gene_id)) row_gene_id else NA_character_,
                matched_gene_name = if (nzchar(row_symbol)) row_symbol else if (nzchar(row_gene_id)) row_gene_id else input_gene,
                best_alias_used = as.character(row$query_term_original[1] %||% ""),
                alias_index_match = row,
                alias_index_fast_region = TRUE
            ))
        }
    }

    org_id <- as.character(row$organism_id[1] %||% "")
    bridge_sqlite_con <- NULL
    if (nzchar(org_id) && exists("load_alias_index_sqlite", mode = "function")) {
        bridge_sqlite_con <- tryCatch(
            load_alias_index_sqlite(organism_id = org_id, base_dir = "."),
            error = function(e) NULL
        )
    }

    candidates <- unique(trimws(as.character(c(
        row$local_gene_id, row$local_feature_id, row$local_symbol,
        row$query_term_original
    ) %||% character(0))))
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    for (cand in candidates) {
        res <- search_gene_in_file(
            file_path, cand,
            show_diagnostics = FALSE, match_mode = "exact",
            return_meta = TRUE, include_bridge_tokens = TRUE,
            bridge_sqlite_con = bridge_sqlite_con
        )
        if (!is.null(res$data) && is.data.frame(res$data) && nrow(res$data) > 0L) {
            out <- res
            out$best_alias_used <- as.character(row$query_term_original[1] %||% "")
            out$alias_index_match <- row
            return(out)
        }
    }
    NULL
}

resolve_bridge_via_sqlite <- function(con, gene_names, idx) {
    if (!inherits(con, "SQLiteConnection")) return(integer(0))
    terms <- unique(toupper(trimws(as.character(gene_names %||% character(0)))))
    terms <- terms[nzchar(terms)]
    if (length(terms) == 0L) return(integer(0))

    placeholders <- paste(rep("?", length(terms)), collapse = ",")
    sql <- sprintf(
        "SELECT DISTINCT local_gene_id, local_feature_id FROM alias_index WHERE query_term_upper IN (%s)",
        placeholders
    )
    result <- tryCatch(
        DBI::dbGetQuery(con, sql, params = as.list(terms)),
        error = function(e) NULL
    )
    if (is.null(result) || nrow(result) == 0L) return(integer(0))

    bridge_ids <- unique(c(
        trimws(as.character(result$local_gene_id %||% character(0))),
        trimws(as.character(result$local_feature_id %||% character(0)))
    ))
    bridge_ids <- bridge_ids[nzchar(bridge_ids)]
    if (length(bridge_ids) == 0L) return(integer(0))

    norms <- tolower(bridge_ids)
    hits <- gene_lookup_hits(idx, norms)
    hits <- unique(hits[!is.na(hits) & hits >= 1L & hits <= length(idx$gene_rows)])
    if (length(hits) == 0L) {
        comps <- normalize_gene_compact(bridge_ids)
        hits <- gene_lookup_hits(idx, comps, "comp")
        hits <- unique(hits[!is.na(hits) & hits >= 1L & hits <= length(idx$gene_rows)])
    }
    idx$gene_rows[hits]
}

write_alias_index_tsv <- function(alias_index, organism_id, base_dir = ".") {
    out <- normalize_alias_index_df(alias_index)
    path <- alias_index_path(organism_id, base_dir = base_dir)
    if (!nzchar(path)) stop("Missing organism_id for alias index output.")
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    con <- gzfile(path, open = "wt")
    on.exit(close(con), add = TRUE)
    utils::write.table(out, con, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    path
}

# ── Build SQLite database from TSV.gz ─────────────────────────────────────

alias_sqlite_compact_columns <- c(
    "organism_id",
    "query_term_original",
    "query_term_upper",
    "query_term_clean_basic",
    "query_term_clean_strict",
    "term_type",
    "local_gene_id",
    "local_feature_id",
    "local_symbol",
    "confidence",
    "source_db"
)

filter_external_alias_rows <- function(df) {
    df <- normalize_alias_index_df(df)
    if (nrow(df) == 0L) return(df)
    src <- toupper(trimws(as.character(df$source_db %||% "")))
    ev <- tolower(trimws(as.character(df$evidence_source %||% "")))
    df[!(ev == "local_annotation" | src == "GFF"), , drop = FALSE]
}

write_alias_sqlite_compact <- function(df, sqlite_path, schema_version = 2L) {
    if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
        stop("DBI and RSQLite packages are required to build alias SQLite databases")
    }
    out <- normalize_alias_index_df(df)
    out <- out[, alias_sqlite_compact_columns, drop = FALSE]
    out <- out[!duplicated(paste(
        out$organism_id,
        out$query_term_upper,
        out$query_term_clean_basic,
        out$query_term_clean_strict,
        out$local_gene_id,
        out$local_feature_id,
        out$term_type,
        out$source_db,
        sep = "\r"
    )), , drop = FALSE]

    sqlite_path <- as.character(sqlite_path %||% "")
    if (!nzchar(sqlite_path)) stop("Missing sqlite_path.")
    dir.create(dirname(sqlite_path), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(sqlite_path)) unlink(sqlite_path, force = TRUE)

    con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
    on.exit({
        try(DBI::dbExecute(con, "PRAGMA optimize"), silent = TRUE)
        try(DBI::dbDisconnect(con), silent = TRUE)
    }, add = TRUE)

    DBI::dbExecute(con, "PRAGMA journal_mode = OFF")
    DBI::dbExecute(con, "PRAGMA synchronous = OFF")
    DBI::dbExecute(con, "PRAGMA temp_store = MEMORY")
    DBI::dbWriteTable(con, "alias_index", as.data.frame(out), overwrite = TRUE)

    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_query_term_original ON alias_index(query_term_original)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_upper ON alias_index(query_term_upper)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_clean_basic ON alias_index(query_term_clean_basic)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_clean_strict ON alias_index(query_term_clean_strict)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_local_gene ON alias_index(local_gene_id)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_local_feature ON alias_index(local_feature_id)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_local_symbol ON alias_index(local_symbol)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_local_symbol_upper ON alias_index(UPPER(local_symbol))")

    meta <- data.frame(
        key = c("schema_version", "format", "row_count"),
        value = c(as.character(schema_version), "external_compact", as.character(nrow(out))),
        stringsAsFactors = FALSE
    )
    DBI::dbWriteTable(con, "alias_index_meta", meta, overwrite = TRUE)
    DBI::dbExecute(con, "VACUUM")

    list(path = sqlite_path, row_count = nrow(out), format = "external_compact", schema_version = schema_version)
}

build_alias_sqlite_external_compact <- function(source_sqlite_path, sqlite_path) {
    if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
        stop("DBI and RSQLite packages are required to build alias SQLite databases")
    }
    source_sqlite_path <- as.character(source_sqlite_path %||% "")
    if (!nzchar(source_sqlite_path) || !file.exists(source_sqlite_path)) {
        stop("Source SQLite file not found: ", source_sqlite_path)
    }
    source_con <- DBI::dbConnect(RSQLite::SQLite(), source_sqlite_path, flags = RSQLite::SQLITE_RO)
    on.exit(try(DBI::dbDisconnect(source_con), silent = TRUE), add = TRUE)
    source_fields <- DBI::dbListFields(source_con, "alias_index")
    has_evidence_source <- "evidence_source" %in% source_fields
    select_cols <- paste(
        "organism_id, query_term_original, query_term_upper, query_term_clean_basic,",
        "query_term_clean_strict, term_type, local_gene_id, local_feature_id, local_symbol,",
        "confidence, source_db"
    )
    if (isTRUE(has_evidence_source)) {
        select_cols <- paste(select_cols, ", evidence_source")
        where_clause <- "WHERE NOT (evidence_source = 'local_annotation' OR source_db = 'GFF')"
    } else {
        where_clause <- "WHERE NOT (source_db = 'GFF')"
    }
    df <- DBI::dbGetQuery(
        source_con,
        paste("SELECT", select_cols, "FROM alias_index", where_clause)
    )
    if (is.null(df) || nrow(df) == 0L) {
        if (file.exists(sqlite_path)) unlink(sqlite_path, force = TRUE)
        return(list(path = as.character(sqlite_path %||% ""), row_count = 0L, format = "external_compact", schema_version = 2L))
    }
    write_alias_sqlite_compact(df, sqlite_path = sqlite_path, schema_version = 2L)
}

build_alias_sqlite_from_tsv <- function(tsv_path, sqlite_path, external_compact = TRUE) {
    if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
        stop("DBI and RSQLite packages are required to build alias SQLite databases")
    }
    if (!file.exists(tsv_path)) stop("TSV file not found: ", tsv_path)

    if (requireNamespace("vroom", quietly = TRUE)) {
        df <- vroom::vroom(tsv_path, delim = "\t", progress = FALSE, show_col_types = FALSE)
    } else {
        df <- read.delim(gzfile(tsv_path, open = "rt"), sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
    }
    if (isTRUE(external_compact)) {
        df <- filter_external_alias_rows(as.data.frame(df))
        if (nrow(df) == 0L) {
            if (file.exists(sqlite_path)) unlink(sqlite_path, force = TRUE)
            return(invisible(sqlite_path))
        }
        return(invisible(write_alias_sqlite_compact(df, sqlite_path = sqlite_path)$path))
    }

    if (file.exists(sqlite_path)) unlink(sqlite_path, force = TRUE)

    con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
    on.exit({
        try(DBI::dbExecute(con, "PRAGMA optimize"), silent = TRUE)
        DBI::dbDisconnect(con)
    }, add = TRUE)

    DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
    DBI::dbExecute(con, "PRAGMA synchronous = OFF")
    DBI::dbExecute(con, "PRAGMA cache_size = -64000")

    DBI::dbWriteTable(con, "alias_index", as.data.frame(df), overwrite = TRUE)
    rm(df)
    invisible(gc())

    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_query_term_original ON alias_index(query_term_original)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_upper ON alias_index(query_term_upper)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_clean_basic ON alias_index(query_term_clean_basic)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_clean_strict ON alias_index(query_term_clean_strict)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_local_gene ON alias_index(local_gene_id)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_local_feature ON alias_index(local_feature_id)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_local_symbol ON alias_index(local_symbol)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_local_symbol_upper ON alias_index(UPPER(local_symbol))")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_term_type ON alias_index(term_type)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_confidence ON alias_index(confidence)")

    invisible(sqlite_path)
}

# ── Prewarming: open SQLite connections eagerly at startup ─────────────────

warm_alias_index <- function(organism_id, base_dir = ".") {
    org <- trimws(as.character(organism_id %||% ""))
    if (!nzchar(org)) return(invisible(FALSE))
    if (!alias_sqlite_is_available()) return(invisible(FALSE))

    sqlite_path <- alias_sqlite_path(org, base_dir = base_dir)
    if (!file.exists(sqlite_path)) return(invisible(FALSE))

    con <- load_alias_index_sqlite(organism_id = org, base_dir = base_dir)
    if (is.null(con)) return(invisible(FALSE))

    tryCatch({
        DBI::dbGetQuery(con, "SELECT 1 FROM alias_index LIMIT 1")
        row_count <- NA_character_
        if ("alias_index_meta" %in% DBI::dbListTables(con)) {
            meta <- DBI::dbGetQuery(
                con,
                "SELECT value FROM alias_index_meta WHERE key = 'row_count' LIMIT 1"
            )
            if (is.data.frame(meta) && nrow(meta) > 0L) {
                row_count <- as.character(meta$value[1] %||% NA_character_)
            }
        }
        suffix <- if (!is.na(row_count) && nzchar(row_count)) {
            sprintf(" (%s rows metadata)", row_count)
        } else {
            ""
        }
        message(sprintf("[AliasIndex] warmed %s.sqlite%s", org, suffix))
    }, error = function(e) NULL)
    invisible(TRUE)
}
