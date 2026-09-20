string_api_request <- function(path, query = list(), has_header = TRUE, seconds = 20) {
    base_url <- sprintf(
        "https://string-db.org/api/%s/%s",
        if (isTRUE(has_header)) "tsv" else "tsv-no-header",
        path
    )
    response <- httr2::request(base_url) %>%
        httr2::req_user_agent("CGVViewer STRING client") %>%
        httr2::req_url_query(!!!c(query, list(caller_identity = "cgvviewer"))) %>%
        httr2::req_timeout(seconds) %>%
        httr2::req_retry(
            max_tries = 2,
            retry_on_failure = TRUE,
            backoff = ~ min(8, 0.5 * (2 ^ (.x - 1)) + stats::runif(1, 0, 0.35))
        ) %>%
        httr2::req_perform()

    body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
    if (!nzchar(trimws(body))) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    out <- utils::read.delim(
        text = body,
        sep = "\t",
        header = isTRUE(has_header),
        stringsAsFactors = FALSE,
        quote = "",
        comment.char = ""
    )
    if (!isTRUE(has_header) && ncol(out) >= 6) {
        colnames(out)[seq_len(6)] <- c(
            "queryIndex", "stringId", "ncbiTaxonId",
            "taxonName", "preferredName", "annotation"
        )
    }
    out
}

string_role_variants <- function(x) {
    vals <- unique(trimws(as.character(x %||% character(0))))
    vals <- vals[nzchar(vals) & !is.na(vals)]
    vals_lc <- tolower(vals)
    unique(c(
        vals_lc,
        gsub("[;._-]+", "", vals_lc),
        gsub(";", "", vals_lc, fixed = TRUE)
    ))
}

string_same_taxid <- function(x, target_taxid) {
    xi <- suppressWarnings(as.integer(x %||% NA_integer_))
    ti <- suppressWarnings(as.integer(target_taxid %||% NA_integer_))
    is.finite(xi) && is.finite(ti) && !is.na(xi) && !is.na(ti) && identical(xi, ti)
}

string_collect_same_taxid_candidates <- function(records, target_taxid) {
    if (!is.list(records) || length(records) == 0L) {
        return(character(0))
    }
    out <- character(0)
    for (rec in records) {
        if (!is.list(rec) || !string_same_taxid(rec$taxid, target_taxid)) next
        out <- c(out, as.character(rec$candidates %||% character(0)))
    }
    out <- unique(trimws(out))
    out[nzchar(out) & !is.na(out)]
}

string_resolve_candidate_map <- function(taxid, candidates, base_dir = ".", resolve_missing = TRUE) {
    candidates <- unique(trimws(as.character(candidates %||% character(0))))
    candidates <- candidates[nzchar(candidates) & !is.na(candidates)]
    empty <- data.frame(
        candidate = character(0),
        string_id = character(0),
        preferred_name = character(0),
        stringsAsFactors = FALSE
    )
    if (length(candidates) == 0L) {
        return(empty)
    }

    rows <- list()
    unresolved <- character(0)
    for (candidate in candidates) {
        cached <- string_resolution_cache_get(taxid, candidate, base_dir = base_dir)
        if (is.list(cached) && isTRUE(cached$found) && nzchar(as.character(cached$string_id %||% ""))) {
            rows[[length(rows) + 1L]] <- data.frame(
                candidate = candidate,
                string_id = trimws(as.character(cached$string_id %||% "")),
                preferred_name = trimws(as.character(cached$preferred_name %||% "")),
                stringsAsFactors = FALSE
            )
        } else if (is.null(cached)) {
            unresolved <- c(unresolved, candidate)
        }
    }

    unresolved <- unique(unresolved)
    if (!isTRUE(resolve_missing)) {
        unresolved <- character(0)
    }
    if (length(unresolved) > 0L) {
        mapped <- string_api_request(
            path = "get_string_ids",
            query = list(
                identifiers = paste(unresolved, collapse = "\r"),
                species = as.character(as.integer(taxid))
            ),
            has_header = FALSE,
            seconds = 20
        )

        query_index <- suppressWarnings(as.integer(mapped$queryIndex))
        if (length(query_index) == nrow(mapped) && any(query_index == 0L, na.rm = TRUE)) {
            query_index <- query_index + 1L
        }
        for (candidate_idx in seq_along(unresolved)) {
            hits <- which(query_index == candidate_idx)
            if (length(hits) == 0L) {
                string_resolution_cache_set(taxid, unresolved[candidate_idx], list(found = FALSE), base_dir = base_dir)
                next
            }
            hit <- hits[1L]
            payload <- list(
                found = TRUE,
                string_id = trimws(as.character(mapped$stringId[hit] %||% "")),
                preferred_name = trimws(as.character(mapped$preferredName[hit] %||% ""))
            )
            string_resolution_cache_set(taxid, unresolved[candidate_idx], payload, base_dir = base_dir)
            if (nzchar(payload$string_id)) {
                rows[[length(rows) + 1L]] <- data.frame(
                    candidate = unresolved[candidate_idx],
                    string_id = payload$string_id,
                    preferred_name = payload$preferred_name,
                    stringsAsFactors = FALSE
                )
            }
        }
    }

    if (length(rows) == 0L) {
        return(empty)
    }
    unique(do.call(rbind, rows))
}

string_pick_cached_resolution <- function(taxid, candidates, base_dir = ".") {
    for (candidate in candidates) {
        cached <- string_resolution_cache_get(taxid, candidate, base_dir = base_dir)
        if (is.null(cached)) {
            return(NULL)
        }
        if (is.list(cached) && isTRUE(cached$found) && nzchar(as.character(cached$string_id %||% ""))) {
            return(c(
                cached,
                list(candidate = candidate, cache_hit = TRUE)
            ))
        }
    }
    NULL
}

string_resolve_candidates <- function(taxid, candidates, base_dir = ".") {
    candidates <- unique(trimws(as.character(candidates %||% character(0))))
    candidates <- candidates[nzchar(candidates)]
    if (length(candidates) == 0L) {
        return(NULL)
    }

    cached <- string_pick_cached_resolution(taxid, candidates, base_dir = base_dir)
    if (!is.null(cached)) {
        return(cached)
    }

    # Only an unresolved get_string_ids batch treats HTTP 404 as no mapping.
    mapped <- tryCatch(
        string_api_request(
            path = "get_string_ids",
            query = list(
                identifiers = paste(candidates, collapse = "\r"),
                species = as.character(as.integer(taxid))
            ),
            has_header = FALSE,
            seconds = 20
        ),
        httr2_http_404 = function(e) data.frame(stringsAsFactors = FALSE)
    )
    if (!is.data.frame(mapped) || nrow(mapped) == 0L) {
        for (candidate in candidates) {
            string_resolution_cache_set(taxid, candidate, list(found = FALSE), base_dir = base_dir)
        }
        return(NULL)
    }

    query_index <- suppressWarnings(as.integer(mapped$queryIndex))
    if (length(query_index) == nrow(mapped) && any(query_index == 0L, na.rm = TRUE)) {
        query_index <- query_index + 1L
    }
    for (candidate_idx in seq_along(candidates)) {
        hits <- which(query_index == candidate_idx)
        if (length(hits) == 0L) {
            string_resolution_cache_set(taxid, candidates[candidate_idx], list(found = FALSE), base_dir = base_dir)
            next
        }
        hit <- hits[1L]
        payload <- list(
            found = TRUE,
            string_id = trimws(as.character(mapped$stringId[hit] %||% "")),
            preferred_name = trimws(as.character(mapped$preferredName[hit] %||% ""))
        )
        string_resolution_cache_set(taxid, candidates[candidate_idx], payload, base_dir = base_dir)
    }
    string_pick_cached_resolution(taxid, candidates, base_dir = base_dir)
}

string_network_display_terms <- function(taxid, screen_candidates, base_dir = ".", resolve_missing = TRUE) {
    screen_candidates <- unique(trimws(as.character(screen_candidates %||% character(0))))
    screen_candidates <- screen_candidates[nzchar(screen_candidates) & !is.na(screen_candidates)]
    mapped <- tryCatch(
        string_resolve_candidate_map(taxid, screen_candidates, base_dir = base_dir, resolve_missing = resolve_missing),
        error = function(e) data.frame(string_id = character(0), preferred_name = character(0), stringsAsFactors = FALSE)
    )
    string_role_variants(c(
        screen_candidates,
        mapped$string_id %||% character(0),
        mapped$preferred_name %||% character(0)
    ))
}

string_apply_display_roles <- function(network_payload, snapshot, base_dir = ".", resolve_missing = TRUE) {
    if (!is.list(network_payload) ||
        !is.data.frame(network_payload$nodes) ||
        nrow(network_payload$nodes) == 0L) {
        return(network_payload)
    }
    nodes <- network_payload$nodes
    taxid <- suppressWarnings(as.integer(snapshot$taxid %||% network_payload$taxid %||% NA_integer_))
    resolved_id <- trimws(as.character(network_payload$resolved_id %||% ""))
    screen_terms <- string_network_display_terms(
        taxid,
        snapshot$screen_variants %||% character(0),
        base_dir = base_dir,
        resolve_missing = resolve_missing
    )

    node_ids <- trimws(as.character(nodes$id %||% character(0)))
    node_labels <- trimws(as.character(nodes$label %||% character(0)))
    node_label_lc <- tolower(node_labels)
    node_label_norm <- gsub("[;._-]+", "", node_label_lc)
    node_id_lc <- tolower(node_ids)

    is_target <- node_ids == resolved_id
    is_plotted <- (
        node_label_lc %in% screen_terms |
        node_label_norm %in% screen_terms |
        node_id_lc %in% screen_terms
    ) & !is_target

    nodes$role <- ifelse(is_target, "target", ifelse(is_plotted, "plotted", "neighbor"))
    network_payload$nodes <- nodes
    network_payload$role_applied_at <- as.numeric(Sys.time())
    network_payload
}

string_network_to_payload <- function(net_data, resolved, snapshot) {
    if (!is.data.frame(net_data) || nrow(net_data) == 0L) {
        return(NULL)
    }
    nodes <- unique(data.frame(
        id = c(net_data$stringId_A, net_data$stringId_B),
        label = c(net_data$preferredName_A, net_data$preferredName_B),
        stringsAsFactors = FALSE
    ))

    edges <- data.frame(
        from = net_data$stringId_A,
        to = net_data$stringId_B,
        score = suppressWarnings(as.numeric(net_data$score)),
        stringsAsFactors = FALSE
    )
    list(
        resolved_id = as.character(resolved$string_id),
        preferred_name = as.character(resolved$preferred_name %||% ""),
        taxid = suppressWarnings(as.integer(snapshot$taxid %||% NA_integer_)),
        nodes = nodes,
        edges = edges,
        fetched_at = as.numeric(Sys.time())
    )
}

string_resolve_and_fetch <- function(snapshot, base_dir = ".") {
    taxid <- suppressWarnings(as.integer(snapshot$taxid %||% NA_integer_))
    candidates <- unique(trimws(as.character(snapshot$id_candidates %||% character(0))))
    candidates <- candidates[nzchar(candidates)]
    required_score <- suppressWarnings(as.integer(snapshot$required_score %||% 600L))
    add_nodes <- suppressWarnings(as.integer(snapshot$add_nodes %||% 8L))

    resolved <- string_resolve_candidates(taxid, candidates, base_dir = base_dir)
    if (is.null(resolved) || !nzchar(as.character(resolved$string_id %||% ""))) {
        return(list(ok = FALSE, reason = "not_found"))
    }

    cached_network <- string_network_cache_get(
        taxid,
        resolved$string_id,
        required_score,
        add_nodes,
        base_dir = base_dir
    )
    if (is.list(cached_network) && is.data.frame(cached_network$nodes) && is.data.frame(cached_network$edges)) {
        return(list(
            ok = TRUE,
            cache_hit = TRUE,
            payload = string_apply_display_roles(cached_network, snapshot, base_dir = base_dir)
        ))
    }

    net_data <- string_api_request(
        path = "network",
        query = list(
            identifiers = resolved$string_id,
            species = as.character(taxid),
            required_score = as.character(required_score),
            add_nodes = as.character(add_nodes)
        ),
        has_header = TRUE,
        seconds = 30
    )
    payload <- string_network_to_payload(net_data, resolved, snapshot)
    if (is.null(payload)) {
        return(list(ok = FALSE, reason = "no_interactions"))
    }
    string_network_cache_set(
        taxid,
        resolved$string_id,
        required_score,
        add_nodes,
        payload,
        base_dir = base_dir
    )
    prune_string_disk_cache(base_dir = base_dir)
    list(
        ok = TRUE,
        cache_hit = FALSE,
        payload = string_apply_display_roles(payload, snapshot, base_dir = base_dir)
    )
}

string_try_cached_payload <- function(snapshot, base_dir = ".") {
    taxid <- suppressWarnings(as.integer(snapshot$taxid %||% NA_integer_))
    required_score <- suppressWarnings(as.integer(snapshot$required_score %||% 600L))
    add_nodes <- suppressWarnings(as.integer(snapshot$add_nodes %||% 8L))
    resolved <- string_pick_cached_resolution(
        taxid,
        snapshot$id_candidates %||% character(0),
        base_dir = base_dir
    )
    if (is.null(resolved) || !nzchar(as.character(resolved$string_id %||% ""))) {
        return(NULL)
    }
    payload <- string_network_cache_get(
        taxid,
        resolved$string_id,
        required_score,
        add_nodes,
        base_dir = base_dir
    )
    if (!is.list(payload) || !is.data.frame(payload$nodes) || !is.data.frame(payload$edges)) {
        return(NULL)
    }
    string_apply_display_roles(payload, snapshot, base_dir = base_dir, resolve_missing = FALSE)
}
