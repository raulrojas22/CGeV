# R/modules.R

# Plot module UI function (compartida por homólogos y ortólogos)
plotUIModule <- function(id) {
    ns <- NS(id)
    tagList(
        div(
            class = "promoter-plot-container",
            style = "width: 100%; height: 110px; margin: 0; position: relative;",
            div(
                class = "plot-loading-placeholder",
                role = "status",
                `aria-live` = "polite",
                `aria-label` = "Rendering gene visualization",
                style = "position:absolute; inset:0; display:flex; align-items:center; justify-content:center; z-index:0; pointer-events:none; background:#FFFFFF;",
                div(
                    class = "plot-loading-placeholder-content",
                    div(
                        class = "app-dna-loader app-dna-loader--spinner-sm",
                        `aria-hidden` = "true",
                        div(class = "app-dna-wave app-dna-wave-top", lapply(seq_len(16), function(i) span(class = "app-dna-node", style = sprintf("--i:%d;", i - 1)))),
                        div(class = "app-dna-wave app-dna-wave-bottom", lapply(seq_len(16), function(i) span(class = "app-dna-node", style = sprintf("--i:%d;", i - 1))))
                    ),
                    span(class = "plot-loading-placeholder-text", "Rendering gene visualization…")
                )
            ),
            girafeOutput(ns("plot"), height = "110px", width = "100%")
        )
    )
}

# Aliases para compatibilidad con las llamadas en server.R
plotUIHomologous <- plotUIModule
plotUIOrtologous <- plotUIModule

# Defensive fallback for perf helpers.
if (!exists("app_perf_new_run", mode = "function")) {
    app_perf_new_run <- function(prefix = "RUN") {
        p <- as.character(prefix)
        if (length(p) == 0 || is.na(p[1]) || !nzchar(trimws(p[1]))) {
            p <- "RUN"
        } else {
            p <- trimws(p[1])
        }
        list(
            id = paste0(p, "-fallback"),
            t0 = as.numeric(proc.time()[["elapsed"]])
        )
    }
}
if (!exists("app_perf_mark", mode = "function")) {
    app_perf_mark <- function(run = NULL, step = "", context = "APP") {
        invisible(NA_real_)
    }
}
if (!exists("refresh_girafe_widget_uid", mode = "function")) {
    .next_girafe_uid_local <- local({
        counter <- 0L
        function() {
            counter <<- counter + 1L
            ts_ms <- suppressWarnings(as.integer(round(as.numeric(Sys.time()) * 1000)))
            if (!is.finite(ts_ms)) {
                ts_ms <- as.integer(counter)
            }
            paste0("svg_", ts_ms, "_", counter)
        }
    })
    refresh_girafe_widget_uid <- function(widget_obj) {
        if (is.null(widget_obj)) return(widget_obj)
        if (!inherits(widget_obj, "girafe")) return(widget_obj)
        out <- widget_obj
        if (is.list(out$x)) {
            old_uid <- trimws(as.character(out$x$uid %||% ""))
            new_uid <- .next_girafe_uid_local()
            if (nzchar(old_uid) && nzchar(new_uid) && !identical(old_uid, new_uid)) {
                if (is.character(out$x$html) && length(out$x$html) > 0L) {
                    out$x$html <- gsub(old_uid, new_uid, out$x$html, fixed = TRUE)
                }
                if (is.character(out$x$js) && length(out$x$js) > 0L) {
                    out$x$js <- gsub(old_uid, new_uid, out$x$js, fixed = TRUE)
                }
            }
            out$x$uid <- new_uid
        }
        out
    }
}

.cgv_girafe_plot_cache <- new.env(parent = emptyenv())
.cgv_girafe_plot_cache_order <- character(0)

get_girafe_plot_cache_max_entries <- function() {
    raw <- suppressWarnings(as.integer(Sys.getenv("APP_GIRAFE_PLOT_CACHE_MAX_ENTRIES", "48")))
    if (!is.finite(raw) || raw < 0L) {
        return(48L)
    }
    raw
}

normalize_girafe_plot_signature <- function(plot_signature = NULL, fallback_id = NULL) {
    sig <- trimws(as.character(plot_signature %||% ""))
    sig <- sig[!is.na(sig) & nzchar(sig)]
    if (length(sig) > 0L) {
        return(sig[1])
    }
    paste0("plot_id:", as.character(fallback_id %||% ""))
}

make_girafe_plot_cache_key <- function(plot_context, plot_signature = NULL, fallback_id = NULL,
                                       max_gene_length_key = 0, visual_mode = "compact",
                                       theme_mode = "light", is_colorblind_mode = FALSE,
                                       seq_len_key = 0L, has_neighbor_context = FALSE,
                                       compact_feature_interactivity = NA,
                                       orientation_mode = "genomic") {
    max_len_txt <- format(max_gene_length_key %||% 0, scientific = FALSE, trim = TRUE)
    paste(
        as.character(plot_context %||% "plot"),
        normalize_girafe_plot_signature(plot_signature, fallback_id),
        max_len_txt,
        as.character(visual_mode %||% "compact"),
        as.character(theme_mode %||% "light"),
        as.character(isTRUE(is_colorblind_mode)),
        as.character(seq_len_key %||% 0L),
        as.character(isTRUE(has_neighbor_context)),
        as.character(compact_feature_interactivity),
        normalize_gene_plot_orientation_mode(orientation_mode),
        sep = "|"
    )
}

normalize_gene_plot_orientation_mode <- function(value = "genomic") {
    mode <- tolower(trimws(as.character(value %||% "genomic")[1]))
    if (!mode %in% c("genomic", "transcription")) {
        mode <- "genomic"
    }
    mode
}

gene_plot_axis_should_reverse <- function(orientation_mode = "genomic", strand = "+") {
    identical(normalize_gene_plot_orientation_mode(orientation_mode), "transcription") &&
        identical(trimws(as.character(strand %||% "+")[1]), "-")
}

get_shared_girafe_plot_cache <- function(cache_key) {
    key <- as.character(cache_key %||% "")
    if (!nzchar(key) || get_girafe_plot_cache_max_entries() <= 0L) {
        return(NULL)
    }
    if (!exists(key, envir = .cgv_girafe_plot_cache, inherits = FALSE)) {
        return(NULL)
    }
    .cgv_girafe_plot_cache_order <<- c(setdiff(.cgv_girafe_plot_cache_order, key), key)
    get(key, envir = .cgv_girafe_plot_cache, inherits = FALSE)
}

set_shared_girafe_plot_cache <- function(cache_key, plot_obj) {
    key <- as.character(cache_key %||% "")
    max_entries <- get_girafe_plot_cache_max_entries()
    if (!nzchar(key) || max_entries <= 0L || is.null(plot_obj)) {
        return(invisible(FALSE))
    }
    assign(key, plot_obj, envir = .cgv_girafe_plot_cache)
    .cgv_girafe_plot_cache_order <<- c(setdiff(.cgv_girafe_plot_cache_order, key), key)
    overflow <- length(.cgv_girafe_plot_cache_order) - max_entries
    if (overflow > 0L) {
        stale <- head(.cgv_girafe_plot_cache_order, overflow)
        rm(list = stale, envir = .cgv_girafe_plot_cache)
        .cgv_girafe_plot_cache_order <<- setdiff(.cgv_girafe_plot_cache_order, stale)
    }
    invisible(TRUE)
}

is_compact_feature_interactivity_enabled <- function() {
    TRUE
}

is_ortho_server_render_nudge_enabled <- function() {
    raw <- tolower(trimws(as.character(Sys.getenv("APP_ORTHO_SERVER_RENDER_NUDGE", "0") %||% "0")))
    !raw %in% c("", "0", "false", "no", "off")
}

is_ortho_suspend_hidden_enabled <- function() {
    raw <- tolower(trimws(as.character(Sys.getenv("APP_ORTHO_SUSPEND_HIDDEN", "1") %||% "1")))
    !raw %in% c("", "0", "false", "no", "off")
}

should_inline_fast_sequence_prefetch <- function(genome_path, start_pos, end_pos) {
    if (!isTRUE(app_env_flag("APP_INLINE_FAST_SEQUENCE_PREFETCH", TRUE))) {
        return(FALSE)
    }
    path <- trimws(as.character(genome_path %||% ""))
    if (!nzchar(path) || !file.exists(path) || !grepl("\\.2bit$", path, ignore.case = TRUE)) {
        return(FALSE)
    }
    start_num <- suppressWarnings(as.numeric(start_pos %||% NA_real_))
    end_num <- suppressWarnings(as.numeric(end_pos %||% NA_real_))
    span <- abs(end_num - start_num) + 1
    max_span <- app_env_int(
        "APP_INLINE_FAST_SEQUENCE_MAX_BP",
        2000000L,
        min_value = 1000L,
        max_value = 50000000L
    )
    is.finite(span) && span > 0 && span <= max_span
}

deferred_plot_enrichment_delay_seconds <- function(extra_seconds = 0) {
    base_delay <- if (isTRUE(app_env_flag("APP_INLINE_FAST_SEQUENCE_PREFETCH", TRUE))) 0.15 else 1.5
    extra_num <- suppressWarnings(as.numeric(extra_seconds %||% 0))
    if (!is.finite(extra_num) || extra_num < 0) extra_num <- 0
    base_delay + extra_num
}

# Función auxiliar para procesar datos del gen (compartida)
process_gene_data <- function(data) {
    df_separated <- as.data.frame(data, stringsAsFactors = FALSE)
    df_separated$V3_norm <- tolower(trimws(as.character(df_separated$V3)))

    df_gene <- df_separated[df_separated$V3_norm == "gene", , drop = FALSE]

    tx_level_types <- c(
        "mrna", "transcript", "lnc_rna", "trna", "rrna", "snorna", "snrna", "mirna",
        "ncrna", "primary_transcript", "pre_mirna", "guide_rna", "rnase_p_rna",
        "rnase_mrp_rna", "telomerase_rna", "antisense_rna", "srp_rna", "scarna",
        "vault_rna", "y_rna", "antisense_lncrna", "lncrna"
    )
    df_transcript <- df_separated[df_separated$V3_norm %in% tx_level_types, , drop = FALSE]

    other_idx <- df_separated$V3_norm %in% c("exon", "cds", "start_codon", "stop_codon") |
        grepl("utr", df_separated$V3_norm)
    df_other <- df_separated[other_idx, , drop = FALSE]

    if (nrow(df_other) > 0) {
        group <- rep("other", nrow(df_other))
        v3_other <- df_other$V3_norm
        group[v3_other == "exon"] <- "exon"
        group[v3_other == "cds"] <- "cds"
        group[v3_other %in% c("start_codon", "stop_codon")] <- "codon"
        group[grepl("utr", v3_other)] <- "utr"
        df_other$group <- group
        df_other$text <- gsub(";", "\n", as.character(df_other$V9))
    } else if (nrow(df_gene) > 0) {
        df_other <- df_gene
        df_other$group <- "gene"
        df_other$V3_norm <- "gene"
        df_other$text <- gsub(";", "\n", as.character(df_other$V9))
    } else {
        df_other$group <- character(0)
        df_other$text <- character(0)
    }

    df <- data.frame(
        y = rep(1, nrow(df_other)),
        xstart = as.numeric(df_other$V4),
        xend = as.numeric(df_other$V5),
        group = factor(df_other$group),
        text = df_other$text,
        feature_type = as.character(df_other$V3_norm),
        seqid = as.character(df_other$V1),
        source = as.character(df_other$V2),
        feature_raw = as.character(df_other$V3),
        score = as.character(df_other$V6),
        strand = as.character(df_other$V7),
        phase = as.character(df_other$V8),
        attributes_raw = as.character(df_other$V9),
        stringsAsFactors = FALSE
    )

    df$largo <- df$xend - df$xstart + 1
    df_gene$largo <- as.numeric(df_gene$V5) - as.numeric(df_gene$V4) + 1

    list(df = df, df_gene = df_gene, df_transcript = df_transcript)
}

truncate_neighbor_label <- function(x, max_chars = 18) {
    x <- as.character(x %||% "None")
    x <- trimws(utils::URLdecode(x))
    x <- str_remove(x, regex("^(transcript|gene)\\s*:\\s*", ignore_case = TRUE))
    if (!nzchar(x)) {
        return("None")
    }
    if (nchar(x) <= max_chars) {
        return(x)
    }
    paste0(substr(x, 1, max_chars - 3), "...")
}

build_genomic_ruler_spec <- function(start_pos, end_pos, target_breaks = 4L, max_ticks = 6L) {
    start_num <- suppressWarnings(as.numeric(start_pos %||% NA_real_))
    end_num <- suppressWarnings(as.numeric(end_pos %||% NA_real_))
    if (!is.finite(start_num) || !is.finite(end_num)) {
        return(NULL)
    }
    if (end_num < start_num) {
        tmp <- start_num
        start_num <- end_num
        end_num <- tmp
    }
    if (end_num <= start_num) {
        return(NULL)
    }

    span_bp <- end_num - start_num
    max_abs_coord <- max(abs(c(start_num, end_num)))
    if (max_abs_coord >= 1e6) {
        unit_scale <- 1e6
        unit_label <- "Mb"
    } else if (max_abs_coord >= 1e3) {
        unit_scale <- 1e3
        unit_label <- "kb"
    } else {
        unit_scale <- 1
        unit_label <- "bp"
    }

    target_breaks <- suppressWarnings(as.integer(target_breaks %||% 4L))
    if (!is.finite(target_breaks) || target_breaks < 2L) target_breaks <- 4L
    max_ticks <- suppressWarnings(as.integer(max_ticks %||% 6L))
    if (!is.finite(max_ticks) || max_ticks < 2L) max_ticks <- 6L

    inner_ticks <- pretty(c(start_num, end_num), n = target_breaks)
    inner_ticks <- inner_ticks[
        is.finite(inner_ticks) &
            inner_ticks > start_num &
            inner_ticks < end_num
    ]
    # Keep endpoint labels visually distinct from the first/last pretty break.
    # The plot reserves a fixed central width, so a 10% edge gap is a safer
    # readability threshold than relying on the genomic span alone.
    min_edge_gap <- span_bp * 0.10
    inner_ticks <- inner_ticks[
        (inner_ticks - start_num) >= min_edge_gap &
            (end_num - inner_ticks) >= min_edge_gap
    ]
    max_inner <- max(0L, max_ticks - 2L)
    if (length(inner_ticks) > max_inner && max_inner > 0L) {
        keep_idx <- unique(as.integer(round(seq(1, length(inner_ticks), length.out = max_inner))))
        inner_ticks <- inner_ticks[keep_idx]
    } else if (max_inner == 0L) {
        inner_ticks <- numeric(0)
    }
    ticks <- sort(unique(c(start_num, inner_ticks, end_num)))

    span_display <- span_bp / unit_scale
    digits <- if (identical(unit_label, "bp")) {
        0L
    } else if (span_display >= 10) {
        1L
    } else if (span_display >= 0.01) {
        3L
    } else if (span_display >= 0.001) {
        4L
    } else {
        5L
    }
    scaled_ticks <- ticks / unit_scale
    labels <- formatC(scaled_ticks, format = "f", digits = digits, big.mark = ",")
    while (length(unique(labels)) < length(labels) && digits < 7L) {
        digits <- digits + 1L
        labels <- formatC(scaled_ticks, format = "f", digits = digits, big.mark = ",")
    }
    labels <- paste(labels, unit_label)

    exact_bp <- format(
        round(ticks),
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
    )
    data.frame(
        value = ticks,
        label = labels,
        exact_bp = exact_bp,
        hjust = c(0, rep(0.5, max(0L, length(ticks) - 2L)), 1),
        unit = unit_label,
        stringsAsFactors = FALSE
    )
}

prune_genomic_ruler_spec_for_width <- function(
    ruler_spec,
    start_pos,
    end_pos,
    display_width_in,
    text_size_mm = 3.55,
    min_gap_in = 0.10
) {
    if (is.null(ruler_spec) || !is.data.frame(ruler_spec) || nrow(ruler_spec) <= 1L) {
        return(ruler_spec)
    }

    start_num <- suppressWarnings(as.numeric(start_pos %||% NA_real_))
    end_num <- suppressWarnings(as.numeric(end_pos %||% NA_real_))
    width_in <- suppressWarnings(as.numeric(display_width_in %||% NA_real_))
    text_mm <- suppressWarnings(as.numeric(text_size_mm %||% 3.55))
    gap_in <- suppressWarnings(as.numeric(min_gap_in %||% 0.10))
    if (!is.finite(start_num) || !is.finite(end_num) || end_num <= start_num ||
        !is.finite(width_in) || width_in <= 0) {
        return(ruler_spec)
    }
    if (!is.finite(text_mm) || text_mm <= 0) text_mm <- 3.55
    if (!is.finite(gap_in) || gap_in < 0) gap_in <- 0.10

    # ggplot text size is expressed in millimetres. A typical UI sans-serif
    # glyph occupies about 54% of the font size horizontally.
    label_width_in <- pmax(
        0.18,
        nchar(as.character(ruler_spec$label), type = "width") * text_mm * 0.54 / 25.4
    )
    normalized_x <- (as.numeric(ruler_spec$value) - start_num) / (end_num - start_num)
    label_x_in <- normalized_x * width_in
    label_hjust <- suppressWarnings(as.numeric(ruler_spec$hjust))
    label_hjust[!is.finite(label_hjust)] <- 0.5
    label_left <- label_x_in - label_hjust * label_width_in
    label_right <- label_x_in + (1 - label_hjust) * label_width_in

    labels_fit <- function(keep_idx) {
        keep_idx <- sort(unique(as.integer(keep_idx)))
        if (length(keep_idx) <= 1L) return(TRUE)
        all(
            label_right[keep_idx[-length(keep_idx)]] + gap_in <=
                label_left[keep_idx[-1L]]
        )
    }

    n_ticks <- nrow(ruler_spec)
    endpoint_idx <- unique(c(1L, n_ticks))
    if (!labels_fit(endpoint_idx)) {
        midpoint <- (start_num + end_num) / 2
        unit_label <- as.character(ruler_spec$unit[1] %||% "bp")
        unit_scale <- switch(unit_label, Mb = 1e6, kb = 1e3, bp = 1, 1)
        first_numeric_label <- sub(
            paste0("\\s+", unit_label, "$"),
            "",
            as.character(ruler_spec$label[1])
        )
        digits <- if (grepl("\\.", first_numeric_label)) {
            nchar(sub("^.*\\.", "", first_numeric_label))
        } else {
            0L
        }
        midpoint_label <- paste(
            formatC(midpoint / unit_scale, format = "f", digits = digits, big.mark = ","),
            unit_label
        )
        return(data.frame(
            value = midpoint,
            label = midpoint_label,
            exact_bp = format(
                round(midpoint),
                big.mark = ",",
                scientific = FALSE,
                trim = TRUE
            ),
            hjust = 0.5,
            unit = unit_label,
            stringsAsFactors = FALSE
        ))
    }

    inner_idx <- if (n_ticks > 2L) seq.int(2L, n_ticks - 1L) else integer(0)
    best_keep <- endpoint_idx
    best_count <- length(best_keep)
    best_spacing <- -Inf
    subset_count <- 2^length(inner_idx)
    for (mask in seq.int(0, subset_count - 1L)) {
        selected_inner <- if (length(inner_idx) == 0L) {
            integer(0)
        } else {
            inner_idx[vapply(
                seq_along(inner_idx),
                function(bit) bitwAnd(mask, bitwShiftL(1L, bit - 1L)) != 0L,
                logical(1)
            )]
        }
        keep_idx <- sort(c(endpoint_idx, selected_inner))
        if (!labels_fit(keep_idx)) next
        spacing_score <- if (length(keep_idx) > 1L) {
            min(diff(label_x_in[keep_idx]))
        } else {
            width_in
        }
        if (length(keep_idx) > best_count ||
            (length(keep_idx) == best_count && spacing_score > best_spacing)) {
            best_keep <- keep_idx
            best_count <- length(keep_idx)
            best_spacing <- spacing_score
        }
    }

    ruler_spec[best_keep, , drop = FALSE]
}

assign_genomic_overlap_lanes <- function(starts, ends, padding_bp = 0) {
    starts_num <- suppressWarnings(as.numeric(starts))
    ends_num <- suppressWarnings(as.numeric(ends))
    n_intervals <- length(starts_num)
    if (n_intervals == 0L) return(integer(0))
    if (length(ends_num) != n_intervals || any(!is.finite(starts_num)) || any(!is.finite(ends_num))) {
        stop("Overlap lane coordinates must be finite vectors of equal length.")
    }

    interval_start <- pmin(starts_num, ends_num)
    interval_end <- pmax(starts_num, ends_num)
    padding_num <- suppressWarnings(as.numeric(padding_bp %||% 0))
    if (!is.finite(padding_num) || padding_num < 0) padding_num <- 0

    order_idx <- order(interval_start, interval_end, seq_len(n_intervals))
    lane_ends <- numeric(0)
    lane_index <- integer(n_intervals)
    for (idx in order_idx) {
        available <- which(interval_start[idx] > (lane_ends + padding_num))
        lane <- if (length(available) > 0L) available[1] else length(lane_ends) + 1L
        if (lane > length(lane_ends)) {
            lane_ends <- c(lane_ends, interval_end[idx])
        } else {
            lane_ends[lane] <- interval_end[idx]
        }
        lane_index[idx] <- lane
    }
    lane_index
}

has_genomic_overlap_context <- function(neighbor_context) {
    if (is.null(neighbor_context) || !is.list(neighbor_context)) return(FALSE)

    overlap_flags <- neighbor_context$flags
    if (is.list(overlap_flags)) {
        if (isTRUE(overlap_flags$has_overlap)) return(TRUE)
        overlap_count <- suppressWarnings(as.integer(overlap_flags$overlap_count %||% 0L))
        if (is.finite(overlap_count) && overlap_count > 0L) return(TRUE)
    }

    overlapping <- neighbor_context$overlapping
    if (is.data.frame(overlapping)) {
        if (nrow(overlapping) > 0L) return(TRUE)
    } else if (is.list(overlapping)) {
        if (!is.null(overlapping$neighbor_start) || !is.null(overlapping$neighbor_end)) {
            return(TRUE)
        }
        overlap_entries <- Filter(function(entry) {
            is.list(entry) || (is.data.frame(entry) && nrow(entry) > 0L)
        }, overlapping)
        if (length(overlap_entries) > 0L) return(TRUE)
    }

    legacy_neighbors <- list(neighbor_context$upstream, neighbor_context$downstream)
    any(vapply(legacy_neighbors, function(neighbor) {
        if (is.null(neighbor)) return(FALSE)
        distance <- if (is.data.frame(neighbor)) {
            suppressWarnings(as.numeric(neighbor$dist_bp))
        } else if (is.list(neighbor)) {
            suppressWarnings(as.numeric(neighbor$dist_bp %||% NA_real_))
        } else {
            NA_real_
        }
        any(is.finite(distance) & distance < 0)
    }, logical(1)))
}

classify_neighbor_relation <- function(target_start, target_end, neighbor_start, neighbor_end) {
    coords <- suppressWarnings(as.numeric(c(
        target_start, target_end, neighbor_start, neighbor_end
    )))
    names(coords) <- c("target_start", "target_end", "neighbor_start", "neighbor_end")
    if (any(!is.finite(coords))) {
        return(list(
            category = "unknown",
            relation = "unknown",
            relation_label = "Unknown relation",
            gap_bp = NA_real_,
            overlap_bp = NA_real_,
            clipped_start = NA_real_,
            clipped_end = NA_real_
        ))
    }

    target_lo <- min(coords[["target_start"]], coords[["target_end"]])
    target_hi <- max(coords[["target_start"]], coords[["target_end"]])
    neighbor_lo <- min(coords[["neighbor_start"]], coords[["neighbor_end"]])
    neighbor_hi <- max(coords[["neighbor_start"]], coords[["neighbor_end"]])

    if (neighbor_hi < target_lo) {
        gap_bp <- target_lo - neighbor_hi - 1
        adjacent <- identical(as.numeric(gap_bp), 0)
        return(list(
            category = if (adjacent) "adjacent" else "separated",
            relation = if (adjacent) "adjacent_left" else "separated_left",
            relation_label = if (adjacent) "Adjacent on left (0 bp gap)" else "Separated on left",
            gap_bp = gap_bp,
            overlap_bp = 0,
            clipped_start = NA_real_,
            clipped_end = NA_real_
        ))
    }
    if (neighbor_lo > target_hi) {
        gap_bp <- neighbor_lo - target_hi - 1
        adjacent <- identical(as.numeric(gap_bp), 0)
        return(list(
            category = if (adjacent) "adjacent" else "separated",
            relation = if (adjacent) "adjacent_right" else "separated_right",
            relation_label = if (adjacent) "Adjacent on right (0 bp gap)" else "Separated on right",
            gap_bp = gap_bp,
            overlap_bp = 0,
            clipped_start = NA_real_,
            clipped_end = NA_real_
        ))
    }

    clipped_start <- max(target_lo, neighbor_lo)
    clipped_end <- min(target_hi, neighbor_hi)
    overlap_bp <- clipped_end - clipped_start + 1
    same_span <- neighbor_lo == target_lo && neighbor_hi == target_hi
    neighbor_inside <- neighbor_lo >= target_lo && neighbor_hi <= target_hi
    target_inside <- neighbor_lo <= target_lo && neighbor_hi >= target_hi
    relation <- if (same_span) {
        "same_span"
    } else if (neighbor_inside) {
        "neighbor_inside_query"
    } else if (target_inside) {
        "neighbor_contains_query"
    } else if (neighbor_lo < target_lo) {
        "partial_overlap_left"
    } else {
        "partial_overlap_right"
    }
    relation_label <- switch(
        relation,
        same_span = "Same genomic span",
        neighbor_inside_query = "Neighbor lies inside query",
        neighbor_contains_query = "Neighbor contains query",
        partial_overlap_left = "Partial overlap from left",
        partial_overlap_right = "Partial overlap from right",
        "Overlap"
    )

    list(
        category = "overlap",
        relation = relation,
        relation_label = relation_label,
        gap_bp = NA_real_,
        overlap_bp = overlap_bp,
        clipped_start = clipped_start,
        clipped_end = clipped_end
    )
}

extract_composition_percentages <- function(composition_label) {
    lbl <- as.character(composition_label %||% "")
    if (!nzchar(lbl) || grepl("N/A", lbl, ignore.case = TRUE)) {
        return(NULL)
    }
    bases <- c("A", "T", "C", "G")
    vals <- vapply(bases, function(b) {
        m <- str_match(lbl, paste0("\\b", b, "\\s*=\\s*([0-9]+(?:\\.[0-9]+)?)%"))
        if (is.na(m[1, 2])) NA_real_ else as.numeric(m[1, 2])
    }, numeric(1))
    if (any(!is.finite(vals))) {
        return(NULL)
    }
    data.frame(base = bases, pct = vals, stringsAsFactors = FALSE)
}

.cgv_gene_plot_model_cache <- new.env(parent = emptyenv())
.cgv_gene_plot_model_cache_order <- character(0)
.cgv_gene_plot_model_version <- "v2"

make_gene_plot_model_data_key <- function(df, df_gene, df_transcript = NULL) {
    # Computed once per module from the actual immutable annotation data.
    # Gene names/spans alone cannot distinguish edited features or annotations.
    digest::digest(list(df, df_gene, df_transcript), algo = "sha256")
}

make_gene_plot_model_cache_key <- function(data_key, visual_mode = "compact",
                                            compact_feature_interactivity = TRUE,
                                            overlap_tol_bp = 2) {
    data_key <- trimws(as.character(data_key %||% ""))
    if (!nzchar(data_key)) return("")
    visual_mode <- match.arg(visual_mode, c("compact", "detailed"))
    paste(.cgv_gene_plot_model_version, data_key, visual_mode,
          identical(visual_mode, "compact") && isTRUE(compact_feature_interactivity),
          overlap_tol_bp, sep = "|")
}

get_gene_plot_model_cache_max_entries <- function() {
    raw <- suppressWarnings(as.integer(Sys.getenv("APP_GENE_PLOT_MODEL_CACHE_MAX_ENTRIES", "48")))
    if (!is.finite(raw) || raw < 0L) 48L else raw
}

get_gene_plot_model_cache <- function(cache_key) {
    key <- trimws(as.character(cache_key %||% ""))
    if (!nzchar(key) || !exists(key, envir = .cgv_gene_plot_model_cache, inherits = FALSE)) {
        return(NULL)
    }
    .cgv_gene_plot_model_cache_order <<- c(setdiff(.cgv_gene_plot_model_cache_order, key), key)
    get(key, envir = .cgv_gene_plot_model_cache, inherits = FALSE)
}

set_gene_plot_model_cache <- function(cache_key, model) {
    key <- trimws(as.character(cache_key %||% ""))
    max_entries <- get_gene_plot_model_cache_max_entries()
    if (!nzchar(key) || max_entries <= 0L || is.null(model)) {
        return(invisible(FALSE))
    }
    assign(key, model, envir = .cgv_gene_plot_model_cache)
    .cgv_gene_plot_model_cache_order <<- c(setdiff(.cgv_gene_plot_model_cache_order, key), key)
    while (length(.cgv_gene_plot_model_cache_order) > max_entries) {
        evict <- .cgv_gene_plot_model_cache_order[1]
        .cgv_gene_plot_model_cache_order <<- .cgv_gene_plot_model_cache_order[-1]
        if (exists(evict, envir = .cgv_gene_plot_model_cache, inherits = FALSE)) {
            rm(list = evict, envir = .cgv_gene_plot_model_cache)
        }
    }
    invisible(TRUE)
}

merge_gene_plot_ranges <- function(starts, ends) {
    starts <- as.numeric(starts)
    ends <- as.numeric(ends)
    keep <- is.finite(starts) & is.finite(ends)
    starts <- starts[keep]
    ends <- ends[keep]
    if (length(starts) == 0L) {
        return(data.frame(xstart = numeric(0), xend = numeric(0), stringsAsFactors = FALSE))
    }
    ord <- order(starts, ends)
    starts <- starts[ord]
    ends <- ends[ord]
    group_start <- c(TRUE, starts[-1] > cummax(ends)[-length(ends)])
    group_id <- cumsum(group_start)
    data.frame(
        xstart = as.numeric(tapply(starts, group_id, min)),
        xend = as.numeric(tapply(ends, group_id, max)),
        stringsAsFactors = FALSE
    )
}

assign_features_to_rects <- function(feature_starts, feature_ends, rect_starts, rect_ends, tolerance_bp = 2) {
    n_rect <- length(rect_starts)
    if (length(feature_starts) == 0L || n_rect == 0L) {
        return(vector("list", n_rect))
    }
    feature_starts <- as.numeric(feature_starts)
    feature_ends <- as.numeric(feature_ends)
    rect_starts <- as.numeric(rect_starts)
    rect_ends <- as.numeric(rect_ends)
    tol <- as.numeric(tolerance_bp %||% 0)

    pairs <- lapply(seq_along(feature_starts), function(j) {
        first <- findInterval(feature_starts[j], rect_ends + tol + 1) + 1L
        last <- findInterval(feature_ends[j] + tol, rect_starts)
        if (first > n_rect || last < first) return(NULL)
        cbind(rect = seq.int(first, min(last, n_rect)), feature = j)
    })
    pairs <- Filter(Negate(is.null), pairs)
    if (length(pairs) == 0L) {
        return(vector("list", n_rect))
    }
    pair_matrix <- do.call(rbind, pairs)
    split_features <- split(pair_matrix[, "feature"], factor(pair_matrix[, "rect"], levels = seq_len(n_rect)))
    unname(lapply(split_features, as.integer))
}

prepare_gene_plot_model <- function(df, df_gene, df_transcript = NULL, visual_mode = "compact",
                                    compact_feature_interactivity = TRUE, overlap_tol_bp = 2) {
    visual_mode <- match.arg(visual_mode, c("compact", "detailed"))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df <- df[is.finite(df$xstart) & is.finite(df$xend), , drop = FALSE]
    df_gene <- as.data.frame(df_gene, stringsAsFactors = FALSE)
    df_gene$V4 <- suppressWarnings(as.numeric(df_gene$V4))
    df_gene$V5 <- suppressWarnings(as.numeric(df_gene$V5))
    df_gene <- df_gene[is.finite(df_gene$V4) & is.finite(df_gene$V5), , drop = FALSE]
    if (is.null(df_transcript) || nrow(df_transcript) == 0L) {
        df_transcript <- data.frame()
    } else {
        df_transcript <- as.data.frame(df_transcript, stringsAsFactors = FALSE)
        df_transcript$V4 <- suppressWarnings(as.numeric(df_transcript$V4))
        df_transcript$V5 <- suppressWarnings(as.numeric(df_transcript$V5))
    }

    compact_rects <- data.frame(xstart = numeric(0), xend = numeric(0), stringsAsFactors = FALSE)
    feature_to_rect <- list()
    if (identical(visual_mode, "compact")) {
        compact_rects <- merge_gene_plot_ranges(df$xstart, df$xend)
        feature_to_rect <- if (isTRUE(compact_feature_interactivity)) {
            assign_features_to_rects(
                df$xstart, df$xend, compact_rects$xstart, compact_rects$xend, overlap_tol_bp
            )
        } else {
            vector("list", nrow(compact_rects))
        }
    }

    list(
        version = .cgv_gene_plot_model_version,
        df = df,
        df_gene = df_gene,
        df_transcript = df_transcript,
        compact_rects = compact_rects,
        feature_to_rect = feature_to_rect
    )
}

# Función auxiliar para crear el gráfico (compartida)
create_gene_plot <- function(df, df_gene, df_transcript = NULL, current_transcript_length, length_difference, composicion_secuencia, gene_length_label, transcript_length_label, neighbor_context = NULL, visual_mode = "compact", width_svg = 22, height_svg = 1.75, organism_label = NULL, annotation_file_path = NULL, use_report_map = FALSE, report_path = "", plot_id = NULL, plot_context = NULL, genome_fasta_path = NULL, is_dark_theme = FALSE, is_colorblind_mode = FALSE, gene_display_name = NULL, precomputed_genomic_span = NULL, model_cache_key = NULL, orientation_mode = "genomic", caller_started_at = NULL) {
    entry_delay_ms <- app_perf_elapsed_ms(caller_started_at)
    visual_mode <- match.arg(visual_mode, c("compact", "detailed"))
    orientation_mode <- normalize_gene_plot_orientation_mode(orientation_mode)
    plot_perf <- app_perf_new_run(sprintf("PLOT_BUILD-%s", as.character(plot_id %||% "NA")))
    plot_perf_context <- switch(
        tolower(trimws(as.character(plot_context %||% ""))),
        homologous = "HOMO_PLOT_BUILD",
        orthologous = "ORTHO_PLOT_BUILD",
        "PLOT_BUILD"
    )
    app_perf_mark(plot_perf, sprintf("start mode=%s", visual_mode), plot_perf_context)
    if (is.finite(entry_delay_ms)) {
        app_perf_mark_ms(plot_perf, "create_gene_plot_entry_delay_ms", entry_delay_ms, plot_perf_context)
    }
    model_t0 <- app_perf_now()
    is_compact_mode <- identical(visual_mode, "compact")
    compact_feature_interactivity <- !is_compact_mode || is_compact_feature_interactivity_enabled()
    prepared_model_key <- make_gene_plot_model_cache_key(
        model_cache_key, visual_mode, compact_feature_interactivity, overlap_tol_bp = 2
    )
    prepared_model <- get_gene_plot_model_cache(prepared_model_key)
    model_cache_hit <- !is.null(prepared_model)
    if (!model_cache_hit) {
        prepared_model <- prepare_gene_plot_model(
            df,
            df_gene,
            df_transcript,
            visual_mode = visual_mode,
            compact_feature_interactivity = compact_feature_interactivity,
            overlap_tol_bp = 2
        )
        set_gene_plot_model_cache(prepared_model_key, prepared_model)
    }
    df <- prepared_model$df
    df_gene <- prepared_model$df_gene
    df_transcript <- prepared_model$df_transcript
    app_perf_mark(plot_perf, "data normalize done", plot_perf_context)
    app_perf_mark(plot_perf, sprintf("model_cache_hit=%d", as.integer(model_cache_hit)), plot_perf_context)
    is_dark_theme <- isTRUE(is_dark_theme)
    is_colorblind_mode <- isTRUE(is_colorblind_mode)
    gene_line_color <- if (is_dark_theme) "#C7D6E4" else "#2C3E50"
    gene_direction_color <- if (is_dark_theme) "#9EB2C4" else "#97A6B2"
    center_label_col <- if (is_dark_theme) "#E6F0FA" else "#4B6072"
    panel_bg_fill <- if (is_dark_theme) "#102438" else "#FFFFFF"
    format_bp <- function(x) format(as.integer(round(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
    pick_first_value <- function(...) {
        vals <- list(...)
        for (v in vals) {
            vv <- as.character(v %||% "")
            if (length(vv) > 0 && !is.na(vv[1]) && nzchar(trimws(vv[1]))) {
                return(vv[1])
            }
        }
        "N/A"
    }
    esc_html <- function(x) {
        out <- as.character(x %||% "")
        out <- gsub("&", "&amp;", out, fixed = TRUE)
        out <- gsub("<", "&lt;", out, fixed = TRUE)
        out <- gsub(">", "&gt;", out, fixed = TRUE)
        out
    }
    genome_fasta_path <- trimws(as.character(genome_fasta_path %||% ""))
    has_gc_source <- nzchar(genome_fasta_path) && file.exists(genome_fasta_path)
    needs_feature_gc <- isTRUE(has_gc_source)
    format_gc_pct <- function(gc_pct) {
        if (!is.finite(gc_pct)) {
            return("N/A")
        }
        sprintf("%.2f%%", as.numeric(gc_pct))
    }
    calc_gc_pct <- function(seq_txt) {
        seq_txt <- toupper(gsub("\\s+", "", as.character(seq_txt %||% "")))
        if (!nzchar(seq_txt)) {
            return(NA_real_)
        }
        known_bases <- gsub("[^ATCG]", "", seq_txt)
        known_total <- nchar(known_bases)
        if (known_total <= 0) {
            return(NA_real_)
        }
        gc_count <- nchar(gsub("[^GC]", "", known_bases))
        round(100 * gc_count / known_total, 2)
    }
    calc_gc_pct_batch <- function(seqs) {
        seqs <- toupper(gsub("\\s+", "", as.character(seqs)))
        known <- gsub("[^ATCG]", "", seqs)
        known_total <- nchar(known)
        gc_count <- nchar(gsub("[^GC]", "", known))
        result <- ifelse(known_total > 0L, round(100 * gc_count / known_total, 2), NA_real_)
        result[!nzchar(seqs)] <- NA_real_
        result
    }
    batch_gc_pct <- function(starts, ends) {
        if (!nzchar(full_transcript_seq)) return(rep(NA_real_, length(starts)))
        start_i <- as.integer(round(pmin(starts, ends)))
        end_i <- as.integer(round(pmax(starts, ends)))
        rel_start <- start_i - transcript_min + 1L
        rel_end <- end_i - transcript_min + 1L
        seq_len_total <- nchar(full_transcript_seq)
        valid <- rel_start >= 1L & rel_end <= seq_len_total
        result <- rep(NA_real_, length(starts))
        if (any(valid)) {
            if (!is.null(gc_index)) {
                result[valid] <- gc_percent_from_index(gc_index, rel_start[valid], rel_end[valid])
            } else {
                seqs <- substring(full_transcript_seq, rel_start[valid], rel_end[valid])
                result[valid] <- calc_gc_pct_batch(seqs)
            }
        }
        result
    }
    transcript_min <- min(df$xstart, na.rm = TRUE)
    transcript_max <- max(df$xend, na.rm = TRUE)

    full_transcript_seq <- ""
    if (isTRUE(needs_feature_gc) && is.finite(transcript_min) && is.finite(transcript_max)) {
      precomp <- trimws(as.character(precomputed_genomic_span %||% ""))
      if (nzchar(precomp)) {
        full_transcript_seq <- precomp
      } else {
        full_transcript_seq <- tryCatch(
          extract_sequence_from_fasta(genome_fasta_path, df$seqid[1], as.integer(transcript_min), as.integer(transcript_max)),
          error = function(e) ""
        )
      }
    }
    app_perf_mark(plot_perf, sprintf("gc source ready len=%d", as.integer(nchar(full_transcript_seq %||% ""))), plot_perf_context)
    gc_index <- make_genomic_gc_index(full_transcript_seq)
    # ggplot objects can retain this call environment; do not retain the prefix
    # arrays after the tooltip values have been materialized in the widget.
    on.exit({ gc_index <- NULL }, add = TRUE)

    get_feature_gc_pct <- function(seqid, start_pos, end_pos) {
      if (!nzchar(full_transcript_seq)) return(NA_real_)
      
      start_i <- as.integer(round(min(start_pos, end_pos)))
      end_i <- as.integer(round(max(start_pos, end_pos)))
      
      # Calcular índices relativos al bloque extraído
      rel_start <- start_i - transcript_min + 1
      rel_end <- end_i - transcript_min + 1
      
      # Validación para evitar errores si el exón sale del margen extraído
      if (rel_start < 1 || rel_end > nchar(full_transcript_seq)) return(NA_real_)
      if (!is.null(gc_index)) return(gc_percent_from_index(gc_index, rel_start, rel_end))
      
      seq_txt <- substr(full_transcript_seq, rel_start, rel_end)
      calc_gc_pct(seq_txt)
    }
    get_region_gc_pct <- function(start_pos, end_pos) {
      get_feature_gc_pct("", start_pos, end_pos)
    }
    feature_label_map <- function(ft) {
        ft <- tolower(trimws(as.character(ft %||% "")))
        case_when(
            ft == "exon" ~ "EXON",
            ft == "cds" ~ "CDS",
            ft == "start_codon" ~ "START CODON",
            ft == "stop_codon" ~ "STOP CODON",
            grepl("utr", ft) ~ "UTR",
            TRUE ~ toupper(ft)
        )
    }
    feature_priority_map <- function(ft) {
        ft <- tolower(trimws(as.character(ft %||% "")))
        case_when(
            ft == "exon" ~ 5,
            ft == "cds" ~ 4,
            grepl("utr", ft) ~ 3,
            ft %in% c("start_codon", "stop_codon") ~ 2,
            TRUE ~ 1
        )
    }

    extract_feature_id <- function(attr_txt) {
        attrs <- parse_gff_attributes(as.character(attr_txt %||% ""))
        pick_first_value(
            attrs[["id"]][1],
            attrs[["name"]][1],
            attrs[["gene_id"]][1],
            attrs[["transcript_id"]][1],
            attrs[["parent"]][1]
        )
    }

    format_attr_lines <- function(attrs, exclude_keys = character()) {
        keys <- names(attrs)
        keys <- keys[!is.na(keys) & nzchar(keys)]
        if (length(keys) == 0) {
            return(character())
        }
        if (length(exclude_keys) > 0) {
            keys <- keys[!(tolower(keys) %in% tolower(exclude_keys))]
        }
        if (length(keys) == 0) {
            return(character())
        }
        vapply(keys, function(k) {
            vals <- attrs[[k]]
            vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
            val_txt <- if (length(vals) == 0) "N/A" else paste(unique(vals), collapse = ", ")
            lbl <- stringr::str_to_title(gsub("_", " ", k))
            sprintf("<b>%s:</b> %s", esc_html(lbl), esc_html(val_txt))
        }, character(1))
    }

    if (identical(visual_mode, "compact")) {
        df <- df %>%
            mutate(
                feature_type_norm = tolower(trimws(as.character(feature_type))),
                feature_uid = paste0("feature_", seq_len(n()))
            )
    } else {
        df <- df %>%
            mutate(
                feature_type_norm = tolower(trimws(as.character(feature_type))),
                feature_label = feature_label_map(feature_type_norm),
                feature_priority = feature_priority_map(feature_type_norm),
                feature_draw_rank = case_when(
                    feature_type_norm == "exon" ~ 1,
                    feature_type_norm == "cds" ~ 2,
                    grepl("utr", feature_type_norm) ~ 3,
                    feature_type_norm %in% c("start_codon", "stop_codon") ~ 4,
                    TRUE ~ 5
                )
            ) %>%
            arrange(feature_draw_rank, xstart, xend) %>%
            mutate(feature_uid = paste0("feature_", row_number()))
    }
    # Parse GFF attributes ONCE per feature in detailed mode.
    # feature_attrs_cache is computed first so both feature_id extraction and
    # tooltip building can reuse the already-parsed list, avoiding a second full
    # lapply(parse_gff_attributes) pass that was previously duplicating this work.
    feature_attrs_cache <- if (identical(visual_mode, "detailed")) {
        lapply(df$attributes_raw, function(attr_txt) {
            parse_gff_attributes(as.character(attr_txt %||% ""))
        })
    } else {
        vector("list", nrow(df))
    }
    df$feature_id <- if (identical(visual_mode, "detailed")) {
        vapply(seq_len(nrow(df)), function(i) {
            attrs <- feature_attrs_cache[[i]]
            pick_first_value(
                attrs[["id"]][1],
                attrs[["name"]][1],
                attrs[["gene_id"]][1],
                attrs[["transcript_id"]][1],
                attrs[["parent"]][1]
            )
        }, character(1))
    } else {
        rep("N/A", nrow(df))
    }
    if (isTRUE(needs_feature_gc)) {
        df$gc_pct <- batch_gc_pct(df$xstart, df$xend)
        df$gc_label <- ifelse(is.finite(df$gc_pct), sprintf("%.2f%%", df$gc_pct), "N/A")
    } else {
        df$gc_pct <- rep(NA_real_, nrow(df))
        df$gc_label <- rep("N/A", nrow(df))
    }

    overlap_tol_bp <- 2
    app_perf_mark(plot_perf, "feature typing done", plot_perf_context)
    app_perf_mark(plot_perf, sprintf("compact_feature_interactivity=%s", as.character(isTRUE(compact_feature_interactivity))), plot_perf_context)
    build_overlap_html <- function(idx) {
        idx <- as.integer(idx)
        idx <- idx[idx >= 1 & idx <= nrow(df)]
        if (length(idx) == 0) {
            return("<b style='color:var(--app-message-accent);font-weight:700;'>Feature In This Region</b><br/>N/A")
        }
        if (!identical(visual_mode, "detailed")) {
            feature_count <- length(idx)
            exon_n <- sum(df$feature_type_norm[idx] == "exon", na.rm = TRUE)
            cds_n <- sum(df$feature_type_norm[idx] == "cds", na.rm = TRUE)
            utr_n <- sum(grepl("utr", df$feature_type_norm[idx]), na.rm = TRUE)
            codon_n <- sum(df$feature_type_norm[idx] %in% c("start_codon", "stop_codon"), na.rm = TRUE)
            region_gc <- get_region_gc_pct(min(df$xstart[idx], na.rm = TRUE), max(df$xend[idx], na.rm = TRUE))
            gc_txt <- format_gc_pct(region_gc)
            return(
                paste0(
                    "<b style='color:var(--app-message-accent);font-weight:700;'>Features In This Region</b><br/>",
                    "<b>Total Features:</b> ", as.integer(feature_count), "<br/>",
                    "<b>Exons:</b> ", as.integer(exon_n), "<br/>",
                    "<b>CDS:</b> ", as.integer(cds_n), "<br/>",
                    "<b>UTR:</b> ", as.integer(utr_n), "<br/>",
                    "<b>Codons:</b> ", as.integer(codon_n), "<br/>",
                    "<b>%GC (mean):</b> ", gc_txt
                )
            )
        }
        idx <- idx[order(-df$feature_priority[idx], df$xstart[idx], df$xend[idx], df$feature_label[idx])]
        attrs_list <- feature_attrs_cache[idx]
        sep_html <- "<br/><hr style='border:none;border-top:1px solid #B6CCC6;margin:5px 0;'/>"
        shared_keys <- character()
        shared_lines <- character()
        if (length(idx) > 1) {
            key_lists <- lapply(attrs_list, function(a) names(a)[!is.na(names(a)) & nzchar(names(a))])
            common_keys <- if (length(key_lists) > 0) Reduce(intersect, key_lists) else character()
            if (length(common_keys) > 0) {
                shared_lines <- vapply(common_keys, function(k) {
                    vals_per_feature <- lapply(attrs_list, function(a) {
                        v <- a[[k]]
                        v <- v[!is.na(v) & nzchar(trimws(v))]
                        sort(unique(v))
                    })
                    sig <- vapply(vals_per_feature, function(v) paste(v, collapse = " | "), character(1))
                    if (length(sig) == 0 || any(!nzchar(sig))) {
                        return(NA_character_)
                    }
                    if (length(unique(sig)) != 1) {
                        return(NA_character_)
                    }
                    lbl <- stringr::str_to_title(gsub("_", " ", k))
                    sprintf("<b>%s:</b> %s", esc_html(lbl), esc_html(sig[1]))
                }, character(1))
                keep_shared <- !is.na(shared_lines)
                shared_lines <- shared_lines[keep_shared]
                shared_keys <- common_keys[keep_shared]
            }
        }
        feature_blocks <- vapply(seq_along(idx), function(k) {
            j <- idx[k]
            fid <- as.character(df$feature_id[j] %||% "N/A")
            sid <- if (is.na(fid) || !nzchar(trimws(fid)) || fid == "N/A") "N/A" else esc_html(fid)

            feature_attr_lines <- format_attr_lines(attrs_list[[k]], exclude_keys = shared_keys)
            if (length(feature_attr_lines) == 0) {
                raw_txt <- utils::URLdecode(as.character(df$attributes_raw[j] %||% ""))
                raw_txt <- trimws(raw_txt)
                if (nzchar(raw_txt)) {
                    feature_attr_lines <- sprintf("<b>Raw Attributes:</b> %s", esc_html(raw_txt))
                }
            }
            feature_attr_block <- if (length(feature_attr_lines) == 0) {
                "<b>Feature Attributes:</b> N/A"
            } else {
                paste0("<b>Feature Attributes:</b><br/>", paste(feature_attr_lines, collapse = "<br/>"))
            }
            sprintf(
                "<b style='color:var(--app-message-accent);font-weight:700;'>%s</b><br/><b>Start:</b> %s<br/><b>End:</b> %s<br/><b>ID:</b> %s<br/><b>GC:</b> %s<br/>%s",
                esc_html(df$feature_label[j]),
                format_bp(df$xstart[j]),
                format_bp(df$xend[j]),
                sid,
                df$gc_label[j],
                feature_attr_block
            )
        }, character(1))
        feature_blocks <- unique(feature_blocks)
        shared_block <- if (length(shared_lines) > 0) {
            paste0(
                sep_html,
                "<b style='color:var(--app-message-accent);font-weight:700;'>Shared Attributes</b><br/>",
                paste(shared_lines, collapse = "<br/>")
            )
        } else {
            ""
        }
        title_txt <- if (length(idx) > 1) "Overlapping Features In This Region" else "Feature In This Region"
        paste0(
            "<b style='color:var(--app-message-accent);font-weight:700;'>", title_txt, "</b><br/>",
            paste(feature_blocks, collapse = sep_html),
            shared_block
        )
    }
    app_perf_mark(plot_perf, "tooltip builders ready", plot_perf_context)

    build_single_feature_attr_block <- function(attrs, attr_txt) {
        attr_lines <- format_attr_lines(attrs)
        if (length(attr_lines) > 0) {
            return(paste0("<b>Feature Attributes:</b><br/>", paste(attr_lines, collapse = "<br/>")))
        }
        raw_txt <- utils::URLdecode(as.character(attr_txt %||% ""))
        raw_txt <- trimws(raw_txt)
        if (nzchar(raw_txt)) {
            return(sprintf("<b>Raw Attributes:</b> %s", esc_html(raw_txt)))
        }
        "<b>Feature Attributes:</b> N/A"
    }
    if (identical(visual_mode, "detailed")) {
        feature_attr_html <- vapply(seq_len(nrow(df)), function(i) {
            build_single_feature_attr_block(feature_attrs_cache[[i]], df$attributes_raw[i])
        }, character(1))

        df <- df %>%
            mutate(
                # Solo exón/CDS muestran atributos completos; el resto usa tooltip liviano.
                tooltip_html_detailed = case_when(
                    feature_type_norm %in% c("exon", "cds") ~ sprintf(
                        "<b style='color:var(--app-message-accent);font-weight:700;'>%s</b><br/><b>Seqid:</b> %s<br/><b>Source:</b> %s<br/><b>Type:</b> %s<br/><b>Start:</b> %s<br/><b>End:</b> %s<br/><b>Score:</b> %s<br/><b>Strand:</b> %s<br/><b>Phase:</b> %s<br/><b>Length:</b> %s bp<br/><b>GC:</b> %s<br/><hr style='border:none;border-top:1px solid #B6CCC6;margin:5px 0;'/>%s",
                        feature_label, esc_html(seqid), esc_html(source), esc_html(feature_raw), format_bp(xstart), format_bp(xend), esc_html(score), esc_html(strand), esc_html(phase), format_bp(largo), gc_label, feature_attr_html
                    ),
                    TRUE ~ sprintf(
                        "<b style='color:var(--app-message-accent);font-weight:700;'>%s</b><br/><b>Start:</b> %s<br/><b>End:</b> %s<br/><b>Length:</b> %s bp<br/><b>GC:</b> %s",
                        feature_label, format_bp(xstart), format_bp(xend), format_bp(largo), gc_label
                    )
                )
            )
    } else {
        df$tooltip_html_detailed <- rep("", nrow(df))
    }

    # Build intron segments from exon features only (not CDS/UTR).
    row_ft <- tolower(as.character(df$feature_type %||% rep("", nrow(df))))
    exon_mask <- row_ft == "exon"
    intron_df <- data.frame()
    if (any(exon_mask)) {
        exon_ranges <- data.frame(
            seqid = as.character(df$seqid[exon_mask]),
            xstart = suppressWarnings(as.numeric(df$xstart[exon_mask])),
            xend = suppressWarnings(as.numeric(df$xend[exon_mask])),
            stringsAsFactors = FALSE
        )
        keep_ex <- !is.na(exon_ranges$seqid) &
            nzchar(exon_ranges$seqid) &
            is.finite(exon_ranges$xstart) &
            is.finite(exon_ranges$xend)
        exon_ranges <- exon_ranges[keep_ex, , drop = FALSE]
        if (nrow(exon_ranges) > 1) {
            ord_ex <- order(exon_ranges$seqid, exon_ranges$xstart, exon_ranges$xend)
            exon_ranges <- exon_ranges[ord_ex, , drop = FALSE]
            exon_ranges <- exon_ranges[!duplicated(paste(exon_ranges$seqid, exon_ranges$xstart, exon_ranges$xend, sep = "::")), , drop = FALSE]
            if (nrow(exon_ranges) > 1) {
                prev_seqid <- exon_ranges$seqid[-nrow(exon_ranges)]
                prev_xend <- exon_ranges$xend[-nrow(exon_ranges)]
                curr_seqid <- exon_ranges$seqid[-1]
                curr_xstart <- exon_ranges$xstart[-1]
                gap_keep <- (curr_seqid == prev_seqid) & (curr_xstart > (prev_xend + 1))
                if (any(gap_keep)) {
                    intron_start_bp <- as.integer(round(prev_xend[gap_keep] + 1))
                    intron_end_bp <- as.integer(round(curr_xstart[gap_keep] - 1))
                    intron_bp <- as.integer(round(curr_xstart[gap_keep] - prev_xend[gap_keep] - 1))
                    intron_tmp <- data.frame(
                        seqid = curr_seqid[gap_keep],
                        intron_start = as.numeric(prev_xend[gap_keep] + 1),
                        intron_end = as.numeric(curr_xstart[gap_keep] - 1),
                        intron_start_bp = intron_start_bp,
                        intron_end_bp = intron_end_bp,
                        intron_bp = intron_bp,
                        stringsAsFactors = FALSE
                    )
                    keep_intron_tmp <- is.finite(intron_tmp$intron_start) &
                        is.finite(intron_tmp$intron_end) &
                        (intron_tmp$intron_end >= intron_tmp$intron_start) &
                        is.finite(intron_tmp$intron_bp) &
                        (intron_tmp$intron_bp > 0)
                    intron_tmp <- intron_tmp[keep_intron_tmp, , drop = FALSE]
                    if (nrow(intron_tmp) > 0) {
                        if (isTRUE(needs_feature_gc)) {
                            intron_gc <- batch_gc_pct(intron_tmp$intron_start_bp, intron_tmp$intron_end_bp)
                            intron_gc_label <- ifelse(is.finite(intron_gc), sprintf("%.2f%%", intron_gc), "N/A")
                        } else {
                            intron_gc_label <- rep("N/A", nrow(intron_tmp))
                        }
                        intron_tmp$tooltip <- sprintf(
                            "<b style='color:var(--app-message-accent);font-weight:700;'>INTRON</b><br/><b>Start:</b> %s<br/><b>End:</b> %s<br/><b>Length:</b> %s bp<br/><b>GC:</b> %s",
                            format(as.integer(round(intron_tmp$intron_start_bp)), big.mark = ","),
                            format(as.integer(round(intron_tmp$intron_end_bp)), big.mark = ","),
                            format(as.integer(round(intron_tmp$intron_bp)), big.mark = ","),
                            intron_gc_label
                        )
                        intron_tmp$data_id <- paste0("intron_", seq_len(nrow(intron_tmp)))
                        intron_df <- intron_tmp[, c("intron_start", "intron_end", "intron_bp", "tooltip", "data_id"), drop = FALSE]
                    }
                }
            }
        }
    }
    app_perf_mark(plot_perf, sprintf("introns done n=%d", as.integer(nrow(intron_df))), plot_perf_context)
    if (nrow(intron_df) > 0) {
        intron_df$tooltip <- as.character(intron_df$tooltip)
        intron_df$data_id <- as.character(intron_df$data_id)
        keep_intron <- is.finite(intron_df$intron_start) &
            is.finite(intron_df$intron_end) &
            (intron_df$intron_end > intron_df$intron_start) &
            !is.na(intron_df$tooltip) &
            nzchar(trimws(intron_df$tooltip)) &
            !is.na(intron_df$data_id) &
            nzchar(trimws(intron_df$data_id))
        intron_df <- intron_df[keep_intron, , drop = FALSE]
    }

    # Reserve the deeper lane only when an overlap label actually needs it.
    # Cards without overlaps can keep the primary gene closer to the ruler
    # while the flanking-neighbor labels remain in their independent side
    # panels.
    has_overlap_layout <- has_genomic_overlap_context(neighbor_context)
    structure_y_offset <- if (identical(visual_mode, "compact")) {
        if (isTRUE(has_overlap_layout)) -0.070 else -0.025
    } else {
        if (isTRUE(has_overlap_layout)) -0.025 else 0
    }
    structure_y_base <- 1 + structure_y_offset

    df_rect <- if (visual_mode == "compact") {
        compact_rects <- prepared_model$compact_rects
        if (nrow(compact_rects) > 0) {
            compact_rects$plot_group <- "compact"
            compact_rects$ymin_feat <- 0.965 + structure_y_offset
            compact_rects$ymax_feat <- 1.035 + structure_y_offset
            compact_rects$feature_uid <- paste0("compact_region_", seq_len(nrow(compact_rects)))
            if (isTRUE(compact_feature_interactivity)) {
                # Pre-assign features to merged rects only when compact feature
                # tooltips are enabled; otherwise girafe has far fewer SVG hooks.
                feature_to_rect <- prepared_model$feature_to_rect
                compact_rects$plot_tooltip <- vapply(seq_len(nrow(compact_rects)), function(i) {
                    idx <- feature_to_rect[[i]]
                    region_length <- as.numeric(compact_rects$xend[i] - compact_rects$xstart[i] + 1)
                    paste0(
                        "<b style='color:var(--app-message-accent);font-weight:700;'>Merged Gene Region</b><br/>",
                        "<b>Start:</b> ", format_bp(compact_rects$xstart[i]), "<br/>",
                        "<b>End:</b> ", format_bp(compact_rects$xend[i]), "<br/>",
                        "<b>Length:</b> ", format_bp(region_length), " bp<br/>",
                        "<hr style='border:none;border-top:1px solid #B6CCC6;margin:5px 0;'/>",
                        build_overlap_html(idx)
                    )
                }, character(1))
            } else {
                compact_rects$plot_tooltip <- rep("", nrow(compact_rects))
            }
        } else {
            compact_rects <- data.frame(
                xstart = numeric(0), xend = numeric(0),
                plot_group = character(0),
                ymin_feat = numeric(0), ymax_feat = numeric(0),
                feature_uid = character(0),
                plot_tooltip = character(0),
                stringsAsFactors = FALSE
            )
        }
        compact_rects
    } else {
        df %>%
            mutate(
                plot_group = case_when(
                    feature_type_norm == "gene" ~ "gene",
                    feature_type_norm == "exon" ~ "exon",
                    feature_type_norm == "cds" ~ "cds",
                    grepl("utr", feature_type_norm) ~ "utr",
                    feature_type_norm %in% c("start_codon", "stop_codon") ~ "codon",
                    TRUE ~ "other"
                ),
                ymin_feat = case_when(
                    feature_type_norm == "exon" ~ 0.960,
                    feature_type_norm == "cds" ~ 0.915,
                    grepl("utr", feature_type_norm) ~ 0.870,
                    feature_type_norm %in% c("start_codon", "stop_codon") ~ 0.825,
                    TRUE ~ 0.780
                ),
                ymax_feat = case_when(
                    feature_type_norm == "exon" ~ 1.040,
                    feature_type_norm == "cds" ~ 0.955,
                    grepl("utr", feature_type_norm) ~ 0.910,
                    feature_type_norm %in% c("start_codon", "stop_codon") ~ 0.865,
                    TRUE ~ 0.820
                ),
                ymin_feat = ymin_feat + structure_y_offset,
                ymax_feat = ymax_feat + structure_y_offset,
                plot_tooltip = tooltip_html_detailed
            ) %>%
            arrange(feature_draw_rank, xstart, xend)
    }
    app_perf_mark(plot_perf, sprintf("rectangles done n=%d", as.integer(nrow(df_rect))), plot_perf_context)
    if (nrow(df_rect) > 0) {
        df_rect$plot_tooltip <- as.character(df_rect$plot_tooltip)
        df_rect$feature_uid <- as.character(df_rect$feature_uid)
        keep_rect <- is.finite(df_rect$xstart) &
            is.finite(df_rect$xend) &
            is.finite(df_rect$ymin_feat) &
            is.finite(df_rect$ymax_feat) &
            (df_rect$xend > df_rect$xstart) &
            !is.na(df_rect$feature_uid) &
            nzchar(trimws(df_rect$feature_uid))
        if (isTRUE(compact_feature_interactivity)) {
            keep_rect <- keep_rect &
                !is.na(df_rect$plot_tooltip) &
                nzchar(trimws(df_rect$plot_tooltip))
        }
        df_rect <- df_rect[keep_rect, , drop = FALSE]
    }
    app_perf_mark(
        plot_perf,
        sprintf("features ready rect=%d intron=%d", as.integer(nrow(df_rect)), as.integer(nrow(intron_df))),
        plot_perf_context
    )

    custom_colors <- get_transcript_feature_palette(
        is_dark_theme = is_dark_theme,
        is_colorblind_mode = is_colorblind_mode
    )
    if (visual_mode == "compact") {
        neighbor_rule_col <- "#5F7282"
        neighbor_dash_col <- gene_direction_color
        promoter_dash_col <- "#2C93AB"
        neighbor_tick_col <- "#70818D"
        neighbor_label_col <- "#4F6270"
        neighbor_tick_size <- 3.2
        neighbor_name_size <- 3.4
        center_gene_size <- 4.1
        center_gene_label_y <- 1.075
        neighbor_gene_label_y <- 0.955 + structure_y_offset
        neighbor_dist_size <- 2.7
        neighbor_marker_size <- 2.8
        neighbor_marker_fill <- "#5D8FB8"
        neighbor_marker_col <- "#3A6D95"
        neighbor_marker_stroke <- 0.42
    } else {
        neighbor_rule_col <- "#34495E"
        neighbor_dash_col <- gene_direction_color
        promoter_dash_col <- "#2587A3"
        neighbor_tick_col <- "#5D6D7E"
        neighbor_label_col <- "#2C3E50"
        neighbor_tick_size <- 3.4
        neighbor_name_size <- 3.6
        center_gene_size <- 4.2
        center_gene_label_y <- 1.100
        neighbor_gene_label_y <- 0.939 + structure_y_offset
        neighbor_dist_size <- 2.9
        neighbor_marker_size <- 3.0
        neighbor_marker_fill <- "#2C7FB8"
        neighbor_marker_col <- "#1B4F72"
        neighbor_marker_stroke <- 0.46
    }
    if (is_dark_theme) {
        neighbor_rule_col <- "#9BB3C7"
        neighbor_dash_col <- "#AFC2D2"
        promoter_dash_col <- "#5DBED6"
        neighbor_tick_col <- "#D7E4F0"
        neighbor_label_col <- "#EAF2FB"
        neighbor_marker_fill <- "#76AFD8"
        neighbor_marker_col <- "#BCD2E6"
    }
    genomic_ruler_line_col <- if (is_dark_theme) "#73C7E2" else "#2F7895"
    genomic_ruler_text_col <- if (is_dark_theme) "#B8DDEA" else "#294F65"
    genomic_ruler_context_col <- if (is_dark_theme) "#789AB0" else "#7892A2"
    genomic_overlap_col <- if (is_dark_theme) "#C3A2F2" else "#7A5AA6"
    genomic_ruler_text_size <- if (visual_mode == "compact") 3.55 else 3.35
    if (is_colorblind_mode) {
        genomic_ruler_line_col <- if (is_dark_theme) "#8FC2F0" else "#4C78A8"
        genomic_ruler_text_col <- if (is_dark_theme) "#C6DDF4" else "#355A7A"
        genomic_ruler_context_col <- if (is_dark_theme) "#8BA8C2" else "#6F879C"
        genomic_overlap_col <- if (is_dark_theme) "#F0A6CE" else "#A64D79"
    }

    target_start <- suppressWarnings(min(df$xstart, na.rm = TRUE))
    target_end <- suppressWarnings(max(df$xend, na.rm = TRUE))
    if (!is.finite(target_start) || !is.finite(target_end) || target_end <= target_start) {
        target_start <- suppressWarnings(min(df_gene$V4, na.rm = TRUE))
        target_end <- suppressWarnings(max(df_gene$V5, na.rm = TRUE))
    }
    if (!is.finite(target_start) || !is.finite(target_end) || target_end <= target_start) {
        target_start <- suppressWarnings(min(as.numeric(df_transcript$V4), na.rm = TRUE))
        target_end <- suppressWarnings(max(as.numeric(df_transcript$V5), na.rm = TRUE))
    }
    if (!is.finite(target_start) || !is.finite(target_end) || target_end <= target_start) {
        target_start <- suppressWarnings(min(as.numeric(df$xstart), na.rm = TRUE))
        target_end <- suppressWarnings(max(as.numeric(df$xend), na.rm = TRUE))
    }
    if (!is.finite(current_transcript_length) || current_transcript_length <= 0) {
        current_transcript_length <- as.numeric(target_end - target_start + 1)
    }
    gene_strand <- pick_first_value(
        if (nrow(df_gene) > 0) df_gene$V7[1] else NA_character_,
        if (nrow(df_transcript) > 0) df_transcript$V7[1] else NA_character_,
        if (nrow(df) > 0) df$strand[1] else NA_character_,
        "+"
    )
    gene_strand <- trimws(as.character(gene_strand %||% "+"))
    if (!gene_strand %in% c("+", "-")) gene_strand <- "+"
    reverse_gene_axis <- gene_plot_axis_should_reverse(orientation_mode, gene_strand)

    # Use symmetric virtual extension so scaling does not leave a visible right-side tail.
    total_plot_length <- current_transcript_length + length_difference
    symmetric_extension <- max(0, length_difference) / 2
    virtual_target_start <- target_start - symmetric_extension
    virtual_target_end <- target_end + symmetric_extension
    neighbor_panel_width <- 0.255 * total_plot_length
    panel_gap <- 0.006 * total_plot_length

    # Keep a minimum visual buffer so left-oriented arrowheads never collide with the first exon.
    arrow_ext <- max(0.18 * current_transcript_length, 0.08 * total_plot_length)
    arrow_left_tip <- if (gene_strand == "-") target_start - arrow_ext else target_start
    arrow_right_tip <- if (gene_strand == "+") target_end + arrow_ext else target_end

    # Keep side panels outside arrowheads so dashed connectors never cross arrows.
    side_scale_padding <- 0.022 * total_plot_length
    left_panel_end_raw <- virtual_target_start - side_scale_padding - panel_gap
    right_panel_start_raw <- virtual_target_end + side_scale_padding + panel_gap
    min_connector_len <- 0.032 * total_plot_length
    left_panel_end <- min(left_panel_end_raw, arrow_left_tip - min_connector_len)
    left_panel_start <- left_panel_end - neighbor_panel_width
    right_panel_start <- max(right_panel_start_raw, arrow_right_tip + min_connector_len)
    right_panel_end <- right_panel_start + neighbor_panel_width

    edge_pad <- 0.004 * total_plot_length
    raw_left <- left_panel_start - edge_pad
    raw_right <- right_panel_end + edge_pad

    # Balance both sides around the transcript center so we avoid one-sided blank areas.
    plot_mid <- (virtual_target_start + virtual_target_end) / 2
    left_span <- plot_mid - raw_left
    right_span <- raw_right - plot_mid
    if (left_span > right_span) {
        delta <- left_span - right_span
        right_panel_start <- right_panel_start + delta
        right_panel_end <- right_panel_end + delta
        raw_right <- raw_right + delta
    } else if (right_span > left_span) {
        delta <- right_span - left_span
        left_panel_start <- left_panel_start - delta
        left_panel_end <- left_panel_end - delta
        raw_left <- raw_left - delta
    }

    # Keep a subtle border on both sides without wasting horizontal space.
    scene_width <- max(raw_right - raw_left, .Machine$double.eps)
    outer_margin <- 0.02 * scene_width
    plot_left <- raw_left - outer_margin
    plot_right <- raw_right + outer_margin

    d_min <- 10
    d_max <- 1e5
    base_line_df <- data.frame(
        x = target_start,
        xend = target_end,
        y = structure_y_base,
        yend = structure_y_base
    )
    full_width_gene_guide_df <- data.frame(
        x = left_panel_start,
        xend = right_panel_end,
        y = structure_y_base,
        yend = structure_y_base
    )
    arrow_line_df <- data.frame(
        x = if (gene_strand == "-") arrow_left_tip else target_end,
        xend = if (gene_strand == "+") arrow_right_tip else target_start,
        y = structure_y_base,
        yend = structure_y_base
    )
    arrow_line_start <- as.numeric(arrow_line_df$x[1])
    arrow_line_end <- as.numeric(arrow_line_df$xend[1])
    # Place the arrow tip a bit beyond the midpoint toward the strand direction.
    tip_fraction <- 0.58
    tip_t <- if (gene_strand == "+") tip_fraction else (1 - tip_fraction)
    arrow_tip_x <- arrow_line_start + (arrow_line_end - arrow_line_start) * tip_t
    # Thick dashed "arrow shaft" must end exactly at the arrow tip.
    arrow_shaft_df <- data.frame(
        x = if (gene_strand == "+") arrow_line_start else arrow_tip_x,
        xend = if (gene_strand == "+") arrow_tip_x else arrow_line_end,
        y = structure_y_base,
        yend = structure_y_base
    )
    arrow_head_len <- 0.012 * scene_width
    arrow_head_half_height <- 0.022
    arrow_base_x <- if (gene_strand == "-") arrow_tip_x + arrow_head_len else arrow_tip_x - arrow_head_len
    arrow_head_df <- data.frame(
        x = c(arrow_base_x, arrow_base_x, arrow_tip_x),
        y = c(structure_y_base - arrow_head_half_height, structure_y_base + arrow_head_half_height, structure_y_base),
        grp = 1
    )
    gene_attr <- pick_first_value(
        if (nrow(df_gene) > 0) as.character(df_gene$V9[1] %||% NA_character_) else NA_character_,
        if (nrow(df_transcript) > 0) as.character(df_transcript$V9[1] %||% NA_character_) else NA_character_,
        if (nrow(df) > 0) as.character(df$attributes_raw[1] %||% NA_character_) else NA_character_,
        ""
    )
    gene_name_value <- pick_first_value(
        extract_primary_gene_name(gene_attr),
        extract_primary_gene_id(gene_attr)
    )
    tx_row <- if (nrow(df_transcript) > 0) df_transcript[1, , drop = FALSE] else NULL
    tx_attr <- if (!is.null(tx_row)) as.character(tx_row$V9[1] %||% "") else ""
    tx_attrs <- parse_gff_attributes(tx_attr %||% "")
    gene_attrs <- parse_gff_attributes(gene_attr %||% "")
    tx_id <- pick_first_value(
        tx_attrs[["id"]][1], tx_attrs[["transcript_id"]][1], tx_attrs[["name"]][1],
        gene_attrs[["id"]][1], gene_attrs[["gene_id"]][1], gene_attrs[["locus_tag"]][1], gene_attrs[["name"]][1]
    )
    tx_name <- pick_first_value(
        tx_attrs[["name"]][1], tx_attrs[["transcript"]][1], tx_id,
        gene_attrs[["name"]][1], gene_attrs[["gene"]][1], gene_attrs[["gene_name"]][1], gene_name_value
    )
    tx_biotype <- pick_first_value(
        tx_attrs[["biotype"]][1],
        tx_attrs[["transcript_biotype"]][1],
        tx_attrs[["gene_biotype"]][1],
        tx_attrs[["gbkey"]][1],
        gene_attrs[["gene_biotype"]][1],
        gene_attrs[["gbkey"]][1],
        if (nrow(df_rect) > 0) as.character(df_rect$plot_group[1]) else NA_character_
    )
    tx_chr <- pick_first_value(
        if (!is.null(tx_row)) tx_row$V1[1] else NA_character_,
        if (nrow(df_gene) > 0) df_gene$V1[1] else NA_character_,
        if (nrow(df) > 0) df$seqid[1] else NA_character_
    )
    tx_chr_short <- tryCatch(
        get_short_chromosome_name(
            tx_chr,
            as.character(annotation_file_path %||% ""),
            use_report_map = isTRUE(use_report_map),
            report_path = as.character(report_path %||% "")
        ),
        error = function(e) tx_chr
    )
    ruler_spec <- build_genomic_ruler_spec(target_start, target_end)
    ruler_display_width_in <- suppressWarnings(
        as.numeric(width_svg) *
            ((target_end - target_start) / max(plot_right - plot_left, .Machine$double.eps))
    )
    ruler_spec <- prune_genomic_ruler_spec_for_width(
        ruler_spec,
        target_start,
        target_end,
        display_width_in = ruler_display_width_in,
        text_size_mm = genomic_ruler_text_size,
        min_gap_in = 0.11
    )
    # Keep the genomic axis clear of exon/CDS structures without increasing the
    # plot height. The gene name sits on the axis and masks the line beneath it.
    ruler_line_y <- center_gene_label_y
    ruler_tick_top_y <- center_gene_label_y + 0.013
    ruler_label_y <- center_gene_label_y + 0.031
    ruler_line_df <- data.frame()
    ruler_tick_df <- data.frame()
    ruler_label_df <- data.frame()
    if (!is.null(ruler_spec) && nrow(ruler_spec) >= 1L) {
        ruler_range_tooltip <- paste0(
            "<b style='color:var(--app-message-accent);font-weight:700;'>Genomic Context Scale</b><br/>",
            "<b>Chromosome / Sequence:</b> ", esc_html(tx_chr_short), "<br/>",
            "<b>Displayed Start:</b> ", format_bp(target_start), " bp<br/>",
            "<b>Displayed End:</b> ", format_bp(target_end), " bp<br/>",
            "<b>Displayed Span:</b> ", format_bp(target_end - target_start + 1), " bp<br/>",
            if (identical(orientation_mode, "transcription")) {
                paste0(
                    "<b>Display Orientation:</b> 5&#8242;&rarr;3&#8242;",
                    if (isTRUE(reverse_gene_axis)) " (minus-strand axis mirrored)" else "",
                    "<br/>"
                )
            } else {
                "<b>Display Orientation:</b> genomic coordinates<br/>"
            },
            "<span style='opacity:.82;'>Solid center: linear coordinates. Dashed sides: compressed neighbor distance.</span>"
        )
        ruler_line_df <- data.frame(
            x = target_start,
            xend = target_end,
            y = ruler_line_y,
            yend = ruler_line_y,
            tooltip = ruler_range_tooltip,
            data_id = "genomic_ruler_line",
            stringsAsFactors = FALSE
        )
        ruler_tick_df <- data.frame(
            x = ruler_spec$value,
            xend = ruler_spec$value,
            y = ruler_line_y,
            yend = ruler_tick_top_y,
            tooltip = paste0(
                "<b style='color:var(--app-message-accent);font-weight:700;'>Genomic Position</b><br/>",
                "<b>Chromosome / Sequence:</b> ", esc_html(tx_chr_short), "<br/>",
                "<b>Position:</b> ", ruler_spec$exact_bp, " bp"
            ),
            data_id = paste0("genomic_ruler_tick_", seq_len(nrow(ruler_spec))),
            stringsAsFactors = FALSE
        )
        ruler_label_hjust <- suppressWarnings(as.numeric(ruler_spec$hjust))
        if (isTRUE(reverse_gene_axis)) {
            ruler_label_hjust <- 1 - ruler_label_hjust
        }
        ruler_label_df <- data.frame(
            x = ruler_spec$value,
            y = ruler_label_y,
            label = ruler_spec$label,
            hjust = ruler_label_hjust,
            tooltip = ruler_tick_df$tooltip,
            data_id = paste0("genomic_ruler_label_", seq_len(nrow(ruler_spec))),
            stringsAsFactors = FALSE
        )
    }
    tx_start_num <- suppressWarnings(as.numeric(if (!is.null(tx_row)) tx_row$V4[1] else target_start))
    tx_end_num <- suppressWarnings(as.numeric(if (!is.null(tx_row)) tx_row$V5[1] else target_end))
    if (!is.finite(tx_start_num)) tx_start_num <- target_start
    if (!is.finite(tx_end_num)) tx_end_num <- target_end
    tx_strand <- pick_first_value(if (!is.null(tx_row)) tx_row$V7[1] else NA_character_, gene_strand)
    tx_strand <- trimws(as.character(tx_strand %||% gene_strand))
    if (!tx_strand %in% c("+", "-")) tx_strand <- gene_strand
    gene_display_label <- trimws(utils::URLdecode(as.character(gene_display_name %||% "")))
    center_gene_label <- pick_first_value(gene_display_label, gene_name_value, tx_name, tx_id)
    organism_display <- trimws(as.character(organism_label %||% "N/A"))
    if (!nzchar(organism_display)) organism_display <- "N/A"
    encode_data_id_value <- function(x) {
        xx <- trimws(as.character(x %||% "N/A"))
        if (!nzchar(xx)) xx <- "N/A"
        utils::URLencode(xx, reserved = TRUE)
    }
    promoter_side_label <- if (gene_strand == "+") "Upstream (left side)" else "Upstream (right side)"
    tx_start_int <- if (is.finite(tx_start_num)) as.integer(round(tx_start_num)) else NA_integer_
    tx_end_int <- if (is.finite(tx_end_num)) as.integer(round(tx_end_num)) else NA_integer_
    plot_id_txt <- trimws(as.character(plot_id %||% ""))
    if (!nzchar(plot_id_txt)) plot_id_txt <- "N/A"
    plot_context_txt <- trimws(as.character(plot_context %||% ""))
    if (!nzchar(plot_context_txt)) plot_context_txt <- "N/A"
    genomic_neighbor_action_hint <- if (tolower(plot_context_txt) %in% c("homologous", "homo", "multi_gene", "multigene")) {
        "<span style='opacity:.82;'>Click to visualize this gene below.</span>"
    } else {
        paste0(
            "<span style='opacity:.82;'>Click to inspect this annotation. ",
            "Adding it below is available only in Multi-Gene Search.</span>"
        )
    }
    promoter_popup_data_id <- paste0(
        "promoter_region",
        "|panel=", encode_data_id_value(plot_context_txt),
        "|plot_id=", encode_data_id_value(plot_id_txt),
        "|organism=", encode_data_id_value(organism_display),
        "|gene=", encode_data_id_value(center_gene_label),
        "|transcript=", encode_data_id_value(tx_name),
        "|transcript_id=", encode_data_id_value(tx_id),
        "|seqid=", encode_data_id_value(tx_chr),
        "|chromosome=", encode_data_id_value(tx_chr_short),
        "|tx_start=", encode_data_id_value(tx_start_int),
        "|tx_end=", encode_data_id_value(tx_end_int),
        "|strand=", encode_data_id_value(tx_strand),
        "|side=", encode_data_id_value(promoter_side_label)
    )
    genomic_neighbor_data_id <- function(kind, neighbor, relation_label, relation_detail = "") {
        neighbor_id <- pick_first_value(neighbor$neighbor_id, neighbor$neighbor_name, neighbor$neighbor_label)
        neighbor_name <- pick_first_value(neighbor$neighbor_label, neighbor$neighbor_name, neighbor$neighbor_id)
        neighbor_chr <- pick_first_value(neighbor$neighbor_chr, tx_chr)
        neighbor_start <- suppressWarnings(as.numeric(neighbor$neighbor_start %||% NA_real_))
        neighbor_end <- suppressWarnings(as.numeric(neighbor$neighbor_end %||% NA_real_))
        neighbor_strand <- pick_first_value(neighbor$neighbor_strand, "N/A")
        paste0(
            "genomic_ruler_", kind,
            "|panel=", encode_data_id_value(plot_context_txt),
            "|plot_id=", encode_data_id_value(plot_id_txt),
            "|organism=", encode_data_id_value(organism_display),
            "|source_gene=", encode_data_id_value(center_gene_label),
            "|neighbor_id=", encode_data_id_value(neighbor_id),
            "|neighbor_name=", encode_data_id_value(neighbor_name),
            "|seqid=", encode_data_id_value(neighbor_chr),
            "|chromosome=", encode_data_id_value(neighbor_chr),
            "|start=", encode_data_id_value(if (is.finite(neighbor_start)) as.integer(round(neighbor_start)) else "N/A"),
            "|end=", encode_data_id_value(if (is.finite(neighbor_end)) as.integer(round(neighbor_end)) else "N/A"),
            "|strand=", encode_data_id_value(neighbor_strand),
            "|relation=", encode_data_id_value(relation_label),
            "|detail=", encode_data_id_value(relation_detail)
        )
    }
    center_title <- if (!is.null(tx_row)) "Transcript Annotation" else "Gene Annotation"
    center_tooltip <- sprintf(
        "<b style='color:var(--app-message-accent);font-weight:700;'>%s</b><br/><b>Gene Name:</b> %s<br/><b>Transcript Id:</b> %s<br/><b>Transcript Name:</b> %s<br/><b>Chromosome:</b> %s<br/><b>Start:</b> %s<br/><b>End:</b> %s<br/><b>Strand:</b> %s<br/><b>Biotype:</b> %s",
        center_title,
        center_gene_label,
        tx_id,
        tx_name,
        tx_chr,
        format_bp(tx_start_num),
        format_bp(tx_end_num),
        tx_strand,
        tx_biotype
    )
    center_label_df <- data.frame(
        x = (plot_left + plot_right) / 2,
        y = center_gene_label_y,
        label = center_gene_label,
        tooltip = center_tooltip,
        data_id = "center_gene_label",
        stringsAsFactors = FALSE
    )

    if (visual_mode == "compact") {
        plot_ymin <- 0.875
        plot_ymax <- 1.130
    } else {
        detailed_feature_ymin <- if (nrow(df_rect) > 0) suppressWarnings(min(df_rect$ymin_feat, na.rm = TRUE)) else 0.825
        if (!is.finite(detailed_feature_ymin)) detailed_feature_ymin <- 0.825
        plot_ymin <- max(0.72, detailed_feature_ymin - 0.07)
        plot_ymax <- max(1.145, ruler_label_y + 0.020)
    }

    gene_x_scale <- if (isTRUE(reverse_gene_axis)) {
        scale_x_reverse(expand = expansion(mult = 0, add = 0))
    } else {
        scale_x_continuous(expand = expansion(mult = 0, add = 0))
    }

    gg_lines <- ggplot() +
        # Permanent full-width guide for the gene track. Neighbor and overlap
        # toggles only control their glyphs and labels, never this baseline.
        geom_segment(
            data = full_width_gene_guide_df,
            aes(x = x, xend = xend, y = y, yend = yend),
            colour = neighbor_dash_col,
            linewidth = 0.42,
            linetype = "22",
            lineend = "butt"
        ) +

        # Línea base
        geom_segment(
            data = base_line_df,
            aes(x = x, xend = xend, y = y, yend = yend),
            color = gene_line_color, linewidth = 1.05
        ) +

        # Flecha de dirección
        geom_segment(
            data = arrow_line_df,
            aes(x = x, xend = xend, y = y, yend = yend),
            colour = neighbor_dash_col,
            linewidth = 0.42,
            linetype = "22",
            lineend = "butt"
        ) +
        geom_segment(
            data = arrow_shaft_df,
            aes(x = x, xend = xend, y = y, yend = yend),
            colour = gene_direction_color,
            linewidth = 0.62,
            linetype = "22",
            lineend = "butt"
        ) +

        # Solid triangular arrowhead with no solid shaft.
        geom_polygon(
            data = arrow_head_df,
            aes(x = x, y = y, group = grp),
            fill = gene_direction_color,
            color = gene_direction_color,
            linewidth = 0.15
        ) +
        {
            if (nrow(intron_df) > 0 && isTRUE(compact_feature_interactivity)) {
                geom_segment_interactive(
                    data = intron_df,
                    aes(
                        x = intron_start, xend = intron_end,
                        y = structure_y_base, yend = structure_y_base,
                        tooltip = tooltip, data_id = data_id
                    ),
                    color = gene_line_color,
                    linewidth = 0.8,
                    linetype = "solid"
                )
            } else if (nrow(intron_df) > 0) {
                geom_segment(
                    data = intron_df,
                    aes(
                        x = intron_start, xend = intron_end,
                        y = structure_y_base, yend = structure_y_base
                    ),
                    color = gene_line_color,
                    linewidth = 0.8,
                    linetype = "solid"
                )
            } else {
                NULL
            }
        } +
        {
            if (nrow(df_rect) > 0 && isTRUE(compact_feature_interactivity)) {
                geom_rect_interactive(
                    data = df_rect,
                    aes(
                        xmin = xstart, xmax = xend,
                        ymin = ymin_feat, ymax = ymax_feat,
                        fill = plot_group,
                        tooltip = plot_tooltip,
                        data_id = feature_uid
                    ),
                    alpha = 1
                )
            } else if (nrow(df_rect) > 0) {
                geom_rect(
                    data = df_rect,
                    aes(
                        xmin = xstart, xmax = xend,
                        ymin = ymin_feat, ymax = ymax_feat,
                        fill = plot_group
                    ),
                    alpha = 1
                )
            } else {
                NULL
            }
        } +
        {
            if (nrow(df_rect) > 0) scale_fill_manual(values = custom_colors, na.value = "#95A5A6") else NULL
        } +
        {
            if (nrow(ruler_line_df) > 0) {
                geom_segment_interactive(
                    data = ruler_line_df,
                    aes(
                        x = x, xend = xend, y = y, yend = yend,
                        tooltip = tooltip, data_id = data_id
                    ),
                    color = genomic_ruler_line_col,
                    linewidth = 0.42,
                    lineend = "round"
                )
            } else {
                NULL
            }
        } +
        {
            if (nrow(ruler_tick_df) > 0) {
                geom_segment_interactive(
                    data = ruler_tick_df,
                    aes(
                        x = x, xend = xend, y = y, yend = yend,
                        tooltip = tooltip, data_id = data_id
                    ),
                    color = genomic_ruler_line_col,
                    linewidth = 0.34,
                    lineend = "round"
                )
            } else {
                NULL
            }
        } +
        {
            if (nrow(ruler_label_df) > 0) {
                geom_text_interactive(
                    data = ruler_label_df,
                    aes(
                        x = x, y = y, label = label, hjust = hjust,
                        tooltip = tooltip, data_id = data_id
                    ),
                    size = genomic_ruler_text_size,
                    color = genomic_ruler_text_col,
                    vjust = 0.5,
                    lineheight = 0.9
                )
            } else {
                NULL
            }
        } +
        geom_label_interactive(
            data = center_label_df,
            aes(x = x, y = y, label = label, tooltip = tooltip, data_id = data_id),
            size = center_gene_size,
            color = center_label_col,
            fill = panel_bg_fill,
            label.size = 0,
            label.padding = grid::unit(0.12, "lines"),
            label.r = grid::unit(0.08, "lines"),
            fontface = "bold",
            hjust = 0.5,
            vjust = 0.5
        ) +
        gene_x_scale +
        scale_y_continuous(expand = expansion(mult = 0, add = 0)) +
        coord_cartesian(
          xlim = c(plot_left, plot_right),
          ylim = c(plot_ymin, plot_ymax),
          clip = "off"
        ) +
        theme_minimal() +
        theme(
            legend.position = "none",
            panel.grid = element_blank(),
            panel.background = element_rect(fill = panel_bg_fill, colour = panel_bg_fill),
            plot.background = element_rect(fill = panel_bg_fill, colour = panel_bg_fill),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = margin(0, 0, 0, 0)
        )

    # Length/composition metadata is shown in card footers (server.R),
    # so we keep the plot area focused on gene structure + neighbors.

    if (!is.null(neighbor_context)) {
        interval_ratio <- 1.4
        interval_weights <- interval_ratio^(0:3)
        interval_edges <- c(0, cumsum(interval_weights) / sum(interval_weights))
        to_panel_s <- function(bp) {
            if (bp <= 0) {
                return(0)
            }
            bp_clamped <- min(as.numeric(bp), d_max)
            # Piecewise decade index:
            # 0..100 bp -> [0,1], then 100..100k bp -> [1,4] in log10 decades.
            u <- if (bp_clamped <= 100) {
                bp_clamped / 100
            } else {
                1 + log10(bp_clamped / 100)
            }
            u <- min(max(u, 0), 4)
            seg <- min(3, floor(u))
            frac <- u - seg
            left <- interval_edges[seg + 1]
            right <- interval_edges[seg + 2]
            s <- left + (right - left) * frac
            min(max(s, 0), 1)
        }

        ticks_vals <- c(0, 100, 1000, 10000, 100000)
        tick_s <- vapply(ticks_vals, to_panel_s, numeric(1))
        side_tick_s <- tick_s[-1]
        side_tick_labels <- c("100 bp", "1 kb", "10 kb", "\u2265100 kb")

        left_ticks <- data.frame(
            x = left_panel_end - side_tick_s * neighbor_panel_width,
            xend = left_panel_end - side_tick_s * neighbor_panel_width,
            y = ruler_line_y,
            yend = ruler_tick_top_y,
            label = side_tick_labels,
            hjust = c(0.5, 0.5, 0.5, 0),
            tooltip = paste0(
                "<b style='color:var(--app-message-accent);font-weight:700;'>Compressed Neighbor Distance</b><br/>",
                "<b>Side:</b> Left<br/>",
                "<b>Distance from query-gene boundary:</b> ", side_tick_labels,
                "<br/><span style='opacity:.82;'>Logarithmically compressed for context.</span>"
            ),
            data_id = paste0("genomic_ruler_context_left_tick_", seq_along(side_tick_s)),
            stringsAsFactors = FALSE
        )
        right_ticks <- data.frame(
            x = right_panel_start + side_tick_s * neighbor_panel_width,
            xend = right_panel_start + side_tick_s * neighbor_panel_width,
            y = ruler_line_y,
            yend = ruler_tick_top_y,
            label = side_tick_labels,
            hjust = c(0.5, 0.5, 0.5, 1),
            tooltip = paste0(
                "<b style='color:var(--app-message-accent);font-weight:700;'>Compressed Neighbor Distance</b><br/>",
                "<b>Side:</b> Right<br/>",
                "<b>Distance from query-gene boundary:</b> ", side_tick_labels,
                "<br/><span style='opacity:.82;'>Logarithmically compressed for context.</span>"
            ),
            data_id = paste0("genomic_ruler_context_right_tick_", seq_along(side_tick_s)),
            stringsAsFactors = FALSE
        )
        if (isTRUE(reverse_gene_axis)) {
            left_ticks$hjust <- 1 - left_ticks$hjust
            right_ticks$hjust <- 1 - right_ticks$hjust
        }
        context_side_segments <- data.frame(
            x = c(left_panel_start, target_end),
            xend = c(target_start, right_panel_end),
            y = c(ruler_line_y, ruler_line_y),
            yend = c(ruler_line_y, ruler_line_y),
            tooltip = c(
                "<b style='color:var(--app-message-accent);font-weight:700;'>Left Neighbor Context</b><br/>Compressed distance scale from the query-gene boundary (up to 100 kb).",
                "<b style='color:var(--app-message-accent);font-weight:700;'>Right Neighbor Context</b><br/>Compressed distance scale from the query-gene boundary (up to 100 kb)."
            ),
            data_id = c("genomic_ruler_context_left", "genomic_ruler_context_right"),
            stringsAsFactors = FALSE
        )
        context_breaks <- data.frame(
            x = c(
                (left_panel_end + target_start) / 2,
                (target_end + right_panel_start) / 2
            ),
            y = c(ruler_line_y, ruler_line_y),
            label = c("//", "//"),
            tooltip = c(
                "Scale break: left neighbor distances are compressed.",
                "Scale break: right neighbor distances are compressed."
            ),
            data_id = c("genomic_ruler_context_left_break", "genomic_ruler_context_right_break"),
            stringsAsFactors = FALSE
        )
        promoter_fraction <- 0.58
        split_connector_segments <- function(panel_x, gene_x, is_promoter_side = FALSE) {
            panel_x <- as.numeric(panel_x)
            gene_x <- as.numeric(gene_x)
            if (!is.finite(panel_x) || !is.finite(gene_x) || panel_x == gene_x) {
                return(data.frame(x = numeric(0), xend = numeric(0), part = character(0), stringsAsFactors = FALSE))
            }
            if (!isTRUE(is_promoter_side)) {
                return(data.frame(x = panel_x, xend = gene_x, part = "base", stringsAsFactors = FALSE))
            }
            # Keep a visual/interactive gap near the gene edge so promoter and exon
            # do not share the same hover pixel when the scene is highly compressed.
            edge_gap <- max(2, 0.004 * scene_width)
            max_gap <- abs(panel_x - gene_x) * 0.45
            if (!is.finite(max_gap) || max_gap <= 0) {
                return(data.frame(x = panel_x, xend = gene_x, part = "base", stringsAsFactors = FALSE))
            }
            edge_gap <- min(edge_gap, max_gap)
            dir_sign <- if (panel_x > gene_x) 1 else -1
            promoter_start <- gene_x + dir_sign * edge_gap
            split_x <- promoter_start + (panel_x - promoter_start) * promoter_fraction
            data.frame(
                # base: non-promoter side of the connector
                # promoter: visual promoter segment (includes anti-flicker gap)
                # promoter_hit: invisible interactive subsegment (stops before the exon edge)
                x = c(panel_x, split_x, split_x),
                xend = c(split_x, gene_x, promoter_start),
                part = c("base", "promoter", "promoter_hit"),
                stringsAsFactors = FALSE
            )
        }
        connector_segments <- rbind(
            split_connector_segments(
                panel_x = left_panel_end,
                gene_x = arrow_left_tip,
                is_promoter_side = (gene_strand == "+")
            ),
            split_connector_segments(
                panel_x = right_panel_start,
                gene_x = arrow_right_tip,
                is_promoter_side = (gene_strand == "-")
            )
        )
        if (nrow(connector_segments) > 0) {
            connector_segments$y <- structure_y_base
            connector_segments$yend <- structure_y_base
            is_prom_part <- connector_segments$part %in% c("promoter", "promoter_hit")
            connector_segments$promoter_tooltip <- ifelse(is_prom_part, "Promoter region", NA_character_)
            connector_segments$promoter_data_id <- ifelse(is_prom_part, promoter_popup_data_id, NA_character_)
        }
        promoter_click_target <- connector_segments[connector_segments$part == "promoter_hit", , drop = FALSE]
        if (nrow(promoter_click_target) > 0) {
            promoter_click_target$promoter_tooltip <- as.character(promoter_click_target$promoter_tooltip)
            promoter_click_target$promoter_data_id <- as.character(promoter_click_target$promoter_data_id)
            keep_prom <- is.finite(promoter_click_target$x) &
                is.finite(promoter_click_target$xend) &
                (abs(promoter_click_target$xend - promoter_click_target$x) > 1e-9) &
                !is.na(promoter_click_target$promoter_tooltip) &
                nzchar(trimws(promoter_click_target$promoter_tooltip)) &
                !is.na(promoter_click_target$promoter_data_id) &
                nzchar(trimws(promoter_click_target$promoter_data_id))
            promoter_click_target <- promoter_click_target[keep_prom, , drop = FALSE]
        }
        both_ticks <- rbind(left_ticks, right_ticks)
        both_tick_labels <- both_ticks
        both_tick_labels$y <- ruler_label_y
        connector_base <- connector_segments[connector_segments$part == "base", , drop = FALSE]
        connector_promoter <- connector_segments[connector_segments$part == "promoter", , drop = FALSE]
        lower_context_segments <- data.frame(
            x = c(left_panel_start, right_panel_start),
            xend = c(left_panel_end, right_panel_end),
            y = c(structure_y_base, structure_y_base),
            yend = c(structure_y_base, structure_y_base),
            tooltip = c(
                "Left neighbor position projected onto the compressed genomic-context scale.",
                "Right neighbor position projected onto the compressed genomic-context scale."
            ),
            data_id = c(
                "genomic_ruler_lower_context_left",
                "genomic_ruler_lower_context_right"
            ),
            stringsAsFactors = FALSE
        )

        gg_lines <- gg_lines +
            geom_segment_interactive(
                data = context_side_segments,
                aes(
                    x = x, xend = xend, y = y, yend = yend,
                    tooltip = tooltip, data_id = data_id
                ),
                color = genomic_ruler_context_col,
                linewidth = 0.42,
                linetype = "22",
                lineend = "butt"
            ) +
            geom_segment_interactive(
                data = both_ticks,
                aes(
                    x = x, xend = xend, y = y, yend = yend,
                    tooltip = tooltip, data_id = data_id
                ),
                color = genomic_ruler_context_col,
                linewidth = 0.34,
                lineend = "round"
            ) +
            geom_text_interactive(
                data = both_tick_labels,
                aes(
                    x = x, y = y, label = label, hjust = hjust,
                    tooltip = tooltip, data_id = data_id
                ),
                size = genomic_ruler_text_size * 0.96,
                color = genomic_ruler_text_col,
                vjust = 0.5,
                lineheight = 0.9
            ) +
            geom_label_interactive(
                data = context_breaks,
                aes(
                    x = x, y = y, label = label,
                    tooltip = tooltip, data_id = data_id
                ),
                size = 2.7,
                color = genomic_ruler_context_col,
                fill = panel_bg_fill,
                label.size = 0,
                label.padding = grid::unit(0.04, "lines"),
                label.r = grid::unit(0.02, "lines"),
                fontface = "bold",
                hjust = 0.5,
                vjust = 0.5
            ) +
            geom_segment_interactive(
                data = lower_context_segments,
                aes(
                    x = x, xend = xend, y = y, yend = yend,
                    tooltip = tooltip, data_id = data_id
                ),
                color = neighbor_dash_col,
                linewidth = 0.42,
                linetype = "22",
                lineend = "butt"
            ) +
            geom_segment(
                data = connector_base,
                aes(x = x, xend = xend, y = y, yend = yend),
                color = neighbor_dash_col,
                linewidth = 0.42,
                linetype = "22",
                lineend = "butt"
            ) +
            geom_segment(
                data = connector_promoter,
                aes(
                    x = x, xend = xend, y = y, yend = yend
                ),
                color = promoter_dash_col,
                linewidth = 0.48,
                linetype = "22",
                lineend = "butt"
            ) +
            # Invisible wide hit area to make promoter clicks easier without changing appearance.
            geom_segment_interactive(
                data = promoter_click_target,
                aes(
                    x = x, xend = xend, y = y, yend = yend,
                    tooltip = promoter_tooltip, data_id = promoter_data_id
                ),
                color = promoter_dash_col,
                linewidth = 6.2,
                alpha = 0.001,
                linetype = "solid",
                lineend = "butt"
            )

        strand_arrow <- function(strand) {
            strand_txt <- trimws(as.character(strand %||% ""))
            if (identical(strand_txt, "+")) "\u2192" else if (identical(strand_txt, "-")) "\u2190" else ""
        }
        compact_distance_label <- function(bp) {
            bp_num <- suppressWarnings(as.numeric(bp))
            if (!is.finite(bp_num)) return("N/A")
            if (bp_num >= 1e6) {
                out <- sprintf("%.1f Mb", bp_num / 1e6)
            } else if (bp_num >= 1e3) {
                out <- sprintf("%.1f kb", bp_num / 1e3)
            } else {
                out <- sprintf("%d bp", as.integer(round(bp_num)))
            }
            sub("\\.0\\s", " ", out)
        }
        empty_track_df <- function() {
            data.frame(
                side = character(0), x = numeric(0), y = numeric(0),
                yend = numeric(0), glyph_from = numeric(0), glyph_to = numeric(0),
                name_text = character(0), strand_direction = character(0),
                marker_kind = character(0), tooltip = character(0),
                data_id = character(0), stringsAsFactors = FALSE
            )
        }
        make_side_df <- function(side_name, neighbor, is_left = TRUE) {
            dist_bp_raw <- suppressWarnings(as.numeric(neighbor$dist_bp %||% NA_real_))
            neighbor_start_num <- suppressWarnings(as.numeric(neighbor$neighbor_start %||% NA_real_))
            neighbor_end_num <- suppressWarnings(as.numeric(neighbor$neighbor_end %||% NA_real_))
            if (!is.finite(dist_bp_raw) || dist_bp_raw < 0 ||
                !is.finite(neighbor_start_num) || !is.finite(neighbor_end_num)) {
                return(empty_track_df())
            }

            relation <- classify_neighbor_relation(
                target_start, target_end, neighbor_start_num, neighbor_end_num
            )
            marker_kind <- if (identical(relation$category, "adjacent")) "adjacent" else "separated"
            marker_x <- if (is_left) {
                left_panel_end - to_panel_s(dist_bp_raw) * neighbor_panel_width
            } else {
                right_panel_start + to_panel_s(dist_bp_raw) * neighbor_panel_width
            }
            neighbor_display_clean <- truncate_neighbor_label(
                pick_first_value(neighbor$neighbor_label, neighbor$neighbor_name, neighbor$neighbor_id),
                max_chars = 100
            )
            neighbor_name_short <- truncate_neighbor_label(neighbor_display_clean, max_chars = 22)
            arrow_txt <- strand_arrow(neighbor$neighbor_strand)
            strand_txt <- trimws(as.character(neighbor$neighbor_strand %||% ""))
            strand_direction <- if (identical(strand_txt, "+")) {
                "forward"
            } else if (identical(strand_txt, "-")) {
                "reverse"
            } else {
                "unknown"
            }
            distance_short <- compact_distance_label(dist_bp_raw)
            relation_short <- if (identical(marker_kind, "adjacent")) {
                "adjacent \u00b7 0 bp gap"
            } else if (dist_bp_raw > d_max) {
                paste0(distance_short, " \u00b7 beyond scale")
            } else {
                paste0(distance_short, " gap")
            }
            name_text <- paste0(
                neighbor_name_short,
                if (nzchar(arrow_txt)) paste0(" ", arrow_txt) else "",
                "\n", relation_short
            )
            side_title <- if (is_left) "Left Neighbor" else "Right Neighbor"
            tooltip <- sprintf(
                paste0(
                    "<b style='color:var(--app-message-accent);font-weight:700;'>%s</b><br/>",
                    "<span style='font-size:13px;font-weight:700;'>%s</span><br/>",
                    "<b>Relation:</b> %s<br/>",
                    "<b>Gap To Query Gene:</b> %s (%s bp)<br/>",
                    "<b>Neighbor Gene:</b> %s<br/>",
                    "<b>Neighbor Chromosome:</b> %s<br/>",
                    "<b>Neighbor Start:</b> %s<br/>",
                    "<b>Neighbor End:</b> %s<br/>",
                    "<b>Neighbor Strand:</b> %s<br/>",
                    "<b>Glyph:</b> symbolic gene body (length not to scale)<br/>",
                    genomic_neighbor_action_hint
                ),
                side_title,
                esc_html(neighbor_display_clean),
                relation$relation_label,
                distance_short,
                format_bp(dist_bp_raw),
                esc_html(neighbor_display_clean),
                esc_html(neighbor$neighbor_chr %||% "N/A"),
                format_bp(neighbor_start_num),
                format_bp(neighbor_end_num),
                esc_html(neighbor$neighbor_strand %||% "N/A")
            )
            glyph_width <- min(0.022 * scene_width, 0.22 * neighbor_panel_width)
            glyph_start <- if (is_left) marker_x - glyph_width else marker_x
            glyph_end <- if (is_left) marker_x else marker_x + glyph_width
            panel_lo <- if (is_left) left_panel_start else right_panel_start
            panel_hi <- if (is_left) left_panel_end else right_panel_end
            glyph_start <- min(max(glyph_start, panel_lo), panel_hi)
            glyph_end <- min(max(glyph_end, panel_lo), panel_hi)
            glyph_lo <- min(glyph_start, glyph_end)
            glyph_hi <- max(glyph_start, glyph_end)
            glyph_from <- if (identical(strand_direction, "reverse")) glyph_hi else glyph_lo
            glyph_to <- if (identical(strand_direction, "reverse")) glyph_lo else glyph_hi
            glyph_offset <- if (identical(visual_mode, "compact")) {
                if (isTRUE(has_overlap_layout)) 0.040 else 0.020
            } else {
                if (isTRUE(has_overlap_layout)) 0.035 else 0.018
            }
            glyph_y <- ruler_line_y - glyph_offset
            data.frame(
                side = side_name,
                x = marker_x,
                y = ruler_line_y,
                yend = glyph_y,
                glyph_from = glyph_from,
                glyph_to = glyph_to,
                name_text = name_text,
                strand_direction = strand_direction,
                marker_kind = marker_kind,
                tooltip = tooltip,
                data_id = genomic_neighbor_data_id(
                    "neighbor",
                    neighbor,
                    relation_label = relation$relation_label,
                    relation_detail = relation_short
                ),
                stringsAsFactors = FALSE
            )
        }

        side_df <- rbind(
            make_side_df("left", neighbor_context$upstream, is_left = TRUE),
            make_side_df("right", neighbor_context$downstream, is_left = FALSE)
        )
        if (nrow(side_df) > 0) {
            side_df$x <- suppressWarnings(as.numeric(side_df$x))
            side_df$tooltip <- as.character(side_df$tooltip)
            side_df$data_id <- as.character(side_df$data_id)
            side_df$name_text <- as.character(side_df$name_text)
            side_df <- side_df[
                is.finite(side_df$x) &
                    !is.na(side_df$tooltip) & nzchar(trimws(side_df$tooltip)) &
                    !is.na(side_df$data_id) & nzchar(trimws(side_df$data_id)) &
                    !is.na(side_df$name_text) & nzchar(trimws(side_df$name_text)),
                ,
                drop = FALSE
            ]
        }
        side_label_offset <- if (identical(visual_mode, "compact")) {
            if (isTRUE(has_overlap_layout)) 0.088 else 0.060
        } else {
            if (isTRUE(has_overlap_layout)) 0.078 else 0.055
        }
        side_label_y <- ruler_line_y - side_label_offset
        directional_side_df <- side_df[
            side_df$strand_direction %in% c("forward", "reverse"),
            ,
            drop = FALSE
        ]
        unknown_strand_side_df <- side_df[
            !side_df$strand_direction %in% c("forward", "reverse"),
            ,
            drop = FALSE
        ]

        as_neighbor_list <- function(x) {
            if (is.null(x)) return(list())
            if (is.data.frame(x)) {
                return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
            }
            if (is.list(x) && !is.null(x$neighbor_start)) return(list(x))
            if (is.list(x)) return(Filter(is.list, x))
            list()
        }
        overlap_neighbors <- as_neighbor_list(neighbor_context$overlapping)
        legacy_neighbors <- list(neighbor_context$upstream, neighbor_context$downstream)
        legacy_neighbors <- Filter(function(nb) {
            is.list(nb) && is.finite(suppressWarnings(as.numeric(nb$dist_bp %||% NA_real_))) &&
                suppressWarnings(as.numeric(nb$dist_bp)) < 0
        }, legacy_neighbors)
        overlap_neighbors <- c(overlap_neighbors, legacy_neighbors)

        overlap_band_df <- data.frame()
        if (length(overlap_neighbors) > 0) {
            overlap_keys <- vapply(overlap_neighbors, function(nb) {
                paste(
                    as.character(nb$neighbor_id %||% nb$neighbor_name %||% ""),
                    as.character(nb$neighbor_start %||% ""),
                    as.character(nb$neighbor_end %||% ""),
                    as.character(nb$neighbor_strand %||% ""),
                    sep = "::"
                )
            }, character(1))
            overlap_neighbors <- overlap_neighbors[!duplicated(overlap_keys)]
            overlap_rows <- lapply(seq_along(overlap_neighbors), function(i) {
                neighbor <- overlap_neighbors[[i]]
                neighbor_start_num <- suppressWarnings(as.numeric(neighbor$neighbor_start %||% NA_real_))
                neighbor_end_num <- suppressWarnings(as.numeric(neighbor$neighbor_end %||% NA_real_))
                relation <- classify_neighbor_relation(
                    target_start, target_end, neighbor_start_num, neighbor_end_num
                )
                if (!identical(relation$category, "overlap")) return(NULL)
                neighbor_display_clean <- truncate_neighbor_label(
                    pick_first_value(neighbor$neighbor_label, neighbor$neighbor_name, neighbor$neighbor_id),
                    max_chars = 100
                )
                neighbor_name_short <- truncate_neighbor_label(neighbor_display_clean, max_chars = 18)
                arrow_txt <- strand_arrow(neighbor$neighbor_strand)
                relation_short <- switch(
                    relation$relation,
                    same_span = "same span",
                    neighbor_inside_query = "inside query",
                    neighbor_contains_query = "contains query",
                    partial_overlap_left = "overlaps left edge",
                    partial_overlap_right = "overlaps right edge",
                    "overlap"
                )
                overlap_short <- compact_distance_label(relation$overlap_bp)
                tooltip <- sprintf(
                    paste0(
                        "<b style='color:var(--app-message-accent);font-weight:700;'>Overlapping Neighbor</b><br/>",
                        "<span style='font-size:13px;font-weight:700;'>%s</span><br/>",
                        "<b>Relation:</b> %s<br/>",
                        "<b>Shared Interval:</b> %s - %s<br/>",
                        "<b>Overlap Length:</b> %s (%s bp)<br/>",
                        "<b>Neighbor Start:</b> %s<br/>",
                        "<b>Neighbor End:</b> %s<br/>",
                        "<b>Neighbor Strand:</b> %s<br/>",
                        genomic_neighbor_action_hint
                    ),
                    esc_html(neighbor_display_clean),
                    relation$relation_label,
                    format_bp(relation$clipped_start),
                    format_bp(relation$clipped_end),
                    overlap_short,
                    format_bp(relation$overlap_bp),
                    format_bp(neighbor_start_num),
                    format_bp(neighbor_end_num),
                    esc_html(neighbor$neighbor_strand %||% "N/A")
                )
                data.frame(
                    x = relation$clipped_start,
                    xend = relation$clipped_end,
                    midpoint = (relation$clipped_start + relation$clipped_end) / 2,
                    neighbor_name = neighbor_name_short,
                    strand_arrow = arrow_txt,
                    relation_short = relation_short,
                    overlap_bp = relation$overlap_bp,
                    tooltip = tooltip,
                    data_id = genomic_neighbor_data_id(
                        "overlap",
                        neighbor,
                        relation_label = relation$relation_label,
                        relation_detail = paste0(relation_short, " \u00b7 ", overlap_short, " shared")
                    ),
                    stringsAsFactors = FALSE
                )
            })
            overlap_rows <- Filter(Negate(is.null), overlap_rows)
            if (length(overlap_rows) > 0) overlap_band_df <- do.call(rbind, overlap_rows)
        }

        if (nrow(side_df) > 0) {
            gg_lines <- gg_lines +
                geom_segment_interactive(
                    data = side_df,
                    aes(
                        x = x, xend = x, y = y, yend = yend,
                        tooltip = tooltip, data_id = data_id
                    ),
                    color = neighbor_marker_col,
                    linewidth = 0.38,
                    lineend = "round"
                ) +
                geom_text_interactive(
                    data = side_df,
                    aes(
                        x = x, y = side_label_y, label = name_text,
                        tooltip = tooltip, data_id = data_id
                    ),
                    size = neighbor_name_size * 0.88,
                    color = neighbor_label_col,
                    lineheight = 0.88,
                    vjust = 0.5
                )
        }
        if (nrow(directional_side_df) > 0) {
            gg_lines <- gg_lines +
                geom_segment_interactive(
                    data = directional_side_df,
                    aes(
                        x = glyph_from, xend = glyph_to, y = yend, yend = yend,
                        tooltip = tooltip, data_id = data_id
                    ),
                    color = neighbor_marker_col,
                    linewidth = 2.15,
                    lineend = "round",
                    arrow = grid::arrow(
                        type = "closed",
                        length = grid::unit(0.055, "inches")
                    )
                )
        }
        if (nrow(unknown_strand_side_df) > 0) {
            gg_lines <- gg_lines +
                geom_segment_interactive(
                    data = unknown_strand_side_df,
                    aes(
                        x = glyph_from, xend = glyph_to, y = yend, yend = yend,
                        tooltip = tooltip, data_id = data_id
                    ),
                    color = neighbor_marker_col,
                    linewidth = 2.15,
                    lineend = "round"
                )
        }
        if (nrow(overlap_band_df) > 0) {
            overlap_band_df$lane <- assign_genomic_overlap_lanes(
                overlap_band_df$x,
                overlap_band_df$xend,
                padding_bp = max(1, (target_end - target_start) * 0.004)
            )
            lane_count <- max(overlap_band_df$lane)
            lane_top_y <- ruler_line_y - if (identical(visual_mode, "compact")) 0.040 else 0.035
            lane_bottom_y <- ruler_line_y - if (identical(visual_mode, "compact")) 0.055 else 0.048
            lane_positions <- if (lane_count <= 1L) {
                lane_top_y
            } else {
                seq(lane_top_y, lane_bottom_y, length.out = lane_count)
            }
            overlap_band_df$y <- lane_positions[overlap_band_df$lane]
            overlap_band_df$yend <- overlap_band_df$y
            overlap_band_df$marker_y <- overlap_band_df$y
            overlap_stem_df <- rbind(
                transform(
                    overlap_band_df,
                    stem_x = x,
                    stem_id = paste0(data_id, "|part=start")
                ),
                transform(
                    overlap_band_df,
                    stem_x = xend,
                    stem_id = paste0(data_id, "|part=end")
                )
            )
            if (nrow(overlap_band_df) == 1L) {
                overlap_label_text <- paste0(
                    overlap_band_df$neighbor_name[1],
                    if (nzchar(overlap_band_df$strand_arrow[1])) {
                        paste0(" ", overlap_band_df$strand_arrow[1])
                    } else {
                        ""
                    },
                    " \u00b7 ", overlap_band_df$relation_short[1]
                )
                overlap_label_tooltip <- overlap_band_df$tooltip[1]
                overlap_label_x <- overlap_band_df$midpoint[1]
                overlap_label_y <- overlap_band_df$y[1] -
                    if (identical(visual_mode, "compact")) 0.040 else 0.030
                overlap_label_data_id <- overlap_band_df$data_id[1]
            } else {
                overlap_label_text <- paste0(
                    nrow(overlap_band_df), " overlapping neighbors \u00b7 ",
                    lane_count, if (lane_count == 1L) " lane" else " lanes"
                )
                overlap_lengths <- vapply(
                    overlap_band_df$overlap_bp,
                    compact_distance_label,
                    character(1)
                )
                overlap_label_tooltip <- paste0(
                    "<b style='color:var(--app-message-accent);font-weight:700;'>Overlapping Neighbors</b><br/>",
                    paste(
                        paste0(
                            esc_html(overlap_band_df$neighbor_name),
                            ": ", overlap_band_df$relation_short,
                            " (", overlap_lengths, ")"
                        ),
                        collapse = "<br/>"
                    )
                )
                overlap_label_x <- mean(c(target_start, target_end))
                overlap_label_y <- lane_bottom_y -
                    if (identical(visual_mode, "compact")) 0.035 else 0.028
                overlap_label_data_id <- "genomic_ruler_overlap_summary"
            }
            overlap_label_df <- data.frame(
                x = overlap_label_x,
                y = overlap_label_y,
                label = overlap_label_text,
                tooltip = overlap_label_tooltip,
                data_id = overlap_label_data_id,
                stringsAsFactors = FALSE
            )
            gg_lines <- gg_lines +
                geom_segment_interactive(
                    data = overlap_stem_df,
                    aes(
                        x = stem_x, xend = stem_x,
                        y = ruler_line_y, yend = y,
                        tooltip = tooltip, data_id = stem_id
                    ),
                    color = genomic_overlap_col,
                    linewidth = 0.46,
                    lineend = "round"
                ) +
                geom_segment_interactive(
                    data = overlap_band_df,
                    aes(
                        x = x, xend = xend, y = y, yend = yend,
                        tooltip = tooltip, data_id = data_id
                    ),
                    color = genomic_overlap_col,
                    linewidth = 2.15,
                    lineend = "round",
                    alpha = 0.88
                ) +
                geom_point_interactive(
                    data = overlap_band_df,
                    aes(
                        x = midpoint, y = marker_y,
                        tooltip = tooltip, data_id = data_id
                    ),
                    shape = 23,
                    size = 2.25,
                    fill = panel_bg_fill,
                    color = genomic_overlap_col,
                    stroke = 0.5
                ) +
                geom_label_interactive(
                    data = overlap_label_df,
                    aes(
                        x = x, y = y, label = label,
                        tooltip = tooltip, data_id = data_id
                    ),
                    size = neighbor_name_size * 0.82,
                    color = genomic_overlap_col,
                    fill = panel_bg_fill,
                    label.size = 0.25,
                    label.padding = grid::unit(0.08, "lines"),
                    label.r = grid::unit(0.06, "lines"),
                    fontface = "bold",
                    vjust = 0.5
                )
        }
    }

    # Disable hover restyling to avoid rapid style thrash in ultra-dense regions.
    hover_css_str <- ""

    app_perf_mark_ms(plot_perf, "model_build_ms", app_perf_elapsed_ms(model_t0), plot_perf_context)
    girafe_t0 <- as.numeric(proc.time()[["elapsed"]])
    app_perf_mark(plot_perf, "girafe build start", plot_perf_context)
    plot_widget <- withCallingHandlers(
        girafe(
            ggobj = gg_lines,
            width_svg = width_svg,
            height_svg = height_svg,
            options = list(
                opts_hover(css = hover_css_str),
                opts_selection(type = "none"),
                opts_tooltip(
                    css = paste(
                        "background: var(--app-message-bg);",
                        "color: var(--app-message-text);",
                        "border: 1px solid var(--app-message-border);",
                        "border-radius: var(--app-message-radius);",
                        "padding: 8px 11px;",
                        "font-family: var(--app-font-family), Roboto, 'Helvetica Neue', Arial, sans-serif;",
                        "font-size: 12px;",
                        "font-weight: 400;",
                        "line-height: 1.4;",
                        "letter-spacing: 0.01em;",
                        "box-shadow: var(--app-message-shadow);"
                    ),
                    delay_mouseover = 45,
                    delay_mouseout = 180
                ),
                opts_toolbar(position = "topright", saveaspng = FALSE, hidden = c("selection", "zoom", "misc")),
                opts_sizing(rescale = FALSE)
            )
        ),
        warning = function(w) {
            msg <- conditionMessage(w)
            if (grepl("Failed setting attribute 'title'", msg, fixed = TRUE) ||
                grepl("Failed setting attribute 'data-id'", msg, fixed = TRUE) ||
                grepl("set_attr(name = attrName", msg, fixed = TRUE)) {
                invokeRestart("muffleWarning")
            }
        }
    )
    app_perf_mark(plot_perf, "girafe build done", plot_perf_context)
    app_perf_mark_ms(plot_perf, "girafe_build_ms", app_perf_elapsed_ms(girafe_t0), plot_perf_context)
    compact_t0 <- app_perf_now()
    if (exists("compact_girafe_widget", mode = "function")) {
        plot_widget <- compact_girafe_widget(
            plot_widget,
            label = paste0("gene_card:", as.character(plot_context %||% "plot"), ":", as.character(plot_id %||% "NA"))
        )
    }
    app_perf_mark_ms(plot_perf, "compact_svg_ms", app_perf_elapsed_ms(compact_t0), plot_perf_context)
    plot_html_bytes <- tryCatch(
        nchar(as.character(plot_widget$x$html %||% ""), type = "bytes"),
        error = function(e) NA_integer_
    )
    if (is.finite(plot_html_bytes)) {
        app_perf_mark(plot_perf, sprintf("plot_html_bytes=%d", as.integer(plot_html_bytes)), plot_perf_context)
    }
    plot_widget
}

# Función server del módulo de Plot para homólogos
plotServerHomologous <- function(id, data, max_gene_length, min_gene_coord, max_gene_coord, genSequences, plotIndex, gene_name, genome_fasta_path = NULL, annotation_file_path = NULL, visual_mode = NULL, orientation_mode = NULL, organism_name = NULL, use_report_map = FALSE, report_path = "", app_theme = NULL, app_colorblind = NULL, precomputed_neighbor_context = NULL, prefetch_sequence = TRUE, prefetch_neighbor_context = FALSE, perf_run_id = NULL, on_plot_ready = NULL, plot_signature = NULL) {
    moduleServer(
        id,
        function(input, output, session) {
            # OPT-4: Process data ONCE at module init (data is a static data.frame)
            module_init_t0 <- app_perf_now()
            processed_cache <- process_gene_data(data)
            model_data_key <- make_gene_plot_model_data_key(
                processed_cache$df, processed_cache$df_gene, processed_cache$df_transcript
            )

            # Reactive value to store gene info
            gene_info <- reactiveVal(NULL)
            genomic_span_seq <- reactiveVal("")
            neighbor_context <- reactiveVal(NULL)
            neighbor_context_resolved <- reactiveVal(FALSE)
            cached_plot_key <- reactiveVal("")
            cached_plot_obj <- reactiveVal(NULL)
            render_nonce <- reactiveVal(0L)
            deferred_sequence_prefetch_started <- reactiveVal(FALSE)
            gc_span_prefetch_started <- reactiveVal(FALSE)
            plot_ready_notified <- FALSE
            module_destroyed <- FALSE
            is_homo_plot_cache_enabled <- function() {
                raw <- tolower(trimws(as.character(Sys.getenv("APP_HOMO_PLOT_CACHE", "1") %||% "1")))
                !raw %in% c("", "0", "false", "no", "off")
            }
            is_homo_sequence_deferred <- function() {
                raw <- tolower(trimws(as.character(Sys.getenv("APP_HOMO_DEFER_SEQUENCE", "0") %||% "0")))
                !raw %in% c("", "0", "false", "no", "off")
            }
            is_feature_gc_deferred <- function() {
                raw <- tolower(trimws(as.character(Sys.getenv("APP_DEFER_FEATURE_GC", "0") %||% "0")))
                !raw %in% c("", "0", "false", "no", "off")
            }
            notify_plot_ready <- function() {
                if (!isTRUE(plot_ready_notified)) {
                    if (is.function(on_plot_ready)) {
                        tryCatch(on_plot_ready(as.character(plotIndex)), error = function(e) NULL)
                    }
                    plot_ready_notified <<- TRUE
                }
                invisible(NULL)
            }
            module_perf <- app_perf_new_run(sprintf("HOMO-%s", as.character(plotIndex %||% id)))
            perf_parent <- trimws(as.character(perf_run_id %||% ""))
            if (nzchar(perf_parent)) {
                module_perf$id <- paste0(perf_parent, "|plot=", as.character(plotIndex))
            }
            app_perf_mark(module_perf, "module init", "HOMO_MOD")
            app_perf_mark_ms(module_perf, "module_init_ms", app_perf_elapsed_ms(module_init_t0), "HOMO_MOD")
            destroy_module <- function() {
                if (isTRUE(module_destroyed)) {
                    return(invisible(FALSE))
                }
                module_destroyed <<- TRUE
                tryCatch({
                    output$plot <- NULL
                }, error = function(e) NULL)
                tryCatch(gene_info(NULL), error = function(e) NULL)
                tryCatch(neighbor_context(NULL), error = function(e) NULL)
                tryCatch(neighbor_context_resolved(FALSE), error = function(e) NULL)
                tryCatch(cached_plot_key(""), error = function(e) NULL)
                tryCatch(cached_plot_obj(NULL), error = function(e) NULL)
                processed_cache <<- NULL
                invisible(TRUE)
            }
            session$onSessionEnded(function() {
                destroy_module()
            })

            resolve_neighbor_context_sync <- function(step_prefix = "neighbor context") {
                neighbor_t0 <- app_perf_now()
                df_gene <- processed_cache$df_gene
                df_plot <- processed_cache$df
                if (is.null(annotation_file_path) || !nzchar(annotation_file_path) || (nrow(df_gene) <= 0 && nrow(df_plot) <= 0)) {
                    return(NULL)
                }
                app_perf_mark(module_perf, sprintf("%s start", as.character(step_prefix)), "HOMO_MOD")
                tgt_chr <- as.character(df_gene$V1[1] %||% df_plot$seqid[1] %||% data$V1[1] %||% NA_character_)
                tgt_start <- suppressWarnings(as.numeric(min(df_gene$V4, na.rm = TRUE)))
                tgt_end <- suppressWarnings(as.numeric(max(df_gene$V5, na.rm = TRUE)))
                if (!is.finite(tgt_start) || !is.finite(tgt_end) || tgt_end < tgt_start) {
                    tgt_start <- suppressWarnings(as.numeric(min(df_plot$xstart, na.rm = TRUE)))
                    tgt_end <- suppressWarnings(as.numeric(max(df_plot$xend, na.rm = TRUE)))
                }
                if (!is.finite(tgt_start) || !is.finite(tgt_end) || tgt_end < tgt_start) {
                    tgt_start <- suppressWarnings(as.numeric(min(data$V4, na.rm = TRUE)))
                    tgt_end <- suppressWarnings(as.numeric(max(data$V5, na.rm = TRUE)))
                }
                tgt_attr <- as.character(df_gene$V9[1] %||% data$V9[1] %||% "")
                target_gene <- data.frame(
                    gene_id = extract_primary_gene_id(tgt_attr),
                    chr = tgt_chr,
                    start = tgt_start,
                    end = tgt_end,
                    strand = as.character(df_gene$V7[1] %||% data$V7[1] %||% NA_character_),
                    stringsAsFactors = FALSE
                )
                out_ctx <- get_neighbor_context_for_target(annotation_file_path, target_gene)
                app_perf_mark(module_perf, sprintf("%s done", as.character(step_prefix)), "HOMO_MOD")
                app_perf_mark_ms(module_perf, "neighbor_context_ms", app_perf_elapsed_ms(neighbor_t0), "HOMO_MOD")
                out_ctx
            }

            resolve_gene_info_sync <- function(step_prefix = "prefetch", composition_only = FALSE) {
                df_gene <- processed_cache$df_gene
                df_plot <- processed_cache$df
                df_transcript <- processed_cache$df_transcript

                tx_start <- suppressWarnings(min(as.numeric(df_transcript$V4), na.rm = TRUE))
                tx_end <- suppressWarnings(max(as.numeric(df_transcript$V5), na.rm = TRUE))
                if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                    tx_start <- suppressWarnings(min(df_plot$xstart, na.rm = TRUE))
                    tx_end <- suppressWarnings(max(df_plot$xend, na.rm = TRUE))
                }
                if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                    tx_start <- suppressWarnings(min(df_gene$V4, na.rm = TRUE))
                    tx_end <- suppressWarnings(max(df_gene$V5, na.rm = TRUE))
                }
                if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                    tx_start <- suppressWarnings(min(as.numeric(data$V4), na.rm = TRUE))
                    tx_end <- suppressWarnings(max(data$V5, na.rm = TRUE))
                }

                tx_attr <- if (nrow(df_transcript) > 0) as.character(df_transcript$V9[1] %||% "") else ""
                tx_attrs <- parse_gff_attributes(tx_attr %||% "")
                gene_attr <- if (nrow(df_gene) > 0) as.character(df_gene$V9[1] %||% "") else ""
                tx_label_candidates <- c(
                    tx_attrs[["name"]][1],
                    tx_attrs[["transcript"]][1],
                    tx_attrs[["id"]][1],
                    tx_attrs[["transcript_id"]][1],
                    extract_primary_gene_name(gene_attr),
                    extract_primary_gene_id(gene_attr)
                )
                tx_label_candidates <- as.character(tx_label_candidates %||% character(0))
                tx_label_candidates <- tx_label_candidates[!is.na(tx_label_candidates) & nzchar(trimws(tx_label_candidates))]
                tx_label <- if (length(tx_label_candidates) > 0) tx_label_candidates[1] else "transcript"
                tx_label <- trimws(utils::URLdecode(tx_label))
                if (!nzchar(tx_label)) tx_label <- "transcript"
                tx_strand <- trimws(as.character(df_transcript$V7[1] %||% df_gene$V7[1] %||% data$V7[1] %||% "+"))
                if (!nzchar(tx_strand)) tx_strand <- "+"
                if (!tx_strand %in% c("+", "-")) tx_strand <- "+"

                exon_idx <- which(tolower(as.character(df_plot$feature_type %||% rep("", nrow(df_plot)))) == "exon")
                if (length(exon_idx) == 0) {
                    exon_idx <- which(tolower(as.character(df_plot$feature_type %||% rep("", nrow(df_plot)))) == "cds")
                }
                exon_ranges <- if (length(exon_idx) > 0) {
                    data.frame(
                        start = suppressWarnings(as.numeric(df_plot$xstart[exon_idx])),
                        end = suppressWarnings(as.numeric(df_plot$xend[exon_idx])),
                        stringsAsFactors = FALSE
                    )
                } else {
                    NULL
                }
                chr_for_fetch <- as.character(
                    df_gene$V1[1] %||%
                        df_transcript$V1[1] %||%
                        df_plot$seqid[1] %||%
                        data$V1[1] %||%
                        ""
                )

                if (isTRUE(composition_only)) {
                    comp_info <- NULL
                    if (!is.null(genome_fasta_path) && nzchar(genome_fasta_path) && file.exists(genome_fasta_path) &&
                        !is.null(exon_ranges) && nrow(normalize_exon_ranges(exon_ranges)) > 0L) {
                        comp_info <- tryCatch(
                            get_transcript_composition_cached(genome_fasta_path, chr_for_fetch, exon_ranges, strand = tx_strand),
                            error = function(e) NULL
                        )
                    }
                    comp_blob <- if (!is.null(comp_info)) make_sequence_composition_blob(comp_info) else ""
                    current_genSequences <- isolate(genSequences())
                    current_genSequences[[plotIndex]] <- comp_blob
                    genSequences(current_genSequences)
                    app_perf_mark(module_perf, sprintf("%s composition_only ready=%s", as.character(step_prefix), as.character(nzchar(comp_blob))), "HOMO_MOD")
                    return(list(sequence = "", file_content = comp_blob, fasta_id = tx_label, composition = comp_info, composition_blob = comp_blob))
                }

                result <- list(sequence = "", file_content = "", fasta_id = tx_label, composition_blob = "")
                if (!is.null(genome_fasta_path) && nzchar(genome_fasta_path) && file.exists(genome_fasta_path)) {
                    result <- fetch_gene_data_sync(
                        chr_for_fetch,
                        list(start = as.numeric(tx_start), end = as.numeric(tx_end)),
                        fasta_path = genome_fasta_path,
                        fasta_id = tx_label,
                        exon_ranges = exon_ranges,
                        strand = tx_strand
                    )
                }
                current_genSequences <- isolate(genSequences())
                current_genSequences[[plotIndex]] <- result$file_content %||% ""
                genSequences(current_genSequences)
                seq_len <- nchar(as.character(result$sequence %||% ""))
                app_perf_mark(module_perf, sprintf("%s resolved seq_len=%d", as.character(step_prefix), as.integer(seq_len)), "HOMO_MOD")
                result
            }

            schedule_gc_span_prefetch <- function(df_plot) {
                if (!isTRUE(is_feature_gc_deferred()) || isTRUE(gc_span_prefetch_started())) {
                    return(invisible(FALSE))
                }
                current_span <- trimws(as.character(isolate(genomic_span_seq()) %||% ""))
                if (nzchar(current_span)) {
                    return(invisible(FALSE))
                }
                gs_source_ok <- nzchar(as.character(genome_fasta_path %||% "")) && file.exists(genome_fasta_path)
                gs_start <- suppressWarnings(min(df_plot$xstart, na.rm = TRUE))
                gs_end <- suppressWarnings(max(df_plot$xend, na.rm = TRUE))
                gs_seqid <- as.character(df_plot$seqid[1] %||% "")
                if (!isTRUE(gs_source_ok) || !is.finite(gs_start) || !is.finite(gs_end) ||
                    gs_end <= gs_start || !nzchar(gs_seqid)) {
                    return(invisible(FALSE))
                }
                gc_span_prefetch_started(TRUE)
                run_prefetch <- function() {
                    if (isTRUE(module_destroyed)) return(invisible(NULL))
                    gc_t0 <- app_perf_now()
                    app_perf_mark(module_perf, "feature GC span prefetch start", "HOMO_MOD")
                    gs_seq <- tryCatch(
                        extract_sequence_from_fasta(genome_fasta_path, gs_seqid, as.integer(gs_start), as.integer(gs_end)),
                        error = function(e) ""
                    )
                    if (nzchar(gs_seq) && !isTRUE(module_destroyed)) {
                        genomic_span_seq(gs_seq)
                        render_nonce(as.integer(isolate(render_nonce()) %||% 0L) + 1L)
                    }
                    app_perf_mark(module_perf, sprintf("feature GC span prefetch done len=%d", as.integer(nchar(gs_seq %||% ""))), "HOMO_MOD")
                    app_perf_mark_ms(module_perf, "feature_gc_span_prefetch_ms", app_perf_elapsed_ms(gc_t0), "HOMO_MOD")
                    invisible(NULL)
                }
                if (requireNamespace("later", quietly = TRUE)) later::later(run_prefetch, delay = deferred_plot_enrichment_delay_seconds()) else run_prefetch()
                invisible(TRUE)
            }

            schedule_deferred_sequence_prefetch <- function() {
                if (!isTRUE(is_homo_sequence_deferred()) || isTRUE(deferred_sequence_prefetch_started())) {
                    return(invisible(FALSE))
                }
                if (!is.null(isolate(gene_info()))) {
                    return(invisible(FALSE))
                }
                deferred_sequence_prefetch_started(TRUE)
                run_prefetch <- function() {
                    if (isTRUE(module_destroyed)) return(invisible(NULL))
                    sequence_t0 <- app_perf_now()
                    app_perf_mark(module_perf, "deferred sequence prefetch start", "HOMO_MOD")
                    info <- tryCatch(
                        resolve_gene_info_sync("deferred_sequence", composition_only = TRUE),
                        error = function(e) {
                            app_perf_mark(module_perf, sprintf("deferred sequence error: %s", as.character(e$message %||% "unknown")), "HOMO_MOD")
                            list(sequence = "", file_content = "")
                        }
                    )
                    if (!isTRUE(module_destroyed)) {
                        gene_info(info)
                    }
                    app_perf_mark_ms(module_perf, "deferred_sequence_prefetch_ms", app_perf_elapsed_ms(sequence_t0), "HOMO_MOD")
                    invisible(NULL)
                }
                if (requireNamespace("later", quietly = TRUE)) later::later(run_prefetch, delay = deferred_plot_enrichment_delay_seconds(0.5)) else run_prefetch()
                invisible(TRUE)
            }

            # This module receives a static data.frame. Starting its one-shot
            # prefetch through observe() postponed it until a later Shiny flush;
            # on large searches that idle queue wait was several seconds longer
            # than the sequence read itself. Run it during module construction so
            # the first bound output already has sequence/GC data available.
            run_initial_prefetch <- function() {
                req(data)
                app_perf_mark(module_perf, "initial prefetch start", "HOMO_MOD")
                # OPT-4: Use pre-computed processed data
                df_gene <- processed_cache$df_gene
                df_plot <- processed_cache$df
                df_transcript <- processed_cache$df_transcript
                defer_sequence <- isTRUE(is_homo_sequence_deferred())
                defer_feature_gc <- isTRUE(is_feature_gc_deferred())

                if (!is.null(precomputed_neighbor_context)) {
                    neighbor_context(precomputed_neighbor_context)
                    neighbor_context_resolved(TRUE)
                    app_perf_mark(module_perf, "neighbor context precomputed", "HOMO_MOD")
                } else if (isTRUE(prefetch_neighbor_context)) {
                    neighbor_context(resolve_neighbor_context_sync("neighbor context prefetch"))
                    neighbor_context_resolved(TRUE)
                } else {
                    neighbor_context(NULL)
                    neighbor_context_resolved(FALSE)
                    app_perf_mark(module_perf, "neighbor context deferred", "HOMO_MOD")
                }

                if (isTRUE(prefetch_sequence)) {
                    app_perf_mark(module_perf, "async prefetch launch", "HOMO_MOD")
                    tx_start <- suppressWarnings(min(as.numeric(df_transcript$V4), na.rm = TRUE))
                    tx_end <- suppressWarnings(max(as.numeric(df_transcript$V5), na.rm = TRUE))
                    if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                        tx_start <- suppressWarnings(min(df_plot$xstart, na.rm = TRUE))
                        tx_end <- suppressWarnings(max(df_plot$xend, na.rm = TRUE))
                    }
                    if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                        tx_start <- suppressWarnings(min(df_gene$V4, na.rm = TRUE))
                        tx_end <- suppressWarnings(max(df_gene$V5, na.rm = TRUE))
                    }
                    if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                        tx_start <- suppressWarnings(min(as.numeric(data$V4), na.rm = TRUE))
                        tx_end <- suppressWarnings(max(as.numeric(data$V5), na.rm = TRUE))
                    }

                    tx_attr <- if (nrow(df_transcript) > 0) as.character(df_transcript$V9[1] %||% "") else ""
                    tx_attrs <- parse_gff_attributes(tx_attr %||% "")
                    gene_attr <- if (nrow(df_gene) > 0) as.character(df_gene$V9[1] %||% "") else ""
                    tx_label_candidates <- c(
                        tx_attrs[["name"]][1],
                        tx_attrs[["transcript"]][1],
                        tx_attrs[["id"]][1],
                        tx_attrs[["transcript_id"]][1],
                        extract_primary_gene_name(gene_attr),
                        extract_primary_gene_id(gene_attr)
                    )
                    tx_label_candidates <- as.character(tx_label_candidates %||% character(0))
                    tx_label_candidates <- tx_label_candidates[!is.na(tx_label_candidates) & nzchar(trimws(tx_label_candidates))]
                    tx_label <- if (length(tx_label_candidates) > 0) tx_label_candidates[1] else "transcript"
                    tx_label <- trimws(utils::URLdecode(tx_label))
                    if (!nzchar(tx_label)) tx_label <- "transcript"
                    tx_strand <- trimws(as.character(df_transcript$V7[1] %||% df_gene$V7[1] %||% data$V7[1] %||% "+"))
                    if (!nzchar(tx_strand)) tx_strand <- "+"
                    if (!tx_strand %in% c("+", "-")) tx_strand <- "+"

                    exon_idx <- which(tolower(as.character(df_plot$feature_type %||% rep("", nrow(df_plot)))) == "exon")
                    if (length(exon_idx) == 0) {
                        exon_idx <- which(tolower(as.character(df_plot$feature_type %||% rep("", nrow(df_plot)))) == "cds")
                    }
                    exon_ranges <- if (length(exon_idx) > 0) {
                        data.frame(
                            start = suppressWarnings(as.numeric(df_plot$xstart[exon_idx])),
                            end = suppressWarnings(as.numeric(df_plot$xend[exon_idx])),
                            stringsAsFactors = FALSE
                        )
                    } else {
                        NULL
                    }
                    chr_for_fetch <- as.character(
                        df_gene$V1[1] %||%
                            df_transcript$V1[1] %||%
                            df_plot$seqid[1] %||%
                            data$V1[1] %||%
                            ""
                    )

                    gs_source_ok <- nzchar(as.character(genome_fasta_path %||% "")) && file.exists(genome_fasta_path)
                    gs_start <- suppressWarnings(min(df_plot$xstart, na.rm = TRUE))
                    gs_end <- suppressWarnings(max(df_plot$xend, na.rm = TRUE))
                    gs_seqid <- as.character(df_plot$seqid[1] %||% "")

                    local_genome <- genome_fasta_path
                    local_gs_seqid <- gs_seqid
                    local_gs_start <- gs_start
                    local_gs_end <- gs_end
                    local_gs_ok <- isTRUE(gs_source_ok)
                    local_chr <- chr_for_fetch
                    local_tx_coords <- list(start = as.numeric(tx_start), end = as.numeric(tx_end))
                    local_tx_label <- tx_label
                    local_exon_ranges <- exon_ranges
                    local_tx_strand <- tx_strand
                    inline_fast <- should_inline_fast_sequence_prefetch(
                        local_genome,
                        min(c(local_gs_start, local_tx_coords$start), na.rm = TRUE),
                        max(c(local_gs_end, local_tx_coords$end), na.rm = TRUE)
                    )
                    # A small 2bit span is cheaper to read now than to rebuild the
                    # complete ggiraph widget after the deferred GC fetch.
                    local_need_gc_span <- !isTRUE(defer_feature_gc) || isTRUE(inline_fast)
                    local_need_sequence <- isTRUE(prefetch_sequence) && !isTRUE(defer_sequence)

                    fn_extract_seq <- extract_sequence_from_fasta
                    fn_fetch_gene <- fetch_gene_data_sync

                    run_sequence_prefetch <- function() {
                        gs_seq <- ""
                        gc_span_fetch_ms <- 0
                        if (isTRUE(local_need_gc_span) && isTRUE(local_gs_ok) && is.finite(local_gs_start) && is.finite(local_gs_end) &&
                            local_gs_end > local_gs_start && nzchar(local_gs_seqid)) {
                            gc_span_t0 <- app_perf_now()
                            gs_seq <- tryCatch(
                                fn_extract_seq(local_genome, local_gs_seqid, as.integer(local_gs_start), as.integer(local_gs_end)),
                                error = function(e) ""
                            )
                            gc_span_fetch_ms <- app_perf_elapsed_ms(gc_span_t0)
                        }
                        seq_result <- list(sequence = "", file_content = "", fasta_id = local_tx_label)
                        gene_sequence_fetch_ms <- 0
                        if (isTRUE(local_need_sequence) && !is.null(local_genome) && nzchar(local_genome) && file.exists(local_genome)) {
                            gene_sequence_t0 <- app_perf_now()
                            seq_result <- tryCatch(
                                fn_fetch_gene(local_chr, local_tx_coords,
                                    fasta_path = local_genome, fasta_id = local_tx_label,
                                    exon_ranges = local_exon_ranges, strand = local_tx_strand),
                                error = function(e) list(sequence = "", file_content = "", fasta_id = local_tx_label)
                            )
                            gene_sequence_fetch_ms <- app_perf_elapsed_ms(gene_sequence_t0)
                        }
                        list(
                            gs_seq = gs_seq,
                            seq_result = seq_result,
                            gc_span_fetch_ms = gc_span_fetch_ms,
                            gene_sequence_fetch_ms = gene_sequence_fetch_ms
                        )
                    }
                    apply_sequence_prefetch <- function(result, source_label = "async") {
                        apply_prefetch_t0 <- app_perf_now()
                        if (isTRUE(module_destroyed)) return(invisible(NULL))
                        app_perf_mark_ms(module_perf, "gc_span_fetch_ms", result$gc_span_fetch_ms %||% 0, "HOMO_MOD")
                        app_perf_mark_ms(module_perf, "gene_sequence_fetch_ms", result$gene_sequence_fetch_ms %||% 0, "HOMO_MOD")
                        span_bp <- if (is.finite(local_gs_start) && is.finite(local_gs_end)) {
                            max(0, as.numeric(local_gs_end) - as.numeric(local_gs_start) + 1)
                        } else {
                            0
                        }
                        app_perf_mark(
                            module_perf,
                            sprintf("%s prefetch source=%s span_bp=%.0f", source_label, tools::file_ext(local_genome %||% ""), span_bp),
                            "HOMO_MOD"
                        )
                        genomic_span_seq(result$gs_seq %||% "")
                        if (isTRUE(local_need_sequence)) {
                            gene_info(result$seq_result %||% list(sequence = "", file_content = ""))
                            current_genSequences <- isolate(genSequences())
                            current_genSequences[[plotIndex]] <- result$seq_result$file_content %||% ""
                            genSequences(current_genSequences)
                            seq_len <- nchar(as.character(result$seq_result$sequence %||% ""))
                            app_perf_mark(
                                module_perf,
                                sprintf("%s prefetch done seq_len=%d", source_label, as.integer(seq_len)),
                                "HOMO_MOD"
                            )
                        } else {
                            gene_info(NULL)
                            app_perf_mark(module_perf, sprintf("%s prefetch skipped sequence for first paint", source_label), "HOMO_MOD")
                        }
                        app_perf_mark_ms(module_perf, "apply_sequence_prefetch_ms", app_perf_elapsed_ms(apply_prefetch_t0), "HOMO_MOD")
                        invisible(NULL)
                    }
                    handle_sequence_prefetch_error <- function(err, source_label = "async") {
                        if (isTRUE(module_destroyed)) return(invisible(NULL))
                        app_perf_mark(
                            module_perf,
                            sprintf("%s prefetch error: %s", source_label, as.character(err$message %||% "unknown")),
                            "HOMO_MOD"
                        )
                        gene_info(list(sequence = "", file_content = ""))
                        current_genSequences <- isolate(genSequences())
                        current_genSequences[[plotIndex]] <- ""
                        genSequences(current_genSequences)
                        invisible(NULL)
                    }

                    if (!isTRUE(local_need_gc_span) && !isTRUE(local_need_sequence)) {
                        gene_info(NULL)
                        app_perf_mark(module_perf, "initial sequence and GC prefetch fully deferred", "HOMO_MOD")
                    } else if (isTRUE(inline_fast)) {
                        inline_t0 <- app_perf_now()
                        app_perf_mark(module_perf, "inline fast prefetch start", "HOMO_MOD")
                        tryCatch(
                            apply_sequence_prefetch(run_sequence_prefetch(), "inline fast"),
                            error = function(err) handle_sequence_prefetch_error(err, "inline fast")
                        )
                        app_perf_mark_ms(
                            module_perf,
                            "inline_fast_prefetch_ms",
                            app_perf_elapsed_ms(inline_t0),
                            "HOMO_MOD"
                        )
                    } else {
                        promises::future_promise(
                            run_sequence_prefetch(),
                            seed = FALSE
                        ) %...>% (function(result) {
                            apply_sequence_prefetch(result, "async")
                        }) %...!% (function(err) {
                            handle_sequence_prefetch_error(err, "async")
                        })
                    }
                } else {
                    gene_info(NULL)
                    app_perf_mark(module_perf, "prefetch disabled", "HOMO_MOD")
                }
                invisible(NULL)
            }
            run_initial_prefetch()

            render_trigger <- reactive({
                info_evt <- NULL
                defer_sequence <- isTRUE(is_homo_sequence_deferred())
                if (isTRUE(prefetch_sequence) && !isTRUE(defer_sequence)) {
                    info_evt <- gene_info()
                    if (is.null(info_evt)) {
                        return(NULL)
                    }
                } else {
                    info_evt <- isolate(gene_info())
                }
                visual_mode_evt <- "compact"
                if (!is.null(visual_mode)) {
                    visual_mode_evt <- tryCatch(as.character(visual_mode())[1], error = function(e) "compact")
                }
                visual_mode_evt <- tolower(visual_mode_evt %||% "compact")
                if (!visual_mode_evt %in% c("compact", "detailed")) visual_mode_evt <- "compact"
                orientation_mode_evt <- "genomic"
                if (!is.null(orientation_mode)) {
                    orientation_mode_evt <- tryCatch(as.character(orientation_mode())[1], error = function(e) "genomic")
                }
                orientation_mode_evt <- normalize_gene_plot_orientation_mode(orientation_mode_evt)
                theme_mode_evt <- "light"
                if (!is.null(app_theme)) {
                    theme_mode_evt <- tryCatch(as.character(app_theme())[1], error = function(e) "light")
                }
                theme_mode_evt <- tolower(theme_mode_evt %||% "light")
                if (!theme_mode_evt %in% c("light", "dark")) theme_mode_evt <- "light"
                colorblind_evt <- FALSE
                if (!is.null(app_colorblind)) {
                    colorblind_evt <- tryCatch(isTRUE(app_colorblind()), error = function(e) FALSE)
                }
                seq_len_evt <- 0L
                if (!is.null(info_evt) && !is.null(info_evt$sequence)) {
                    seq_len_evt <- suppressWarnings(as.integer(nchar(as.character(info_evt$sequence %||% ""))))
                    if (!is.finite(seq_len_evt)) seq_len_evt <- 0L
                }
                max_gene_length_evt <- suppressWarnings(as.numeric(max_gene_length() %||% 0))
                if (!is.finite(max_gene_length_evt) || is.na(max_gene_length_evt) || max_gene_length_evt < 0) {
                    max_gene_length_evt <- 0
                }
                neighbor_evt <- if (isTRUE(prefetch_neighbor_context)) {
                    !is.null(neighbor_context())
                } else {
                    !is.null(isolate(neighbor_context()))
                }
                gc_span_len_evt <- if (isTRUE(is_feature_gc_deferred())) {
                    suppressWarnings(as.integer(nchar(as.character(genomic_span_seq() %||% ""))))
                } else {
                    0L
                }
                if (!is.finite(gc_span_len_evt)) gc_span_len_evt <- 0L
                list(
                    visual_mode_evt,
                    orientation_mode_evt,
                    theme_mode_evt,
                    colorblind_evt,
                    as.logical(neighbor_evt),
                    seq_len_evt,
                    as.integer(gc_span_len_evt),
                    as.integer(round(max_gene_length_evt)),
                    render_nonce()
                )
            })

            output$plot <- bindEvent(renderGirafe({
                render_prepare_t0 <- app_perf_now()
                on.exit(notify_plot_ready(), add = TRUE)
                req(data)
                info <- gene_info()
                if (!isTRUE(prefetch_neighbor_context) && !isTRUE(neighbor_context_resolved()) && is.null(precomputed_neighbor_context)) {
                    lazy_ctx <- tryCatch(
                        resolve_neighbor_context_sync("neighbor context lazy"),
                        error = function(e) {
                            app_perf_mark(module_perf, sprintf("neighbor context lazy error: %s", as.character(e$message %||% "unknown")), "HOMO_MOD")
                            NULL
                        }
                    )
                    neighbor_context(lazy_ctx)
                    neighbor_context_resolved(TRUE)
                }
                current_neighbor_context <- if (isTRUE(prefetch_neighbor_context)) neighbor_context() else isolate(neighbor_context())
                app_perf_mark(module_perf, sprintf("render start info_ready=%s", ifelse(is.null(info), "FALSE", "TRUE")), "HOMO_MOD")
                defer_sequence <- isTRUE(is_homo_sequence_deferred())
                if (isTRUE(prefetch_sequence) && !isTRUE(defer_sequence)) {
                    req(!is.null(info))
                }
                width_svg <- 16.7

                # Detectamos el modo visual AHORA para darle la altura exacta al SVG
                this_visual_mode <- "compact"
                if (!is.null(visual_mode)) {
                    candidate_mode <- tryCatch(as.character(visual_mode())[1], error = function(e) NA_character_)
                    if (tolower(candidate_mode %||% "compact") %in% c("compact", "detailed")) {
                        this_visual_mode <- tolower(candidate_mode)
                    }
                }
                this_orientation_mode <- "genomic"
                if (!is.null(orientation_mode)) {
                    this_orientation_mode <- tryCatch(as.character(orientation_mode())[1], error = function(e) "genomic")
                }
                this_orientation_mode <- normalize_gene_plot_orientation_mode(this_orientation_mode)

                # Si es compacta, el lienzo es delgado. Si es detallada, crece.
                height_svg <- if (this_visual_mode == "compact") 1.15 else 1.85
                # OPT-4: Use pre-computed processed data
                df <- processed_cache$df
                df_gene <- processed_cache$df_gene
                df_transcript <- processed_cache$df_transcript

                # The legacy plot argument is unused; footer composition already
                # comes from the prepared sequence data.
                composicion_secuencia <- NULL

                transcript_start <- suppressWarnings(min(df$xstart, na.rm = TRUE))
                transcript_end <- suppressWarnings(max(df$xend, na.rm = TRUE))
                if (!is.finite(transcript_start) || !is.finite(transcript_end) || transcript_end <= transcript_start) {
                    transcript_start <- suppressWarnings(min(df_gene$V4, na.rm = TRUE))
                    transcript_end <- suppressWarnings(max(df_gene$V5, na.rm = TRUE))
                }
                current_transcript_length <- as.numeric(transcript_end - transcript_start + 1)
                if (!is.finite(current_transcript_length) || current_transcript_length <= 0) current_transcript_length <- 1
                current_max_gene_length <- isolate(suppressWarnings(as.numeric(max_gene_length() %||% current_transcript_length)))
                if (!is.finite(current_max_gene_length) || current_max_gene_length <= 0) {
                    current_max_gene_length <- current_transcript_length
                }
                length_difference <- max(0, current_max_gene_length - current_transcript_length)

                gene_length_bp <- if (nrow(df_gene) > 0) as.numeric(max(df_gene$V5) - min(df_gene$V4) + 1) else current_transcript_length
                gene_length_label <- sprintf("Gene Length: %s pb", format(as.integer(round(gene_length_bp)), big.mark = ","))
                transcript_length_label <- sprintf("Transcript Length: %s pb", format(as.integer(round(current_transcript_length)), big.mark = ","))
                theme_mode <- "light"
                if (!is.null(app_theme)) {
                    theme_mode <- tryCatch(as.character(app_theme())[1], error = function(e) "light")
                }
                theme_mode <- tolower(theme_mode %||% "light")
                is_dark_theme <- identical(theme_mode, "dark")

                is_colorblind_mode <- FALSE
                if (!is.null(app_colorblind)) {
                    is_colorblind_mode <- tryCatch(isTRUE(app_colorblind()), error = function(e) FALSE)
                }

                cache_enabled <- isTRUE(is_homo_plot_cache_enabled())
                max_gene_length_key <- suppressWarnings(as.numeric(max_gene_length() %||% 0))
                if (!is.finite(max_gene_length_key)) max_gene_length_key <- 0
                seq_len_key <- 0L
                if (!is.null(info) && !is.null(info$sequence)) {
                    seq_len_key <- suppressWarnings(as.integer(nchar(as.character(info$sequence %||% ""))))
                    if (!is.finite(seq_len_key)) seq_len_key <- 0L
                }
                has_neighbor_context <- !is.null(current_neighbor_context)
                gc_span_len_key <- suppressWarnings(as.integer(nchar(as.character(genomic_span_seq() %||% ""))))
                if (!is.finite(gc_span_len_key)) gc_span_len_key <- 0L
                defer_feature_gc <- isTRUE(is_feature_gc_deferred())
                compact_feature_key <- paste(
                    if (identical(this_visual_mode, "compact")) is_compact_feature_interactivity_enabled() else TRUE,
                    if (isTRUE(defer_feature_gc)) gc_span_len_key else "eager",
                    sep = ":gc="
                )
                cache_key <- make_girafe_plot_cache_key(
                    "homologous",
                    plot_signature = plot_signature,
                    fallback_id = plotIndex,
                    max_gene_length_key = max_gene_length_key,
                    visual_mode = this_visual_mode,
                    theme_mode = theme_mode,
                    is_colorblind_mode = is_colorblind_mode,
                    seq_len_key = seq_len_key,
                    has_neighbor_context = has_neighbor_context,
                    compact_feature_interactivity = compact_feature_key,
                    orientation_mode = this_orientation_mode
                )

                if (cache_enabled) {
                    if (identical(cached_plot_key(), cache_key)) {
                        cached_plot <- cached_plot_obj()
                        if (!is.null(cached_plot)) {
                            app_perf_mark(module_perf, "render cache hit", "HOMO_MOD")
                            return(refresh_girafe_widget_uid(cached_plot))
                        }
                    }
                    shared_plot <- get_shared_girafe_plot_cache(cache_key)
                    if (!is.null(shared_plot)) {
                        app_perf_mark(module_perf, "render shared cache hit", "HOMO_MOD")
                        cached_plot_key(cache_key)
                        cached_plot_obj(shared_plot)
                        return(refresh_girafe_widget_uid(shared_plot))
                    }
                    app_perf_mark(module_perf, "render cache miss", "HOMO_MOD")
                } else {
                    app_perf_mark(module_perf, "render cache disabled", "HOMO_MOD")
                    if (!identical(cached_plot_key(), "") || !is.null(cached_plot_obj())) {
                        cached_plot_key("")
                        cached_plot_obj(NULL)
                    }
                }

                gc_timing_enabled <- isTRUE(app_perf_enabled())
                gc_before <- if (gc_timing_enabled) sum(gc.time(on = TRUE)) else NA_real_
                create_t0 <- app_perf_now()
                app_perf_mark_ms(module_perf, "render_prepare_ms", app_perf_elapsed_ms(render_prepare_t0), "HOMO_MOD")
                app_perf_mark(module_perf, "create_gene_plot start", "HOMO_MOD")
                span_for_plot <- genomic_span_seq()
                genome_for_plot <- if (isTRUE(defer_feature_gc) && !nzchar(trimws(as.character(span_for_plot %||% "")))) {
                    ""
                } else {
                    genome_fasta_path
                }
                plot_obj <- create_gene_plot(
                    df, df_gene, df_transcript, current_transcript_length, length_difference,
                    composicion_secuencia, gene_length_label, transcript_length_label,
                    neighbor_context = current_neighbor_context,
                    visual_mode = this_visual_mode,
                    width_svg = width_svg,
                    height_svg = height_svg,
                    organism_label = organism_name,
                    annotation_file_path = annotation_file_path,
                    use_report_map = use_report_map,
                    report_path = report_path,
                    plot_id = as.character(plotIndex),
                    plot_context = "homologous",
                    genome_fasta_path = genome_for_plot,
                    is_dark_theme = is_dark_theme,
                    is_colorblind_mode = is_colorblind_mode,
                    gene_display_name = gene_name,
                    precomputed_genomic_span = span_for_plot,
                    model_cache_key = model_data_key,
                    orientation_mode = this_orientation_mode,
                    caller_started_at = create_t0
                )
                if (gc_timing_enabled) {
                    gc_after <- sum(gc.time())
                    app_perf_mark_ms(
                        module_perf,
                        "create_gene_plot_gc_ms",
                        1000 * max(0, gc_after - gc_before),
                        "HOMO_MOD"
                    )
                }
                if (isTRUE(defer_feature_gc) && !nzchar(trimws(as.character(span_for_plot %||% "")))) {
                    schedule_gc_span_prefetch(df)
                }
                if (isTRUE(defer_sequence) && is.null(info)) {
                    schedule_deferred_sequence_prefetch()
                }
                app_perf_mark(module_perf, "create_gene_plot done", "HOMO_MOD")
                app_perf_mark_ms(module_perf, "create_gene_plot_ms", app_perf_elapsed_ms(create_t0), "HOMO_MOD")
                if (cache_enabled) {
                    cached_plot_key(cache_key)
                    cached_plot_obj(plot_obj)
                    set_shared_girafe_plot_cache(cache_key, plot_obj)
                }
                plot_obj
            }), render_trigger(), ignoreNULL = TRUE)
            # Card creation is already progressive: only the visible canonical
            # card (or an explicitly requested isoform) gets a server module.
            # Rendering that module eagerly avoids waiting for the browser to
            # bind the just-inserted output before the first SVG can be built.
            outputOptions(output, "plot", suspendWhenHidden = FALSE)
            app_perf_mark(module_perf, "plot output registered eager", "HOMO_MOD")
            nudge_render_nonce <- function(reason = "manually") {
                if (isTRUE(module_destroyed)) {
                    return(invisible(FALSE))
                }
                current_nonce <- isolate(render_nonce())
                if (!is.finite(suppressWarnings(as.numeric(current_nonce)))) {
                    current_nonce <- 0L
                }
                render_nonce(as.integer(current_nonce) + 1L)
                app_perf_mark(module_perf, sprintf("render nonce nudged %s", reason), "HOMO_MOD")
                invisible(TRUE)
            }
            list(
                destroy = destroy_module,
                is_destroyed = function() {
                    isTRUE(module_destroyed)
                },
                nudge_render = function() {
                    nudge_render_nonce("manually")
                },
                set_background_render = function(enabled = TRUE) {
                    if (isTRUE(module_destroyed)) {
                        return(invisible(FALSE))
                    }
                    enabled <- isTRUE(enabled)
                    outputOptions(output, "plot", suspendWhenHidden = !enabled)
                    if (enabled) {
                        nudge_render_nonce("for Figure Studio")
                    }
                    invisible(TRUE)
                }
            )
        }
    )
}

# Función server del módulo de Plot para ortólogos
plotServerOrtologous <- function(id, data, max_gene_length, min_gene_coord, max_gene_coord, genSequences, plotIndex, gene_name, genome_fasta_path = NULL, annotation_file_path = NULL, visual_mode = NULL, orientation_mode = NULL, organism_name = NULL, use_report_map = FALSE, report_path = "", app_theme = NULL, app_colorblind = NULL, precomputed_neighbor_context = NULL, prefetch_sequence = TRUE, prefetch_neighbor_context = FALSE, perf_run_id = NULL, on_plot_ready = NULL, plot_signature = NULL) {
    moduleServer(
        id,
        function(input, output, session) {
            # OPT-4: Process data ONCE at module init (data is a static data.frame)
            module_init_t0 <- app_perf_now()
            processed_cache <- process_gene_data(data)
            model_data_key <- make_gene_plot_model_data_key(
                processed_cache$df, processed_cache$df_gene, processed_cache$df_transcript
            )

            gene_info <- reactiveVal(NULL)
            genomic_span_seq <- reactiveVal("")
            neighbor_context <- reactiveVal(NULL)
            neighbor_context_resolved <- reactiveVal(FALSE)
            cached_plot_key <- reactiveVal("")
            cached_plot_obj <- reactiveVal(NULL)
            render_nonce <- reactiveVal(0L)
            deferred_sequence_prefetch_started <- reactiveVal(FALSE)
            gc_span_prefetch_started <- reactiveVal(FALSE)
            plot_ready_notified <- FALSE
            module_destroyed <- FALSE
            is_ortho_plot_cache_enabled <- function() {
                raw <- tolower(trimws(as.character(Sys.getenv("APP_ORTHO_PLOT_CACHE", "1") %||% "1")))
                !raw %in% c("", "0", "false", "no", "off")
            }
            is_ortho_sequence_deferred <- function() {
                raw <- tolower(trimws(as.character(Sys.getenv("APP_ORTHO_DEFER_SEQUENCE", "0") %||% "0")))
                !raw %in% c("", "0", "false", "no", "off")
            }
            is_feature_gc_deferred <- function() {
                raw <- tolower(trimws(as.character(Sys.getenv("APP_DEFER_FEATURE_GC", "0") %||% "0")))
                !raw %in% c("", "0", "false", "no", "off")
            }
            schedule_gc_span_prefetch <- function(df_plot) {
                if (!isTRUE(is_feature_gc_deferred()) || isTRUE(gc_span_prefetch_started())) {
                    return(invisible(FALSE))
                }
                current_span <- trimws(as.character(isolate(genomic_span_seq()) %||% ""))
                if (nzchar(current_span)) {
                    return(invisible(FALSE))
                }
                gs_source_ok <- nzchar(as.character(genome_fasta_path %||% "")) && file.exists(genome_fasta_path)
                gs_start <- suppressWarnings(min(df_plot$xstart, na.rm = TRUE))
                gs_end <- suppressWarnings(max(df_plot$xend, na.rm = TRUE))
                gs_seqid <- as.character(df_plot$seqid[1] %||% "")
                if (!isTRUE(gs_source_ok) || !is.finite(gs_start) || !is.finite(gs_end) ||
                    gs_end <= gs_start || !nzchar(gs_seqid)) {
                    return(invisible(FALSE))
                }
                gc_span_prefetch_started(TRUE)
                run_prefetch <- function() {
                    if (isTRUE(module_destroyed)) {
                        return(invisible(NULL))
                    }
                    gc_t0 <- app_perf_now()
                    app_perf_mark(module_perf, "feature GC span prefetch start", "ORTHO_MOD")
                    gs_seq <- tryCatch(
                        extract_sequence_from_fasta(
                            genome_fasta_path,
                            gs_seqid,
                            as.integer(gs_start),
                            as.integer(gs_end)
                        ),
                        error = function(e) ""
                    )
                    if (nzchar(gs_seq) && !isTRUE(module_destroyed)) {
                        genomic_span_seq(gs_seq)
                        render_nonce(as.integer(isolate(render_nonce()) %||% 0L) + 1L)
                    }
                    app_perf_mark(
                        module_perf,
                        sprintf("feature GC span prefetch done len=%d", as.integer(nchar(gs_seq %||% ""))),
                        "ORTHO_MOD"
                    )
                    app_perf_mark_ms(module_perf, "feature_gc_span_prefetch_ms", app_perf_elapsed_ms(gc_t0), "ORTHO_MOD")
                    invisible(NULL)
                }
                if (requireNamespace("later", quietly = TRUE)) later::later(run_prefetch, delay = deferred_plot_enrichment_delay_seconds()) else run_prefetch()
                invisible(TRUE)
            }
            schedule_deferred_sequence_prefetch <- function() {
                if (!isTRUE(is_ortho_sequence_deferred()) || isTRUE(deferred_sequence_prefetch_started())) {
                    return(invisible(FALSE))
                }
                if (!is.null(isolate(gene_info()))) {
                    return(invisible(FALSE))
                }
                deferred_sequence_prefetch_started(TRUE)
                run_prefetch <- function() {
                    if (isTRUE(module_destroyed)) {
                        return(invisible(NULL))
                    }
                    sequence_t0 <- app_perf_now()
                    app_perf_mark(module_perf, "deferred sequence prefetch start", "ORTHO_MOD")
                    info <- tryCatch(
                        resolve_gene_info_sync("deferred_sequence", composition_only = TRUE),
                        error = function(e) {
                            app_perf_mark(module_perf, sprintf("deferred sequence error: %s", as.character(e$message %||% "unknown")), "ORTHO_MOD")
                            list(sequence = "", file_content = "")
                        }
                    )
                    if (!isTRUE(module_destroyed)) {
                        gene_info(info)
                    }
                    app_perf_mark_ms(module_perf, "deferred_sequence_prefetch_ms", app_perf_elapsed_ms(sequence_t0), "ORTHO_MOD")
                    invisible(NULL)
                }
                if (requireNamespace("later", quietly = TRUE)) later::later(run_prefetch, delay = deferred_plot_enrichment_delay_seconds(0.5)) else run_prefetch()
                invisible(TRUE)
            }
            notify_plot_ready <- function() {
                if (!isTRUE(plot_ready_notified)) {
                    if (is.function(on_plot_ready)) {
                        tryCatch(on_plot_ready(as.character(plotIndex)), error = function(e) NULL)
                    }
                    plot_ready_notified <<- TRUE
                }
                invisible(NULL)
            }
            module_perf <- app_perf_new_run(sprintf("ORTHO-%s", as.character(plotIndex %||% id)))
            perf_parent <- trimws(as.character(perf_run_id %||% ""))
            if (nzchar(perf_parent)) {
                module_perf$id <- paste0(perf_parent, "|plot=", as.character(plotIndex))
            }
            app_perf_mark(module_perf, "module init", "ORTHO_MOD")
            app_perf_mark_ms(module_perf, "module_init_ms", app_perf_elapsed_ms(module_init_t0), "ORTHO_MOD")
            destroy_module <- function() {
                if (isTRUE(module_destroyed)) {
                    return(invisible(FALSE))
                }
                module_destroyed <<- TRUE
                tryCatch({
                    output$plot <- NULL
                }, error = function(e) NULL)
                tryCatch(gene_info(NULL), error = function(e) NULL)
                tryCatch(neighbor_context(NULL), error = function(e) NULL)
                tryCatch(neighbor_context_resolved(FALSE), error = function(e) NULL)
                tryCatch(cached_plot_key(""), error = function(e) NULL)
                tryCatch(cached_plot_obj(NULL), error = function(e) NULL)
                processed_cache <<- NULL
                invisible(TRUE)
            }
            session$onSessionEnded(function() {
                destroy_module()
            })

            resolve_neighbor_context_sync <- function(step_prefix = "neighbor context") {
                neighbor_t0 <- app_perf_now()
                df_gene <- processed_cache$df_gene
                df_plot <- processed_cache$df
                if (is.null(annotation_file_path) || !nzchar(annotation_file_path) || (nrow(df_gene) <= 0 && nrow(df_plot) <= 0)) {
                    return(NULL)
                }
                app_perf_mark(module_perf, sprintf("%s start", as.character(step_prefix)), "ORTHO_MOD")
                tgt_chr <- as.character(df_gene$V1[1] %||% df_plot$seqid[1] %||% data$V1[1] %||% NA_character_)
                tgt_start <- suppressWarnings(as.numeric(min(df_gene$V4, na.rm = TRUE)))
                tgt_end <- suppressWarnings(as.numeric(max(df_gene$V5, na.rm = TRUE)))
                if (!is.finite(tgt_start) || !is.finite(tgt_end) || tgt_end < tgt_start) {
                    tgt_start <- suppressWarnings(as.numeric(min(df_plot$xstart, na.rm = TRUE)))
                    tgt_end <- suppressWarnings(as.numeric(max(df_plot$xend, na.rm = TRUE)))
                }
                if (!is.finite(tgt_start) || !is.finite(tgt_end) || tgt_end < tgt_start) {
                    tgt_start <- suppressWarnings(as.numeric(min(data$V4, na.rm = TRUE)))
                    tgt_end <- suppressWarnings(as.numeric(max(data$V5, na.rm = TRUE)))
                }
                tgt_attr <- as.character(df_gene$V9[1] %||% data$V9[1] %||% "")
                target_gene <- data.frame(
                    gene_id = extract_primary_gene_id(tgt_attr),
                    chr = tgt_chr,
                    start = tgt_start,
                    end = tgt_end,
                    strand = as.character(df_gene$V7[1] %||% data$V7[1] %||% NA_character_),
                    stringsAsFactors = FALSE
                )
                out_ctx <- get_neighbor_context_for_target(annotation_file_path, target_gene)
                app_perf_mark(module_perf, sprintf("%s done", as.character(step_prefix)), "ORTHO_MOD")
                app_perf_mark_ms(module_perf, "neighbor_context_ms", app_perf_elapsed_ms(neighbor_t0), "ORTHO_MOD")
                out_ctx
            }

            resolve_gene_info_sync <- function(step_prefix = "prefetch", composition_only = FALSE) {
                df_gene <- processed_cache$df_gene
                df_plot <- processed_cache$df
                df_transcript <- processed_cache$df_transcript

                tx_start <- suppressWarnings(min(as.numeric(df_transcript$V4), na.rm = TRUE))
                tx_end <- suppressWarnings(max(as.numeric(df_transcript$V5), na.rm = TRUE))
                if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                    tx_start <- suppressWarnings(min(df_plot$xstart, na.rm = TRUE))
                    tx_end <- suppressWarnings(max(df_plot$xend, na.rm = TRUE))
                }
                if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                    tx_start <- suppressWarnings(min(df_gene$V4, na.rm = TRUE))
                    tx_end <- suppressWarnings(max(df_gene$V5, na.rm = TRUE))
                }
                if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                    tx_start <- suppressWarnings(min(as.numeric(data$V4), na.rm = TRUE))
                    tx_end <- suppressWarnings(max(as.numeric(data$V5), na.rm = TRUE))
                }

                tx_attr <- if (nrow(df_transcript) > 0) as.character(df_transcript$V9[1] %||% "") else ""
                tx_attrs <- parse_gff_attributes(tx_attr %||% "")
                gene_attr <- if (nrow(df_gene) > 0) as.character(df_gene$V9[1] %||% "") else ""
                tx_label_candidates <- c(
                    tx_attrs[["name"]][1],
                    tx_attrs[["transcript"]][1],
                    tx_attrs[["id"]][1],
                    tx_attrs[["transcript_id"]][1],
                    extract_primary_gene_name(gene_attr),
                    extract_primary_gene_id(gene_attr)
                )
                tx_label_candidates <- as.character(tx_label_candidates %||% character(0))
                tx_label_candidates <- tx_label_candidates[!is.na(tx_label_candidates) & nzchar(trimws(tx_label_candidates))]
                tx_label <- if (length(tx_label_candidates) > 0) tx_label_candidates[1] else "transcript"
                tx_label <- trimws(utils::URLdecode(tx_label))
                if (!nzchar(tx_label)) tx_label <- "transcript"
                tx_strand <- trimws(as.character(df_transcript$V7[1] %||% df_gene$V7[1] %||% data$V7[1] %||% "+"))
                if (!nzchar(tx_strand)) tx_strand <- "+"
                if (!tx_strand %in% c("+", "-")) tx_strand <- "+"

                exon_idx <- which(tolower(as.character(df_plot$feature_type %||% rep("", nrow(df_plot)))) == "exon")
                if (length(exon_idx) == 0) {
                    exon_idx <- which(tolower(as.character(df_plot$feature_type %||% rep("", nrow(df_plot)))) == "cds")
                }
                exon_ranges <- if (length(exon_idx) > 0) {
                    data.frame(
                        start = suppressWarnings(as.numeric(df_plot$xstart[exon_idx])),
                        end = suppressWarnings(as.numeric(df_plot$xend[exon_idx])),
                        stringsAsFactors = FALSE
                    )
                } else {
                    NULL
                }
                chr_for_fetch <- as.character(
                    df_gene$V1[1] %||%
                        df_transcript$V1[1] %||%
                        df_plot$seqid[1] %||%
                        data$V1[1] %||%
                        ""
                )

                if (isTRUE(composition_only)) {
                    comp_info <- NULL
                    if (!is.null(genome_fasta_path) && nzchar(genome_fasta_path) && file.exists(genome_fasta_path) &&
                        !is.null(exon_ranges) && nrow(normalize_exon_ranges(exon_ranges)) > 0L) {
                        comp_info <- tryCatch(
                            get_transcript_composition_cached(genome_fasta_path, chr_for_fetch, exon_ranges, strand = tx_strand),
                            error = function(e) NULL
                        )
                    }
                    comp_blob <- if (!is.null(comp_info)) make_sequence_composition_blob(comp_info) else ""
                    current_genSequences <- isolate(genSequences())
                    current_genSequences[[plotIndex]] <- comp_blob
                    genSequences(current_genSequences)
                    app_perf_mark(module_perf, sprintf("%s composition_only ready=%s", as.character(step_prefix), as.character(nzchar(comp_blob))), "ORTHO_MOD")
                    return(list(sequence = "", file_content = comp_blob, fasta_id = tx_label, composition = comp_info, composition_blob = comp_blob))
                }

                result <- fetch_gene_data_sync(
                    chr_for_fetch,
                    list(start = as.numeric(tx_start), end = as.numeric(tx_end)),
                    fasta_path = genome_fasta_path,
                    fasta_id = tx_label,
                    exon_ranges = exon_ranges,
                    strand = tx_strand
                )
                current_genSequences <- isolate(genSequences())
                current_genSequences[[plotIndex]] <- result$file_content
                genSequences(current_genSequences)
                seq_len <- nchar(as.character(result$sequence %||% ""))
                app_perf_mark(module_perf, sprintf("%s resolved seq_len=%d", as.character(step_prefix), as.integer(seq_len)), "ORTHO_MOD")
                result
            }

            # As in the homologous module, data is static and this prefetch is a
            # one-shot initialization step. Execute it now instead of waiting for
            # a later reactive flush before the card can become complete.
            run_initial_prefetch <- function() {
                req(data)
                app_perf_mark(module_perf, "initial prefetch start", "ORTHO_MOD")
                df_gene <- processed_cache$df_gene
                df_plot <- processed_cache$df
                df_transcript <- processed_cache$df_transcript

                defer_sequence <- isTRUE(is_ortho_sequence_deferred())
                gs_source_ok <- nzchar(as.character(genome_fasta_path %||% "")) && file.exists(genome_fasta_path)
                gs_start <- suppressWarnings(min(df_plot$xstart, na.rm = TRUE))
                gs_end <- suppressWarnings(max(df_plot$xend, na.rm = TRUE))
                gs_seqid <- as.character(df_plot$seqid[1] %||% "")

                if (!is.null(precomputed_neighbor_context)) {
                    neighbor_context(precomputed_neighbor_context)
                    neighbor_context_resolved(TRUE)
                    app_perf_mark(module_perf, "neighbor context precomputed", "ORTHO_MOD")
                } else {
                    neighbor_context(NULL)
                    neighbor_context_resolved(FALSE)
                }

                # Compute all I/O arguments in the main thread (CPU-only, fast)
                tx_start <- suppressWarnings(min(as.numeric(df_transcript$V4), na.rm = TRUE))
                tx_end <- suppressWarnings(max(as.numeric(df_transcript$V5), na.rm = TRUE))
                if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                    tx_start <- suppressWarnings(min(df_plot$xstart, na.rm = TRUE))
                    tx_end <- suppressWarnings(max(df_plot$xend, na.rm = TRUE))
                }
                if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                    tx_start <- suppressWarnings(min(df_gene$V4, na.rm = TRUE))
                    tx_end <- suppressWarnings(max(df_gene$V5, na.rm = TRUE))
                }
                if (!is.finite(tx_start) || !is.finite(tx_end) || tx_end <= tx_start) {
                    tx_start <- suppressWarnings(min(as.numeric(data$V4), na.rm = TRUE))
                    tx_end <- suppressWarnings(max(as.numeric(data$V5), na.rm = TRUE))
                }

                tx_attr <- if (nrow(df_transcript) > 0) as.character(df_transcript$V9[1] %||% "") else ""
                tx_attrs <- parse_gff_attributes(tx_attr %||% "")
                gene_attr <- if (nrow(df_gene) > 0) as.character(df_gene$V9[1] %||% "") else ""
                tx_label_candidates <- c(
                    tx_attrs[["name"]][1],
                    tx_attrs[["transcript"]][1],
                    tx_attrs[["id"]][1],
                    tx_attrs[["transcript_id"]][1],
                    extract_primary_gene_name(gene_attr),
                    extract_primary_gene_id(gene_attr)
                )
                tx_label_candidates <- as.character(tx_label_candidates %||% character(0))
                tx_label_candidates <- tx_label_candidates[!is.na(tx_label_candidates) & nzchar(trimws(tx_label_candidates))]
                tx_label <- if (length(tx_label_candidates) > 0) tx_label_candidates[1] else "transcript"
                tx_label <- trimws(utils::URLdecode(tx_label))
                if (!nzchar(tx_label)) tx_label <- "transcript"
                tx_strand <- trimws(as.character(df_transcript$V7[1] %||% df_gene$V7[1] %||% data$V7[1] %||% "+"))
                if (!nzchar(tx_strand)) tx_strand <- "+"
                if (!tx_strand %in% c("+", "-")) tx_strand <- "+"

                exon_idx <- which(tolower(as.character(df_plot$feature_type %||% rep("", nrow(df_plot)))) == "exon")
                if (length(exon_idx) == 0) {
                    exon_idx <- which(tolower(as.character(df_plot$feature_type %||% rep("", nrow(df_plot)))) == "cds")
                }
                exon_ranges <- if (length(exon_idx) > 0) {
                    data.frame(
                        start = suppressWarnings(as.numeric(df_plot$xstart[exon_idx])),
                        end = suppressWarnings(as.numeric(df_plot$xend[exon_idx])),
                        stringsAsFactors = FALSE
                    )
                } else {
                    NULL
                }
                chr_for_fetch <- as.character(
                    df_gene$V1[1] %||%
                        df_transcript$V1[1] %||%
                        df_plot$seqid[1] %||%
                        data$V1[1] %||%
                        ""
                )

                tgt_chr <- as.character(df_gene$V1[1] %||% df_plot$seqid[1] %||% data$V1[1] %||% NA_character_)
                tgt_start <- suppressWarnings(as.numeric(min(df_gene$V4, na.rm = TRUE)))
                tgt_end <- suppressWarnings(as.numeric(max(df_gene$V5, na.rm = TRUE)))
                if (!is.finite(tgt_start) || !is.finite(tgt_end) || tgt_end < tgt_start) {
                    tgt_start <- suppressWarnings(as.numeric(min(df_plot$xstart, na.rm = TRUE)))
                    tgt_end <- suppressWarnings(as.numeric(max(df_plot$xend, na.rm = TRUE)))
                }
                if (!is.finite(tgt_start) || !is.finite(tgt_end) || tgt_end < tgt_start) {
                    tgt_start <- suppressWarnings(as.numeric(min(data$V4, na.rm = TRUE)))
                    tgt_end <- suppressWarnings(as.numeric(max(data$V5, na.rm = TRUE)))
                }
                tgt_attr <- as.character(df_gene$V9[1] %||% data$V9[1] %||% "")
                target_gene_for_ctx <- data.frame(
                    gene_id = extract_primary_gene_id(tgt_attr),
                    chr = tgt_chr,
                    start = tgt_start,
                    end = tgt_end,
                    strand = as.character(df_gene$V7[1] %||% data$V7[1] %||% NA_character_),
                    stringsAsFactors = FALSE
                )

                # Capture simple-data locals for the future worker
                local_genome <- genome_fasta_path
                local_annotation <- annotation_file_path
                local_gs_seqid <- gs_seqid
                local_gs_start <- gs_start
                local_gs_end <- gs_end
                local_gs_ok <- isTRUE(gs_source_ok)
                local_chr <- chr_for_fetch
                local_tx_coords <- list(start = as.numeric(tx_start), end = as.numeric(tx_end))
                local_tx_label <- tx_label
                local_exon_ranges <- exon_ranges
                local_tx_strand <- tx_strand
                local_target_gene <- target_gene_for_ctx
                local_need_neighbor <- is.null(precomputed_neighbor_context) && isTRUE(prefetch_neighbor_context)
                local_need_sequence <- isTRUE(prefetch_sequence) && !isTRUE(defer_sequence)

                fn_extract_seq <- extract_sequence_from_fasta
                fn_fetch_gene <- fetch_gene_data_sync
                fn_get_neighbor <- get_neighbor_context_for_target

                inline_fast <- should_inline_fast_sequence_prefetch(
                    local_genome,
                    min(c(local_gs_start, local_tx_coords$start), na.rm = TRUE),
                    max(c(local_gs_end, local_tx_coords$end), na.rm = TRUE)
                ) && !isTRUE(local_need_neighbor)

                run_prefetch_payload <- function() {
                    gs_seq <- ""
                    gc_span_fetch_ms <- 0
                    if (isTRUE(local_gs_ok) && is.finite(local_gs_start) && is.finite(local_gs_end) &&
                        local_gs_end > local_gs_start && nzchar(local_gs_seqid)) {
                        gc_span_t0 <- app_perf_now()
                        gs_seq <- tryCatch(
                            fn_extract_seq(local_genome, local_gs_seqid, as.integer(local_gs_start), as.integer(local_gs_end)),
                            error = function(e) ""
                        )
                        gc_span_fetch_ms <- app_perf_elapsed_ms(gc_span_t0)
                    }
                    seq_result <- list(sequence = "", file_content = "", fasta_id = local_tx_label)
                    gene_sequence_fetch_ms <- 0
                    if (isTRUE(local_need_sequence) && !is.null(local_genome) && nzchar(local_genome) && file.exists(local_genome)) {
                        gene_sequence_t0 <- app_perf_now()
                        seq_result <- tryCatch(
                            fn_fetch_gene(local_chr, local_tx_coords,
                                fasta_path = local_genome, fasta_id = local_tx_label,
                                exon_ranges = local_exon_ranges, strand = local_tx_strand),
                            error = function(e) list(sequence = "", file_content = "", fasta_id = local_tx_label)
                        )
                        gene_sequence_fetch_ms <- app_perf_elapsed_ms(gene_sequence_t0)
                    }
                    ctx <- NULL
                    neighbor_prefetch_ms <- 0
                    if (isTRUE(local_need_neighbor) && !is.null(local_annotation) && nzchar(local_annotation) && file.exists(local_annotation)) {
                        neighbor_t0 <- app_perf_now()
                        ctx <- tryCatch(fn_get_neighbor(local_annotation, local_target_gene), error = function(e) NULL)
                        neighbor_prefetch_ms <- app_perf_elapsed_ms(neighbor_t0)
                    }
                    list(
                        gs_seq = gs_seq,
                        seq_result = seq_result,
                        ctx = ctx,
                        gc_span_fetch_ms = gc_span_fetch_ms,
                        gene_sequence_fetch_ms = gene_sequence_fetch_ms,
                        neighbor_prefetch_ms = neighbor_prefetch_ms
                    )
                }
                apply_prefetch_payload <- function(result, source_label = "async") {
                    apply_prefetch_t0 <- app_perf_now()
                    if (isTRUE(module_destroyed)) return(invisible(NULL))
                    app_perf_mark_ms(module_perf, "gc_span_fetch_ms", result$gc_span_fetch_ms %||% 0, "ORTHO_MOD")
                    app_perf_mark_ms(module_perf, "gene_sequence_fetch_ms", result$gene_sequence_fetch_ms %||% 0, "ORTHO_MOD")
                    app_perf_mark_ms(module_perf, "neighbor_prefetch_ms", result$neighbor_prefetch_ms %||% 0, "ORTHO_MOD")
                    span_bp <- if (is.finite(local_gs_start) && is.finite(local_gs_end)) {
                        max(0, as.numeric(local_gs_end) - as.numeric(local_gs_start) + 1)
                    } else {
                        0
                    }
                    app_perf_mark(
                        module_perf,
                        sprintf("%s prefetch source=%s span_bp=%.0f", source_label, tools::file_ext(local_genome %||% ""), span_bp),
                        "ORTHO_MOD"
                    )
                    genomic_span_seq(result$gs_seq %||% "")
                    if (isTRUE(local_need_sequence)) {
                        gene_info(result$seq_result %||% list(sequence = "", file_content = ""))
                        current_genSequences <- isolate(genSequences())
                        current_genSequences[[plotIndex]] <- result$seq_result$file_content %||% ""
                        genSequences(current_genSequences)
                        seq_len <- nchar(as.character(result$seq_result$sequence %||% ""))
                        app_perf_mark(module_perf, sprintf("%s prefetch done seq_len=%d", source_label, as.integer(seq_len)), "ORTHO_MOD")
                    } else {
                        gene_info(NULL)
                    }
                    if (isTRUE(local_need_neighbor)) {
                        neighbor_context(result$ctx)
                        neighbor_context_resolved(TRUE)
                    }
                    app_perf_mark_ms(module_perf, "apply_sequence_prefetch_ms", app_perf_elapsed_ms(apply_prefetch_t0), "ORTHO_MOD")
                    invisible(NULL)
                }
                handle_prefetch_error <- function(err, source_label = "async") {
                    if (isTRUE(module_destroyed)) return(invisible(NULL))
                    app_perf_mark(module_perf, sprintf("%s prefetch error: %s", source_label, as.character(err$message %||% "unknown")), "ORTHO_MOD")
                    gene_info(list(sequence = "", file_content = ""))
                    current_genSequences <- isolate(genSequences())
                    current_genSequences[[plotIndex]] <- ""
                    genSequences(current_genSequences)
                    if (isTRUE(local_need_neighbor)) {
                        neighbor_context(NULL)
                        neighbor_context_resolved(FALSE)
                    }
                    invisible(NULL)
                }

                if (isTRUE(local_gs_ok)) {
                    gc_span_prefetch_started(TRUE)
                }
                if (isTRUE(inline_fast)) {
                    inline_t0 <- app_perf_now()
                    app_perf_mark(module_perf, "inline fast prefetch start", "ORTHO_MOD")
                    tryCatch(
                        apply_prefetch_payload(run_prefetch_payload(), "inline fast"),
                        error = function(err) handle_prefetch_error(err, "inline fast")
                    )
                    app_perf_mark_ms(
                        module_perf,
                        "inline_fast_prefetch_ms",
                        app_perf_elapsed_ms(inline_t0),
                        "ORTHO_MOD"
                    )
                } else {
                    app_perf_mark(module_perf, "async prefetch launch", "ORTHO_MOD")
                    promises::future_promise(
                        run_prefetch_payload(),
                        seed = FALSE
                    ) %...>% (function(result) {
                        apply_prefetch_payload(result, "async")
                    }) %...!% (function(err) {
                        handle_prefetch_error(err, "async")
                    })
                }
                invisible(NULL)
            }
            run_initial_prefetch()

            render_trigger <- reactive({
                # With lazy sequence mode, avoid tracking gene_info as a reactive dependency
                # so the internal gene_info(info) assignment does not trigger a second rerender.
                defer_sequence <- isTRUE(is_ortho_sequence_deferred())
                if (isTRUE(prefetch_sequence) && !isTRUE(defer_sequence)) {
                    info_evt <- gene_info()
                    if (is.null(info_evt)) {
                        return(NULL)
                    }
                } else {
                    info_evt <- isolate(gene_info())
                }
                visual_mode_evt <- "compact"
                if (!is.null(visual_mode)) {
                    visual_mode_evt <- tryCatch(as.character(visual_mode())[1], error = function(e) "compact")
                }
                visual_mode_evt <- tolower(visual_mode_evt %||% "compact")
                if (!visual_mode_evt %in% c("compact", "detailed")) visual_mode_evt <- "compact"
                orientation_mode_evt <- "genomic"
                if (!is.null(orientation_mode)) {
                    orientation_mode_evt <- tryCatch(as.character(orientation_mode())[1], error = function(e) "genomic")
                }
                orientation_mode_evt <- normalize_gene_plot_orientation_mode(orientation_mode_evt)
                theme_mode_evt <- "light"
                if (!is.null(app_theme)) {
                    theme_mode_evt <- tryCatch(as.character(app_theme())[1], error = function(e) "light")
                }
                theme_mode_evt <- tolower(theme_mode_evt %||% "light")
                if (!theme_mode_evt %in% c("light", "dark")) theme_mode_evt <- "light"
                colorblind_evt <- FALSE
                if (!is.null(app_colorblind)) {
                    colorblind_evt <- tryCatch(isTRUE(app_colorblind()), error = function(e) FALSE)
                }
                seq_len_evt <- 0L
                if (!is.null(info_evt) && !is.null(info_evt$sequence)) {
                    seq_len_evt <- suppressWarnings(as.integer(nchar(as.character(info_evt$sequence %||% ""))))
                    if (!is.finite(seq_len_evt)) seq_len_evt <- 0L
                }
                max_gene_length_evt <- suppressWarnings(as.numeric(max_gene_length() %||% 0))
                if (!is.finite(max_gene_length_evt) || is.na(max_gene_length_evt) || max_gene_length_evt < 0) {
                    max_gene_length_evt <- 0
                }
                neighbor_evt <- if (isTRUE(prefetch_neighbor_context)) {
                    !is.null(neighbor_context())
                } else {
                    !is.null(isolate(neighbor_context()))
                }
                gc_span_len_evt <- if (isTRUE(is_feature_gc_deferred())) {
                    suppressWarnings(as.integer(nchar(as.character(genomic_span_seq() %||% ""))))
                } else {
                    0L
                }
                if (!is.finite(gc_span_len_evt)) gc_span_len_evt <- 0L
                list(
                    visual_mode_evt,
                    orientation_mode_evt,
                    theme_mode_evt,
                    colorblind_evt,
                    as.logical(neighbor_evt),
                    seq_len_evt,
                    as.integer(gc_span_len_evt),
                    as.integer(round(max_gene_length_evt)),
                    render_nonce()
                )
            })

            output$plot <- bindEvent(renderGirafe({
                render_prepare_t0 <- app_perf_now()
                on.exit(notify_plot_ready(), add = TRUE)
                req(data)
                info <- gene_info()
                if (!isTRUE(prefetch_neighbor_context) && !isTRUE(neighbor_context_resolved()) && is.null(precomputed_neighbor_context)) {
                    lazy_ctx <- tryCatch(
                        resolve_neighbor_context_sync("neighbor context lazy"),
                        error = function(e) {
                            app_perf_mark(module_perf, sprintf("neighbor context lazy error: %s", as.character(e$message %||% "unknown")), "ORTHO_MOD")
                            NULL
                        }
                    )
                    neighbor_context(lazy_ctx)
                    neighbor_context_resolved(TRUE)
                }
                defer_sequence <- isTRUE(is_ortho_sequence_deferred())
                if (!isTRUE(prefetch_sequence) && !isTRUE(defer_sequence) && is.null(info)) {
                    app_perf_mark(module_perf, "lazy sequence fetch start", "ORTHO_MOD")
                    info <- tryCatch(
                        resolve_gene_info_sync("lazy_fetch"),
                        error = function(e) {
                            app_perf_mark(module_perf, sprintf("lazy fetch error: %s", as.character(e$message %||% "unknown")), "ORTHO_MOD")
                            list(sequence = "", file_content = "")
                        }
                    )
                    gene_info(info)
                }
                current_neighbor_context <- if (isTRUE(prefetch_neighbor_context)) neighbor_context() else isolate(neighbor_context())
                app_perf_mark(module_perf, sprintf("render start info_ready=%s", ifelse(is.null(info), "FALSE", "TRUE")), "ORTHO_MOD")
                if (isTRUE(prefetch_sequence) && !isTRUE(defer_sequence)) {
                    req(!is.null(info))
                }
                width_svg <- 16.7
                
                # Detectamos el modo visual AHORA para darle la altura exacta al SVG
                this_visual_mode <- "compact"
                if (!is.null(visual_mode)) {
                    candidate_mode <- tryCatch(as.character(visual_mode())[1], error = function(e) NA_character_)
                    if (tolower(candidate_mode %||% "compact") %in% c("compact", "detailed")) {
                        this_visual_mode <- tolower(candidate_mode)
                    }
                }
                this_orientation_mode <- "genomic"
                if (!is.null(orientation_mode)) {
                    this_orientation_mode <- tryCatch(as.character(orientation_mode())[1], error = function(e) "genomic")
                }
                this_orientation_mode <- normalize_gene_plot_orientation_mode(this_orientation_mode)

                # Si es compacta, el lienzo es delgado. Si es detallada, crece.
                height_svg <- if (this_visual_mode == "compact") 1.15 else 1.85
                # Reuse preprocessed structures computed at module init.
                df <- processed_cache$df
                df_gene <- processed_cache$df_gene
                df_transcript <- processed_cache$df_transcript

                # Kept for signature compatibility; create_gene_plot does not
                # consume this argument. The footer uses prepared composition.
                composicion_secuencia <- NULL

                transcript_start <- suppressWarnings(min(df$xstart, na.rm = TRUE))
                transcript_end <- suppressWarnings(max(df$xend, na.rm = TRUE))
                if (!is.finite(transcript_start) || !is.finite(transcript_end) || transcript_end <= transcript_start) {
                    transcript_start <- suppressWarnings(min(df_gene$V4, na.rm = TRUE))
                    transcript_end <- suppressWarnings(max(df_gene$V5, na.rm = TRUE))
                }
                current_transcript_length <- as.numeric(transcript_end - transcript_start + 1)
                if (!is.finite(current_transcript_length) || current_transcript_length <= 0) current_transcript_length <- 1
                current_max_gene_length <- isolate(suppressWarnings(as.numeric(max_gene_length() %||% current_transcript_length)))
                if (!is.finite(current_max_gene_length) || current_max_gene_length <= 0) {
                    current_max_gene_length <- current_transcript_length
                }
                length_difference <- max(0, current_max_gene_length - current_transcript_length)

                gene_length_bp <- if (nrow(df_gene) > 0) as.numeric(max(df_gene$V5) - min(df_gene$V4) + 1) else current_transcript_length
                gene_length_label <- sprintf("Gene Length: %s pb", format(as.integer(round(gene_length_bp)), big.mark = ","))
                transcript_length_label <- sprintf("Transcript Length: %s pb", format(as.integer(round(current_transcript_length)), big.mark = ","))

                this_visual_mode <- "compact"
                if (!is.null(visual_mode)) {
                    candidate_mode <- tryCatch(as.character(visual_mode())[1], error = function(e) NA_character_)
                    candidate_mode <- tolower(candidate_mode %||% "compact")
                    if (candidate_mode %in% c("compact", "detailed")) this_visual_mode <- candidate_mode
                }
                theme_mode <- "light"
                if (!is.null(app_theme)) {
                    theme_mode <- tryCatch(as.character(app_theme())[1], error = function(e) "light")
                }
                theme_mode <- tolower(theme_mode %||% "light")
                is_dark_theme <- identical(theme_mode, "dark")

                is_colorblind_mode <- FALSE
                if (!is.null(app_colorblind)) {
                    is_colorblind_mode <- tryCatch(isTRUE(app_colorblind()), error = function(e) FALSE)
                }

                cache_enabled <- isTRUE(is_ortho_plot_cache_enabled())
                max_gene_length_key <- current_max_gene_length
                if (!is.finite(max_gene_length_key)) max_gene_length_key <- 0
                seq_len_key <- 0L
                if (!is.null(info) && !is.null(info$sequence)) {
                    seq_len_key <- suppressWarnings(as.integer(nchar(as.character(info$sequence %||% ""))))
                    if (!is.finite(seq_len_key)) seq_len_key <- 0L
                }
                has_neighbor_context <- !is.null(current_neighbor_context)
                gc_span_len_key <- suppressWarnings(as.integer(nchar(as.character(genomic_span_seq() %||% ""))))
                if (!is.finite(gc_span_len_key)) gc_span_len_key <- 0L
                defer_feature_gc <- isTRUE(is_feature_gc_deferred())
                compact_feature_key <- paste(
                    if (identical(this_visual_mode, "compact")) is_compact_feature_interactivity_enabled() else TRUE,
                    if (isTRUE(defer_feature_gc)) gc_span_len_key else "eager",
                    sep = ":gc="
                )
                cache_key <- make_girafe_plot_cache_key(
                    "orthologous",
                    plot_signature = plot_signature,
                    fallback_id = plotIndex,
                    max_gene_length_key = max_gene_length_key,
                    visual_mode = this_visual_mode,
                    theme_mode = theme_mode,
                    is_colorblind_mode = is_colorblind_mode,
                    seq_len_key = seq_len_key,
                    has_neighbor_context = has_neighbor_context,
                    compact_feature_interactivity = compact_feature_key,
                    orientation_mode = this_orientation_mode
                )

                if (cache_enabled) {
                    if (identical(cached_plot_key(), cache_key)) {
                        cached_plot <- cached_plot_obj()
                        if (!is.null(cached_plot)) {
                            app_perf_mark(module_perf, "render cache hit", "ORTHO_MOD")
                            return(refresh_girafe_widget_uid(cached_plot))
                        }
                    }
                    shared_plot <- get_shared_girafe_plot_cache(cache_key)
                    if (!is.null(shared_plot)) {
                        app_perf_mark(module_perf, "render shared cache hit", "ORTHO_MOD")
                        cached_plot_key(cache_key)
                        cached_plot_obj(shared_plot)
                        return(refresh_girafe_widget_uid(shared_plot))
                    }
                    app_perf_mark(module_perf, "render cache miss", "ORTHO_MOD")
                } else {
                    app_perf_mark(module_perf, "render cache disabled", "ORTHO_MOD")
                    if (!identical(cached_plot_key(), "") || !is.null(cached_plot_obj())) {
                        cached_plot_key("")
                        cached_plot_obj(NULL)
                    }
                }

                gc_timing_enabled <- isTRUE(app_perf_enabled())
                gc_before <- if (gc_timing_enabled) sum(gc.time(on = TRUE)) else NA_real_
                create_t0 <- app_perf_now()
                app_perf_mark_ms(module_perf, "render_prepare_ms", app_perf_elapsed_ms(render_prepare_t0), "ORTHO_MOD")
                app_perf_mark(module_perf, "create_gene_plot start", "ORTHO_MOD")
                span_for_plot <- genomic_span_seq()
                genome_for_plot <- if (isTRUE(defer_feature_gc) && !nzchar(trimws(as.character(span_for_plot %||% "")))) {
                    ""
                } else {
                    genome_fasta_path
                }
                plot_obj <- create_gene_plot(
                    df, df_gene, df_transcript, current_transcript_length, length_difference,
                    composicion_secuencia, gene_length_label, transcript_length_label,
                    neighbor_context = current_neighbor_context,
                    visual_mode = this_visual_mode,
                    width_svg = width_svg,
                    height_svg = height_svg,
                    organism_label = organism_name,
                    annotation_file_path = annotation_file_path,
                    use_report_map = use_report_map,
                    report_path = report_path,
                    plot_id = as.character(plotIndex),
                    plot_context = "orthologous",
                    genome_fasta_path = genome_for_plot,
                    is_dark_theme = is_dark_theme,
                    is_colorblind_mode = is_colorblind_mode,
                    gene_display_name = gene_name,
                    precomputed_genomic_span = span_for_plot,
                    model_cache_key = model_data_key,
                    orientation_mode = this_orientation_mode,
                    caller_started_at = create_t0
                )
                if (gc_timing_enabled) {
                    gc_after <- sum(gc.time())
                    app_perf_mark_ms(
                        module_perf,
                        "create_gene_plot_gc_ms",
                        1000 * max(0, gc_after - gc_before),
                        "ORTHO_MOD"
                    )
                }
                if (isTRUE(defer_feature_gc) && !nzchar(trimws(as.character(span_for_plot %||% "")))) {
                    schedule_gc_span_prefetch(df)
                }
                if (isTRUE(defer_sequence) && is.null(info)) {
                    schedule_deferred_sequence_prefetch()
                }
                app_perf_mark(module_perf, "create_gene_plot done", "ORTHO_MOD")
                app_perf_mark_ms(module_perf, "create_gene_plot_ms", app_perf_elapsed_ms(create_t0), "ORTHO_MOD")
                if (cache_enabled) {
                    cached_plot_key(cache_key)
                    cached_plot_obj(plot_obj)
                    set_shared_girafe_plot_cache(cache_key, plot_obj)
                }
                plot_obj
            }), render_trigger(), ignoreNULL = TRUE)
            default_suspend_when_hidden <- isTRUE(is_ortho_suspend_hidden_enabled())
            # Cross-species modules are also instantiated progressively, one
            # visible card/batch at a time. Do not add a second client-binding
            # gate after that server-side admission control.
            outputOptions(output, "plot", suspendWhenHidden = FALSE)
            app_perf_mark(module_perf, "plot output registered eager", "ORTHO_MOD")
            nudge_render_nonce <- function(reason = "manually") {
                if (isTRUE(module_destroyed)) {
                    return(invisible(FALSE))
                }
                current_nonce <- isolate(render_nonce())
                if (!is.finite(suppressWarnings(as.numeric(current_nonce)))) {
                    current_nonce <- 0L
                }
                render_nonce(as.integer(current_nonce) + 1L)
                app_perf_mark(module_perf, sprintf("render nonce nudged %s", reason), "ORTHO_MOD")
                invisible(TRUE)
            }
            if (isTRUE(is_ortho_server_render_nudge_enabled())) {
                nudge_after_flush <- function() {
                    nudge_render_nonce("after flush")
                    invisible(NULL)
                }
                tryCatch({
                    if (!is.null(session) && is.function(session$onFlushed)) {
                        session$onFlushed(nudge_after_flush, once = TRUE)
                    } else {
                        nudge_after_flush()
                    }
                }, error = function(e) {
                    app_perf_mark(module_perf, sprintf("render nonce nudge error: %s", as.character(e$message %||% "unknown")), "ORTHO_MOD")
                })
            } else {
                app_perf_mark(module_perf, "server render nonce nudge disabled", "ORTHO_MOD")
            }
            list(
                destroy = destroy_module,
                is_destroyed = function() {
                    isTRUE(module_destroyed)
                },
                nudge_render = function() {
                    nudge_render_nonce("manually")
                },
                set_background_render = function(enabled = TRUE) {
                    if (isTRUE(module_destroyed)) {
                        return(invisible(FALSE))
                    }
                    enabled <- isTRUE(enabled)
                    outputOptions(
                        output,
                        "plot",
                        suspendWhenHidden = if (enabled) FALSE else default_suspend_when_hidden
                    )
                    if (enabled) {
                        nudge_render_nonce("for Figure Studio")
                    }
                    invisible(TRUE)
                }
            )
        }
    )
}
