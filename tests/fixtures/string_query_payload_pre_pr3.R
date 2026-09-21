# Frozen unchanged reference from server.R at 3a27a5e77409faf7dc4abfe1de5b21d3f3e05c1d.
    build_string_query_payload <- function(pid, ctx) {
        d <- get_chart_plot_data(pid, ctx)
        if (is.null(d$file_data) || nrow(d$file_data) == 0) {
            return(list(error_widget = build_string_error_widget("Plot data not available", "This plot is no longer available. Generate it again and retry.")))
        }

        is_invalid_str <- function(x) {
            length(x) == 0 || is.na(x) || !nzchar(x) || x == "Gene"
        }
        is_invalid_num <- function(x) {
            length(x) == 0 || is.na(x) || !is.finite(x) || x <= 0
        }

        titulo <- if (identical(ctx, "homo")) titlesHomologous()[[pid]] else titlesOrthologous()[[pid]]
        ann_path <- if (identical(ctx, "homo")) annotationPathsHomologous()[[pid]] else annotationPathsOrthologous()[[pid]]
        gene_title <- trimws(as.character(extract_title_field(titulo, "Gene") %||% "Gene"))
        org_name <- as.character(d$org_info$name %||% "Organism")

        types <- tolower(trimws(as.character(d$file_data$V3)))
        gene_rows <- which(types == "gene")
        gene_attr <- if (length(gene_rows) > 0) as.character(d$file_data$V9[gene_rows[1]]) else ""
        gene_name <- tryCatch(extract_primary_gene_name(gene_attr), error = function(e) "")
        if (is_invalid_str(gene_name)) {
            gene_name <- gene_title
        }
        if (is_invalid_str(gene_name)) {
            return(list(
                org_name = org_name,
                gene_title = gene_title,
                error_widget = build_string_error_widget("No gene name", "Could not resolve a stable gene name for STRING query.", font_size = 16)
            ))
        }

        taxid <- suppressWarnings(as.numeric(d$org_info$taxid))
        if (is_invalid_num(taxid)) {
            taxid <- suppressWarnings(as.numeric(resolve_taxid_for_go_lookup(ann_path, d$org_info$name)))
        }
        if (is_invalid_num(taxid)) {
            return(list(
                org_name = org_name,
                gene_title = gene_title,
                error_widget = build_string_error_widget(
                    "TaxID not found",
                    "Organism TaxID not found. STRING requires a valid TaxID.",
                    icon_code = "f06a",
                    icon_color = "#C0392B",
                    font_size = 16
                )
            ))
        }

        plot_taxid_for_string <- function(plot_id, ctx_name, plot_d = NULL) {
            plot_id <- as.character(plot_id %||% "")
            if (is.null(plot_d)) {
                plot_d <- get_chart_plot_data(plot_id, ctx_name)
            }
            ann_path_i <- tryCatch(
                if (identical(ctx_name, "homo")) annotationPathsHomologous()[[plot_id]] else annotationPathsOrthologous()[[plot_id]],
                error = function(e) ""
            )
            org_info_i <- plot_d$org_info %||% list()
            tx <- suppressWarnings(as.numeric(org_info_i$taxid %||% NA_real_))
            if (is_invalid_num(tx)) {
                tx <- suppressWarnings(as.numeric(resolve_taxid_for_go_lookup(ann_path_i, org_info_i$name %||% "")))
            }
            tx
        }

        extract_gff_ids_for_plot <- function(plot_id, ctx_name, plot_d = NULL) {
            ids <- c()
            tryCatch({
                if (is.null(plot_d)) {
                    plot_d <- get_chart_plot_data(as.character(plot_id), ctx_name)
                }
                if (!is.null(plot_d$file_data) && nrow(plot_d$file_data) > 0) {
                    for (row_attr in as.character(plot_d$file_data$V9)) {
                        pa <- tryCatch(parse_gff_attributes(row_attr), error = function(e) list())
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
                }
            }, error = function(e) NULL)
            unique(ids[nzchar(ids)])
        }

        collect_screen_records <- function(ctx_name, ids, titles_map) {
            records <- list()
            for (i_id in ids %||% integer(0)) {
                id_chr <- as.character(i_id)
                plot_d <- tryCatch(get_chart_plot_data(id_chr, ctx_name), error = function(e) NULL)
                if (is.null(plot_d) || is.null(plot_d$file_data) || nrow(plot_d$file_data) == 0L) next
                t_str <- tryCatch(titles_map[[id_chr]], error = function(e) "")
                g_name <- trimws(as.character(extract_title_field(t_str, "Gene") %||% ""))
                candidates <- c()
                if (!is_invalid_str(g_name)) candidates <- c(candidates, g_name)
                candidates <- c(candidates, extract_gff_ids_for_plot(id_chr, ctx_name, plot_d = plot_d))
                candidates <- unique(trimws(candidates))
                candidates <- candidates[nzchar(candidates) & !is.na(candidates)]
                if (length(candidates) == 0L) next
                records[[length(records) + 1L]] <- list(
                    taxid = plot_taxid_for_string(id_chr, ctx_name, plot_d = plot_d),
                    candidates = candidates
                )
            }
            records
        }

        screen_records <- c(
            tryCatch(collect_screen_records("homo", activePlotIdsHomologous(), titlesHomologous() %||% list()), error = function(e) list()),
            tryCatch(collect_screen_records("ortho", activePlotIdsOrthologous(), titlesOrthologous() %||% list()), error = function(e) list())
        )
        screen_variants <- tryCatch(
            string_collect_same_taxid_candidates(screen_records, taxid),
            error = function(e) character(0)
        )

        id_candidates <- unique(c(
            gene_name,
            gsub(";", ".", gene_name, fixed = TRUE),
            gsub(";", "-", gene_name, fixed = TRUE),
            gsub(";", "", gene_name, fixed = TRUE)
        ))
        tryCatch({
            all_attrs_raw <- as.character(d$file_data$V9)
            for (row_i in seq_along(all_attrs_raw)) {
                parsed <- tryCatch(parse_gff_attributes(all_attrs_raw[row_i]), error = function(e) list())
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
        gene_id_fallback <- tryCatch(extract_primary_gene_id(gene_attr), error = function(e) "")
        if (!is_invalid_str(gene_id_fallback)) {
            id_candidates <- c(id_candidates, gene_id_fallback)
        }
        id_candidates <- unique(trimws(id_candidates))
        id_candidates <- id_candidates[nzchar(id_candidates) & !is.na(id_candidates) & id_candidates != "Gene"]
        if (length(id_candidates) > 64L) {
            id_candidates <- id_candidates[seq_len(64L)]
        }

        list(
            org_name = org_name,
            gene_title = gene_title,
            gene_name = gene_name,
            taxid = as.numeric(taxid),
            id_candidates = id_candidates,
            screen_variants = screen_variants,
            required_score = 600L,
            add_nodes = 8L,
            is_dark = identical(tolower(as.character(input$app_theme %||% "light")), "dark")
        )
    }
