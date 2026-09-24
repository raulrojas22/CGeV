init_plot_lifecycle_domain <- function(
    input,
    output,
    session,
    preferredSearchWorkflow_fn,
    preloadedRegistry_rv,
    searchStatusHomologous_rv,
    searchStatusOrthologous_rv,
    activePlotIdsHomologous_rv,
    titlesHomologous_rv,
    existingPlotsHomologous_rv,
    fileDataHomologous_rv,
    chrNamesHomologous_rv,
    genSequencesHomologous_rv,
    plotSignaturesHomologous_rv,
    annotationPathsHomologous_rv,
    genomePathsHomologous_rv,
    organismInfoHomologous_rv,
    plotMetricsHomologous_rv,
    plotGeneMetaHomologous_rv,
    closeObserversBoundHomologous_rv,
    activePlotIdsOrthologous_rv,
    titlesOrthologous_rv,
    existingPlotsOrthologous_rv,
    fileDataOrthologous_rv,
    chrNamesOrthologous_rv,
    genSequencesOrthologous_rv,
    plotSignaturesOrthologous_rv,
    annotationPathsOrthologous_rv,
    genomePathsOrthologous_rv,
    organismInfoOrthologous_rv,
    plotMetricsOrthologous_rv,
    plotGeneMetaOrthologous_rv,
    closeObserversBoundOrthologous_rv,
    homoRenderedPlotIds_rv,
    homoInsertedCardIds_rv,
    homoHydratedIsoformIds_rv = NULL,
    homoFooterOutputsBound_rv,
    homoDownloadOutputsBound_rv,
    homoPlotTimingTracker_rv,
    orthoRenderedPlotIds_rv,
    orthoInsertedCardIds_rv,
    orthoFooterOutputsBound_rv,
    orthoDownloadOutputsBound_rv,
    orthoPlotTimingTracker_rv,
    homoVisibleCount_rv,
    homoInitialVisibleCount,
    orthoVisibleCount_rv,
    orthoInitialVisibleCount,
    max_gene_length_homo_rv,
    min_gene_coord_homo_rv,
    max_gene_coord_homo_rv,
    max_gene_length_ortho_rv,
    min_gene_coord_ortho_rv,
    max_gene_coord_ortho_rv,
    orthoAlignedRenderCache_rv,
    orthoAlignedTrackCache_env,
    orthoAlignedSceneCache_env,
    orthoAlignedPlotCache_env,
    orthoAlignedSeqCache_env,
    orthoAlignedGcCache_env,
    orthoHomologyCache_env,
    clear_summary_cache_scope_fn,
    clear_analytics_cache_scope_fn,
    empty_plot_timing_tracker_fn,
    drop_plot_timing_id_fn,
    mark_plot_ready_timing_fn,
    append_status_fn,
    emit_popup_status_fn,
    compute_transcript_span_fn = NULL,
    extract_title_field_fn = NULL,
    sanitize_fasta_filename_token_fn = NULL,
    build_sequence_blob_from_plot_fn = NULL,
    build_transcript_fasta_content_fn = NULL,
    build_gene_sequence_content_fn = NULL,
    build_selected_sequence_fasta_content_fn = NULL
) {
    set_named_value <- function(rv, key, value) {
        x <- rv()
        x[[as.character(key)]] <- value
        rv(x)
    }

    remove_named_value <- function(rv, key) {
        x <- rv()
        x[[as.character(key)]] <- NULL
        rv(x)
    }

    clear_dynamic_output_binding <- function(output_id) {
        output_id <- as.character(output_id %||% "")
        if (!nzchar(output_id)) {
            return(invisible(NULL))
        }
        tryCatch({
            output[[output_id]] <- NULL
        }, error = function(e) NULL)
        invisible(NULL)
    }

    selected_sequence_download_type <- function(input_prefix, id_chr, default_type = "transcript") {
        input_id <- paste0(as.character(input_prefix %||% ""), "_type_", as.character(id_chr %||% ""))
        raw <- tryCatch(input[[input_id]], error = function(e) NULL)
        if (exists("normalize_sequence_download_type", mode = "function")) {
            normalize_sequence_download_type(raw, default = default_type)
        } else {
            typ <- tolower(trimws(as.character(raw %||% default_type)))
            if (!typ %in% c("gene", "transcript", "cds", "cds_segments", "introns")) default_type else typ
        }
    }

    sequence_download_filename <- function(base_name, selected_type, default_type, fallback = "sequence") {
        stem <- if (is.function(sanitize_fasta_filename_token_fn)) {
            sanitize_fasta_filename_token_fn(base_name, fallback = fallback)
        } else {
            gsub("[^A-Za-z0-9._-]+", "_", as.character(base_name %||% fallback))
        }
        if (!nzchar(stem)) stem <- fallback
        suffix <- if (identical(selected_type, default_type)) "" else paste0("_", selected_type)
        paste0(stem, suffix, ".fasta")
    }

    remove_plot_card_dom <- function(panel = c("homologous", "orthologous"), id_chr = "") {
        panel <- match.arg(panel)
        id_chr <- as.character(id_chr %||% "")
        if (!nzchar(id_chr)) {
            return(invisible(NULL))
        }
        # Remove both the transcript card and the gene summary card (multi-tx case).
        # gene summary card id: homo-gene-card-{id} / ortho-gene-card-{id}
        card_prefix      <- if (identical(panel, "homologous")) "homo-card-"      else "ortho-card-"
        gene_card_prefix <- if (identical(panel, "homologous")) "homo-gene-card-" else "ortho-gene-card-"
        shinyjs::runjs(sprintf(
            paste0(
                "['%s%s','%s%s'].forEach(function(eid){",
                " var node=document.getElementById(eid);",
                " if(node && node.parentNode){",
                "  if(window.Shiny&&Shiny.unbindAll){Shiny.unbindAll(node);}",
                "  node.parentNode.removeChild(node);",
                " }",
                "});"
            ),
            card_prefix, id_chr,
            gene_card_prefix, id_chr
        ))
        invisible(NULL)
    }

    destroy_plot_module_handle <- function(handle) {
        if (is.null(handle)) {
            return(invisible(FALSE))
        }
        if (is.list(handle) && is.function(handle$destroy)) {
            tryCatch(handle$destroy(), error = function(e) NULL)
            return(invisible(TRUE))
        }
        invisible(FALSE)
    }

    resolve_compute_transcript_span <- function(block_data) {
        if (is.function(compute_transcript_span_fn)) {
            return(compute_transcript_span_fn(block_data))
        }
        if (is.null(block_data) || nrow(block_data) == 0) {
            return(c(start = NA_real_, end = NA_real_))
        }
        row_types <- tolower(trimws(as.character(block_data$V3 %||% rep("", nrow(block_data)))))
        tx_level_types <- c(
            "mrna", "transcript", "lnc_rna", "trna", "rrna", "snorna", "snrna", "mirna",
            "ncrna", "primary_transcript", "pre_mirna", "guide_rna", "rnase_p_rna",
            "rnase_mrp_rna", "telomerase_rna", "antisense_rna", "srp_rna", "scarna",
            "vault_rna", "y_rna", "antisense_lncrna", "lncrna"
        )
        tx_idx <- which(row_types %in% tx_level_types)
        if (length(tx_idx) > 0) {
            source_df <- block_data[tx_idx, , drop = FALSE]
        } else {
            feat_idx <- which(row_types %in% c("exon", "cds", "start_codon", "stop_codon") | grepl("utr", row_types))
            if (length(feat_idx) == 0) feat_idx <- which(row_types == "gene")
            source_df <- if (length(feat_idx) > 0) block_data[feat_idx, , drop = FALSE] else block_data
        }
        starts <- suppressWarnings(as.numeric(source_df$V4))
        ends <- suppressWarnings(as.numeric(source_df$V5))
        start_val <- suppressWarnings(min(starts, na.rm = TRUE))
        end_val <- suppressWarnings(max(ends, na.rm = TRUE))
        if (!is.finite(start_val) || !is.finite(end_val)) {
            return(c(start = NA_real_, end = NA_real_))
        }
        c(start = start_val, end = end_val)
    }

    compute_plot_ranges <- function(data_list, plot_ids = NULL) {
        if (!is.null(plot_ids)) {
            active_keys <- unique(as.character(plot_ids %||% character(0)))
            active_keys <- active_keys[nzchar(active_keys)]
            data_keys <- names(data_list %||% list())
            keep_keys <- intersect(active_keys, as.character(data_keys %||% character(0)))
            data_list <- if (length(keep_keys) > 0L) {
                data_list[keep_keys]
            } else {
                list()
            }
        }
        if (length(data_list) == 0) {
            return(list(max_len = 0, min_coord = Inf, max_coord = -Inf))
        }
        lengths <- numeric(0)
        mins <- numeric(0)
        maxs <- numeric(0)
        for (df in data_list) {
            if (is.null(df) || nrow(df) == 0) next
            tx_span <- resolve_compute_transcript_span(df)
            span_start <- as.numeric(tx_span[["start"]])
            span_end <- as.numeric(tx_span[["end"]])
            if (!is.finite(span_start) || !is.finite(span_end)) next
            mins <- c(mins, span_start)
            maxs <- c(maxs, span_end)
            lengths <- c(lengths, span_end - span_start + 1)
        }
        if (length(lengths) == 0) {
            return(list(max_len = 0, min_coord = Inf, max_coord = -Inf))
        }
        list(
            max_len = max(lengths),
            min_coord = min(mins),
            max_coord = max(maxs)
        )
    }

    update_numeric_reactive <- function(rv, value) {
        old <- suppressWarnings(as.numeric(isolate(rv()) %||% NA_real_))
        new <- suppressWarnings(as.numeric(value %||% NA_real_))
        same <- length(old) == 1L && length(new) == 1L &&
            ((is.na(old) && is.na(new)) || identical(old, new))
        if (!isTRUE(same)) {
            rv(new)
            return(invisible(TRUE))
        }
        invisible(FALSE)
    }

    recalc_plot_ranges_homologous <- function() {
        ranges <- compute_plot_ranges(
            fileDataHomologous_rv(),
            plot_ids = activePlotIdsHomologous_rv()
        )
        update_numeric_reactive(max_gene_length_homo_rv, ranges$max_len)
        update_numeric_reactive(min_gene_coord_homo_rv, ranges$min_coord)
        update_numeric_reactive(max_gene_coord_homo_rv, ranges$max_coord)
        invisible(NULL)
    }

    recalc_plot_ranges_orthologous <- function() {
        ranges <- compute_plot_ranges(
            fileDataOrthologous_rv(),
            plot_ids = activePlotIdsOrthologous_rv()
        )
        update_numeric_reactive(max_gene_length_ortho_rv, ranges$max_len)
        update_numeric_reactive(min_gene_coord_ortho_rv, ranges$min_coord)
        update_numeric_reactive(max_gene_coord_ortho_rv, ranges$max_coord)
        invisible(NULL)
    }

    unbind_homologous_footer_output <- function(id_chr) {
        id_chr <- as.character(id_chr %||% "")
        if (!nzchar(id_chr)) {
            return(invisible(NULL))
        }
        clear_dynamic_output_binding(paste0("homo_footer_", id_chr))
        bound_now <- as.character(homoFooterOutputsBound_rv() %||% character(0))
        bound_keep <- setdiff(bound_now, id_chr)
        if (!identical(bound_now, bound_keep)) {
            homoFooterOutputsBound_rv(bound_keep)
        }
        invisible(NULL)
    }

    unbind_orthologous_footer_output <- function(id_chr) {
        id_chr <- as.character(id_chr %||% "")
        if (!nzchar(id_chr)) {
            return(invisible(NULL))
        }
        clear_dynamic_output_binding(paste0("ortho_footer_", id_chr))
        bound_now <- as.character(orthoFooterOutputsBound_rv() %||% character(0))
        bound_keep <- setdiff(bound_now, id_chr)
        if (!identical(bound_now, bound_keep)) {
            orthoFooterOutputsBound_rv(bound_keep)
        }
        invisible(NULL)
    }

    register_homologous_download_output <- function(id_chr) {
        id_chr <- as.character(id_chr %||% "")
        if (!nzchar(id_chr)) {
            return(invisible(NULL))
        }
        for (sequence_type in c("gene", "transcript", "cds", "cds_segments", "introns")) local({
            id_local <- id_chr
            selected_type_local <- sequence_type
            output[[paste0("download_homo_", id_local, "_", selected_type_local)]] <- downloadHandler(
                filename = function() {
                    gene_meta <- tryCatch(plotGeneMetaHomologous_rv()[[id_local]], error = function(e) NULL)
                    is_gene   <- isTRUE(gene_meta$is_canonical)
                    default_type <- if (is_gene) "gene" else "transcript"
                    selected_type <- selected_type_local
                    if (is_gene) {
                        gene_name <- as.character(gene_meta$display_gene_name %||% gene_meta$matched_gene_name %||% "gene")
                        sequence_download_filename(gene_name, selected_type, default_type, fallback = paste0("gene_", id_local))
                    } else {
                        ttl    <- tryCatch(titlesHomologous_rv()[[id_local]], error = function(e) "")
                        tx_name <- if (is.function(extract_title_field_fn)) extract_title_field_fn(ttl, "Transcript") else ""
                        sequence_download_filename(tx_name, selected_type, default_type, fallback = paste0("transcript_", id_local))
                    }
                },
                content = function(file) {
                    gene_meta <- tryCatch(plotGeneMetaHomologous_rv()[[id_local]], error = function(e) NULL)
                    is_gene   <- isTRUE(gene_meta$is_canonical)
                    default_type <- if (is_gene) "gene" else "transcript"
                    selected_type <- selected_type_local
                    plot_data <- tryCatch(fileDataHomologous_rv()[[id_local]], error = function(e) NULL)
                    ttl       <- tryCatch(titlesHomologous_rv()[[id_local]], error = function(e) "")
                    genome_path <- tryCatch(genomePathsHomologous_rv()[[id_local]], error = function(e) NULL)
                    if (is.function(build_selected_sequence_fasta_content_fn)) {
                        fasta_txt <- build_selected_sequence_fasta_content_fn(
                            sequence_type = selected_type,
                            plot_data = plot_data,
                            gene_meta = gene_meta,
                            title_txt = ttl,
                            genome_path = genome_path,
                            fallback_id = paste0(default_type, "_", id_local)
                        )
                        writeLines(fasta_txt, file)
                    } else if (is_gene && is.function(build_gene_sequence_content_fn)) {
                        fasta_txt <- build_gene_sequence_content_fn(
                            genome_path = genome_path,
                            gene_name   = as.character(gene_meta$display_gene_name %||% gene_meta$matched_gene_name %||% "gene"),
                            chr_name    = as.character(gene_meta$seqid %||% ""),
                            gene_start  = gene_meta$gene_start_bp,
                            gene_end    = gene_meta$gene_end_bp,
                            strand      = as.character(gene_meta$strand %||% "+")
                        )
                        writeLines(fasta_txt, file)
                    } else {
                        seqs      <- genSequencesHomologous_rv()
                        seq_blob  <- as.character(seqs[[id_local]] %||% "")
                        if ((!nzchar(trimws(seq_blob)) || (exists("is_sequence_composition_blob", mode = "function") && isTRUE(is_sequence_composition_blob(seq_blob)))) && is.function(build_sequence_blob_from_plot_fn)) {
                            seq_blob <- build_sequence_blob_from_plot_fn(
                                plot_data   = plot_data,
                                title_txt   = ttl,
                                genome_path = genome_path
                            )
                            if (nzchar(trimws(seq_blob))) {
                                seqs[[id_local]] <- seq_blob
                                genSequencesHomologous_rv(seqs)
                            }
                        }
                        if (is.function(build_transcript_fasta_content_fn)) {
                            fasta_txt <- build_transcript_fasta_content_fn(
                                seq_blob    = seq_blob,
                                plot_data   = plot_data,
                                title_txt   = ttl,
                                fallback_id = paste0("transcript_", id_local)
                            )
                            writeLines(fasta_txt, file)
                        } else {
                            writeLines("", file)
                        }
                    }
                }
            )
        })
        invisible(NULL)
    }

    register_homologous_transcript_download <- function(tx_plot_id, id_chr, tx_id) {
        tx_plot_id <- as.character(tx_plot_id %||% "")
        if (!nzchar(tx_plot_id)) return(invisible(NULL))
        for (sequence_type in c("gene", "transcript", "cds", "cds_segments", "introns")) local({
            tx_local_id <- tx_plot_id
            parent_id <- id_chr
            t_id <- tx_id
            selected_type_local <- sequence_type
            output[[paste0("download_homo_", tx_local_id, "_", selected_type_local)]] <- downloadHandler(
                filename = function() {
                    selected_type <- selected_type_local
                    sequence_download_filename(t_id, selected_type, "transcript", fallback = paste0("transcript_", tx_local_id))
                },
                content = function(file) {
                    seqs <- genSequencesHomologous_rv()
                    seq_blob <- as.character(seqs[[parent_id]] %||% "")
                    plot_data <- tryCatch(fileDataHomologous_rv()[[parent_id]], error = function(e) NULL)
                    # Fake a title so build_transcript_fasta_content_fn picks up OUR tx_id
                    fake_ttl <- paste0("Transcript: ", t_id)
                    selected_type <- selected_type_local
                    genome_path <- tryCatch(genomePathsHomologous_rv()[[parent_id]], error = function(e) NULL)
                    
                    if (is.function(build_selected_sequence_fasta_content_fn)) {
                        gene_meta <- tryCatch(plotGeneMetaHomologous_rv()[[parent_id]], error = function(e) NULL)
                        fasta_txt <- build_selected_sequence_fasta_content_fn(
                            sequence_type = selected_type,
                            plot_data = plot_data,
                            gene_meta = gene_meta,
                            title_txt = fake_ttl,
                            genome_path = genome_path,
                            fallback_id = paste0("transcript_", tx_local_id)
                        )
                        writeLines(fasta_txt, file)
                    } else {
                    if ((!nzchar(trimws(seq_blob)) || (exists("is_sequence_composition_blob", mode = "function") && isTRUE(is_sequence_composition_blob(seq_blob)))) && is.function(build_sequence_blob_from_plot_fn)) {
                        seq_blob <- build_sequence_blob_from_plot_fn(
                            plot_data = plot_data,
                            title_txt = fake_ttl,
                            genome_path = genome_path
                        )
                        if (nzchar(trimws(seq_blob))) {
                            seqs[[parent_id]] <- seq_blob
                            genSequencesHomologous_rv(seqs)
                        }
                    }
                    if (is.function(build_transcript_fasta_content_fn)) {
                        fasta_txt <- build_transcript_fasta_content_fn(
                            seq_blob = seq_blob,
                            plot_data = plot_data,
                            title_txt = fake_ttl,
                            fallback_id = paste0("transcript_", tx_local_id)
                        )
                        writeLines(fasta_txt, file)
                    } else {
                        writeLines("", file)
                    }
                    }
                }
            )
        })

        invisible(NULL)
    }

    register_orthologous_download_output <- function(id_chr) {
        id_chr <- as.character(id_chr %||% "")
        if (!nzchar(id_chr)) {
            return(invisible(NULL))
        }
        for (sequence_type in c("gene", "transcript", "cds", "cds_segments", "introns")) local({
            id_local <- id_chr
            selected_type_local <- sequence_type
            output[[paste0("download_ortho_", id_local, "_", selected_type_local)]] <- downloadHandler(
                filename = function() {
                    gene_meta <- tryCatch(plotGeneMetaOrthologous_rv()[[id_local]], error = function(e) NULL)
                    is_gene   <- isTRUE(gene_meta$is_canonical)
                    default_type <- if (is_gene) "gene" else "transcript"
                    selected_type <- selected_type_local
                    if (is_gene) {
                        gene_name <- as.character(gene_meta$display_gene_name %||% gene_meta$matched_gene_name %||% "gene")
                        sequence_download_filename(gene_name, selected_type, default_type, fallback = paste0("gene_", id_local))
                    } else {
                        ttl    <- tryCatch(titlesOrthologous_rv()[[id_local]], error = function(e) "")
                        tx_name <- if (is.function(extract_title_field_fn)) extract_title_field_fn(ttl, "Transcript") else ""
                        sequence_download_filename(tx_name, selected_type, default_type, fallback = paste0("transcript_", id_local))
                    }
                },
                content = function(file) {
                    gene_meta <- tryCatch(plotGeneMetaOrthologous_rv()[[id_local]], error = function(e) NULL)
                    is_gene   <- isTRUE(gene_meta$is_canonical)
                    default_type <- if (is_gene) "gene" else "transcript"
                    selected_type <- selected_type_local
                    plot_data <- tryCatch(fileDataOrthologous_rv()[[id_local]], error = function(e) NULL)
                    ttl       <- tryCatch(titlesOrthologous_rv()[[id_local]], error = function(e) "")
                    genome_path <- tryCatch(genomePathsOrthologous_rv()[[id_local]], error = function(e) NULL)
                    if (is.function(build_selected_sequence_fasta_content_fn)) {
                        fasta_txt <- build_selected_sequence_fasta_content_fn(
                            sequence_type = selected_type,
                            plot_data = plot_data,
                            gene_meta = gene_meta,
                            title_txt = ttl,
                            genome_path = genome_path,
                            fallback_id = paste0(default_type, "_", id_local)
                        )
                        writeLines(fasta_txt, file)
                    } else if (is_gene && is.function(build_gene_sequence_content_fn)) {
                        fasta_txt <- build_gene_sequence_content_fn(
                            genome_path = genome_path,
                            gene_name   = as.character(gene_meta$display_gene_name %||% gene_meta$matched_gene_name %||% "gene"),
                            chr_name    = as.character(gene_meta$seqid %||% ""),
                            gene_start  = gene_meta$gene_start_bp,
                            gene_end    = gene_meta$gene_end_bp,
                            strand      = as.character(gene_meta$strand %||% "+")
                        )
                        writeLines(fasta_txt, file)
                    } else {
                        seqs      <- genSequencesOrthologous_rv()
                        seq_blob  <- as.character(seqs[[id_local]] %||% "")
                        if ((!nzchar(trimws(seq_blob)) || (exists("is_sequence_composition_blob", mode = "function") && isTRUE(is_sequence_composition_blob(seq_blob)))) && is.function(build_sequence_blob_from_plot_fn)) {
                            seq_blob <- build_sequence_blob_from_plot_fn(
                                plot_data   = plot_data,
                                title_txt   = ttl,
                                genome_path = genome_path
                            )
                            if (nzchar(trimws(seq_blob))) {
                                seqs[[id_local]] <- seq_blob
                                genSequencesOrthologous_rv(seqs)
                            }
                        }
                        if (is.function(build_transcript_fasta_content_fn)) {
                            fasta_txt <- build_transcript_fasta_content_fn(
                                seq_blob    = seq_blob,
                                plot_data   = plot_data,
                                title_txt   = ttl,
                                fallback_id = paste0("transcript_", id_local)
                            )
                            writeLines(fasta_txt, file)
                        } else {
                            writeLines("", file)
                        }
                    }
                }
            )
        })
        invisible(NULL)
    }

    register_orthologous_transcript_download <- function(tx_plot_id, id_chr, tx_id) {
        tx_plot_id <- as.character(tx_plot_id %||% "")
        if (!nzchar(tx_plot_id)) return(invisible(NULL))
        for (sequence_type in c("gene", "transcript", "cds", "cds_segments", "introns")) local({
            tx_local_id <- tx_plot_id
            parent_id <- id_chr
            t_id <- tx_id
            selected_type_local <- sequence_type
            output[[paste0("download_ortho_", tx_local_id, "_", selected_type_local)]] <- downloadHandler(
                filename = function() {
                    selected_type <- selected_type_local
                    sequence_download_filename(t_id, selected_type, "transcript", fallback = paste0("transcript_", tx_local_id))
                },
                content = function(file) {
                    seqs <- genSequencesOrthologous_rv()
                    seq_blob <- as.character(seqs[[parent_id]] %||% "")
                    plot_data <- tryCatch(fileDataOrthologous_rv()[[parent_id]], error = function(e) NULL)
                    # Fake a title so build_transcript_fasta_content_fn picks up OUR tx_id
                    fake_ttl <- paste0("Transcript: ", t_id)
                    selected_type <- selected_type_local
                    genome_path <- tryCatch(genomePathsOrthologous_rv()[[parent_id]], error = function(e) NULL)
                    
                    if (is.function(build_selected_sequence_fasta_content_fn)) {
                        gene_meta <- tryCatch(plotGeneMetaOrthologous_rv()[[parent_id]], error = function(e) NULL)
                        fasta_txt <- build_selected_sequence_fasta_content_fn(
                            sequence_type = selected_type,
                            plot_data = plot_data,
                            gene_meta = gene_meta,
                            title_txt = fake_ttl,
                            genome_path = genome_path,
                            fallback_id = paste0("transcript_", tx_local_id)
                        )
                        writeLines(fasta_txt, file)
                    } else {
                    if ((!nzchar(trimws(seq_blob)) || (exists("is_sequence_composition_blob", mode = "function") && isTRUE(is_sequence_composition_blob(seq_blob)))) && is.function(build_sequence_blob_from_plot_fn)) {
                        seq_blob <- build_sequence_blob_from_plot_fn(
                            plot_data = plot_data,
                            title_txt = fake_ttl,
                            genome_path = genome_path
                        )
                        if (nzchar(trimws(seq_blob))) {
                            seqs[[parent_id]] <- seq_blob
                            genSequencesOrthologous_rv(seqs)
                        }
                    }
                    if (is.function(build_transcript_fasta_content_fn)) {
                        fasta_txt <- build_transcript_fasta_content_fn(
                            seq_blob = seq_blob,
                            plot_data = plot_data,
                            title_txt = fake_ttl,
                            fallback_id = paste0("transcript_", tx_local_id)
                        )
                        writeLines(fasta_txt, file)
                    } else {
                        writeLines("", file)
                    }
                    }
                }
            )
        })
        invisible(NULL)
    }

    unbind_homologous_download_output <- function(id_chr) {
        id_chr <- as.character(id_chr %||% "")
        if (!nzchar(id_chr)) {
            return(invisible(NULL))
        }
        for (sequence_type in c("gene", "transcript", "cds", "cds_segments", "introns")) {
            clear_dynamic_output_binding(paste0("download_homo_", id_chr, "_", sequence_type))
        }
        bound_now <- as.character(homoDownloadOutputsBound_rv() %||% character(0))
        bound_keep <- setdiff(bound_now, id_chr)
        if (!identical(bound_now, bound_keep)) {
            homoDownloadOutputsBound_rv(bound_keep)
        }
        invisible(NULL)
    }

    unbind_orthologous_download_output <- function(id_chr) {
        id_chr <- as.character(id_chr %||% "")
        if (!nzchar(id_chr)) {
            return(invisible(NULL))
        }
        for (sequence_type in c("gene", "transcript", "cds", "cds_segments", "introns")) {
            clear_dynamic_output_binding(paste0("download_ortho_", id_chr, "_", sequence_type))
        }
        bound_now <- as.character(orthoDownloadOutputsBound_rv() %||% character(0))
        bound_keep <- setdiff(bound_now, id_chr)
        if (!identical(bound_now, bound_keep)) {
            orthoDownloadOutputsBound_rv(bound_keep)
        }
        invisible(NULL)
    }

    destroy_homologous_plot_runtime <- function(id_chr) {
        id_chr <- as.character(id_chr %||% "")
        if (!nzchar(id_chr)) {
            return(invisible(NULL))
        }
        handle <- tryCatch(existingPlotsHomologous_rv()[[id_chr]], error = function(e) NULL)
        destroy_plot_module_handle(handle)
        remove_plot_card_dom("homologous", id_chr)
        unbind_homologous_footer_output(id_chr)
        unbind_homologous_download_output(id_chr)
        ready_now <- as.character(homoRenderedPlotIds_rv() %||% character(0))
        ready_keep <- setdiff(ready_now, id_chr)
        if (!identical(ready_now, ready_keep)) {
            homoRenderedPlotIds_rv(ready_keep)
        }
        inserted_now <- as.character(homoInsertedCardIds_rv() %||% character(0))
        inserted_keep <- setdiff(inserted_now, id_chr)
        if (!identical(inserted_now, inserted_keep)) {
            homoInsertedCardIds_rv(inserted_keep)
        }
        if (is.function(homoHydratedIsoformIds_rv)) {
            hydrated_now <- as.character(homoHydratedIsoformIds_rv() %||% character(0))
            hydrated_keep <- setdiff(hydrated_now, id_chr)
            if (!identical(hydrated_now, hydrated_keep)) {
                homoHydratedIsoformIds_rv(hydrated_keep)
            }
        }
        drop_plot_timing_id_fn(homoPlotTimingTracker_rv, id_chr)
        invisible(NULL)
    }

    destroy_orthologous_plot_runtime <- function(id_chr) {
        id_chr <- as.character(id_chr %||% "")
        if (!nzchar(id_chr)) {
            return(invisible(NULL))
        }
        handle <- tryCatch(existingPlotsOrthologous_rv()[[id_chr]], error = function(e) NULL)
        destroy_plot_module_handle(handle)
        remove_plot_card_dom("orthologous", id_chr)
        unbind_orthologous_footer_output(id_chr)
        unbind_orthologous_download_output(id_chr)
        ready_now <- as.character(orthoRenderedPlotIds_rv() %||% character(0))
        ready_keep <- setdiff(ready_now, id_chr)
        if (!identical(ready_now, ready_keep)) {
            orthoRenderedPlotIds_rv(ready_keep)
        }
        inserted_now <- as.character(orthoInsertedCardIds_rv() %||% character(0))
        inserted_keep <- setdiff(inserted_now, id_chr)
        if (!identical(inserted_now, inserted_keep)) {
            orthoInsertedCardIds_rv(inserted_keep)
        }
        drop_plot_timing_id_fn(orthoPlotTimingTracker_rv, id_chr)
        invisible(NULL)
    }

    clear_homologous_visualizations <- function() {
        runtime_ids <- unique(c(
            names(tryCatch(existingPlotsHomologous_rv(), error = function(e) list())),
            as.character(homoFooterOutputsBound_rv() %||% character(0)),
            as.character(homoDownloadOutputsBound_rv() %||% character(0))
        ))
        runtime_ids <- runtime_ids[nzchar(runtime_ids)]
        if (length(runtime_ids) > 0L) {
            invisible(lapply(runtime_ids, destroy_homologous_plot_runtime))
        }
        activePlotIdsHomologous_rv(integer())
        titlesHomologous_rv(list())
        existingPlotsHomologous_rv(list())
        fileDataHomologous_rv(list())
        chrNamesHomologous_rv(list())
        genSequencesHomologous_rv(list())
        plotSignaturesHomologous_rv(list())
        annotationPathsHomologous_rv(list())
        genomePathsHomologous_rv(list())
        organismInfoHomologous_rv(list())
        plotMetricsHomologous_rv(list())
        plotGeneMetaHomologous_rv(list())
        closeObserversBoundHomologous_rv(character())
        homoRenderedPlotIds_rv(character())
        homoInsertedCardIds_rv(character())
        if (is.function(homoHydratedIsoformIds_rv)) {
            homoHydratedIsoformIds_rv(character())
        }
        homoFooterOutputsBound_rv(character())
        homoDownloadOutputsBound_rv(character())
        homoPlotTimingTracker_rv(empty_plot_timing_tracker_fn())
        clear_summary_cache_scope_fn("homo", drop_rows = TRUE)
        clear_analytics_cache_scope_fn("homo")
        shinyjs::runjs("var el=document.getElementById('homo-plot-cards-container'); if(el){if(window.Shiny&&Shiny.unbindAll){Shiny.unbindAll(el);} el.innerHTML='';}")
        homoVisibleCount_rv(homoInitialVisibleCount)
        recalc_plot_ranges_homologous()
    }

    clear_orthologous_visualizations <- function() {
        runtime_ids <- unique(c(
            names(tryCatch(existingPlotsOrthologous_rv(), error = function(e) list())),
            as.character(orthoFooterOutputsBound_rv() %||% character(0)),
            as.character(orthoDownloadOutputsBound_rv() %||% character(0))
        ))
        runtime_ids <- runtime_ids[nzchar(runtime_ids)]
        if (length(runtime_ids) > 0L) {
            invisible(lapply(runtime_ids, destroy_orthologous_plot_runtime))
        }
        activePlotIdsOrthologous_rv(integer())
        titlesOrthologous_rv(list())
        existingPlotsOrthologous_rv(list())
        fileDataOrthologous_rv(list())
        chrNamesOrthologous_rv(list())
        genSequencesOrthologous_rv(list())
        plotSignaturesOrthologous_rv(list())
        annotationPathsOrthologous_rv(list())
        genomePathsOrthologous_rv(list())
        organismInfoOrthologous_rv(list())
        plotMetricsOrthologous_rv(list())
        plotGeneMetaOrthologous_rv(list())
        closeObserversBoundOrthologous_rv(character())
        orthoRenderedPlotIds_rv(character())
        orthoInsertedCardIds_rv(character())
        orthoFooterOutputsBound_rv(character())
        orthoDownloadOutputsBound_rv(character())
        orthoPlotTimingTracker_rv(empty_plot_timing_tracker_fn())
        clear_summary_cache_scope_fn("ortho", drop_rows = TRUE)
        clear_analytics_cache_scope_fn("ortho")
        orthoAlignedRenderCache_rv(list(key = "", plot_obj = NULL))
        track_keys <- ls(envir = orthoAlignedTrackCache_env, all.names = TRUE)
        if (length(track_keys) > 0L) {
            rm(list = track_keys, envir = orthoAlignedTrackCache_env)
        }
        scene_keys <- ls(envir = orthoAlignedSceneCache_env, all.names = TRUE)
        if (length(scene_keys) > 0L) {
            rm(list = scene_keys, envir = orthoAlignedSceneCache_env)
        }
        plot_keys <- ls(envir = orthoAlignedPlotCache_env, all.names = TRUE)
        if (length(plot_keys) > 0L) {
            rm(list = plot_keys, envir = orthoAlignedPlotCache_env)
        }
        seq_keys <- ls(envir = orthoAlignedSeqCache_env, all.names = TRUE)
        if (length(seq_keys) > 0L) {
            rm(list = seq_keys, envir = orthoAlignedSeqCache_env)
        }
        gc_keys <- ls(envir = orthoAlignedGcCache_env, all.names = TRUE)
        if (length(gc_keys) > 0L) {
            rm(list = gc_keys, envir = orthoAlignedGcCache_env)
        }
        hom_keys <- ls(envir = orthoHomologyCache_env, all.names = TRUE)
        if (length(hom_keys) > 0L) {
            rm(list = hom_keys, envir = orthoHomologyCache_env)
        }
        shinyjs::runjs("var el=document.getElementById('ortho-plot-cards-container'); if(el){if(window.Shiny&&Shiny.unbindAll){Shiny.unbindAll(el);} el.innerHTML='';}")
        orthoVisibleCount_rv(orthoInitialVisibleCount)
        recalc_plot_ranges_orthologous()
    }

    clear_all_visualizations <- function() {
        clear_homologous_visualizations()
        clear_orthologous_visualizations()
        invisible(NULL)
    }

    panel_display_label <- function(panel = c("homologous", "orthologous")) {
        panel <- match.arg(panel)
        if (identical(panel, "homologous")) "Multi-Gene Search" else "Cross-Species Gene Search"
    }

    get_plot_count_for_panel <- function(panel = c("homologous", "orthologous")) {
        panel <- match.arg(panel)
        if (identical(panel, "homologous")) {
            length(activePlotIdsHomologous_rv() %||% integer(0))
        } else {
            length(activePlotIdsOrthologous_rv() %||% integer(0))
        }
    }

    resolve_visualization_clear_panels <- function() {
        active_panel <- as.character(input$navtabs %||% "")
        if (!(active_panel %in% c("homologous", "orthologous"))) {
            active_panel <- as.character(preferredSearchWorkflow_fn() %||% "homologous")
        }
        if (!(active_panel %in% c("homologous", "orthologous"))) {
            active_panel <- "homologous"
        }
        other_panel <- if (identical(active_panel, "homologous")) "orthologous" else "homologous"
        list(active = active_panel, other = other_panel)
    }

    clear_visualizations_for_panel <- function(panel = c("homologous", "orthologous")) {
        panel <- match.arg(panel)
        if (identical(panel, "homologous")) {
            clear_homologous_visualizations()
        } else {
            clear_orthologous_visualizations()
        }
        invisible(NULL)
    }

    resolve_plot_removal_ids <- function(
        id_txt,
        active_ids,
        plot_gene_meta,
        annotation_paths,
        organism_info
    ) {
        id_txt <- as.character(id_txt %||% "")
        active_keys <- unique(as.character(active_ids %||% character(0)))
        active_keys <- active_keys[nzchar(active_keys)]
        if (!nzchar(id_txt) || !(id_txt %in% active_keys)) {
            return(id_txt)
        }

        ref_meta <- tryCatch(plot_gene_meta[[id_txt]], error = function(e) NULL)
        if (is.null(ref_meta) || !isTRUE(ref_meta$is_canonical)) {
            return(id_txt)
        }

        normalize_group_value <- function(x) {
            value <- trimws(tolower(as.character(x %||% "")))
            value <- value[!is.na(value) & nzchar(value)]
            if (length(value) > 0L) value[[1L]] else ""
        }
        gene_group_key <- function(meta) {
            for (value in list(
                meta$matched_gene_id,
                meta$display_gene_name,
                meta$matched_gene_name,
                meta$query_gene
            )) {
                key <- normalize_group_value(value)
                if (nzchar(key)) return(key)
            }
            ""
        }
        source_group_key <- function(plot_id, meta) {
            annotation_key <- normalize_group_value(annotation_paths[[plot_id]])
            if (nzchar(annotation_key)) {
                return(paste0("annotation:", annotation_key))
            }
            organism_key <- normalize_group_value(
                meta$organism_scientific %||% organism_info[[plot_id]]$name
            )
            if (nzchar(organism_key)) paste0("organism:", organism_key) else ""
        }

        ref_gene_key <- gene_group_key(ref_meta)
        ref_source_key <- source_group_key(id_txt, ref_meta)
        if (!nzchar(ref_gene_key) || !nzchar(ref_source_key)) {
            return(id_txt)
        }

        group_ids <- Filter(function(candidate_id) {
            candidate_meta <- tryCatch(plot_gene_meta[[candidate_id]], error = function(e) NULL)
            if (is.null(candidate_meta) || isTRUE(candidate_meta$is_canonical_copy)) {
                return(FALSE)
            }
            identical(gene_group_key(candidate_meta), ref_gene_key) &&
                identical(source_group_key(candidate_id, candidate_meta), ref_source_key)
        }, active_keys)
        unique(c(id_txt, as.character(group_ids %||% character(0))))
    }

    remove_homologous_plot <- function(id_local, announce = TRUE) {
        id_txt <- as.character(id_local %||% "")
        if (!nzchar(id_txt)) {
            return(invisible(FALSE))
        }
        id_int <- suppressWarnings(as.integer(id_txt))
        if (!is.finite(id_int)) {
            return(invisible(FALSE))
        }
        remove_ids <- resolve_plot_removal_ids(
            id_txt = id_txt,
            active_ids = activePlotIdsHomologous_rv(),
            plot_gene_meta = plotGeneMetaHomologous_rv(),
            annotation_paths = annotationPathsHomologous_rv(),
            organism_info = organismInfoHomologous_rv()
        )
        state_keys <- unique(c(remove_ids, paste0(remove_ids, "_c")))
        invisible(lapply(state_keys, destroy_homologous_plot_runtime))
        activePlotIdsHomologous_rv(setdiff(
            activePlotIdsHomologous_rv(),
            suppressWarnings(as.integer(remove_ids))
        ))
        for (key in state_keys) {
            remove_named_value(titlesHomologous_rv, key)
            remove_named_value(existingPlotsHomologous_rv, key)
            remove_named_value(fileDataHomologous_rv, key)
            remove_named_value(chrNamesHomologous_rv, key)
            remove_named_value(genSequencesHomologous_rv, key)
            remove_named_value(plotSignaturesHomologous_rv, key)
            remove_named_value(annotationPathsHomologous_rv, key)
            remove_named_value(genomePathsHomologous_rv, key)
            remove_named_value(organismInfoHomologous_rv, key)
            remove_named_value(plotMetricsHomologous_rv, key)
            remove_named_value(plotGeneMetaHomologous_rv, key)
        }
        clear_summary_cache_scope_fn("homo", drop_rows = FALSE)
        clear_analytics_cache_scope_fn("homo")
        recalc_plot_ranges_homologous()
        if (isTRUE(announce)) {
            append_status_fn(searchStatusHomologous_rv, "Visualization removed.")
        }
        invisible(TRUE)
    }

    remove_orthologous_plot <- function(id_local, announce = TRUE) {
        id_txt <- as.character(id_local %||% "")
        if (!nzchar(id_txt)) {
            return(invisible(FALSE))
        }
        id_int <- suppressWarnings(as.integer(id_txt))
        if (!is.finite(id_int)) {
            return(invisible(FALSE))
        }
        remove_ids <- resolve_plot_removal_ids(
            id_txt = id_txt,
            active_ids = activePlotIdsOrthologous_rv(),
            plot_gene_meta = plotGeneMetaOrthologous_rv(),
            annotation_paths = annotationPathsOrthologous_rv(),
            organism_info = organismInfoOrthologous_rv()
        )
        state_keys <- unique(c(remove_ids, paste0(remove_ids, "_c")))
        invisible(lapply(state_keys, destroy_orthologous_plot_runtime))
        activePlotIdsOrthologous_rv(setdiff(
            activePlotIdsOrthologous_rv(),
            suppressWarnings(as.integer(remove_ids))
        ))
        for (key in state_keys) {
            remove_named_value(titlesOrthologous_rv, key)
            remove_named_value(existingPlotsOrthologous_rv, key)
            remove_named_value(fileDataOrthologous_rv, key)
            remove_named_value(chrNamesOrthologous_rv, key)
            remove_named_value(genSequencesOrthologous_rv, key)
            remove_named_value(plotSignaturesOrthologous_rv, key)
            remove_named_value(annotationPathsOrthologous_rv, key)
            remove_named_value(genomePathsOrthologous_rv, key)
            remove_named_value(organismInfoOrthologous_rv, key)
            remove_named_value(plotMetricsOrthologous_rv, key)
            remove_named_value(plotGeneMetaOrthologous_rv, key)
        }
        clear_summary_cache_scope_fn("ortho", drop_rows = FALSE)
        clear_analytics_cache_scope_fn("ortho")
        recalc_plot_ranges_orthologous()
        if (isTRUE(announce)) {
            append_status_fn(searchStatusOrthologous_rv, "Visualization removed.")
        }
        invisible(TRUE)
    }

    run_clear_selected_visualizations <- function(active_panel = c("homologous", "orthologous"), include_other = FALSE) {
        active_panel <- match.arg(active_panel)
        other_panel <- if (identical(active_panel, "homologous")) "orthologous" else "homologous"

        n_active <- get_plot_count_for_panel(active_panel)
        n_other <- get_plot_count_for_panel(other_panel)

        clear_visualizations_for_panel(active_panel)
        if (isTRUE(include_other)) {
            clear_visualizations_for_panel(other_panel)
        }

        active_label <- panel_display_label(active_panel)
        other_label <- panel_display_label(other_panel)

        if (isTRUE(include_other)) {
            msg <- sprintf(
                "Visualizations cleared: %s (%d) and %s (%d).",
                active_label,
                as.integer(n_active),
                other_label,
                as.integer(n_other)
            )
            append_status_fn(searchStatusHomologous_rv, msg)
            append_status_fn(searchStatusOrthologous_rv, msg)
            emit_popup_status_fn("Visualizations", msg, tone = "info", clear = FALSE)
            return(invisible(TRUE))
        }

        msg <- sprintf("Visualizations cleared in %s (%d).", active_label, as.integer(n_active))
        if (identical(active_panel, "homologous")) {
            append_status_fn(searchStatusHomologous_rv, msg)
        } else {
            append_status_fn(searchStatusOrthologous_rv, msg)
        }
        emit_popup_status_fn("Visualizations", msg, tone = "info", clear = FALSE)
        invisible(TRUE)
    }

    get_plot_delete_title <- function(panel = c("homologous", "orthologous"), id_local = "") {
        panel <- match.arg(panel)
        id_txt <- as.character(id_local %||% "")
        if (!nzchar(id_txt)) {
            return("Visualization")
        }
        ttl <- if (identical(panel, "homologous")) {
            as.character(titlesHomologous_rv()[[id_txt]] %||% "")
        } else {
            as.character(titlesOrthologous_rv()[[id_txt]] %||% "")
        }
        ttl <- trimws(ttl)
        if (!nzchar(ttl)) "Visualization" else ttl
    }

    safe_normalize_path <- function(path_txt) {
        p <- trimws(as.character(path_txt %||% ""))
        if (!nzchar(p)) {
            return("")
        }
        normalizePath(p, winslash = "/", mustWork = FALSE)
    }

    resolve_report_map_context <- function(annotation_path = "") {
        ann_norm <- safe_normalize_path(annotation_path)
        if (!nzchar(ann_norm)) {
            return(list(use_report_map = FALSE, report_path = "", organism_name = ""))
        }

        reg <- preloadedRegistry_rv()
        if (is.null(reg) || nrow(reg) == 0 || !"annotation_path" %in% colnames(reg)) {
            return(list(use_report_map = FALSE, report_path = "", organism_name = ""))
        }
        reg_ann <- as.character(reg$annotation_path %||% rep("", nrow(reg)))
        reg_ann_norm <- vapply(reg_ann, safe_normalize_path, character(1))
        hit <- which(reg_ann_norm == ann_norm)[1]
        if (!is.finite(hit)) {
            return(list(use_report_map = FALSE, report_path = "", organism_name = ""))
        }

        rpt <- as.character(reg$assembly_report_path[hit] %||% "")
        org_name <- as.character(reg$organism[hit] %||% "")
        list(
            use_report_map = nzchar(trimws(rpt)),
            report_path = rpt,
            organism_name = org_name
        )
    }

    instantiate_homologous_plot_module <- function(id_chr) {
        id_txt <- as.character(id_chr %||% "")
        if (!nzchar(id_txt)) {
            return(invisible(FALSE))
        }
        already <- tryCatch(existingPlotsHomologous_rv()[[id_txt]], error = function(e) NULL)
        if (!is.null(already)) {
            return(invisible(TRUE))
        }
        block_data <- tryCatch(fileDataHomologous_rv()[[id_txt]], error = function(e) NULL)
        if (is.null(block_data) || !is.data.frame(block_data) || nrow(block_data) == 0) {
            return(invisible(FALSE))
        }

        annotation_path <- as.character(annotationPathsHomologous_rv()[[id_txt]] %||% "")
        genome_path <- as.character(genomePathsHomologous_rv()[[id_txt]] %||% "")
        org_info <- tryCatch(organismInfoHomologous_rv()[[id_txt]], error = function(e) NULL)
        org_name <- as.character(org_info$name %||% "")
        report_ctx <- resolve_report_map_context(annotation_path)
        use_report_map <- isTRUE(report_ctx$use_report_map)
        report_path <- as.character(report_ctx$report_path %||% "")
        if (!nzchar(org_name)) {
            org_name <- as.character(report_ctx$organism_name %||% "")
        }
        if (!nzchar(org_name)) {
            org_name <- "Organism"
        }
        gene_meta <- tryCatch(plotGeneMetaHomologous_rv()[[id_txt]], error = function(e) NULL)
        gene_name <- as.character(gene_meta$display_gene_name %||% gene_meta$matched_gene_name %||%
            (if (is.function(extract_title_field_fn)) extract_title_field_fn(titlesHomologous_rv()[[id_txt]] %||% "", "Gene") else NA_character_) %||%
            "Gene")
        if (!nzchar(trimws(gene_name))) gene_name <- "Gene"
        precomputed_neighbor_context <- tryCatch(gene_meta$precomputed_neighbor_context, error = function(e) NULL)
        plot_sig <- as.character(tryCatch(plotSignaturesHomologous_rv()[[id_txt]], error = function(e) "") %||% "")

        module_handle <- local({
            this_id <- id_txt
            this_data <- block_data
            this_gene_name <- gene_name
            this_genome <- if (nzchar(genome_path)) genome_path else NULL
            this_ann <- if (nzchar(annotation_path)) annotation_path else NULL
            this_org <- org_name
            this_use_report <- use_report_map
            this_report <- report_path
            this_plot_sig <- plot_sig
            plot_args <- list(
                paste0("plot_homo_", this_id),
                this_data,
                max_gene_length_homo_rv,
                min_gene_coord_homo_rv,
                max_gene_coord_homo_rv,
                genSequencesHomologous_rv,
                as.character(this_id),
                this_gene_name,
                this_genome,
                this_ann,
                visual_mode = reactive({
                    mode <- tolower(as.character(input$homo_visual_mode %||% "compact"))
                    if (!mode %in% c("compact", "detailed")) mode <- "compact"
                    mode
                }),
                orientation_mode = reactive({
                    mode <- tolower(trimws(as.character(input$homo_orientation_pick %||% "genomic")))
                    if (!mode %in% c("genomic", "transcription")) mode <- "genomic"
                    mode
                }),
                organism_name = this_org,
                use_report_map = this_use_report,
                report_path = this_report,
                app_theme = reactive({
                    mode <- tolower(as.character(input$app_theme %||% "light"))
                    if (!mode %in% c("light", "dark")) mode <- "light"
                    mode
                }),
                app_colorblind = reactive({
                    isTRUE(input$colorblind_mode)
                })
            )
            if ("plot_signature" %in% names(formals(plotServerHomologous))) {
                plot_args$plot_signature <- this_plot_sig
            }
            if ("precomputed_neighbor_context" %in% names(formals(plotServerHomologous)) && !is.null(precomputed_neighbor_context)) {
                plot_args$precomputed_neighbor_context <- precomputed_neighbor_context
            }
            if ("on_plot_ready" %in% names(formals(plotServerHomologous))) {
                plot_args$on_plot_ready <- function(pid = NULL) {
                    pid_chr <- as.character(pid %||% this_id)
                    if (!nzchar(pid_chr)) {
                        return(invisible(NULL))
                    }
                    ready <- as.character(homoRenderedPlotIds_rv() %||% character(0))
                    if (!(pid_chr %in% ready)) {
                        homoRenderedPlotIds_rv(c(ready, pid_chr))
                    }
                    mark_plot_ready_timing_fn(homoPlotTimingTracker_rv, pid_chr, context = "HOMO_TIMING")
                    invisible(NULL)
                }
            }
            do.call(plotServerHomologous, plot_args)
        })
        set_named_value(existingPlotsHomologous_rv, id_txt, module_handle %||% TRUE)
        invisible(TRUE)
    }

    instantiate_orthologous_plot_module <- function(id_chr) {
        id_txt <- as.character(id_chr %||% "")
        if (!nzchar(id_txt)) {
            return(invisible(FALSE))
        }
        already <- tryCatch(existingPlotsOrthologous_rv()[[id_txt]], error = function(e) NULL)
        if (!is.null(already)) {
            return(invisible(TRUE))
        }
        block_data <- tryCatch(fileDataOrthologous_rv()[[id_txt]], error = function(e) NULL)
        if (is.null(block_data) || !is.data.frame(block_data) || nrow(block_data) == 0) {
            return(invisible(FALSE))
        }

        annotation_path <- as.character(annotationPathsOrthologous_rv()[[id_txt]] %||% "")
        genome_path <- as.character(genomePathsOrthologous_rv()[[id_txt]] %||% "")
        org_info <- tryCatch(organismInfoOrthologous_rv()[[id_txt]], error = function(e) NULL)
        org_name <- as.character(org_info$name %||% "")
        report_ctx <- resolve_report_map_context(annotation_path)
        use_report_map <- isTRUE(report_ctx$use_report_map)
        report_path <- as.character(report_ctx$report_path %||% "")
        if (!nzchar(org_name)) {
            org_name <- as.character(report_ctx$organism_name %||% "")
        }
        if (!nzchar(org_name)) {
            org_name <- "Organism"
        }
        gene_meta <- tryCatch(plotGeneMetaOrthologous_rv()[[id_txt]], error = function(e) NULL)
        gene_name <- as.character(gene_meta$display_gene_name %||% gene_meta$matched_gene_name %||%
            (if (is.function(extract_title_field_fn)) extract_title_field_fn(titlesOrthologous_rv()[[id_txt]] %||% "", "Gene") else NA_character_) %||%
            "Gene")
        if (!nzchar(trimws(gene_name))) gene_name <- "Gene"
        precomputed_neighbor_context <- tryCatch(gene_meta$precomputed_neighbor_context, error = function(e) NULL)
        plot_sig <- as.character(tryCatch(plotSignaturesOrthologous_rv()[[id_txt]], error = function(e) "") %||% "")

        module_handle <- local({
            this_id <- id_txt
            this_data <- block_data
            this_gene_name <- gene_name
            this_genome <- if (nzchar(genome_path)) genome_path else NULL
            this_ann <- if (nzchar(annotation_path)) annotation_path else NULL
            this_org <- org_name
            this_use_report <- use_report_map
            this_report <- report_path
            this_plot_sig <- plot_sig
            plot_args <- list(
                paste0("plot_ortho_", this_id),
                this_data,
                max_gene_length_ortho_rv,
                min_gene_coord_ortho_rv,
                max_gene_coord_ortho_rv,
                genSequencesOrthologous_rv,
                as.character(this_id),
                this_gene_name,
                this_genome,
                this_ann,
                visual_mode = reactive({
                    mode <- tolower(as.character(input$ortho_visual_mode %||% "compact"))
                    if (!mode %in% c("compact", "detailed", "aligned")) mode <- "compact"
                    mode
                }),
                orientation_mode = reactive({
                    mode <- tolower(trimws(as.character(input$ortho_orientation_pick %||% "genomic")))
                    if (!mode %in% c("genomic", "transcription")) mode <- "genomic"
                    mode
                }),
                organism_name = this_org,
                use_report_map = this_use_report,
                report_path = this_report,
                app_theme = reactive({
                    mode <- tolower(as.character(input$app_theme %||% "light"))
                    if (!mode %in% c("light", "dark")) mode <- "light"
                    mode
                }),
                app_colorblind = reactive({
                    isTRUE(input$colorblind_mode)
                })
            )
            if ("plot_signature" %in% names(formals(plotServerOrtologous))) {
                plot_args$plot_signature <- this_plot_sig
            }
            if ("prefetch_sequence" %in% names(formals(plotServerOrtologous))) {
                plot_args$prefetch_sequence <- TRUE
            }
            if ("prefetch_neighbor_context" %in% names(formals(plotServerOrtologous))) {
                plot_args$prefetch_neighbor_context <- TRUE
            }
            if ("precomputed_neighbor_context" %in% names(formals(plotServerOrtologous)) && !is.null(precomputed_neighbor_context)) {
                plot_args$precomputed_neighbor_context <- precomputed_neighbor_context
            }
            if ("on_plot_ready" %in% names(formals(plotServerOrtologous))) {
                plot_args$on_plot_ready <- function(pid = NULL) {
                    pid_chr <- as.character(pid %||% this_id)
                    if (!nzchar(pid_chr)) {
                        return(invisible(NULL))
                    }
                    ready <- as.character(orthoRenderedPlotIds_rv() %||% character(0))
                    if (!(pid_chr %in% ready)) {
                        orthoRenderedPlotIds_rv(c(ready, pid_chr))
                    }
                    mark_plot_ready_timing_fn(orthoPlotTimingTracker_rv, pid_chr, context = "ORTHO_TIMING")
                    invisible(NULL)
                }
            }
            do.call(plotServerOrtologous, plot_args)
        })
        set_named_value(existingPlotsOrthologous_rv, id_txt, module_handle %||% TRUE)
        invisible(TRUE)
    }

    list(
        set_named_value = set_named_value,
        remove_named_value = remove_named_value,
        clear_dynamic_output_binding = clear_dynamic_output_binding,
        destroy_plot_module_handle = destroy_plot_module_handle,
        compute_plot_ranges = compute_plot_ranges,
        recalc_plot_ranges_homologous = recalc_plot_ranges_homologous,
        recalc_plot_ranges_orthologous = recalc_plot_ranges_orthologous,
        unbind_homologous_footer_output = unbind_homologous_footer_output,
        unbind_orthologous_footer_output = unbind_orthologous_footer_output,
        register_homologous_download_output = register_homologous_download_output,
        register_orthologous_download_output = register_orthologous_download_output,
        unbind_homologous_download_output = unbind_homologous_download_output,
        unbind_orthologous_download_output = unbind_orthologous_download_output,
        destroy_homologous_plot_runtime = destroy_homologous_plot_runtime,
        destroy_orthologous_plot_runtime = destroy_orthologous_plot_runtime,
        clear_homologous_visualizations = clear_homologous_visualizations,
        clear_orthologous_visualizations = clear_orthologous_visualizations,
        clear_all_visualizations = clear_all_visualizations,
        panel_display_label = panel_display_label,
        get_plot_count_for_panel = get_plot_count_for_panel,
        resolve_visualization_clear_panels = resolve_visualization_clear_panels,
        clear_visualizations_for_panel = clear_visualizations_for_panel,
        remove_homologous_plot = remove_homologous_plot,
        remove_orthologous_plot = remove_orthologous_plot,
        run_clear_selected_visualizations = run_clear_selected_visualizations,
        get_plot_delete_title = get_plot_delete_title,
        safe_normalize_path = safe_normalize_path,
        resolve_report_map_context = resolve_report_map_context,
        instantiate_homologous_plot_module = instantiate_homologous_plot_module,
        instantiate_orthologous_plot_module = instantiate_orthologous_plot_module
    )
}
