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

# One instance per Shiny session. At most one current projection per active plot.
# Exact source equality avoids file timestamps/hash collisions and notices row
# order, coordinates, malformed values, organism and annotation-source changes.
new_string_annotation_state <- function() {
    entries <- new.env(parent = emptyenv())
    list(
        get = function(key, source) {
            timing <- isTRUE(app_perf_enabled())
            if (timing) t0 <- app_perf_now()
            entry <- entries[[key]]
            reuse <- !is.null(entry) && identical(entry$source, source)
            if (!reuse) {
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
        prune = function(keys) {
            stale <- setdiff(ls(entries, all.names = TRUE), keys)
            if (length(stale)) rm(list = stale, envir = entries)
            invisible(NULL)
        }
    )
}
