# ==============================================================================
# R/utils.R
# Funciones auxiliares para manejo de secuencias, GFF, FASTA y búsqueda de genes.
# Actualizado para integración con gene_search_lib.R y control de paralelismo.
# ==============================================================================

# Operador null-coalescing simple: devuelve `a` si no es NULL, sino `b`.
# ATENCION: no protege contra vectores vacíos, NA ni strings "".
# Usar para argumentos de función donde solo importa distinguir NULL vs. valor.
# Ver también %|||% para una validación más estricta.
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Lightweight debug/performance toggles.
# Disabled by default. Enable debug and performance logs independently with:
# Sys.setenv(APP_DEBUG_LOGS = "1", APP_PERF_TIMING = "1")
app_debug_enabled <- function() {
    raw <- tolower(trimws(as.character(Sys.getenv("APP_DEBUG_LOGS", "0") %||% "0")))
    !raw %in% c("", "0", "false", "no", "off")
}

app_perf_enabled <- function() {
    raw <- tolower(trimws(as.character(Sys.getenv("APP_PERF_TIMING", Sys.getenv("APP_DEBUG_LOGS", "0")) %||% "0")))
    !raw %in% c("", "0", "false", "no", "off")
}

# Optional persistent capture for manual before/after comparisons.
# Nothing is written unless APP_PERF_TIMING is enabled and APP_PERF_LOG_DIR is set.
# Each R process gets its own file so concurrent ShinyProxy sessions never append
# to the same log.
.app_perf_log_path <- NULL
.app_perf_log_pid <- NA_integer_

app_perf_log_file <- function() {
    if (!isTRUE(app_perf_enabled())) {
        return("")
    }
    current_pid <- as.integer(Sys.getpid())
    if (!is.null(.app_perf_log_path) && identical(.app_perf_log_pid, current_pid)) {
        return(.app_perf_log_path)
    }

    log_root <- trimws(as.character(Sys.getenv("APP_PERF_LOG_DIR", "") %||% ""))
    if (!nzchar(log_root)) {
        .app_perf_log_path <<- ""
        .app_perf_log_pid <<- current_pid
        return("")
    }

    safe_token <- function(x, fallback) {
        out <- trimws(as.character(x %||% ""))[1L]
        out <- gsub("[^A-Za-z0-9._-]+", "_", out)
        out <- gsub("^_+|_+$", "", out)
        if (nzchar(out)) out else fallback
    }

    run_label <- safe_token(Sys.getenv("APP_PERF_RUN_LABEL", "manual"), "manual")
    node_name <- safe_token(Sys.info()[["nodename"]] %||% "host", "host")
    log_dir <- file.path(log_root, run_label)
    dir_ok <- tryCatch({
        dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
        dir.exists(log_dir) && file.access(log_dir, mode = 2L) == 0L
    }, error = function(e) FALSE)
    if (!isTRUE(dir_ok)) {
        .app_perf_log_path <<- ""
        .app_perf_log_pid <<- current_pid
        return("")
    }

    started_tag <- format(Sys.time(), "%Y%m%dT%H%M%OS3Z", tz = "UTC")
    started_tag <- gsub("[^0-9TZ]", "", started_tag)
    path <- file.path(
        log_dir,
        sprintf(
            "perf_%s_%s_%s_pid%s.log",
            run_label,
            started_tag,
            node_name,
            as.character(current_pid)
        )
    )

    meta_keys <- c(
        "CGV_IMAGE", "APP_BUILD_REVISION",
        "APP_FUTURE_MODE", "APP_FUTURE_WORKERS",
        "APP_LASTZ_WORKERS", "APP_LASTZ_GLOBAL_WORKERS",
        "APP_MEMORY_CACHE_BUDGET_MB", "APP_MEMORY_CACHE_PROCESS_COUNT",
        "APP_GFF_CACHE_MAX_MB", "APP_GFF_GENE_INDEX_CACHE_MAX_MB",
        "APP_GFF_GENE_LIGHT_CACHE_MAX_MB", "APP_IDENTITY_DEBOUNCE_MS",
        "APP_SEQ_EXTRACT_CACHE_MAX_MB", "APP_SPLICED_SEQ_CACHE_MAX_MB",
        "APP_ALIAS_SQLITE_CACHE_MB", "APP_ALIAS_SQLITE_MAX_CONNECTIONS",
        "APP_GIRAFE_COMPACT_SVG", "APP_GIRAFE_SVG_DECIMALS",
        "APP_HOMO_RENDER_CHUNK_SIZE", "APP_HOMO_AUTO_RENDER_DELAY_MS",
        "APP_ORTHO_RENDER_CHUNK_SIZE", "APP_ORTHO_AUTO_RENDER_MORE",
        "APP_ORTHO_AUTO_RENDER_DELAY_MS", "APP_HOMO_INITIAL_VISIBLE",
        "APP_ORTHO_INITIAL_VISIBLE", "APP_ISOFORM_RENDER_BATCH_SIZE",
        "APP_ISOFORM_RENDER_BATCH_DELAY_MS",
        "APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY"
    )
    meta_values <- Sys.getenv(meta_keys, unset = "")
    meta_lines <- sprintf("# meta %s=%s", meta_keys, gsub("[\r\n]+", " ", meta_values))
    header <- c(
        "# CGV_PERF_LOG_V1",
        sprintf("# run_label=%s", run_label),
        sprintf("# started_utc=%s", format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")),
        sprintf("# process_id=%s", as.character(current_pid)),
        sprintf("# host=%s", node_name),
        sprintf("# r_version=%s", gsub("[\r\n]+", " ", R.version.string)),
        meta_lines,
        sprintf("# capture_id=%s", started_tag)
    )
    wrote_header <- tryCatch({
        writeLines(header, con = path, useBytes = TRUE)
        TRUE
    }, error = function(e) FALSE)

    .app_perf_log_path <<- if (isTRUE(wrote_header)) path else ""
    .app_perf_log_pid <<- current_pid
    if (isTRUE(wrote_header)) {
        message("[PERF_LOG] ", path)
    }
    .app_perf_log_path
}

app_debug_log <- function(..., .sep = "") {
    if (!isTRUE(app_debug_enabled())) {
        return(invisible(FALSE))
    }
    message(paste0(..., collapse = .sep))
    invisible(TRUE)
}

app_perf_new_run <- function(prefix = "RUN") {
    ts <- format(Sys.time(), "%H%M%OS3")
    ts <- gsub("[^0-9]", "", ts)
    list(
        id = paste0(as.character(prefix %||% "RUN"), "-", ts),
        t0 = as.numeric(proc.time()[["elapsed"]])
    )
}

app_perf_mark <- function(run = NULL, step = "", context = "APP") {
    if (!isTRUE(app_perf_enabled())) {
        return(invisible(NA_real_))
    }
    now <- as.numeric(proc.time()[["elapsed"]])
    run_id <- "-"
    elapsed <- NA_real_
    if (is.list(run)) {
        if (!is.null(run$id)) {
            run_id <- as.character(run$id)
        }
        t0 <- suppressWarnings(as.numeric(run$t0 %||% NA_real_))
        if (is.finite(t0)) {
            elapsed <- now - t0
        }
    }
    head <- sprintf("[PERF][%s][%s]", as.character(context %||% "APP"), run_id)
    if (is.finite(elapsed)) {
        head <- sprintf("%s[+%.3fs]", head, elapsed)
    }
    msg <- trimws(as.character(step %||% ""))
    if (!nzchar(msg)) {
        msg <- "tick"
    }
    line <- paste0(head, " ", msg)
    message(line)
    log_path <- app_perf_log_file()
    if (nzchar(log_path)) {
        try(cat(line, "\n", file = log_path, append = TRUE), silent = TRUE)
    }
    invisible(elapsed)
}

app_perf_now <- function() {
    as.numeric(proc.time()[["elapsed"]])
}

app_perf_elapsed_ms <- function(t0) {
    start_val <- suppressWarnings(as.numeric(t0 %||% NA_real_))
    if (!is.finite(start_val)) {
        return(NA_real_)
    }
    1000 * (app_perf_now() - start_val)
}

app_env_flag <- function(name, default = FALSE) {
    default_raw <- if (isTRUE(default)) "1" else "0"
    raw <- tolower(trimws(as.character(Sys.getenv(as.character(name %||% ""), default_raw) %||% default_raw)))
    !raw %in% c("", "0", "false", "no", "off")
}

# Gene Catalog is intentionally kept as a future-release feature. The code may
# ship with the application, but no UI, browser handlers, or server observers are
# registered unless the release explicitly opts in.
gene_catalog_enabled <- function() {
    app_env_flag("APP_GENE_CATALOG_ENABLED", default = FALSE)
}

cross_species_requires_verified_orthology <- function() {
    app_env_flag("APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY", FALSE)
}

app_env_int <- function(name, default = 0L, min_value = NULL, max_value = NULL) {
    raw <- trimws(as.character(Sys.getenv(as.character(name %||% ""), as.character(default %||% 0L)) %||% as.character(default %||% 0L)))
    parsed <- suppressWarnings(as.integer(raw))
    if (!is.finite(parsed) || is.na(parsed)) {
        parsed <- suppressWarnings(as.integer(default %||% 0L))
    }
    if (!is.null(min_value)) {
        min_i <- suppressWarnings(as.integer(min_value))
        if (is.finite(min_i) && !is.na(min_i) && parsed < min_i) parsed <- min_i
    }
    if (!is.null(max_value)) {
        max_i <- suppressWarnings(as.integer(max_value))
        if (is.finite(max_i) && !is.na(max_i) && parsed > max_i) parsed <- max_i
    }
    parsed
}

app_env_path <- function(name, default = "") {
    raw <- trimws(as.character(Sys.getenv(as.character(name %||% ""), as.character(default %||% "")) %||% ""))
    if (!nzchar(raw)) {
        return("")
    }
    normalizePath(raw, winslash = "/", mustWork = FALSE)
}

get_cgv_data_root <- function(base_dir = ".") {
    root <- app_env_path("CGV_DATA_ROOT", "")
    if (nzchar(root)) {
        return(root)
    }
    normalizePath(base_dir, winslash = "/", mustWork = FALSE)
}

get_cgv_cache_root <- function(base_dir = ".") {
    root <- app_env_path("CGV_CACHE_DIR", Sys.getenv("APP_CACHE_ROOT", ""))
    if (nzchar(root)) {
        return(root)
    }
    normalizePath(file.path(base_dir, "cache"), winslash = "/", mustWork = FALSE)
}

compact_girafe_svg_html <- function(html, decimals = 2L) {
    if (!is.character(html) || length(html) == 0L || !nzchar(html[1])) {
        return(html)
    }

    out <- html
    out <- gsub(">\\s+<", "><", out, perl = TRUE)

    decimals_i <- suppressWarnings(as.integer(decimals %||% 2L))
    if (!is.finite(decimals_i) || is.na(decimals_i) || decimals_i < 0L) {
        return(out)
    }

    # The numeric pass below can only rewrite ASCII decimals carrying at least
    # four fractional digits inside opening tags. When no such candidate exists
    # anywhere in the payload, tag extraction and replacement are provably
    # no-ops; skipping them avoids hundreds of milliseconds of UTF-8
    # gregexpr/regmatches work on large SVGs for no byte change.
    if (!grepl("[0-9]+\\.[0-9]{4,}", out, useBytes = TRUE)) {
        return(out)
    }

    # ggplot-generated SVG coordinates often carry far more precision than the
    # browser can show. Only opening-tag attributes are compacted: visible labels,
    # tooltips and other text nodes must retain their genomic precision.
    tag_matches <- gregexpr("<[^>]+>", out, perl = TRUE)
    tags <- regmatches(out, tag_matches)
    if (!length(tags) || !length(tags[[1]])) {
        return(out)
    }

    compact_tag <- function(tag) {
        matches <- gregexpr(
            "(?<![A-Za-z0-9_])-?\\d+\\.\\d{4,}(?![A-Za-z0-9_])",
            tag,
            perl = TRUE
        )
        vals <- regmatches(tag, matches)
        if (!length(vals) || !length(vals[[1]])) return(tag)
        regmatches(tag, matches) <- lapply(vals, function(x) {
            nums <- suppressWarnings(as.numeric(x))
            repl <- ifelse(
                is.finite(nums),
                format(round(nums, decimals_i), scientific = FALSE, trim = TRUE),
                x
            )
            repl <- sub("\\.?0+$", "", repl, perl = TRUE)
            repl[repl == "-0"] <- "0"
            repl
        })
        tag
    }
    regmatches(out, tag_matches) <- list(vapply(tags[[1]], compact_tag, character(1)))
    out
}

compact_girafe_widget <- function(widget_obj, label = NULL) {
    if (is.null(widget_obj) || !inherits(widget_obj, "girafe") || !is.list(widget_obj$x)) {
        return(widget_obj)
    }
    if (!isTRUE(app_env_flag("APP_GIRAFE_COMPACT_SVG", TRUE))) {
        return(widget_obj)
    }
    if (!is.character(widget_obj$x$html) || length(widget_obj$x$html) == 0L) {
        return(widget_obj)
    }

    min_bytes <- app_env_int("APP_GIRAFE_COMPACT_MIN_BYTES", 0L, min_value = 0L)
    before_bytes <- nchar(widget_obj$x$html, type = "bytes", allowNA = TRUE)
    if (!is.finite(before_bytes) || before_bytes < min_bytes) {
        return(widget_obj)
    }

    decimals <- app_env_int("APP_GIRAFE_SVG_DECIMALS", 1L, min_value = 0L, max_value = 8L)
    compact_html <- compact_girafe_svg_html(widget_obj$x$html, decimals = decimals)
    after_bytes <- nchar(compact_html, type = "bytes", allowNA = TRUE)
    widget_obj$x$html <- compact_html

    if (isTRUE(app_env_flag("APP_TRANSPORT_TIMING", FALSE)) && is.finite(after_bytes) && is.finite(before_bytes)) {
        label_txt <- trimws(as.character(label %||% "girafe"))
        if (!nzchar(label_txt)) label_txt <- "girafe"
        message(sprintf(
            "[TRANSPORT][girafe] label=%s raw=%dB compact=%dB saved=%dB decimals=%d",
            label_txt,
            as.integer(before_bytes),
            as.integer(after_bytes),
            as.integer(max(0, before_bytes - after_bytes)),
            as.integer(decimals)
        ))
    }
    widget_obj
}

app_perf_mark_ms <- function(run = NULL, key = "", elapsed_ms = NA_real_, context = "APP") {
    key_txt <- trimws(as.character(key %||% ""))
    if (!nzchar(key_txt)) {
        key_txt <- "elapsed_ms"
    }
    elapsed_num <- suppressWarnings(as.numeric(elapsed_ms %||% NA_real_))
    label <- if (is.finite(elapsed_num)) sprintf("%.1f", elapsed_num) else "NA"
    app_perf_mark(run, sprintf("%s=%s", key_txt, label), context)
    invisible(elapsed_num)
}

# --- 1. ANÁLISIS DE SECUENCIAS ---

# The caller owns case/whitespace normalization and the percentage denominator.
count_sequence_bases <- function(sequence) {
    if (is.na(sequence)) return(stats::setNames(rep(NA_integer_, 4L), c("A", "T", "C", "G")))
    counts <- tabulate(as.integer(charToRaw(sequence)), nbins = 255L)
    stats::setNames(counts[c(65L, 84L, 67L, 71L)], c("A", "T", "C", "G"))
}

# Character coordinates equal byte coordinates only for ASCII sequences. Keep
# larger/non-ASCII spans on the existing substring path to bound working memory.
make_genomic_gc_index <- function(sequence, max_bases = 2000000L) {
    n <- nchar(sequence, type = "bytes")
    if (n == 0L || n > max_bases || n != nchar(sequence, type = "chars")) return(NULL)
    bytes <- as.integer(charToRaw(sequence))
    known <- bytes %in% c(65L, 84L, 67L, 71L, 97L, 116L, 99L, 103L)
    gc <- bytes %in% c(67L, 71L, 99L, 103L)
    list(length = n, known = c(0L, cumsum(known)), gc = c(0L, cumsum(gc)))
}

gc_percent_from_index <- function(index, starts, ends) {
    starts <- as.integer(starts)
    ends <- as.integer(ends)
    out <- rep(NA_real_, length(starts))
    valid <- !is.na(starts) & !is.na(ends) & starts >= 1L & ends >= starts & ends <= index$length
    known <- index$known[ends[valid] + 1L] - index$known[starts[valid]]
    gc <- index$gc[ends[valid] + 1L] - index$gc[starts[valid]]
    out[valid] <- ifelse(known > 0L, round(100 * gc / known, 2), NA_real_)
    out
}

calculate_sequence_composition <- function(gen_sequence) {
    if (is.null(gen_sequence) || length(gen_sequence) == 0 || is.na(gen_sequence) || gen_sequence == "") {
        return(list(composition = "Sequence Composition: N/A (Sequence empty)", length = 0))
    }
    gen_sequence <- toupper(as.character(gen_sequence[[1]] %||% ""))
    longitud_total <- nchar(gen_sequence, type = "bytes", allowNA = FALSE, keepNA = FALSE)
    if (!is.finite(longitud_total) || longitud_total <= 0L) {
        return(list(composition = "Sequence Composition: N/A (Sequence empty)", length = 0))
    }

    counts <- count_sequence_bases(gen_sequence)
    conteo_A <- counts[["A"]]
    conteo_T <- counts[["T"]]
    conteo_C <- counts[["C"]]
    conteo_G <- counts[["G"]]

    list(
        composition = sprintf(
            "Sequence Composition: A = %.2f%%\tT = %.2f%%  C = %.2f%%  G = %.2f%%",
            (conteo_A / longitud_total) * 100, (conteo_T / longitud_total) * 100,
            (conteo_C / longitud_total) * 100, (conteo_G / longitud_total) * 100
        ),
        length = longitud_total
    )
}

format_sequence_composition_from_counts <- function(counts, denominator = NULL) {
    src <- counts %||% c(A = 0L, T = 0L, C = 0L, G = 0L)
    if (is.null(names(src))) names(src) <- rep("", length(src))
    counts <- suppressWarnings(as.integer(src[c("A", "T", "C", "G")]))
    counts[!is.finite(counts)] <- 0L
    names(counts) <- c("A", "T", "C", "G")
    denom <- suppressWarnings(as.numeric(denominator %||% sum(counts, na.rm = TRUE)))
    if (!is.finite(denom) || denom <= 0) {
        return("Sequence Composition: N/A (Sequence empty)")
    }
    sprintf(
        "Sequence Composition: A = %.2f%%\tT = %.2f%%  C = %.2f%%  G = %.2f%%",
        100 * as.numeric(counts[["A"]]) / denom,
        100 * as.numeric(counts[["T"]]) / denom,
        100 * as.numeric(counts[["C"]]) / denom,
        100 * as.numeric(counts[["G"]]) / denom
    )
}

make_sequence_composition_blob <- function(comp) {
    comp <- comp %||% list()
    src <- comp$counts %||% c(A = 0L, T = 0L, C = 0L, G = 0L)
    if (is.null(names(src))) names(src) <- rep("", length(src))
    counts <- suppressWarnings(as.integer(src[c("A", "T", "C", "G")]))
    counts[!is.finite(counts)] <- 0L
    names(counts) <- c("A", "T", "C", "G")
    fields <- c(
        "##CGV_SEQUENCE_COMPOSITION_V1",
        paste0("length=", as.integer(comp$length %||% 0L)),
        paste0("known_total=", as.integer(comp$known_total %||% sum(counts, na.rm = TRUE))),
        paste0("A=", counts[["A"]]),
        paste0("T=", counts[["T"]]),
        paste0("C=", counts[["C"]]),
        paste0("G=", counts[["G"]]),
        paste0("composition=", utils::URLencode(as.character(comp$composition %||% ""), reserved = TRUE))
    )
    paste(fields, collapse = "\t")
}

is_sequence_composition_blob <- function(blob_text) {
    startsWith(as.character(blob_text %||% ""), "##CGV_SEQUENCE_COMPOSITION_V1\t")
}

parse_sequence_composition_blob <- function(blob_text) {
    txt <- as.character(blob_text %||% "")
    if (!is_sequence_composition_blob(txt)) {
        return(NULL)
    }
    fields <- strsplit(txt, "\t", fixed = TRUE)[[1]]
    kv <- fields[-1]
    vals <- list()
    for (item in kv) {
        pos <- regexpr("=", item, fixed = TRUE)[1]
        if (!is.finite(pos) || pos < 2) next
        key <- substr(item, 1, pos - 1L)
        val <- substr(item, pos + 1L, nchar(item))
        vals[[key]] <- val
    }
    counts <- c(
        A = suppressWarnings(as.integer(vals$A %||% 0L)),
        T = suppressWarnings(as.integer(vals$T %||% 0L)),
        C = suppressWarnings(as.integer(vals$C %||% 0L)),
        G = suppressWarnings(as.integer(vals$G %||% 0L))
    )
    counts[!is.finite(counts)] <- 0L
    composition <- tryCatch(utils::URLdecode(as.character(vals$composition %||% "")), error = function(e) "")
    if (!nzchar(composition)) {
        composition <- format_sequence_composition_from_counts(counts, denominator = suppressWarnings(as.numeric(vals$known_total %||% sum(counts))))
    }
    list(
        composition = composition,
        length = suppressWarnings(as.integer(vals$length %||% sum(counts))),
        known_total = suppressWarnings(as.integer(vals$known_total %||% sum(counts))),
        counts = counts,
        sequence_optional = NULL
    )
}

# --- 2. DETECCIÓN DE ORGANISMO Y GESTIÓN DE GENOMAS ---

detect_organism_from_gff <- function(file_path, original_name = NULL) {
    lines <- tryCatch(readLines(file_path, n = 200, warn = FALSE), error = function(e) character())
    text <- paste(lines, collapse = "\n")

    organism_name <- NULL
    taxid_val <- NULL

    taxid_match <- stringr::str_match(text, stringr::regex("taxon:(\\d+)|taxid\\s*[:=]\\s*(\\d+)", ignore_case = TRUE))
    if (!all(is.na(taxid_match))) {
        taxid_val <- as.integer(na.omit(c(taxid_match[1, 2], taxid_match[1, 3]))[1])
    }

    header_patterns <- c("##species\\s*[:= ]\\s*([^\\n]+)", "##organism\\s*[:= ]\\s*([^\\n]+)", "organism=([^;\\n]+)", "species=([^;\\n]+)")
    for (pat in header_patterns) {
        m <- stringr::str_match(text, stringr::regex(pat, ignore_case = TRUE))
        if (!all(is.na(m)) && nzchar(stringr::str_trim(stringr::str_remove_all(m[1, 2], '["\']')))) {
            raw_value <- stringr::str_trim(stringr::str_remove_all(m[1, 2], '["\']'))
            url_taxid <- stringr::str_match(raw_value, "[?&]id=(\\d+)")
            if (!is.na(url_taxid[1, 2])) {
                if (is.null(taxid_val)) taxid_val <- as.integer(url_taxid[1, 2])
            } else if (!grepl("^https?://", raw_value)) {
                organism_name <- raw_value
            }
            break
        }
    }

    if (!is.null(organism_name) || !is.null(taxid_val)) {
        return(list(organism = organism_name, taxid = taxid_val, source = "header"))
    }

    fname <- tolower(original_name %||% basename(file_path))
    patterns <- list(
        list("Oryza sativa ssp. japonica", c("japonica", "irgsp", "nipponbare")),
        list("Oryza sativa ssp. indica", c("indica")),
        list("Oryza sativa", c("rice", "oryza", "osativa")),
        list("Arabidopsis thaliana", c("arabidopsis", "thaliana", "athaliana")),
        list("Zea mays", c("maize", "corn", "zea", "zmays")),
        list("Brachypodium distachyon", c("brachypodium", "bdistachyon", "stiff_brome")),
        list("Hordeum vulgare", c("barley", "hordeum", "vulgare")),
        list("Sorghum bicolor", c("sorghum", "sbicolor")),
        list("Homo sapiens", c("human", "homo", "hsapiens")),
        list("Mus musculus", c("mouse", "musculus", "mmusculus")),
        list("Saccharomyces cerevisiae", c("yeast", "saccharomyces", "scerevisiae")),
        list("Drosophila melanogaster", c("drosophila", "melanogaster")),
        list("Caenorhabditis elegans", c("elegans", "celegans", "c_elegans")),
        list("Danio rerio", c("zebrafish", "danio", "drerio")),
        list("Candida albicans", c("candida", "albicans")),
        list("Sus scrofa", c("pig", "scrofa", "sscrofa")),
        list("Canis lupus familiaris", c("canine", "dog", "canis")),
        list("Equus caballus", c("horse", "equus", "ecaballus")),
        list("Pan troglodytes", c("chimpanzee", "chimp", "pantroglodytes")),
        list("Vitis vinifera", c("grape", "vitis", "vvinifera")),
        list("Solanum lycopersicum", c("tomato", "solanum", "lycopersicum")),
        list("Phaseolus vulgaris", c("bean", "phaseolus", "pvulgaris")),
        list("Malus domestica", c("apple", "malus", "mdomestica")),
        list("Fragaria vesca", c("strawberry", "fragaria", "fvesca")),
        list("Botrytis cinerea", c("botrytis", "bcinerea")),
        list("Neurospora crassa", c("neurospora", "ncrassa")),
        list("Xenopus laevis", c("xenopus", "xlaevis")),
        list("Desmodus rotundus", c("vampire", "desmodus")),
        list("Gallus gallus", c("chicken", "gallus")),
        list("Rattus norvegicus", c("rat", "rattus", "rnorvegicus")),
        list("Bos taurus", c("cow", "cattle", "bovine", "bos", "btaurus")),
        list("Glycine max", c("soybean", "glycine", "gmax")),
        list("Nicotiana tabacum", c("tobacco", "nicotiana", "ntabacum")),
        list("Gossypium hirsutum", c("cotton", "gossypium")),
        list("Capsicum annuum", c("pepper", "capsicum", "cannuum")),
        list("Citrus sinensis", c("orange", "citrus", "csinensis")),
        list("Ovis aries", c("sheep", "ovis", "oaries")),
        list("Apis mellifera", c("honeybee", "bee", "apis", "amellifera"))
    )
    for (p in patterns) {
        if (any(stringr::str_detect(fname, paste(p[[2]], collapse = "|")))) {
            return(list(organism = p[[1]], taxid = NULL, source = "filename"))
        }
    }

    gcf_pattern <- "GC[FA]_\\d{9}\\.\\d+"
    gcf_match <- stringr::str_extract(fname, stringr::regex(gcf_pattern, ignore_case = TRUE))
    if (!is.na(gcf_match)) {
        return(list(organism = NULL, taxid = NULL, source = "none", assembly_accession = gcf_match))
    }

    list(organism = NULL, taxid = NULL, source = "none")
}

resolve_organism_name_ncbi <- function(taxid) {
    if (is.null(taxid) || is.na(taxid)) return(NULL)
    url <- sprintf(
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=taxonomy&id=%d&retmode=xml",
        as.integer(taxid)
    )
    result <- tryCatch({
        resp <- httr::GET(url, httr::timeout(8))
        if (httr::status_code(resp) != 200) return(NULL)
        body <- httr::content(resp, as = "text", encoding = "UTF-8")
        sci_match <- stringr::str_match(body, "<ScientificName>([^<]+)</ScientificName>")
        if (!is.na(sci_match[1, 2])) {
            trimws(sci_match[1, 2])
        } else {
            NULL
        }
    }, error = function(e) NULL)
    result
}

parse_organism_from_assembly_report <- function(report_path) {
    if (is.null(report_path) || !nzchar(report_path) || !file.exists(report_path)) return(NULL)
    lines <- tryCatch(readLines(report_path, n = 50, warn = FALSE), error = function(e) character())
    org_name <- NULL
    org_taxid <- NULL
    for (ln in lines) {
        if (is.null(org_name) && grepl("^#\\s*Organism name:", ln, ignore.case = TRUE)) {
            name <- sub("^#\\s*Organism name:\\s*", "", ln, ignore.case = TRUE)
            name <- sub("\\s*\\(.*\\)\\s*$", "", name)
            name <- trimws(name)
            if (nzchar(name)) org_name <- name
        }
        if (is.null(org_taxid) && grepl("^#\\s*Taxid:", ln, ignore.case = TRUE)) {
            tid <- sub("^#\\s*Taxid:\\s*", "", ln, ignore.case = TRUE)
            tid <- trimws(tid)
            if (grepl("^\\d+$", tid)) org_taxid <- as.integer(tid)
        }
        if (!is.null(org_name)) break
    }
    if (!is.null(org_name)) {
        result <- org_name
        if (!is.null(org_taxid)) attr(result, "taxid") <- org_taxid
        return(result)
    }
    NULL
}

resolve_organism_cascade <- function(det, assembly_report_path = NULL) {
    org_name <- det$organism
    taxid <- det$taxid
    source <- det$source %||% "none"
    confidence <- "none"

    if (!is.null(assembly_report_path) && nzchar(assembly_report_path) && file.exists(assembly_report_path)) {
        report_name <- parse_organism_from_assembly_report(assembly_report_path)
        if (!is.null(report_name) && nzchar(report_name)) {
            org_name <- report_name
            source <- "assembly_report"
            confidence <- "high"
            return(list(organism = org_name, taxid = taxid, source = source, confidence = confidence))
        }
    }

    if (!is.null(org_name) && nzchar(org_name)) {
        confidence <- if (identical(source, "header")) "high" else "medium"
        return(list(organism = org_name, taxid = taxid, source = source, confidence = confidence))
    }

    if (!is.null(taxid) && !is.na(taxid)) {
        ncbi_name <- resolve_organism_name_ncbi(taxid)
        if (!is.null(ncbi_name) && nzchar(ncbi_name)) {
            return(list(organism = ncbi_name, taxid = taxid, source = "ncbi_taxonomy", confidence = "high"))
        }
    }

    list(organism = org_name, taxid = taxid, source = source, confidence = confidence)
}

normalize_org_key <- function(x) {
    x <- tolower(x %||% "")
    x <- gsub("[^a-z0-9]+", " ", x)
    trimws(x)
}

.genome_registry_cache <- new.env(parent = emptyenv())

get_genome_registry <- function(genomes_dir = "genomes") {
    registry_path <- file.path(genomes_dir, "registry.tsv")
    if (!file.exists(registry_path)) {
        return(data.frame())
    }
    cache_key <- normalizePath(registry_path, winslash = "/", mustWork = FALSE)
    finfo <- tryCatch(file.info(registry_path), error = function(e) NULL)
    mtime_key <- if (!is.null(finfo) && nrow(finfo) > 0L) as.numeric(finfo$mtime[[1]]) else NA_real_
    size_key <- if (!is.null(finfo) && nrow(finfo) > 0L) as.numeric(finfo$size[[1]]) else NA_real_
    cached <- get0(cache_key, envir = .genome_registry_cache, inherits = FALSE, ifnotfound = NULL)
    if (is.list(cached) &&
        identical(cached$mtime, mtime_key) &&
        identical(cached$size, size_key) &&
        is.data.frame(cached$data)) {
        return(cached$data)
    }

    out <- tryCatch(read.delim(registry_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    if (nrow(out) == 0) {
        return(data.frame())
    }
    for (mc in setdiff(c("organism", "taxid", "fasta", "aliases"), colnames(out))) out[[mc]] <- NA_character_
    assign(cache_key, list(mtime = mtime_key, size = size_key, data = out), envir = .genome_registry_cache)
    out
}

resolve_genome_fasta <- function(det_info = NULL, uploaded_fasta_path = NULL, genomes_dir = "genomes") {
    if (!is.null(uploaded_fasta_path) && nzchar(uploaded_fasta_path) && file.exists(uploaded_fasta_path)) {
        return(list(path = uploaded_fasta_path, source = "uploaded", found = TRUE, organism = NULL))
    }

    reg <- get_genome_registry(genomes_dir)
    if (nrow(reg) == 0) {
        return(list(path = NULL, source = "registry_missing", found = FALSE, organism = NULL))
    }

    org <- if (!is.null(det_info)) det_info$organism else NULL
    taxid <- if (!is.null(det_info)) det_info$taxid else NULL
    org_key <- normalize_org_key(org)

    if (!is.null(taxid) && !all(is.na(reg$taxid))) {
        hit <- reg[as.character(reg$taxid) == as.character(taxid), , drop = FALSE]
        if (nrow(hit) > 0) {
            p <- hit$fasta[1]
            p2 <- if (grepl("^/", p) || grepl("^[a-zA-Z]:", p)) p else file.path(genomes_dir, p)
            if (file.exists(p2)) {
                return(list(path = p2, source = "registry_taxid", found = TRUE, organism = hit$organism[1]))
            }
        }
    }

    if (!is.null(org) && nzchar(org_key)) {
        for (i in seq_len(nrow(reg))) {
            keys <- c(reg$organism[i], unlist(strsplit(reg$aliases[i] %||% "", "\\|")))
            norm_keys <- unique(vapply(keys, normalize_org_key, character(1)))
            norm_keys <- norm_keys[nzchar(norm_keys)]
            if (org_key %in% norm_keys || any(stringr::str_detect(org_key, stringr::fixed(norm_keys))) || any(stringr::str_detect(norm_keys, stringr::fixed(org_key)))) {
                p <- reg$fasta[i]
                p2 <- if (grepl("^/", p) || grepl("^[a-zA-Z]:", p)) p else file.path(genomes_dir, p)
                if (file.exists(p2)) {
                    app_debug_log("[Genome Resolve] Match by Name found: ", p2)
                    return(list(path = p2, source = "registry_organism", found = TRUE, organism = reg$organism[i]))
                }
            }
        }
    }
    list(path = NULL, source = "not_found", found = FALSE, organism = NULL)
}

# Operador null-coalescing estricto: devuelve `a` solo si no es NULL,
# tiene longitud > 0, no es todo-NA y el primer elemento no es string vacío.
# Usar cuando se necesita garantizar un valor realmente utilizable (no solo no-NULL).
`%|||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a)) && nzchar(as.character(a[1]))) a else b

# --- 3. CACHÉ Y PARSEO DE GFF ---

.fasta_header_cache <- new.env(parent = emptyenv())
.fasta_seqnames_cache <- new.env(parent = emptyenv())
.fasta_resolved_seqname_cache <- new.env(parent = emptyenv())
.fafile_handle_cache <- new.env(parent = emptyenv())
.fasta_fallback_seq_cache <- new.env(parent = emptyenv())
.spliced_seq_cache <- new.env(parent = emptyenv())
.gff_cache <- new.env(parent = emptyenv())
.seq_extract_cache <- new.env(parent = emptyenv())
.gff_gene_index_cache <- new.env(parent = emptyenv())
.gff_gene_light_index_cache <- new.env(parent = emptyenv())
.partial_gene_suggestion_cache <- new.env(parent = emptyenv())
.gff_genes_table_cache <- new.env(parent = emptyenv())
.gff_genes_chr_index_cache <- new.env(parent = emptyenv())
.gff_chr_length_cache <- new.env(parent = emptyenv())
.preloaded_registry_cache <- new.env(parent = emptyenv())
.twobit_seqinfo_cache <- new.env(parent = emptyenv())
.twobit_handle_cache <- new.env(parent = emptyenv())
.twobit_native_index_cache <- new.env(parent = emptyenv())
.twobit_seqnames_sidecar_version <- 1L
.tabix_index_override_cache <- new.env(parent = emptyenv())
.tabix_seqnames_cache <- new.env(parent = emptyenv())
.transcript_composition_cache <- new.env(parent = emptyenv())
.assembly_report_cache <- new.env(parent = emptyenv())
.assembly_stats_cache <- new.env(parent = emptyenv())
.annotation_report_path_cache <- new.env(parent = emptyenv())
.neighbor_context_cache <- new.env(parent = emptyenv())
.orthologous_local_lookup_cache <- new.env(parent = emptyenv())
.annotation_disk_cache_maintenance <- new.env(parent = emptyenv())
.lastz_disk_cache_maintenance <- new.env(parent = emptyenv())
.gff_autocomplete_cache_validation <- new.env(parent = emptyenv())
.gff_index_cache_version <- "desc-clean-v2"
.gff_autocomplete_cache_version <- 1L

.cache_meta_hidden_key <- ".__cache_meta__"
.cache_access_counter <- 0
.coordinated_memory_cache_state <- new.env(parent = emptyenv())
.coordinated_memory_cache_state$bytes <- 0

clear_preloaded_species_registry_cache <- function() {
    rm(list = ls(envir = .preloaded_registry_cache, all.names = TRUE), envir = .preloaded_registry_cache)
    invisible(TRUE)
}

parse_positive_int_env <- function(var_name, default_value) {
    raw <- trimws(as.character(Sys.getenv(as.character(var_name %||% ""), as.character(default_value)) %||% as.character(default_value)))
    val <- suppressWarnings(as.integer(raw))
    if (!is.finite(val) || is.na(val) || val < 1L) {
        val <- as.integer(default_value %||% 1L)
    }
    val
}

parse_positive_bytes_env_mb <- function(var_name, default_mb) {
    raw <- trimws(as.character(Sys.getenv(as.character(var_name %||% ""), as.character(default_mb)) %||% as.character(default_mb)))
    val <- suppressWarnings(as.numeric(raw))
    if (!is.finite(val) || is.na(val) || val <= 0) {
        val <- as.numeric(default_mb %||% 1)
    }
    as.numeric(val) * 1024^2
}

sanitize_autocomplete_choices <- function(choices, max_total = 20000L) {
    vals <- as.character(choices %||% character(0))
    vals <- gsub("[\u00A0\u2007\u202F]", " ", vals, perl = TRUE)
    vals <- trimws(vals)
    vals <- vals[!is.na(vals) & nzchar(vals)]
    vals <- vals[nchar(vals) <= 80]
    vals <- unique(vals)
    max_cap <- suppressWarnings(as.integer(max_total))
    if (!is.finite(max_cap) || is.na(max_cap) || max_cap <= 0L) {
        max_cap <- 20000L
    }
    if (length(vals) > max_cap) {
        vals <- vals[seq_len(max_cap)]
    }
    vals
}

sanitize_cache_key <- function(x) {
    y <- as.character(x %||% "")
    y <- gsub("[^A-Za-z0-9._-]", "_", y)
    y <- gsub("_+", "_", y)
    if (!nzchar(y)) y <- "cache_key"
    y
}

cache_env_entry_keys <- function(env) {
    setdiff(ls(env, all.names = TRUE), .cache_meta_hidden_key)
}

cache_env_meta_get <- function(env) {
    meta <- get0(.cache_meta_hidden_key, envir = env, inherits = FALSE, ifnotfound = NULL)
    if (!is.list(meta)) {
        meta <- list(access = numeric(0), bytes = numeric(0))
    }
    if (!is.numeric(meta$access)) {
        meta$access <- numeric(0)
    }
    if (!is.numeric(meta$bytes)) {
        meta$bytes <- numeric(0)
    }
    meta
}

cache_env_meta_set <- function(env, meta) {
    assign(.cache_meta_hidden_key, meta, envir = env)
    invisible(meta)
}

cache_meta_named_number <- function(values, key, default = NA_real_) {
    key_txt <- as.character(key %||% "")[1L]
    value_names <- names(values)
    if (!is.numeric(values) || is.na(key_txt) || !nzchar(key_txt) ||
        is.null(value_names) || !key_txt %in% value_names) {
        return(as.numeric(default))
    }
    out <- suppressWarnings(as.numeric(unname(values[key_txt][1L])))
    if (length(out) == 0L) as.numeric(default) else out[[1L]]
}

cache_env_drop <- function(env, key) {
    key_txt <- as.character(key %||% "")
    if (!nzchar(key_txt)) {
        return(invisible(FALSE))
    }
    coordinated <- isTRUE(is_coordinated_memory_cache_env(env))
    meta <- cache_env_meta_get(env)
    has_entry <- exists(key_txt, envir = env, inherits = FALSE)
    entry_bytes <- cache_meta_named_number(meta$bytes, key_txt)
    if (!is.finite(entry_bytes) || is.na(entry_bytes) || entry_bytes < 0) {
        # A legacy/direct assignment is absent from both metadata and the O(1)
        # tracker. Reconcile before subtracting it so unrelated tracked entries
        # are not accidentally charged for this removal.
        if (isTRUE(coordinated) && isTRUE(has_entry)) {
            coordinated_memory_cache_tracked_bytes(recalculate = TRUE)
        }
        entry_bytes <- if (isTRUE(has_entry)) {
            as.numeric(utils::object.size(get(key_txt, envir = env, inherits = FALSE)))
        } else {
            0
        }
    }
    if (isTRUE(has_entry)) {
        rm(list = key_txt, envir = env)
    }
    meta$access <- meta$access[setdiff(names(meta$access), key_txt)]
    meta$bytes <- meta$bytes[setdiff(names(meta$bytes), key_txt)]
    cache_env_meta_set(env, meta)
    if (isTRUE(coordinated)) {
        coordinated_memory_cache_adjust_bytes(-entry_bytes)
    }
    invisible(TRUE)
}

cache_env_touch <- function(env, key, bytes = NA_real_) {
    key_txt <- as.character(key %||% "")
    if (!nzchar(key_txt)) {
        return(invisible(NULL))
    }
    meta <- cache_env_meta_get(env)
    access <- meta$access
    bytes_map <- meta$bytes
    discovered_legacy_bytes <- FALSE
    next_access <- suppressWarnings(as.numeric(.cache_access_counter %||% 0))
    if (!is.finite(next_access) || is.na(next_access) || next_access < 0) {
        next_access <- 0
    }
    next_access <- next_access + 1
    .cache_access_counter <<- next_access
    access[key_txt] <- next_access
    if (is.finite(bytes)) {
        bytes_map[key_txt] <- as.numeric(bytes)
    } else if (!key_txt %in% names(bytes_map) && exists(key_txt, envir = env, inherits = FALSE)) {
        bytes_map[key_txt] <- as.numeric(utils::object.size(get(key_txt, envir = env, inherits = FALSE)))
        discovered_legacy_bytes <- TRUE
    }
    meta$access <- access
    meta$bytes <- bytes_map
    cache_env_meta_set(env, meta)
    if (isTRUE(discovered_legacy_bytes) && isTRUE(is_coordinated_memory_cache_env(env))) {
        # The direct/legacy value was not part of the incremental tracker.
        coordinated_memory_cache_tracked_bytes(recalculate = TRUE)
    }
    invisible(NULL)
}

cache_env_get <- function(env, key, default = NULL) {
    key_txt <- as.character(key %||% "")
    if (!nzchar(key_txt) || !exists(key_txt, envir = env, inherits = FALSE)) {
        return(default)
    }
    value <- get(key_txt, envir = env, inherits = FALSE)
    cache_env_touch(env, key_txt)
    value
}

cache_env_set <- function(env, key, value, max_size = NULL, max_bytes = NULL) {
    key_txt <- as.character(key %||% "")
    if (!nzchar(key_txt)) {
        return(invisible(value))
    }
    coordinated <- isTRUE(is_coordinated_memory_cache_env(env))
    old_bytes <- 0
    if (isTRUE(coordinated) && exists(key_txt, envir = env, inherits = FALSE)) {
        old_meta <- cache_env_meta_get(env)
        old_bytes <- cache_meta_named_number(old_meta$bytes, key_txt)
        if (!is.finite(old_bytes) || is.na(old_bytes) || old_bytes < 0) {
            # Direct assignments pre-dating the coordinated helper were never
            # added to the incremental tracker. Reconcile once before replacing.
            coordinated_memory_cache_tracked_bytes(recalculate = TRUE)
            old_bytes <- as.numeric(utils::object.size(get(key_txt, envir = env, inherits = FALSE)))
        }
    }
    value_bytes <- as.numeric(utils::object.size(value))
    assign(key_txt, value, envir = env)
    cache_env_touch(env, key_txt, bytes = value_bytes)
    if (isTRUE(coordinated)) {
        # Account for the replacement before local trimming. If the just-written
        # entry (or another entry) is then evicted, trim_cache_env subtracts the
        # corresponding bytes exactly once.
        coordinated_memory_cache_adjust_bytes(value_bytes - old_bytes)
    }
    trim_cache_env(env, max_size = max_size %||% Inf, max_bytes = max_bytes)
    if (isTRUE(coordinated)) {
        enforce_coordinated_memory_cache_budget()
    }
    invisible(value)
}

# Poda un cache de entorno cuando supera max_size o max_bytes.
# Cuando el entorno mantiene metadatos de acceso, elimina primero las entradas menos usadas recientemente.
trim_cache_env <- function(env, max_size = 1000L, max_bytes = NULL) {
    entries <- cache_env_entry_keys(env)
    n <- length(entries)
    max_size_int <- suppressWarnings(as.integer(max_size %||% 1000L))
    if (!is.finite(max_size_int) || is.na(max_size_int) || max_size_int < 1L) {
        max_size_int <- Inf
    }
    max_bytes_num <- suppressWarnings(as.numeric(max_bytes %||% NA_real_))
    if (!is.finite(max_bytes_num) || is.na(max_bytes_num) || max_bytes_num <= 0) {
        max_bytes_num <- NA_real_
    }

    if (n == 0L) {
        cache_env_meta_set(env, list(access = numeric(0), bytes = numeric(0)))
        return(invisible(NULL))
    }

    meta <- cache_env_meta_get(env)
    access <- meta$access
    bytes_map <- meta$bytes
    missing_access <- setdiff(entries, names(access))
    if (length(missing_access) > 0L) {
        seed_vals <- seq_len(length(missing_access))
        names(seed_vals) <- missing_access
        access <- c(access, seed_vals)
    }
    missing_bytes <- setdiff(entries, names(bytes_map))
    if (length(missing_bytes) > 0L) {
        computed <- vapply(missing_bytes, function(k) {
            if (!exists(k, envir = env, inherits = FALSE)) {
                return(0)
            }
            as.numeric(utils::object.size(get(k, envir = env, inherits = FALSE)))
        }, numeric(1))
        bytes_map <- c(bytes_map, computed)
    }
    access <- access[entries]
    bytes_map <- bytes_map[entries]

    total_bytes <- sum(as.numeric(bytes_map), na.rm = TRUE)
    over_size <- is.finite(max_size_int) && n > max_size_int
    over_bytes <- is.finite(max_bytes_num) && total_bytes > max_bytes_num
    if (over_size || over_bytes) {
        order_access <- order(as.numeric(access), na.last = TRUE)
        drop_keys <- character(0)
        keep_n <- n
        keep_bytes <- total_bytes
        for (idx in order_access) {
            if (!(is.finite(max_size_int) && keep_n > max_size_int) && !(is.finite(max_bytes_num) && keep_bytes > max_bytes_num)) {
                break
            }
            key_drop <- entries[[idx]]
            drop_keys <- c(drop_keys, key_drop)
            keep_n <- keep_n - 1L
            keep_bytes <- keep_bytes - as.numeric(bytes_map[[idx]] %||% 0)
        }
        if (length(drop_keys) > 0L) {
            dropped_bytes <- sum(as.numeric(bytes_map[drop_keys]), na.rm = TRUE)
            rm(list = drop_keys, envir = env)
            entries <- setdiff(entries, drop_keys)
            access <- access[setdiff(names(access), drop_keys)]
            bytes_map <- bytes_map[setdiff(names(bytes_map), drop_keys)]
            if (isTRUE(is_coordinated_memory_cache_env(env))) {
                coordinated_memory_cache_adjust_bytes(-dropped_bytes)
            }
        }
    }

    access <- access[entries]
    bytes_map <- bytes_map[entries]
    cache_env_meta_set(env, list(access = access, bytes = bytes_map))
    invisible(NULL)
}

# Process-wide share of the coordinated cache budget. The web configuration treats
# APP_MEMORY_CACHE_BUDGET_MB as a total for the main R process and its persistent
# future workers; each process enforces its propagated share independently.
# Session-local caches, file handles and SQLite connections remain outside it.
coordinated_memory_cache_envs <- function() {
    list(
        gff = .gff_cache,
        gene_index = .gff_gene_index_cache,
        gene_light = .gff_gene_light_index_cache,
        genes_table = .gff_genes_table_cache,
        genes_chr = .gff_genes_chr_index_cache,
        seq_extract = .seq_extract_cache,
        spliced_seq = .spliced_seq_cache,
        fasta_fallback = .fasta_fallback_seq_cache,
        transcript_composition = .transcript_composition_cache,
        orthologous_local = .orthologous_local_lookup_cache
    )
}

is_coordinated_memory_cache_env <- function(env, cache_envs = coordinated_memory_cache_envs()) {
    if (!is.environment(env) || !is.list(cache_envs) || length(cache_envs) == 0L) {
        return(FALSE)
    }
    any(vapply(cache_envs, function(candidate) {
        is.environment(candidate) && identical(env, candidate)
    }, logical(1)))
}

coordinated_memory_cache_adjust_bytes <- function(delta) {
    current <- suppressWarnings(as.numeric(.coordinated_memory_cache_state$bytes %||% NA_real_))
    if (!is.finite(current) || is.na(current) || current < 0) {
        current <- sum(vapply(coordinated_memory_cache_envs(), cache_env_usage_bytes, numeric(1)), na.rm = TRUE)
    }
    delta_num <- suppressWarnings(as.numeric(delta %||% 0))
    if (!is.finite(delta_num) || is.na(delta_num)) delta_num <- 0
    .coordinated_memory_cache_state$bytes <- max(0, current + delta_num)
    invisible(.coordinated_memory_cache_state$bytes)
}

coordinated_memory_cache_tracked_bytes <- function(recalculate = FALSE) {
    current <- suppressWarnings(as.numeric(.coordinated_memory_cache_state$bytes %||% NA_real_))
    if (isTRUE(recalculate) || !is.finite(current) || is.na(current) || current < 0) {
        current <- sum(vapply(coordinated_memory_cache_envs(), cache_env_usage_bytes, numeric(1)), na.rm = TRUE)
        .coordinated_memory_cache_state$bytes <- current
    }
    as.numeric(current)
}

get_coordinated_memory_cache_total_budget_mb <- function(
    runtime = Sys.getenv("CGV_RUNTIME", ""),
    raw_value = Sys.getenv("APP_MEMORY_CACHE_BUDGET_MB", "")
) {
    runtime_txt <- tolower(trimws(as.character(runtime %||% ""))[1L])
    default_mb <- if (identical(runtime_txt, "desktop")) 1024 else 384
    parsed <- suppressWarnings(as.numeric(trimws(as.character(raw_value %||% ""))[1L]))
    if (!is.finite(parsed) || is.na(parsed) || parsed <= 0) {
        parsed <- default_mb
    }
    # Prevent accidental values that either disable useful caching or let cache
    # payloads consume essentially all memory on a large workstation.
    min(8192, max(32, parsed))
}

get_coordinated_memory_cache_process_count <- function(
    runtime = Sys.getenv("CGV_RUNTIME", ""),
    future_mode = Sys.getenv("APP_FUTURE_MODE", "sequential"),
    raw_process_count = Sys.getenv("APP_MEMORY_CACHE_PROCESS_COUNT", ""),
    raw_workers = Sys.getenv("APP_FUTURE_WORKERS", "2")
) {
    runtime_txt <- tolower(trimws(as.character(runtime %||% ""))[1L])
    if (identical(runtime_txt, "desktop")) return(1L)

    mode_txt <- tolower(trimws(as.character(future_mode %||% "sequential"))[1L])
    if (!identical(mode_txt, "multisession")) return(1L)

    process_count <- suppressWarnings(as.integer(trimws(as.character(raw_process_count %||% ""))[1L]))
    if (!is.finite(process_count) || is.na(process_count) || process_count < 1L) {
        workers <- suppressWarnings(as.integer(trimws(as.character(raw_workers %||% "2"))[1L]))
        if (!is.finite(workers) || is.na(workers) || workers < 1L) workers <- 2L
        workers <- as.integer(min(32L, max(1L, workers)))
        process_count <- workers + 1L
    }
    as.integer(min(33L, max(1L, process_count)))
}

get_coordinated_memory_cache_budget_mb <- function(
    runtime = Sys.getenv("CGV_RUNTIME", ""),
    raw_value = Sys.getenv("APP_MEMORY_CACHE_BUDGET_MB", ""),
    future_mode = Sys.getenv("APP_FUTURE_MODE", "sequential"),
    raw_process_count = Sys.getenv("APP_MEMORY_CACHE_PROCESS_COUNT", ""),
    raw_workers = Sys.getenv("APP_FUTURE_WORKERS", "2")
) {
    total_mb <- get_coordinated_memory_cache_total_budget_mb(
        runtime = runtime,
        raw_value = raw_value
    )
    runtime_txt <- tolower(trimws(as.character(runtime %||% ""))[1L])
    if (identical(runtime_txt, "desktop")) return(total_mb)
    process_count <- get_coordinated_memory_cache_process_count(
        runtime = runtime,
        future_mode = future_mode,
        raw_process_count = raw_process_count,
        raw_workers = raw_workers
    )
    as.numeric(total_mb) / as.numeric(process_count)
}

get_coordinated_memory_cache_budget_bytes <- function(...) {
    as.numeric(get_coordinated_memory_cache_budget_mb(...)) * 1024^2
}

cache_env_usage_rows <- function(env, cache_name = "cache") {
    if (!is.environment(env)) {
        return(data.frame(
            cache = character(0), key = character(0), access = numeric(0),
            bytes = numeric(0), stringsAsFactors = FALSE
        ))
    }
    entries <- cache_env_entry_keys(env)
    if (length(entries) == 0L) {
        return(data.frame(
            cache = character(0), key = character(0), access = numeric(0),
            bytes = numeric(0), stringsAsFactors = FALSE
        ))
    }
    meta <- cache_env_meta_get(env)
    access <- suppressWarnings(as.numeric(meta$access[entries]))
    bytes <- suppressWarnings(as.numeric(meta$bytes[entries]))
    missing_bytes <- !is.finite(bytes) | is.na(bytes) | bytes < 0
    if (any(missing_bytes)) {
        bytes[missing_bytes] <- vapply(entries[missing_bytes], function(key) {
            if (!exists(key, envir = env, inherits = FALSE)) return(0)
            as.numeric(utils::object.size(get(key, envir = env, inherits = FALSE)))
        }, numeric(1))
    }
    # Entries written outside cache_env_set have no access metadata. Treat them
    # as oldest so they cannot make coordinated caches grow without bound.
    access[!is.finite(access) | is.na(access)] <- -Inf
    data.frame(
        cache = rep(as.character(cache_name %||% "cache"), length(entries)),
        key = entries,
        access = access,
        bytes = bytes,
        stringsAsFactors = FALSE
    )
}

cache_env_usage_bytes <- function(env) {
    if (!is.environment(env)) return(0)
    entries <- cache_env_entry_keys(env)
    if (length(entries) == 0L) return(0)
    meta <- cache_env_meta_get(env)
    bytes <- suppressWarnings(as.numeric(meta$bytes[entries]))
    missing_bytes <- !is.finite(bytes) | is.na(bytes) | bytes < 0
    if (any(missing_bytes)) {
        bytes[missing_bytes] <- vapply(entries[missing_bytes], function(key) {
            if (!exists(key, envir = env, inherits = FALSE)) return(0)
            as.numeric(utils::object.size(get(key, envir = env, inherits = FALSE)))
        }, numeric(1))
    }
    sum(bytes, na.rm = TRUE)
}

coordinated_memory_cache_stats <- function(
    cache_envs = coordinated_memory_cache_envs(),
    budget_bytes = get_coordinated_memory_cache_budget_bytes()
) {
    if (!is.list(cache_envs)) cache_envs <- list()
    cache_names <- names(cache_envs)
    if (is.null(cache_names)) cache_names <- rep("", length(cache_envs))
    blank_names <- is.na(cache_names) | !nzchar(cache_names)
    cache_names[blank_names] <- paste0("cache_", which(blank_names))
    rows <- lapply(seq_along(cache_envs), function(i) {
        cache_env_usage_rows(cache_envs[[i]], cache_name = cache_names[[i]])
    })
    nonempty_rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
    entries <- if (length(nonempty_rows) > 0L) {
        do.call(rbind, nonempty_rows)
    } else {
        data.frame(
            cache = character(0), key = character(0), access = numeric(0),
            bytes = numeric(0), stringsAsFactors = FALSE
        )
    }
    by_cache <- data.frame(
        cache = cache_names,
        entries = vapply(cache_envs, function(env) {
            if (is.environment(env)) length(cache_env_entry_keys(env)) else 0L
        }, integer(1)),
        bytes = vapply(seq_along(cache_envs), function(i) {
            if (nrow(rows[[i]]) == 0L) 0 else sum(rows[[i]]$bytes, na.rm = TRUE)
        }, numeric(1)),
        stringsAsFactors = FALSE
    )
    list(
        budget_bytes = suppressWarnings(as.numeric(budget_bytes %||% NA_real_)),
        total_bytes = sum(entries$bytes, na.rm = TRUE),
        total_entries = nrow(entries),
        by_cache = by_cache,
        entries = entries
    )
}

enforce_coordinated_memory_cache_budget <- function(
    budget_bytes = get_coordinated_memory_cache_budget_bytes(),
    cache_envs = NULL
) {
    budget <- suppressWarnings(as.numeric(budget_bytes %||% NA_real_))
    is_default_registry <- is.null(cache_envs)
    if (isTRUE(is_default_registry)) cache_envs <- coordinated_memory_cache_envs()
    total_bytes <- if (isTRUE(is_default_registry)) {
        coordinated_memory_cache_tracked_bytes()
    } else if (is.list(cache_envs) && length(cache_envs) > 0L) {
        sum(vapply(cache_envs, cache_env_usage_bytes, numeric(1)), na.rm = TRUE)
    } else {
        0
    }
    if (!is.finite(budget) || is.na(budget) || budget <= 0 || total_bytes <= budget) {
        return(invisible(list(
            evicted = data.frame(cache = character(0), key = character(0), bytes = numeric(0), stringsAsFactors = FALSE),
            before_bytes = total_bytes,
            after_bytes = total_bytes,
            budget_bytes = budget
        )))
    }

    # Building the cross-cache entry ranking is intentionally deferred until
    # the cheap byte sum above proves that eviction is necessary.
    before <- coordinated_memory_cache_stats(cache_envs = cache_envs, budget_bytes = budget)
    if (isTRUE(is_default_registry)) {
        .coordinated_memory_cache_state$bytes <- before$total_bytes
    }
    entries <- before$entries
    entries$.row_order <- seq_len(nrow(entries))
    entries <- entries[order(entries$access, entries$.row_order, na.last = TRUE), , drop = FALSE]
    cache_names <- names(cache_envs)
    if (is.null(cache_names)) cache_names <- rep("", length(cache_envs))
    blank_names <- is.na(cache_names) | !nzchar(cache_names)
    cache_names[blank_names] <- paste0("cache_", which(blank_names))
    names(cache_envs) <- cache_names

    remaining <- before$total_bytes
    evicted <- list()
    evicted_n <- 0L
    for (i in seq_len(nrow(entries))) {
        if (remaining <= budget) break
        cache_name <- entries$cache[[i]]
        key <- entries$key[[i]]
        env <- cache_envs[[cache_name]]
        if (!is.environment(env) || !exists(key, envir = env, inherits = FALSE)) next
        cache_env_drop(env, key)
        entry_bytes <- suppressWarnings(as.numeric(entries$bytes[[i]] %||% 0))
        if (!is.finite(entry_bytes) || is.na(entry_bytes) || entry_bytes < 0) entry_bytes <- 0
        remaining <- max(0, remaining - entry_bytes)
        evicted_n <- evicted_n + 1L
        evicted[[evicted_n]] <- data.frame(
            cache = cache_name,
            key = key,
            bytes = entry_bytes,
            stringsAsFactors = FALSE
        )
    }
    evicted_df <- if (length(evicted) > 0L) do.call(rbind, evicted) else data.frame(
        cache = character(0), key = character(0), bytes = numeric(0), stringsAsFactors = FALSE
    )
    if (isTRUE(is_default_registry)) {
        # cache_env_drop already performs the incremental subtraction. Reconcile
        # with the independently computed remainder to avoid cumulative drift.
        .coordinated_memory_cache_state$bytes <- remaining
    }
    invisible(list(
        evicted = evicted_df,
        before_bytes = before$total_bytes,
        after_bytes = remaining,
        budget_bytes = budget
    ))
}

read_memory_control_text <- function(path) {
    p <- as.character(path %||% "")[1L]
    if (!nzchar(p) || !file.exists(p) || file.access(p, mode = 4L) != 0L) return("")
    out <- tryCatch(readLines(p, n = 1L, warn = FALSE), error = function(e) character(0))
    if (length(out) == 0L) "" else trimws(as.character(out[[1L]] %||% ""))
}

parse_memory_control_bytes <- function(value) {
    txt <- trimws(as.character(value %||% "")[1L])
    if (!grepl("^[0-9]+$", txt)) return(NA_real_)
    parsed <- suppressWarnings(as.numeric(txt))
    if (!is.finite(parsed) || is.na(parsed) || parsed < 0) NA_real_ else parsed
}

read_cgroup_memory_stats <- function(
    v2_current_path = "/sys/fs/cgroup/memory.current",
    v2_max_path = "/sys/fs/cgroup/memory.max",
    v1_usage_path = "/sys/fs/cgroup/memory/memory.usage_in_bytes",
    v1_limit_path = "/sys/fs/cgroup/memory/memory.limit_in_bytes"
) {
    current_v2_txt <- read_memory_control_text(v2_current_path)
    limit_v2_txt <- read_memory_control_text(v2_max_path)
    current_v2 <- parse_memory_control_bytes(current_v2_txt)
    v2_unlimited <- identical(tolower(limit_v2_txt), "max")
    limit_v2 <- if (isTRUE(v2_unlimited)) NA_real_ else parse_memory_control_bytes(limit_v2_txt)
    if (is.finite(current_v2) || is.finite(limit_v2) || isTRUE(v2_unlimited)) {
        ratio <- if (is.finite(current_v2) && is.finite(limit_v2) && limit_v2 > 0) current_v2 / limit_v2 else NA_real_
        return(list(
            version = "v2", current_bytes = current_v2, limit_bytes = limit_v2,
            unlimited = isTRUE(v2_unlimited), usage_ratio = ratio
        ))
    }

    current_v1 <- parse_memory_control_bytes(read_memory_control_text(v1_usage_path))
    limit_v1 <- parse_memory_control_bytes(read_memory_control_text(v1_limit_path))
    # Linux commonly exposes a very large sentinel instead of infinity in v1.
    v1_unlimited <- is.finite(limit_v1) && limit_v1 >= 2^60
    if (isTRUE(v1_unlimited)) limit_v1 <- NA_real_
    if (is.finite(current_v1) || is.finite(limit_v1) || isTRUE(v1_unlimited)) {
        ratio <- if (is.finite(current_v1) && is.finite(limit_v1) && limit_v1 > 0) current_v1 / limit_v1 else NA_real_
        return(list(
            version = "v1", current_bytes = current_v1, limit_bytes = limit_v1,
            unlimited = isTRUE(v1_unlimited), usage_ratio = ratio
        ))
    }
    list(version = "none", current_bytes = NA_real_, limit_bytes = NA_real_, unlimited = FALSE, usage_ratio = NA_real_)
}

read_process_rss_bytes <- function(status_path = "/proc/self/status", allow_ps_fallback = TRUE) {
    p <- as.character(status_path %||% "")[1L]
    if (nzchar(p) && file.exists(p) && file.access(p, mode = 4L) == 0L) {
        lines <- tryCatch(readLines(p, warn = FALSE), error = function(e) character(0))
        rss_line <- lines[grepl("^VmRSS:[[:space:]]*[0-9]+[[:space:]]+kB", lines)]
        if (length(rss_line) > 0L) {
            rss_kb <- suppressWarnings(as.numeric(sub(
                "^VmRSS:[[:space:]]*([0-9]+)[[:space:]]+kB.*$", "\\1", rss_line[[1L]]
            )))
            if (is.finite(rss_kb) && !is.na(rss_kb) && rss_kb >= 0) return(rss_kb * 1024)
        }
    }
    if (!isTRUE(allow_ps_fallback)) return(NA_real_)
    rss_kb <- tryCatch({
        out <- suppressWarnings(system2(
            "ps", c("-o", "rss=", "-p", as.character(Sys.getpid())),
            stdout = TRUE, stderr = FALSE
        ))
        suppressWarnings(as.numeric(trimws(as.character(out[[1L]] %||% ""))))
    }, error = function(e) NA_real_)
    if (!is.finite(rss_kb) || is.na(rss_kb) || rss_kb < 0) NA_real_ else rss_kb * 1024
}

app_memory_telemetry_snapshot <- function(
    cache_envs = coordinated_memory_cache_envs(),
    budget_bytes = get_coordinated_memory_cache_budget_bytes(),
    ...
) {
    cache <- coordinated_memory_cache_stats(cache_envs = cache_envs, budget_bytes = budget_bytes)
    cgroup <- read_cgroup_memory_stats(...)
    list(
        pid = as.integer(Sys.getpid()),
        runtime = tolower(trimws(as.character(Sys.getenv("CGV_RUNTIME", "web") %||% "web"))[1L]),
        rss_bytes = read_process_rss_bytes(),
        cgroup_version = cgroup$version,
        cgroup_current_bytes = cgroup$current_bytes,
        cgroup_limit_bytes = cgroup$limit_bytes,
        cgroup_usage_ratio = cgroup$usage_ratio,
        cache_bytes = cache$total_bytes,
        cache_entries = cache$total_entries,
        cache_budget_bytes = cache$budget_bytes,
        cache_total_budget_bytes = get_coordinated_memory_cache_total_budget_mb() * 1024^2,
        cache_process_count = get_coordinated_memory_cache_process_count(),
        cache_by_name = cache$by_cache
    )
}

annotation_memory_cache_limits <- list(
    gff_max_entries = parse_positive_int_env("APP_GFF_CACHE_MAX_ENTRIES", 2L),
    gff_max_bytes = parse_positive_bytes_env_mb("APP_GFF_CACHE_MAX_MB", 900),
    gene_index_max_entries = parse_positive_int_env("APP_GFF_GENE_INDEX_CACHE_MAX_ENTRIES", 6L),
    gene_index_max_bytes = parse_positive_bytes_env_mb("APP_GFF_GENE_INDEX_CACHE_MAX_MB", 1200),
    gene_light_max_entries = parse_positive_int_env("APP_GFF_GENE_LIGHT_CACHE_MAX_ENTRIES", 24L),
    gene_light_max_bytes = parse_positive_bytes_env_mb("APP_GFF_GENE_LIGHT_CACHE_MAX_MB", 650),
    genes_table_max_entries = parse_positive_int_env("APP_GFF_GENES_TABLE_CACHE_MAX_ENTRIES", 24L),
    genes_table_max_bytes = parse_positive_bytes_env_mb("APP_GFF_GENES_TABLE_CACHE_MAX_MB", 300),
    genes_chr_index_max_entries = parse_positive_int_env("APP_GFF_GENES_CHR_INDEX_CACHE_MAX_ENTRIES", 48L),
    genes_chr_index_max_bytes = parse_positive_bytes_env_mb("APP_GFF_GENES_CHR_INDEX_CACHE_MAX_MB", 160),
    seq_extract_max_entries = 1000L,
    seq_extract_max_bytes = parse_positive_bytes_env_mb(
        "APP_SEQ_EXTRACT_CACHE_MAX_MB",
        if (identical(tolower(trimws(Sys.getenv("CGV_RUNTIME", ""))), "desktop")) 256 else 96
    ),
    spliced_seq_max_entries = 1200L,
    spliced_seq_max_bytes = parse_positive_bytes_env_mb(
        "APP_SPLICED_SEQ_CACHE_MAX_MB",
        if (identical(tolower(trimws(Sys.getenv("CGV_RUNTIME", ""))), "desktop")) 192 else 64
    ),
    fasta_fallback_seq_max_entries = parse_positive_int_env("APP_FASTA_FALLBACK_SEQ_CACHE_MAX_ENTRIES", 8L),
    fasta_fallback_seq_max_bytes = parse_positive_bytes_env_mb("APP_FASTA_FALLBACK_SEQ_CACHE_MAX_MB", 96),
    fasta_fallback_seq_max_bp = parse_positive_int_env("APP_FASTA_FALLBACK_SEQ_CACHE_MAX_BP", 5000000L)
)

# Convert genomic feature coordinates into transcript-order display coordinates.
# Plus-strand transcripts stay left-to-right by genomic start.
# Minus-strand transcripts are mirrored so exon 1 is displayed on the left.
compute_transcript_relative_coords <- function(df, strand = "+", anchor_df = NULL) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
        return(list(rel_start = numeric(0), rel_end = numeric(0), origin = NA_real_))
    }

    base_df <- if (!is.null(anchor_df) && is.data.frame(anchor_df) && nrow(anchor_df) > 0L) {
        anchor_df
    } else {
        df
    }

    xs <- suppressWarnings(as.numeric(df$xstart))
    xe <- suppressWarnings(as.numeric(df$xend))
    base_xs <- suppressWarnings(as.numeric(base_df$xstart))
    base_xe <- suppressWarnings(as.numeric(base_df$xend))

    base_min <- suppressWarnings(min(base_xs, na.rm = TRUE))
    base_max <- suppressWarnings(max(base_xe, na.rm = TRUE))
    strand_one <- trimws(as.character(strand %||% "+"))[1L]
    if (!strand_one %in% c("+", "-")) strand_one <- "+"

    if (identical(strand_one, "-") && is.finite(base_max)) {
        rel_start <- base_max - xe
        rel_end <- base_max - xs
        origin <- base_max
    } else {
        if (!is.finite(base_min)) base_min <- suppressWarnings(min(xs, na.rm = TRUE))
        if (!is.finite(base_min)) base_min <- 0
        rel_start <- xs - base_min
        rel_end <- xe - base_min
        origin <- base_min
    }

    list(
        rel_start = pmin(rel_start, rel_end),
        rel_end = pmax(rel_start, rel_end),
        origin = origin
    )
}

adjust_color_dark <- function(hex_color) {
    if (is.null(hex_color) || !is.character(hex_color) || nchar(hex_color) != 7) {
        return(hex_color)
    }
    r <- as.integer(substr(hex_color, 2L, 3L), base = 16L)
    g <- as.integer(substr(hex_color, 4L, 5L), base = 16L)
    b <- as.integer(substr(hex_color, 6L, 7L), base = 16L)
    factor <- 0.75
    r_new <- as.integer(r * factor)
    g_new <- as.integer(g * factor)
    b_new <- as.integer(b * factor)
    sprintf("#%02X%02X%02X", r_new, g_new, b_new)
}

get_transcript_feature_palette <- function(is_dark_theme = FALSE, is_colorblind_mode = FALSE, custom_overrides = NULL) {
    is_dark_theme <- isTRUE(is_dark_theme)
    is_colorblind_mode <- isTRUE(is_colorblind_mode)

    if (is_colorblind_mode) {
        if (is_dark_theme) {
            palette <- c(
                "compact" = "#56B4E9",
                "gene" = "#56B4E9",
                "exon" = "#56B4E9",
                "cds" = "#009E73",
                "utr" = "#CC79A7",
                "codon" = "#F0E442",
                "other" = "#D55E00"
            )
        } else {
            palette <- c(
                "compact" = "#0072B2",
                "gene" = "#0072B2",
                "exon" = "#0072B2",
                "cds" = "#009E73",
                "utr" = "#CC79A7",
                "codon" = "#E69F00",
                "other" = "#56B4E9"
            )
        }
    } else if (is_dark_theme) {
        palette <- c(
            "compact" = "#FF7B8F",
            "gene" = "#FF9DAF",
            "exon" = "#FF6881",
            "cds" = "#F4B36A",
            "utr" = "#55C7E8",
            "codon" = "#B89CFF",
            "other" = "#8297AC"
        )
    } else {
        palette <- c(
            "compact" = "#F7687C",
            "gene" = "#FFB7BF",
            "exon" = "#F45D75",
            "cds" = "#E8A44F",
            "utr" = "#5BC0EB",
            "codon" = "#7CCFB8",
            "other" = "#B9C1C9"
        )
    }

    if (!is.null(custom_overrides) && is.list(custom_overrides)) {
        for (name in names(custom_overrides)) {
            if (!is.null(custom_overrides[[name]]) && is.character(custom_overrides[[name]]) && nchar(custom_overrides[[name]]) == 7) {
                palette[name] <- custom_overrides[[name]]
            }
        }
    }

    palette
}

compute_spliced_feature_relative_coords <- function(df, strand = "+", gap = 14) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
        return(list(rel_start = numeric(0), rel_end = numeric(0)))
    }

    xs <- suppressWarnings(as.numeric(df$xstart))
    xe <- suppressWarnings(as.numeric(df$xend))
    strand_one <- trimws(as.character(strand %||% "+"))[1L]
    if (!strand_one %in% c("+", "-")) strand_one <- "+"

    ord <- if (identical(strand_one, "-")) order(-xs, -xe, na.last = TRUE) else order(xs, xe, na.last = TRUE)
    rel_start <- rep(NA_real_, nrow(df))
    rel_end <- rep(NA_real_, nrow(df))
    cursor <- 0
    gap_val <- max(0, suppressWarnings(as.numeric(gap %||% 0)))

    for (idx in ord) {
        if (!is.finite(xs[idx]) || !is.finite(xe[idx])) next
        width_val <- abs(xe[idx] - xs[idx])
        if (!is.finite(width_val) || width_val <= 0) width_val <- 1
        rel_start[idx] <- cursor
        rel_end[idx] <- cursor + width_val
        cursor <- rel_end[idx] + gap_val
    }

    list(
        rel_start = pmin(rel_start, rel_end),
        rel_end = pmax(rel_start, rel_end)
    )
}

get_annotation_disk_cache_dir <- function(base_dir = ".") {
    override <- app_env_path("APP_ANNOTATION_DISK_CACHE_DIR", "")
    if (nzchar(override)) {
        return(override)
    }
    normalizePath(file.path(get_cgv_cache_root(base_dir), "annotation_index"), winslash = "/", mustWork = FALSE)
}

normalize_cache_filename_family <- function(fname) {
    nm <- basename(as.character(fname %||% ""))
    nm <- sub("\\.rds$", "", nm, ignore.case = TRUE)
    nm <- sub("^gene_light__[^_]+__", "", nm)
    nm <- sub("_desc-clean-v[0-9]+$", "", nm)
    nm <- gsub("_[0-9]+(?:\\.[0-9]+)?$", "", nm, perl = TRUE)
    nm <- gsub("_[0-9]+_[0-9]+$", "", nm, perl = TRUE)
    nm
}

detect_annotation_cache_family <- function(fname, known_annotation_basenames = character(0)) {
    nm <- basename(as.character(fname %||% ""))
    stem <- sub("\\.rds$", "", nm, ignore.case = TRUE)
    kind <- sub("__.*$", "", stem)
    sanitized_known <- vapply(as.character(known_annotation_basenames %||% character(0)), sanitize_cache_key, character(1))
    sanitized_known <- unique(sanitized_known[nzchar(sanitized_known)])
    if (length(sanitized_known) > 0L) {
        hits <- sanitized_known[vapply(sanitized_known, function(tok) grepl(tok, stem, fixed = TRUE), logical(1))]
        if (length(hits) > 0L) {
            hits <- hits[order(nchar(hits), decreasing = TRUE)]
            return(paste(kind, hits[[1]], sep = "::"))
        }
    }
    paste(kind, normalize_cache_filename_family(fname), sep = "::")
}

maintain_annotation_disk_cache <- function(base_dir = ".") {
    cdir <- get_annotation_disk_cache_dir(base_dir = base_dir)
    if (!dir.exists(cdir)) {
        return(invisible(NULL))
    }

    maintain_key <- normalizePath(cdir, winslash = "/", mustWork = FALSE)
    if (isTRUE(get0(maintain_key, envir = .annotation_disk_cache_maintenance, inherits = FALSE, ifnotfound = FALSE))) {
        return(invisible(NULL))
    }

    files <- list.files(cdir, pattern = "\\.rds$", full.names = TRUE)
    if (length(files) == 0L) {
        assign(maintain_key, TRUE, envir = .annotation_disk_cache_maintenance)
        return(invisible(NULL))
    }

    annotation_dir <- file.path(base_dir, "annotations")
    known_basenames <- if (dir.exists(annotation_dir)) {
        basename(list.files(annotation_dir, pattern = "\\.gff(?:3)?(?:\\.gz)?$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE))
    } else {
        character(0)
    }
    fam_keys <- vapply(files, detect_annotation_cache_family, character(1), known_annotation_basenames = known_basenames)
    groups <- split(files, fam_keys)
    removed <- 0L
    repaired <- 0L
    for (grp in groups) {
        grp <- grp[file.exists(grp)]
        if (length(grp) <= 1L) {
            next
        }
        info <- file.info(grp)
        # Prefer the newer canonical cache filenames, which are typically longer
        # because they include more identity information (for example file size).
        ord <- order(-nchar(basename(grp)), -as.numeric(info$mtime), na.last = TRUE)
        keep <- grp[[ord[[1]]]]
        drop <- setdiff(grp, keep)
        if (!file.exists(keep) || length(drop) == 0L) {
            next
        }
        unlink(drop, force = TRUE)
        removed <- removed + length(drop)
        repaired <- repaired + 1L
    }

    max_files <- parse_positive_int_env("APP_ANNOTATION_DISK_CACHE_MAX_FILES", 192L)
    files_after <- list.files(cdir, pattern = "\\.rds$", full.names = TRUE)
    if (length(files_after) > max_files) {
        info_after <- file.info(files_after)
        ord_oldest <- order(as.numeric(info_after$mtime), na.last = TRUE)
        drop_extra <- files_after[ord_oldest[seq_len(length(files_after) - max_files)]]
        unlink(drop_extra, force = TRUE)
        removed <- removed + length(drop_extra)
    }

    if (removed > 0L || repaired > 0L) {
        app_debug_log(sprintf("[Cache] Annotation disk cache maintenance: removed %d stale file(s) across %d duplicate family(ies).", removed, repaired))
    }
    assign(maintain_key, TRUE, envir = .annotation_disk_cache_maintenance)
    invisible(NULL)
}

can_use_persistent_cache_for_path <- function(file_path, base_dir = ".") {
    p <- normalizePath(as.character(file_path %||% ""), winslash = "/", mustWork = FALSE)
    roots <- unique(c(
        normalizePath(base_dir, winslash = "/", mustWork = FALSE),
        get_cgv_data_root(base_dir),
        get_cgv_cache_root(base_dir)
    ))
    roots <- roots[nzchar(roots)]
    if (!nzchar(p) || length(roots) == 0L) {
        return(FALSE)
    }
    any(vapply(roots, function(root) startsWith(p, paste0(root, "/")) || identical(p, root), logical(1)))
}

canonical_cache_identity_path <- function(file_path, base_dir = ".") {
    p <- normalizePath(as.character(file_path %||% ""), winslash = "/", mustWork = FALSE)
    if (!nzchar(p)) {
        return("")
    }

    known_roots <- unique(c(
        normalizePath(base_dir, winslash = "/", mustWork = FALSE),
        get_cgv_data_root(base_dir),
        get_cgv_cache_root(base_dir),
        "/app"
    ))
    known_roots <- known_roots[nzchar(known_roots)]

    for (root in known_roots) {
        prefix <- paste0(root, "/")
        if (identical(p, root)) {
            return(".")
        }
        if (startsWith(p, prefix)) {
            return(substr(p, nchar(prefix) + 1L, nchar(p)))
        }
    }

    for (seg in c("annotations", "genomes", "go_annotations", "cache")) {
        marker <- paste0("/", seg, "/")
        hit <- regexpr(marker, p, fixed = TRUE)[1]
        if (is.finite(hit) && hit > 0L) {
            return(substr(p, hit + 1L, nchar(p)))
        }
    }

    p
}

get_gff_disk_index_path <- function(file_path, cache_kind = "gene_light", base_dir = ".") {
    cdir <- get_annotation_disk_cache_dir(base_dir = base_dir)
    key <- gff_cache_key(file_path)
    file.path(cdir, paste0(cache_kind, "__", .gff_index_cache_version, "__", sanitize_cache_key(key), ".rds"))
}

find_existing_gff_disk_index_path <- function(file_path, cache_kind = "gene_light", base_dir = ".") {
    cpath <- get_gff_disk_index_path(file_path, cache_kind = cache_kind, base_dir = base_dir)
    if (file.exists(cpath)) {
        return(cpath)
    }

    cdir <- get_annotation_disk_cache_dir(base_dir = base_dir)
    if (!dir.exists(cdir)) {
        return("")
    }

    files <- list.files(cdir, pattern = "\\.rds$", full.names = TRUE)
    if (length(files) == 0L) {
        return("")
    }

    target_family <- detect_annotation_cache_family(cpath, known_annotation_basenames = basename(as.character(file_path %||% "")))
    file_families <- vapply(
        files,
        detect_annotation_cache_family,
        character(1),
        known_annotation_basenames = basename(as.character(file_path %||% ""))
    )
    candidates <- files[file_families == target_family]
    if (length(candidates) == 0L) {
        return("")
    }

    info <- file.info(candidates)
    ord <- order(-nchar(basename(candidates)), -as.numeric(info$mtime), na.last = TRUE)
    candidates[[ord[[1]]]]
}

load_gff_index_from_disk <- function(file_path, cache_kind = "gene_light", base_dir = ".") {
    maintain_annotation_disk_cache(base_dir = base_dir)
    disk_kind <- if (identical(cache_kind, "gene_light")) "gene_light_compact_v1" else cache_kind
    cpath <- if (identical(cache_kind, "gene_light")) {
        get_gff_disk_index_path(file_path, cache_kind = disk_kind, base_dir = base_dir)
    } else {
        find_existing_gff_disk_index_path(file_path, cache_kind = disk_kind, base_dir = base_dir)
    }
    if (!file.exists(cpath) && identical(cache_kind, "gene_light")) {
        cpath <- find_existing_gff_disk_index_path(file_path, cache_kind = cache_kind, base_dir = base_dir)
    }
    if (!file.exists(cpath)) {
        return(NULL)
    }
    idx_obj <- tryCatch(readRDS(cpath), error = function(e) NULL)
    if (is.null(idx_obj)) {
        return(NULL)
    }

    if (identical(cache_kind, "gene_light")) {
        idx_obj <- compact_gff_gene_light_index(slim_gff_gene_light_index(idx_obj))
    }
    expected_path <- get_gff_disk_index_path(file_path, cache_kind = disk_kind, base_dir = base_dir)
    if (!identical(cache_kind, "gene_light") && nzchar(expected_path) && !identical(cpath, expected_path) && !file.exists(expected_path)) {
        try(saveRDS(idx_obj, expected_path, compress = "gzip"), silent = TRUE)
    }
    idx_obj
}

save_gff_index_to_disk <- function(file_path, idx_obj, cache_kind = "gene_light", base_dir = ".") {
    if (!can_use_persistent_cache_for_path(file_path, base_dir = base_dir)) {
        return(invisible(FALSE))
    }
    maintain_annotation_disk_cache(base_dir = base_dir)
    if (identical(cache_kind, "gene_light")) {
        idx_obj <- compact_gff_gene_light_index(slim_gff_gene_light_index(idx_obj))
        cache_kind <- "gene_light_compact_v1"
    }
    cpath <- get_gff_disk_index_path(file_path, cache_kind = cache_kind, base_dir = base_dir)
    cdir <- dirname(cpath)
    if (!dir.exists(cdir)) dir.create(cdir, recursive = TRUE, showWarnings = FALSE)
    ok <- atomic_save_rds(idx_obj, cpath, compress = "gzip")
    invisible(ok)
}

atomic_save_rds <- function(object, path, compress = "gzip") {
    target <- as.character(path %||% "")
    if (!nzchar(target)) {
        return(FALSE)
    }
    target_dir <- dirname(target)
    if (!dir.exists(target_dir)) {
        dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    }
    tmp <- tempfile(pattern = paste0(".", basename(target), "."), tmpdir = target_dir, fileext = ".part")
    backup <- paste0(target, ".bak")
    on.exit(unlink(tmp, force = TRUE), add = TRUE)
    ok <- tryCatch({
        saveRDS(object, tmp, compress = compress)
        verified <- readRDS(tmp)
        if (is.null(verified) && !is.null(object)) {
            stop("temporary RDS validation failed")
        }
        rm(verified)
        if (file.exists(backup)) {
            unlink(backup, force = TRUE)
        }
        had_target <- file.exists(target)
        if (had_target && !file.rename(target, backup)) {
            stop("could not stage existing RDS for replacement")
        }
        installed <- file.rename(tmp, target)
        if (!installed) {
            if (had_target && file.exists(backup)) {
                file.rename(backup, target)
            }
            stop("could not install replacement RDS")
        }
        if (file.exists(backup)) {
            unlink(backup, force = TRUE)
        }
        TRUE
    }, error = function(e) {
        if (!file.exists(target) && file.exists(backup)) {
            file.rename(backup, target)
        }
        FALSE
    })
    isTRUE(ok)
}

build_gff_autocomplete_cache <- function(file_path, idx, max_suggestions = 20000L) {
    p <- as.character(file_path %||% "")
    if (!nzchar(p) || !file.exists(p) || !is.list(idx) || !is.data.frame(idx$genes_df)) {
        return(NULL)
    }
    attrs <- as.character(idx$genes_df$attributes %||% rep("", nrow(idx$genes_df)))
    display <- extract_partial_gene_display_names(attrs)
    display <- sanitize_autocomplete_choices(display, max_total = max_suggestions)
    keys <- as.character(normalize_partial_gene_query(display))
    keep <- !is.na(keys) & nzchar(keys)
    display <- display[keep]
    keys <- keys[keep]
    list(
        display = display,
        keys = keys,
        version = .gff_autocomplete_cache_version,
        annotation_key = gff_cache_key(p)
    )
}

validate_gff_autocomplete_cache <- function(cache_obj, file_path) {
    p <- as.character(file_path %||% "")
    expected_key <- if (nzchar(p) && file.exists(p)) gff_cache_key(p) else ""
    is.list(cache_obj) &&
        identical(suppressWarnings(as.integer(cache_obj$version %||% NA_integer_)), .gff_autocomplete_cache_version) &&
        identical(as.character(cache_obj$annotation_key %||% ""), expected_key) &&
        is.character(cache_obj$display) &&
        is.character(cache_obj$keys) &&
        length(cache_obj$display) == length(cache_obj$keys)
}

load_gff_autocomplete_cache <- function(file_path, base_dir = ".") {
    p <- as.character(file_path %||% "")
    if (!nzchar(p) || !file.exists(p)) {
        return(NULL)
    }
    key <- gff_cache_key(p)
    cached <- get0(key, envir = .gff_autocomplete_cache_validation, inherits = FALSE, ifnotfound = NULL)
    if (is.list(cached) && validate_gff_autocomplete_cache(cached, p)) {
        return(cached)
    }
    cpath <- get_gff_disk_index_path(p, cache_kind = "autocomplete", base_dir = base_dir)
    if (!file.exists(cpath)) {
        cpath <- find_existing_gff_disk_index_path(p, cache_kind = "autocomplete", base_dir = base_dir)
    }
    cache_obj <- if (nzchar(cpath) && file.exists(cpath)) {
        tryCatch(readRDS(cpath), error = function(e) NULL)
    } else {
        NULL
    }
    if (!validate_gff_autocomplete_cache(cache_obj, p)) {
        return(NULL)
    }
    assign(key, cache_obj, envir = .gff_autocomplete_cache_validation)
    cache_obj
}

ensure_gff_autocomplete_cache <- function(file_path, idx, base_dir = ".") {
    p <- as.character(file_path %||% "")
    existing <- load_gff_autocomplete_cache(p, base_dir = base_dir)
    if (!is.null(existing)) {
        return(existing)
    }
    cache_obj <- tryCatch(build_gff_autocomplete_cache(p, idx), error = function(e) NULL)
    if (is.null(cache_obj)) {
        return(NULL)
    }
    saved <- save_gff_index_to_disk(p, cache_obj, cache_kind = "autocomplete", base_dir = base_dir)
    if (isTRUE(saved)) {
        assign(gff_cache_key(p), cache_obj, envir = .gff_autocomplete_cache_validation)
    }
    cache_obj
}

# Flat integer ranges avoid one R list allocation per alias. Compact aliases
# whose complete row vector equals the normal alias reuse that range; signed
# positions retain the exact original compact-map order, including collisions.
compact_gff_gene_light_index <- function(idx) {
    if (!is.list(idx) || !is.null(idx$comp_order)) return(idx)
    pack <- function(map) list(
        tokens = names(map) %||% character(0),
        ends = as.integer(cumsum(lengths(map))),
        rows = as.integer(unlist(map, use.names = FALSE))
    )
    normal <- idx$norm_map
    compact <- idx$comp_map
    if (!is.list(normal) || !is.list(compact)) return(idx)
    positions <- match(names(compact), names(normal))
    shared <- vapply(seq_along(compact), function(i) {
        !is.na(positions[i]) && identical(compact[[i]], normal[[positions[i]]])
    }, logical(1))
    order <- positions
    order[!shared] <- -seq_len(sum(!shared))
    idx$norm_map <- pack(normal)
    idx$comp_map <- pack(compact[!shared])
    idx$comp_order <- as.integer(order)
    idx
}

gene_lookup_tokens <- function(idx, kind = "norm") {
    map <- if (identical(kind, "comp")) idx$comp_map else idx$norm_map
    if (is.null(idx$comp_order)) return(names(map) %||% character(0))
    if (!identical(kind, "comp")) return(map$tokens)
    order <- idx$comp_order
    shared <- order > 0L
    tokens <- character(length(order))
    tokens[shared] <- idx$norm_map$tokens[order[shared]]
    tokens[!shared] <- idx$comp_map$tokens[-order[!shared]]
    tokens
}

gene_lookup_hits <- function(idx, tokens, kind = "norm") {
    map <- if (identical(kind, "comp")) idx$comp_map else idx$norm_map
    if (is.null(idx$comp_order)) return(unlist(map[tokens], use.names = FALSE))
    positions <- match(tokens, gene_lookup_tokens(idx, kind))
    positions <- positions[!is.na(positions)]
    if (identical(kind, "comp")) positions <- idx$comp_order[positions]
    unlist(lapply(positions, function(position) {
        range_map <- if (position > 0L) idx$norm_map else idx$comp_map
        i <- abs(position)
        first <- if (i == 1L) 1L else range_map$ends[i - 1L] + 1L
        last <- range_map$ends[i]
        if (first > last) return(integer(0))
        range_map$rows[seq.int(first, last)]
    }), use.names = FALSE)
}

slim_gff_gene_light_index <- function(idx) {
    if (!is.list(idx)) {
        return(idx)
    }
    keep <- intersect(c("genes_df", "gene_rows", "norm_map", "comp_map", "comp_order"), names(idx))
    idx[keep]
}

slim_gff_gene_light_index_file <- function(file_path, base_dir = ".") {
    p <- as.character(file_path %||% "")
    cpath <- find_existing_gff_disk_index_path(p, cache_kind = "gene_light", base_dir = base_dir)
    if (!nzchar(cpath) || !file.exists(cpath)) {
        return(invisible(FALSE))
    }
    idx <- tryCatch(readRDS(cpath), error = function(e) NULL)
    if (!is.list(idx) || is.null(idx$genes_df) || is.null(idx$norm_map)) {
        return(invisible(FALSE))
    }
    removable <- intersect(c("norm_list", "comp_list", "all_norm_tokens"), names(idx))
    ensure_gff_autocomplete_cache(p, idx, base_dir = base_dir)
    if (length(removable) == 0L) {
        return(invisible(TRUE))
    }
    slim <- slim_gff_gene_light_index(idx)
    invisible(atomic_save_rds(slim, cpath, compress = "gzip"))
}

precompute_annotation_index_cache <- function(annotation_file_path, base_dir = ".") {
    p <- as.character(annotation_file_path %||% "")
    if (!nzchar(p) || !file.exists(p)) {
        return(NULL)
    }
    idx <- build_gff_gene_light_index(p)
    slim_idx <- compact_gff_gene_light_index(slim_gff_gene_light_index(idx))
    save_gff_index_to_disk(p, slim_idx, cache_kind = "gene_light", base_dir = base_dir)
    ensure_gff_autocomplete_cache(p, slim_idx, base_dir = base_dir)
    # Pre-warm the genes table so the first plot doesn't have to build it
    tryCatch(get_genes_table_from_annotation(p), error = function(e) NULL)
    idx
}

clear_annotation_index_cache <- function(base_dir = ".") {
    cdir <- get_annotation_disk_cache_dir(base_dir = base_dir)
    if (dir.exists(cdir)) unlink(cdir, recursive = TRUE, force = TRUE)
    invisible(TRUE)
}

set_tabix_index_override <- function(annotation_path, index_path) {
    ap <- as.character(annotation_path %||% "")
    ip <- as.character(index_path %||% "")
    if (!nzchar(ap) || !nzchar(ip)) {
        return(invisible(NULL))
    }
    key <- normalizePath(ap, winslash = "/", mustWork = FALSE)
    assign(key, normalizePath(ip, winslash = "/", mustWork = FALSE), envir = .tabix_index_override_cache)
    invisible(NULL)
}

get_tabix_index_override <- function(annotation_path) {
    ap <- as.character(annotation_path %||% "")
    if (!nzchar(ap)) {
        return("")
    }
    key <- normalizePath(ap, winslash = "/", mustWork = FALSE)
    if (!exists(key, envir = .tabix_index_override_cache, inherits = FALSE)) {
        return("")
    }
    as.character(get(key, envir = .tabix_index_override_cache, inherits = FALSE) %||% "")
}

is_twobit_file <- function(path_value) {
    p <- as.character(path_value %||% "")
    nzchar(p) && grepl("\\.2bit$", p, ignore.case = TRUE)
}

find_existing_tabix_index <- function(annotation_path) {
    p <- as.character(annotation_path %||% "")
    if (!nzchar(p)) {
        return("")
    }
    candidates <- c(paste0(p, ".tbi"), paste0(p, ".csi"))
    hit <- candidates[file.exists(candidates)]
    if (length(hit) == 0) "" else hit[1]
}

is_tabix_annotation_file <- function(path_value, index_path = NULL) {
    p <- as.character(path_value %||% "")
    if (!nzchar(p) || !file.exists(p)) {
        return(FALSE)
    }
    if (!grepl("\\.(gff3?|gtf)\\.(gz|bgz)$", p, ignore.case = TRUE)) {
        return(FALSE)
    }
    idx <- as.character(index_path %||% "")
    if (!nzchar(idx)) idx <- get_tabix_index_override(p)
    if (!nzchar(idx) || !file.exists(idx)) idx <- find_existing_tabix_index(p)
    file.exists(idx)
}

get_tabix_seqnames_cached <- function(file_path) {
    p <- normalizePath(as.character(file_path %||% ""), winslash = "/", mustWork = FALSE)
    if (!nzchar(p) || !file.exists(p)) {
        return(character(0))
    }
    idx_override <- get_tabix_index_override(p)
    idx_path <- if (nzchar(idx_override) && file.exists(idx_override)) idx_override else find_existing_tabix_index(p)
    info_parts <- c(p, idx_path)
    info_sig <- vapply(info_parts, function(path_i) {
        if (!nzchar(path_i) || !file.exists(path_i)) return("")
        fi <- file.info(path_i)
        paste(as.character(fi$size[1] %||% ""), as.character(as.numeric(fi$mtime[1] %||% NA_real_)), sep = ":")
    }, character(1))
    key <- paste(c(p, idx_path, info_sig), collapse = "||")
    cached <- cache_env_get(.tabix_seqnames_cache, key, default = NULL)
    if (!is.null(cached)) {
        return(as.character(cached %||% character(0)))
    }
    tabix_bin <- Sys.which("tabix")
    adjacent_indexes <- normalizePath(c(paste0(p, ".tbi"), paste0(p, ".csi")), winslash = "/", mustWork = FALSE)
    can_use_cli <- nzchar(tabix_bin) && (!nzchar(idx_path) || normalizePath(idx_path, winslash = "/", mustWork = FALSE) %in% adjacent_indexes)
    seq_names <- if (isTRUE(can_use_cli)) {
        tryCatch(
            as.character(system2(tabix_bin, c("-l", shQuote(p)), stdout = TRUE, stderr = FALSE)),
            error = function(e) character(0)
        )
    } else if (requireNamespace("Rsamtools", quietly = TRUE)) {
        tbx <- if (nzchar(idx_path) && file.exists(idx_path)) {
            Rsamtools::TabixFile(p, index = idx_path)
        } else {
            Rsamtools::TabixFile(p)
        }
        tryCatch(as.character(Rsamtools::seqnamesTabix(tbx)), error = function(e) character(0))
    } else {
        character(0)
    }
    seq_names <- seq_names[nzchar(seq_names)]
    cache_env_set(.tabix_seqnames_cache, key, seq_names, max_size = 64L)
    seq_names
}

resolve_catalog_path <- function(path_value, base_dir = ".") {
    p <- as.character(path_value %||% "")
    if (!nzchar(p)) {
        return(NA_character_)
    }
    if (grepl("^/", p) || grepl("^[A-Za-z]:", p)) {
        return(normalizePath(p, winslash = "/", mustWork = FALSE))
    }
    p_norm <- gsub("\\\\", "/", p)
    first <- sub("/.*$", "", p_norm)
    if (first %in% c("annotations", "genomes", "go_annotations", "ncbi_downloads", "data")) {
        return(normalizePath(file.path(get_cgv_data_root(base_dir), p_norm), winslash = "/", mustWork = FALSE))
    }
    if (identical(first, "cache")) {
        cache_rel <- sub("^cache/?", "", p_norm)
        return(normalizePath(file.path(get_cgv_cache_root(base_dir), cache_rel), winslash = "/", mustWork = FALSE))
    }
    normalizePath(file.path(base_dir, p_norm), winslash = "/", mustWork = FALSE)
}

strip_data_extensions <- function(x) {
    out <- as.character(x %||% "")
    if (!nzchar(out)) {
        return("")
    }
    repeat {
        prev <- out
        out <- sub("\\.(gz|bgz)$", "", out, ignore.case = TRUE)
        out <- sub("\\.(gff3?|gtf|txt|fa|fasta|fna|2bit)$", "", out, ignore.case = TRUE)
        if (identical(prev, out)) break
    }
    out
}

build_stats_stem_candidates <- function(annotation_path = "", annotation_tabix_path = "", genome_path = "", genome_2bit_path = "") {
    raw <- c(
        basename(as.character(genome_2bit_path %||% "")),
        basename(as.character(genome_path %||% "")),
        basename(as.character(annotation_tabix_path %||% "")),
        basename(as.character(annotation_path %||% ""))
    )
    raw <- raw[nzchar(raw)]
    if (length(raw) == 0) {
        return(character(0))
    }
    stem <- unique(vapply(raw, strip_data_extensions, character(1)))
    stem <- stem[nzchar(stem)]
    stem_no_genomic <- unique(c(
        stem,
        sub("_genomic$", "", stem, ignore.case = TRUE),
        sub("\\.genomic$", "", stem, ignore.case = TRUE)
    ))
    stem_no_genomic[nzchar(stem_no_genomic)]
}

resolve_stats_files_for_entry <- function(annotation_path = "", annotation_tabix_path = "", genome_path = "", genome_2bit_path = "", base_dir = ".") {
    stats_dir <- resolve_catalog_path(file.path("genomes", "stats"), base_dir = base_dir)
    if (is.na(stats_dir) || !dir.exists(stats_dir)) {
        return(list(report = "", stats = ""))
    }
    cands <- build_stats_stem_candidates(
        annotation_path = annotation_path,
        annotation_tabix_path = annotation_tabix_path,
        genome_path = genome_path,
        genome_2bit_path = genome_2bit_path
    )
    if (length(cands) == 0) {
        return(list(report = "", stats = ""))
    }

    report <- ""
    stats <- ""
    for (s in cands) {
        rp <- file.path(stats_dir, paste0(s, "_assembly_report.txt"))
        sp <- file.path(stats_dir, paste0(s, "_assembly_stats.txt"))
        if (!nzchar(report) && file.exists(rp)) report <- normalizePath(rp, winslash = "/", mustWork = FALSE)
        if (!nzchar(stats) && file.exists(sp)) stats <- normalizePath(sp, winslash = "/", mustWork = FALSE)
        if (nzchar(report) && nzchar(stats)) break
    }
    list(report = report, stats = stats)
}

parse_assembly_report_file <- function(report_path) {
    rp <- as.character(report_path %||% "")
    if (!nzchar(rp) || !file.exists(rp)) {
        return(list(meta = list(), chr_map = list()))
    }
    key <- normalizePath(rp, winslash = "/", mustWork = FALSE)
    if (exists(key, envir = .assembly_report_cache, inherits = FALSE)) {
        return(get(key, envir = .assembly_report_cache, inherits = FALSE))
    }

    out <- list(meta = list(), chr_map = list())
    lines <- tryCatch(readLines(rp, warn = FALSE), error = function(e) character(0))
    if (length(lines) == 0) {
        assign(key, out, envir = .assembly_report_cache)
        return(out)
    }

    meta_matches <- stringr::str_match(lines, "^#\\s*([^#][^:]+):\\s*(.*)$")
    meta_keys <- tolower(trimws(as.character(meta_matches[, 2] %||% "")))
    meta_values <- trimws(as.character(meta_matches[, 3] %||% ""))
    meta_keep <- !is.na(meta_keys) & nzchar(meta_keys) & !is.na(meta_values) & nzchar(meta_values)
    if (any(meta_keep)) {
        meta_keys <- meta_keys[meta_keep]
        meta_values <- meta_values[meta_keep]
        first <- !duplicated(meta_keys)
        out$meta <- as.list(stats::setNames(meta_values[first], meta_keys[first]))
    }

    tbl_idx <- grep("^#\\s*Sequence-Name\\t", lines)
    if (length(tbl_idx) > 0) {
        data_lines <- lines[seq.int(tbl_idx[1] + 1L, length(lines))]
        data_lines <- data_lines[!startsWith(data_lines, "#") & nzchar(trimws(data_lines))]
        if (length(data_lines) > 0) {
            table_text <- paste(data_lines, collapse = "\n")
            report_df <- tryCatch(
                utils::read.delim(
                    text = table_text,
                    header = FALSE,
                    sep = "\t",
                    quote = "",
                    comment.char = "",
                    fill = TRUE,
                    stringsAsFactors = FALSE
                ),
                error = function(e) data.frame()
            )
            if (nrow(report_df) > 0L && ncol(report_df) >= 7L) {
                seq_name <- trimws(as.character(report_df[[1]] %||% ""))
                assigned <- trimws(as.character(report_df[[3]] %||% ""))
                genbank <- trimws(as.character(report_df[[5]] %||% ""))
                refseq <- trimws(as.character(report_df[[7]] %||% ""))
                ucsc <- if (ncol(report_df) >= 10L) trimws(as.character(report_df[[10]] %||% "")) else rep("", nrow(report_df))
                label <- ifelse(
                    nzchar(assigned) & !tolower(assigned) %in% c("na", "."),
                    assigned,
                    seq_name
                )
                ids <- as.character(t(cbind(seq_name, genbank, refseq, ucsc)))
                labels <- rep(label, each = 4L)
                keys <- as.vector(rbind(ids, sub("\\.\\d+$", "", ids)))
                key_labels <- rep(labels, each = 2L)
                valid <- !is.na(keys) & nzchar(trimws(keys)) &
                    !tolower(trimws(keys)) %in% c("na", "all", ".") &
                    !is.na(key_labels) & nzchar(trimws(key_labels)) &
                    !tolower(trimws(key_labels)) %in% c("na", "all", ".")
                keys <- tolower(trimws(keys[valid]))
                key_labels <- trimws(key_labels[valid])
                first <- !duplicated(keys)
                out$chr_map <- as.list(stats::setNames(key_labels[first], keys[first]))
            }
        }
    }

    assign(key, out, envir = .assembly_report_cache)
    out
}

parse_assembly_stats_file <- function(stats_path) {
    sp <- as.character(stats_path %||% "")
    if (!nzchar(sp) || !file.exists(sp)) {
        return(list(meta = list(), metrics = data.frame(statistic = character(0), value = character(0), stringsAsFactors = FALSE)))
    }
    key <- normalizePath(sp, winslash = "/", mustWork = FALSE)
    if (exists(key, envir = .assembly_stats_cache, inherits = FALSE)) {
        return(get(key, envir = .assembly_stats_cache, inherits = FALSE))
    }

    out <- list(meta = list(), metrics = data.frame(statistic = character(0), value = character(0), stringsAsFactors = FALSE))
    lines <- tryCatch(readLines(sp, warn = FALSE), error = function(e) character(0))
    if (length(lines) == 0) {
        assign(key, out, envir = .assembly_stats_cache)
        return(out)
    }

    meta_pat <- "^#\\s*([^#][^:]+):\\s*(.*)$"
    for (ln in lines) {
        m <- stringr::str_match(ln, meta_pat)
        if (!all(is.na(m))) {
            k <- tolower(trimws(as.character(m[1, 2] %||% "")))
            v <- trimws(as.character(m[1, 3] %||% ""))
            if (nzchar(k) && nzchar(v) && is.null(out$meta[[k]])) {
                out$meta[[k]] <- v
            }
        }
    }

    hdr_idx <- grep("^#\\s*unit-name\\t", lines)
    if (length(hdr_idx) > 0) {
        data_lines <- lines[seq.int(hdr_idx[1] + 1L, length(lines))]
        data_lines <- data_lines[!startsWith(data_lines, "#") & nzchar(trimws(data_lines))]
        if (length(data_lines) > 0) {
            rows <- lapply(data_lines, function(ln) {
                tok <- strsplit(ln, "\t", fixed = TRUE)[[1]]
                if (length(tok) < 6) {
                    return(NULL)
                }
                data.frame(
                    unit_name = as.character(tok[1]),
                    statistic = as.character(tok[5]),
                    value = as.character(tok[6]),
                    stringsAsFactors = FALSE
                )
            })
            rows <- rows[!vapply(rows, is.null, logical(1))]
            if (length(rows) > 0) {
                df <- do.call(rbind,  rows)
                df <- df[tolower(trimws(df$unit_name)) == "all", c("statistic", "value"), drop = FALSE]
                if (nrow(df) > 0) {
                    df$statistic <- trimws(as.character(df$statistic))
                    df$value <- trimws(as.character(df$value))
                    df <- df[nzchar(df$statistic), , drop = FALSE]
                    df <- df[!duplicated(df$statistic), , drop = FALSE]
                    out$metrics <- df
                }
            }
        }
    }

    assign(key, out, envir = .assembly_stats_cache)
    out
}

get_assembly_info_bundle <- function(report_path = "", stats_path = "") {
    rep_info <- parse_assembly_report_file(report_path)
    st_info <- parse_assembly_stats_file(stats_path)
    meta <- st_info$meta
    rep_meta <- rep_info$meta
    if (length(rep_meta) > 0) {
        for (k in names(rep_meta)) {
            if (is.null(meta[[k]]) || !nzchar(as.character(meta[[k]] %||% ""))) {
                meta[[k]] <- rep_meta[[k]]
            }
        }
    }
    list(
        meta = meta,
        metrics = st_info$metrics,
        chr_map = rep_info$chr_map
    )
}

get_assembly_report_path_for_annotation <- function(annotation_file_path, base_dir = ".") {
    ap <- as.character(annotation_file_path %||% "")
    if (!nzchar(ap) || !file.exists(ap)) {
        return("")
    }
    key <- normalizePath(ap, winslash = "/", mustWork = FALSE)
    if (exists(key, envir = .annotation_report_path_cache, inherits = FALSE)) {
        return(as.character(get(key, envir = .annotation_report_path_cache, inherits = FALSE) %||% ""))
    }

    reg <- get_preloaded_species_registry(registry_path = file.path("annotations", "registry.tsv"), base_dir = base_dir)
    if (nrow(reg) == 0) {
        assign(key, "", envir = .annotation_report_path_cache)
        return("")
    }
    ann_norm <- normalizePath(as.character(reg$annotation_path %||% ""), winslash = "/", mustWork = FALSE)
    hit_idx <- which(ann_norm == key)
    if (length(hit_idx) == 0) {
        assign(key, "", envir = .annotation_report_path_cache)
        return("")
    }
    rp <- as.character(reg$assembly_report_path[hit_idx[1]] %||% "")
    if (!nzchar(rp) || !file.exists(rp)) rp <- ""
    assign(key, rp, envir = .annotation_report_path_cache)
    rp
}

resolve_icon_web_path <- function(icon_value, base_dir = ".") {
    default_icon <- if (file.exists(normalizePath(file.path(base_dir, "www", "icons", "DNA.ico"), winslash = "/", mustWork = FALSE))) "icons/DNA.ico" else "icons/dna.ico"
    p <- trimws(as.character(icon_value %||% ""))
    if (!nzchar(p)) {
        return(default_icon)
    }
    if (grepl("^https?://", p, ignore.case = TRUE)) {
        return(p)
    }
    if (startsWith(p, "/")) {
        p_rel <- sub("^/+", "", p)
        if (file.exists(normalizePath(file.path(base_dir, "www", p_rel), winslash = "/", mustWork = FALSE))) {
            return(p_rel)
        }
        return(default_icon)
    }

    p_abs <- if (grepl("^[A-Za-z]:", p) || startsWith(p, "/")) {
        normalizePath(p, winslash = "/", mustWork = FALSE)
    } else {
        normalizePath(file.path(base_dir, p), winslash = "/", mustWork = FALSE)
    }
    www_dir <- normalizePath(file.path(base_dir, "www"), winslash = "/", mustWork = FALSE)
    if (file.exists(p_abs) && startsWith(p_abs, paste0(www_dir, "/"))) {
        rel <- substring(p_abs, nchar(www_dir) + 2)
        return(rel)
    }

    p2 <- sub("^www/", "", p)
    if (file.exists(normalizePath(file.path(base_dir, "www", p2), winslash = "/", mustWork = FALSE))) {
        return(p2)
    }
    default_icon
}

get_preloaded_species_registry <- function(registry_path = file.path("annotations", "registry.tsv"), base_dir = ".") {
    reg_abs <- resolve_catalog_path(registry_path, base_dir = base_dir)
    if (is.na(reg_abs) || !file.exists(reg_abs)) {
        return(data.frame())
    }

    finfo <- file.info(reg_abs)
    cache_key <- paste0(reg_abs, "::", as.numeric(finfo$mtime[1] %||% 0))
    if (exists(cache_key, envir = .preloaded_registry_cache, inherits = FALSE)) {
        return(get(cache_key, envir = .preloaded_registry_cache, inherits = FALSE))
    }

    reg <- tryCatch(
        read.delim(reg_abs, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) data.frame()
    )
    if (nrow(reg) == 0) {
        return(data.frame())
    }

    required_cols <- c(
        "species_id", "label", "organism", "taxid",
        "annotation", "annotation_tabix", "annotation_index",
        "genome", "genome_2bit", "aliases", "icon"
    )
    for (nm in setdiff(required_cols, colnames(reg))) reg[[nm]] <- NA_character_

    reg$species_id <- ifelse(
        nzchar(trimws(as.character(reg$species_id))),
        trimws(as.character(reg$species_id)),
        make.names(trimws(as.character(reg$organism %||% reg$label)), unique = TRUE)
    )
    reg$label <- ifelse(
        nzchar(trimws(as.character(reg$label))),
        trimws(as.character(reg$label)),
        trimws(as.character(reg$organism))
    )
    reg$organism <- trimws(as.character(reg$organism))
    reg$taxid <- suppressWarnings(as.integer(reg$taxid))

    if ("genome_fasta" %in% colnames(reg)) {
        missing_genome <- !nzchar(trimws(as.character(reg$genome %||% "")))
        reg$genome[missing_genome] <- as.character(reg$genome_fasta[missing_genome] %||% "")
    }

    reg$annotation_rel <- trimws(as.character(reg$annotation))
    reg$annotation_tabix_rel <- trimws(as.character(reg$annotation_tabix))
    reg$annotation_index_rel <- trimws(as.character(reg$annotation_index))
    reg$genome_rel <- trimws(as.character(reg$genome))
    reg$genome_2bit_rel <- trimws(as.character(reg$genome_2bit))

    reg$annotation_plain_path <- vapply(reg$annotation_rel, resolve_catalog_path, character(1), base_dir = base_dir)
    reg$annotation_tabix_path <- vapply(reg$annotation_tabix_rel, resolve_catalog_path, character(1), base_dir = base_dir)
    reg$annotation_index_path <- vapply(reg$annotation_index_rel, resolve_catalog_path, character(1), base_dir = base_dir)
    reg$genome_plain_path <- vapply(reg$genome_rel, resolve_catalog_path, character(1), base_dir = base_dir)
    reg$genome_2bit_path <- vapply(reg$genome_2bit_rel, resolve_catalog_path, character(1), base_dir = base_dir)

    reg$annotation_tabix_ready <- vapply(seq_len(nrow(reg)), function(i) {
        tabix_path <- as.character(reg$annotation_tabix_path[i] %||% "")
        index_path <- as.character(reg$annotation_index_path[i] %||% "")
        if (is.na(tabix_path)) tabix_path <- ""
        if (is.na(index_path) || !nzchar(index_path) || !file.exists(index_path)) index_path <- find_existing_tabix_index(tabix_path)
        ok <- is_tabix_annotation_file(tabix_path, index_path = index_path)
        if (ok) set_tabix_index_override(tabix_path, index_path)
        ok
    }, logical(1))
    reg$annotation_mode <- ifelse(reg$annotation_tabix_ready, "tabix", "plain")
    reg$annotation_path <- ifelse(reg$annotation_tabix_ready, reg$annotation_tabix_path, reg$annotation_plain_path)

    reg$genome_2bit_exists <- vapply(reg$genome_2bit_path, function(p) {
        p <- as.character(p %||% "")
        !is.na(p) && nzchar(p) && file.exists(p)
    }, logical(1))
    reg$genome_mode <- ifelse(reg$genome_2bit_exists, "2bit", "fasta")
    reg$genome_path <- ifelse(reg$genome_2bit_exists, reg$genome_2bit_path, reg$genome_plain_path)

    stats_files <- lapply(seq_len(nrow(reg)), function(i) {
        resolve_stats_files_for_entry(
            annotation_path = as.character(reg$annotation_rel[i] %||% ""),
            annotation_tabix_path = as.character(reg$annotation_tabix_rel[i] %||% ""),
            genome_path = as.character(reg$genome_rel[i] %||% ""),
            genome_2bit_path = as.character(reg$genome_2bit_rel[i] %||% ""),
            base_dir = base_dir
        )
    })
    reg$assembly_report_path <- vapply(stats_files, function(x) as.character(x$report %||% ""), character(1))
    reg$assembly_stats_path <- vapply(stats_files, function(x) as.character(x$stats %||% ""), character(1))
    reg$assembly_report_exists <- vapply(reg$assembly_report_path, function(p) {
        p <- as.character(p %||% "")
        nzchar(p) && file.exists(p)
    }, logical(1))
    reg$assembly_stats_exists <- vapply(reg$assembly_stats_path, function(p) {
        p <- as.character(p %||% "")
        nzchar(p) && file.exists(p)
    }, logical(1))

    reg$annotation_exists <- vapply(reg$annotation_path, function(p) {
        p <- as.character(p %||% "")
        !is.na(p) && nzchar(p) && file.exists(p)
    }, logical(1))
    reg$genome_exists <- vapply(reg$genome_path, function(p) {
        p <- as.character(p %||% "")
        !is.na(p) && nzchar(p) && file.exists(p)
    }, logical(1))
    reg$icon_url <- vapply(as.character(reg$icon %||% ""), resolve_icon_web_path, character(1), base_dir = base_dir)
    reg$ready <- reg$annotation_exists

    out <- reg
    assign(cache_key, out, envir = .preloaded_registry_cache)
    out
}

gff_cache_key <- function(file_path) {
    finfo <- file.info(file_path)
    cache_id_path <- canonical_cache_identity_path(file_path, base_dir = ".")
    if (nrow(finfo) == 0 || is.na(finfo$mtime[1])) {
        return(paste0(cache_id_path %||% as.character(file_path %||% ""), "::unknown::", .gff_index_cache_version))
    }
    size_num <- suppressWarnings(as.numeric(finfo$size[1] %||% NA_real_))
    mtime_num <- suppressWarnings(as.numeric(finfo$mtime[1] %||% NA_real_))
    size_key <- if (is.finite(size_num) && !is.na(size_num)) as.character(as.integer(round(size_num))) else "unknown_size"
    mtime_key <- if (is.finite(mtime_num) && !is.na(mtime_num)) as.character(as.integer(round(mtime_num))) else "unknown_mtime"
    paste0(
        cache_id_path,
        "::",
        size_key,
        "::",
        mtime_key,
        "::",
        .gff_index_cache_version
    )
}

parse_gff_attributes <- function(attr) {
    parts <- unlist(strsplit(attr, ";", fixed = TRUE))
    kv <- lapply(parts, function(p) {
        p <- trimws(p)
        if (!nzchar(p)) {
            return(NULL)
        }
        if (grepl("=", p, fixed = TRUE)) {
            kvp <- strsplit(p, "=", fixed = TRUE)[[1]]
            list(key = safe_url_decode(kvp[1]), value = safe_url_decode(paste(kvp[-1], collapse = "=")))
        } else {
            m <- stringr::str_match(p, '^([^\\s]+)\\s+"([^"]+)"')
            if (!all(is.na(m))) list(key = safe_url_decode(m[1, 2]), value = safe_url_decode(m[1, 3])) else list(key = NA_character_, value = safe_url_decode(p))
        }
    })
    kv <- kv[!vapply(kv, is.null, logical(1))]
    if (length(kv) == 0) {
        return(list())
    }
    keys <- tolower(vapply(kv, function(x) x$key, character(1)))
    vals <- vapply(kv, function(x) x$value, character(1))
    split(vals, keys)
}

safe_url_decode <- function(x) {
    x_chr <- as.character(x)
    if (length(x_chr) == 0) {
        return(character(0))
    }
    na_mask <- is.na(x_chr)
    x_tmp <- x_chr
    x_tmp[na_mask] <- ""
    # Protect stray '%' that are not valid hex escapes.
    x_tmp <- gsub("%(?![0-9A-Fa-f]{2})", "%25", x_tmp, perl = TRUE)
    out <- tryCatch(
        suppressWarnings(utils::URLdecode(x_tmp)),
        error = function(e) x_tmp
    )
    out <- as.character(out)
    out[na_mask] <- NA_character_
    out
}

normalize_gene_token <- function(x) {
    x <- tolower(safe_url_decode(x))
    x <- gsub("[\"']", "", x)
    trimws(x)
}
normalize_gene_compact <- function(x) {
    x <- normalize_gene_token(x)
    gsub("[^a-z0-9]+", "", x)
}

is_known_gene_stable_id <- function(x) {
    v <- tolower(trimws(as.character(x %||% "")))
    v <- gsub("[\"']", "", safe_url_decode(v))
    grepl("^(gene:|transcript:|cds:)?bgiosga\\d+([_-][tp]a)?$", v) ||
        grepl("^loc_os\\d{2}g\\d{5,}$", v) ||
        grepl("^os\\d{2}g\\d{5,}$", v) ||
        grepl("^ens[a-z]{0,6}g\\d+$", v)
}

extract_gene_query_alpha_core <- function(q) {
    comp <- gsub("[^a-z0-9]+", "", normalize_gene_token(q))
    a <- gsub("[^a-z]+", "", comp)
    if (startsWith(a, "os") && nchar(a) > 4) a <- sub("^os", "", a)
    a
}

extract_gene_query_digit_core <- function(q) {
    comp <- gsub("[^a-z0-9]+", "", normalize_gene_token(q))
    gsub("[^0-9]+", "", comp)
}

extract_gene_query_digit_chunks <- function(q) {
    qn <- normalize_gene_token(q)
    unlist(regmatches(qn, gregexpr("[0-9]+", qn, perl = TRUE)))
}

extract_token_digit_chunks <- function(tok) {
    tc <- gsub("[^a-z0-9]+", "", tolower(as.character(tok %||% "")))
    unlist(regmatches(tc, gregexpr("[0-9]+", tc, perl = TRUE)))
}

digit_chunks_compatible <- function(query_chunks, token_chunks) {
    q <- as.character(query_chunks %||% character(0))
    t <- as.character(token_chunks %||% character(0))
    q <- q[nzchar(q)]
    t <- t[nzchar(t)]
    if (length(q) == 0 || length(t) == 0) {
        return(FALSE)
    }
    if (identical(q, t)) {
        return(TRUE)
    }
    if (identical(paste0(q, collapse = ""), paste0(t, collapse = ""))) {
        return(TRUE)
    }
    # Common notation compatibility: HKT1;1 can map to HKT1.
    if (length(q) >= 2 && length(t) == 1 && all(q == q[1]) && identical(q[1], t[1])) {
        return(TRUE)
    }
    FALSE
}

is_symbol_like_gene_query <- function(q) {
    comp <- gsub("[^a-z0-9]+", "", normalize_gene_token(q))
    if (!nzchar(comp)) {
        return(FALSE)
    }
    grepl("[a-z]", comp) && grepl("[0-9]", comp) && !is_known_gene_stable_id(comp)
}

is_low_specific_gene_family_query <- function(q) {
    comp <- normalize_partial_gene_query(q)
    nzchar(comp) &&
        nchar(comp) >= 2L &&
        nchar(comp) <= 8L &&
        grepl("^[a-z]+$", comp)
}

is_same_low_specific_family_alias <- function(input_gene, alias_candidate) {
    input_comp <- normalize_partial_gene_query(input_gene)
    alias_comp <- normalize_partial_gene_query(alias_candidate)
    nzchar(input_comp) &&
        nzchar(alias_comp) &&
        identical(input_comp, alias_comp) &&
        is_low_specific_gene_family_query(input_gene)
}

is_ambiguous_low_specific_family_alias <- function(input_gene, alias_candidate) {
    if (!is_low_specific_gene_family_query(input_gene)) {
        return(FALSE)
    }
    input_alpha <- extract_gene_query_alpha_core(input_gene)
    alias_alpha <- extract_gene_query_alpha_core(alias_candidate)
    if (!nzchar(input_alpha) || !identical(input_alpha, alias_alpha)) {
        return(FALSE)
    }
    alias_txt <- normalize_lookup_alias(alias_candidate)
    alias_digits <- extract_gene_query_digit_core(alias_txt)
    nzchar(alias_digits) && !grepl("[;._:-]", alias_txt)
}

collect_result_evidence_tokens <- function(res_obj, alias_candidate = NULL) {
    txt <- c(
        alias_candidate,
        as.character(res_obj$matched_gene_name %||% ""),
        as.character(res_obj$matched_gene_id %||% "")
    )

    if (!is.null(res_obj$data) && nrow(res_obj$data) > 0 && "V9" %in% colnames(res_obj$data)) {
        attrs <- parse_gff_attributes(as.character(res_obj$data$V9[1] %||% ""))
        keys <- c("name", "gene", "gene_name", "description", "product", "note", "gene_synonym", "id", "gene_id", "locus_tag")
        for (k in keys) {
            v <- attrs[[k]]
            if (!is.null(v) && length(v) > 0) {
                txt <- c(txt, as.character(v))
            }
        }
    }

    txt <- as.character(txt %||% character(0))
    txt <- trimws(safe_url_decode(txt))
    txt <- txt[!is.na(txt) & nzchar(txt)]
    txt <- txt[tolower(txt) != "na"]
    unique(tolower(txt))
}

collect_result_identity_tokens <- function(res_obj, alias_candidate = NULL) {
    txt <- c(
        alias_candidate,
        as.character(res_obj$matched_gene_name %||% ""),
        as.character(res_obj$matched_gene_id %||% "")
    )

    if (!is.null(res_obj$data) && nrow(res_obj$data) > 0 && "V9" %in% colnames(res_obj$data)) {
        attrs <- parse_gff_attributes(as.character(res_obj$data$V9[1] %||% ""))
        keys <- c(
            "id", "name", "gene", "gene_name", "gene_id", "gene_synonym",
            "gene_synonyms", "synonym", "alias", "dbxref", "locus",
            "locus_tag", "transcript_id", "transcript", "protein_id"
        )
        for (k in intersect(keys, names(attrs))) {
            v <- attrs[[k]]
            if (!is.null(v) && length(v) > 0) {
                split_vals <- unlist(strsplit(as.character(v), "[,| ]"))
                txt <- c(txt, as.character(v), split_vals)
            }
        }
    }

    txt <- as.character(txt %||% character(0))
    txt <- trimws(safe_url_decode(txt))
    txt <- txt[!is.na(txt) & nzchar(txt)]
    txt <- txt[tolower(txt) != "na"]
    unique(tolower(txt))
}

is_gene_symbol_extension_alias <- function(input_gene, alias_candidate) {
    input_comp <- gsub("[^a-z0-9]+", "", normalize_gene_token(input_gene))
    alias_comp <- gsub("[^a-z0-9]+", "", normalize_gene_token(alias_candidate))
    if (!nzchar(input_comp) || !nzchar(alias_comp) || identical(input_comp, alias_comp)) {
        return(FALSE)
    }
    if (!grepl("[a-z]", input_comp) || !grepl("[0-9]", input_comp)) {
        return(FALSE)
    }
    startsWith(alias_comp, input_comp)
}

is_compact_gene_identity_match <- function(input_gene, token) {
    input_clean <- tolower(trimws(safe_url_decode(input_gene)))
    token_clean <- tolower(trimws(safe_url_decode(token)))
    if (!nzchar(input_clean) || !nzchar(token_clean)) {
        return(FALSE)
    }
    if (identical(input_clean, token_clean)) {
        return(TRUE)
    }

    input_compact <- gsub("[^a-z0-9]+", "", input_clean)
    token_compact <- gsub("[^a-z0-9]+", "", token_clean)
    if (!nzchar(input_compact) || !nzchar(token_compact) || !identical(input_compact, token_compact)) {
        return(FALSE)
    }

    input_alpha <- extract_gene_query_alpha_core(input_gene)
    token_alpha <- extract_gene_query_alpha_core(token)
    input_digits <- extract_gene_query_digit_chunks(input_gene)
    token_digits <- extract_token_digit_chunks(token)
    identical(input_alpha, token_alpha) &&
        digit_chunks_compatible(input_digits, token_digits)
}

is_external_bridge_identifier_alias <- function(alias_candidate) {
    txt <- normalize_lookup_alias(alias_candidate)
    low <- tolower(txt)
    comp <- gsub("[^a-z0-9]+", "", low)
    if (!nzchar(comp)) {
        return(FALSE)
    }
    is_known_gene_stable_id(txt) ||
        grepl("^loc[a-z0-9]*[0-9][a-z0-9]*$", comp) ||
        grepl("^[a-z]{1,3}_[0-9]+(\\.[0-9]+)?$", low) ||
        grepl("^[a-z]{2}_[0-9]+(\\.[0-9]+)?$", low) ||
        grepl("^[a-z]{1,4}[0-9]{5,}(\\.[0-9]+)?$", low)
}

is_alias_result_compatible <- function(input_gene, alias_candidate, res_obj,
                                      organism = NULL, taxid = NULL) {
    if (is.null(res_obj) || is.null(res_obj$data) || nrow(res_obj$data) == 0) {
        return(FALSE)
    }
    if (is_same_low_specific_family_alias(input_gene, alias_candidate)) {
        return(FALSE)
    }
    if (is_ambiguous_low_specific_family_alias(input_gene, alias_candidate)) {
        return(FALSE)
    }
    if (is_gene_symbol_extension_alias(input_gene, alias_candidate)) {
        return(FALSE)
    }
    if (!is_symbol_like_gene_query(input_gene)) {
        return(TRUE)
    }

    identity <- collect_result_identity_tokens(res_obj, alias_candidate)
    if (length(identity) == 0) {
        return(FALSE)
    }

    if (any(vapply(identity, is_compact_gene_identity_match, logical(1), input_gene = input_gene))) {
        return(TRUE)
    }

    # External aliases may bridge to local IDs (for example LOC identifiers),
    # but symbol-like queries must not resolve through description-only terms.
    !is_gene_symbol_extension_alias(input_gene, alias_candidate) &&
        is_external_bridge_identifier_alias(alias_candidate)
}

normalize_lookup_alias <- function(x) {
    v <- as.character(x %||% "")
    v <- trimws(utils::URLdecode(v))
    if (is.na(v)) "" else v
}

is_low_specific_lookup_alias <- function(x) {
    a <- tolower(normalize_lookup_alias(x))
    if (!nzchar(a)) {
        return(TRUE)
    }
    if (grepl("(^|[-_.])e\\d+$", a)) {
        return(TRUE)
    }
    if (grepl("^cds[:._-]", a)) {
        return(TRUE)
    }
    if (grepl("^transcript[:._-]", a)) {
        return(TRUE)
    }
    if (grepl("^mrna[:._-]", a)) {
        return(TRUE)
    }
    FALSE
}

lookup_alias_quality_score <- function(alias_txt, input_gene) {
    a <- normalize_lookup_alias(alias_txt)
    if (!nzchar(a)) {
        return(-999)
    }
    al <- tolower(a)
    input_norm <- tolower(normalize_lookup_alias(input_gene))
    score <- 0
    if (grepl("^[A-Za-z0-9;._:+-]+$", a)) score <- score + 2 else score <- score - 2
    if (grepl("\\s", a)) score <- score - 3
    if (nchar(a) > 40) score <- score - 2
    if (is_low_specific_lookup_alias(a)) score <- score - 8
    if (nzchar(input_norm) && identical(al, input_norm)) score <- score + 4

    in_comp <- gsub("[^a-z0-9]+", "", tolower(as.character(input_gene %||% "")))
    al_comp <- gsub("[^a-z0-9]+", "", al)
    if (nzchar(in_comp) && identical(al_comp, in_comp)) score <- score + 9

    in_alpha <- gsub("[^a-z]+", "", tolower(as.character(input_gene %||% "")))
    if (nzchar(in_alpha) && grepl(in_alpha, al, fixed = TRUE)) score <- score + 5
    score
}

is_internal_gene_display_label <- function(x) {
    txt <- normalize_lookup_alias(x)
    if (!nzchar(txt)) {
        return(TRUE)
    }
    low <- tolower(txt)
    comp <- gsub("[^a-z0-9]+", "", low)
    if (!nzchar(comp)) {
        return(TRUE)
    }
    if (isTRUE(is_known_gene_stable_id(txt))) {
        return(TRUE)
    }
    if (grepl("^(gene|transcript|mrna|cds|rna)[:._-]", low)) {
        return(TRUE)
    }
    if (grepl("^loc[_:-]?[a-z0-9]+$", low) && grepl("[0-9]", low)) {
        return(TRUE)
    }
    if (grepl("^[a-z][0-9][a-z0-9]{4,9}$", low) && !grepl("[;._-]", txt)) {
        return(TRUE)
    }
    FALSE
}

lookup_display_gene_score <- function(label, input_gene = "", matched_gene_name = "", matched_gene_id = "") {
    txt <- normalize_lookup_alias(label)
    if (!nzchar(txt)) {
        return(-Inf)
    }

    low <- tolower(txt)
    score <- 0

    if (grepl("^[A-Za-z0-9;._:+-]+$", txt)) {
        score <- score + 10
    } else {
        score <- score - 4
    }
    if (grepl("\\s", txt)) {
        score <- score - 10
    }
    if (nchar(txt) <= 20) {
        score <- score + 4
    }
    if (grepl("[A-Za-z]", txt)) {
        score <- score + 3
    }
    if (grepl("[A-Z]", txt)) {
        score <- score + 8
    }
    if (grepl("[0-9]", txt)) {
        score <- score + 3
    }
    if (is_symbol_like_gene_query(txt)) {
        score <- score + 14
    }
    if (is_low_specific_lookup_alias(txt)) {
        score <- score - 12
    }
    if (is_internal_gene_display_label(txt)) {
        score <- score - 120
    }

    input_norm <- normalize_lookup_alias(input_gene)
    input_comp <- normalize_gene_compact(input_norm)
    txt_comp <- normalize_gene_compact(txt)
    if (nzchar(input_norm) && identical(low, tolower(input_norm))) {
        score <- score + 10
    }
    if (nzchar(input_comp) && identical(txt_comp, input_comp)) {
        score <- score + 24
    }

    input_alpha <- extract_gene_query_alpha_core(input_norm)
    input_digit <- extract_gene_query_digit_core(input_norm)
    txt_alpha <- extract_gene_query_alpha_core(txt)
    txt_digit <- extract_gene_query_digit_core(txt)
    if (nzchar(input_alpha) && identical(txt_alpha, input_alpha)) {
        score <- score + 8
    }
    if (nzchar(input_digit) && identical(txt_digit, input_digit)) {
        score <- score + 8
    }
    if (digit_chunks_compatible(extract_gene_query_digit_chunks(input_norm), extract_token_digit_chunks(txt))) {
        score <- score + 6
    }

    matched_name_norm <- normalize_lookup_alias(matched_gene_name)
    matched_id_norm <- normalize_lookup_alias(matched_gene_id)
    if (nzchar(matched_name_norm) && identical(low, tolower(matched_name_norm))) {
        score <- score + 6
    }
    if (nzchar(matched_id_norm) && identical(low, tolower(matched_id_norm))) {
        score <- score - 20
    }

    score
}

choose_lookup_display_gene_name <- function(res_obj, query_candidates = character(0), best_alias_used = "", input_gene = "") {
    res_list <- if (is.list(res_obj)) res_obj else list()
    query_vec <- normalize_lookup_query_candidates(query_candidates)
    input_label <- normalize_lookup_alias(input_gene)
    if (!nzchar(input_label) && length(query_vec) > 0) {
        input_label <- query_vec[1]
    }

    matched_name <- normalize_lookup_alias(res_list$matched_gene_name %||% "")
    matched_id <- normalize_lookup_alias(res_list$matched_gene_id %||% "")
    include_input_label <- !is_low_specific_gene_family_query(input_label) || !nzchar(matched_name)
    candidates <- normalize_lookup_query_candidates(c(
        best_alias_used,
        matched_name,
        query_vec,
        if (isTRUE(include_input_label)) input_label else character(0),
        matched_id
    ))
    if (length(candidates) == 0) {
        return("")
    }

    scores <- vapply(
        candidates,
        lookup_display_gene_score,
        numeric(1),
        input_gene = input_label,
        matched_gene_name = matched_name,
        matched_gene_id = matched_id
    )
    ord <- order(-scores, nchar(candidates), seq_along(candidates))
    best <- candidates[ord][1]
    if (!nzchar(best) || !is.finite(scores[ord][1])) {
        return("")
    }
    best
}

lookup_result_name_penalty <- function(res_obj) {
    nm <- tolower(trimws(as.character(res_obj$matched_gene_name %||% "")))
    if (!nzchar(nm)) {
        return(-2L)
    }
    if (grepl("(^|[-_.])e\\d+$", nm)) {
        return(-6L)
    }
    if (grepl("^transcript[:._-]", nm)) {
        return(-5L)
    }
    if (grepl("^mrna[:._-]", nm)) {
        return(-5L)
    }
    if (grepl("^cds[:._-]", nm)) {
        return(-5L)
    }
    0L
}

classify_lookup_result_rank <- function(res_obj) {
    if (is.null(res_obj) || is.null(res_obj$data) || nrow(res_obj$data) == 0) {
        return(0L)
    }
    feat <- tolower(trimws(as.character(res_obj$data$V3 %||% character(0))))
    if (length(feat) == 0) {
        return(0L)
    }
    has_gene <- any(feat == "gene")
    tx_level_types <- c(
        "mrna", "transcript", "lnc_rna", "trna", "rrna", "snorna", "snrna", "mirna",
        "ncrna", "primary_transcript", "pre_mirna", "guide_rna", "rnase_p_rna",
        "rnase_mrp_rna", "telomerase_rna", "antisense_rna", "srp_rna", "scarna",
        "vault_rna", "y_rna", "antisense_lncrna", "lncrna"
    )
    has_tx <- any(feat %in% tx_level_types)
    has_struct <- any(feat %in% c("exon", "cds", "start_codon", "stop_codon") | grepl("utr", feat))
    if (has_gene) {
        return(3L)
    }
    if (has_tx) {
        return(2L)
    }
    if (has_struct) {
        return(1L)
    }
    0L
}

normalize_lookup_query_candidates <- function(vals) {
    x <- as.character(vals %||% character(0))
    x <- unique(vapply(x, normalize_lookup_alias, character(1)))
    x <- x[nzchar(x)]
    x
}

attach_lookup_result_meta <- function(res_obj, query_candidates = character(0), best_alias_used = "", input_gene = "") {
    out <- if (is.list(res_obj)) {
        res_obj
    } else {
        list(
            data = NULL,
            matched_gene_id = NA_character_,
            matched_gene_name = NA_character_
        )
    }
    out$query_candidates <- normalize_lookup_query_candidates(query_candidates)
    out$best_alias_used <- normalize_lookup_alias(best_alias_used)
    out$display_gene_name <- choose_lookup_display_gene_name(
        out,
        query_candidates = out$query_candidates,
        best_alias_used = out$best_alias_used,
        input_gene = input_gene
    )
    out
}

resolve_external_alias_bridge <- function(input_gene, query_candidates, file_path,
                                          organism = NULL, taxid = NULL,
                                          search_fun = search_gene_in_file) {
    bridge_perf <- app_perf_new_run("ALIAS_BRIDGE")
    app_perf_mark(
        bridge_perf,
        sprintf(
            "start gene=%s candidates=%d file=%s",
            as.character(input_gene %||% ""),
            as.integer(length(query_candidates %||% character(0))),
            basename(as.character(file_path %||% ""))
        ),
        "ALIAS_BRIDGE"
    )
    query_candidates_used <- normalize_lookup_query_candidates(query_candidates)
    if (length(query_candidates_used) == 0) {
        query_candidates_used <- normalize_lookup_query_candidates(c(input_gene))
    }

    alias_candidates <- as.character(query_candidates_used)
    alias_candidates <- unique(vapply(alias_candidates, normalize_lookup_alias, character(1)))
    alias_candidates <- alias_candidates[nzchar(alias_candidates)]
    if (is_low_specific_gene_family_query(input_gene)) {
        alias_candidates <- alias_candidates[!vapply(
            alias_candidates,
            is_same_low_specific_family_alias,
            logical(1),
            input_gene = input_gene
        )]
        alias_candidates <- alias_candidates[!vapply(
            alias_candidates,
            is_ambiguous_low_specific_family_alias,
            logical(1),
            input_gene = input_gene
        )]
    }
    if (length(alias_candidates) == 0) {
        app_perf_mark(bridge_perf, "no alias candidates", "ALIAS_BRIDGE")
        return(list(
            found = FALSE,
            result = NULL,
            best_alias_used = "",
            query_candidates = query_candidates_used,
            alias_candidates = character(0),
            alias_scores = numeric(0)
        ))
    }

    alias_scores <- vapply(alias_candidates, lookup_alias_quality_score, numeric(1), input_gene = input_gene)
    ord <- order(-alias_scores, nchar(alias_candidates), tolower(alias_candidates))
    alias_candidates <- alias_candidates[ord]
    alias_scores <- alias_scores[ord]
    app_perf_mark(
        bridge_perf,
        sprintf("ranked alias candidates=%d top=%s", as.integer(length(alias_candidates)), as.character(alias_candidates[1] %||% "")),
        "ALIAS_BRIDGE"
    )

    best_res <- NULL
    best_total <- -Inf
    best_alias_used <- ""
    local_hits <- 0L
    for (k in seq_along(alias_candidates)) {
        cand <- alias_candidates[k]
        cand_res <- search_fun(
            file_path,
            cand,
            show_diagnostics = FALSE,
            match_mode = "exact",
            return_meta = TRUE,
            include_bridge_tokens = FALSE
        )
        if (is.null(cand_res$data) || nrow(cand_res$data) == 0) {
            next
        }
        local_hits <- local_hits + 1L
        if (!is_alias_result_compatible(input_gene, cand, cand_res, organism = organism, taxid = taxid)) {
            next
        }

        cand_rank <- classify_lookup_result_rank(cand_res)
        cand_total <- cand_rank * 100 + alias_scores[k] + lookup_result_name_penalty(cand_res)
        if (is.null(best_res) || cand_total > best_total) {
            best_res <- cand_res
            best_total <- cand_total
            best_alias_used <- cand
        }

        if (cand_rank >= 3L && alias_scores[k] >= 2 && lookup_result_name_penalty(cand_res) >= 0) {
            break
        }
    }

    if (!is.null(best_res) && !is.null(best_res$data) && nrow(best_res$data) > 0) {
        app_perf_mark(
            bridge_perf,
            sprintf(
                "resolved alias=%s local_hits=%d matched=%s",
                as.character(best_alias_used %||% ""),
                as.integer(local_hits),
                as.character(best_res$matched_gene_name %||% "")
            ),
            "ALIAS_BRIDGE"
        )
    } else {
        app_perf_mark(
            bridge_perf,
            sprintf("no exact local alias match local_hits=%d", as.integer(local_hits)),
            "ALIAS_BRIDGE"
        )
    }

    list(
        found = !is.null(best_res) && !is.null(best_res$data) && nrow(best_res$data) > 0,
        result = best_res,
        best_alias_used = best_alias_used,
        query_candidates = query_candidates_used,
        alias_candidates = alias_candidates,
        alias_scores = alias_scores
    )
}

extract_gene_candidates_from_attr <- function(attr) {
    attrs <- parse_gff_attributes(attr)
    if (length(attrs) == 0) {
        return(character())
    }
    keys <- c("id", "name", "gene_id", "gene", "gene_synonym", "dbxref", "alias", "locus", "locus_tag", "transcript_id", "transcript", "protein_id", "description", "product", "note")
    keys_present <- intersect(keys, names(attrs))
    vals <- character()
    for (k in keys_present) {
        raw_vals <- attrs[[k]]
        split_pattern <- if (k %in% c("description", "product", "note")) "[,|]" else "[,| ]"
        split_vals <- unlist(strsplit(raw_vals, split_pattern))
        if (k %in% c("description", "product", "note")) {
            # For description fields: only add raw value + specific gene-like tokens
            # Do NOT add arbitrary split fragments as they cause false matches
            vals <- c(vals, raw_vals)

            # Also add the description text STRIPPED of [Source:...] / [...] metadata
            # suffixes. This is critical for matching aliases like
            # "Sodium transporter HKT1;5" against descriptions like
            # "Sodium transporter HKT1%3B5 [Source:UniProtKB/TrEMBL%3BAcc:B8A6S2]"
            cleaned <- trimws(sub("\\s*\\[.*\\]\\s*$", "", raw_vals))
            if (nzchar(cleaned) && cleaned != raw_vals) {
                vals <- c(vals, cleaned)
            }

            # Extract specific gene-identifier patterns
            os_g <- unlist(regmatches(raw_vals, gregexpr("\\bOs\\d{2}g\\d{5,}\\b", raw_vals, ignore.case = TRUE)))
            loc_os <- unlist(regmatches(raw_vals, gregexpr("\\bLOC_Os\\d{2}g\\d{5,}\\b", raw_vals, ignore.case = TRUE)))
            vals <- c(vals, os_g, loc_os)

            # Extract parenthesized content, but ONLY if it looks like a gene/accession ID
            parens <- unlist(regmatches(raw_vals, gregexpr("\\(([^\\s)]+)\\)", raw_vals)))
            parens <- gsub("[()]", "", parens)
            # Only keep tokens that look like identifiers (alphanumeric with at least one letter and one digit, or Os-like)
            is_gene_like <- grepl("^[A-Za-z]+[0-9]", parens) | grepl("^[0-9]+[A-Za-z]", parens) | grepl("^Os\\d{2}g", parens, ignore.case = TRUE)
            vals <- c(vals, parens[is_gene_like])
        } else {
            vals <- c(vals, raw_vals, split_vals[nzchar(split_vals)])
        }
    }
    if (length(vals) == 0) vals <- safe_url_decode(attr)
    unique(vals[nzchar(trimws(vals))])
}

extract_bridge_gene_like_tokens_from_attr <- function(attr) {
    attrs <- parse_gff_attributes(attr)
    if (length(attrs) == 0) {
        return(character(0))
    }

    desc_keys <- intersect(c("description", "product", "note"), names(attrs))
    if (length(desc_keys) == 0) {
        return(character(0))
    }

    out <- character(0)
    token_pattern <- "\\b[A-Za-z][A-Za-z0-9]*(?:[;._-][A-Za-z0-9]+)*\\b"
    for (k in desc_keys) {
        raw_vals <- as.character(attrs[[k]] %||% character(0))
        raw_vals <- raw_vals[nzchar(trimws(raw_vals))]
        if (length(raw_vals) == 0) {
            next
        }

        cleaned_vals <- trimws(sub("\\s*\\[.*\\]\\s*$", "", safe_url_decode(raw_vals)))
        cleaned_vals <- cleaned_vals[nzchar(cleaned_vals)]
        if (length(cleaned_vals) == 0) {
            next
        }

        for (txt in cleaned_vals) {
            matches <- unlist(regmatches(txt, gregexpr(token_pattern, txt, perl = TRUE)))
            if (length(matches) == 0) {
                next
            }
            keep <- vapply(matches, function(tok) {
                tok_trim <- trimws(as.character(tok %||% ""))
                if (!nzchar(tok_trim)) {
                    return(FALSE)
                }
                if (nchar(tok_trim) < 3 || nchar(tok_trim) > 40) {
                    return(FALSE)
                }
                grepl("[A-Za-z]", tok_trim) && grepl("[0-9]", tok_trim)
            }, logical(1))
            out <- c(out, matches[keep])
        }
    }

    unique(out[nzchar(trimws(out))])
}

search_gene_rows_via_bridge_descriptions <- function(attr_vec, gene_names) {
    attrs <- as.character(attr_vec %||% character(0))
    genes <- unique(as.character(gene_names %||% character(0)))
    genes <- genes[nzchar(trimws(genes))]
    if (length(attrs) == 0 || length(genes) == 0) {
        return(integer(0))
    }

    attrs_dec <- safe_url_decode(attrs)
    hits <- rep(FALSE, length(attrs_dec))
    for (gene_txt in genes) {
        gene_dec <- trimws(safe_url_decode(gene_txt))
        if (!nzchar(gene_dec)) {
            next
        }
        pat <- paste0("(^|[^A-Za-z0-9])", escape_regex(gene_dec), "([^A-Za-z0-9]|$)")
        hits <- hits | grepl(pat, attrs_dec, perl = TRUE, ignore.case = TRUE)
    }
    which(hits)
}

escape_regex <- function(string) gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)

empty_gff_df <- function() {
    data.frame(
        seqid = character(),
        source = character(),
        type = character(),
        start = numeric(),
        end = numeric(),
        score = character(),
        strand = character(),
        phase = character(),
        attributes = character(),
        stringsAsFactors = FALSE
    )
}

parse_gff_lines_to_df <- function(lines) {
    if (length(lines) == 0) {
        return(empty_gff_df())
    }
    keep <- !startsWith(lines, "#") & nzchar(trimws(lines))
    if (!any(keep)) {
        return(empty_gff_df())
    }
    data_lines <- lines[keep]
    fields <- strsplit(data_lines, "\t", fixed = TRUE)
    ncols <- lengths(fields)
    valid <- ncols >= 9
    if (!any(valid)) {
        return(empty_gff_df())
    }
    fields <- fields[valid]
    ncols <- ncols[valid]

    # Vectorized: build matrix for first 8 columns, handle col 9+ separately
    mat <- do.call(rbind,  lapply(fields, function(x) x[1:8]))
    # Column 9+: paste remaining columns (handles embedded tabs in attributes)
    attr_col <- vapply(seq_along(fields), function(i) {
        paste(fields[[i]][9:ncols[i]], collapse = "\t")
    }, character(1))

    data.frame(
        seqid = mat[, 1],
        source = mat[, 2],
        type = mat[, 3],
        start = suppressWarnings(as.numeric(mat[, 4])),
        end = suppressWarnings(as.numeric(mat[, 5])),
        score = mat[, 6],
        strand = mat[, 7],
        phase = mat[, 8],
        attributes = attr_col,
        stringsAsFactors = FALSE
    )
}

stream_gff_gene_rows <- function(file_path, chunk_size = 50000L) {
    if (is.null(file_path) || !nzchar(file_path) || !file.exists(file_path)) {
        return(empty_gff_df())
    }
    con <- if (grepl("\\.(gz|bgz)$", file_path, ignore.case = TRUE)) gzfile(file_path, open = "rt") else file(file_path, open = "r")
    on.exit(close(con), add = TRUE)

    gene_pieces <- list()
    cds_pieces <- list()
    gene_idx <- 0L
    cds_idx <- 0L
    # Regex to match the 3rd tab-delimited column being "gene" or "CDS"
    gene_pattern <- "^[^\t]+\t[^\t]+\tgene\t"
    cds_pattern <- "^[^\t]+\t[^\t]+\t[Cc][Dd][Ss]\t"
    repeat {
        lines <- readLines(con, n = as.integer(chunk_size), warn = FALSE)
        if (length(lines) == 0) break
        # Pre-filter with vectorized grep — skip parsing non-gene lines entirely
        gene_mask <- grepl(gene_pattern, lines, perl = TRUE)
        if (any(gene_mask)) {
            parsed <- parse_gff_lines_to_df(lines[gene_mask])
            if (nrow(parsed) > 0) {
                gene_idx <- gene_idx + 1L
                gene_pieces[[gene_idx]] <- parsed
            }
        } else {
            cds_mask <- grepl(cds_pattern, lines, perl = TRUE)
            if (any(cds_mask)) {
                parsed <- parse_gff_lines_to_df(lines[cds_mask])
                if (nrow(parsed) > 0) {
                    cds_idx <- cds_idx + 1L
                    cds_pieces[[cds_idx]] <- parsed
                }
            }
        }
    }
    if (length(gene_pieces) > 0) {
        return(do.call(rbind, gene_pieces))
    }
    if (length(cds_pieces) > 0) {
        return(do.call(rbind, cds_pieces))
    }
    empty_gff_df()
}

build_gene_lookup_maps <- function(gene_attrs) {
    norm_list <- vector("list", length(gene_attrs))
    comp_list <- vector("list", length(gene_attrs))
    for (i in seq_along(gene_attrs)) {
        cands <- extract_gene_candidates_from_attr(gene_attrs[[i]])
        n <- unique(normalize_gene_token(cands))
        c <- unique(normalize_gene_compact(cands))
        norm_list[[i]] <- n[nzchar(n)]
        comp_list[[i]] <- c[nzchar(c)]
    }
    norm_tokens <- unlist(norm_list, use.names = FALSE)
    comp_tokens <- unlist(comp_list, use.names = FALSE)
    norm_rows <- if (length(norm_tokens) > 0) rep(seq_along(gene_attrs), lengths(norm_list)) else integer(0)
    comp_rows <- if (length(comp_tokens) > 0) rep(seq_along(gene_attrs), lengths(comp_list)) else integer(0)
    norm_map <- if (length(norm_tokens) > 0) lapply(split(norm_rows, norm_tokens), unique) else list()
    comp_map <- if (length(comp_tokens) > 0) lapply(split(comp_rows, comp_tokens), unique) else list()
    list(norm_map = norm_map, comp_map = comp_map)
}

build_gff_gene_light_index <- function(file_path) {
    key <- gff_cache_key(file_path)
    cached_idx <- cache_env_get(.gff_gene_light_index_cache, key, default = NULL)
    if (!is.null(cached_idx)) {
        ensure_gff_autocomplete_cache(file_path, cached_idx, base_dir = ".")
        return(cached_idx)
    }

    idx_disk <- load_gff_index_from_disk(file_path, cache_kind = "gene_light", base_dir = ".")
    if (!is.null(idx_disk) && is.list(idx_disk) && !is.null(idx_disk$genes_df) && !is.null(idx_disk$norm_map)) {
        ensure_gff_autocomplete_cache(file_path, idx_disk, base_dir = ".")
        idx_disk <- slim_gff_gene_light_index(idx_disk)
        cache_env_set(
            .gff_gene_light_index_cache,
            key,
            idx_disk,
            max_size = annotation_memory_cache_limits$gene_light_max_entries,
            max_bytes = annotation_memory_cache_limits$gene_light_max_bytes
        )
        return(idx_disk)
    }

    genes_df <- stream_gff_gene_rows(file_path)
    attrs <- as.character(genes_df$attributes %||% rep("", nrow(genes_df)))
    maps <- build_gene_lookup_maps(attrs)
    idx <- c(
        list(
            genes_df = genes_df,
            gene_rows = seq_len(nrow(genes_df))
        ),
        maps
    )
    idx <- compact_gff_gene_light_index(slim_gff_gene_light_index(idx))
    save_gff_index_to_disk(file_path, idx, cache_kind = "gene_light", base_dir = ".")
    ensure_gff_autocomplete_cache(file_path, idx, base_dir = ".")
    cache_env_set(
        .gff_gene_light_index_cache,
        key,
        idx,
        max_size = annotation_memory_cache_limits$gene_light_max_entries,
        max_bytes = annotation_memory_cache_limits$gene_light_max_bytes
    )
    idx
}

load_gff_gene_light_index_if_available <- function(file_path, base_dir = ".") {
    key <- gff_cache_key(file_path)
    cached_idx <- cache_env_get(.gff_gene_light_index_cache, key, default = NULL)
    if (!is.null(cached_idx)) {
        ensure_gff_autocomplete_cache(file_path, cached_idx, base_dir = base_dir)
        return(cached_idx)
    }
    idx_disk <- load_gff_index_from_disk(file_path, cache_kind = "gene_light", base_dir = base_dir)
    if (!is.null(idx_disk) && is.list(idx_disk) && !is.null(idx_disk$genes_df) && !is.null(idx_disk$norm_map)) {
        ensure_gff_autocomplete_cache(file_path, idx_disk, base_dir = base_dir)
        idx_disk <- slim_gff_gene_light_index(idx_disk)
        cache_env_set(
            .gff_gene_light_index_cache,
            key,
            idx_disk,
            max_size = annotation_memory_cache_limits$gene_light_max_entries,
            max_bytes = annotation_memory_cache_limits$gene_light_max_bytes
        )
        return(idx_disk)
    }
    NULL
}

resolve_seqname_in_vector <- function(seqid, seq_names = character(0)) {
    seqid <- as.character(seqid %||% "")
    if (!nzchar(seqid) || length(seq_names) == 0) {
        return(NULL)
    }
    candidates <- unique(c(
        seqid,
        paste0("chr", seqid),
        sub("^chr", "", seqid, ignore.case = TRUE),
        paste0("Chr", seqid),
        paste0("chromosome:IRGSP-1.0:", seqid),
        paste0("chromosome:IRGSP:", seqid)
    ))
    hit <- seq_names[seq_names %in% candidates]
    if (length(hit) > 0) {
        return(hit[1])
    }
    for (cand in candidates) {
        matches <- grep(paste0("(^|:)\\Q", cand, "\\E($|:|\\s)"), seq_names, value = TRUE)
        if (length(matches) > 0) {
            return(matches[1])
        }
    }
    NULL
}

scan_tabix_region_gff <- function(file_path, seqid, start_pos, end_pos) {
    if (!is_tabix_annotation_file(file_path)) {
        return(empty_gff_df())
    }
    seqid <- as.character(seqid %||% "")
    if (!nzchar(seqid)) {
        return(empty_gff_df())
    }
    st <- max(1L, as.integer(start_pos %||% 1L))
    en <- max(st, as.integer(end_pos %||% st))

    idx_override <- get_tabix_index_override(file_path)
    seq_names <- get_tabix_seqnames_cached(file_path)
    resolved_seqname <- resolve_seqname_in_vector(seqid, seq_names = seq_names) %||% seqid
    tabix_bin <- Sys.which("tabix")
    adjacent_indexes <- normalizePath(c(paste0(file_path, ".tbi"), paste0(file_path, ".csi")), winslash = "/", mustWork = FALSE)
    can_use_cli <- nzchar(tabix_bin) && (!nzchar(idx_override) || normalizePath(idx_override, winslash = "/", mustWork = FALSE) %in% adjacent_indexes)
    lines <- if (isTRUE(can_use_cli)) {
        region_text <- sprintf("%s:%d-%d", resolved_seqname, st, en)
        tryCatch(
            as.character(system2(tabix_bin, c(shQuote(file_path), shQuote(region_text)), stdout = TRUE, stderr = FALSE)),
            error = function(e) character(0)
        )
    } else if (requireNamespace("Rsamtools", quietly = TRUE)) {
        tbx <- if (nzchar(idx_override) && file.exists(idx_override)) {
            Rsamtools::TabixFile(file_path, index = idx_override)
        } else {
            Rsamtools::TabixFile(file_path)
        }
        region <- GenomicRanges::GRanges(
            seqnames = resolved_seqname,
            ranges = IRanges::IRanges(start = st, end = en)
        )
        tryCatch(
            {
                raw <- Rsamtools::scanTabix(tbx, param = region)
                if (length(raw) == 0) character(0) else raw[[1]]
            },
            error = function(e) character(0)
        )
    } else {
        character(0)
    }
    parse_gff_lines_to_df(lines)
}

extract_gene_block_from_df <- function(df, gene_id) {
    if (is.null(df) || nrow(df) == 0 || is.null(gene_id) || !nzchar(gene_id)) {
        return(data.frame())
    }

    # Build variant list: decoded form, URL-encoded form, and original as-is
    gene_id_decoded <- tryCatch(safe_url_decode(gene_id), error = function(e) gene_id)
    gene_id_encoded <- tryCatch(utils::URLencode(gene_id_decoded, reserved = TRUE), error = function(e) gene_id)
    # Also try encoding only the semicolon which is the most common case
    gene_id_semi_enc <- gsub(";", "%3B", gene_id, fixed = TRUE)

    variants <- unique(c(gene_id, gene_id_decoded, gene_id_encoded, gene_id_semi_enc))
    variants <- variants[nzchar(variants)]

    # Build a single regex pattern that matches any variant
    variants_esc <- vapply(variants, escape_regex, character(1))
    id_pattern <- paste0("(", paste(variants_esc, collapse = "|"), ")")

    direct_children <- df |>
        dplyr::filter(grepl(paste0("ID=", id_pattern, "(;|$)"), attributes) |
            grepl(paste0("Parent=", id_pattern, "(;|$)"), attributes))

    child_ids <- stringr::str_extract(direct_children$attributes, "ID=[^;]+") |>
        stringr::str_remove("ID=") |>
        stats::na.omit()
    grandchildren <- if (length(child_ids) > 0) {
        # Child IDs may also be URL-encoded, build variants for each
        child_variants <- unique(c(child_ids, tryCatch(safe_url_decode(child_ids), error = function(e) character(0))))
        child_variants <- child_variants[!is.na(child_variants) & nzchar(child_variants)]
        child_pattern <- paste0("(", paste(escape_regex(child_variants), collapse = "|"), ")")
        df |> dplyr::filter(grepl(paste0("Parent=", child_pattern, "(;|$)"), attributes))
    } else {
        data.frame()
    }
    result_df <- as.data.frame(dplyr::distinct(dplyr::bind_rows(direct_children, grandchildren)))
    if (nrow(result_df) == 0) {
        return(result_df)
    }
    colnames(result_df) <- paste0("V", 1:9)
    result_df
}

subset_gff_df_to_gene_region <- function(df, target_gene_row, padding_bp = 0L) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
        is.null(target_gene_row) || !is.data.frame(target_gene_row) || nrow(target_gene_row) == 0L) {
        return(df)
    }
    chr <- as.character(target_gene_row$seqid[1] %||% target_gene_row$V1[1] %||% "")
    start_pos <- suppressWarnings(as.numeric(target_gene_row$start[1] %||% target_gene_row$V4[1] %||% NA_real_))
    end_pos <- suppressWarnings(as.numeric(target_gene_row$end[1] %||% target_gene_row$V5[1] %||% NA_real_))
    if (!nzchar(chr) || !is.finite(start_pos) || !is.finite(end_pos)) {
        return(df)
    }
    if (end_pos < start_pos) {
        tmp <- start_pos
        start_pos <- end_pos
        end_pos <- tmp
    }
    pad <- suppressWarnings(as.numeric(padding_bp %||% 0))
    if (!is.finite(pad) || pad < 0) {
        pad <- 0
    }
    region_start <- max(1, start_pos - pad)
    region_end <- end_pos + pad

    seq_col <- if ("seqid" %in% colnames(df)) "seqid" else if ("V1" %in% colnames(df)) "V1" else ""
    start_col <- if ("start" %in% colnames(df)) "start" else if ("V4" %in% colnames(df)) "V4" else ""
    end_col <- if ("end" %in% colnames(df)) "end" else if ("V5" %in% colnames(df)) "V5" else ""
    if (!nzchar(seq_col) || !nzchar(start_col) || !nzchar(end_col)) {
        return(df)
    }

    row_start <- suppressWarnings(as.numeric(df[[start_col]]))
    row_end <- suppressWarnings(as.numeric(df[[end_col]]))
    keep <- as.character(df[[seq_col]]) == chr &
        is.finite(row_start) &
        is.finite(row_end) &
        row_end >= region_start &
        row_start <= region_end
    if (!any(keep, na.rm = TRUE)) {
        return(df)
    }
    df[keep %in% TRUE, , drop = FALSE]
}

build_gff_gene_index <- function(file_path) {
    key <- gff_cache_key(file_path)
    cached_idx <- cache_env_get(.gff_gene_index_cache, key, default = NULL)
    if (!is.null(cached_idx)) {
        return(cached_idx)
    }

    df <- load_gff_cached(file_path)
    gene_rows <- which(tolower(df$type) == "gene")
    if (length(gene_rows) == 0L) {
        # Fallback for annotations that only define CDS-level features.
        gene_rows <- which(tolower(df$type) == "cds")
    }
    attrs <- as.character(df$attributes[gene_rows] %||% rep("", length(gene_rows)))
    maps <- build_gene_lookup_maps(attrs)
    idx <- c(
        list(df = df, gene_rows = gene_rows),
        maps
    )
    cache_env_set(
        .gff_gene_index_cache,
        key,
        idx,
        max_size = annotation_memory_cache_limits$gene_index_max_entries,
        max_bytes = annotation_memory_cache_limits$gene_index_max_bytes
    )
    idx
}

search_gene_rows_with_index <- function(idx, gene_names, match_mode = c("flex", "exact")) {
    match_mode <- match.arg(match_mode)
    gene_names <- unique(as.character(gene_names))[nzchar(unique(as.character(gene_names)))]
    if (length(gene_names) == 0) {
        return(integer(0))
    }
    gene_norms <- normalize_gene_token(gene_names)
    gene_compacts <- normalize_gene_compact(gene_names)
    gene_norms <- unique(gene_norms[nzchar(gene_norms)])
    gene_compacts <- unique(gene_compacts[nzchar(gene_compacts)])

    collect_lookup_hits <- function() {
        norm_hits <- if (length(gene_norms) > 0) {
            gene_lookup_hits(idx, gene_norms)
        } else {
            integer(0)
        }
        comp_hits <- if (length(gene_compacts) > 0) {
            gene_lookup_hits(idx, gene_compacts, "comp")
        } else {
            integer(0)
        }
        rel <- unique(c(norm_hits, comp_hits))
        rel <- suppressWarnings(as.integer(rel))
        rel[!is.na(rel) & rel >= 1L & rel <= length(idx$gene_rows)]
    }

    if (match_mode == "exact") {
        rel <- collect_lookup_hits()
        return(idx$gene_rows[rel])
    }

    # Flex mode uses the same normalized/compact alias tokens as exact mode.
    # Querying the token maps avoids scanning every gene row on every no-hit search.
    rel <- sort(collect_lookup_hits())
    idx$gene_rows[rel]
}

normalize_partial_gene_query <- function(x) {
    comp <- normalize_gene_compact(x)
    comp <- gsub("[^a-z0-9]+", "", comp)
    trimws(comp)
}

# Vectorize the ordinary names while retaining scalar URL-decoding semantics:
# a malformed escape in one name must not change the other names in the batch.
# Keep vapply's names, since some suggestion tables inherit them as row names.
normalize_partial_gene_choices <- function(choices) {
    vals <- as.character(choices)
    keys <- rep(NA_character_, length(vals))
    escaped <- !is.na(vals) & grepl("%", vals, fixed = TRUE)
    keys[!escaped] <- normalize_partial_gene_query(vals[!escaped])
    if (any(escaped)) {
        keys[escaped] <- vapply(vals[escaped], normalize_partial_gene_query, character(1))
    }
    stats::setNames(keys, names(choices) %||% vals)
}

alias_sqlite_prefix_upper_bound <- function(prefix) {
    prefix <- as.character(prefix %||% "")
    if (length(prefix) == 0L || is.na(prefix[[1L]])) return("")
    prefix <- prefix[[1L]]
    chars <- utf8ToInt(prefix)
    if (length(chars) == 0L) return("")
    intToUtf8(c(chars[-length(chars)], chars[[length(chars)]] + 1L))
}

partial_alias_sqlite_index_row <- function(indexes, index_name) {
    if (!is.data.frame(indexes) || !("name" %in% names(indexes))) {
        return(data.frame())
    }
    indexes[as.character(indexes$name) == as.character(index_name), , drop = FALSE]
}

partial_alias_sqlite_has_column_index <- function(con, indexes, index_name, column_name) {
    index_row <- partial_alias_sqlite_index_row(indexes, index_name)
    if (nrow(index_row) != 1L ||
        ("partial" %in% names(index_row) && !identical(as.integer(index_row$partial), 0L))) {
        return(FALSE)
    }
    safe_name <- gsub("'", "''", as.character(index_name), fixed = TRUE)
    info <- tryCatch(
        DBI::dbGetQuery(con, sprintf("PRAGMA index_xinfo('%s')", safe_name)),
        error = function(e) data.frame()
    )
    if (!is.data.frame(info) || !all(c("name", "key") %in% names(info))) {
        return(FALSE)
    }
    key_rows <- info[as.integer(info$key) == 1L, , drop = FALSE]
    if (!identical(as.character(key_rows$name), as.character(column_name))) {
        return(FALSE)
    }
    if ("coll" %in% names(key_rows) &&
        !identical(toupper(as.character(key_rows$coll)), "BINARY")) {
        return(FALSE)
    }
    !("desc" %in% names(key_rows)) || identical(as.integer(key_rows$desc), 0L)
}

partial_alias_sqlite_has_symbol_upper_index <- function(con, indexes) {
    index_name <- "idx_local_symbol_upper"
    index_row <- partial_alias_sqlite_index_row(indexes, index_name)
    if (nrow(index_row) != 1L ||
        ("partial" %in% names(index_row) && !identical(as.integer(index_row$partial), 0L))) {
        return(FALSE)
    }
    info <- tryCatch(
        DBI::dbGetQuery(con, "PRAGMA index_xinfo('idx_local_symbol_upper')"),
        error = function(e) data.frame()
    )
    if (!is.data.frame(info) || !("key" %in% names(info))) {
        return(FALSE)
    }
    key_rows <- info[as.integer(info$key) == 1L, , drop = FALSE]
    if (nrow(key_rows) != 1L ||
        ("cid" %in% names(key_rows) && !identical(as.integer(key_rows$cid), -2L)) ||
        ("coll" %in% names(key_rows) && !identical(toupper(as.character(key_rows$coll)), "BINARY")) ||
        ("desc" %in% names(key_rows) && !identical(as.integer(key_rows$desc), 0L))) {
        return(FALSE)
    }
    index_sql <- tryCatch(
        DBI::dbGetQuery(
            con,
            "SELECT sql FROM sqlite_master WHERE type = 'index' AND name = 'idx_local_symbol_upper' LIMIT 1"
        )$sql[1],
        error = function(e) ""
    )
    normalized_sql <- toupper(gsub("[^A-Za-z0-9_()]+", "", as.character(index_sql %||% "")))
    identical(
        normalized_sql,
        "CREATEINDEXIDX_LOCAL_SYMBOL_UPPERONALIAS_INDEX(UPPER(LOCAL_SYMBOL))"
    )
}

query_partial_alias_rows_sqlite <- function(con, like_value, type_sql, row_limit_sql = "",
                                            prefix_value = NULL) {
    select_sql <- paste(
        "SELECT query_term_original, query_term_clean_strict, query_term_upper,",
        "local_gene_id, local_symbol, term_type, confidence, source_db"
    )
    order_sql <- paste(
        "ORDER BY CASE confidence WHEN 'HIGH' THEN 1 WHEN 'MEDIUM' THEN 2 ELSE 3 END,",
        "LENGTH(query_term_original), query_term_original"
    )
    like_sql <- paste(
        "(query_term_clean_strict LIKE ?1 OR query_term_upper LIKE ?1 OR",
        "UPPER(local_symbol) LIKE ?1)"
    )
    legacy_sql <- sprintf(
        "%s FROM alias_index WHERE term_type IN (%s) AND LENGTH(query_term_original) <= 100 AND %s %s%s",
        select_sql,
        type_sql,
        like_sql,
        order_sql,
        row_limit_sql
    )
    prefix <- trimws(as.character(prefix_value %||% ""))
    has_prefix_indexes <- FALSE
    if (nzchar(prefix) && !is.null(con) && requireNamespace("DBI", quietly = TRUE)) {
        has_prefix_indexes <- isTRUE(tryCatch({
            indexes <- DBI::dbGetQuery(con, "PRAGMA index_list('alias_index')")
            partial_alias_sqlite_has_column_index(
                con, indexes, "idx_clean_strict", "query_term_clean_strict"
            ) &&
                partial_alias_sqlite_has_column_index(
                    con, indexes, "idx_upper", "query_term_upper"
                ) &&
                partial_alias_sqlite_has_symbol_upper_index(con, indexes)
        }, error = function(e) FALSE))
    }
    if (isTRUE(has_prefix_indexes)) {
        legacy_plan <- tryCatch(
            DBI::dbGetQuery(con, paste("EXPLAIN QUERY PLAN", legacy_sql), params = list(like_value)),
            error = function(e) data.frame()
        )
        legacy_uses_term_type_index <- is.data.frame(legacy_plan) &&
            "detail" %in% names(legacy_plan) &&
            any(grepl("USING INDEX idx_term_type", as.character(legacy_plan$detail), fixed = TRUE))
        optimized_order_sql <- paste0(
            order_sql,
            if (isTRUE(legacy_uses_term_type_index)) ", term_type, a.rowid" else ", a.rowid"
        )
        sql <- sprintf(
            paste0(
                "WITH candidate_rowids AS (",
                "SELECT rowid FROM alias_index INDEXED BY idx_clean_strict ",
                "WHERE query_term_clean_strict >= ?2 AND query_term_clean_strict < ?3 UNION ",
                "SELECT rowid FROM alias_index INDEXED BY idx_upper ",
                "WHERE query_term_upper >= ?2 AND query_term_upper < ?3 UNION ",
                "SELECT rowid FROM alias_index INDEXED BY idx_local_symbol_upper ",
                "WHERE UPPER(local_symbol) >= ?2 AND UPPER(local_symbol) < ?3) ",
                "%s FROM candidate_rowids c CROSS JOIN alias_index a ON a.rowid = c.rowid ",
                "WHERE term_type IN (%s) AND LENGTH(query_term_original) <= 100 AND %s %s%s"
            ),
            select_sql,
            type_sql,
            like_sql,
            optimized_order_sql,
            row_limit_sql
        )
        optimized_result <- tryCatch(
            DBI::dbGetQuery(
                con,
                sql,
                params = list(like_value, prefix, alias_sqlite_prefix_upper_bound(prefix))
            ),
            error = function(e) e
        )
        if (!inherits(optimized_result, "error")) {
            return(optimized_result)
        }
    }
    tryCatch(DBI::dbGetQuery(con, legacy_sql, params = list(like_value)), error = function(e) data.frame())
}

extract_partial_gene_display_name <- function(attr) {
    attrs <- parse_gff_attributes(attr %||% "")
    vals <- c(
        attrs[["gene_name"]][1],
        attrs[["gene"]][1],
        attrs[["name"]][1],
        attrs[["alias"]][1],
        attrs[["gene_synonym"]][1],
        attrs[["locus_tag"]][1],
        attrs[["gene_id"]][1],
        attrs[["id"]][1]
    )
    vals <- trimws(safe_url_decode(as.character(vals %||% character(0))))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0L) {
        return("")
    }
    vals[1]
}

empty_partial_gene_suggestions_df <- function(source_labels = FALSE, source_label_preview = FALSE) {
    out <- data.frame(
        gene_name = character(0),
        file_label = character(0),
        match_type = character(0),
        score = numeric(0),
        source_count = integer(0),
        local_gene_id = character(0),
        local_symbol = character(0),
        term_type = character(0),
        source_db = character(0),
        confidence = character(0),
        match_role = character(0),
        requires_confirmation = logical(0),
        stringsAsFactors = FALSE
    )
    if (isTRUE(source_labels)) out$source_labels <- character(0)
    if (isTRUE(source_label_preview)) out$source_label_preview <- character(0)
    out
}

normalize_partial_gene_suggestions_df <- function(df, source_labels = FALSE, source_label_preview = FALSE) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
        return(empty_partial_gene_suggestions_df(
            source_labels = source_labels,
            source_label_preview = source_label_preview
        ))
    }
    empty <- empty_partial_gene_suggestions_df(
        source_labels = source_labels || "source_labels" %in% names(df),
        source_label_preview = source_label_preview || "source_label_preview" %in% names(df)
    )
    for (nm in names(empty)) {
        if (!nm %in% names(df)) {
            if (identical(nm, "score")) {
                df[[nm]] <- rep(NA_real_, nrow(df))
            } else if (identical(nm, "source_count")) {
                df[[nm]] <- rep(NA_integer_, nrow(df))
            } else if (identical(nm, "requires_confirmation")) {
                df[[nm]] <- rep(FALSE, nrow(df))
            } else {
                df[[nm]] <- rep("", nrow(df))
            }
        }
    }
    df <- df[, names(empty), drop = FALSE]
    char_cols <- setdiff(names(df), c("score", "source_count", "requires_confirmation"))
    for (nm in char_cols) df[[nm]] <- as.character(df[[nm]] %||% "")
    df$score <- suppressWarnings(as.numeric(df$score))
    df$source_count <- suppressWarnings(as.integer(df$source_count))
    df$requires_confirmation <- as.logical(df$requires_confirmation %||% FALSE)
    df$requires_confirmation[is.na(df$requires_confirmation)] <- FALSE
    df
}

collapse_partial_gene_suggestions_by_locus <- function(df, query = "") {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
        out <- normalize_partial_gene_suggestions_df(df)
        out$alias_names <- character(0)
        return(out)
    }
    has_source_labels <- "source_labels" %in% names(df)
    has_source_preview <- "source_label_preview" %in% names(df)
    out <- normalize_partial_gene_suggestions_df(
        df,
        source_labels = has_source_labels,
        source_label_preview = has_source_preview
    )
    out$alias_names <- rep("", nrow(out))

    query_key <- normalize_partial_gene_query(query)
    gene_key <- normalize_partial_gene_query(out$gene_name)
    file_key <- tolower(trimws(as.character(out$file_label %||% "")))
    local_ids <- trimws(as.character(out$local_gene_id %||% ""))
    local_symbols <- trimws(as.character(out$local_symbol %||% ""))

    # Direct annotation-name matches may not carry a locus ID, while alias rows do.
    # Infer the missing ID only when the same official symbol maps unambiguously to
    # one locus inside the same organism/annotation source.
    known <- nzchar(local_ids) & nzchar(local_symbols)
    if (any(known)) {
        symbol_map <- split(local_ids[known], paste(file_key[known], normalize_partial_gene_query(local_symbols[known]), sep = "\r"))
        symbol_map <- lapply(symbol_map, function(ids) unique(ids[nzchar(ids)]))
        missing <- which(!nzchar(local_ids))
        for (idx in missing) {
            ids <- symbol_map[[paste(file_key[[idx]], gene_key[[idx]], sep = "\r")]] %||% character(0)
            if (length(ids) == 1L) {
                local_ids[[idx]] <- ids[[1L]]
                out$local_gene_id[[idx]] <- ids[[1L]]
            }
        }
    }

    identity_key <- ifelse(
        nzchar(local_ids),
        paste(file_key, "locus", local_ids, sep = "\r"),
        paste(file_key, "name", gene_key, sep = "\r")
    )
    groups <- split(seq_len(nrow(out)), identity_key)
    collapsed <- lapply(groups, function(idx) {
        rows <- out[idx, , drop = FALSE]
        row_gene_keys <- normalize_partial_gene_query(rows$gene_name)
        exact <- nzchar(query_key) & row_gene_keys == query_key
        match_exact <- tolower(as.character(rows$match_type %||% "")) == "exact"
        score <- suppressWarnings(as.numeric(rows$score %||% 0))
        score[!is.finite(score)] <- 0
        ord <- order(!(exact | match_exact), -score, nchar(as.character(rows$gene_name)), tolower(as.character(rows$gene_name)))
        best <- rows[ord[[1L]], , drop = FALSE]

        symbols <- unique(trimws(as.character(rows$local_symbol %||% "")))
        symbols <- symbols[nzchar(symbols)]
        preferred_symbols <- symbols[!grepl("^LOC[0-9]+$", symbols, ignore.case = TRUE)]
        canonical <- if (length(preferred_symbols) > 0L) preferred_symbols[[1L]] else ""
        if (nzchar(canonical)) {
            canonical_rows <- which(normalize_partial_gene_query(rows$gene_name) == normalize_partial_gene_query(canonical))
            if (length(canonical_rows) > 0L) {
                best <- rows[canonical_rows[[1L]], , drop = FALSE]
            }
            best$gene_name <- canonical
            best$local_symbol <- canonical
        }

        all_names <- unique(trimws(as.character(rows$gene_name %||% "")))
        all_names <- all_names[nzchar(all_names)]
        aliases <- all_names[normalize_partial_gene_query(all_names) != normalize_partial_gene_query(best$gene_name[[1L]])]
        best$alias_names <- paste(aliases, collapse = " | ")
        ids <- unique(trimws(as.character(rows$local_gene_id %||% "")))
        ids <- ids[nzchar(ids)]
        if (length(ids) > 0L) best$local_gene_id <- ids[[1L]]
        best$score <- max(score, na.rm = TRUE)
        best$match_type <- if (any(exact | match_exact)) {
            "exact"
        } else if (any(as.character(rows$match_type %||% "") == "prefix")) {
            "prefix"
        } else {
            "contains"
        }
        counts <- suppressWarnings(as.integer(rows$source_count %||% 1L))
        counts[!is.finite(counts) | counts < 1L] <- 1L
        best$source_count <- max(counts)
        best$requires_confirmation <- all(as.logical(rows$requires_confirmation %||% FALSE), na.rm = TRUE)
        best
    })
    collapsed <- do.call(rbind, collapsed)
    collapsed_exact <- tolower(as.character(collapsed$match_type %||% "")) == "exact"
    collapsed <- collapsed[order(!collapsed_exact, -suppressWarnings(as.numeric(collapsed$score %||% 0)), tolower(as.character(collapsed$gene_name))), , drop = FALSE]
    rownames(collapsed) <- NULL
    collapsed
}

is_verified_local_description_alias_match <- function(match_row) {
    if (is.null(match_row) || !is.data.frame(match_row) || nrow(match_row) == 0L) return(FALSE)
    row <- match_row[1, , drop = FALSE]
    local_gene_id <- trimws(as.character(row$local_gene_id[1] %||% ""))
    if (!nzchar(local_gene_id)) return(FALSE)
    term_type <- tolower(trimws(as.character(row$term_type[1] %||% "")))
    source_db <- toupper(trimws(as.character(row$source_db[1] %||% "")))
    evidence_source <- tolower(trimws(as.character(row$evidence_source[1] %||% "")))
    term_type %in% c("description", "product", "note") &&
        (identical(source_db, "GFF") || identical(evidence_source, "local_annotation"))
}

extract_partial_gene_display_names <- function(attrs) {
    attrs <- as.character(attrs %||% character(0))
    if (length(attrs) == 0L) return(character(0))

    pick <- function(pattern) {
        out <- stringr::str_match(attrs, pattern)[, 2]
        sanitize_gene_display_name_batch(out)
    }
    gene_name <- pick('(?:^|;|\\t)\\s*gene_name\\s*[= ]\\s*"?([^;"\\t]+)')
    gene_field <- pick("(?:^|;)\\s*gene=([^;]+)")
    name_field <- pick("(?:^|;)\\s*Name=([^;]+)")
    alias_field <- pick("(?:^|;)\\s*Alias=([^;]+)")
    synonym_field <- pick("(?:^|;)\\s*gene_synonym=([^;]+)")
    locus_tag <- pick("(?:^|;)\\s*locus_tag=([^;]+)")
    gene_id <- pick("(?:^|;)\\s*gene_id=([^;]+)")
    id_field <- pick("(?:^|;)\\s*ID=([^;]+)")

    if (length(synonym_field) > 0L) {
        synonym_field <- vapply(strsplit(as.character(synonym_field %||% ""), ",", fixed = TRUE), function(x) {
            vals <- trimws(as.character(x %||% character(0)))
            vals <- vals[nzchar(vals) & !is.na(vals)]
            if (length(vals) == 0L) "" else vals[[1L]]
        }, character(1))
        synonym_field <- sanitize_gene_display_name_batch(synonym_field)
    }

    candidates <- data.frame(
        gene_name = gene_name,
        gene = gene_field,
        name = name_field,
        alias = alias_field,
        synonym = synonym_field,
        locus_tag = locus_tag,
        gene_id = gene_id,
        id = id_field,
        stringsAsFactors = FALSE
    )
    out <- rep("", nrow(candidates))
    for (col in names(candidates)) {
        vals <- trimws(safe_url_decode(as.character(candidates[[col]] %||% "")))
        take <- !nzchar(out) & !is.na(vals) & nzchar(vals)
        out[take] <- vals[take]
    }
    out
}

find_partial_gene_suggestions_from_choices <- function(choices, query, file_label = NULL,
                                                       max_suggestions = 10L,
                                                       min_query_chars = 2L) {
    q_comp <- normalize_partial_gene_query(query)
    min_query_chars <- suppressWarnings(as.integer(min_query_chars %||% 2L))
    if (!is.finite(min_query_chars) || is.na(min_query_chars) || min_query_chars < 1L) {
        min_query_chars <- 2L
    }
    if (nchar(q_comp) < min_query_chars) {
        return(empty_partial_gene_suggestions_df())
    }
    max_suggestions <- suppressWarnings(as.numeric(max_suggestions %||% 10L))
    if (is.na(max_suggestions) || max_suggestions < 1) {
        max_suggestions <- 10L
    }
    display <- trimws(as.character(choices %||% character(0)))
    display <- display[!is.na(display) & nzchar(display) & nchar(display) <= 80]
    if (length(display) == 0L) {
        return(empty_partial_gene_suggestions_df())
    }
    comp <- normalize_partial_gene_choices(display)
    exact_hit <- comp == q_comp
    hit_prefix <- !exact_hit & startsWith(comp, q_comp)
    hit_contains <- !exact_hit & !hit_prefix & grepl(q_comp, comp, fixed = TRUE)
    hit <- exact_hit | hit_prefix | hit_contains
    if (!any(hit)) {
        return(empty_partial_gene_suggestions_df())
    }
    out <- data.frame(
        gene_name = display[hit],
        file_label = rep(as.character(file_label %||% ""), sum(hit)),
        match_type = ifelse(exact_hit[hit], "exact", ifelse(hit_prefix[hit], "prefix", "contains")),
        score = ifelse(exact_hit[hit], 160, ifelse(hit_prefix[hit], 100, 60)) -
            pmax(0, nchar(comp[hit]) - nchar(q_comp)),
        source_count = rep(1L, sum(hit)),
        stringsAsFactors = FALSE
    )
    out <- normalize_partial_gene_suggestions_df(out)
    out$key <- tolower(normalize_gene_compact(out$gene_name))
    out <- out[!duplicated(out$key), , drop = FALSE]
    out$key <- NULL
    out <- out[order(-out$score, nchar(out$gene_name), tolower(out$gene_name)), , drop = FALSE]
    if (is.finite(max_suggestions) && nrow(out) > max_suggestions) {
        out <- out[seq_len(as.integer(max_suggestions)), , drop = FALSE]
    }
    rownames(out) <- NULL
    normalize_partial_gene_suggestions_df(out)
}

find_partial_gene_suggestions_in_index <- function(file_path, query, file_label = NULL,
                                                   max_suggestions = 10L,
                                                   min_query_chars = 2L,
                                                   allow_build_index = FALSE) {
    p <- as.character(file_path %||% "")
    if (!nzchar(p) || !file.exists(p)) {
        return(empty_partial_gene_suggestions_df())
    }
    idx <- if (isTRUE(allow_build_index)) {
        tryCatch(build_gff_gene_light_index(p), error = function(e) NULL)
    } else {
        tryCatch(load_gff_gene_light_index_if_available(p, base_dir = "."), error = function(e) NULL)
    }
    if (is.null(idx) || !is.list(idx) || is.null(idx$genes_df) || !is.data.frame(idx$genes_df) || nrow(idx$genes_df) == 0L) {
        return(empty_partial_gene_suggestions_df())
    }
    q_comp <- normalize_partial_gene_query(query)
    row_idx <- seq_len(nrow(idx$genes_df))
    tokens <- gene_lookup_tokens(idx, "comp")
    if (length(tokens) > 0L) {
        tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
        hit_tokens <- tokens[startsWith(tokens, q_comp) | grepl(q_comp, tokens, fixed = TRUE)]
        if (length(hit_tokens) == 0L) {
            return(empty_partial_gene_suggestions_df())
        }
        rel <- unique(gene_lookup_hits(idx, hit_tokens, "comp"))
        rel <- suppressWarnings(as.integer(rel))
        rel <- rel[!is.na(rel) & rel >= 1L & rel <= nrow(idx$genes_df)]
        if (length(rel) == 0L) {
            return(empty_partial_gene_suggestions_df())
        }
        row_idx <- rel
    }
    attrs <- as.character(idx$genes_df$attributes[row_idx] %||% rep("", length(row_idx)))
    choices <- extract_partial_gene_display_names(attrs)
    find_partial_gene_suggestions_from_choices(
        choices = choices,
        query = query,
        file_label = file_label %||% basename(p),
        max_suggestions = max_suggestions,
        min_query_chars = min_query_chars
    )
}

find_partial_gene_alias_suggestions_sqlite <- function(annotation_paths, query, file_labels = NULL,
                                                       det_list = NULL, max_per_file = 20L,
                                                       min_query_chars = 2L, base_dir = ".") {
    if (!exists("load_alias_index_sqlite", mode = "function") || !requireNamespace("DBI", quietly = TRUE)) {
        return(empty_partial_gene_suggestions_df())
    }
    q_comp <- normalize_partial_gene_query(query)
    min_query_chars <- suppressWarnings(as.integer(min_query_chars %||% 2L))
    if (!is.finite(min_query_chars) || is.na(min_query_chars) || min_query_chars < 1L) min_query_chars <- 2L
    if (nchar(q_comp) < min_query_chars) {
        return(empty_partial_gene_suggestions_df())
    }
    paths <- as.character(annotation_paths %||% character(0))
    paths <- paths[nzchar(paths) & file.exists(paths)]
    if (length(paths) == 0L) {
        return(empty_partial_gene_suggestions_df())
    }
    labels <- as.character(file_labels %||% basename(paths))
    if (length(labels) != length(paths)) labels <- basename(paths)
    max_per_file <- suppressWarnings(as.numeric(max_per_file %||% 20L))
    if (is.na(max_per_file) || max_per_file < 1) max_per_file <- 20L
    dets <- det_list
    if (!is.list(dets)) dets <- vector("list", length(paths))
    if (length(dets) < length(paths)) length(dets) <- length(paths)

    allowed_types <- c(
        "name", "gene_name", "gene", "gene_symbol", "alias", "gene_synonym",
        "gene_synonyms", "synonym", "external_gene_name", "locus_tag",
        "id", "dbxref", "entrezgene_id", "ensembl_gene_id", "uniprot_id",
        "description", "product", "note"
    )
    type_sql <- paste(sprintf("'%s'", allowed_types), collapse = ",")
    q_like_prefix <- paste0(toupper(q_comp), "%")
    q_like_contains <- paste0("%", toupper(q_comp), "%")
    confidence_rank <- c(HIGH = 30L, MEDIUM = 20L, LOW = 10L)

    rows <- lapply(seq_along(paths), function(i) {
        det <- tryCatch(dets[[i]], error = function(e) NULL)
        det <- det %||% list()
        org_id <- trimws(as.character(det$species_id %||% det$preloaded_id %||% ""))
        if (!nzchar(org_id)) return(NULL)

        con <- tryCatch(load_alias_index_sqlite(organism_id = org_id, base_dir = base_dir), error = function(e) NULL)
        if (is.null(con)) return(NULL)

        query_alias_rows <- function(like_value) {
            row_limit_sql <- if (is.finite(max_per_file)) {
                sprintf(" LIMIT %d", as.integer(min(.Machine$integer.max, max_per_file * 4)))
            } else {
                ""
            }
            query_partial_alias_rows_sqlite(
                con = con,
                like_value = like_value,
                type_sql = type_sql,
                row_limit_sql = row_limit_sql,
                prefix_value = if (identical(like_value, q_like_prefix)) toupper(q_comp) else NULL
            )
        }

        prefix_rows <- normalize_alias_index_df(query_alias_rows(q_like_prefix))
        contains_rows <- normalize_alias_index_df(query_alias_rows(q_like_contains))
        prefix_rows$.partial_match_type <- rep("prefix", nrow(prefix_rows))
        contains_rows$.partial_match_type <- rep("contains", nrow(contains_rows))
        alias_rows <- rbind(prefix_rows, contains_rows)
        if (nrow(alias_rows) > 0L) {
            alias_key <- paste(
                normalize_partial_gene_query(alias_rows$query_term_original),
                trimws(as.character(alias_rows$local_gene_id %||% "")),
                trimws(as.character(alias_rows$local_symbol %||% "")),
                sep = "\r"
            )
            alias_rows <- alias_rows[!duplicated(alias_key), , drop = FALSE]
        }
        if (!is.data.frame(alias_rows) || nrow(alias_rows) == 0L) return(NULL)

        display <- trimws(as.character(alias_rows$local_symbol %||% ""))
        loc_like <- grepl("^LOC[0-9]+$", display, ignore.case = TRUE)
        term_display <- trimws(as.character(alias_rows$query_term_original %||% ""))
        term_type <- tolower(trimws(as.character(alias_rows$term_type %||% "")))
        extract_query_token <- function(txt) {
            txt <- trimws(as.character(txt %||% ""))
            if (!nzchar(txt)) return("")
            matches <- regmatches(txt, gregexpr("[A-Za-z][A-Za-z0-9._;:-]*", txt, perl = TRUE))[[1]]
            matches <- trimws(as.character(matches %||% character(0)))
            matches <- matches[nzchar(matches) & nchar(matches) <= 40L]
            if (length(matches) == 0L) return("")
            hit <- grepl(q_comp, normalize_partial_gene_query(matches), fixed = TRUE)
            matches <- matches[hit]
            if (length(matches) == 0L) return("")
            matches[[1]]
        }
        term_token <- vapply(term_display, extract_query_token, character(1))
        internal_like <- grepl("^(gene|transcript|rna|cds)[:_-]", display, ignore.case = TRUE)
        term_gene_like <- nzchar(term_token) & term_type %in% c(
            "alias", "gene_synonym", "gene_synonyms", "synonym",
            "description", "product", "note"
        )
        use_term <- (!nzchar(display) | loc_like | internal_like | term_gene_like) & nzchar(term_token)
        display[use_term] <- term_token[use_term]
        display[!nzchar(display)] <- trimws(as.character(alias_rows$local_gene_id[!nzchar(display)] %||% ""))

        keep <- nzchar(display)
        if (!any(keep)) return(NULL)
        alias_rows <- alias_rows[keep, , drop = FALSE]
        display <- display[keep]
        row_match_type <- as.character(alias_rows$.partial_match_type %||% "contains")
        conf <- toupper(trimws(as.character(alias_rows$confidence %||% "")))
        cr <- unname(confidence_rank[conf])
        cr[is.na(cr)] <- 1L
        exact_display <- normalize_partial_gene_query(display) == q_comp
        score <- cr + ifelse(exact_display, 140, ifelse(row_match_type == "prefix", 70, 35))
        score <- score - pmax(0, nchar(normalize_partial_gene_query(display)) - nchar(q_comp))
        src <- trimws(as.character(alias_rows$source_db %||% ""))
        ev <- tolower(trimws(as.character(alias_rows$evidence_source %||% "")))
        tt <- tolower(trimws(as.character(alias_rows$term_type %||% "")))
        is_local_source <- ev == "local_annotation" | toupper(src) == "GFF"
        match_role <- ifelse(tt %in% c("id", "local_id", "gene_id", "ensembl_gene_id"), "stable_id",
            ifelse(tt %in% c("name", "gene_name", "gene", "gene_symbol"), "official_symbol",
                ifelse(tt %in% c("alias", "gene_synonym", "gene_synonyms", "synonym", "external_gene_name"), "synonym", "other")))
        requires_confirmation <- !is_local_source & nzchar(trimws(as.character(alias_rows$local_gene_id %||% "")))

        out <- data.frame(
            gene_name = display,
            file_label = rep(labels[[i]], length(display)),
            match_type = ifelse(exact_display, "exact", row_match_type),
            score = score,
            source_count = rep(1L, length(display)),
            local_gene_id = trimws(as.character(alias_rows$local_gene_id %||% "")),
            local_symbol = trimws(as.character(alias_rows$local_symbol %||% "")),
            term_type = tt,
            source_db = src,
            confidence = conf,
            match_role = match_role,
            requires_confirmation = requires_confirmation,
            stringsAsFactors = FALSE
        )
        out$key <- normalize_partial_gene_query(out$gene_name)
        out$display_preference <- ifelse(grepl("\\.", out$gene_name), 1L, 0L)
        out <- out[order(-out$score, -out$display_preference, nchar(out$gene_name), tolower(out$gene_name)), , drop = FALSE]
        out <- out[!duplicated(out$key), , drop = FALSE]
        out$display_preference <- NULL
        out$key <- NULL
        if (is.finite(max_per_file) && nrow(out) > max_per_file) {
            out <- out[seq_len(as.integer(max_per_file)), , drop = FALSE]
        }
        normalize_partial_gene_suggestions_df(out)
    })
    rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
    if (length(rows) == 0L) return(empty_partial_gene_suggestions_df())
    normalize_partial_gene_suggestions_df(do.call(rbind, rows))
}

find_deterministic_partial_gene_suggestions <- function(annotation_paths, query, file_labels = NULL,
                                                        det_list = NULL, max_per_file = 20L,
                                                        max_total = 20L, min_query_chars = 2L,
                                                        min_shared_organisms = 1L,
                                                        source_label_preview = 4L,
                                                        include_alias_sql = TRUE,
                                                        base_dir = ".") {
    q_comp <- normalize_partial_gene_query(query)
    min_query_chars <- suppressWarnings(as.integer(min_query_chars %||% 2L))
    if (!is.finite(min_query_chars) || is.na(min_query_chars) || min_query_chars < 1L) min_query_chars <- 2L
    if (nchar(q_comp) < min_query_chars) {
        return(empty_partial_gene_suggestions_df(source_labels = TRUE, source_label_preview = TRUE))
    }
    paths <- as.character(annotation_paths %||% character(0))
    paths <- paths[nzchar(paths) & file.exists(paths)]
    if (length(paths) == 0L) {
        return(empty_partial_gene_suggestions_df(source_labels = TRUE, source_label_preview = TRUE))
    }
    labels <- as.character(file_labels %||% basename(paths))
    if (length(labels) != length(paths)) labels <- basename(paths)
    max_per_file <- suppressWarnings(as.numeric(max_per_file %||% 20L))
    if (is.na(max_per_file) || max_per_file < 1) max_per_file <- 20L
    max_total <- suppressWarnings(as.numeric(max_total %||% 20L))
    if (is.na(max_total) || max_total < 1) max_total <- 20L
    min_shared <- suppressWarnings(as.integer(min_shared_organisms %||% 1L))
    if (!is.finite(min_shared) || is.na(min_shared) || min_shared < 1L) min_shared <- 1L

    empty_out <- function() empty_partial_gene_suggestions_df(source_labels = TRUE, source_label_preview = TRUE)
    aggregate_rows <- function(rows) {
        rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
        if (length(rows) == 0L) return(empty_out())
        rows <- lapply(rows, normalize_partial_gene_suggestions_df)
        all_rows <- do.call(rbind, rows)
        all_rows$gene_key <- normalize_partial_gene_query(all_rows$gene_name)
        all_rows$aggregation_key <- all_rows$gene_key
        local_ids <- trimws(as.character(all_rows$local_gene_id %||% ""))
        file_keys <- trimws(as.character(all_rows$file_label %||% ""))
        collision_groups <- split(seq_len(nrow(all_rows)), paste(all_rows$gene_key, file_keys, sep = "\r"))
        for (idx in collision_groups) {
            ids <- unique(local_ids[idx][nzchar(local_ids[idx])])
            if (length(ids) > 1L) {
                all_rows$aggregation_key[idx] <- paste(all_rows$gene_key[idx], local_ids[idx], sep = "\r")
            }
        }
        grouped <- do.call(rbind, lapply(split(all_rows, all_rows$aggregation_key), function(df) {
            df <- df[order(-suppressWarnings(as.numeric(df$score %||% 0)), nchar(df$gene_name), tolower(df$gene_name)), , drop = FALSE]
            best <- df[1, , drop = FALSE]
            labels_u <- unique(trimws(as.character(df$file_label %||% "")))
            labels_u <- labels_u[nzchar(labels_u)]
            best$file_label <- paste(labels_u, collapse = ", ")
            best$source_labels <- best$file_label
            best$source_count <- length(labels_u)
            best$score <- max(suppressWarnings(as.numeric(df$score %||% 0)), na.rm = TRUE) + best$source_count * 5
            best$match_type <- if (any(as.character(df$match_type %||% "") == "exact")) {
                "exact"
            } else if (any(as.character(df$match_type %||% "") == "prefix")) {
                "prefix"
            } else {
                "contains"
            }
            best
        }))
        grouped$gene_key <- NULL
        grouped$aggregation_key <- NULL
        grouped$source_count <- suppressWarnings(as.integer(grouped$source_count %||% 1L))
        grouped$source_count[is.na(grouped$source_count) | grouped$source_count < 1L] <- 1L
        grouped <- grouped[grouped$source_count >= min_shared, , drop = FALSE]
        if (nrow(grouped) == 0L) return(empty_out())
        is_prefix <- as.character(grouped$match_type %||% "") == "prefix"
        grouped <- grouped[order(
            -grouped$source_count,
            -as.integer(is_prefix),
            -suppressWarnings(as.numeric(grouped$score %||% 0)),
            nchar(as.character(grouped$gene_name %||% "")),
            tolower(as.character(grouped$gene_name %||% ""))
        ), , drop = FALSE]
        source_label_preview <- suppressWarnings(as.integer(source_label_preview %||% 4L))
        if (!is.finite(source_label_preview) || is.na(source_label_preview) || source_label_preview < 1L) source_label_preview <- 4L
        grouped$source_label_preview <- vapply(strsplit(as.character(grouped$source_labels %||% ""), ",", fixed = TRUE), function(x) {
            labels_u <- unique(trimws(as.character(x %||% character(0))))
            labels_u <- labels_u[nzchar(labels_u)]
            if (length(labels_u) == 0L) return("")
            if (length(labels_u) <= source_label_preview) return(paste(labels_u, collapse = ", "))
            paste0(paste(labels_u[seq_len(source_label_preview)], collapse = ", "), sprintf(" +%d more", length(labels_u) - source_label_preview))
        }, character(1))
        if (is.finite(max_total) && nrow(grouped) > max_total) {
            grouped <- grouped[seq_len(as.integer(max_total)), , drop = FALSE]
        }
        rownames(grouped) <- NULL
        normalize_partial_gene_suggestions_df(grouped, source_labels = TRUE, source_label_preview = TRUE)
    }

    rows <- list()
    for (i in seq_along(paths)) {
        ac <- tryCatch(load_gff_autocomplete_cache(paths[[i]], base_dir = base_dir), error = function(e) NULL)
        choices <- if (is.list(ac)) as.character(ac$display %||% character(0)) else character(0)
        if (length(choices) > 0L) {
            rows <- c(rows, list(find_partial_gene_suggestions_from_choices(
                choices = choices,
                query = query,
                file_label = labels[[i]],
                max_suggestions = max_per_file,
                min_query_chars = min_query_chars
            )))
        }
        rows <- c(rows, list(find_partial_gene_suggestions_in_index(
            file_path = paths[[i]],
            query = query,
            file_label = labels[[i]],
            max_suggestions = max_per_file,
            min_query_chars = min_query_chars,
            allow_build_index = FALSE
        )))
    }
    if (isTRUE(include_alias_sql)) {
        rows <- c(rows, list(find_partial_gene_alias_suggestions_sqlite(
            annotation_paths = paths,
            query = query,
            file_labels = labels,
            det_list = det_list,
            max_per_file = max_per_file,
            min_query_chars = min_query_chars,
            base_dir = base_dir
        )))
    }
    aggregate_rows(rows)
}

find_partial_gene_suggestions_streaming <- function(file_path, query, file_label = NULL,
                                                    max_suggestions = 10L,
                                                    min_query_chars = 2L,
                                                    chunk_size = 50000L) {
    p <- as.character(file_path %||% "")
    q_comp <- normalize_partial_gene_query(query)
    min_query_chars <- suppressWarnings(as.integer(min_query_chars %||% 2L))
    if (!is.finite(min_query_chars) || is.na(min_query_chars) || min_query_chars < 1L) {
        min_query_chars <- 2L
    }
    if (!nzchar(p) || !file.exists(p) || nchar(q_comp) < min_query_chars) {
        return(empty_partial_gene_suggestions_df())
    }
    max_suggestions <- suppressWarnings(as.integer(max_suggestions %||% 10L))
    if (!is.finite(max_suggestions) || is.na(max_suggestions) || max_suggestions < 1L) {
        max_suggestions <- 10L
    }
    chunk_size <- suppressWarnings(as.integer(chunk_size %||% 50000L))
    if (!is.finite(chunk_size) || is.na(chunk_size) || chunk_size < 1000L) {
        chunk_size <- 50000L
    }

    con <- if (grepl("\\.(gz|bgz)$", p, ignore.case = TRUE)) gzfile(p, open = "rt") else file(p, open = "r")
    on.exit(close(con), add = TRUE)

    gene_pattern <- "^[^\t]+\t[^\t]+\t(gene|pseudogene|[Cc][Dd][Ss])\t"
    out_names <- character(0)
    out_types <- character(0)
    out_scores <- numeric(0)
    seen <- character(0)

    repeat {
        lines <- readLines(con, n = chunk_size, warn = FALSE)
        if (length(lines) == 0L) break
        lines <- lines[!startsWith(lines, "#") & grepl(gene_pattern, lines, perl = TRUE)]
        if (length(lines) == 0L) next

        fields <- strsplit(lines, "\t", fixed = TRUE)
        attrs <- vapply(fields, function(x) if (length(x) >= 9L) x[[9L]] else "", character(1))
        attrs <- attrs[nzchar(attrs)]
        if (length(attrs) == 0L) next

        display <- vapply(attrs, extract_partial_gene_display_name, character(1))
        display <- trimws(display)
        display <- display[!is.na(display) & nzchar(display) & nchar(display) <= 80]
        if (length(display) == 0L) next

        comp <- normalize_partial_gene_choices(display)
        hit_prefix <- startsWith(comp, q_comp)
        hit_contains <- !hit_prefix & grepl(q_comp, comp, fixed = TRUE)
        hit <- hit_prefix | hit_contains
        if (!any(hit)) next

        hit_names <- display[hit]
        hit_keys <- tolower(normalize_gene_compact(hit_names))
        keep <- !hit_keys %in% seen
        if (!any(keep)) next

        hit_names <- hit_names[keep]
        hit_keys <- hit_keys[keep]
        hit_prefix <- hit_prefix[hit][keep]
        hit_comp <- comp[hit][keep]
        seen <- c(seen, hit_keys)
        out_names <- c(out_names, hit_names)
        out_types <- c(out_types, ifelse(hit_prefix, "prefix", "contains"))
        out_scores <- c(out_scores, ifelse(hit_prefix, 100, 60) - pmax(0, nchar(hit_comp) - nchar(q_comp)))

        if (length(out_names) >= max_suggestions) {
            break
        }
    }

    if (length(out_names) == 0L) {
        return(empty_partial_gene_suggestions_df())
    }

    out <- data.frame(
        gene_name = out_names,
        file_label = rep(as.character(file_label %||% basename(p)), length(out_names)),
        match_type = out_types,
        score = out_scores,
        source_count = rep(1L, length(out_names)),
        stringsAsFactors = FALSE
    )
    ord <- order(-out$score, nchar(out$gene_name), tolower(out$gene_name))
    out <- out[ord, , drop = FALSE]
    if (nrow(out) > max_suggestions) {
        out <- out[seq_len(max_suggestions), , drop = FALSE]
    }
    rownames(out) <- NULL
    normalize_partial_gene_suggestions_df(out)
}

find_partial_gene_suggestions_grep <- function(file_path, query, file_label = NULL,
                                               max_suggestions = 10L,
                                               min_query_chars = 2L) {
    p <- as.character(file_path %||% "")
    q_comp <- normalize_partial_gene_query(query)
    min_query_chars <- suppressWarnings(as.integer(min_query_chars %||% 2L))
    if (!is.finite(min_query_chars) || is.na(min_query_chars) || min_query_chars < 1L) {
        min_query_chars <- 2L
    }
    if (!nzchar(p) || !file.exists(p) || nchar(q_comp) < min_query_chars) {
        return(NULL)
    }
    seed <- extract_gene_query_alpha_core(query)
    if (!nzchar(seed) || nchar(seed) < 2L) {
        seed <- q_comp
    }
    if (!nzchar(seed) || nchar(seed) < min_query_chars) {
        return(NULL)
    }
    tool <- if (grepl("\\.(gz|bgz)$", p, ignore.case = TRUE)) "zgrep" else "grep"
    if (!nzchar(Sys.which(tool))) {
        return(NULL)
    }
    max_suggestions <- suppressWarnings(as.integer(max_suggestions %||% 10L))
    if (!is.finite(max_suggestions) || is.na(max_suggestions) || max_suggestions < 1L) {
        max_suggestions <- 10L
    }
    pattern <- paste0("^[^\t]+\t[^\t]+\t(gene|pseudogene|[Cc][Dd][Ss])\t.*", seed)
    lines <- tryCatch(
        system2(tool, args = c("-E", "-i", shQuote(pattern), shQuote(p)), stdout = TRUE, stderr = FALSE),
        warning = function(w) character(0),
        error = function(e) character(0)
    )
    lines <- as.character(lines %||% character(0))
    lines <- lines[nzchar(lines)]
    if (length(lines) == 0L) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    fields <- strsplit(lines, "\t", fixed = TRUE)
    attrs <- vapply(fields, function(x) if (length(x) >= 9L) x[[9L]] else "", character(1))
    attrs <- attrs[nzchar(attrs)]
    if (length(attrs) == 0L) {
        return(NULL)
    }
    display <- vapply(attrs, extract_partial_gene_display_name, character(1))
    display <- trimws(display)
    display <- display[!is.na(display) & nzchar(display) & nchar(display) <= 80]
    if (length(display) == 0L) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }
    comp <- normalize_partial_gene_choices(display)
    hit_prefix <- startsWith(comp, q_comp)
    hit_contains <- !hit_prefix & grepl(q_comp, comp, fixed = TRUE)
    hit <- hit_prefix | hit_contains
    if (!any(hit)) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }
    out <- data.frame(
        gene_name = display[hit],
        file_label = rep(as.character(file_label %||% basename(p)), sum(hit)),
        match_type = ifelse(hit_prefix[hit], "prefix", "contains"),
        score = ifelse(hit_prefix[hit], 100, 60) - pmax(0, nchar(comp[hit]) - nchar(q_comp)),
        source_count = rep(1L, sum(hit)),
        stringsAsFactors = FALSE
    )
    out$key <- tolower(normalize_gene_compact(out$gene_name))
    out <- out[!duplicated(out$key), , drop = FALSE]
    out$key <- NULL
    ord <- order(-out$score, nchar(out$gene_name), tolower(out$gene_name))
    out <- out[ord, , drop = FALSE]
    if (nrow(out) > max_suggestions) {
        out <- out[seq_len(max_suggestions), , drop = FALSE]
    }
    rownames(out) <- NULL
    out
}

find_partial_gene_suggestions_in_file <- function(file_path, query, file_label = NULL,
                                                  max_suggestions = 10L,
                                                  min_query_chars = 2L) {
    p <- as.character(file_path %||% "")
    q <- trimws(as.character(query %||% ""))
    q_comp <- normalize_partial_gene_query(q)
    min_query_chars <- suppressWarnings(as.integer(min_query_chars %||% 2L))
    if (!is.finite(min_query_chars) || is.na(min_query_chars) || min_query_chars < 1L) {
        min_query_chars <- 2L
    }
    if (!nzchar(p) || !file.exists(p) || nchar(q_comp) < min_query_chars) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }
    max_suggestions <- suppressWarnings(as.integer(max_suggestions %||% 10L))
    if (!is.finite(max_suggestions) || is.na(max_suggestions) || max_suggestions < 1L) {
        max_suggestions <- 10L
    }

    cache_key <- tryCatch(
        paste(
            "partial-gene-suggestions-v1",
            gff_cache_key(p),
            tolower(q_comp),
            sep = "||"
        ),
        error = function(e) ""
    )
    apply_suggestion_label <- function(value) {
        if (is.data.frame(value) && "file_label" %in% names(value)) {
            value$file_label <- rep(as.character(file_label %||% basename(p)), nrow(value))
        }
        value
    }
    if (nzchar(cache_key) && exists(cache_key, envir = .partial_gene_suggestion_cache, inherits = FALSE)) {
        cached <- get(cache_key, envir = .partial_gene_suggestion_cache, inherits = FALSE)
        if (is.data.frame(cached)) {
            if (nrow(cached) > max_suggestions) {
                cached <- cached[seq_len(max_suggestions), , drop = FALSE]
            }
            return(apply_suggestion_label(cached))
        }
    }
    cache_and_return <- function(value) {
        if (nzchar(cache_key) && is.data.frame(value)) {
            assign(cache_key, value, envir = .partial_gene_suggestion_cache)
        }
        apply_suggestion_label(value)
    }

    grep_suggestions <- tryCatch(
        find_partial_gene_suggestions_grep(
            file_path = p,
            query = q,
            file_label = file_label,
            max_suggestions = max_suggestions,
            min_query_chars = min_query_chars
        ),
        error = function(e) NULL
    )
    if (is.data.frame(grep_suggestions)) {
        return(cache_and_return(grep_suggestions))
    }

    streamed <- tryCatch(
        find_partial_gene_suggestions_streaming(
            file_path = p,
            query = q,
            file_label = file_label,
            max_suggestions = max_suggestions,
            min_query_chars = min_query_chars
        ),
        error = function(e) NULL
    )
    if (is.data.frame(streamed)) {
        return(cache_and_return(streamed))
    }

    idx <- tryCatch(build_gff_gene_light_index(p), error = function(e) NULL)
    if (is.null(idx) || !is.list(idx) || is.null(idx$genes_df) || !is.data.frame(idx$genes_df) || nrow(idx$genes_df) == 0L) {
        return(cache_and_return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        )))
    }

    attrs <- as.character(idx$genes_df$attributes %||% rep("", nrow(idx$genes_df)))
    display <- vapply(attrs, extract_partial_gene_display_name, character(1))
    display <- trimws(display)
    keep <- !is.na(display) & nzchar(display) & nchar(display) <= 80
    if (!any(keep)) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    display <- display[keep]
    comp <- normalize_partial_gene_choices(display)
    hit_prefix <- startsWith(comp, q_comp)
    hit_contains <- !hit_prefix & grepl(q_comp, comp, fixed = TRUE)
    hit <- hit_prefix | hit_contains
    if (!any(hit)) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    out <- data.frame(
        gene_name = display[hit],
        file_label = rep(as.character(file_label %||% basename(p)), sum(hit)),
        match_type = ifelse(hit_prefix[hit], "prefix", "contains"),
        score = ifelse(hit_prefix[hit], 100, 60) - pmax(0, nchar(comp[hit]) - nchar(q_comp)),
        source_count = rep(1L, sum(hit)),
        stringsAsFactors = FALSE
    )
    out <- out[!duplicated(tolower(out$gene_name)), , drop = FALSE]
    ord <- order(-out$score, nchar(out$gene_name), tolower(out$gene_name))
    out <- out[ord, , drop = FALSE]
    if (nrow(out) > max_suggestions) {
        out <- out[seq_len(max_suggestions), , drop = FALSE]
    }
    rownames(out) <- NULL
    cache_and_return(out)
}

find_partial_gene_suggestions <- function(annotation_paths, query, file_labels = NULL,
                                          max_per_file = 10L, max_total = 15L,
                                          min_query_chars = 2L) {
    paths <- as.character(annotation_paths %||% character(0))
    paths <- paths[nzchar(paths)]
    if (length(paths) == 0L) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }
    labels <- as.character(file_labels %||% basename(paths))
    if (length(labels) != length(paths)) {
        labels <- basename(paths)
    }
    max_total <- suppressWarnings(as.integer(max_total %||% 15L))
    if (!is.finite(max_total) || is.na(max_total) || max_total < 1L) {
        max_total <- 15L
    }

    rows <- lapply(seq_along(paths), function(i) {
        find_partial_gene_suggestions_in_file(
            file_path = paths[[i]],
            query = query,
            file_label = labels[[i]],
            max_suggestions = max_per_file,
            min_query_chars = min_query_chars
        )
    })
    rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
    if (length(rows) == 0L) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    all_rows <- do.call(rbind,  rows)
    all_rows$gene_key <- tolower(normalize_gene_compact(all_rows$gene_name))
    split_rows <- split(all_rows, all_rows$gene_key)
    grouped <- do.call(rbind,  lapply(split_rows, function(df) {
        df <- df[order(-df$score, nchar(df$gene_name), tolower(df$gene_name)), , drop = FALSE]
        best <- df[1, , drop = FALSE]
        best$file_label <- paste(unique(df$file_label), collapse = ", ")
        best$source_count <- length(unique(df$file_label))
        best$score <- max(df$score, na.rm = TRUE) + (best$source_count * 5)
        best$match_type <- if (any(df$match_type == "prefix")) "prefix" else "contains"
        best
    }))
    grouped$gene_key <- NULL
    grouped <- grouped[order(-grouped$source_count, -grouped$score, nchar(grouped$gene_name), tolower(grouped$gene_name)), , drop = FALSE]
    if (nrow(grouped) > max_total) {
        grouped <- grouped[seq_len(max_total), , drop = FALSE]
    }
    rownames(grouped) <- NULL
    grouped
}

find_cross_species_gene_suggestions <- function(annotation_paths, query, file_labels = NULL,
                                                max_per_file = 10L, max_total = 15L,
                                                min_query_chars = 2L,
                                                source_label_preview = 4L) {
    suggestions <- find_partial_gene_suggestions(
        annotation_paths = annotation_paths,
        query = query,
        file_labels = file_labels,
        max_per_file = max_per_file,
        max_total = max_total,
        min_query_chars = min_query_chars
    )
    if (!is.data.frame(suggestions) || nrow(suggestions) == 0L) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            source_labels = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    if (!"source_labels" %in% names(suggestions)) {
        suggestions$source_labels <- as.character(suggestions$file_label %||% "")
    }
    suggestions$source_labels <- vapply(strsplit(as.character(suggestions$source_labels %||% ""), ",", fixed = TRUE), function(x) {
        labels <- unique(trimws(as.character(x %||% character(0))))
        labels <- labels[nzchar(labels)]
        paste(labels, collapse = ", ")
    }, character(1))
    suggestions$file_label <- suggestions$source_labels
    suggestions$source_count <- suppressWarnings(as.integer(suggestions$source_count %||% 1L))
    suggestions$source_count[is.na(suggestions$source_count) | suggestions$source_count < 1L] <- 1L

    is_prefix <- as.character(suggestions$match_type %||% "") == "prefix"
    if (length(is_prefix) != nrow(suggestions)) {
        is_prefix <- rep(FALSE, nrow(suggestions))
    }
    suggestions <- suggestions[order(
        -suggestions$source_count,
        -as.integer(is_prefix),
        -suppressWarnings(as.numeric(suggestions$score %||% 0)),
        nchar(as.character(suggestions$gene_name %||% "")),
        tolower(as.character(suggestions$gene_name %||% ""))
    ), , drop = FALSE]

    source_label_preview <- suppressWarnings(as.integer(source_label_preview %||% 4L))
    if (!is.finite(source_label_preview) || is.na(source_label_preview) || source_label_preview < 1L) {
        source_label_preview <- 4L
    }
    suggestions$source_label_preview <- vapply(strsplit(as.character(suggestions$source_labels %||% ""), ",", fixed = TRUE), function(x) {
        labels <- unique(trimws(as.character(x %||% character(0))))
        labels <- labels[nzchar(labels)]
        if (length(labels) == 0L) return("")
        if (length(labels) <= source_label_preview) return(paste(labels, collapse = ", "))
        paste0(paste(labels[seq_len(source_label_preview)], collapse = ", "), sprintf(" +%d more", length(labels) - source_label_preview))
    }, character(1))

    max_total <- suppressWarnings(as.integer(max_total %||% 15L))
    if (!is.finite(max_total) || is.na(max_total) || max_total < 1L) {
        max_total <- 15L
    }
    if (nrow(suggestions) > max_total) {
        suggestions <- suggestions[seq_len(max_total), , drop = FALSE]
    }
    rownames(suggestions) <- NULL
    suggestions
}

find_cross_species_alias_family_suggestions <- function(annotation_paths, query, file_labels = NULL,
                                                        det_list = NULL, max_per_file = 12L,
                                                        max_total = 20L, min_query_chars = 2L,
                                                        source_label_preview = 4L) {
    q_comp <- normalize_partial_gene_query(query)
    min_query_chars <- suppressWarnings(as.integer(min_query_chars %||% 2L))
    if (!is.finite(min_query_chars) || is.na(min_query_chars) || min_query_chars < 1L) {
        min_query_chars <- 2L
    }
    if (nchar(q_comp) < min_query_chars || !exists("load_alias_index", mode = "function")) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            source_labels = character(0),
            source_label_preview = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    paths <- as.character(annotation_paths %||% character(0))
    paths <- paths[nzchar(paths)]
    if (length(paths) == 0L) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            source_labels = character(0),
            source_label_preview = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }
    labels <- as.character(file_labels %||% basename(paths))
    if (length(labels) != length(paths)) {
        labels <- basename(paths)
    }
    max_per_file <- suppressWarnings(as.numeric(max_per_file %||% 12L))
    if (is.na(max_per_file) || max_per_file < 1) {
        max_per_file <- 12L
    }

    preferred_types <- c(
        "name", "gene_name", "gene", "gene_symbol", "alias", "gene_synonym",
        "gene_synonyms", "synonym", "external_gene_name", "locus_tag",
        "id", "dbxref", "entrezgene_id"
    )
    confidence_rank <- c(HIGH = 30L, MEDIUM = 20L, LOW = 10L)

    rows <- lapply(seq_along(paths), function(i) {
        det <- tryCatch(det_list[[i]], error = function(e) NULL)
        det <- det %||% list()
        idx <- tryCatch(
            load_alias_index(
                organism_id = as.character(det$species_id %||% det$preloaded_id %||% ""),
                annotation_path = paths[[i]],
                organism_name = as.character(det$organism %||% labels[[i]] %||% ""),
                taxid = as.character(det$taxid %||% ""),
                base_dir = ".",
                allow_gff_fallback = TRUE
            ),
            error = function(e) NULL
        )
        if (is.null(idx)) {
            return(NULL)
        }

        alias_rows <- if (inherits(idx, "SQLiteConnection")) {
            if (!requireNamespace("DBI", quietly = TRUE)) {
                return(NULL)
            }
            like_param <- paste0("%", q_comp, "%")
            alias_limit_sql <- if (is.finite(max_per_file)) {
                sprintf(" LIMIT %d", as.integer(min(.Machine$integer.max, max_per_file * 8)))
            } else {
                ""
            }
            tryCatch(
                DBI::dbGetQuery(
                    idx,
                    paste0(
                        "SELECT * FROM alias_index WHERE ",
                        "(LOWER(query_term_clean_strict) LIKE ?1 OR LOWER(local_symbol) LIKE ?1 OR LOWER(description) LIKE ?1) ",
                        "AND LENGTH(query_term_original) <= 180",
                        alias_limit_sql
                    ),
                    params = list(like_param)
                ),
                error = function(e) data.frame()
            )
        } else {
            idx <- normalize_alias_index_df(idx)
            if (nrow(idx) == 0L) {
                return(NULL)
            }
            key <- tolower(as.character(idx$query_term_clean_strict %||% ""))
            sym <- normalize_partial_gene_query(idx$local_symbol %||% "")
            desc <- normalize_partial_gene_query(idx$description %||% "")
            idx[grepl(q_comp, key, fixed = TRUE) |
                grepl(q_comp, sym, fixed = TRUE) |
                grepl(q_comp, desc, fixed = TRUE), , drop = FALSE]
        }
        alias_rows <- normalize_alias_index_df(alias_rows)
        if (nrow(alias_rows) == 0L) {
            return(NULL)
        }

        term_comp <- normalize_partial_gene_query(alias_rows$query_term_original)
        sym_comp <- normalize_partial_gene_query(alias_rows$local_symbol)
        desc_comp <- normalize_partial_gene_query(alias_rows$description)
        hit_prefix <- startsWith(term_comp, q_comp) | startsWith(sym_comp, q_comp)
        hit_contains <- !hit_prefix & (
            grepl(q_comp, term_comp, fixed = TRUE) |
            grepl(q_comp, sym_comp, fixed = TRUE) |
            grepl(q_comp, desc_comp, fixed = TRUE)
        )
        hit <- hit_prefix | hit_contains
        alias_rows <- alias_rows[hit, , drop = FALSE]
        if (nrow(alias_rows) == 0L) {
            return(NULL)
        }
        term_comp <- term_comp[hit]
        sym_comp <- sym_comp[hit]
        hit_prefix <- hit_prefix[hit]

        alias_rows$display_gene <- trimws(as.character(alias_rows$local_symbol %||% ""))
        loc_like <- grepl("^LOC[0-9]+$", alias_rows$display_gene, ignore.case = TRUE)
        term_gene_like <- grepl("[A-Za-z]", alias_rows$query_term_original) &
            grepl(q_comp, normalize_partial_gene_query(alias_rows$query_term_original), fixed = TRUE) &
            nchar(as.character(alias_rows$query_term_original %||% "")) <= 40L
        replace_display <- !nzchar(alias_rows$display_gene) | loc_like
        alias_rows$display_gene[replace_display & term_gene_like] <- trimws(as.character(alias_rows$query_term_original[replace_display & term_gene_like]))
        alias_rows$display_gene[!nzchar(alias_rows$display_gene)] <- trimws(as.character(alias_rows$local_gene_id[!nzchar(alias_rows$display_gene)] %||% ""))
        alias_rows <- alias_rows[nzchar(alias_rows$display_gene), , drop = FALSE]
        if (nrow(alias_rows) == 0L) {
            return(NULL)
        }

        conf <- toupper(trimws(as.character(alias_rows$confidence %||% "")))
        cr <- unname(confidence_rank[conf])
        cr[is.na(cr)] <- 1L
        type_rank <- match(tolower(trimws(as.character(alias_rows$term_type %||% ""))), preferred_types)
        type_rank[is.na(type_rank)] <- length(preferred_types) + 1L
        score <- cr + ifelse(hit_prefix, 60, 30) - type_rank - pmax(0, nchar(term_comp) - nchar(q_comp))
        gene_key <- trimws(as.character(alias_rows$local_gene_id %||% ""))
        alias_rows$.score <- score
        alias_rows$.match_type <- ifelse(hit_prefix, "prefix", "contains")
        alias_rows$.gene_key <- gene_key
        alias_rows <- alias_rows[order(-alias_rows$.score, nchar(alias_rows$display_gene), tolower(alias_rows$display_gene)), , drop = FALSE]
        alias_rows <- alias_rows[!duplicated(alias_rows$.gene_key), , drop = FALSE]
        if (is.finite(max_per_file) && nrow(alias_rows) > max_per_file) {
            alias_rows <- alias_rows[seq_len(as.integer(max_per_file)), , drop = FALSE]
        }
        data.frame(
            gene_name = alias_rows$display_gene,
            file_label = rep(labels[[i]], nrow(alias_rows)),
            source_labels = rep(labels[[i]], nrow(alias_rows)),
            match_type = alias_rows$.match_type,
            score = alias_rows$.score,
            source_count = rep(1L, nrow(alias_rows)),
            stringsAsFactors = FALSE
        )
    })
    rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
    if (length(rows) == 0L) {
        return(data.frame(
            gene_name = character(0),
            file_label = character(0),
            source_labels = character(0),
            source_label_preview = character(0),
            match_type = character(0),
            score = numeric(0),
            source_count = integer(0),
            stringsAsFactors = FALSE
        ))
    }

    all_rows <- do.call(rbind,  rows)
    all_rows$group_key <- normalize_partial_gene_query(all_rows$gene_name)
    grouped <- do.call(rbind,  lapply(split(all_rows, all_rows$group_key), function(df) {
        df <- df[order(-df$score, nchar(df$gene_name), tolower(df$gene_name)), , drop = FALSE]
        best <- df[1, , drop = FALSE]
        labels_u <- unique(trimws(as.character(df$file_label %||% "")))
        labels_u <- labels_u[nzchar(labels_u)]
        best$file_label <- paste(labels_u, collapse = ", ")
        best$source_labels <- best$file_label
        best$source_count <- length(labels_u)
        best$score <- max(df$score, na.rm = TRUE) + best$source_count * 8
        best$match_type <- if (any(df$match_type == "prefix")) "prefix" else "contains"
        best
    }))
    grouped$group_key <- NULL
    grouped <- grouped[order(-grouped$source_count, -grouped$score, nchar(grouped$gene_name), tolower(grouped$gene_name)), , drop = FALSE]

    source_label_preview <- suppressWarnings(as.integer(source_label_preview %||% 4L))
    if (!is.finite(source_label_preview) || is.na(source_label_preview) || source_label_preview < 1L) {
        source_label_preview <- 4L
    }
    grouped$source_label_preview <- vapply(strsplit(as.character(grouped$source_labels %||% ""), ",", fixed = TRUE), function(x) {
        labels_u <- unique(trimws(as.character(x %||% character(0))))
        labels_u <- labels_u[nzchar(labels_u)]
        if (length(labels_u) == 0L) return("")
        if (length(labels_u) <= source_label_preview) return(paste(labels_u, collapse = ", "))
        paste0(paste(labels_u[seq_len(source_label_preview)], collapse = ", "), sprintf(" +%d more", length(labels_u) - source_label_preview))
    }, character(1))

    max_total <- suppressWarnings(as.numeric(max_total %||% 20L))
    if (is.na(max_total) || max_total < 1) {
        max_total <- 20L
    }
    if (is.finite(max_total) && nrow(grouped) > max_total) {
        grouped <- grouped[seq_len(as.integer(max_total)), , drop = FALSE]
    }
    rownames(grouped) <- NULL
    grouped
}

should_route_to_partial_gene_suggestions <- function(file_path, query, min_query_chars = 2L) {
    q_comp <- normalize_partial_gene_query(query)
    min_query_chars <- suppressWarnings(as.integer(min_query_chars %||% 2L))
    if (!is.finite(min_query_chars) || is.na(min_query_chars) || min_query_chars < 1L) {
        min_query_chars <- 2L
    }
    if (nchar(q_comp) < min_query_chars) return(FALSE)
    suggestions <- find_partial_gene_suggestions_in_index(
        file_path = file_path,
        query = query,
        max_suggestions = 10L,
        min_query_chars = min_query_chars,
        allow_build_index = TRUE
    )
    if (!is.data.frame(suggestions) || nrow(suggestions) == 0L) {
        return(FALSE)
    }
    suggestion_comp <- normalize_partial_gene_query(suggestions$gene_name)
    any(startsWith(suggestion_comp, q_comp) | grepl(q_comp, suggestion_comp, fixed = TRUE))
}

load_gff_cached <- function(file_path) {
    key <- gff_cache_key(file_path)
    cached_df <- cache_env_get(.gff_cache, key, default = NULL)
    if (!is.null(cached_df)) {
        return(cached_df)
    }
    col_names <- c("seqid", "source", "type", "start", "end", "score", "strand", "phase", "attributes")
    df <- vroom::vroom(file_path, comment = "#", col_names = col_names, show_col_types = FALSE, delim = "\t", quote = "", altrep = FALSE)
    cache_env_set(
        .gff_cache,
        key,
        df,
        max_size = annotation_memory_cache_limits$gff_max_entries,
        max_bytes = annotation_memory_cache_limits$gff_max_bytes
    )
    df
}

normalize_chr_id <- function(x) {
    x <- tolower(trimws(as.character(x %||% "")))
    x <- stringr::str_remove(x, "^chromosome[:_ ]*")
    x <- stringr::str_remove(x, "^chr")
    trimws(x)
}

.gff_chr_name_map_cache <- new.env(parent = emptyenv())

get_chromosome_name_map <- function(annotation_file_path) {
    if (is.null(annotation_file_path) || !nzchar(annotation_file_path) || !file.exists(annotation_file_path)) {
        return(list())
    }

    key <- gff_cache_key(annotation_file_path)
    if (exists(key, envir = .gff_chr_name_map_cache, inherits = FALSE)) {
        return(get(key, envir = .gff_chr_name_map_cache, inherits = FALSE))
    }

    name_map <- list()

    # Read the top of the GFF where region declarations are typically located
    tryCatch(
        {
            con <- if (grepl("\\.gz$", annotation_file_path)) gzfile(annotation_file_path, "r") else file(annotation_file_path, "r")
            lines <- readLines(con, n = 2000, warn = FALSE)
            close(con)

            region_lines <- grep("\\tregion\\t", lines, value = TRUE, ignore.case = TRUE)
            if (length(region_lines) > 0) {
                for (ln in region_lines) {
                    tok <- strsplit(ln, "\\t")[[1]]
                    if (length(tok) < 9) next
                    chr_id <- trimws(tok[1])
                    attrs <- tok[9]

                    # Check for chromosome=*
                    m_chr <- stringr::str_match(attrs, "(?:^|;)chromosome=([^;]+)")[, 2]
                    if (!is.na(m_chr) && nzchar(trimws(m_chr))) {
                        name_map[[chr_id]] <- paste("Chr", trimws(m_chr))
                        next
                    }

                    # Check for Name=*
                    m_name <- stringr::str_match(attrs, "(?:^|;)Name=([^;]+)")[, 2]
                    if (!is.na(m_name) && nzchar(trimws(m_name))) {
                        val <- trimws(m_name)
                        # Exclude non-chromosomal names like Pltd, MT, etc if they don't look intuitive
                        # Although user might like MT or Pltd. Let's just use it if it's short.
                        if (nchar(val) <= 5 || grepl("^[0-9]+$", val)) {
                            name_map[[chr_id]] <- if (grepl("^[0-9]+$", val)) paste("Chr", val) else val
                        }
                    }
                }
            }
        },
        error = function(e) {
            app_debug_log("Error reading chromosome names: ", e$message)
        }
    )

    assign(key, name_map, envir = .gff_chr_name_map_cache)
    return(name_map)
}

get_short_chromosome_name <- function(chr_id, annotation_file_path, use_report_map = FALSE, report_path = "") {
    chr_id <- as.character(chr_id %||% "")
    if (!nzchar(chr_id) || is.na(chr_id) || is.null(annotation_file_path)) {
        return(chr_id)
    }

    # Fast path: if the chromosome name is already short and readable, skip expensive lookups
    if (nchar(chr_id) <= 7 && !grepl("^NC_|^NT_|^NW_|^NZ_|^AC_|^CM", chr_id)) {
        return(chr_id)
    }

    if (!exists(".short_chr_label_cache", envir = .GlobalEnv, inherits = FALSE)) {
        assign(".short_chr_label_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
    }
    short_chr_cache <- get(".short_chr_label_cache", envir = .GlobalEnv, inherits = FALSE)
    cache_key <- paste(
        normalizePath(as.character(annotation_file_path %||% ""), winslash = "/", mustWork = FALSE),
        tolower(trimws(chr_id)),
        as.character(isTRUE(use_report_map)),
        normalizePath(as.character(report_path %||% ""), winslash = "/", mustWork = FALSE),
        sep = "||"
    )
    cached_short <- cache_env_get(short_chr_cache, cache_key, default = NULL)
    if (!is.null(cached_short) && nzchar(as.character(cached_short %||% ""))) {
        return(as.character(cached_short))
    }

    resolved_chr <- chr_id
    if (isTRUE(use_report_map)) {
        rp <- as.character(report_path %||% "")
        if (!nzchar(rp)) {
            rp <- get_assembly_report_path_for_annotation(annotation_file_path, base_dir = ".")
        }
        if (nzchar(rp) && file.exists(rp)) {
            rep_key <- normalizePath(rp, winslash = "/", mustWork = FALSE)
            if (exists(rep_key, envir = .assembly_report_cache, inherits = FALSE)) {
                rep_info <- get(rep_key, envir = .assembly_report_cache, inherits = FALSE)
                rep_map <- rep_info$chr_map
                if (is.list(rep_map) && length(rep_map) > 0) {
                    lookup_keys <- unique(c(
                        tolower(trimws(chr_id)),
                        tolower(trimws(sub("\\.\\d+$", "", chr_id))),
                        normalize_chr_id(chr_id)
                    ))
                    for (kk in lookup_keys) {
                        if (!nzchar(kk)) next
                        hit <- rep_map[[kk]]
                        if (!is.null(hit)) {
                            hit_txt <- trimws(as.character(hit %||% ""))
                            if (nzchar(hit_txt)) {
                                resolved_chr <- hit_txt
                                cache_env_set(short_chr_cache, cache_key, resolved_chr, max_size = 12000L)
                                return(resolved_chr)
                            }
                        }
                    }
                }
            }
        }
    }

    chr_map_key <- gff_cache_key(annotation_file_path)
    if (exists(chr_map_key, envir = .gff_chr_name_map_cache, inherits = FALSE)) {
        name_map <- get(chr_map_key, envir = .gff_chr_name_map_cache, inherits = FALSE)
    } else {
        name_map <- get_chromosome_name_map(annotation_file_path)
    }
    if (!is.null(name_map[[chr_id]])) {
        resolved_chr <- as.character(name_map[[chr_id]])
        cache_env_set(short_chr_cache, cache_key, resolved_chr, max_size = 12000L)
        return(resolved_chr)
    }

    # Simple regex fallback if mapping not found, but it has NC_ format
    if (grepl("^NC_0+(\\d+)\\.\\d+$", chr_id)) {
        num <- sub("^NC_0+(\\d+)\\.\\d+$", "\\1", chr_id)
        resolved_chr <- paste("Chr", num)
        cache_env_set(short_chr_cache, cache_key, resolved_chr, max_size = 12000L)
        return(resolved_chr)
    }

    cache_env_set(short_chr_cache, cache_key, resolved_chr, max_size = 12000L)
    return(resolved_chr)
}

get_chromosome_length_map <- function(annotation_file_path) {
    if (is.null(annotation_file_path) || !nzchar(annotation_file_path) || !file.exists(annotation_file_path)) {
        return(list(raw = numeric(0), norm = numeric(0), source = "missing"))
    }

    key <- gff_cache_key(annotation_file_path)
    if (exists(key, envir = .gff_chr_length_cache, inherits = FALSE)) {
        return(get(key, envir = .gff_chr_length_cache, inherits = FALSE))
    }

    raw_map <- numeric(0)

    # First try explicit sequence-region declarations.
    hdr <- tryCatch(readLines(annotation_file_path, n = 5000, warn = FALSE), error = function(e) character())
    seq_lines <- grep("^##sequence-region\\s+", hdr, value = TRUE)
    if (length(seq_lines) > 0) {
        for (ln in seq_lines) {
            tok <- strsplit(trimws(ln), "\\s+")[[1]]
            if (length(tok) < 4) next
            chr_id <- tok[2]
            st <- suppressWarnings(as.numeric(tok[3]))
            en <- suppressWarnings(as.numeric(tok[4]))
            if (!is.finite(st) || !is.finite(en)) next
            chr_len <- abs(en - st) + 1
            if (!is.na(chr_len) && chr_len > 0) {
                prev <- suppressWarnings(as.numeric(raw_map[chr_id]))
                if (!length(prev) || !is.finite(prev)) prev <- 0
                raw_map[chr_id] <- max(prev, chr_len)
            }
        }
    }

    # Fallback/inference: max coordinate per seqid.
    if (length(raw_map) == 0) {
        if (is_tabix_annotation_file(annotation_file_path)) {
            genes_df <- get_genes_table_from_annotation(annotation_file_path)
            if (!is.null(genes_df) && nrow(genes_df) > 0) {
                inferred <- genes_df |>
                    dplyr::transmute(seqid = as.character(chr), end = as.numeric(end)) |>
                    dplyr::filter(is.finite(end), !is.na(seqid), nzchar(seqid)) |>
                    dplyr::group_by(seqid) |>
                    dplyr::summarise(chr_len = max(end, na.rm = TRUE), .groups = "drop")
                if (nrow(inferred) > 0) raw_map <- stats::setNames(as.numeric(inferred$chr_len), inferred$seqid)
            }
        } else {
            df <- load_gff_cached(annotation_file_path)
            if (!is.null(df) && nrow(df) > 0) {
                inferred <- df |>
                    dplyr::transmute(seqid = as.character(seqid), end = as.numeric(end)) |>
                    dplyr::filter(is.finite(end), !is.na(seqid), nzchar(seqid)) |>
                    dplyr::group_by(seqid) |>
                    dplyr::summarise(chr_len = max(end, na.rm = TRUE), .groups = "drop")
                if (nrow(inferred) > 0) {
                    raw_map <- stats::setNames(as.numeric(inferred$chr_len), inferred$seqid)
                }
            }
        }
    }

    norm_map <- numeric(0)
    if (length(raw_map) > 0) {
        norm_names <- normalize_chr_id(names(raw_map))
        keep <- nzchar(norm_names)
        if (any(keep)) {
            tmp <- data.frame(norm = norm_names[keep], len = as.numeric(raw_map[keep]), stringsAsFactors = FALSE) |>
                dplyr::group_by(norm) |>
                dplyr::summarise(len = max(len, na.rm = TRUE), .groups = "drop")
            norm_map <- stats::setNames(as.numeric(tmp$len), tmp$norm)
        }
    }

    out <- list(
        raw = raw_map,
        norm = norm_map,
        source = if (length(seq_lines) > 0) "seq-region" else "inferred"
    )
    assign(key, out, envir = .gff_chr_length_cache)
    out
}

get_chromosome_length_for_chr <- function(annotation_file_path, chr_id) {
    if (is.null(chr_id) || !nzchar(as.character(chr_id %||% ""))) {
        return(NA_real_)
    }
    cmap <- get_chromosome_length_map(annotation_file_path)
    chr_raw <- as.character(chr_id)
    if (chr_raw %in% names(cmap$raw)) {
        return(as.numeric(cmap$raw[[chr_raw]]))
    }
    chr_norm <- normalize_chr_id(chr_raw)
    if (nzchar(chr_norm) && chr_norm %in% names(cmap$norm)) {
        return(as.numeric(cmap$norm[[chr_norm]]))
    }
    NA_real_
}

parse_locus_window_flank_bp <- function(span_mode = "10kb", default_bp = 10000L) {
    raw_mode <- span_mode %||% "10kb"
    numeric_mode <- suppressWarnings(as.numeric(raw_mode[1L]))
    if (length(numeric_mode) > 0L && is.finite(numeric_mode) && numeric_mode >= 0) {
        return(as.integer(round(numeric_mode)))
    }
    mode_txt <- tolower(trimws(as.character(raw_mode[1L])))
    out <- switch(
        mode_txt,
        "gene" = 0L,
        "5kb" = 5000L,
        "10kb" = 10000L,
        "25kb" = 25000L,
        "50kb" = 50000L,
        suppressWarnings(as.integer(default_bp))
    )
    if (!is.finite(out) || is.na(out) || out < 0L) {
        out <- suppressWarnings(as.integer(default_bp))
    }
    as.integer(out)
}

compute_feature_block_span <- function(block_data, preferred_types = character(0), fallback_types = character(0)) {
    if (is.null(block_data)) {
        return(list(start = NA_real_, end = NA_real_, seqid = "", strand = ""))
    }
    df <- as.data.frame(block_data, stringsAsFactors = FALSE)
    if (nrow(df) == 0L) {
        return(list(start = NA_real_, end = NA_real_, seqid = "", strand = ""))
    }
    row_types <- tolower(trimws(as.character(df$V3 %||% rep("", nrow(df)))))
    choose_rows <- function(types) {
        if (length(types) == 0L) return(df[0, , drop = FALSE])
        df[row_types %in% tolower(types), , drop = FALSE]
    }
    span_df <- choose_rows(preferred_types)
    if (nrow(span_df) == 0L) {
        span_df <- choose_rows(fallback_types)
    }
    if (nrow(span_df) == 0L) {
        span_df <- df
    }
    starts <- suppressWarnings(as.numeric(span_df$V4))
    ends <- suppressWarnings(as.numeric(span_df$V5))
    start_val <- suppressWarnings(min(starts, na.rm = TRUE))
    end_val <- suppressWarnings(max(ends, na.rm = TRUE))
    if (!is.finite(start_val) || !is.finite(end_val) || end_val < start_val) {
        start_val <- NA_real_
        end_val <- NA_real_
    }
    seqid_candidates <- c(as.character(span_df$V1 %||% ""), as.character(df$V1 %||% ""))
    seqid_txt <- ""
    for (val in seqid_candidates) {
        if (!is.na(val) && nzchar(trimws(val))) {
            seqid_txt <- trimws(val)
            break
        }
    }
    strand_candidates <- c(as.character(span_df$V7 %||% ""), as.character(df$V7 %||% ""))
    strand_txt <- ""
    for (val in strand_candidates) {
        txt <- trimws(as.character(val %||% ""))
        if (txt %in% c("+", "-")) {
            strand_txt <- txt
            break
        }
    }
    list(start = start_val, end = end_val, seqid = seqid_txt, strand = strand_txt)
}

extract_plot_locus_context <- function(plot_data,
                                       annotation_path = "",
                                       genome_path = "",
                                       flank_bp = 10000L,
                                       plot_id = "",
                                       title_txt = "",
                                       organism_info = NULL,
                                       gene_meta = NULL) {
    if (is.null(plot_data)) {
        return(NULL)
    }
    df <- as.data.frame(plot_data, stringsAsFactors = FALSE)
    if (nrow(df) == 0L) {
        return(NULL)
    }

    tx_types <- c(
        "mrna", "transcript", "lnc_rna", "trna", "rrna", "snorna", "snrna", "mirna",
        "ncrna", "primary_transcript", "pre_mirna", "guide_rna", "rnase_p_rna",
        "rnase_mrp_rna", "telomerase_rna", "antisense_rna", "srp_rna", "scarna",
        "vault_rna", "y_rna", "antisense_lncrna", "lncrna"
    )
    feature_types <- c("exon", "cds", "start_codon", "stop_codon")
    gene_span <- compute_feature_block_span(df, preferred_types = "gene", fallback_types = c(tx_types, feature_types))
    tx_span <- compute_feature_block_span(df, preferred_types = tx_types, fallback_types = c(feature_types, "gene"))

    seqid_txt <- trimws(as.character(gene_span$seqid %||% tx_span$seqid %||% ""))
    if (!nzchar(seqid_txt)) {
        return(NULL)
    }
    strand_txt <- trimws(as.character(tx_span$strand %||% gene_span$strand %||% ""))
    if (!strand_txt %in% c("+", "-")) {
        strand_txt <- "+"
    }
    gene_start <- suppressWarnings(as.numeric(gene_span$start))
    gene_end <- suppressWarnings(as.numeric(gene_span$end))
    tx_start <- suppressWarnings(as.numeric(tx_span$start))
    tx_end <- suppressWarnings(as.numeric(tx_span$end))
    if (!is.finite(gene_start) || !is.finite(gene_end)) {
        gene_start <- tx_start
        gene_end <- tx_end
    }
    if (!is.finite(tx_start) || !is.finite(tx_end)) {
        tx_start <- gene_start
        tx_end <- gene_end
    }
    if (!is.finite(gene_start) || !is.finite(gene_end)) {
        return(NULL)
    }

    flank_int <- parse_locus_window_flank_bp(flank_bp, default_bp = 10000L)
    chr_len <- suppressWarnings(as.numeric(get_chromosome_length_for_chr(annotation_path, seqid_txt)))
    window_start <- max(1L, as.integer(round(gene_start - flank_int)))
    window_end <- as.integer(round(gene_end + flank_int))
    if (is.finite(chr_len) && chr_len > 0) {
        window_end <- min(as.integer(round(chr_len)), window_end)
    }
    if (!is.finite(window_end) || window_end < window_start) {
        return(NULL)
    }

    org_name <- trimws(as.character((organism_info %||% list())$name %||% (gene_meta %||% list())$organism_scientific %||% ""))
    gene_label <- trimws(as.character((gene_meta %||% list())$display_gene_name %||% (gene_meta %||% list())$matched_gene_name %||% ""))
    matched_gene_id <- trimws(as.character((gene_meta %||% list())$matched_gene_id %||% ""))

    list(
        plot_id = trimws(as.character(plot_id %||% "")),
        title = trimws(as.character(title_txt %||% "")),
        organism_name = org_name,
        gene_label = gene_label,
        matched_gene_id = matched_gene_id,
        annotation_path = trimws(as.character(annotation_path %||% "")),
        genome_path = trimws(as.character(genome_path %||% "")),
        seqid = seqid_txt,
        strand = strand_txt,
        gene_start = as.integer(round(gene_start)),
        gene_end = as.integer(round(gene_end)),
        tx_start = if (is.finite(tx_start)) as.integer(round(tx_start)) else NA_integer_,
        tx_end = if (is.finite(tx_end)) as.integer(round(tx_end)) else NA_integer_,
        flank_bp = flank_int,
        window_start = as.integer(window_start),
        window_end = as.integer(window_end),
        chr_len = if (is.finite(chr_len)) as.integer(round(chr_len)) else NA_integer_,
        has_genome = nzchar(trimws(as.character(genome_path %||% ""))) && file.exists(as.character(genome_path %||% ""))
    )
}

extract_locus_window_sequence <- function(locus_ctx) {
    if (is.null(locus_ctx) || !is.list(locus_ctx)) {
        return("")
    }
    fasta_path <- trimws(as.character(locus_ctx$genome_path %||% ""))
    seqid_txt <- trimws(as.character(locus_ctx$seqid %||% ""))
    raw_s <- locus_ctx$window_start %||% NA_integer_
    raw_e <- locus_ctx$window_end %||% NA_integer_
    s <- suppressWarnings(as.integer(raw_s[1L]))
    e <- suppressWarnings(as.integer(raw_e[1L]))
    if (!nzchar(fasta_path) || !file.exists(fasta_path) || !nzchar(seqid_txt) ||
        length(s) == 0L || !is.finite(s) || length(e) == 0L || !is.finite(e) || e < s) {
        return("")
    }
    seq_txt <- tryCatch(
        extract_sequence_from_fasta(fasta_path, seqid_txt, s, e),
        error = function(e) ""
    )
    toupper(gsub("[^ACGTN]", "", as.character(seq_txt %||% "")))
}

build_lastz_job_spec <- function(reference_ctx,
                                 query_ctx,
                                 out_path = "",
                                 format_name = "general",
                                 extra_args = character(0)) {
    ref_ctx <- reference_ctx %||% NULL
    qry_ctx <- query_ctx %||% NULL
    if (is.null(ref_ctx) || is.null(qry_ctx)) {
        return(NULL)
    }
    list(
        engine = "lastz",
        binary = trimws(as.character(Sys.getenv("APP_LASTZ_BIN", "lastz") %||% "lastz")),
        reference = ref_ctx,
        query = qry_ctx,
        format = trimws(as.character(format_name %||% "general")),
        out_path = trimws(as.character(out_path %||% "")),
        extra_args = as.character(extra_args %||% character(0))
    )
}

resolve_lastz_binary <- function(candidate = Sys.getenv("APP_LASTZ_BIN", "lastz")) {
    candidate_txt <- trimws(as.character(candidate %||% "lastz"))
    if (!nzchar(candidate_txt)) {
        candidate_txt <- "lastz"
    }

    if (grepl("/", candidate_txt, fixed = TRUE)) {
        if (file.exists(candidate_txt)) {
            return(list(
                available = TRUE,
                path = normalizePath(candidate_txt, winslash = "/", mustWork = FALSE),
                source = "explicit_path",
                requested = candidate_txt
            ))
        }
    }

    path_hit <- Sys.which(candidate_txt)
    if (nzchar(path_hit)) {
        return(list(
            available = TRUE,
            path = normalizePath(path_hit, winslash = "/", mustWork = FALSE),
            source = if (identical(candidate_txt, "lastz")) "path" else "named_binary",
            requested = candidate_txt
        ))
    }

    common_paths <- c(
        "/opt/homebrew/bin/lastz",
        "/usr/local/bin/lastz",
        "/usr/bin/lastz"
    )
    common_hit <- common_paths[file.exists(common_paths)][1]
    if (length(common_hit) == 1L && nzchar(common_hit)) {
        return(list(
            available = TRUE,
            path = normalizePath(common_hit, winslash = "/", mustWork = FALSE),
            source = "common_path",
            requested = candidate_txt
        ))
    }

    list(
        available = FALSE,
        path = "",
        source = "missing",
        requested = candidate_txt
    )
}

lastz_process_timeout_ms <- function(default_seconds = 90L) {
    raw <- trimws(as.character(Sys.getenv(
        "APP_LASTZ_TIMEOUT_SECONDS",
        as.character(default_seconds)
    ) %||% as.character(default_seconds)))
    seconds <- suppressWarnings(as.integer(raw))
    if (!is.finite(seconds) || is.na(seconds) || seconds < 5L) {
        seconds <- as.integer(default_seconds)
    }
    as.integer(min(seconds, 3600L) * 1000L)
}

lastz_max_sequence_bp <- function(default_bp = 2000000L) {
    raw <- trimws(as.character(Sys.getenv(
        "APP_LASTZ_MAX_SEQUENCE_BP",
        as.character(default_bp)
    ) %||% as.character(default_bp)))
    max_bp <- suppressWarnings(as.integer(raw))
    if (!is.finite(max_bp) || is.na(max_bp) || max_bp < 1000L) {
        max_bp <- as.integer(default_bp)
    }
    max_bp
}

lastz_disk_cache_enabled <- function() {
    isTRUE(app_env_flag("APP_LASTZ_DISK_CACHE", TRUE))
}

lastz_disk_cache_dir <- function(base_dir = ".", create = FALSE) {
    override <- app_env_path("APP_LASTZ_DISK_CACHE_DIR", "")
    root <- if (nzchar(override)) {
        override
    } else {
        file.path(get_cgv_cache_root(base_dir), "lastz_alignments")
    }
    root <- normalizePath(root, winslash = "/", mustWork = FALSE)
    if (isTRUE(create) && nzchar(root) && !dir.exists(root)) {
        dir.create(root, recursive = TRUE, showWarnings = FALSE)
    }
    root
}

lastz_public_genome_roots <- function(base_dir = ".") {
    roots <- c(
        app_env_path("CGV_GENOMES_DIR", ""),
        file.path(get_cgv_data_root(base_dir), "genomes"),
        file.path(normalizePath(base_dir, winslash = "/", mustWork = FALSE), "genomes"),
        "/app/genomes"
    )
    roots <- unique(vapply(roots[nzchar(roots)], function(path) {
        normalizePath(path, winslash = "/", mustWork = FALSE)
    }, character(1)))
    roots[nzchar(roots)]
}

lastz_context_is_public_genome <- function(ctx, base_dir = ".") {
    path <- trimws(as.character((ctx %||% list())$genome_path %||% ""))
    if (!nzchar(path) || !file.exists(path)) return(FALSE)
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    roots <- lastz_public_genome_roots(base_dir)
    any(vapply(roots, function(root) {
        identical(path, root) || startsWith(path, paste0(root, "/"))
    }, logical(1)))
}

lastz_disk_cache_eligible <- function(reference_ctx, query_ctx, base_dir = ".") {
    isTRUE(lastz_disk_cache_enabled()) &&
        isTRUE(lastz_context_is_public_genome(reference_ctx, base_dir = base_dir)) &&
        isTRUE(lastz_context_is_public_genome(query_ctx, base_dir = base_dir))
}

lastz_disk_cache_path <- function(cache_key, base_dir = ".", create = FALSE) {
    key <- gsub("[^A-Za-z0-9._-]+", "_", trimws(as.character(cache_key %||% "")))
    if (!nzchar(key)) return("")
    file.path(lastz_disk_cache_dir(base_dir = base_dir, create = create), paste0(key, ".rds"))
}

lastz_disk_cache_limits <- function() {
    list(
        max_bytes = parse_positive_bytes_env_mb("APP_LASTZ_DISK_CACHE_MAX_MB", 512),
        ttl_seconds = as.numeric(app_env_int("APP_LASTZ_DISK_CACHE_TTL_DAYS", 30L, min_value = 1L, max_value = 3650L)) * 86400
    )
}

maintain_lastz_disk_cache <- function(base_dir = ".", force = FALSE) {
    cache_dir <- lastz_disk_cache_dir(base_dir = base_dir, create = FALSE)
    if (!dir.exists(cache_dir)) return(invisible(NULL))
    key <- normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
    last_run <- suppressWarnings(as.numeric(get0(key, envir = .lastz_disk_cache_maintenance, inherits = FALSE, ifnotfound = NA_real_)))
    now <- as.numeric(Sys.time())
    if (!isTRUE(force) && is.finite(last_run) && now - last_run < 60) return(invisible(NULL))
    assign(key, now, envir = .lastz_disk_cache_maintenance)

    files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(files) == 0L) return(invisible(NULL))
    info <- file.info(files)
    limits <- lastz_disk_cache_limits()
    ages <- now - as.numeric(info$mtime)
    expired <- !is.finite(ages) | ages > limits$ttl_seconds
    if (any(expired)) {
        unlink(files[expired], force = TRUE)
        files <- files[!expired]
        info <- info[!expired, , drop = FALSE]
    }
    if (length(files) == 0L) return(invisible(NULL))
    sizes <- suppressWarnings(as.numeric(info$size))
    sizes[!is.finite(sizes)] <- 0
    total_bytes <- sum(sizes)
    if (total_bytes > limits$max_bytes) {
        oldest_first <- order(as.numeric(info$mtime), na.last = FALSE)
        remove_idx <- integer(0)
        for (idx in oldest_first) {
            if (total_bytes <= limits$max_bytes) break
            remove_idx <- c(remove_idx, idx)
            total_bytes <- total_bytes - sizes[[idx]]
        }
        if (length(remove_idx) > 0L) unlink(files[remove_idx], force = TRUE)
    }
    invisible(NULL)
}

lastz_disk_cache_get <- function(cache_key,
                                 reference_ctx,
                                 query_ctx,
                                 base_dir = ".",
                                 touch = TRUE) {
    if (!isTRUE(lastz_disk_cache_eligible(reference_ctx, query_ctx, base_dir = base_dir))) return(NULL)
    path <- lastz_disk_cache_path(cache_key, base_dir = base_dir, create = FALSE)
    if (!nzchar(path) || !file.exists(path)) return(NULL)
    info <- tryCatch(file.info(path), error = function(e) NULL)
    limits <- lastz_disk_cache_limits()
    if (is.null(info) || nrow(info) == 0L || as.numeric(Sys.time()) - as.numeric(info$mtime[[1L]]) > limits$ttl_seconds) {
        unlink(path, force = TRUE)
        return(NULL)
    }
    payload <- tryCatch(readRDS(path), error = function(e) NULL)
    valid <- is.list(payload) &&
        identical(as.character(payload$version %||% ""), "lastz-disk-v1") &&
        identical(as.character(payload$cache_key %||% ""), as.character(cache_key %||% "")) &&
        is.list(payload$result) &&
        identical(trimws(as.character(payload$result$status %||% "")), "ok")
    if (!isTRUE(valid)) {
        unlink(path, force = TRUE)
        return(NULL)
    }
    if (isTRUE(touch)) try(suppressWarnings(Sys.setFileTime(path, Sys.time())), silent = TRUE)
    payload$result
}

lastz_disk_cache_set <- function(cache_key,
                                 result,
                                 reference_ctx,
                                 query_ctx,
                                 base_dir = ".") {
    if (!is.list(result) || !identical(trimws(as.character(result$status %||% "")), "ok")) return(invisible(result))
    if (!isTRUE(lastz_disk_cache_eligible(reference_ctx, query_ctx, base_dir = base_dir))) return(invisible(result))
    path <- lastz_disk_cache_path(cache_key, base_dir = base_dir, create = TRUE)
    cache_dir <- dirname(path)
    if (!nzchar(path) || !dir.exists(cache_dir) || file.access(cache_dir, mode = 2L) != 0L) return(invisible(result))
    tmp <- tempfile(pattern = ".lastz-cache-", tmpdir = cache_dir, fileext = ".rds")
    on.exit(if (file.exists(tmp)) unlink(tmp, force = TRUE), add = TRUE)
    payload <- list(
        version = "lastz-disk-v1",
        cache_key = as.character(cache_key %||% ""),
        created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        result = result
    )
    saved <- tryCatch({
        saveRDS(payload, tmp, compress = "gzip")
        TRUE
    }, error = function(e) FALSE)
    if (isTRUE(saved)) {
        if (file.exists(path) || !file.rename(tmp, path)) unlink(tmp, force = TRUE)
    }
    maintain_lastz_disk_cache(base_dir = base_dir)
    invisible(result)
}

cgv_global_lastz_slot_count <- function() {
    app_env_int("APP_LASTZ_GLOBAL_WORKERS", 0L, min_value = 0L, max_value = 16L)
}

cgv_with_global_lastz_slot <- function(fn, base_dir = ".") {
    slots <- cgv_global_lastz_slot_count()
    if (slots < 1L) return(fn())
    root <- file.path(get_cgv_cache_root(base_dir), "lastz_global_slots")
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    try(Sys.chmod(root, mode = "0777", use_umask = FALSE), silent = TRUE)
    wait_seconds <- app_env_int(
        "APP_LASTZ_GLOBAL_QUEUE_WAIT_SECONDS",
        1800L,
        min_value = 30L,
        max_value = 86400L
    )
    stale_seconds <- max(
        300L,
        as.integer(ceiling(lastz_process_timeout_ms() / 1000)) + 120L
    )
    deadline <- as.numeric(Sys.time()) + wait_seconds
    acquired <- ""
    repeat {
        for (slot in seq_len(slots)) {
            candidate <- file.path(root, sprintf("slot-%02d.lock", slot))
            if (isTRUE(dir.create(candidate, recursive = FALSE, showWarnings = FALSE))) {
                acquired <- candidate
                break
            }
            info <- tryCatch(file.info(candidate), error = function(e) NULL)
            age <- if (!is.null(info) && nrow(info) > 0L && !is.na(info$mtime[[1L]])) {
                as.numeric(Sys.time()) - as.numeric(info$mtime[[1L]])
            } else {
                NA_real_
            }
            if (is.finite(age) && age > stale_seconds) {
                unlink(candidate, recursive = TRUE, force = TRUE)
            }
        }
        if (nzchar(acquired)) break
        if (as.numeric(Sys.time()) >= deadline) {
            return(list(
                status = 124L,
                stdout = character(0),
                stderr = "Timed out while waiting for the shared LASTZ execution slot.",
                timeout = TRUE
            ))
        }
        Sys.sleep(0.1)
    }
    on.exit(
        if (nzchar(acquired) && dir.exists(acquired)) {
            unlink(acquired, recursive = TRUE, force = TRUE)
        },
        add = TRUE
    )
    fn()
}

run_lastz_process <- function(binary, args) {
    run <- function() {
        if (requireNamespace("processx", quietly = TRUE)) {
            return(processx::run(
                binary,
                args = args,
                echo = FALSE,
                error_on_status = FALSE,
                timeout = lastz_process_timeout_ms()
            ))
        }
        sys2_out <- system2(binary, args = args, stdout = TRUE, stderr = TRUE)
        sys2_status <- attr(sys2_out, "status")
        list(
            status = if (is.null(sys2_status) || length(sys2_status) == 0L) {
                0L
            } else {
                suppressWarnings(as.integer(sys2_status[1L]))
            },
            stdout = if (is.character(sys2_out)) sys2_out else character(0),
            stderr = character(0),
            timeout = FALSE
        )
    }
    cgv_with_global_lastz_slot(run)
}

wrap_fasta_sequence_lines <- function(seq_txt, width = 80L) {
    seq_txt <- gsub("\\s+", "", as.character(seq_txt %||% ""))
    if (!nzchar(seq_txt)) {
        return(character(0))
    }
    width <- suppressWarnings(as.integer(width))
    if (!is.finite(width) || width < 1L) {
        width <- 80L
    }
    starts <- seq.int(1L, nchar(seq_txt), by = width)
    substring(seq_txt, starts, pmin(starts + width - 1L, nchar(seq_txt)))
}

write_locus_temp_fasta <- function(seq_txt, record_name = "sequence", dir_path = tempdir()) {
    seq_clean <- toupper(gsub("[^ACGTN]", "", as.character(seq_txt %||% "")))
    if (!nzchar(seq_clean)) {
        return("")
    }
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    record_txt <- trimws(gsub("[^A-Za-z0-9._:-]+", "_", as.character(record_name %||% "sequence")))
    if (!nzchar(record_txt)) {
        record_txt <- "sequence"
    }
    out_path <- tempfile(pattern = paste0("ctv_", record_txt, "_"), fileext = ".fa", tmpdir = dir_path)
    writeLines(c(paste0(">", record_txt), wrap_fasta_sequence_lines(seq_clean, width = 80L)), out_path, useBytes = TRUE)
    out_path
}

parse_lastz_general_output <- function(raw_lines,
                                       reference_ctx = NULL,
                                       query_ctx = NULL) {
    if (is.null(raw_lines) || length(raw_lines) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    txt <- paste(as.character(raw_lines), collapse = "\n")
    txt <- trimws(txt)
    if (!nzchar(txt)) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    con <- textConnection(txt)
    on.exit(close(con), add = TRUE)
    fields <- c(
        "name1", "start1", "end1",
        "name2", "strand2", "start2_pos", "end2_pos",
        "identity_pct", "coverage_pct",
        "length1", "length2",
        "nmatch", "nmismatch", "ncolumn", "score", "cigarx"
    )
    df <- tryCatch(
        utils::read.table(
            con,
            sep = "\t",
            quote = "",
            comment.char = "",
            header = FALSE,
            stringsAsFactors = FALSE,
            fill = TRUE,
            col.names = fields
        ),
        error = function(e) data.frame(stringsAsFactors = FALSE)
    )
    if (!is.data.frame(df) || nrow(df) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    num_cols <- c(
        "start1", "end1", "start2_pos", "end2_pos",
        "identity_pct", "coverage_pct",
        "length1", "length2",
        "nmatch", "nmismatch", "ncolumn", "score"
    )
    for (nm in intersect(num_cols, names(df))) {
        raw_num <- as.character(df[[nm]] %||% "")
        raw_num <- gsub("%", "", raw_num, fixed = TRUE)
        df[[nm]] <- suppressWarnings(as.numeric(raw_num))
    }
    df$strand2 <- trimws(as.character(df$strand2 %||% ""))
    df$cigarx <- trimws(as.character(df$cigarx %||% ""))
    df$ref_start <- suppressWarnings(as.integer(round(df$start1)))
    df$ref_end <- suppressWarnings(as.integer(round(df$end1)))
    df$qry_start <- suppressWarnings(as.integer(round(pmin(df$start2_pos, df$end2_pos, na.rm = TRUE))))
    df$qry_end <- suppressWarnings(as.integer(round(pmax(df$start2_pos, df$end2_pos, na.rm = TRUE))))
    df$align_len <- suppressWarnings(as.integer(round(pmax(df$length1, df$length2, df$ncolumn, na.rm = TRUE))))
    df$ref_plot_id <- as.character((reference_ctx %||% list())$plot_id %||% "")
    df$query_plot_id <- as.character((query_ctx %||% list())$plot_id %||% "")
    df$ref_seqid <- as.character((reference_ctx %||% list())$seqid %||% "")
    df$query_seqid <- as.character((query_ctx %||% list())$seqid %||% "")
    raw_ref_ws <- (reference_ctx %||% list())$window_start %||% NA_integer_
    raw_qry_ws <- (query_ctx %||% list())$window_start %||% NA_integer_
    df$ref_window_start <- suppressWarnings(as.integer(raw_ref_ws[1L]))
    df$query_window_start <- suppressWarnings(as.integer(raw_qry_ws[1L]))
    if (nrow(df) > 0L && length(df$ref_window_start) > 0L && all(is.finite(df$ref_window_start))) {
        df$ref_genomic_start <- df$ref_window_start + df$ref_start - 1L
        df$ref_genomic_end <- df$ref_window_start + df$ref_end - 1L
    } else {
        df$ref_genomic_start <- df$ref_start
        df$ref_genomic_end <- df$ref_end
    }
    if (nrow(df) > 0L && length(df$query_window_start) > 0L && all(is.finite(df$query_window_start))) {
        df$query_genomic_start <- df$query_window_start + df$qry_start - 1L
        df$query_genomic_end <- df$query_window_start + df$qry_end - 1L
    } else {
        df$query_genomic_start <- df$qry_start
        df$query_genomic_end <- df$qry_end
    }
    df
}

parse_lastz_cigarx_segments <- function(df_blocks,
                                        reference_ctx = NULL,
                                        query_ctx = NULL) {
    if (!is.data.frame(df_blocks) || nrow(df_blocks) == 0L || !"cigarx" %in% names(df_blocks)) {
        return(data.frame(stringsAsFactors = FALSE))
    }

    ctx_width <- function(ctx) {
        start_bp <- suppressWarnings(as.integer((ctx %||% list())$window_start %||% NA_integer_))
        end_bp <- suppressWarnings(as.integer((ctx %||% list())$window_end %||% NA_integer_))
        width <- end_bp - start_bp + 1L
        if (!is.finite(width) || width < 1L) NA_integer_ else as.integer(width)
    }
    parse_ops <- function(cigar_txt) {
        cigar_txt <- toupper(gsub("\\s+", "", trimws(as.character(cigar_txt %||% ""))))
        if (!nzchar(cigar_txt)) return(NULL)
        match_obj <- gregexpr("[0-9]*[=XID]", cigar_txt, perl = TRUE)[[1L]]
        if (length(match_obj) == 0L || identical(match_obj[[1L]], -1L)) return(NULL)
        tokens <- regmatches(cigar_txt, list(match_obj))[[1L]]
        if (!identical(paste(tokens, collapse = ""), cigar_txt)) return(NULL)
        ops <- substring(tokens, nchar(tokens), nchar(tokens))
        len_txt <- substring(tokens, 1L, pmax(0L, nchar(tokens) - 1L))
        lens <- suppressWarnings(as.integer(ifelse(nzchar(len_txt), len_txt, "1")))
        if (any(!is.finite(lens)) || any(lens < 1L)) return(NULL)
        data.frame(op = ops, len = lens, stringsAsFactors = FALSE)
    }

    ref_window_start <- suppressWarnings(as.integer((reference_ctx %||% list())$window_start %||% 1L))
    query_window_start <- suppressWarnings(as.integer((query_ctx %||% list())$window_start %||% 1L))
    if (!is.finite(ref_window_start)) ref_window_start <- 1L
    if (!is.finite(query_window_start)) query_window_start <- 1L
    ref_width <- ctx_width(reference_ctx)
    qry_width <- ctx_width(query_ctx)
    out_rows <- list()

    for (alignment_idx in seq_len(nrow(df_blocks))) {
        block <- df_blocks[alignment_idx, , drop = FALSE]
        ops <- parse_ops(block$cigarx[[1L]])
        if (!is.data.frame(ops) || nrow(ops) == 0L) next

        ref_cursor <- suppressWarnings(as.integer(block$start1[[1L]]))
        qry_low <- suppressWarnings(as.integer(min(block$start2_pos[[1L]], block$end2_pos[[1L]], na.rm = TRUE)))
        qry_high <- suppressWarnings(as.integer(max(block$start2_pos[[1L]], block$end2_pos[[1L]], na.rm = TRUE)))
        strand_txt <- if (identical(trimws(as.character(block$strand2[[1L]] %||% "+")), "-")) "-" else "+"
        qry_direction <- if (identical(strand_txt, "-")) -1L else 1L
        qry_cursor <- if (identical(strand_txt, "-")) qry_high else qry_low
        if (!all(is.finite(c(ref_cursor, qry_cursor)))) next

        segment_rank <- 0L
        segment_ref_start <- NA_integer_
        segment_qry_path_start <- NA_integer_
        segment_matches <- 0L
        segment_mismatches <- 0L

        flush_segment <- function() {
            segment_bp <- as.integer(segment_matches + segment_mismatches)
            if (!is.finite(segment_ref_start) || !is.finite(segment_qry_path_start) || segment_bp < 1L) {
                return(invisible(NULL))
            }
            segment_rank <<- segment_rank + 1L
            ref_local_end <- as.integer(segment_ref_start + segment_bp - 1L)
            qry_path_end <- as.integer(segment_qry_path_start + qry_direction * (segment_bp - 1L))
            qry_plus_start <- min(segment_qry_path_start, qry_path_end)
            qry_plus_end <- max(segment_qry_path_start, qry_path_end)
            if (identical(strand_txt, "-") && is.finite(qry_width)) {
                qry_axis_start <- as.integer(qry_width - qry_plus_end + 1L)
                qry_axis_end <- as.integer(qry_width - qry_plus_start + 1L)
            } else {
                qry_axis_start <- as.integer(qry_plus_start)
                qry_axis_end <- as.integer(qry_plus_end)
            }
            identity_pct <- 100 * as.numeric(segment_matches) / as.numeric(segment_bp)
            out_rows[[length(out_rows) + 1L]] <<- data.frame(
                ref_plot_id = as.character((reference_ctx %||% list())$plot_id %||% ""),
                query_plot_id = as.character((query_ctx %||% list())$plot_id %||% ""),
                ref_seqid = as.character((reference_ctx %||% list())$seqid %||% ""),
                query_seqid = as.character((query_ctx %||% list())$seqid %||% ""),
                ref_start = as.numeric(ref_window_start + segment_ref_start - 1L),
                ref_end = as.numeric(ref_window_start + ref_local_end - 1L),
                qry_start = as.numeric(query_window_start + qry_plus_start - 1L),
                qry_end = as.numeric(query_window_start + qry_plus_end - 1L),
                ref_local_start = as.integer(segment_ref_start),
                ref_local_end = as.integer(ref_local_end),
                qry_lav_start = as.integer(qry_axis_start),
                qry_lav_end = as.integer(qry_axis_end),
                strand = strand_txt,
                identity_pct = as.numeric(identity_pct),
                segment_bp = segment_bp,
                alignment_id = as.integer(alignment_idx),
                segment_rank = as.integer(segment_rank),
                score = suppressWarnings(as.numeric(block$score[[1L]] %||% NA_real_)),
                ref_width = ref_width,
                qry_width = qry_width,
                ref_window_start = as.integer(ref_window_start),
                query_window_start = as.integer(query_window_start),
                source_engine = "lastz",
                source_format = "general-cigarx",
                stringsAsFactors = FALSE
            )
            invisible(NULL)
        }

        for (op_idx in seq_len(nrow(ops))) {
            op <- ops$op[[op_idx]]
            run_len <- ops$len[[op_idx]]
            if (op %in% c("=", "X")) {
                if (!is.finite(segment_ref_start)) {
                    segment_ref_start <- ref_cursor
                    segment_qry_path_start <- qry_cursor
                }
                if (identical(op, "=")) {
                    segment_matches <- segment_matches + run_len
                } else {
                    segment_mismatches <- segment_mismatches + run_len
                }
                ref_cursor <- ref_cursor + run_len
                qry_cursor <- qry_cursor + qry_direction * run_len
            } else {
                flush_segment()
                segment_ref_start <- NA_integer_
                segment_qry_path_start <- NA_integer_
                segment_matches <- 0L
                segment_mismatches <- 0L
                if (identical(op, "I")) {
                    qry_cursor <- qry_cursor + qry_direction * run_len
                } else if (identical(op, "D")) {
                    ref_cursor <- ref_cursor + run_len
                }
            }
        }
        flush_segment()
    }

    out_df <- do.call(rbind, out_rows)
    if (!is.data.frame(out_df) || nrow(out_df) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    compute_gap_free_segment_identity(extract_gap_free_segments_from_alignment(out_df))
}

run_local_locus_alignment <- function(reference_ctx,
                                      query_ctx,
                                      engine = "lastz",
                                      out_path = "",
                                      format_name = "general",
                                      extra_args = character(0)) {
    lastz_perf <- app_perf_new_run("LASTZ_GENERAL")
    app_perf_mark(lastz_perf, "start", "LASTZ")
    engine_txt <- tolower(trimws(as.character(engine %||% "lastz")))
    if (!engine_txt %in% c("lastz")) {
        return(list(
            status = "unsupported_engine",
            engine = engine_txt,
            blocks = data.frame(stringsAsFactors = FALSE)
        ))
    }
    job <- build_lastz_job_spec(reference_ctx, query_ctx, out_path = out_path, format_name = format_name, extra_args = extra_args)
    if (is.null(job)) {
        return(list(
            status = "invalid_input",
            engine = engine_txt,
            blocks = data.frame(stringsAsFactors = FALSE)
        ))
    }
    bin_info <- resolve_lastz_binary(job$binary %||% "lastz")
    bin_path <- trimws(as.character(bin_info$path %||% ""))
    if (!isTRUE(bin_info$available) || !nzchar(bin_path)) {
        return(list(
            status = "engine_unavailable",
            engine = engine_txt,
            binary = as.character(bin_info$requested %||% job$binary %||% "lastz"),
            binary_path = "",
            binary_source = as.character(bin_info$source %||% "missing"),
            setup_hint = "Install LASTZ locally or set APP_LASTZ_BIN to the binary path.",
            job = job,
            blocks = data.frame(stringsAsFactors = FALSE)
        ))
    }
    ref_seq <- extract_locus_window_sequence(reference_ctx)
    qry_seq <- extract_locus_window_sequence(query_ctx)
    if (!nzchar(ref_seq) || !nzchar(qry_seq)) {
        return(list(
            status = "missing_sequence",
            engine = engine_txt,
            binary = bin_path,
            binary_path = bin_path,
            binary_source = as.character(bin_info$source %||% ""),
            job = job,
            blocks = data.frame(stringsAsFactors = FALSE)
        ))
    }
    max_sequence_bp <- lastz_max_sequence_bp()
    if (nchar(ref_seq) > max_sequence_bp || nchar(qry_seq) > max_sequence_bp) {
        return(list(
            status = "window_too_large",
            engine = engine_txt,
            binary = bin_path,
            binary_path = bin_path,
            binary_source = as.character(bin_info$source %||% ""),
            job = job,
            reference_width = nchar(ref_seq),
            query_width = nchar(qry_seq),
            setup_hint = sprintf(
                "Reduce the locus span below %s bp before running LASTZ.",
                format(max_sequence_bp, big.mark = ",")
            ),
            blocks = data.frame(stringsAsFactors = FALSE)
        ))
    }
    run_dir <- tempfile(pattern = "ctv_lastz_")
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    ref_name <- paste0("ref_", sanitize_cache_key((reference_ctx %||% list())$plot_id %||% "reference"))
    qry_name <- paste0("qry_", sanitize_cache_key((query_ctx %||% list())$plot_id %||% "query"))
    ref_fa <- write_locus_temp_fasta(ref_seq, record_name = ref_name, dir_path = run_dir)
    qry_fa <- write_locus_temp_fasta(qry_seq, record_name = qry_name, dir_path = run_dir)
    if (!nzchar(ref_fa) || !nzchar(qry_fa)) {
        unlink(run_dir, recursive = TRUE, force = TRUE)
        return(list(
            status = "temp_fasta_failed",
            engine = engine_txt,
            binary = bin_path,
            binary_path = bin_path,
            binary_source = as.character(bin_info$source %||% ""),
            job = job,
            blocks = data.frame(stringsAsFactors = FALSE)
        ))
    }
    out_fields <- paste(
        c(
            "name1", "start1", "end1",
            "name2", "strand2", "start2+", "end2+",
            "id%", "cov%",
            "length1", "length2",
            "nmatch", "nmismatch", "ncolumn", "score", "cigarx"
        ),
        collapse = ","
    )
    out_file <- if (nzchar(trimws(as.character(out_path %||% "")))) {
        as.character(out_path)
    } else {
        file.path(run_dir, "lastz_general.tsv")
    }
    args <- c(
        ref_fa,
        qry_fa,
        paste0("--format=general-:", out_fields),
        "--ambiguous=iupac",
        paste0("--output=", out_file)
    )
    if (length(extra_args) > 0L) {
        args <- c(args, as.character(extra_args))
    }
    run_res <- tryCatch(
        run_lastz_process(bin_path, args),
        error = function(e) list(
            status = 1L,
            stdout = character(0),
            stderr = as.character(e$message %||% "unknown error"),
            timeout = grepl("timed?\\s*out|timeout", conditionMessage(e), ignore.case = TRUE)
        )
    )
    app_perf_mark(
        lastz_perf,
        sprintf("system_done ref_bp=%d query_bp=%d", as.integer(nchar(ref_seq)), as.integer(nchar(qry_seq))),
        "LASTZ"
    )
    exit_status <- suppressWarnings(as.integer(run_res$status[1L] %||% 1L))
    if (length(exit_status) == 0L || is.na(exit_status)) exit_status <- 1L
    raw_lines <- character(0)
    if (file.exists(out_file)) {
        raw_lines <- tryCatch(readLines(out_file, warn = FALSE), error = function(e) character(0))
    }
    parsed_blocks <- parse_lastz_general_output(raw_lines, reference_ctx = reference_ctx, query_ctx = query_ctx)
    parsed_segments <- parse_lastz_cigarx_segments(parsed_blocks, reference_ctx = reference_ctx, query_ctx = query_ctx)
    unlink(run_dir, recursive = TRUE, force = TRUE)
    if (length(exit_status) == 0L || !is.finite(exit_status)) {
        exit_status <- 1L
    }
    timed_out <- isTRUE(run_res$timeout)
    status_txt <- if (timed_out) {
        "engine_timeout"
    } else if (exit_status == 0L) {
        "ok"
    } else {
        "engine_error"
    }
    app_perf_mark(
        lastz_perf,
        sprintf("finish status=%s exit=%d blocks=%d", status_txt, as.integer(exit_status), as.integer(nrow(parsed_blocks %||% data.frame()))),
        "LASTZ"
    )
    list(
        status = status_txt,
        engine = engine_txt,
        binary = bin_path,
        binary_path = bin_path,
        binary_source = as.character(bin_info$source %||% ""),
        job = job,
        reference_width = nchar(ref_seq),
        query_width = nchar(qry_seq),
        exit_status = exit_status,
        timed_out = timed_out,
        stderr = as.character(run_res$stderr %||% character(0)),
        stdout = as.character(run_res$stdout %||% character(0)),
        blocks = parsed_blocks,
        segments = parsed_segments,
        canonical_format = "general-cigarx"
    )
}

build_lastz_multipip_job_spec <- function(reference_ctx,
                                          query_ctx,
                                          out_path = "",
                                          format_name = "lav",
                                          extra_args = character(0)) {
    build_lastz_job_spec(
        reference_ctx = reference_ctx,
        query_ctx = query_ctx,
        out_path = out_path,
        format_name = format_name,
        extra_args = extra_args
    )
}

parse_lastz_lav_output <- function(raw_lines,
                                   reference_ctx = NULL,
                                   query_ctx = NULL) {
    if (is.null(raw_lines) || length(raw_lines) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    txt <- paste(as.character(raw_lines), collapse = "\n")
    txt <- trimws(txt)
    if (!nzchar(txt)) {
        return(data.frame(stringsAsFactors = FALSE))
    }

    raw_ref_end <- (reference_ctx %||% list())$window_end %||% NA_integer_
    raw_ref_start <- (reference_ctx %||% list())$window_start %||% NA_integer_
    raw_qry_end <- (query_ctx %||% list())$window_end %||% NA_integer_
    raw_qry_start <- (query_ctx %||% list())$window_start %||% NA_integer_
    ref_width <- suppressWarnings(as.integer(raw_ref_end[1L])) -
        suppressWarnings(as.integer(raw_ref_start[1L])) + 1L
    qry_width <- suppressWarnings(as.integer(raw_qry_end[1L])) -
        suppressWarnings(as.integer(raw_qry_start[1L])) + 1L
    if (length(ref_width) == 0L || !is.finite(ref_width) || ref_width <= 0L) ref_width <- NA_integer_
    if (length(qry_width) == 0L || !is.finite(qry_width) || qry_width <= 0L) qry_width <- NA_integer_

    lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
    in_s_block <- FALSE
    in_a_block <- FALSE
    s_line_idx <- 0L
    current_strand <- "+"
    current_alignment_id <- 0L
    current_score <- NA_real_
    segment_rank <- 0L
    out_rows <- list()

    lav_plus_to_genomic <- function(qry_start_plus, qry_end_plus, strand_txt, query_width, window_start) {
        if (!is.finite(query_width) || !is.finite(window_start)) {
            return(c(NA_real_, NA_real_))
        }
        if (identical(strand_txt, "-")) {
            plus_start <- query_width - max(qry_start_plus, qry_end_plus) + 1L
            plus_end <- query_width - min(qry_start_plus, qry_end_plus) + 1L
        } else {
            plus_start <- min(qry_start_plus, qry_end_plus)
            plus_end <- max(qry_start_plus, qry_end_plus)
        }
        c(
            as.numeric(window_start) + as.numeric(plus_start) - 1,
            as.numeric(window_start) + as.numeric(plus_end) - 1
        )
    }

    for (ln in lines) {
        line_txt <- trimws(as.character(ln %||% ""))
        if (!nzchar(line_txt)) next

        if (identical(line_txt, "s {")) {
            in_s_block <- TRUE
            s_line_idx <- 0L
            next
        }
        if (in_s_block) {
            if (identical(line_txt, "}")) {
                in_s_block <- FALSE
                next
            }
            if (startsWith(line_txt, "\"")) {
                s_line_idx <- s_line_idx + 1L
                path_txt <- sub("^\"([^\"]*)\".*$", "\\1", line_txt)
                if (s_line_idx >= 2L) {
                    current_strand <- if (endsWith(path_txt, "-")) "-" else "+"
                }
            }
            next
        }

        if (identical(line_txt, "a {")) {
            in_a_block <- TRUE
            current_alignment_id <- current_alignment_id + 1L
            current_score <- NA_real_
            segment_rank <- 0L
            next
        }
        if (in_a_block && identical(line_txt, "}")) {
            in_a_block <- FALSE
            next
        }
        if (!in_a_block) {
            next
        }

        if (grepl("^s\\s+", line_txt)) {
            parts <- strsplit(line_txt, "\\s+")[[1]]
            if (length(parts) >= 2L) {
                current_score <- suppressWarnings(as.numeric(parts[[2]]))
            }
            next
        }

        if (grepl("^l\\s+", line_txt)) {
            parts <- strsplit(line_txt, "\\s+")[[1]]
            if (length(parts) < 6L) {
                next
            }
            ref_s <- suppressWarnings(as.integer(parts[[2]]))
            qry_s_lav <- suppressWarnings(as.integer(parts[[3]]))
            ref_e <- suppressWarnings(as.integer(parts[[4]]))
            qry_e_lav <- suppressWarnings(as.integer(parts[[5]]))
            identity_pct <- suppressWarnings(as.numeric(parts[[6]]))
            if (!all(is.finite(c(ref_s, qry_s_lav, ref_e, qry_e_lav, identity_pct)))) {
                next
            }
            segment_rank <- segment_rank + 1L
            ref_bp <- abs(ref_e - ref_s) + 1L
            qry_bp <- abs(qry_e_lav - qry_s_lav) + 1L
            ref_genomic <- c(
                as.numeric((reference_ctx %||% list())$window_start %||% 1L) + min(ref_s, ref_e) - 1,
                as.numeric((reference_ctx %||% list())$window_start %||% 1L) + max(ref_s, ref_e) - 1
            )
            qry_genomic <- lav_plus_to_genomic(
                qry_start_plus = qry_s_lav,
                qry_end_plus = qry_e_lav,
                strand_txt = current_strand,
                query_width = qry_width,
                window_start = as.numeric((query_ctx %||% list())$window_start %||% 1L)
            )
            out_rows[[length(out_rows) + 1L]] <- data.frame(
                ref_plot_id = as.character((reference_ctx %||% list())$plot_id %||% ""),
                query_plot_id = as.character((query_ctx %||% list())$plot_id %||% ""),
                ref_seqid = as.character((reference_ctx %||% list())$seqid %||% ""),
                query_seqid = as.character((query_ctx %||% list())$seqid %||% ""),
                ref_start = min(ref_genomic, na.rm = TRUE),
                ref_end = max(ref_genomic, na.rm = TRUE),
                qry_start = min(qry_genomic, na.rm = TRUE),
                qry_end = max(qry_genomic, na.rm = TRUE),
                ref_local_start = min(ref_s, ref_e),
                ref_local_end = max(ref_s, ref_e),
                qry_lav_start = qry_s_lav,
                qry_lav_end = qry_e_lav,
                strand = current_strand,
                identity_pct = as.numeric(identity_pct),
                segment_bp = as.integer(max(ref_bp, qry_bp)),
                alignment_id = as.integer(current_alignment_id),
                segment_rank = as.integer(segment_rank),
                score = as.numeric(current_score),
                ref_width = ref_width,
                qry_width = qry_width,
                ref_window_start = suppressWarnings(as.integer((reference_ctx %||% list())$window_start %||% NA_integer_)),
                query_window_start = suppressWarnings(as.integer((query_ctx %||% list())$window_start %||% NA_integer_)),
                source_engine = "lastz",
                stringsAsFactors = FALSE
            )
        }
    }

    out_df <- do.call(rbind,  out_rows)
    if (!is.data.frame(out_df) || nrow(out_df) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    out_df
}

extract_gap_free_segments_from_alignment <- function(df_segments) {
    if (!is.data.frame(df_segments) || nrow(df_segments) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    out <- df_segments
    out$identity_pct <- suppressWarnings(as.numeric(out$identity_pct))
    out$segment_bp <- suppressWarnings(as.integer(out$segment_bp))
    out$alignment_id <- suppressWarnings(as.integer(out$alignment_id))
    out$segment_rank <- suppressWarnings(as.integer(out$segment_rank))
    out <- out[
        is.finite(out$identity_pct) &
            is.finite(out$segment_bp) &
            out$segment_bp > 0L &
            is.finite(out$ref_start) &
            is.finite(out$ref_end) &
            is.finite(out$qry_start) &
            is.finite(out$qry_end),
        ,
        drop = FALSE
    ]
    if (nrow(out) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    out[order(out$query_plot_id, out$strand, out$alignment_id, out$segment_rank, out$ref_start, out$qry_start), , drop = FALSE]
}

compute_gap_free_segment_identity <- function(df_segments) {
    if (!is.data.frame(df_segments) || nrow(df_segments) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    out <- df_segments
    out$identity_weighted_bp <- suppressWarnings(as.numeric(out$identity_pct)) * suppressWarnings(as.numeric(out$segment_bp))
    out
}

prune_reference_overlaps_per_query <- function(df_segments) {
    if (!is.data.frame(df_segments) || nrow(df_segments) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }

    subtract_interval_set <- function(start_bp, end_bp, covered_ranges) {
        pending <- data.frame(start = start_bp, end = end_bp, stringsAsFactors = FALSE)
        if (!is.data.frame(covered_ranges) || nrow(covered_ranges) == 0L) {
            return(pending)
        }
        for (ii in seq_len(nrow(covered_ranges))) {
            if (nrow(pending) == 0L) break
            cov_s <- covered_ranges$start[[ii]]
            cov_e <- covered_ranges$end[[ii]]
            next_pending <- list()
            for (jj in seq_len(nrow(pending))) {
                cur_s <- pending$start[[jj]]
                cur_e <- pending$end[[jj]]
                if (cov_e < cur_s || cov_s > cur_e) {
                    next_pending[[length(next_pending) + 1L]] <- data.frame(start = cur_s, end = cur_e, stringsAsFactors = FALSE)
                } else {
                    if (cov_s > cur_s) {
                        next_pending[[length(next_pending) + 1L]] <- data.frame(start = cur_s, end = cov_s - 1L, stringsAsFactors = FALSE)
                    }
                    if (cov_e < cur_e) {
                        next_pending[[length(next_pending) + 1L]] <- data.frame(start = cov_e + 1L, end = cur_e, stringsAsFactors = FALSE)
                    }
                }
            }
            pending <- if (length(next_pending) > 0L) do.call(rbind,  next_pending) else data.frame(start = numeric(0), end = numeric(0))
        }
        pending
    }

    split_segment_piece <- function(seg_row, piece_ref_start, piece_ref_end) {
        delta_start <- as.integer(piece_ref_start - seg_row$ref_start)
        piece_len <- as.integer(piece_ref_end - piece_ref_start + 1L)
        piece_qry_lav_start <- as.integer(seg_row$qry_lav_start + delta_start)
        piece_qry_lav_end <- as.integer(piece_qry_lav_start + piece_len - 1L)
        if (identical(as.character(seg_row$strand %||% "+"), "-")) {
            qry_plus_start <- as.integer(seg_row$qry_width - piece_qry_lav_end + 1L)
            qry_plus_end <- as.integer(seg_row$qry_width - piece_qry_lav_start + 1L)
        } else {
            qry_plus_start <- piece_qry_lav_start
            qry_plus_end <- piece_qry_lav_end
        }
        qry_window_start <- suppressWarnings(as.integer(seg_row$query_window_start %||% NA_integer_))
        seg_row$ref_start <- as.numeric(piece_ref_start)
        seg_row$ref_end <- as.numeric(piece_ref_end)
        seg_row$qry_lav_start <- as.integer(piece_qry_lav_start)
        seg_row$qry_lav_end <- as.integer(piece_qry_lav_end)
        seg_row$qry_start <- as.numeric(qry_window_start + min(qry_plus_start, qry_plus_end) - 1L)
        seg_row$qry_end <- as.numeric(qry_window_start + max(qry_plus_start, qry_plus_end) - 1L)
        seg_row$segment_bp <- as.integer(piece_len)
        seg_row
    }

    iranges_ok <- requireNamespace("IRanges", quietly = TRUE)
    out_rows <- list()
    out_idx <- 0L
    split(df_segments, interaction(df_segments$query_plot_id, df_segments$strand, drop = TRUE)) |>
        lapply(function(df_group) {
            df_ord <- df_group[order(
                -ifelse(is.finite(df_group$identity_pct), df_group$identity_pct, -Inf),
                -ifelse(is.finite(df_group$score), df_group$score, -Inf),
                -ifelse(is.finite(df_group$segment_bp), df_group$segment_bp, -Inf),
                df_group$ref_start,
                df_group$qry_start
            ), , drop = FALSE]
            covered <- if (iranges_ok) IRanges::IRanges() else data.frame(start = numeric(0), end = numeric(0), stringsAsFactors = FALSE)
            for (ii in seq_len(nrow(df_ord))) {
                seg <- df_ord[ii, , drop = FALSE]
                uncovered <- if (iranges_ok) {
                    seg_ir <- IRanges::IRanges(
                        start = as.integer(seg$ref_start[[1]]),
                        end = as.integer(seg$ref_end[[1]])
                    )
                    uncovered_ir <- suppressWarnings(IRanges::setdiff(seg_ir, covered))
                    if (length(uncovered_ir) == 0L) {
                        data.frame(start = numeric(0), end = numeric(0), stringsAsFactors = FALSE)
                    } else {
                        data.frame(
                            start = IRanges::start(uncovered_ir),
                            end = IRanges::end(uncovered_ir),
                            stringsAsFactors = FALSE
                        )
                    }
                } else {
                    subtract_interval_set(
                        start_bp = as.integer(seg$ref_start[[1]]),
                        end_bp = as.integer(seg$ref_end[[1]]),
                        covered_ranges = covered
                    )
                }
                if (!is.data.frame(uncovered) || nrow(uncovered) == 0L) {
                    next
                }
                for (jj in seq_len(nrow(uncovered))) {
                    piece <- split_segment_piece(seg, uncovered$start[[jj]], uncovered$end[[jj]])
                    piece$is_pruned <- as.logical(
                        uncovered$start[[jj]] != as.integer(seg$ref_start[[1]]) ||
                            uncovered$end[[jj]] != as.integer(seg$ref_end[[1]])
                    )
                    out_idx <<- out_idx + 1L
                    out_rows[[out_idx]] <<- piece
                }
                if (iranges_ok) {
                    covered <- suppressWarnings(IRanges::reduce(c(
                        covered,
                        IRanges::IRanges(start = uncovered$start, end = uncovered$end)
                    )))
                } else {
                    covered <- rbind(
                        covered,
                        data.frame(
                            start = uncovered$start,
                            end = uncovered$end,
                            stringsAsFactors = FALSE
                        )
                    )
                    covered <- covered[order(covered$start, covered$end), , drop = FALSE]
                }
            }
            NULL
        })

    out_df <- do.call(rbind,  out_rows)
    if (!is.data.frame(out_df) || nrow(out_df) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    out_df[order(out_df$query_plot_id, out_df$strand, out_df$ref_start, out_df$qry_start), , drop = FALSE]
}

classify_reference_underlay_context <- function(df_segments, ref_feature_df = NULL) {
    if (!is.data.frame(df_segments) || nrow(df_segments) == 0L) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    out <- df_segments
    out$underlay_context <- "outside_gene"
    if (!is.data.frame(ref_feature_df) || nrow(ref_feature_df) == 0L) {
        return(out)
    }
    if (!requireNamespace("IRanges", quietly = TRUE)) {
        ov_len <- function(seg_s, seg_e, feat_s, feat_e) {
            max(0, min(seg_e, feat_e) - max(seg_s, feat_s) + 1)
        }
        cds_df <- ref_feature_df[ref_feature_df$feature_group == "cds", , drop = FALSE]
        utr_df <- ref_feature_df[ref_feature_df$feature_group == "utr", , drop = FALSE]
        exon_df <- ref_feature_df[ref_feature_df$feature_group == "exon", , drop = FALSE]
        for (ii in seq_len(nrow(out))) {
            seg_s <- out$ref_start[[ii]]
            seg_e <- out$ref_end[[ii]]
            cds_bp <- if (nrow(cds_df) > 0L) sum(mapply(ov_len, seg_s, seg_e, cds_df$xstart_clip, cds_df$xend_clip)) else 0
            utr_bp <- if (nrow(utr_df) > 0L) sum(mapply(ov_len, seg_s, seg_e, utr_df$xstart_clip, utr_df$xend_clip)) else 0
            exon_bp <- if (nrow(exon_df) > 0L) sum(mapply(ov_len, seg_s, seg_e, exon_df$xstart_clip, exon_df$xend_clip)) else 0
            out$underlay_context[[ii]] <- if (cds_bp > 0) {
                "cds"
            } else if (utr_bp > 0 || exon_bp > 0) {
                "noncoding_exon"
            } else {
                "intron_or_flank"
            }
        }
        return(out)
    }

    seg_ir <- IRanges::IRanges(
        start = suppressWarnings(as.integer(pmin(out$ref_start, out$ref_end))),
        end = suppressWarnings(as.integer(pmax(out$ref_start, out$ref_end)))
    )
    cds_df <- ref_feature_df[ref_feature_df$feature_group == "cds", , drop = FALSE]
    utr_exon_df <- ref_feature_df[ref_feature_df$feature_group %in% c("utr", "exon"), , drop = FALSE]
    has_cds <- rep(FALSE, nrow(out))
    has_noncoding_exon <- rep(FALSE, nrow(out))

    if (nrow(cds_df) > 0L) {
        cds_hits <- IRanges::findOverlaps(
            seg_ir,
            IRanges::IRanges(start = cds_df$xstart_clip, end = cds_df$xend_clip)
        )
        has_cds[unique(S4Vectors::queryHits(cds_hits))] <- TRUE
    }
    if (nrow(utr_exon_df) > 0L) {
        utr_hits <- IRanges::findOverlaps(
            seg_ir,
            IRanges::IRanges(start = utr_exon_df$xstart_clip, end = utr_exon_df$xend_clip)
        )
        has_noncoding_exon[unique(S4Vectors::queryHits(utr_hits))] <- TRUE
    }
    out$underlay_context[has_cds] <- "cds"
    out$underlay_context[!has_cds & has_noncoding_exon] <- "noncoding_exon"
    out$underlay_context[!has_cds & !has_noncoding_exon] <- "intron_or_flank"
    out
}

run_local_locus_alignment_multipip_lav <- function(reference_ctx,
                                                   query_ctx,
                                                   engine = "lastz",
                                                   out_path = "",
                                                   format_name = "lav",
                                                   extra_args = character(0)) {
    lastz_perf <- app_perf_new_run("LASTZ_MULTIPIP")
    app_perf_mark(lastz_perf, "start", "LASTZ")
    engine_txt <- tolower(trimws(as.character(engine %||% "lastz")))
    if (!engine_txt %in% c("lastz")) {
        return(list(
            status = "unsupported_engine",
            engine = engine_txt,
            segments = data.frame(stringsAsFactors = FALSE)
        ))
    }
    job <- build_lastz_multipip_job_spec(reference_ctx, query_ctx, out_path = out_path, format_name = format_name, extra_args = extra_args)
    if (is.null(job)) {
        return(list(
            status = "invalid_input",
            engine = engine_txt,
            segments = data.frame(stringsAsFactors = FALSE)
        ))
    }
    bin_info <- resolve_lastz_binary(job$binary %||% "lastz")
    bin_path <- trimws(as.character(bin_info$path %||% ""))
    if (!isTRUE(bin_info$available) || !nzchar(bin_path)) {
        return(list(
            status = "engine_unavailable",
            engine = engine_txt,
            binary = as.character(bin_info$requested %||% job$binary %||% "lastz"),
            binary_path = "",
            binary_source = as.character(bin_info$source %||% "missing"),
            setup_hint = "Install LASTZ locally or set APP_LASTZ_BIN to the binary path.",
            job = job,
            segments = data.frame(stringsAsFactors = FALSE)
        ))
    }
    ref_seq <- extract_locus_window_sequence(reference_ctx)
    qry_seq <- extract_locus_window_sequence(query_ctx)
    if (!nzchar(ref_seq) || !nzchar(qry_seq)) {
        return(list(
            status = "missing_sequence",
            engine = engine_txt,
            binary = bin_path,
            binary_path = bin_path,
            binary_source = as.character(bin_info$source %||% ""),
            job = job,
            segments = data.frame(stringsAsFactors = FALSE)
        ))
    }
    max_sequence_bp <- lastz_max_sequence_bp()
    if (nchar(ref_seq) > max_sequence_bp || nchar(qry_seq) > max_sequence_bp) {
        return(list(
            status = "window_too_large",
            engine = engine_txt,
            binary = bin_path,
            binary_path = bin_path,
            binary_source = as.character(bin_info$source %||% ""),
            job = job,
            reference_width = nchar(ref_seq),
            query_width = nchar(qry_seq),
            setup_hint = sprintf(
                "Reduce the locus span below %s bp before running MultiPIP.",
                format(max_sequence_bp, big.mark = ",")
            ),
            segments = data.frame(stringsAsFactors = FALSE)
        ))
    }
    run_dir <- tempfile(pattern = "ctv_lastz_multipip_")
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    ref_name <- paste0("ref_", sanitize_cache_key((reference_ctx %||% list())$plot_id %||% "reference"))
    qry_name <- paste0("qry_", sanitize_cache_key((query_ctx %||% list())$plot_id %||% "query"))
    ref_fa <- write_locus_temp_fasta(ref_seq, record_name = ref_name, dir_path = run_dir)
    qry_fa <- write_locus_temp_fasta(qry_seq, record_name = qry_name, dir_path = run_dir)
    if (!nzchar(ref_fa) || !nzchar(qry_fa)) {
        unlink(run_dir, recursive = TRUE, force = TRUE)
        return(list(
            status = "temp_fasta_failed",
            engine = engine_txt,
            binary = bin_path,
            binary_path = bin_path,
            binary_source = as.character(bin_info$source %||% ""),
            job = job,
            segments = data.frame(stringsAsFactors = FALSE)
        ))
    }
    out_file <- if (nzchar(trimws(as.character(out_path %||% "")))) {
        as.character(out_path)
    } else {
        file.path(run_dir, "lastz_multipip.lav")
    }
    args <- c(
        ref_fa,
        qry_fa,
        "--format=lav",
        "--ambiguous=iupac",
        paste0("--output=", out_file)
    )
    if (length(extra_args) > 0L) {
        args <- c(args, as.character(extra_args))
    }
    run_res <- tryCatch(
        run_lastz_process(bin_path, args),
        error = function(e) list(
            status = 1L,
            stdout = character(0),
            stderr = as.character(e$message %||% "unknown error"),
            timeout = grepl("timed?\\s*out|timeout", conditionMessage(e), ignore.case = TRUE)
        )
    )
    app_perf_mark(
        lastz_perf,
        sprintf("system_done ref_bp=%d query_bp=%d", as.integer(nchar(ref_seq)), as.integer(nchar(qry_seq))),
        "LASTZ"
    )
    exit_status <- suppressWarnings(as.integer(run_res$status[1L] %||% 1L))
    if (length(exit_status) == 0L || is.na(exit_status)) exit_status <- 1L
    raw_lines <- character(0)
    if (file.exists(out_file)) {
        raw_lines <- tryCatch(readLines(out_file, warn = FALSE), error = function(e) character(0))
    }
    parsed_segments <- parse_lastz_lav_output(raw_lines, reference_ctx = reference_ctx, query_ctx = query_ctx)
    parsed_segments <- extract_gap_free_segments_from_alignment(parsed_segments)
    parsed_segments <- compute_gap_free_segment_identity(parsed_segments)
    unlink(run_dir, recursive = TRUE, force = TRUE)
    if (length(exit_status) == 0L || !is.finite(exit_status)) {
        exit_status <- 1L
    }
    timed_out <- isTRUE(run_res$timeout)
    status_txt <- if (timed_out) {
        "engine_timeout"
    } else if (exit_status == 0L) {
        "ok"
    } else {
        "engine_error"
    }
    app_perf_mark(
        lastz_perf,
        sprintf("finish status=%s exit=%d segments=%d", status_txt, as.integer(exit_status), as.integer(nrow(parsed_segments %||% data.frame()))),
        "LASTZ"
    )
    list(
        status = status_txt,
        engine = engine_txt,
        binary = bin_path,
        binary_path = bin_path,
        binary_source = as.character(bin_info$source %||% ""),
        job = job,
        reference_width = nchar(ref_seq),
        query_width = nchar(qry_seq),
        exit_status = exit_status,
        timed_out = timed_out,
        stderr = as.character(run_res$stderr %||% character(0)),
        stdout = as.character(run_res$stdout %||% character(0)),
        segments = parsed_segments
    )
}

run_local_locus_alignment_multipip <- function(reference_ctx,
                                               query_ctx,
                                               engine = "lastz",
                                               out_path = "",
                                               format_name = "general-cigarx",
                                               extra_args = character(0)) {
    result <- run_local_locus_alignment(
        reference_ctx = reference_ctx,
        query_ctx = query_ctx,
        engine = engine,
        out_path = out_path,
        format_name = "general-cigarx",
        extra_args = extra_args
    )
    if (!is.list(result)) {
        return(list(
            status = "engine_error",
            engine = tolower(trimws(as.character(engine %||% "lastz"))),
            blocks = data.frame(stringsAsFactors = FALSE),
            segments = data.frame(stringsAsFactors = FALSE),
            canonical_format = "general-cigarx"
        ))
    }
    if (!is.data.frame(result$blocks)) result$blocks <- data.frame(stringsAsFactors = FALSE)
    if (!is.data.frame(result$segments)) result$segments <- data.frame(stringsAsFactors = FALSE)
    result$canonical_format <- "general-cigarx"
    result
}

run_cached_canonical_lastz <- function(cache_key,
                                       reference_ctx,
                                       query_ctx,
                                       engine = "lastz",
                                       extra_args = character(0),
                                       base_dir = ".") {
    compute_alignment <- function() {
        run_local_locus_alignment(
            reference_ctx = reference_ctx,
            query_ctx = query_ctx,
            engine = engine,
            format_name = "general-cigarx",
            extra_args = extra_args
        )
    }
    if (!isTRUE(lastz_disk_cache_eligible(reference_ctx, query_ctx, base_dir = base_dir))) {
        return(compute_alignment())
    }

    cached <- lastz_disk_cache_get(
        cache_key,
        reference_ctx = reference_ctx,
        query_ctx = query_ctx,
        base_dir = base_dir
    )
    if (!is.null(cached)) return(cached)

    cache_path <- lastz_disk_cache_path(cache_key, base_dir = base_dir, create = TRUE)
    if (!nzchar(cache_path)) return(compute_alignment())
    lock_dir <- paste0(cache_path, ".lock")
    wait_default <- as.integer(ceiling(lastz_process_timeout_ms() / 1000)) + 15L
    wait_seconds <- app_env_int("APP_LASTZ_DISK_CACHE_LOCK_WAIT_SECONDS", wait_default, min_value = 5L, max_value = 3660L)
    stale_seconds <- max(300, wait_seconds + 60L)
    deadline <- as.numeric(Sys.time()) + wait_seconds
    owns_lock <- FALSE

    repeat {
        owns_lock <- isTRUE(dir.create(lock_dir, recursive = FALSE, showWarnings = FALSE))
        if (owns_lock) break
        cached <- lastz_disk_cache_get(
            cache_key,
            reference_ctx = reference_ctx,
            query_ctx = query_ctx,
            base_dir = base_dir
        )
        if (!is.null(cached)) return(cached)
        lock_info <- tryCatch(file.info(lock_dir), error = function(e) NULL)
        lock_age <- if (!is.null(lock_info) && nrow(lock_info) > 0L) {
            as.numeric(Sys.time()) - as.numeric(lock_info$mtime[[1L]])
        } else {
            NA_real_
        }
        if (is.finite(lock_age) && lock_age > stale_seconds) {
            unlink(lock_dir, recursive = TRUE, force = TRUE)
            next
        }
        if (as.numeric(Sys.time()) >= deadline) {
            return(compute_alignment())
        }
        Sys.sleep(0.1)
    }
    on.exit(if (isTRUE(owns_lock) && dir.exists(lock_dir)) unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)

    cached <- lastz_disk_cache_get(
        cache_key,
        reference_ctx = reference_ctx,
        query_ctx = query_ctx,
        base_dir = base_dir
    )
    if (!is.null(cached)) return(cached)
    result <- compute_alignment()
    lastz_disk_cache_set(
        cache_key,
        result,
        reference_ctx = reference_ctx,
        query_ctx = query_ctx,
        base_dir = base_dir
    )
    result
}

# --- 4. FASTA Y SECUENCIAS ---

get_fasta_header_map <- function(fasta_path) {
    key <- normalizePath(fasta_path, winslash = "/", mustWork = FALSE)
    if (exists(key, envir = .fasta_header_cache, inherits = FALSE)) {
        return(get(key, envir = .fasta_header_cache, inherits = FALSE))
    }

    con <- if (grepl("\\.gz$", fasta_path, ignore.case = TRUE)) gzfile(fasta_path, open = "rt") else file(fasta_path, open = "r")
    on.exit(close(con), add = TRUE)
    seqname_to_header <- list()
    chrom_to_seqname <- list()
    header_count <- 0
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        if (!startsWith(line, ">")) next
        header <- sub("^>", "", line)
        token <- strsplit(header, "\\s+")[[1]][1]
        seqname_to_header[[token]] <- header
        if (header_count < 3) {
            header_count <- header_count + 1
        }
        chr_m <- stringr::str_match(header, stringr::regex("chromosome\\s+([0-9a-z]+)", ignore_case = TRUE))
        if (!all(is.na(chr_m))) {
            chrk <- tolower(chr_m[1, 2])
            if (nzchar(chrk) && is.null(chrom_to_seqname[[chrk]])) chrom_to_seqname[[chrk]] <- token
        }
    }
    out <- list(seqname_to_header = seqname_to_header, chrom_to_seqname = chrom_to_seqname)
    assign(key, out, envir = .fasta_header_cache)
    trim_cache_env(.fasta_header_cache, max_size = 200L)
    out
}

get_fasta_index_seqnames <- function(fasta_path) {
    key <- normalizePath(fasta_path, winslash = "/", mustWork = FALSE)
    if (exists(key, envir = .fasta_seqnames_cache, inherits = FALSE)) {
        return(get(key, envir = .fasta_seqnames_cache, inherits = FALSE))
    }

    fai_path <- paste0(fasta_path, ".fai")
    seq_names <- character(0)
    if (file.exists(fai_path)) {
        seq_names <- tryCatch(
            {
                idx <- utils::read.delim(
                    fai_path,
                    header = FALSE,
                    sep = "\t",
                    quote = "",
                    comment.char = "",
                    stringsAsFactors = FALSE
                )
                if (ncol(idx) >= 1) as.character(idx[[1]]) else character(0)
            },
            error = function(e) character(0)
        )
    }
    seq_names <- unique(seq_names[!is.na(seq_names) & nzchar(trimws(seq_names))])
    assign(key, seq_names, envir = .fasta_seqnames_cache)
    trim_cache_env(.fasta_seqnames_cache, max_size = 200L)
    seq_names
}

get_cached_fafile <- function(fasta_path) {
    if (!requireNamespace("Rsamtools", quietly = TRUE)) {
        return(NULL)
    }
    key <- normalizePath(fasta_path, winslash = "/", mustWork = FALSE)
    if (exists(key, envir = .fafile_handle_cache, inherits = FALSE)) {
        fa_cached <- get(key, envir = .fafile_handle_cache, inherits = FALSE)
        return(fa_cached)
    }
    fai_path <- paste0(fasta_path, ".fai")
    if (!file.exists(fai_path) && file.access(dirname(fasta_path), 2) == 0) {
        try(Rsamtools::indexFa(fasta_path), silent = TRUE)
    }
    fa <- tryCatch(Rsamtools::FaFile(fasta_path), error = function(e) NULL)
    if (is.null(fa)) {
        return(NULL)
    }
    assign(key, fa, envir = .fafile_handle_cache)
    trim_cache_env(.fafile_handle_cache, max_size = 40L)
    fa
}

resolve_seqname_in_fasta <- function(fasta_path, seqid, seq_names = NULL) {
    seqid <- as.character(seqid %||% "")
    if (!nzchar(seqid)) {
        return(NULL)
    }
    if (!is.null(seq_names)) {
        return(resolve_seqname_in_vector(seqid, seq_names = seq_names))
    }
    cache_key <- paste(
        normalizePath(fasta_path, winslash = "/", mustWork = FALSE),
        seqid,
        sep = "::"
    )
    if (exists(cache_key, envir = .fasta_resolved_seqname_cache, inherits = FALSE)) {
        cached <- as.character(get(cache_key, envir = .fasta_resolved_seqname_cache, inherits = FALSE) %||% "")
        if (!nzchar(cached)) {
            return(NULL)
        }
        return(cached)
    }

    seq_names_fast <- get_fasta_index_seqnames(fasta_path)
    if (length(seq_names_fast) > 0) {
        hit_fast <- resolve_seqname_in_vector(seqid, seq_names = seq_names_fast)
        if (!is.null(hit_fast)) {
            assign(cache_key, hit_fast, envir = .fasta_resolved_seqname_cache)
            trim_cache_env(.fasta_resolved_seqname_cache, max_size = 5000L)
            return(hit_fast)
        }
    }

    hmap <- get_fasta_header_map(fasta_path)
    known_seqnames <- names(hmap$seqname_to_header)
    hit <- resolve_seqname_in_vector(seqid, seq_names = known_seqnames)
    if (!is.null(hit)) {
        assign(cache_key, hit, envir = .fasta_resolved_seqname_cache)
        trim_cache_env(.fasta_resolved_seqname_cache, max_size = 5000L)
        return(hit)
    }
    s_num <- tolower(gsub("^chr", "", seqid, ignore.case = TRUE))
    if (s_num %in% names(hmap$chrom_to_seqname)) {
        out <- hmap$chrom_to_seqname[[s_num]]
        assign(cache_key, out, envir = .fasta_resolved_seqname_cache)
        trim_cache_env(.fasta_resolved_seqname_cache, max_size = 5000L)
        return(out)
    }
    assign(cache_key, "", envir = .fasta_resolved_seqname_cache)
    trim_cache_env(.fasta_resolved_seqname_cache, max_size = 5000L)
    NULL
}

get_fasta_fallback_seq_cache_key <- function(fasta_path, resolved_seqname) {
    paste(
        normalizePath(as.character(fasta_path %||% ""), winslash = "/", mustWork = FALSE),
        as.character(resolved_seqname %||% ""),
        sep = "::"
    )
}

get_seq_extract_cache_key <- function(fasta_path, seqid, start_pos, end_pos) {
    paste(
        normalizePath(as.character(fasta_path %||% ""), winslash = "/", mustWork = FALSE),
        as.character(seqid %||% ""),
        as.integer(start_pos %||% NA_integer_),
        as.integer(end_pos %||% NA_integer_),
        sep = "::"
    )
}

cache_sequence_extract_result <- function(fasta_path, seqid, start_pos, end_pos, seq_txt, resolved_seqname = NULL) {
    seq_val <- as.character(seq_txt %||% "")
    raw_key <- get_seq_extract_cache_key(fasta_path, seqid, start_pos, end_pos)
    cache_env_set(
        .seq_extract_cache,
        raw_key,
        seq_val,
        max_size = annotation_memory_cache_limits$seq_extract_max_entries,
        max_bytes = annotation_memory_cache_limits$seq_extract_max_bytes
    )
    resolved_txt <- trimws(as.character(resolved_seqname %||% ""))
    raw_seqid <- trimws(as.character(seqid %||% ""))
    if (nzchar(resolved_txt) && !identical(resolved_txt, raw_seqid)) {
        resolved_key <- get_seq_extract_cache_key(fasta_path, resolved_txt, start_pos, end_pos)
        cache_env_set(
            .seq_extract_cache,
            resolved_key,
            seq_val,
            max_size = annotation_memory_cache_limits$seq_extract_max_entries,
            max_bytes = annotation_memory_cache_limits$seq_extract_max_bytes
        )
    }
    invisible(seq_val)
}

twobit_seqnames_cache_root <- function(base_dir = ".") {
    root <- file.path(get_cgv_cache_root(base_dir = base_dir), "genome_seqnames")
    if (!dir.exists(root)) {
        dir.create(root, recursive = TRUE, showWarnings = FALSE)
    }
    normalizePath(root, winslash = "/", mustWork = FALSE)
}

twobit_seqnames_cache_key <- function(two_bit_path, base_dir = ".") {
    p <- normalizePath(as.character(two_bit_path %||% ""), winslash = "/", mustWork = FALSE)
    info <- if (nzchar(p) && file.exists(p)) file.info(p) else NULL
    identity <- canonical_cache_identity_path(p, base_dir = base_dir)
    if (requireNamespace("digest", quietly = TRUE)) {
        return(digest::digest(
            list("twobit-seqnames", .twobit_seqnames_sidecar_version, identity),
            algo = "xxhash64"
        ))
    }
    sanitize_cache_key(paste(
        "twobit-seqnames",
        .twobit_seqnames_sidecar_version,
        identity,
        sep = "||"
    ))
}

twobit_seqnames_sidecar_path <- function(two_bit_path, base_dir = ".") {
    file.path(
        twobit_seqnames_cache_root(base_dir = base_dir),
        paste0(twobit_seqnames_cache_key(two_bit_path, base_dir = base_dir), ".seqnames.rds")
    )
}

twobit_file_fingerprint <- function(two_bit_path) {
    p <- normalizePath(as.character(two_bit_path %||% ""), winslash = "/", mustWork = FALSE)
    if (!nzchar(p) || !file.exists(p)) {
        return(NULL)
    }
    info <- file.info(p)
    list(
        path = p,
        size = suppressWarnings(as.numeric(info$size[1] %||% NA_real_)),
        mtime = suppressWarnings(as.numeric(info$mtime[1] %||% NA_real_))
    )
}

read_twobit_seqnames_sidecar <- function(two_bit_path, base_dir = ".") {
    fp <- twobit_file_fingerprint(two_bit_path)
    if (is.null(fp) || !is.finite(fp$size) || !is.finite(fp$mtime)) {
        return(NULL)
    }

    candidates <- unique(c(
        paste0(fp$path, ".seqnames.rds"),
        twobit_seqnames_sidecar_path(fp$path, base_dir = base_dir)
    ))
    candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
    if (length(candidates) == 0L) {
        return(NULL)
    }

    for (path in candidates) {
        obj <- tryCatch(readRDS(path), error = function(e) NULL)
        valid <- is.list(obj) &&
            identical(as.integer(obj$schema_version %||% NA_integer_), .twobit_seqnames_sidecar_version) &&
            is.character(obj$seqnames) &&
            length(obj$seqnames) > 0L &&
            is.finite(as.numeric(obj$source_size %||% NA_real_)) &&
            is.finite(as.numeric(obj$source_mtime %||% NA_real_)) &&
            identical(as.numeric(obj$source_size), as.numeric(fp$size)) &&
            abs(as.numeric(obj$source_mtime) - as.numeric(fp$mtime)) < 1e-6
        if (isTRUE(valid)) {
            return(as.character(obj$seqnames))
        }
    }
    NULL
}

write_twobit_seqnames_sidecar <- function(two_bit_path, seqnames, base_dir = ".") {
    fp <- twobit_file_fingerprint(two_bit_path)
    vals <- as.character(seqnames %||% character(0))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (is.null(fp) || length(vals) == 0L) {
        return(invisible(FALSE))
    }
    obj <- list(
        schema_version = .twobit_seqnames_sidecar_version,
        source_path = canonical_cache_identity_path(fp$path, base_dir = base_dir),
        source_size = as.numeric(fp$size),
        source_mtime = as.numeric(fp$mtime),
        seqnames = vals,
        created_at = as.numeric(Sys.time())
    )
    invisible(atomic_save_rds(
        obj,
        twobit_seqnames_sidecar_path(fp$path, base_dir = base_dir),
        compress = "gzip"
    ))
}

read_twobit_u32 <- function(con, n = 1L, endian = "little") {
    count <- suppressWarnings(as.integer(n %||% 1L))
    if (!is.finite(count) || is.na(count) || count <= 0L) return(numeric(0))
    values <- readBin(con, what = integer(), n = count, size = 4L, endian = endian)
    values <- as.numeric(values)
    values[values < 0] <- values[values < 0] + 2^32
    values
}

read_twobit_native_index <- function(two_bit_path) {
    path <- normalizePath(as.character(two_bit_path %||% ""), winslash = "/", mustWork = FALSE)
    if (!nzchar(path) || !file.exists(path)) return(NULL)
    file_meta <- file.info(path)
    cache_key <- paste(
        path,
        as.character(file_meta$size[1] %||% ""),
        as.character(as.numeric(file_meta$mtime[1] %||% NA_real_)),
        sep = "::"
    )
    cached <- cache_env_get(.twobit_native_index_cache, cache_key, default = NULL)
    if (!is.null(cached)) return(cached)

    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    signature_raw <- readBin(con, what = "raw", n = 4L)
    if (length(signature_raw) != 4L) return(NULL)
    signature_little <- sum(as.numeric(signature_raw) * 256^(0:3))
    signature_big <- sum(as.numeric(signature_raw) * 256^(3:0))
    canonical_signature <- 0x1A412743
    endian <- if (identical(signature_little, canonical_signature)) {
        "little"
    } else if (identical(signature_big, canonical_signature)) {
        "big"
    } else {
        return(NULL)
    }
    header <- read_twobit_u32(con, n = 3L, endian = endian)
    if (length(header) != 3L || !identical(header[[1L]], 0) || !is.finite(header[[2L]])) return(NULL)
    sequence_count <- suppressWarnings(as.integer(header[[2L]]))
    if (!is.finite(sequence_count) || is.na(sequence_count) || sequence_count < 0L) return(NULL)
    offsets <- numeric(sequence_count)
    seq_names <- character(sequence_count)
    if (sequence_count > 0L) {
        for (idx in seq_len(sequence_count)) {
            name_size <- readBin(con, what = integer(), n = 1L, size = 1L, signed = FALSE)
            if (length(name_size) != 1L || !is.finite(name_size) || name_size < 0L) return(NULL)
            name_raw <- readBin(con, what = "raw", n = as.integer(name_size))
            if (length(name_raw) != as.integer(name_size)) return(NULL)
            seq_names[[idx]] <- rawToChar(name_raw)
            offset <- read_twobit_u32(con, n = 1L, endian = endian)
            if (length(offset) != 1L || !is.finite(offset)) return(NULL)
            offsets[[idx]] <- offset
        }
    }
    names(offsets) <- seq_names
    result <- list(
        path = path,
        endian = endian,
        seqnames = seq_names,
        offsets = offsets
    )
    cache_env_set(.twobit_native_index_cache, cache_key, result, max_size = 40L)
    result
}

extract_sequence_from_2bit_native <- function(two_bit_path, seqid, start_pos, end_pos) {
    index <- tryCatch(read_twobit_native_index(two_bit_path), error = function(e) NULL)
    if (is.null(index) || length(index$offsets %||% numeric(0)) == 0L) return("")
    seq_name <- as.character(seqid %||% "")
    if (!nzchar(seq_name) || !seq_name %in% names(index$offsets)) return("")
    record_offset <- suppressWarnings(as.numeric(index$offsets[[seq_name]] %||% NA_real_))
    if (!is.finite(record_offset) || record_offset < 0) return("")

    con <- file(index$path, open = "rb")
    on.exit(close(con), add = TRUE)
    seek(con, where = record_offset, origin = "start")
    dna_size <- read_twobit_u32(con, n = 1L, endian = index$endian)
    if (length(dna_size) != 1L || !is.finite(dna_size) || dna_size <= 0) return("")
    st <- max(1, suppressWarnings(as.numeric(start_pos %||% 1)))
    en <- max(st, suppressWarnings(as.numeric(end_pos %||% st)))
    st <- min(st, dna_size)
    en <- min(en, dna_size)
    if (!is.finite(st) || !is.finite(en) || en < st) return("")
    st <- floor(st)
    en <- floor(en)

    n_count <- read_twobit_u32(con, n = 1L, endian = index$endian)
    if (length(n_count) != 1L || !is.finite(n_count)) return("")
    n_count <- suppressWarnings(as.integer(n_count))
    n_starts <- read_twobit_u32(con, n = n_count, endian = index$endian)
    n_sizes <- read_twobit_u32(con, n = n_count, endian = index$endian)
    mask_count <- read_twobit_u32(con, n = 1L, endian = index$endian)
    if (length(mask_count) != 1L || !is.finite(mask_count)) return("")
    mask_count <- suppressWarnings(as.integer(mask_count))
    mask_starts <- read_twobit_u32(con, n = mask_count, endian = index$endian)
    mask_sizes <- read_twobit_u32(con, n = mask_count, endian = index$endian)
    reserved <- read_twobit_u32(con, n = 1L, endian = index$endian)
    if (length(reserved) != 1L) return("")
    packed_offset <- seek(con, where = NA, origin = "start")

    byte_start <- floor((st - 1) / 4)
    byte_end <- floor((en - 1) / 4)
    byte_count <- as.integer(byte_end - byte_start + 1)
    seek(con, where = packed_offset + byte_start, origin = "start")
    packed <- readBin(con, what = "raw", n = byte_count)
    if (length(packed) != byte_count) return("")
    byte_values <- as.integer(packed)
    codes <- as.vector(rbind(
        bitwAnd(bitwShiftR(byte_values, 6L), 3L),
        bitwAnd(bitwShiftR(byte_values, 4L), 3L),
        bitwAnd(bitwShiftR(byte_values, 2L), 3L),
        bitwAnd(byte_values, 3L)
    ))
    bases <- c("T", "C", "A", "G")[codes + 1L]
    local_start <- as.integer((st - 1) %% 4 + 1)
    requested_length <- as.integer(en - st + 1)
    sequence_chars <- bases[seq.int(local_start, length.out = requested_length)]

    apply_blocks <- function(block_starts, block_sizes, replacement = "N", lower = FALSE) {
        if (length(block_starts) == 0L || length(sequence_chars) == 0L) return(invisible(NULL))
        request_start0 <- st - 1
        request_end0 <- en - 1
        for (block_idx in seq_along(block_starts)) {
            block_start0 <- as.numeric(block_starts[[block_idx]])
            block_end0 <- block_start0 + as.numeric(block_sizes[[block_idx]]) - 1
            overlap_start0 <- max(request_start0, block_start0)
            overlap_end0 <- min(request_end0, block_end0)
            if (!is.finite(overlap_start0) || !is.finite(overlap_end0) || overlap_end0 < overlap_start0) next
            pos <- seq.int(
                as.integer(overlap_start0 - request_start0 + 1),
                as.integer(overlap_end0 - request_start0 + 1)
            )
            if (isTRUE(lower)) {
                sequence_chars[pos] <<- tolower(sequence_chars[pos])
            } else {
                sequence_chars[pos] <<- replacement
            }
        }
        invisible(NULL)
    }
    apply_blocks(n_starts, n_sizes, replacement = "N", lower = FALSE)
    paste0(sequence_chars, collapse = "")
}

get_twobit_seqnames <- function(two_bit_path, base_dir = ".") {
    if (is.null(two_bit_path) || !nzchar(two_bit_path) || !file.exists(two_bit_path)) {
        return(character(0))
    }
    key <- normalizePath(two_bit_path, winslash = "/", mustWork = FALSE)
    if (exists(key, envir = .twobit_seqinfo_cache, inherits = FALSE)) {
        return(get(key, envir = .twobit_seqinfo_cache, inherits = FALSE))
    }

    sidecar <- read_twobit_seqnames_sidecar(key, base_dir = base_dir)
    if (!is.null(sidecar) && length(sidecar) > 0L) {
        assign(key, sidecar, envir = .twobit_seqinfo_cache)
        return(sidecar)
    }

    if (isTRUE(app_env_flag("APP_NATIVE_TWOBIT_READER", TRUE))) {
        native_index <- tryCatch(read_twobit_native_index(key), error = function(e) NULL)
        native_seqnames <- as.character((native_index %||% list())$seqnames %||% character(0))
        native_seqnames <- native_seqnames[nzchar(native_seqnames)]
        if (length(native_seqnames) > 0L) {
            assign(key, native_seqnames, envir = .twobit_seqinfo_cache)
            tryCatch(write_twobit_seqnames_sidecar(key, native_seqnames, base_dir = base_dir), error = function(e) FALSE)
            return(native_seqnames)
        }
    }

    out <- tryCatch(
        {
            if (!requireNamespace("rtracklayer", quietly = TRUE) || !requireNamespace("GenomeInfoDb", quietly = TRUE)) {
                return(character(0))
            }
            tbf <- if (exists(key, envir = .twobit_handle_cache, inherits = FALSE)) {
                get(key, envir = .twobit_handle_cache, inherits = FALSE)
            } else {
                obj <- rtracklayer::TwoBitFile(two_bit_path)
                assign(key, obj, envir = .twobit_handle_cache)
                trim_cache_env(.twobit_handle_cache, max_size = 40L)
                obj
            }
            si <- GenomeInfoDb::seqinfo(tbf)
            as.character(names(si))
        },
        error = function(e) character(0)
    )
    assign(key, out, envir = .twobit_seqinfo_cache)
    if (length(out) > 0L) {
        tryCatch(write_twobit_seqnames_sidecar(key, out, base_dir = base_dir), error = function(e) FALSE)
    }
    out
}

extract_sequence_from_2bit <- function(two_bit_path, seqid, start_pos, end_pos) {
    if (is.null(two_bit_path) || !nzchar(two_bit_path) || !file.exists(two_bit_path)) {
        return("")
    }
    start_pos <- max(1L, as.integer(start_pos %||% 1L))
    end_pos <- max(start_pos, as.integer(end_pos %||% start_pos))
    twobit_perf <- app_perf_new_run("SEQ_2BIT")
    twobit_total_t0 <- app_perf_now()
    seqnames_t0 <- app_perf_now()
    seq_names <- get_twobit_seqnames(two_bit_path)
    resolved_seqname <- resolve_seqname_in_vector(seqid, seq_names = seq_names) %||% as.character(seqid %||% "")
    app_perf_mark_ms(twobit_perf, "seqnames_resolve_ms", app_perf_elapsed_ms(seqnames_t0), "SEQ_2BIT")
    if (!nzchar(resolved_seqname)) {
        return("")
    }

    if (isTRUE(app_env_flag("APP_NATIVE_TWOBIT_READER", TRUE))) {
        native_t0 <- app_perf_now()
        native_value <- tryCatch(
            extract_sequence_from_2bit_native(
                two_bit_path,
                resolved_seqname,
                start_pos,
                end_pos
            ),
            error = function(e) ""
        )
        app_perf_mark_ms(twobit_perf, "native_twobit_ms", app_perf_elapsed_ms(native_t0), "SEQ_2BIT")
        if (nzchar(native_value)) {
            app_perf_mark(twobit_perf, sprintf("native done len=%d", as.integer(nchar(native_value))), "SEQ_2BIT")
            app_perf_mark_ms(twobit_perf, "twobit_total_ms", app_perf_elapsed_ms(twobit_total_t0), "SEQ_2BIT")
            return(native_value)
        }
        app_perf_mark(twobit_perf, "native reader fallback", "SEQ_2BIT")
    }

    res <- tryCatch(
        {
            namespace_t0 <- app_perf_now()
            if (!requireNamespace("rtracklayer", quietly = TRUE)) {
                return("")
            }
            app_perf_mark_ms(twobit_perf, "rtracklayer_namespace_ms", app_perf_elapsed_ms(namespace_t0), "SEQ_2BIT")
            handle_t0 <- app_perf_now()
            key <- normalizePath(two_bit_path, winslash = "/", mustWork = FALSE)
            tbf <- if (exists(key, envir = .twobit_handle_cache, inherits = FALSE)) {
                get(key, envir = .twobit_handle_cache, inherits = FALSE)
            } else {
                obj <- rtracklayer::TwoBitFile(two_bit_path)
                assign(key, obj, envir = .twobit_handle_cache)
                trim_cache_env(.twobit_handle_cache, max_size = 40L)
                obj
            }
            app_perf_mark_ms(twobit_perf, "twobit_handle_ms", app_perf_elapsed_ms(handle_t0), "SEQ_2BIT")
            range_t0 <- app_perf_now()
            gr <- GenomicRanges::GRanges(
                seqnames = resolved_seqname,
                ranges = IRanges::IRanges(start = start_pos, end = end_pos)
            )
            app_perf_mark_ms(twobit_perf, "range_build_ms", app_perf_elapsed_ms(range_t0), "SEQ_2BIT")
            import_t0 <- app_perf_now()
            seq_set <- rtracklayer::import(
                con = tbf,
                format = "2bit",
                which = gr
            )
            app_perf_mark_ms(twobit_perf, "twobit_import_ms", app_perf_elapsed_ms(import_t0), "SEQ_2BIT")
            stringify_t0 <- app_perf_now()
            seq_value <- if (length(seq_set) == 0) "" else as.character(seq_set[[1]])
            app_perf_mark_ms(twobit_perf, "sequence_stringify_ms", app_perf_elapsed_ms(stringify_t0), "SEQ_2BIT")
            seq_value
        },
        error = function(e) ""
    )
    app_perf_mark_ms(twobit_perf, "twobit_total_ms", app_perf_elapsed_ms(twobit_total_t0), "SEQ_2BIT")
    app_perf_mark(twobit_perf, sprintf("done len=%d", as.integer(nchar(res %||% ""))), "SEQ_2BIT")
    if (nzchar(res)) {
        return(res)
    }

    bin <- Sys.which("twoBitToFa")
    if (nzchar(bin)) {
        cli_start <- max(0L, start_pos - 1L)
        cli_end <- end_pos
        args <- c(
            paste0("-seq=", resolved_seqname),
            paste0("-start=", cli_start),
            paste0("-end=", cli_end),
            two_bit_path,
            "stdout"
        )
        cmd_out <- tryCatch(system2(bin, args = args, stdout = TRUE, stderr = FALSE), error = function(e) character(0))
        if (length(cmd_out) > 1) {
            seq_txt <- paste(cmd_out[!startsWith(cmd_out, ">")], collapse = "")
            if (nzchar(seq_txt)) {
                return(seq_txt)
            }
        }
    }
    ""
}

extract_sequence_from_fasta <- function(fasta_path, seqid, start_pos, end_pos) {
    if (is.null(fasta_path) || !file.exists(fasta_path)) {
        return("")
    }
    start_pos <- as.integer(start_pos)
    end_pos <- as.integer(end_pos)
    seq_cache_key <- get_seq_extract_cache_key(fasta_path, seqid, start_pos, end_pos)
    cached_exact <- cache_env_get(.seq_extract_cache, seq_cache_key, default = NULL)
    if (!is.null(cached_exact)) {
        return(cached_exact)
    }

    # Buscar en caché una región más amplia que cubra el rango solicitado.
    norm_path <- normalizePath(as.character(fasta_path %||% ""), winslash = "/", mustWork = FALSE)
    seqid_txt <- as.character(seqid %||% "")
    if (nzchar(norm_path) && nzchar(seqid_txt)) {
        prefix <- paste0(norm_path, "::", seqid_txt, "::")
        cache_keys <- cache_env_entry_keys(.seq_extract_cache)
        matching <- cache_keys[startsWith(cache_keys, prefix)]
        for (ck in matching) {
            parts <- strsplit(ck, "::", fixed = TRUE)[[1]]
            if (length(parts) >= 4L) {
                ck_start <- suppressWarnings(as.integer(parts[3L]))
                ck_end <- suppressWarnings(as.integer(parts[4L]))
                if (is.finite(ck_start) && is.finite(ck_end) &&
                    ck_start <= start_pos && ck_end >= end_pos) {
                    cached_seq <- cache_env_get(.seq_extract_cache, ck, default = NULL)
                    if (is.character(cached_seq) && nzchar(cached_seq) && nchar(cached_seq) >= (end_pos - ck_start + 1L)) {
                        rel_start <- start_pos - ck_start + 1L
                        rel_end <- end_pos - ck_start + 1L
                        sub_seq <- substr(cached_seq, rel_start, rel_end)
                        cache_env_set(
                            .seq_extract_cache,
                            seq_cache_key,
                            sub_seq,
                            max_size = annotation_memory_cache_limits$seq_extract_max_entries,
                            max_bytes = annotation_memory_cache_limits$seq_extract_max_bytes
                        )
                        return(sub_seq)
                    }
                }
            }
        }
    }

    if (is_twobit_file(fasta_path)) {
        seq_2bit <- extract_sequence_from_2bit(fasta_path, seqid, start_pos, end_pos)
        if (nzchar(seq_2bit)) {
            cache_sequence_extract_result(fasta_path, seqid, start_pos, end_pos, seq_2bit, resolved_seqname = NULL)
            return(seq_2bit)
        }
        cache_sequence_extract_result(fasta_path, seqid, start_pos, end_pos, "", resolved_seqname = NULL)
        return("")
    }

    resolved_seqname <- NULL
    if (requireNamespace("Rsamtools", quietly = TRUE)) {
        res_rsam <- tryCatch(
            {
                fa <- get_cached_fafile(fasta_path)
                if (is.null(fa)) {
                    return(NULL)
                }
                seqid_direct <- trimws(as.character(seqid %||% ""))
                if (nzchar(seqid_direct)) {
                    gr_direct <- GenomicRanges::GRanges(
                        seqnames = seqid_direct,
                        ranges = IRanges::IRanges(start = start_pos, end = end_pos)
                    )
                    direct_res <- tryCatch(
                        as.character(Rsamtools::scanFa(fa, param = gr_direct)[[1]]),
                        error = function(e) NULL
                    )
                    if (!is.null(direct_res) && nzchar(direct_res)) {
                        return(direct_res)
                    }
                }
                resolved_seqname <<- resolve_seqname_in_fasta(fasta_path, seqid, seq_names = NULL)
                if (is.null(resolved_seqname)) {
                    return(NULL)
                }
                gr <- GenomicRanges::GRanges(
                    seqnames = resolved_seqname,
                    ranges = IRanges::IRanges(start = start_pos, end = end_pos)
                )
                as.character(Rsamtools::scanFa(fa, param = gr)[[1]])
            },
            error = function(e) {
                app_debug_log("[Extract Seq] Rsamtools error: ", e$message)
                NULL
            }
        )
        if (!is.null(res_rsam) && nzchar(res_rsam)) {
            cache_sequence_extract_result(fasta_path, seqid, start_pos, end_pos, res_rsam, resolved_seqname = resolved_seqname)
            return(res_rsam)
        }
    }

    resolved_seqname <- resolved_seqname %||% resolve_seqname_in_fasta(fasta_path, seqid, seq_names = NULL)
    if (is.null(resolved_seqname)) {
        cache_sequence_extract_result(fasta_path, seqid, start_pos, end_pos, "", resolved_seqname = NULL)
        return("")
    }
    resolved_cache_key <- get_seq_extract_cache_key(fasta_path, resolved_seqname, start_pos, end_pos)
    if (!identical(resolved_cache_key, seq_cache_key)) {
        cached_resolved <- cache_env_get(.seq_extract_cache, resolved_cache_key, default = NULL)
        if (!is.null(cached_resolved)) {
            cache_env_set(
                .seq_extract_cache,
                seq_cache_key,
                cached_resolved,
                max_size = annotation_memory_cache_limits$seq_extract_max_entries,
                max_bytes = annotation_memory_cache_limits$seq_extract_max_bytes
            )
            return(cached_resolved)
        }
    }
    fallback_seq_key <- get_fasta_fallback_seq_cache_key(fasta_path, resolved_seqname)
    fallback_full_seq <- cache_env_get(.fasta_fallback_seq_cache, fallback_seq_key, default = NULL)
    if (is.character(fallback_full_seq) && length(fallback_full_seq) > 0L && nzchar(fallback_full_seq[1])) {
        cached_full_seq <- as.character(fallback_full_seq[1] %||% "")
        if (nchar(cached_full_seq) >= end_pos) {
            out_cached <- substr(cached_full_seq, start_pos, end_pos)
            cache_sequence_extract_result(fasta_path, seqid, start_pos, end_pos, out_cached, resolved_seqname = resolved_seqname)
            return(out_cached)
        }
    }
    con <- if (grepl("\\.gz$", fasta_path, ignore.case = TRUE)) gzfile(fasta_path, open = "rt") else file(fasta_path, open = "r")
    on.exit(close(con), add = TRUE)
    in_target <- FALSE
    seq_chunks <- character()
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        if (startsWith(line, ">")) {
            header <- sub("^>", "", line)
            token <- strsplit(header, "\\s+")[[1]][1]
            in_target <- identical(token, resolved_seqname)
            next
        }
        if (in_target) seq_chunks <- c(seq_chunks, gsub("\\s+", "", line))
    }
    if (length(seq_chunks) == 0) {
        cache_sequence_extract_result(fasta_path, seqid, start_pos, end_pos, "", resolved_seqname = resolved_seqname)
        return("")
    }
    full_seq <- paste(seq_chunks, collapse = "")
    if (nchar(full_seq) < end_pos) {
        cache_sequence_extract_result(fasta_path, seqid, start_pos, end_pos, "", resolved_seqname = resolved_seqname)
        return("")
    }
    if (nchar(full_seq) <= annotation_memory_cache_limits$fasta_fallback_seq_max_bp) {
        cache_env_set(
            .fasta_fallback_seq_cache,
            fallback_seq_key,
            full_seq,
            max_size = annotation_memory_cache_limits$fasta_fallback_seq_max_entries,
            max_bytes = annotation_memory_cache_limits$fasta_fallback_seq_max_bytes
        )
    }
    out <- substr(full_seq, start_pos, end_pos)
    cache_sequence_extract_result(fasta_path, seqid, start_pos, end_pos, out, resolved_seqname = resolved_seqname)
    out
}

reverse_complement_dna <- function(seq_txt) {
    s <- toupper(gsub("\\s+", "", as.character(seq_txt %||% "")))
    if (!nzchar(s)) {
        return("")
    }
    chars <- strsplit(s, "", fixed = TRUE)[[1]]
    comp_map <- c(
        A = "T", T = "A", C = "G", G = "C", N = "N",
        R = "Y", Y = "R", S = "S", W = "W", K = "M", M = "K",
        B = "V", V = "B", D = "H", H = "D"
    )
    comp <- unname(comp_map[chars])
    comp[is.na(comp)] <- "N"
    paste(rev(comp), collapse = "")
}

normalize_exon_ranges <- function(exon_ranges) {
    if (is.null(exon_ranges)) {
        return(data.frame(start = numeric(0), end = numeric(0)))
    }
    ex <- as.data.frame(exon_ranges, stringsAsFactors = FALSE)
    if (nrow(ex) == 0) {
        return(data.frame(start = numeric(0), end = numeric(0)))
    }

    pick_col <- function(nms, candidates) {
        hit <- intersect(tolower(as.character(nms)), tolower(candidates))
        if (length(hit) == 0) {
            return(NA_character_)
        }
        nms[match(hit[1], tolower(nms))]
    }
    c_start <- pick_col(colnames(ex), c("start", "xstart", "v4"))
    c_end <- pick_col(colnames(ex), c("end", "xend", "v5"))
    if (is.na(c_start) || is.na(c_end)) {
        return(data.frame(start = numeric(0), end = numeric(0)))
    }

    out <- data.frame(
        start = suppressWarnings(as.numeric(ex[[c_start]])),
        end = suppressWarnings(as.numeric(ex[[c_end]])),
        stringsAsFactors = FALSE
    )
    out <- out[is.finite(out$start) & is.finite(out$end), , drop = FALSE]
    out <- out[out$end >= out$start, , drop = FALSE]
    if (nrow(out) == 0) {
        return(data.frame(start = numeric(0), end = numeric(0)))
    }

    ord <- order(out$start, out$end)
    out <- out[ord, , drop = FALSE]

    merged <- data.frame(start = out$start[1], end = out$end[1], stringsAsFactors = FALSE)
    if (nrow(out) > 1) {
        for (i in 2:nrow(out)) {
            if (out$start[i] <= merged$end[nrow(merged)] + 1) {
                merged$end[nrow(merged)] <- max(merged$end[nrow(merged)], out$end[i])
            } else {
                merged <- rbind(merged, data.frame(start = out$start[i], end = out$end[i], stringsAsFactors = FALSE))
            }
        }
    }
    merged
}

extract_spliced_exon_sequence <- function(fasta_path, seqid, exon_ranges, strand = "+") {
    ex <- normalize_exon_ranges(exon_ranges)
    if (nrow(ex) == 0) {
        return("")
    }

    span_start <- suppressWarnings(as.integer(round(min(ex$start, na.rm = TRUE))))
    span_end <- suppressWarnings(as.integer(round(max(ex$end, na.rm = TRUE))))
    span_width <- suppressWarnings(as.integer(span_end - span_start + 1L))
    splice_key <- paste(
        normalizePath(as.character(fasta_path %||% ""), winslash = "/", mustWork = FALSE),
        as.character(seqid %||% ""),
        as.character(strand %||% "+"),
        paste0(ex$start, "-", ex$end, collapse = ";"),
        sep = "::"
    )
    cached_spliced <- cache_env_get(.spliced_seq_cache, splice_key, default = NULL)
    if (!is.null(cached_spliced)) {
        return(as.character(cached_spliced %||% ""))
    }

    sp_perf <- app_perf_new_run("SEQ_SPLICE")
    spliced_total_t0 <- app_perf_now()
    on.exit(app_perf_mark_ms(sp_perf, "spliced_total_ms", app_perf_elapsed_ms(spliced_total_t0), "SEQ_SPLICE"), add = TRUE)
    app_perf_mark(
        sp_perf,
        sprintf(
            "start exons=%d span=%s twobit=%s",
            as.integer(nrow(ex)),
            ifelse(is.finite(span_width), as.character(as.integer(span_width)), "NA"),
            ifelse(isTRUE(is_twobit_file(fasta_path)), "yes", "no")
        ),
        "SEQ_SPLICE"
    )

    # Fastest path for single-exon transcripts.
    if (nrow(ex) == 1) {
        seq_one <- extract_sequence_from_fasta(fasta_path, seqid, ex$start[1], ex$end[1])
        if (!nzchar(seq_one)) {
            app_perf_mark(sp_perf, "single-exon empty", "SEQ_SPLICE")
            return("")
        }
        strand_one <- toupper(trimws(as.character(strand %||% "+")))
        if (identical(strand_one, "-")) {
            seq_one <- reverse_complement_dna(seq_one)
        }
        cache_env_set(
            .spliced_seq_cache,
            splice_key,
            seq_one,
            max_size = annotation_memory_cache_limits$spliced_seq_max_entries,
            max_bytes = annotation_memory_cache_limits$spliced_seq_max_bytes
        )
        app_perf_mark(sp_perf, sprintf("single-exon done len=%d", as.integer(nchar(seq_one))), "SEQ_SPLICE")
        return(seq_one)
    }

    # Fast path: fetch one continuous span and splice exons locally.
    # This is much faster than random-access per exon for compact transcripts.
    if (is.finite(span_start) && is.finite(span_end) && is.finite(span_width) &&
        span_start >= 1L && span_end >= span_start && span_width > 0L && span_width <= 300000L) {
        span_fetch_t0 <- app_perf_now()
        span_seq <- extract_sequence_from_fasta(fasta_path, seqid, span_start, span_end)
        app_perf_mark_ms(sp_perf, "single_span_fetch_ms", app_perf_elapsed_ms(span_fetch_t0), "SEQ_SPLICE")
        if (nzchar(span_seq) && nchar(span_seq) >= span_width) {
            local_splice_t0 <- app_perf_now()
            rel_starts <- as.integer(round(ex$start)) - span_start + 1L
            rel_ends <- as.integer(round(ex$end)) - span_start + 1L
            keep <- is.finite(rel_starts) & is.finite(rel_ends) &
                rel_starts >= 1L & rel_ends <= nchar(span_seq) & rel_ends >= rel_starts
            if (any(keep)) {
                parts_local <- vapply(which(keep), function(i) {
                    substr(span_seq, rel_starts[i], rel_ends[i])
                }, character(1))
                parts_local <- parts_local[nzchar(parts_local)]
                if (length(parts_local) > 0) {
                    seq_spliced_local <- paste0(parts_local, collapse = "")
                    strand_local <- toupper(trimws(as.character(strand %||% "+")))
                    seq_cache_key <- paste(
                        normalizePath(fasta_path, winslash = "/", mustWork = FALSE),
                        as.character(seqid %||% ""),
                        as.integer(span_start),
                        as.integer(span_end),
                        sep = "::"
                    )
                    # Reuse single-span genomic extraction in later feature-level GC computations.
                    cache_env_set(
                        .seq_extract_cache,
                        seq_cache_key,
                        span_seq,
                        max_size = annotation_memory_cache_limits$seq_extract_max_entries,
                        max_bytes = annotation_memory_cache_limits$seq_extract_max_bytes
                    )
                    if (identical(strand_local, "-")) seq_spliced_local <- reverse_complement_dna(seq_spliced_local)
                    cache_env_set(
                        .spliced_seq_cache,
                        splice_key,
                        seq_spliced_local,
                        max_size = annotation_memory_cache_limits$spliced_seq_max_entries,
                        max_bytes = annotation_memory_cache_limits$spliced_seq_max_bytes
                    )
                    app_perf_mark(
                        sp_perf,
                        sprintf("single-span done len=%d span=%d", as.integer(nchar(seq_spliced_local)), as.integer(span_width)),
                        "SEQ_SPLICE"
                    )
                    app_perf_mark_ms(sp_perf, "local_splice_ms", app_perf_elapsed_ms(local_splice_t0), "SEQ_SPLICE")
                    return(seq_spliced_local)
                }
            }
        }
    }

    parts <- character(0)

    # Native 2bit fallback for unusually wide transcripts where fetching one
    # continuous span would be wasteful. It preserves the same exon-by-exon
    # semantics without loading the large rtracklayer namespace.
    if (is_twobit_file(fasta_path) && isTRUE(app_env_flag("APP_NATIVE_TWOBIT_READER", TRUE))) {
        seq_names <- get_twobit_seqnames(fasta_path)
        resolved_seqname <- resolve_seqname_in_vector(seqid, seq_names = seq_names) %||% as.character(seqid %||% "")
        if (nzchar(resolved_seqname)) {
            parts <- tryCatch(
                vapply(seq_len(nrow(ex)), function(i) {
                    extract_sequence_from_2bit_native(
                        fasta_path,
                        resolved_seqname,
                        as.integer(round(as.numeric(ex$start[[i]]))),
                        as.integer(round(as.numeric(ex$end[[i]])))
                    )
                }, character(1)),
                error = function(e) character(0)
            )
        }
        if (length(parts) > 0L && all(nzchar(parts))) {
            app_perf_mark(sp_perf, sprintf("native 2bit parts=%d", as.integer(length(parts))), "SEQ_SPLICE")
        } else {
            parts <- character(0)
        }
    }

    # Fast path for 2bit: fetch all exon ranges in a single import call.
    if (length(parts) == 0L && is_twobit_file(fasta_path) && requireNamespace("rtracklayer", quietly = TRUE)) {
        parts <- tryCatch(
            {
                seq_names <- get_twobit_seqnames(fasta_path)
                resolved_seqname <- resolve_seqname_in_vector(seqid, seq_names = seq_names) %||% as.character(seqid %||% "")
                if (!nzchar(resolved_seqname)) {
                    return(character(0))
                }
                tbf_key <- normalizePath(fasta_path, winslash = "/", mustWork = FALSE)
                tbf <- if (exists(tbf_key, envir = .twobit_handle_cache, inherits = FALSE)) {
                    get(tbf_key, envir = .twobit_handle_cache, inherits = FALSE)
                } else {
                    obj <- rtracklayer::TwoBitFile(fasta_path)
                    assign(tbf_key, obj, envir = .twobit_handle_cache)
                    trim_cache_env(.twobit_handle_cache, max_size = 40L)
                    obj
                }
                gr <- GenomicRanges::GRanges(
                    seqnames = rep(resolved_seqname, nrow(ex)),
                    ranges = IRanges::IRanges(
                        start = as.integer(round(as.numeric(ex$start))),
                        end = as.integer(round(as.numeric(ex$end)))
                    )
                )
                seq_set <- rtracklayer::import(
                    con = tbf,
                    format = "2bit",
                    which = gr
                )
                as.character(seq_set)
            },
            error = function(e) {
                character(0)
            }
        )
        if (length(parts) > 0) {
            app_perf_mark(sp_perf, sprintf("2bit batch parts=%d", as.integer(length(parts))), "SEQ_SPLICE")
        }
    }

    # Fast path: one FASTA handle + one batched scan for all exons.
    # This preserves exon-by-exon splicing semantics but avoids repeated
    # open/index/close overhead per exon.
    if (length(parts) == 0 && !is_twobit_file(fasta_path) && requireNamespace("Rsamtools", quietly = TRUE)) {
        parts <- tryCatch(
            {
                fa <- get_cached_fafile(fasta_path)
                if (is.null(fa)) {
                    return(character(0))
                }
                seqid_direct <- trimws(as.character(seqid %||% ""))
                if (nzchar(seqid_direct)) {
                    gr_direct <- GenomicRanges::GRanges(
                        seqnames = rep(seqid_direct, nrow(ex)),
                        ranges = IRanges::IRanges(
                            start = as.integer(round(as.numeric(ex$start))),
                            end = as.integer(round(as.numeric(ex$end)))
                        )
                    )
                    direct_parts <- tryCatch(
                        as.character(Rsamtools::scanFa(fa, param = gr_direct)),
                        error = function(e) character(0)
                    )
                    if (length(direct_parts) > 0) {
                        app_perf_mark(sp_perf, sprintf("fa direct parts=%d", as.integer(length(direct_parts))), "SEQ_SPLICE")
                        return(direct_parts)
                    }
                }
                resolved_seqname <- resolve_seqname_in_fasta(fasta_path, seqid, seq_names = NULL)
                if (is.null(resolved_seqname)) {
                    return(character(0))
                }

                gr <- GenomicRanges::GRanges(
                    seqnames = rep(resolved_seqname, nrow(ex)),
                    ranges = IRanges::IRanges(
                        start = as.integer(round(as.numeric(ex$start))),
                        end = as.integer(round(as.numeric(ex$end)))
                    )
                )
                out_parts <- as.character(Rsamtools::scanFa(fa, param = gr))
                app_perf_mark(sp_perf, sprintf("fa resolved parts=%d", as.integer(length(out_parts))), "SEQ_SPLICE")
                out_parts
            },
            error = function(e) {
                character(0)
            }
        )
    }

    if (length(parts) == 0) {
        parts <- vapply(seq_len(nrow(ex)), function(i) {
            extract_sequence_from_fasta(fasta_path, seqid, ex$start[i], ex$end[i])
        }, character(1))
        app_perf_mark(sp_perf, sprintf("per-exon fallback parts=%d", as.integer(length(parts))), "SEQ_SPLICE")
    }
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) {
        app_perf_mark(sp_perf, "done empty", "SEQ_SPLICE")
        return("")
    }

    seq_spliced <- paste0(parts, collapse = "")
    strand <- toupper(trimws(as.character(strand %||% "+")))
    if (identical(strand, "-")) seq_spliced <- reverse_complement_dna(seq_spliced)
    cache_env_set(
        .spliced_seq_cache,
        splice_key,
        seq_spliced,
        max_size = annotation_memory_cache_limits$spliced_seq_max_entries,
        max_bytes = annotation_memory_cache_limits$spliced_seq_max_bytes
    )
    app_perf_mark(sp_perf, sprintf("done len=%d", as.integer(nchar(seq_spliced))), "SEQ_SPLICE")
    seq_spliced
}

get_transcript_composition_cache_key <- function(genome_path, seqid, exon_ranges, strand = "+") {
    gp <- normalizePath(as.character(genome_path %||% ""), winslash = "/", mustWork = FALSE)
    fi_sig <- ""
    if (nzchar(gp) && file.exists(gp)) {
        fi <- file.info(gp)
        fi_sig <- paste(as.character(fi$size[1] %||% ""), as.character(as.numeric(fi$mtime[1] %||% NA_real_)), sep = ":")
    }
    ex <- normalize_exon_ranges(exon_ranges)
    exon_sig <- if (nrow(ex) > 0L) {
        paste(paste0(as.integer(round(ex$start)), "-", as.integer(round(ex$end))), collapse = ";")
    } else {
        ""
    }
    paste(gp, fi_sig, as.character(seqid %||% ""), toupper(trimws(as.character(strand %||% "+"))), exon_sig, sep = "||")
}

get_transcript_composition_cached <- function(genome_path, seqid, exon_ranges, strand = "+", spliced_sequence = NULL) {
    key <- get_transcript_composition_cache_key(genome_path, seqid, exon_ranges, strand = strand)
    cached <- cache_env_get(.transcript_composition_cache, key, default = NULL)
    if (!is.null(cached) && is.list(cached) && nzchar(as.character(cached$composition %||% ""))) {
        return(cached)
    }

    # fetch_gene_data_sync has already extracted this exact transcript. Other
    # callers continue to resolve it here. An empty supplied transcript must not
    # be replaced by the genomic fallback sequence (its semantics differ).
    seq_txt <- spliced_sequence
    if (is.null(seq_txt)) seq_txt <- extract_spliced_exon_sequence(genome_path, seqid, exon_ranges, strand = strand)
    seq_clean <- toupper(gsub("\\s+", "", as.character(seq_txt %||% "")))
    counts <- count_sequence_bases(seq_clean)
    known_total <- sum(counts, na.rm = TRUE)
    out <- list(
        composition = format_sequence_composition_from_counts(counts, denominator = known_total),
        length = nchar(seq_clean),
        known_total = as.integer(known_total),
        counts = counts,
        sequence_optional = NULL
    )
    cache_env_set(.transcript_composition_cache, key, out, max_size = 2000L, max_bytes = 16 * 1024^2)
    out
}

wrap_fasta_sequence <- function(seq_txt, width = 80L) {
    s <- toupper(gsub("[^ACGTN]", "", as.character(seq_txt %||% "")))
    if (!nzchar(s)) {
        return("")
    }
    w <- max(1L, as.integer(width %||% 80L))
    n <- nchar(s)
    starts <- seq.int(1L, n, by = w)
    ends <- pmin(starts + w - 1L, n)
    paste(vapply(seq_along(starts), function(i) {
        substr(s, starts[i], ends[i])
    }, character(1)), collapse = "\n")
}

normalize_sequence_download_type <- function(sequence_type, default = "transcript") {
    typ <- as.character(sequence_type %||% default)
    if (length(typ) == 0L || is.na(typ[1]) || !nzchar(trimws(typ[1]))) {
        typ <- as.character(default %||% "transcript")
    }
    typ <- tolower(trimws(typ[1]))
    typ <- gsub("[^a-z_]+", "_", typ)
    typ <- switch(
        typ,
        "gene" = "gene",
        "gen" = "gene",
        "full_gene" = "gene",
        "transcript" = "transcript",
        "transcrito" = "transcript",
        "mrna" = "transcript",
        "cds" = "cds",
        "coding_sequence" = "cds",
        "cds_segment" = "cds_segments",
        "cds_segments" = "cds_segments",
        "coding_segments" = "cds_segments",
        "intron" = "introns",
        "introns" = "introns",
        "intrones" = "introns",
        default
    )
    if (!typ %in% c("gene", "transcript", "cds", "cds_segments", "introns")) {
        typ <- default
    }
    typ
}

sequence_download_extract_title_field <- function(title_txt, field_name) {
    txt <- as.character(title_txt %||% "")
    if (length(txt) == 0L || !nzchar(txt)) {
        return("")
    }
    field <- as.character(field_name %||% "")
    if (length(field) == 0L || !nzchar(field[1])) {
        return("")
    }
    field <- gsub("([.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1", field[1], perl = TRUE)
    m <- stringr::str_match(txt, sprintf("\\b%s\\s*:\\s*([^|]+)", field))
    out <- trimws(as.character(m[1, 2] %||% ""))
    if (is.na(out)) "" else safe_url_decode(out)
}

sequence_download_clean_id <- function(x, fallback = "sequence") {
    out <- trimws(safe_url_decode(as.character(x %||% "")))
    if (length(out) == 0L) {
        out <- ""
    }
    out <- out[1]
    out <- stringr::str_remove(out, stringr::regex("^(transcript|gene)\\s*:\\s*", ignore_case = TRUE))
    out <- trimws(out)
    if (!nzchar(out) || is.na(out)) {
        out <- as.character(fallback %||% "sequence")
    }
    out
}

sequence_download_scope_to_transcript <- function(plot_data, title_txt = "") {
    if (is.null(plot_data) || nrow(plot_data) == 0L ||
        !exists("split_gene_data_by_transcript", mode = "function")) {
        return(plot_data)
    }
    target <- sequence_download_clean_id(
        sequence_download_extract_title_field(title_txt, "Transcript"),
        fallback = ""
    )
    if (!nzchar(target) || identical(toupper(target), "N/A")) {
        return(plot_data)
    }
    transcript_blocks <- tryCatch(split_gene_data_by_transcript(plot_data), error = function(e) list())
    if (length(transcript_blocks) == 0L || is.null(names(transcript_blocks))) {
        return(plot_data)
    }
    block_ids <- vapply(
        names(transcript_blocks),
        sequence_download_clean_id,
        character(1),
        fallback = ""
    )
    match_idx <- which(tolower(block_ids) == tolower(target))[1]
    if (length(match_idx) == 0L || is.na(match_idx)) {
        return(plot_data)
    }
    transcript_blocks[[match_idx]]
}

sequence_download_tx_types <- function() {
    c(
        "mrna", "transcript", "lnc_rna", "trna", "rrna", "snorna", "snrna", "mirna",
        "ncrna", "primary_transcript", "pre_mirna", "guide_rna", "rnase_p_rna",
        "rnase_mrp_rna", "telomerase_rna", "antisense_rna", "srp_rna", "scarna",
        "vault_rna", "y_rna", "antisense_lncrna", "lncrna"
    )
}

sequence_download_model <- function(plot_data) {
    if (is.null(plot_data) || nrow(plot_data) == 0L) {
        empty <- data.frame()
        return(list(df = empty, df_gene = empty, df_transcript = empty, raw = empty))
    }
    raw <- as.data.frame(plot_data, stringsAsFactors = FALSE)
    if (exists("process_gene_data", mode = "function")) {
        processed <- tryCatch(process_gene_data(raw), error = function(e) NULL)
        if (!is.null(processed) && !is.null(processed$df)) {
            processed$raw <- raw
            return(processed)
        }
    }
    row_type <- tolower(trimws(as.character(raw$V3 %||% rep("", nrow(raw)))))
    df_gene <- raw[row_type == "gene", , drop = FALSE]
    df_transcript <- raw[row_type %in% sequence_download_tx_types(), , drop = FALSE]
    df <- data.frame(
        xstart = suppressWarnings(as.numeric(raw$V4)),
        xend = suppressWarnings(as.numeric(raw$V5)),
        feature_type = row_type,
        seqid = as.character(raw$V1 %||% ""),
        strand = as.character(raw$V7 %||% ""),
        attributes_raw = as.character(raw$V9 %||% ""),
        stringsAsFactors = FALSE
    )
    list(df = df, df_gene = df_gene, df_transcript = df_transcript, raw = raw)
}

sequence_download_span <- function(df, preferred_types = character(0), fallback_types = character(0)) {
    if (is.null(df) || nrow(df) == 0L) {
        return(list(start = NA_real_, end = NA_real_, seqid = "", strand = ""))
    }
    if (all(c("V3", "V4", "V5") %in% names(df)) && exists("compute_feature_block_span", mode = "function")) {
        return(compute_feature_block_span(df, preferred_types = preferred_types, fallback_types = fallback_types))
    }
    row_type <- tolower(trimws(as.character(df$feature_type %||% rep("", nrow(df)))))
    pick <- row_type %in% tolower(preferred_types)
    if (!any(pick)) pick <- row_type %in% tolower(fallback_types)
    if (!any(pick)) pick <- rep(TRUE, nrow(df))
    span_df <- df[pick, , drop = FALSE]
    starts <- suppressWarnings(as.numeric(span_df$xstart %||% span_df$V4))
    ends <- suppressWarnings(as.numeric(span_df$xend %||% span_df$V5))
    start_val <- suppressWarnings(min(starts, na.rm = TRUE))
    end_val <- suppressWarnings(max(ends, na.rm = TRUE))
    if (!is.finite(start_val) || !is.finite(end_val) || end_val < start_val) {
        start_val <- NA_real_
        end_val <- NA_real_
    }
    list(
        start = start_val,
        end = end_val,
        seqid = as.character((span_df$seqid %||% span_df$V1 %||% "")[1] %||% ""),
        strand = as.character((span_df$strand %||% span_df$V7 %||% "")[1] %||% "")
    )
}

sequence_download_ranges <- function(df_plot, feature_type) {
    if (is.null(df_plot) || nrow(df_plot) == 0L) {
        return(data.frame(start = numeric(0), end = numeric(0)))
    }
    ft <- tolower(trimws(as.character(df_plot$feature_type %||% rep("", nrow(df_plot)))))
    idx <- which(ft == tolower(feature_type))
    if (length(idx) == 0L) {
        return(data.frame(start = numeric(0), end = numeric(0)))
    }
    normalize_exon_ranges(data.frame(
        start = suppressWarnings(as.numeric(df_plot$xstart[idx])),
        end = suppressWarnings(as.numeric(df_plot$xend[idx])),
        stringsAsFactors = FALSE
    ))
}

sequence_download_feature_segments <- function(df_plot, feature_type) {
    empty <- data.frame(
        start = numeric(0),
        end = numeric(0),
        phase = character(0),
        stringsAsFactors = FALSE
    )
    if (is.null(df_plot) || nrow(df_plot) == 0L) {
        return(empty)
    }
    ft <- tolower(trimws(as.character(df_plot$feature_type %||% rep("", nrow(df_plot)))))
    idx <- which(ft == tolower(feature_type))
    if (length(idx) == 0L) {
        return(empty)
    }
    phase_values <- if ("phase" %in% names(df_plot)) {
        as.character(df_plot$phase[idx])
    } else {
        rep("", length(idx))
    }
    out <- data.frame(
        start = suppressWarnings(as.numeric(df_plot$xstart[idx])),
        end = suppressWarnings(as.numeric(df_plot$xend[idx])),
        phase = phase_values,
        stringsAsFactors = FALSE
    )
    out <- out[is.finite(out$start) & is.finite(out$end) & out$end >= out$start, , drop = FALSE]
    if (nrow(out) == 0L) return(empty)
    duplicate_key <- paste(out$start, out$end, sep = "|")
    out <- out[!duplicated(duplicate_key), , drop = FALSE]
    out <- out[order(out$start, out$end), , drop = FALSE]
    rownames(out) <- NULL
    out
}

sequence_download_intron_ranges <- function(exon_ranges) {
    ex <- normalize_exon_ranges(exon_ranges)
    if (nrow(ex) < 2L) {
        return(data.frame(start = numeric(0), end = numeric(0)))
    }
    starts <- as.integer(round(ex$end[-nrow(ex)] + 1L))
    ends <- as.integer(round(ex$start[-1] - 1L))
    out <- data.frame(start = starts, end = ends, stringsAsFactors = FALSE)
    out <- out[is.finite(out$start) & is.finite(out$end) & out$end >= out$start, , drop = FALSE]
    normalize_exon_ranges(out)
}

build_segmented_sequence_fasta <- function(header_id,
                                           segment_type,
                                           ranges,
                                           genome_path,
                                           seqid,
                                           strand = "+") {
    seg <- as.data.frame(ranges, stringsAsFactors = FALSE)
    if (nrow(seg) == 0L) return("")
    strand_txt <- trimws(as.character(strand %||% "+"))
    if (!strand_txt %in% c("+", "-")) strand_txt <- "+"
    ord <- if (identical(strand_txt, "-")) {
        order(-as.numeric(seg$start), -as.numeric(seg$end))
    } else {
        order(as.numeric(seg$start), as.numeric(seg$end))
    }
    seg <- seg[ord, , drop = FALSE]
    rownames(seg) <- NULL
    n_segments <- nrow(seg)
    gp <- trimws(as.character(genome_path %||% ""))
    seqid_txt <- trimws(as.character(seqid %||% ""))
    type_txt <- if (identical(segment_type, "cds")) "cds_segment" else "intron"

    records <- vapply(seq_len(n_segments), function(i) {
        start_i <- as.integer(round(as.numeric(seg$start[i])))
        end_i <- as.integer(round(as.numeric(seg$end[i])))
        seq_i <- ""
        if (nzchar(gp) && file.exists(gp) && nzchar(seqid_txt)) {
            seq_i <- tryCatch(
                extract_sequence_from_fasta(gp, seqid_txt, start_i, end_i),
                error = function(e) ""
            )
            if (nzchar(seq_i) && identical(strand_txt, "-")) {
                seq_i <- tryCatch(reverse_complement_dna(seq_i), error = function(e) seq_i)
            }
        }
        meta_bits <- c(
            paste0("type=", type_txt),
            paste0("segment=", i, "/", n_segments)
        )
        if (nzchar(seqid_txt)) meta_bits <- c(meta_bits, paste0("chr=", seqid_txt))
        meta_bits <- c(
            meta_bits,
            paste0("start=", start_i),
            paste0("end=", end_i),
            paste0("strand=", strand_txt),
            paste0("length_bp=", end_i - start_i + 1L),
            "order=5prime_to_3prime"
        )
        phase_i <- trimws(as.character(seg$phase[i] %||% ""))
        if (identical(segment_type, "cds") && phase_i %in% c("0", "1", "2")) {
            meta_bits <- c(meta_bits, paste0("phase=", phase_i))
        }
        record_header <- paste0(
            ">", header_id, "_", segment_type, "_", i,
            " | ", paste(meta_bits, collapse = " | ")
        )
        seq_wrapped <- wrap_fasta_sequence(seq_i, width = 80L)
        if (!nzchar(seq_wrapped)) record_header else paste0(record_header, "\n", seq_wrapped)
    }, character(1))
    paste(records, collapse = "\n\n")
}

build_selected_sequence_fasta_content <- function(sequence_type = "transcript",
                                                  plot_data,
                                                  gene_meta = NULL,
                                                  title_txt = "",
                                                  genome_path = NULL,
                                                  fallback_id = "sequence") {
    typ <- normalize_sequence_download_type(sequence_type)
    gp <- trimws(as.character(genome_path %||% ""))
    if (!identical(typ, "gene")) {
        plot_data <- sequence_download_scope_to_transcript(plot_data, title_txt = title_txt)
    }
    model <- sequence_download_model(plot_data)
    df_plot <- model$df
    raw <- model$raw

    tx_types <- sequence_download_tx_types()
    feature_types <- c("exon", "cds", "start_codon", "stop_codon")
    gene_span <- sequence_download_span(raw, preferred_types = "gene", fallback_types = c(tx_types, feature_types))
    tx_span <- sequence_download_span(raw, preferred_types = tx_types, fallback_types = c(feature_types, "gene"))

    meta <- gene_meta %||% list()
    seqid_txt <- trimws(as.character(meta$seqid %||% gene_span$seqid %||% tx_span$seqid %||% ""))
    strand_txt <- trimws(as.character(tx_span$strand %||% meta$strand %||% gene_span$strand %||% "+"))
    if (!strand_txt %in% c("+", "-")) {
        strand_txt <- "+"
    }

    gene_label <- sequence_download_clean_id(
        meta$display_gene_name %||% meta$matched_gene_name %||% sequence_download_extract_title_field(title_txt, "Gene"),
        fallback = fallback_id
    )
    tx_label <- sequence_download_clean_id(
        sequence_download_extract_title_field(title_txt, "Transcript"),
        fallback = gene_label
    )
    if (identical(tx_label, gene_label) && !is.null(plot_data)) {
        labels <- tryCatch(extract_plot_labels(plot_data), error = function(e) NULL)
        tx_label <- sequence_download_clean_id(labels$transcript %||% tx_label, fallback = tx_label)
    }
    header_id <- if (identical(typ, "gene")) gene_label else tx_label
    if (!nzchar(header_id)) {
        header_id <- as.character(fallback_id %||% "sequence")
    }

    seq_txt <- ""
    range_start <- NA_real_
    range_end <- NA_real_
    segments_n <- 0L

    if (identical(typ, "gene")) {
        g_start <- suppressWarnings(as.numeric(meta$gene_start_bp %||% gene_span$start))
        g_end <- suppressWarnings(as.numeric(meta$gene_end_bp %||% gene_span$end))
        if (!is.finite(g_start) || !is.finite(g_end)) {
            g_start <- suppressWarnings(as.numeric(tx_span$start))
            g_end <- suppressWarnings(as.numeric(tx_span$end))
        }
        range_start <- g_start
        range_end <- g_end
        segments_n <- if (is.finite(g_start) && is.finite(g_end)) 1L else 0L
        if (nzchar(gp) && file.exists(gp) && nzchar(seqid_txt) &&
            is.finite(g_start) && is.finite(g_end) && g_end >= g_start) {
            seq_txt <- tryCatch(
                extract_sequence_from_fasta(gp, seqid_txt, as.integer(round(g_start)), as.integer(round(g_end))),
                error = function(e) ""
            )
            if (nzchar(seq_txt) && identical(strand_txt, "-")) {
                seq_txt <- tryCatch(reverse_complement_dna(seq_txt), error = function(e) seq_txt)
            }
        }
    } else {
        exon_ranges <- sequence_download_ranges(df_plot, "exon")
        cds_ranges <- sequence_download_ranges(df_plot, "cds")
        cds_segment_ranges <- sequence_download_feature_segments(df_plot, "cds")
        ranges <- switch(
            typ,
            "transcript" = if (nrow(exon_ranges) > 0L) exon_ranges else cds_ranges,
            "cds" = cds_ranges,
            "cds_segments" = cds_segment_ranges,
            "introns" = sequence_download_intron_ranges(exon_ranges),
            data.frame(start = numeric(0), end = numeric(0))
        )
        segments_n <- nrow(ranges)
        if (segments_n > 0L) {
            range_start <- min(ranges$start, na.rm = TRUE)
            range_end <- max(ranges$end, na.rm = TRUE)
        } else if (identical(typ, "transcript")) {
            range_start <- suppressWarnings(as.numeric(tx_span$start))
            range_end <- suppressWarnings(as.numeric(tx_span$end))
        }
        if (typ %in% c("cds_segments", "introns") && segments_n > 0L) {
            segment_type <- if (identical(typ, "cds_segments")) "cds" else "intron"
            return(build_segmented_sequence_fasta(
                header_id = header_id,
                segment_type = segment_type,
                ranges = ranges,
                genome_path = gp,
                seqid = seqid_txt,
                strand = strand_txt
            ))
        }
        if (nzchar(gp) && file.exists(gp) && nzchar(seqid_txt)) {
            if (segments_n > 0L) {
                seq_txt <- tryCatch(
                    extract_spliced_exon_sequence(gp, seqid_txt, exon_ranges = ranges, strand = strand_txt),
                    error = function(e) ""
                )
            } else if (identical(typ, "transcript") &&
                is.finite(range_start) && is.finite(range_end) && range_end >= range_start) {
                seq_txt <- tryCatch(
                    extract_sequence_from_fasta(gp, seqid_txt, as.integer(round(range_start)), as.integer(round(range_end))),
                    error = function(e) ""
                )
                if (nzchar(seq_txt) && identical(strand_txt, "-")) {
                    seq_txt <- tryCatch(reverse_complement_dna(seq_txt), error = function(e) seq_txt)
                }
            }
        }
    }

    meta_bits <- c(paste0("type=", typ))
    if (nzchar(seqid_txt)) meta_bits <- c(meta_bits, paste0("chr=", seqid_txt))
    if (is.finite(range_start)) meta_bits <- c(meta_bits, paste0("start=", as.integer(round(range_start))))
    if (is.finite(range_end)) meta_bits <- c(meta_bits, paste0("end=", as.integer(round(range_end))))
    meta_bits <- c(meta_bits, paste0("strand=", strand_txt))
    if (typ %in% c("introns", "cds_segments") || segments_n > 1L) {
        meta_bits <- c(meta_bits, paste0("segments=", as.integer(segments_n)))
    }

    seq_wrapped <- wrap_fasta_sequence(seq_txt, width = 80L)
    header <- paste0(">", header_id, " | ", paste(meta_bits, collapse = " | "))
    if (!nzchar(seq_wrapped)) {
        return(paste0(header, "\n"))
    }
    paste0(header, "\n", seq_wrapped)
}

extract_plot_labels <- function(data_df) {
    if (is.null(data_df) || nrow(data_df) == 0) {
        return(list(transcript = "N/A", chromosome = "N/A"))
    }
    cached_meta <- attr(data_df, "cgev_transcript_meta", exact = TRUE)
    if (is.list(cached_meta)) {
        cached_tx <- trimws(as.character(cached_meta$transcript %||% ""))
        cached_chr <- trimws(as.character(cached_meta$chromosome %||% ""))
        if (nzchar(cached_tx) && nzchar(cached_chr)) {
            return(list(transcript = cached_tx, chromosome = cached_chr))
        }
    }
    chromosome <- as.character(data_df$V1[1] %||% "N/A")
    tx_level_types <- c(
        "mrna", "transcript", "lnc_rna", "trna", "rrna", "snorna", "snrna", "mirna",
        "ncrna", "primary_transcript", "pre_mirna", "guide_rna", "rnase_p_rna",
        "rnase_mrp_rna", "telomerase_rna", "antisense_rna", "srp_rna", "scarna",
        "vault_rna", "y_rna", "antisense_lncrna", "lncrna"
    )
    tx_rows <- data_df %>% filter(tolower(V3) %in% tx_level_types)
    tx <- NA_character_
    if (nrow(tx_rows) > 0) {
        attr <- safe_url_decode(tx_rows$V9[1])
        tx <- str_extract(attr, "(^|;)ID=[^;]+") %>% str_remove("(^|;)ID=")
        if (is.na(tx)) tx <- str_extract(attr, "(^|;)transcript_id=[^;]+") %>% str_remove("(^|;)transcript_id=")
        if (is.na(tx)) {
            tx <- str_extract(attr, 'transcript_id "[^"]+"') %>%
                str_remove('transcript_id "') %>%
                str_remove('"')
        }
    }
    tx <- as.character(tx)
    if (is.na(tx) || !nzchar(tx)) tx <- ""
    tx <- trimws(safe_url_decode(tx))
    tx <- str_remove(tx, regex("^(transcript|gene)\\s*:\\s*", ignore_case = TRUE))
    tx <- trimws(tx)
    if (is.na(tx) || !nzchar(tx)) {
        fallback_rows <- data_df %>%
            filter(tolower(V3) %in% c("gene", "cds")) %>%
            head(1)
        if (nrow(fallback_rows) > 0) {
            attr <- as.character(fallback_rows$V9[1] %||% "")
            tx_candidates <- c(
                extract_primary_gene_name(attr),
                extract_primary_gene_id(attr)
            )
            tx_candidates <- tx_candidates[!is.na(tx_candidates) & nzchar(trimws(tx_candidates))]
            tx <- if (length(tx_candidates) > 0) tx_candidates[1] else NA_character_
            tx <- as.character(tx)
            if (is.na(tx) || !nzchar(tx)) tx <- ""
            tx <- trimws(safe_url_decode(tx))
            tx <- str_remove(tx, regex("^(transcript|gene)\\s*:\\s*", ignore_case = TRUE))
            tx <- trimws(tx)
        }
    }
    list(transcript = if (is.na(tx) || !nzchar(tx)) "N/A" else tx, chromosome = chromosome)
}

split_gene_data_by_transcript <- function(data_df) {
    if (is.null(data_df) || nrow(data_df) == 0) {
        return(list())
    }

    split_perf <- app_perf_new_run("TRANSCRIPT_SPLIT")
    split_total_t0 <- app_perf_now()
    on.exit(app_perf_mark_ms(split_perf, "split_total_internal_ms", app_perf_elapsed_ms(split_total_t0), "TRANSCRIPT_SPLIT"), add = TRUE)
    setup_t0 <- app_perf_now()
    df <- as.data.frame(data_df, stringsAsFactors = FALSE)
    if (!all(c("V3", "V9") %in% colnames(df))) {
        return(list(df))
    }

    canonical_link_tokens <- function(x) {
        x <- as.character(x %||% "")
        x <- trimws(safe_url_decode(x))
        x <- gsub('["\\\']', "", x)
        x <- trimws(stringr::str_remove(
            x,
            stringr::regex("^(transcript|gene)\\s*:\\s*", ignore_case = TRUE)
        ))
        x
    }

    n <- nrow(df)
    row_types <- tolower(trimws(as.character(df$V3 %||% rep("", n))))
    gene_rows <- which(row_types == "gene")
    # Recognise all RNA-level feature types used as transcript-level entries
    tx_level_types <- c(
        "mrna", "transcript", "lnc_rna", "trna", "rrna", "snorna", "snrna", "mirna",
        "ncrna", "primary_transcript", "pre_mirna", "guide_rna", "rnase_p_rna",
        "rnase_mrp_rna", "telomerase_rna", "antisense_rna", "srp_rna", "scarna",
        "vault_rna", "y_rna", "antisense_lncrna", "lncrna"
    )
    tx_rows <- which(row_types %in% tx_level_types)
    app_perf_mark_ms(split_perf, "split_setup_ms", app_perf_elapsed_ms(setup_t0), "TRANSCRIPT_SPLIT")
    if (length(tx_rows) == 0) {
        return(list(df))
    }

    attrs_raw <- as.character(df$V9 %||% rep("", n))

    # This splitter needs only ID/transcript_id and Parent. Parsing every GFF/GTF
    # attribute into a list used to dominate large genes even after relationship
    # traversal was indexed. Extract the required fields in vectorised passes.
    id_extract_t0 <- app_perf_now()
    ids <- stringr::str_match(
        attrs_raw,
        stringr::regex("(?:^|;)\\s*ID=([^;]+)", ignore_case = TRUE)
    )[, 2]
    missing_ids <- is.na(ids) | !nzchar(trimws(ids))
    if (any(missing_ids)) {
        transcript_ids <- stringr::str_match(
            attrs_raw[missing_ids],
            stringr::regex('(?:^|;|\\t)\\s*transcript_id\\s*[= ]\\s*"?([^;"\\t]+)', ignore_case = TRUE)
        )[, 2]
        ids[missing_ids] <- transcript_ids
    }
    ids[is.na(ids)] <- ""
    nonempty_ids <- nzchar(ids)
    if (any(nonempty_ids)) ids[nonempty_ids] <- safe_url_decode(trimws(ids[nonempty_ids]))
    app_perf_mark_ms(split_perf, "split_id_extract_ms", app_perf_elapsed_ms(id_extract_t0), "TRANSCRIPT_SPLIT")

    parent_index_t0 <- app_perf_now()
    parent_fields <- stringr::str_extract_all(
        attrs_raw,
        stringr::regex("(?:^|;)\\s*Parent=[^;]*", ignore_case = TRUE)
    )
    # Flatten Parent fields once and build the complete relationship index with
    # vector operations.  Calling URL decoding and regex normalization once per
    # annotation row was disproportionately expensive on large human genes.
    parent_field_n <- lengths(parent_fields)
    parent_field_rows <- rep.int(seq_len(n), parent_field_n)
    parent_field_values <- unlist(parent_fields, use.names = FALSE)
    if (length(parent_field_values) > 0L) {
        parent_field_values <- stringr::str_remove(
            parent_field_values,
            stringr::regex("^(?:;)?\\s*Parent=", ignore_case = TRUE)
        )
        parent_parts <- strsplit(parent_field_values, ",", fixed = TRUE)
        parent_part_rows <- rep.int(parent_field_rows, lengths(parent_parts))
        parent_tokens_flat <- canonical_link_tokens(unlist(parent_parts, use.names = FALSE))
        parent_token_valid <- nzchar(parent_tokens_flat)
        rows_by_parent_token <- split(
            as.integer(parent_part_rows[parent_token_valid]),
            parent_tokens_flat[parent_token_valid]
        )
        rows_by_parent_token <- lapply(rows_by_parent_token, unique)
    } else {
        rows_by_parent_token <- list()
    }
    app_perf_mark_ms(split_perf, "split_parent_index_ms", app_perf_elapsed_ms(parent_index_t0), "TRANSCRIPT_SPLIT")

    tx_key_t0 <- app_perf_now()
    tx_ids_raw <- vapply(tx_rows, function(i) {
        tid <- ids[i]
        if (!nzchar(tid)) {
            raw <- safe_url_decode(attrs_raw[i] %||% "")
            tid <- stringr::str_extract(raw, "(^|;)ID=[^;]+") |> stringr::str_remove("(^|;)ID=")
        }
        as.character(tid %||% "")
    }, character(1))
    tx_ids_display <- vapply(tx_ids_raw, function(tid) {
        t <- trimws(safe_url_decode(tid %||% ""))
        t <- stringr::str_remove(t, stringr::regex("^(transcript|gene)\\s*:\\s*", ignore_case = TRUE))
        trimws(t)
    }, character(1))

    valid_tx <- nzchar(tx_ids_raw) | nzchar(tx_ids_display)
    if (!any(valid_tx)) {
        return(list(df))
    }

    tx_rows_valid <- tx_rows[valid_tx]
    tx_raw_valid <- tx_ids_raw[valid_tx]
    tx_disp_valid <- tx_ids_display[valid_tx]
    tx_base <- stringr::str_remove(tolower(tx_disp_valid), "(?:[-_.]|\\b)\\d+$")
    tx_num <- suppressWarnings(as.numeric(stringr::str_match(tx_disp_valid, "(?:[-_.]|\\b)(\\d+)$")[, 2]))
    tx_num_ord <- ifelse(is.na(tx_num), Inf, tx_num)
    ord <- order(tx_base, tx_num_ord, tolower(tx_disp_valid), tx_rows_valid)
    app_perf_mark_ms(split_perf, "split_tx_key_order_ms", app_perf_elapsed_ms(tx_key_t0), "TRANSCRIPT_SPLIT")

    traversal_t0 <- app_perf_now()
    out <- list()
    for (k in ord) {
        tx_row <- tx_rows_valid[k]
        tx_id_raw <- tx_raw_valid[k]
        tx_id_display <- tx_disp_valid[k]
        selected <- rep(FALSE, n)
        if (length(gene_rows) > 0) selected[gene_rows] <- TRUE
        selected[tx_row] <- TRUE

        frontier <- unique(canonical_link_tokens(tx_id_raw))
        frontier <- frontier[nzchar(frontier)]
        if (length(frontier) == 0) {
            frontier <- unique(canonical_link_tokens(tx_id_display))
            frontier <- frontier[nzchar(frontier)]
        }
        visited_tokens <- character(0)
        repeat {
            frontier <- setdiff(frontier, visited_tokens)
            if (length(frontier) == 0L) break
            visited_tokens <- unique(c(visited_tokens, frontier))
            hit_lists <- unname(rows_by_parent_token[frontier])
            hit_lists[vapply(hit_lists, is.null, logical(1))] <- list(integer(0))
            hits <- unique(as.integer(unlist(hit_lists, use.names = FALSE)))
            hits <- hits[is.finite(hits) & hits >= 1L & hits <= n & !selected[hits]]
            if (length(hits) == 0L) break
            selected[hits] <- TRUE
            new_ids <- unique(ids[hits])
            new_ids <- new_ids[nzchar(new_ids)]
            frontier <- unique(canonical_link_tokens(new_ids))
            frontier <- frontier[nzchar(frontier)]
        }

        sub_df <- df[selected, , drop = FALSE]
        if (nrow(sub_df) == 0) next

        key <- tx_id_display
        if (!nzchar(key)) key <- tx_id_raw
        if (!nzchar(key)) key <- paste0("transcript_", tx_row)
        key_i <- key
        i <- 1
        while (key_i %in% names(out)) {
            i <- i + 1
            key_i <- paste0(key, "_", i)
        }
        sub_types <- row_types[selected]
        sub_starts <- suppressWarnings(as.numeric(sub_df$V4))
        sub_ends <- suppressWarnings(as.numeric(sub_df$V5))
        tx_local_rows <- which(sub_types %in% tx_level_types)
        span_rows <- if (length(tx_local_rows) > 0L) {
            tx_local_rows
        } else {
            feature_rows <- which(sub_types %in% c("exon", "cds", "start_codon", "stop_codon") | grepl("utr", sub_types))
            if (length(feature_rows) > 0L) feature_rows else which(sub_types == "gene")
        }
        if (length(span_rows) == 0L) span_rows <- seq_len(nrow(sub_df))
        span_start <- suppressWarnings(min(sub_starts[span_rows], na.rm = TRUE))
        span_end <- suppressWarnings(max(sub_ends[span_rows], na.rm = TRUE))
        if (!is.finite(span_start)) span_start <- NA_real_
        if (!is.finite(span_end)) span_end <- NA_real_
        make_ranges <- function(type_name) {
            idx <- which(sub_types == type_name & is.finite(sub_starts) & is.finite(sub_ends))
            if (length(idx) == 0L) {
                return(data.frame(start = numeric(0), end = numeric(0), stringsAsFactors = FALSE))
            }
            unique(data.frame(start = sub_starts[idx], end = sub_ends[idx], stringsAsFactors = FALSE))
        }
        attr(sub_df, "cgev_transcript_meta") <- list(
            transcript = unname(tx_id_display),
            chromosome = as.character(sub_df$V1[1] %||% "N/A"),
            row_types = sub_types,
            span = c(start = span_start, end = span_end),
            tx_attr = if (length(tx_local_rows) > 0L) as.character(sub_df$V9[tx_local_rows[1L]] %||% "") else "",
            gene_attr = {
                gene_local_rows <- which(sub_types == "gene")
                if (length(gene_local_rows) > 0L) as.character(sub_df$V9[gene_local_rows[1L]] %||% "") else ""
            },
            exon_ranges = make_ranges("exon"),
            cds_ranges = make_ranges("cds")
        )
        out[[key_i]] <- sub_df
    }
    app_perf_mark_ms(split_perf, "split_traversal_copy_ms", app_perf_elapsed_ms(traversal_t0), "TRANSCRIPT_SPLIT")
    app_perf_mark(split_perf, sprintf("done blocks=%d rows=%d", as.integer(length(out)), as.integer(n)), "TRANSCRIPT_SPLIT")

    if (length(out) == 0) {
        return(list(df))
    }
    out
}

prepare_orthologous_transcript_splits_once <- function(results, split_fun = split_gene_data_by_transcript, prepare_fun = NULL) {
    if (!is.function(split_fun)) {
        stop("split_fun must be a function", call. = FALSE)
    }

    prepared <- vector("list", length(results))
    if (length(results) == 0L) {
        return(prepared)
    }

    for (result_idx in seq_along(results)) {
        result <- results[[result_idx]]
        if (is.null(result) || !isTRUE(result$found)) {
            next
        }
        data <- result$data
        if (!is.data.frame(data) || nrow(data) == 0L) {
            next
        }

        split_t0 <- app_perf_now()
        split_attempt <- tryCatch(
            if (is.function(prepare_fun)) {
                value <- prepare_fun(result)
                list(reusable = TRUE, blocks = value$blocks, canonical = value$canonical)
            } else {
                list(reusable = TRUE, blocks = split_fun(data))
            },
            error = function(e) list(reusable = FALSE, blocks = list())
        )
        blocks <- split_attempt$blocks
        if (length(blocks) == 0L) {
            blocks <- list(data)
        }
        prepared[[result_idx]] <- list(
            blocks = blocks,
            reusable = isTRUE(split_attempt$reusable),
            elapsed_ms = app_perf_elapsed_ms(split_t0)
        )
        if (!is.null(split_attempt$canonical)) prepared[[result_idx]]$canonical <- split_attempt$canonical
    }

    prepared
}

# --- 5. BÚSQUEDA DE GENES (ACTUALIZADA: CONTROL PARALELO) ---

order_external_alias_sources_for_speed <- function(sources) {
    src <- unique(tolower(trimws(as.character(sources %||% character(0)))))
    preferred <- c("mygene", "uniprot", "ncbi", "ensembl")
    preferred[preferred %in% src]
}

external_alias_source_latency_group <- function(source) {
    src <- tolower(trimws(as.character(source %||% "")))
    if (src %in% c("mygene", "uniprot")) "fast" else "slow"
}

build_gene_query_candidates <- function(gene_name, organism = NULL, taxid = NULL, status_callback = NULL, max_aliases = 40, alias_lookup_timeout_sec = 0, use_parallel = TRUE, alias_sources = c("mygene", "ncbi", "uniprot", "ensembl")) {
    expand_local <- function(q) {
        out <- c(q)
        if (grepl(";", q, fixed = TRUE)) out <- c(out, gsub(";", ".", q), gsub(";", "-", q), gsub(";", "", q))
        if (grepl("\\.", q)) out <- c(out, gsub("\\.", ";", q), gsub("\\.", "-", q), gsub("\\.", "", q))
        if (grepl("-", q, fixed = TRUE)) out <- c(out, gsub("-", ";", q), gsub("-", ".", q), gsub("-", "", q))
        unique(out[nzchar(out)])
    }
    local_variants <- expand_local(gene_name)
    external_aliases <- character(0)
    emit_status <- function(msg) {
        if (is.null(status_callback)) {
            return(invisible(NULL))
        }
        try(status_callback(as.character(msg %||% "")), silent = TRUE)
        invisible(NULL)
    }
    allowed_sources <- c("mygene", "ncbi", "uniprot", "ensembl")
    source_labels <- c(mygene = "MyGene", ncbi = "NCBI", uniprot = "UniProt", ensembl = "Ensembl")
    alias_sources <- unique(tolower(trimws(as.character(alias_sources %||% allowed_sources))))
    alias_sources <- alias_sources[alias_sources %in% allowed_sources]
    external_lookup_meta <- list(
        had_errors = FALSE,
        source_errors = character(0),
        skipped = FALSE,
        source_names = alias_sources
    )
    format_source_names <- function(src) {
        if (length(src) == 0) {
            return("none")
        }
        out <- unname(source_labels[src])
        out[is.na(out) | !nzchar(out)] <- src[is.na(out) | !nzchar(out)]
        paste(out, collapse = ", ")
    }

    if (length(alias_sources) == 0) {
        emit_status("\u2022 External stage: Skipped (all external sources disabled in Settings).")
        external_lookup_meta$skipped <- TRUE
        external_lookup_meta$skip_reason <- "sources_disabled"
    } else if (!is.null(taxid) || !is.null(organism)) {
        emit_status(sprintf("\u2022 External stage: Preparing alias lookup (%s)...", format_source_names(alias_sources)))
        tryCatch(
            {
                mode_msg <- if (use_parallel) "Parallel" else "Sequential"
                app_debug_log(sprintf("[DEBUG] Searching aliases for '%s' (Mode: %s)...", gene_name, mode_msg))

                # Pasamos use_parallel y fuentes activas a la librería externa
                alias_fun <- get("get_gene_aliases", mode = "function")
                alias_args <- list(
                    gene = gene_name,
                    taxid = taxid,
                    organism = organism,
                    use_parallel = use_parallel,
                    status_callback = emit_status
                )
                if ("sources" %in% names(formals(alias_fun))) {
                    alias_args$sources <- alias_sources
                }
                external_aliases <- do.call(alias_fun, alias_args)
                external_lookup_meta <- modifyList(
                    external_lookup_meta,
                    attr(external_aliases, "lookup_meta", exact = TRUE) %||% list()
                )

                if (length(external_aliases) > 0) {
                    app_debug_log(sprintf("[DEBUG] Found aliases: %s", paste(external_aliases, collapse = ", ")))
                    emit_status(sprintf("\u2022 External stage: Found %d alias candidate(s).", length(external_aliases)))
                } else {
                    app_debug_log("[DEBUG] No external aliases found.")
                    emit_status("\u2022 External stage: No aliases found.")
                }
            },
            error = function(e) {
                app_debug_log("[DEBUG] Error in external search: ", e$message)
                emit_status("\u2022 External stage: Query failed; using local variants only.")
                external_lookup_meta$had_errors <- TRUE
                external_lookup_meta$source_errors <- unique(c(
                    as.character(external_lookup_meta$source_errors %||% character(0)),
                    trimws(as.character(e$message %||% "external lookup failed"))
                ))
            }
        )
    } else {
        emit_status("\u2022 External stage: Skipped (organism metadata unavailable).")
        external_lookup_meta$skipped <- TRUE
        external_lookup_meta$skip_reason <- "organism_metadata_unavailable"
    }

    all_candidates <- unique(c(local_variants, external_aliases))
    if (length(all_candidates) > max_aliases) {
        all_candidates <- all_candidates[seq_len(max_aliases)]
    }
    attr(all_candidates, "external_lookup_meta") <- external_lookup_meta
    return(all_candidates)
}

search_gene_in_file <- function(file_path, gene_names, show_diagnostics = TRUE, match_mode = c("flex", "exact"), return_meta = FALSE, include_bridge_tokens = FALSE, bridge_sqlite_con = NULL) {
    tryCatch(
        {
            match_mode <- match.arg(match_mode)
            use_tabix <- is_tabix_annotation_file(file_path)
            if (use_tabix) {
                idx <- build_gff_gene_light_index(file_path)
                genes_df <- idx$genes_df
                if (nrow(genes_df) == 0) {
                    if (isTRUE(return_meta)) {
                        return(list(data = NULL, matched_gene_id = NA_character_, matched_gene_name = NA_character_))
                    }
                    return(NULL)
                }
                target_row_ids <- search_gene_rows_with_index(idx, gene_names, match_mode = match_mode)
                if (length(target_row_ids) == 0 && identical(match_mode, "exact") && isTRUE(include_bridge_tokens)) {
                    if (!is.null(bridge_sqlite_con) && exists("resolve_bridge_via_sqlite", mode = "function")) {
                        target_row_ids <- resolve_bridge_via_sqlite(bridge_sqlite_con, gene_names, idx)
                    }
                    if (length(target_row_ids) == 0) {
                        target_row_ids <- search_gene_rows_via_bridge_descriptions(genes_df$attributes, gene_names)
                    }
                }
                target_gene_rows <- if (length(target_row_ids) > 0) genes_df[target_row_ids, , drop = FALSE] else genes_df[0, , drop = FALSE]
            } else {
                idx <- build_gff_gene_index(file_path)
                df <- idx$df
                if (nrow(df) == 0) {
                    if (isTRUE(return_meta)) {
                        return(list(data = NULL, matched_gene_id = NA_character_, matched_gene_name = NA_character_))
                    }
                    return(NULL)
                }
                target_row_ids <- search_gene_rows_with_index(idx, gene_names, match_mode = match_mode)
                if (length(target_row_ids) == 0 && identical(match_mode, "exact") && isTRUE(include_bridge_tokens)) {
                    if (!is.null(bridge_sqlite_con) && exists("resolve_bridge_via_sqlite", mode = "function")) {
                        target_row_ids <- resolve_bridge_via_sqlite(bridge_sqlite_con, gene_names, idx)
                    }
                    if (length(target_row_ids) == 0) {
                        attr_subset <- df$attributes[idx$gene_rows]
                        bridge_rel <- search_gene_rows_via_bridge_descriptions(attr_subset, gene_names)
                        if (length(bridge_rel) > 0) {
                            target_row_ids <- idx$gene_rows[bridge_rel]
                        }
                    }
                }
                target_gene_rows <- if (length(target_row_ids) > 0) df[target_row_ids, , drop = FALSE] else df[0, , drop = FALSE]
            }

            if (nrow(target_gene_rows) == 0) {
                if (isTRUE(return_meta)) {
                    return(list(data = NULL, matched_gene_id = NA_character_, matched_gene_name = NA_character_))
                }
                return(NULL)
            }

            first_attrs <- parse_gff_attributes(target_gene_rows$attributes[1])
            first_type <- tolower(as.character(target_gene_rows$type[1] %||% ""))
            if (first_type %in% c("gene", "pseudogene")) {
                gene_id_candidates <- c(first_attrs[["id"]][1], first_attrs[["gene_id"]][1], first_attrs[["locus_tag"]][1])
            } else {
                # For CDS-only/prokaryotic annotations, Parent often points to the gene-like anchor.
                gene_id_candidates <- c(
                    first_attrs[["parent"]][1],
                    first_attrs[["gene_id"]][1],
                    first_attrs[["locus_tag"]][1],
                    first_attrs[["id"]][1]
                )
            }
            gene_id_candidates <- as.character(gene_id_candidates %||% character(0))
            gene_id_candidates <- trimws(safe_url_decode(gene_id_candidates))
            gene_id_candidates <- trimws(vapply(strsplit(gene_id_candidates, ",", fixed = TRUE), `[`, character(1), 1))
            gene_id_candidates <- gene_id_candidates[!is.na(gene_id_candidates) & nzchar(gene_id_candidates)]
            gene_id <- if (length(gene_id_candidates) > 0) gene_id_candidates[1] else NA_character_

            matched_name_candidates <- c(
                first_attrs[["gene_name"]][1],
                first_attrs[["gene"]][1],
                first_attrs[["name"]][1],
                first_attrs[["locus_tag"]][1],
                gene_id
            )
            matched_name_candidates <- as.character(matched_name_candidates %||% character(0))
            matched_name_candidates <- trimws(safe_url_decode(matched_name_candidates))
            matched_name_candidates <- matched_name_candidates[!is.na(matched_name_candidates) & nzchar(matched_name_candidates)]
            matched_name <- if (length(matched_name_candidates) > 0) matched_name_candidates[1] else gene_id

            if (is.na(gene_id) || !nzchar(gene_id)) {
                gene_id <- stringr::str_extract(safe_url_decode(target_gene_rows$attributes[1]), 'gene_id "[^"]+"') |>
                    stringr::str_remove('gene_id "') |>
                    stringr::str_remove('"')
            }
            can_extract_block <- !is.na(gene_id) && nzchar(gene_id)
            if (!can_extract_block) {
                # Some minimalist annotations omit stable gene IDs; keep the matched row as fallback instead of returning NULL.
                gene_id <- if (!is.na(matched_name) && nzchar(matched_name)) matched_name else NA_character_
            }

            if (use_tabix) {
                target_chr <- as.character(target_gene_rows$seqid[1] %||% "")
                target_start <- as.numeric(target_gene_rows$start[1] %||% NA_real_)
                target_end <- as.numeric(target_gene_rows$end[1] %||% NA_real_)
                region_df <- if (nzchar(target_chr) && is.finite(target_start) && is.finite(target_end)) {
                    scan_tabix_region_gff(file_path, target_chr, target_start, target_end)
                } else {
                    empty_gff_df()
                }
                result_df <- if (can_extract_block) extract_gene_block_from_df(region_df, gene_id) else data.frame()
                if (nrow(result_df) == 0) {
                    fallback_gene <- target_gene_rows[, c("seqid", "source", "type", "start", "end", "score", "strand", "phase", "attributes"), drop = FALSE]
                    result_df <- fallback_gene
                    colnames(result_df) <- paste0("V", 1:9)
                }
            } else {
                result_df <- data.frame()
                if (can_extract_block) {
                    block_source_df <- df
                    if (first_type %in% c("gene", "pseudogene")) {
                        block_source_df <- subset_gff_df_to_gene_region(df, target_gene_rows[1, , drop = FALSE])
                    }
                    result_df <- extract_gene_block_from_df(block_source_df, gene_id)
                    if (nrow(result_df) == 0 && !identical(block_source_df, df)) {
                        result_df <- extract_gene_block_from_df(df, gene_id)
                    }
                }
                if (nrow(result_df) == 0) {
                    fallback_gene <- target_gene_rows[, c("seqid", "source", "type", "start", "end", "score", "strand", "phase", "attributes"), drop = FALSE]
                    result_df <- fallback_gene
                    colnames(result_df) <- paste0("V", 1:9)
                }
            }
            if (isTRUE(return_meta)) {
                return(list(data = result_df, matched_gene_id = gene_id, matched_gene_name = ifelse(is.na(matched_name), gene_id, matched_name)))
            }
            return(result_df)
        },
        error = function(e) {
            if (isTRUE(return_meta)) list(data = NULL, matched_gene_id = NA_character_, matched_gene_name = NA_character_) else NULL
        }
    )
}

orthology_identity_from_lookup_result <- function(result, base_dir = ".") {
    result <- result %||% list()
    lookup <- result$lookup %||% list()
    det <- result$det %||% lookup$det_resolved %||% list()
    match_row <- lookup$alias_index_match
    local_gene_id <- trimws(as.character(lookup$matched_gene_id %||% ""))
    local_feature_id <- ""
    local_symbol <- trimws(as.character(lookup$matched_gene_name %||% ""))
    if (is.data.frame(match_row) && nrow(match_row) > 0L) {
        local_gene_id <- trimws(as.character(match_row$local_gene_id[1] %||% local_gene_id))
        local_feature_id <- trimws(as.character(match_row$local_feature_id[1] %||% ""))
        local_symbol <- trimws(as.character(match_row$local_symbol[1] %||% local_symbol))
    }
    organism <- trimws(as.character(det$organism %||% result$file_label %||% ""))
    species <- if (exists("normalize_ensembl_species_name", mode = "function")) {
        normalize_ensembl_species_name(
            organism = organism,
            ensembl_species = det$ensembl_species %||% det$ensembl_name %||% ""
        )
    } else {
        ""
    }
    ids <- if (exists("ensembl_gene_ids_for_alias_locus", mode = "function")) {
        tryCatch(
            ensembl_gene_ids_for_alias_locus(
                local_gene_id = local_gene_id,
                local_feature_id = local_feature_id,
                local_symbol = local_symbol,
                organism_id = as.character(det$species_id %||% det$preloaded_id %||% ""),
                annotation_path = as.character(result$file_path %||% ""),
                organism_name = organism,
                taxid = as.character(det$taxid %||% ""),
                base_dir = base_dir
            ),
            error = function(e) character(0)
        )
    } else {
        character(0)
    }
    list(
        file_idx = suppressWarnings(as.integer(result$file_idx %||% NA_integer_)),
        file_label = as.character(result$file_label %||% organism),
        organism = organism,
        species = species,
        local_gene_id = local_gene_id,
        local_symbol = local_symbol,
        ensembl_gene_ids = unique(as.character(ids %||% character(0))),
        kingdom = as.character(det$kingdom %||% "")
    )
}

validate_cross_species_orthology_results <- function(results, fetch_fun = NULL, base_dir = ".") {
    found_positions <- which(vapply(results, function(result) isTRUE((result %||% list())$found), logical(1)))
    if (length(found_positions) < 2L) {
        return(list(
            status = "insufficient_matches",
            approved_positions = integer(0),
            rejected_positions = found_positions,
            identities = list(),
            evidence = list(),
            message = "Fewer than two organisms have resolvable local loci."
        ))
    }
    if (is.null(fetch_fun)) {
        if (!exists("fetch_ensembl_orthologs", mode = "function")) {
            return(list(
                status = "evidence_unavailable",
                approved_positions = integer(0),
                rejected_positions = found_positions,
                identities = list(),
                evidence = list(),
                message = "The Ensembl Compara evidence resolver is unavailable."
            ))
        }
        fetch_fun <- fetch_ensembl_orthologs
    }

    identities <- lapply(found_positions, function(pos) orthology_identity_from_lookup_result(results[[pos]], base_dir = base_dir))
    names(identities) <- as.character(found_positions)
    usable <- which(vapply(identities, function(identity) {
        nzchar(as.character(identity$species %||% "")) &&
            length(as.character(identity$ensembl_gene_ids %||% character(0))) == 1L
    }, logical(1)))
    if (length(usable) < 2L) {
        return(list(
            status = "unresolved_identifiers",
            approved_positions = integer(0),
            rejected_positions = found_positions,
            identities = identities,
            evidence = list(),
            message = "At least two loci need one unambiguous Ensembl gene identifier."
        ))
    }

    adjacency <- matrix(FALSE, nrow = length(identities), ncol = length(identities))
    diag(adjacency) <- TRUE
    evidence <- list()
    pair_index <- utils::combn(usable, 2L, simplify = FALSE)
    for (pair in pair_index) {
        left_idx <- pair[[1L]]
        right_idx <- pair[[2L]]
        left <- identities[[left_idx]]
        right <- identities[[right_idx]]
        compara <- if (exists("ensembl_compara_division", mode = "function")) {
            ensembl_compara_division(kingdom = left$kingdom, organism = left$organism)
        } else {
            ""
        }
        fetched <- tryCatch(
            fetch_fun(
                source_gene_id = left$ensembl_gene_ids[[1L]],
                source_species = left$species,
                target_species = right$species,
                compara = compara
            ),
            error = function(e) list(status = "unavailable", rows = NULL, error = conditionMessage(e))
        )
        verdict <- if (exists("evaluate_ensembl_orthology_pair", mode = "function")) {
            evaluate_ensembl_orthology_pair(
                source_gene_id = left$ensembl_gene_ids[[1L]],
                target_gene_id = right$ensembl_gene_ids[[1L]],
                target_species = right$species,
                homology_result = fetched
            )
        } else {
            list(verified = FALSE, status = "evidence_unavailable", homology_type = "")
        }
        evidence[[length(evidence) + 1L]] <- list(
            left_position = found_positions[[left_idx]],
            right_position = found_positions[[right_idx]],
            left = left,
            right = right,
            fetch_status = as.character(fetched$status %||% "unavailable"),
            verdict = verdict
        )
        if (isTRUE(verdict$verified)) {
            adjacency[left_idx, right_idx] <- TRUE
            adjacency[right_idx, left_idx] <- TRUE
        }
    }

    # Connected components represent sets joined by explicit one-to-one
    # Ensembl Compara relationships. A tie is intentionally rejected because
    # choosing one biological group would require user intent.
    remaining <- usable
    components <- list()
    while (length(remaining) > 0L) {
        component <- remaining[[1L]]
        repeat {
            expanded <- unique(c(component, which(apply(adjacency[component, , drop = FALSE], 2L, any))))
            if (setequal(expanded, component)) break
            component <- expanded
        }
        component <- intersect(component, usable)
        components[[length(components) + 1L]] <- component
        remaining <- setdiff(remaining, component)
    }
    sizes <- vapply(components, length, integer(1))
    best_size <- if (length(sizes) > 0L) max(sizes) else 0L
    best <- which(sizes == best_size & sizes >= 2L)
    if (length(best) != 1L) {
        return(list(
            status = if (best_size >= 2L) "ambiguous_orthology_groups" else "no_verified_pair",
            approved_positions = integer(0),
            rejected_positions = found_positions,
            identities = identities,
            evidence = evidence,
            message = if (best_size >= 2L) {
                "More than one equally supported orthology group was found; a reference locus must be selected."
            } else {
                "The same or similar gene name did not produce a verified one-to-one ortholog pair."
            }
        ))
    }
    approved <- found_positions[components[[best[[1L]]]]]
    list(
        status = "verified",
        approved_positions = approved,
        rejected_positions = setdiff(found_positions, approved),
        identities = identities,
        evidence = evidence,
        message = sprintf("Verified one-to-one orthology across %d organisms.", length(approved))
    )
}

select_cross_species_results_by_orthology_policy <- function(
        results,
        require_verified = cross_species_requires_verified_orthology(),
        fetch_fun = NULL,
        base_dir = ".") {
    if (isTRUE(require_verified)) {
        validated <- validate_cross_species_orthology_results(
            results,
            fetch_fun = fetch_fun,
            base_dir = base_dir
        )
        validated$verification_required <- TRUE
        return(validated)
    }

    found_positions <- which(vapply(results, function(result) {
        isTRUE((result %||% list())$found)
    }, logical(1)))
    list(
        status = if (length(found_positions) > 0L) "local_matches" else "no_local_matches",
        approved_positions = found_positions,
        rejected_positions = setdiff(seq_along(results), found_positions),
        identities = list(),
        evidence = list(),
        verification_required = FALSE,
        message = if (length(found_positions) > 0L) {
            sprintf("Accepted unambiguous local matches in %d organism(s).", length(found_positions))
        } else {
            "No unambiguous local matches were found."
        }
    )
}

apply_cross_species_reference_anchor <- function(lookup_jobs, reference_anchor) {
    jobs <- lookup_jobs %||% list()
    anchor <- reference_anchor %||% list()
    scalar_text <- function(value) {
        values <- trimws(as.character(value %||% ""))
        if (length(values) == 0L || is.na(values[[1L]])) "" else values[[1L]]
    }
    local_gene_id <- scalar_text(anchor$local_gene_id)
    if (!nzchar(local_gene_id) || length(jobs) == 0L) {
        return(list(jobs = jobs, reference_idx = integer(0), applied = FALSE))
    }

    anchor_org_id <- scalar_text(anchor$organism_id)
    reference_idx <- which(vapply(jobs, function(job) {
        det <- (job %||% list())$det %||% list()
        job_org_id <- scalar_text(det$species_id %||% det$preloaded_id)
        nzchar(anchor_org_id) && identical(job_org_id, anchor_org_id)
    }, logical(1)))
    if (length(reference_idx) == 0L) {
        anchor_org <- tolower(scalar_text(anchor$organism_name))
        reference_idx <- which(vapply(jobs, function(job) {
            det <- (job %||% list())$det %||% list()
            job_org <- tolower(scalar_text(det$organism))
            nzchar(anchor_org) && identical(job_org, anchor_org)
        }, logical(1)))
    }
    if (length(reference_idx) != 1L) {
        return(list(jobs = jobs, reference_idx = integer(0), applied = FALSE))
    }

    idx <- reference_idx[[1L]]
    jobs[[idx]]$gene_name <- local_gene_id
    jobs[[idx]]$allow_partial_suggestions <- FALSE
    list(jobs = jobs, reference_idx = as.integer(idx), applied = TRUE)
}

run_lookup_pipeline_pure <- function(file_path, input_gene, det_info = NULL, diagnostics = FALSE,
                                     use_parallel = FALSE, file_label = NULL,
                                     enabled_external_sources = c("mygene", "ncbi", "uniprot", "ensembl"),
                                     allow_partial_suggestions = TRUE) {
    lookup_t0 <- app_perf_now()
    query_candidates_used <- normalize_lookup_query_candidates(c(input_gene))
    enabled_external_sources <- unique(tolower(trimws(as.character(enabled_external_sources %||% character(0)))))
    enabled_external_sources <- enabled_external_sources[enabled_external_sources %in% c("mygene", "ncbi", "uniprot", "ensembl")]

    operation_alias_index_lookup <- NULL
    if (exists("make_operation_alias_index_lookup", mode = "function") &&
        exists("search_alias_index_for_context", mode = "function")) {
        operation_alias_index_lookup <- tryCatch(
            make_operation_alias_index_lookup(
                query = input_gene,
                file_path = file_path,
                file_label = file_label,
                base_dir = ".",
                lookup_fun = get("search_alias_index_for_context", mode = "function")
            ),
            error = function(e) NULL
        )
    }
    lookup_alias_index_for_operation <- function(det_resolved) {
        if (is.function(operation_alias_index_lookup)) {
            return(operation_alias_index_lookup(det_resolved))
        }
        list(
            result = search_alias_index_for_context(
                query = input_gene,
                file_path = file_path,
                det_info = det_resolved,
                file_label = file_label,
                base_dir = "."
            ),
            reused = FALSE
        )
    }

    alias_index_preflight <- function(det_resolved) {
        det <- det_resolved %||% list()
        org_id <- trimws(as.character(det$species_id %||% det$preloaded_id %||% ""))
        if (!nzchar(org_id) ||
            !exists("search_alias_index_for_context", mode = "function") ||
            !exists("alias_index_match_to_lookup", mode = "function")) {
            return(NULL)
        }
        alias_index_attempt <- tryCatch(
            lookup_alias_index_for_operation(det),
            error = function(e) NULL
        )
        alias_index_res <- (alias_index_attempt %||% list())$result
        alias_index_status <- as.character((alias_index_res %||% list())$status %||% "no_match")
        alias_index_matches <- (alias_index_res %||% list())$matches
        if (startsWith(alias_index_status, "multiple") &&
            is.data.frame(alias_index_matches) && nrow(alias_index_matches) > 0L) {
            nohit <- attach_lookup_result_meta(
                list(data = NULL, matched_gene_id = NA_character_, matched_gene_name = NA_character_),
                query_candidates = query_candidates_used,
                best_alias_used = "",
                input_gene = input_gene
            )
            nohit$external_lookup_had_errors <- FALSE
            nohit$det_resolved <- det
            nohit$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
            nohit$lookup_stage <- "alias_index_ambiguous"
            nohit$alias_index_status <- alias_index_status
            nohit$alias_index_matches <- alias_index_matches
            return(nohit)
        } else if (startsWith(alias_index_status, "unique") &&
            is.data.frame(alias_index_matches) && nrow(alias_index_matches) > 0L) {
            selected_alias_match <- alias_index_matches[1, , drop = FALSE]
            selected_role <- as.character(selected_alias_match$match_role[1] %||% "")
            if (identical(selected_role, "stable_id")) {
                alias_lookup <- tryCatch(
                    alias_index_match_to_lookup(selected_alias_match, file_path = file_path, input_gene = input_gene),
                    error = function(e) NULL
                )
                if (!is.null(alias_lookup$data) && is.data.frame(alias_lookup$data) && nrow(alias_lookup$data) > 0L) {
                    query_candidates_used <<- normalize_lookup_query_candidates(c(
                        query_candidates_used,
                        selected_alias_match$query_term_original,
                        selected_alias_match$local_gene_id,
                        selected_alias_match$local_symbol
                    ))
                    out <- attach_lookup_result_meta(
                        alias_lookup,
                        query_candidates = query_candidates_used,
                        best_alias_used = as.character(selected_alias_match$query_term_original[1] %||% ""),
                        input_gene = input_gene
                    )
                    out$external_lookup_had_errors <- FALSE
                    out$det_resolved <- det
                    out$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
                    out$lookup_stage <- "alias_index"
                    out$alias_index_status <- alias_index_status
                    out$alias_index_match <- selected_alias_match
                    return(out)
                }
            }
        }
        NULL
    }

    preflight_lookup <- alias_index_preflight(det_info)
    if (!is.null(preflight_lookup)) {
        return(preflight_lookup)
    }

    res <- search_gene_in_file(file_path, input_gene, show_diagnostics = FALSE, match_mode = "exact", return_meta = TRUE)
    if (!is.null(res$data) && nrow(res$data) > 0) {
        out <- attach_lookup_result_meta(
            res,
            query_candidates = query_candidates_used,
            best_alias_used = "",
            input_gene = input_gene
        )
        out$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
        out$lookup_stage <- "local_exact"
        return(out)
    }

    det_resolved <- det_info
    det_has_info <- !is.null(det_resolved) && (!is.null(det_resolved$organism) || !is.null(det_resolved$taxid))
    if (!det_has_info) {
        det_resolved <- tryCatch(
            detect_organism_from_gff(file_path, original_name = file_label %||% basename(file_path)),
            error = function(e) list(organism = NULL, taxid = NULL, source = "none")
        )
    }
    org <- if (!is.null(det_resolved)) det_resolved$organism else NULL
    tx <- if (!is.null(det_resolved)) det_resolved$taxid else NULL

    partial_suggestions_for_query <- if (isTRUE(allow_partial_suggestions)) {
        find_deterministic_partial_gene_suggestions(
            annotation_paths = file_path,
            query = input_gene,
            file_labels = file_label %||% basename(file_path),
            det_list = list(det_resolved %||% list()),
            max_per_file = 20L,
            max_total = 20L,
            min_query_chars = 2L,
            min_shared_organisms = 1L,
            include_alias_sql = TRUE,
            base_dir = "."
        )
    } else {
        empty_partial_gene_suggestions_df(source_labels = TRUE, source_label_preview = TRUE)
    }
    route_to_partial_suggestions <- isTRUE(allow_partial_suggestions) &&
        ((is.data.frame(partial_suggestions_for_query) && nrow(partial_suggestions_for_query) > 0L) ||
            should_route_to_partial_gene_suggestions(file_path, input_gene))
    if (isTRUE(route_to_partial_suggestions)) {
        nohit <- attach_lookup_result_meta(
            list(data = NULL, matched_gene_id = NA_character_, matched_gene_name = NA_character_),
            query_candidates = query_candidates_used,
            best_alias_used = "",
            input_gene = input_gene
        )
        nohit$external_lookup_had_errors <- FALSE
        nohit$det_resolved <- det_resolved
        nohit$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
        nohit$lookup_stage <- "partial_suggestions"
        nohit$partial_gene_suggestions <- partial_suggestions_for_query
        return(nohit)
    }

    if (isTRUE(allow_partial_suggestions)) {
        res <- search_gene_in_file(file_path, input_gene, show_diagnostics = diagnostics, match_mode = "flex", return_meta = TRUE)
        if (!is.null(res$data) && nrow(res$data) > 0) {
            out <- attach_lookup_result_meta(
                res,
                query_candidates = query_candidates_used,
                best_alias_used = "",
                input_gene = input_gene
            )
            out$external_lookup_had_errors <- FALSE
            out$det_resolved <- det_resolved
            out$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
            out$lookup_stage <- "local_flex"
            return(out)
        }
    }

    alias_bridge <- list(found = FALSE, alias_candidates = character(0))
    external_lookup_had_errors <- FALSE
    alias_index_checked <- FALSE

    if (exists("search_alias_index_for_context", mode = "function") &&
        exists("alias_index_match_to_lookup", mode = "function")) {
        alias_index_checked <- TRUE
        alias_index_attempt <- tryCatch(
            lookup_alias_index_for_operation(det_resolved),
            error = function(e) NULL
        )
        alias_index_res <- (alias_index_attempt %||% list())$result
        alias_index_status <- as.character((alias_index_res %||% list())$status %||% "no_match")
        alias_index_matches <- (alias_index_res %||% list())$matches
        if (startsWith(alias_index_status, "multiple") &&
            is.data.frame(alias_index_matches) && nrow(alias_index_matches) > 0L) {
            nohit <- attach_lookup_result_meta(
                list(data = NULL, matched_gene_id = NA_character_, matched_gene_name = NA_character_),
                query_candidates = query_candidates_used,
                best_alias_used = "",
                input_gene = input_gene
            )
            nohit$external_lookup_had_errors <- FALSE
            nohit$det_resolved <- det_resolved
            nohit$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
            nohit$lookup_stage <- "alias_index_ambiguous"
            nohit$alias_index_status <- alias_index_status
            nohit$alias_index_matches <- alias_index_matches
            return(nohit)
        }
        if (startsWith(alias_index_status, "unique") &&
            is.data.frame(alias_index_matches) && nrow(alias_index_matches) > 0L) {
            selected_alias_match <- alias_index_matches[1, , drop = FALSE]
            selected_conf <- toupper(as.character(selected_alias_match$confidence[1] %||% ""))
            if (selected_conf %in% c("HIGH", "MEDIUM") ||
                isTRUE(is_verified_local_description_alias_match(selected_alias_match))) {
                alias_lookup <- tryCatch(
                    alias_index_match_to_lookup(selected_alias_match, file_path = file_path, input_gene = input_gene),
                    error = function(e) NULL
                )
                if (!is.null(alias_lookup$data) && is.data.frame(alias_lookup$data) && nrow(alias_lookup$data) > 0L) {
                    query_candidates_used <- normalize_lookup_query_candidates(c(
                        query_candidates_used,
                        selected_alias_match$query_term_original,
                        selected_alias_match$local_gene_id,
                        selected_alias_match$local_symbol
                    ))
                    out <- attach_lookup_result_meta(
                        alias_lookup,
                        query_candidates = query_candidates_used,
                        best_alias_used = as.character(selected_alias_match$query_term_original[1] %||% ""),
                        input_gene = input_gene
                    )
                    out$external_lookup_had_errors <- FALSE
                    out$det_resolved <- det_resolved
                    out$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
                    out$lookup_stage <- "alias_index"
                    out$alias_index_status <- alias_index_status
                    out$alias_index_match <- selected_alias_match
                    return(out)
                }
            }
        }
    }

    local_bridge_candidates <- build_gene_query_candidates(
        gene_name = input_gene,
        organism = org,
        taxid = tx,
        status_callback = NULL,
        max_aliases = 100,
        use_parallel = FALSE,
        alias_sources = character(0)
    )
    local_bridge_new_candidates <- setdiff(
        normalize_lookup_query_candidates(c(local_bridge_candidates)),
        query_candidates_used
    )
    if (length(local_bridge_new_candidates) > 0L) {
        alias_bridge <- resolve_external_alias_bridge(
            input_gene = input_gene,
            query_candidates = local_bridge_new_candidates,
            file_path = file_path,
            organism = org,
            taxid = tx,
            search_fun = search_gene_in_file
        )
        query_candidates_used <- normalize_lookup_query_candidates(c(query_candidates_used, local_bridge_candidates))
        if (isTRUE(alias_bridge$found)) {
            out <- attach_lookup_result_meta(
                alias_bridge$result,
                query_candidates = query_candidates_used,
                best_alias_used = alias_bridge$best_alias_used,
                input_gene = input_gene
            )
            out$external_lookup_had_errors <- FALSE
            out$det_resolved <- det_resolved
            out$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
            out$lookup_stage <- "local_bridge"
            return(out)
        }
    }

    if (!isTRUE(alias_index_checked) &&
        exists("search_alias_index_for_context", mode = "function") &&
        exists("alias_index_match_to_lookup", mode = "function")) {
        alias_index_attempt <- tryCatch(
            lookup_alias_index_for_operation(det_resolved),
            error = function(e) NULL
        )
        alias_index_res <- (alias_index_attempt %||% list())$result
        alias_index_status <- as.character((alias_index_res %||% list())$status %||% "no_match")
        alias_index_matches <- (alias_index_res %||% list())$matches
        if (startsWith(alias_index_status, "multiple") &&
            is.data.frame(alias_index_matches) && nrow(alias_index_matches) > 0L) {
            nohit <- attach_lookup_result_meta(
                list(data = NULL, matched_gene_id = NA_character_, matched_gene_name = NA_character_),
                query_candidates = query_candidates_used,
                best_alias_used = "",
                input_gene = input_gene
            )
            nohit$external_lookup_had_errors <- FALSE
            nohit$det_resolved <- det_resolved
            nohit$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
            nohit$lookup_stage <- "alias_index_ambiguous"
            nohit$alias_index_status <- alias_index_status
            nohit$alias_index_matches <- alias_index_matches
            return(nohit)
        }
        if (startsWith(alias_index_status, "unique") &&
            is.data.frame(alias_index_matches) && nrow(alias_index_matches) > 0L) {
            selected_alias_match <- alias_index_matches[1, , drop = FALSE]
            selected_conf <- toupper(as.character(selected_alias_match$confidence[1] %||% ""))
            if (selected_conf %in% c("HIGH", "MEDIUM") ||
                isTRUE(is_verified_local_description_alias_match(selected_alias_match))) {
                alias_lookup <- tryCatch(
                    alias_index_match_to_lookup(selected_alias_match, file_path = file_path, input_gene = input_gene),
                    error = function(e) NULL
                )
                if (!is.null(alias_lookup$data) && is.data.frame(alias_lookup$data) && nrow(alias_lookup$data) > 0L) {
                    query_candidates_used <- normalize_lookup_query_candidates(c(
                        query_candidates_used,
                        selected_alias_match$query_term_original,
                        selected_alias_match$local_gene_id,
                        selected_alias_match$local_symbol
                    ))
                    out <- attach_lookup_result_meta(
                        alias_lookup,
                        query_candidates = query_candidates_used,
                        best_alias_used = as.character(selected_alias_match$query_term_original[1] %||% ""),
                        input_gene = input_gene
                    )
                    out$external_lookup_had_errors <- FALSE
                    out$det_resolved <- det_resolved
                    out$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
                    out$lookup_stage <- "alias_index"
                    out$alias_index_status <- alias_index_status
                    out$alias_index_match <- selected_alias_match
                    return(out)
                }
            }
        }
    }

    fast_sources <- intersect(c("mygene", "uniprot"), enabled_external_sources)
    slow_sources <- intersect(c("ncbi", "ensembl"), enabled_external_sources)
    streaming_phases <- list()
    for (src in fast_sources) streaming_phases <- c(streaming_phases, list(src))
    for (src in slow_sources) streaming_phases <- c(streaming_phases, list(src))

    if (length(streaming_phases) > 0 && (!is.null(org) || !is.null(tx))) {
        for (phase_idx in seq_along(streaming_phases)) {
            phase_sources <- streaming_phases[[phase_idx]]
            query_candidates_phase <- build_gene_query_candidates(
                gene_name = input_gene,
                organism = org,
                taxid = tx,
                status_callback = NULL,
                max_aliases = 100,
                use_parallel = use_parallel,
                alias_sources = phase_sources
            )
            phase_lookup_meta <- attr(query_candidates_phase, "external_lookup_meta", exact = TRUE)
            if (isTRUE((phase_lookup_meta %||% list())$had_errors)) {
                external_lookup_had_errors <- TRUE
            }
            previous_candidates <- query_candidates_used
            query_candidates_used <- normalize_lookup_query_candidates(c(query_candidates_used, query_candidates_phase))
            if (length(query_candidates_used) == 0) {
                query_candidates_used <- normalize_lookup_query_candidates(c(input_gene))
            }
            new_alias_candidates <- setdiff(query_candidates_used, previous_candidates)
            if (length(new_alias_candidates) == 0L) {
                next
            }

            alias_bridge <- resolve_external_alias_bridge(
                input_gene = input_gene,
                query_candidates = new_alias_candidates,
                file_path = file_path,
                organism = org,
                taxid = tx,
                search_fun = search_gene_in_file
            )
            if (isTRUE(alias_bridge$found)) {
                out <- attach_lookup_result_meta(
                    alias_bridge$result,
                    query_candidates = query_candidates_used,
                    best_alias_used = alias_bridge$best_alias_used,
                    input_gene = input_gene
                )
                out$external_lookup_had_errors <- isTRUE(external_lookup_had_errors)
                out$det_resolved <- det_resolved
                out$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
                out$lookup_stage <- "external_alias"
                out$external_alias_source <- phase_sources
                return(out)
            }
        }
    }

    nohit <- attach_lookup_result_meta(
        list(data = NULL, matched_gene_id = NA_character_, matched_gene_name = NA_character_),
        query_candidates = query_candidates_used,
        best_alias_used = "",
        input_gene = input_gene
    )
    nohit$external_lookup_had_errors <- isTRUE(external_lookup_had_errors)
    nohit$det_resolved <- det_resolved
    nohit$lookup_elapsed_ms <- app_perf_elapsed_ms(lookup_t0)
    nohit$lookup_stage <- "no_match"
    nohit
}

run_orthologous_lookup_job_pure <- function(job) {
    j <- suppressWarnings(as.integer(job$file_idx %||% NA_integer_))
    file <- as.character(job$file_path %||% "")
    file_label <- as.character(job$file_label %||% basename(file))
    gene_name <- as.character(job$gene_name %||% "")
    forced_genome <- tryCatch(as.character(job$forced_genome %||% ""), error = function(e) "")
    if (length(forced_genome) == 0L || is.na(forced_genome[1])) {
        forced_genome <- ""
    } else {
        forced_genome <- forced_genome[1]
    }

    local_only <- length(as.character(job$enabled_external_sources %||% character(0))) == 0L &&
        !isTRUE(job$allow_partial_suggestions %||% TRUE)
    lookup_cache_key <- ""
    if (isTRUE(local_only) && nzchar(file) && file.exists(file) && nzchar(trimws(gene_name))) {
        lookup_cache_key <- paste(
            "ortho-local-v1",
            gff_cache_key(file),
            normalize_gene_compact(gene_name),
            sep = "|"
        )
        cached_lookup <- cache_env_get(.orthologous_local_lookup_cache, lookup_cache_key, default = NULL)
        if (!is.null(cached_lookup)) {
            return(cached_lookup)
        }
    }

    result <- tryCatch(
        {
            det_local <- job$det
            if (is.null(det_local)) {
                det_local <- detect_organism_from_gff(file, file_label)
            }
            lookup <- run_lookup_pipeline_pure(
                file_path = file,
                input_gene = gene_name,
                det_info = det_local,
                diagnostics = FALSE,
                use_parallel = FALSE,
                file_label = file_label,
                enabled_external_sources = as.character(job$enabled_external_sources %||% character(0)),
                allow_partial_suggestions = isTRUE(job$allow_partial_suggestions %||% TRUE)
            )
            det_final <- lookup$det_resolved %||% det_local
            data <- lookup$data
            if (is.null(data) || nrow(data) == 0) {
                lookup_stage_txt <- as.character((lookup %||% list())$lookup_stage %||% "")
                list(
                    found = FALSE,
                    data = NULL,
                    det = det_final,
                    lookup = lookup,
                    file_idx = j,
                    file_path = file,
                    file_label = file_label,
                    forced_genome = forced_genome,
                    reason = if (identical(lookup_stage_txt, "alias_index_ambiguous")) {
                        "Alias matched multiple genes; user selection required"
                    } else {
                        "No match found"
                    }
                )
            } else {
                neighbor_context <- tryCatch(
                    compute_neighbor_context_from_plot_data(
                        annotation_file_path = file,
                        plot_data = data,
                        fallback_gene_id = as.character(lookup$matched_gene_id %||% "")
                    ),
                    error = function(e) NULL
                )
                list(
                    found = TRUE,
                    data = data,
                    det = det_final,
                    lookup = lookup,
                    file_idx = j,
                    file_path = file,
                    file_label = file_label,
                    forced_genome = forced_genome,
                    neighbor_context = neighbor_context
                )
            }
        },
        error = function(e) {
            list(
                found = FALSE,
                data = NULL,
                det = NULL,
                lookup = NULL,
                file_idx = j,
                file_path = file,
                file_label = file_label,
                forced_genome = forced_genome,
                reason = paste0("Error: ", e$message)
            )
        }
    )
    if (nzchar(lookup_cache_key)) {
        cache_env_set(
            .orthologous_local_lookup_cache,
            lookup_cache_key,
            result,
            max_size = parse_positive_int_env("APP_ORTHO_LOCAL_LOOKUP_CACHE_MAX_ENTRIES", 256L),
            max_bytes = parse_positive_bytes_env_mb("APP_ORTHO_LOCAL_LOOKUP_CACHE_MAX_MB", 192)
        )
    }
    result
}

run_orthologous_lookup_job_worker <- function(job) {
    worker_env <- globalenv()
    libs_key <- ".cgv_lookup_worker_libs_loaded_v1"
    if (!isTRUE(get0(libs_key, envir = worker_env, inherits = FALSE, ifnotfound = FALSE))) {
        if (file.exists(file.path("R", "alias_resolution.R"))) {
            sys.source(file.path("R", "alias_resolution.R"), envir = worker_env)
        }
        sys.source(file.path("R", "utils.R"), envir = worker_env)
        if (file.exists(file.path("R", "gene_search_lib.R"))) {
            sys.source(file.path("R", "gene_search_lib.R"), envir = worker_env)
        }
        assign(libs_key, TRUE, envir = worker_env)
    }
    run_orthologous_lookup_job_pure(job)
}

fetch_gene_data_sync <- function(chr_name, gene_coords, fasta_path = NULL, fasta_id = NULL, exon_ranges = NULL, strand = "+") {
    fetch_perf <- app_perf_new_run("SEQ_FETCH")
    fetch_total_t0 <- app_perf_now()
    on.exit(app_perf_mark_ms(fetch_perf, "fetch_gene_total_ms", app_perf_elapsed_ms(fetch_total_t0), "SEQ"), add = TRUE)
    app_perf_mark(fetch_perf, "start", "SEQ")
    out <- tryCatch(
        {
            chr_name <- as.character(chr_name %||% "")
            start_pos <- suppressWarnings(as.numeric(gene_coords$start %||% NA_real_))
            end_pos <- suppressWarnings(as.numeric(gene_coords$end %||% NA_real_))
            spliced_fetch_t0 <- app_perf_now()
            seq_str <- extract_spliced_exon_sequence(fasta_path, chr_name, exon_ranges = exon_ranges, strand = strand)
            spliced_sequence <- seq_str
            app_perf_mark_ms(fetch_perf, "spliced_sequence_ms", app_perf_elapsed_ms(spliced_fetch_t0), "SEQ")
            app_perf_mark(fetch_perf, sprintf("after spliced len=%d", as.integer(nchar(seq_str %||% ""))), "SEQ")
            if (!nzchar(seq_str) && is.finite(start_pos) && is.finite(end_pos)) {
                seq_str <- extract_sequence_from_fasta(fasta_path, chr_name, start_pos, end_pos)
                app_perf_mark(fetch_perf, sprintf("after fallback len=%d", as.integer(nchar(seq_str %||% ""))), "SEQ")
            }
            composition_t0 <- app_perf_now()
            comp_info <- NULL
            if (!is.null(fasta_path) && nzchar(as.character(fasta_path %||% "")) && file.exists(fasta_path) &&
                !is.null(exon_ranges) && nrow(normalize_exon_ranges(exon_ranges)) > 0L) {
                comp_info <- tryCatch(
                    get_transcript_composition_cached(fasta_path, chr_name, exon_ranges, strand = strand,
                                                     spliced_sequence = spliced_sequence),
                    error = function(e) NULL
                )
            }
            if (is.null(comp_info) && nzchar(seq_str)) {
                comp_calc <- calculate_sequence_composition(seq_str)
                raw_seq <- toupper(gsub("\\s+", "", as.character(seq_str %||% "")))
                counts <- count_sequence_bases(raw_seq)
                comp_info <- list(
                    composition = comp_calc$composition,
                    length = comp_calc$length,
                    known_total = as.integer(sum(counts, na.rm = TRUE)),
                    counts = counts,
                    sequence_optional = NULL
                )
            }
            app_perf_mark_ms(fetch_perf, "composition_prepare_ms", app_perf_elapsed_ms(composition_t0), "SEQ")

            fasta_id <- safe_url_decode(as.character(fasta_id %||% ""))
            fasta_id <- trimws(fasta_id)
            if (!nzchar(fasta_id)) {
                if (nzchar(chr_name) && is.finite(start_pos) && is.finite(end_pos)) {
                    fasta_id <- sprintf("%s:%s-%s", chr_name, as.integer(round(start_pos)), as.integer(round(end_pos)))
                } else {
                    fasta_id <- "transcript"
                }
            }

            header <- if (nzchar(chr_name) && is.finite(start_pos) && is.finite(end_pos)) {
                sprintf(">%s | chr=%s | start=%s | end=%s", fasta_id, chr_name, as.integer(round(start_pos)), as.integer(round(end_pos)))
            } else {
                sprintf(">%s", fasta_id)
            }

            file_content <- if (nzchar(seq_str)) paste0(header, "\n", seq_str) else ""
            app_perf_mark(fetch_perf, "done", "SEQ")
            list(
                title = chr_name,
                sequence = seq_str,
                file_content = file_content,
                fasta_id = fasta_id,
                composition = comp_info,
                composition_blob = if (!is.null(comp_info)) make_sequence_composition_blob(comp_info) else ""
            )
        },
        error = function(e) {
            app_perf_mark(fetch_perf, sprintf("error: %s", as.character(e$message %||% "unknown")), "SEQ")
            list(title = as.character(chr_name %||% "Error"), sequence = "", file_content = "", fasta_id = as.character(fasta_id %||% "transcript"))
        }
    )
    out
}

fetch_gene_data_async <- function(chr_name, gene_coords, fasta_path = NULL, fasta_id = NULL, exon_ranges = NULL, strand = "+") {
    # Compatibility wrapper: return a resolved promise.
    promises::promise_resolve(
        fetch_gene_data_sync(
            chr_name = chr_name,
            gene_coords = gene_coords,
            fasta_path = fasta_path,
            fasta_id = fasta_id,
            exon_ranges = exon_ranges,
            strand = strand
        )
    )
}

# --- 6. CONTEXTO GENÓMICO: GENES VECINOS ---

extract_primary_gene_id <- function(attr) {
    attrs <- parse_gff_attributes(attr %||% "")
    parent_raw <- attrs[["parent"]][1] %||% ""
    parent_first <- trimws(strsplit(as.character(parent_raw), ",", fixed = TRUE)[[1]][1] %||% "")
    gid <- attrs[["gene_id"]][1] %||%
        attrs[["locus_tag"]][1] %||%
        if (nzchar(parent_first)) {
            parent_first
        } else {
            NULL %||%
                attrs[["id"]][1] %||%
                attrs[["gene"]][1]
        }
    if (is.null(gid) || is.na(gid) || !nzchar(gid)) {
        raw <- safe_url_decode(attr %||% "")
        gid <- stringr::str_extract(raw, "(^|;)ID=[^;]+") |> stringr::str_remove("(^|;)ID=")
    }
    gid %||% NA_character_
}

sanitize_gene_display_name <- function(x) {
    y <- as.character(x %||% NA_character_)
    if (length(y) == 0 || is.na(y) || !nzchar(trimws(y))) {
        return(NA_character_)
    }
    y <- safe_url_decode(y)
    y <- trimws(y)
    y <- stringr::str_remove(y, stringr::regex("^(gene|transcript|mrna)\\s*:\\s*", ignore_case = TRUE))
    if (length(y) == 0 || !nzchar(y)) {
        return(NA_character_)
    }
    y
}

extract_primary_gene_name <- function(attr) {
    attrs <- parse_gff_attributes(attr %||% "")
    gname <- attrs[["gene_name"]][1] %||%
        attrs[["gene"]][1] %||%
        attrs[["name"]][1] %||%
        attrs[["gene_symbol"]][1] %||%
        attrs[["symbol"]][1] %||%
        attrs[["locus_tag"]][1] %||%
        attrs[["locus"]][1] %||%
        attrs[["preferred_name"]][1]
    sanitize_gene_display_name(gname %||% NA_character_)
}

extract_primary_gene_id_batch <- function(attrs) {
    attrs <- as.character(attrs %||% "")
    # Try GFF3 ID= first
    gid <- stringr::str_match(attrs, "(?:^|;)\\s*ID=([^;]+)")[, 2]
    # Try GTF gene_id
    miss <- is.na(gid)
    if (any(miss)) {
        gid2 <- stringr::str_match(attrs[miss], '(?:^|;|\\t)\\s*gene_id\\s*[= ]\\s*"?([^;"\\t]+)')[, 2]
        gid[miss] <- gid2
    }
    # Try locus_tag
    miss <- is.na(gid)
    if (any(miss)) {
        gid3 <- stringr::str_match(attrs[miss], "(?:^|;)\\s*locus_tag=([^;]+)")[, 2]
        gid[miss] <- gid3
    }
    # Only decode non-NA values.
    valid_gid <- !is.na(gid)
    if (any(valid_gid)) gid[valid_gid] <- safe_url_decode(gid[valid_gid])
    gid
}

extract_primary_gene_name_batch <- function(attrs) {
    attrs <- as.character(attrs %||% "")
    # Priority order: gene_name, gene, Name, gene_symbol, symbol, locus_tag, locus, preferred_name
    patterns <- c(
        '(?:^|;|\\t)\\s*gene_name\\s*[= ]\\s*"?([^;"\\t]+)',
        '(?:^|;|\\t)\\s*gene\\s*[= ]\\s*"?([^;"\\t]+)',
        "(?:^|;)\\s*Name=([^;]+)",
        '(?:^|;|\\t)\\s*gene_symbol\\s*[= ]\\s*"?([^;"\\t]+)',
        '(?:^|;|\\t)\\s*symbol\\s*[= ]\\s*"?([^;"\\t]+)',
        "(?:^|;)\\s*locus_tag=([^;]+)",
        "(?:^|;)\\s*locus=([^;]+)",
        '(?:^|;|\\t)\\s*preferred_name\\s*[= ]\\s*"?([^;"\\t]+)'
    )
    gname <- rep(NA_character_, length(attrs))
    miss <- rep(TRUE, length(attrs))
    for (pat in patterns) {
        if (!any(miss)) break
        m <- stringr::str_match(attrs[miss], pat)
        found <- !is.na(m[, 2])
        idx_miss <- which(miss)
        gname[idx_miss[found]] <- m[found, 2]
        miss[idx_miss[found]] <- FALSE
    }
    # Only decode non-NA values.
    valid_gname <- !is.na(gname)
    if (any(valid_gname)) gname[valid_gname] <- safe_url_decode(gname[valid_gname])
    gname
}

sanitize_gene_display_name_batch <- function(x) {
    y <- as.character(x %||% NA_character_)
    y <- ifelse(is.na(y) | !nzchar(trimws(y)), NA_character_, y)
    valid <- !is.na(y)
    if (any(valid)) {
        y[valid] <- safe_url_decode(y[valid])
        y[valid] <- trimws(y[valid])
        y[valid] <- stringr::str_remove(y[valid], stringr::regex("^(gene|transcript|mrna)\\s*:\\s*", ignore_case = TRUE))
        y[valid] <- ifelse(nzchar(y[valid]), y[valid], NA_character_)
    }
    y
}

get_genes_table_from_annotation <- function(file_path) {
    key <- gff_cache_key(file_path)
    cached <- cache_env_get(.gff_genes_table_cache, key, default = NULL)
    if (!is.null(cached)) {
        required_cols <- c("gene_id", "neighbor_id", "neighbor_name", "neighbor_label", "chr", "start", "end", "strand")
        if (is.data.frame(cached) && all(required_cols %in% colnames(cached))) {
            return(cached)
        }
        cache_env_drop(.gff_genes_table_cache, key)
    }

    # Always prefer the light gene index for neighbor context:
    # it only streams gene/CDS rows and avoids parsing the full annotation.
    idx_light <- build_gff_gene_light_index(file_path)
    genes_raw <- idx_light$genes_df

    if (nrow(genes_raw) == 0) {
        out <- data.frame()
        cache_env_set(
            .gff_genes_table_cache,
            key,
            out,
            max_size = annotation_memory_cache_limits$genes_table_max_entries,
            max_bytes = annotation_memory_cache_limits$genes_table_max_bytes
        )
        return(out)
    }

    chr_col <- if ("seqid" %in% colnames(genes_raw)) "seqid" else "V1"
    start_col <- if ("start" %in% colnames(genes_raw)) "start" else "V4"
    end_col <- if ("end" %in% colnames(genes_raw)) "end" else "V5"
    strand_col <- if ("strand" %in% colnames(genes_raw)) "strand" else "V7"
    type_col <- if ("type" %in% colnames(genes_raw)) "type" else "V3"
    attrs_col <- if ("attributes" %in% colnames(genes_raw)) "attributes" else "V9"

    attrs <- genes_raw[[attrs_col]] %||% rep("", nrow(genes_raw))
    # Vectorized batch extraction — much faster than per-row vapply
    gene_ids <- sanitize_gene_display_name_batch(extract_primary_gene_id_batch(attrs))
    gene_names <- sanitize_gene_display_name_batch(extract_primary_gene_name_batch(attrs))
    neighbor_name <- ifelse(!is.na(gene_names) & nzchar(trimws(gene_names)), gene_names, NA_character_)
    neighbor_id <- ifelse(!is.na(gene_ids) & nzchar(trimws(gene_ids)), gene_ids, NA_character_)
    # Fallback chain: name → id → gene_id → "Unknown" — never NA
    neighbor_label <- ifelse(!is.na(neighbor_name) & nzchar(trimws(neighbor_name)), neighbor_name,
        ifelse(!is.na(neighbor_id) & nzchar(trimws(neighbor_id)), neighbor_id,
            ifelse(!is.na(gene_ids) & nzchar(trimws(gene_ids)), gene_ids, "Unknown")
        )
    )

    out <- data.frame(
        gene_id = gene_ids,
        neighbor_id = neighbor_id,
        neighbor_name = neighbor_name,
        neighbor_label = neighbor_label,
        chr = as.character(genes_raw[[chr_col]]),
        start = as.numeric(genes_raw[[start_col]]),
        end = as.numeric(genes_raw[[end_col]]),
        strand = as.character(genes_raw[[strand_col]]),
        type = as.character(genes_raw[[type_col]]),
        stringsAsFactors = FALSE
    ) |>
        dplyr::filter(is.finite(start), is.finite(end), !is.na(chr), nzchar(chr)) |>
        dplyr::arrange(chr, start, end)

    cache_env_set(
        .gff_genes_table_cache,
        key,
        out,
        max_size = annotation_memory_cache_limits$genes_table_max_entries,
        max_bytes = annotation_memory_cache_limits$genes_table_max_bytes
    )
    out
}

get_genes_chr_index_from_annotation <- function(file_path, genes_df = NULL) {
    key <- gff_cache_key(file_path)
    cached <- cache_env_get(.gff_genes_chr_index_cache, key, default = NULL)
    if (!is.null(cached)) {
        if (is.list(cached) && !is.null(cached$rows) && !is.null(cached$start) && !is.null(cached$end)) {
            return(cached)
        }
        cache_env_drop(.gff_genes_chr_index_cache, key)
    }

    if (is.null(genes_df)) {
        genes_df <- get_genes_table_from_annotation(file_path)
    }

    if (!is.data.frame(genes_df) || nrow(genes_df) == 0) {
        out <- list(rows = list(), start = list(), end = list())
        cache_env_set(
            .gff_genes_chr_index_cache,
            key,
            out,
            max_size = annotation_memory_cache_limits$genes_chr_index_max_entries,
            max_bytes = annotation_memory_cache_limits$genes_chr_index_max_bytes
        )
        return(out)
    }

    chr_vec <- as.character(genes_df$chr %||% rep("", nrow(genes_df)))
    start_vec <- suppressWarnings(as.numeric(genes_df$start))
    end_vec <- suppressWarnings(as.numeric(genes_df$end))
    valid <- is.finite(start_vec) & is.finite(end_vec) & !is.na(chr_vec) & nzchar(chr_vec)
    idx_valid <- which(valid)
    if (length(idx_valid) == 0) {
        out <- list(rows = list(), start = list(), end = list())
        cache_env_set(
            .gff_genes_chr_index_cache,
            key,
            out,
            max_size = annotation_memory_cache_limits$genes_chr_index_max_entries,
            max_bytes = annotation_memory_cache_limits$genes_chr_index_max_bytes
        )
        return(out)
    }

    chr_valid <- chr_vec[idx_valid]
    rows_by_chr <- split(idx_valid, chr_valid, drop = TRUE)
    starts_by_chr <- lapply(rows_by_chr, function(ix) start_vec[ix])
    ends_by_chr <- lapply(rows_by_chr, function(ix) end_vec[ix])

    out <- list(
        rows = rows_by_chr,
        start = starts_by_chr,
        end = ends_by_chr
    )
    cache_env_set(
        .gff_genes_chr_index_cache,
        key,
        out,
        max_size = annotation_memory_cache_limits$genes_chr_index_max_entries,
        max_bytes = annotation_memory_cache_limits$genes_chr_index_max_bytes
    )
    out
}

get_nearest_neighbors <- function(gene_target, genes_df, chr_index = NULL) {
    empty_neighbor <- list(
        neighbor_id = NA_character_,
        neighbor_name = NA_character_,
        neighbor_label = NA_character_,
        neighbor_start = NA_real_,
        neighbor_end = NA_real_,
        neighbor_chr = NA_character_,
        neighbor_strand = NA_character_,
        dist_bp = NA_real_
    )

    if (is.null(gene_target) || nrow(genes_df) == 0) {
        return(list(
            upstream = empty_neighbor,
            downstream = empty_neighbor,
            overlapping = list(),
            flags = list(
                has_up = FALSE, has_down = FALSE,
                overlap_up = FALSE, overlap_down = FALSE,
                has_overlap = FALSE, overlap_count = 0L
            )
        ))
    }

    target_chr <- as.character(gene_target$chr[1] %||% "")
    target_start <- as.numeric(gene_target$start[1] %||% NA_real_)
    target_end <- as.numeric(gene_target$end[1] %||% NA_real_)
    if (!nzchar(target_chr) || !is.finite(target_start) || !is.finite(target_end)) {
        return(list(
            upstream = empty_neighbor,
            downstream = empty_neighbor,
            overlapping = list(),
            flags = list(
                has_up = FALSE, has_down = FALSE,
                overlap_up = FALSE, overlap_down = FALSE,
                has_overlap = FALSE, overlap_count = 0L
            )
        ))
    }
    chr_vec <- as.character(genes_df$chr %||% rep("", nrow(genes_df)))
    start_vec_full <- suppressWarnings(as.numeric(genes_df$start))
    end_vec_full <- suppressWarnings(as.numeric(genes_df$end))

    idx_chr <- integer(0)
    start_vec_chr <- numeric(0)
    end_vec_chr <- numeric(0)
    if (is.list(chr_index) && !is.null(chr_index$rows) && target_chr %in% names(chr_index$rows)) {
        idx_chr <- as.integer(chr_index$rows[[target_chr]])
        start_vec_chr <- suppressWarnings(as.numeric(chr_index$start[[target_chr]]))
        end_vec_chr <- suppressWarnings(as.numeric(chr_index$end[[target_chr]]))
        keep_local <- is.finite(idx_chr) & idx_chr >= 1L & idx_chr <= nrow(genes_df) &
            is.finite(start_vec_chr) & is.finite(end_vec_chr)
        idx_chr <- idx_chr[keep_local]
        start_vec_chr <- start_vec_chr[keep_local]
        end_vec_chr <- end_vec_chr[keep_local]
    } else {
        valid <- is.finite(start_vec_full) & is.finite(end_vec_full) & !is.na(chr_vec) & nzchar(chr_vec)
        idx_chr <- which(valid & chr_vec == target_chr)
        if (length(idx_chr) > 0) {
            start_vec_chr <- start_vec_full[idx_chr]
            end_vec_chr <- end_vec_full[idx_chr]
        }
    }
    if (length(idx_chr) > 0) {
        same_coords <- start_vec_chr == target_start & end_vec_chr == target_end
        target_id <- trimws(as.character(gene_target$gene_id[1] %||% ""))
        target_strand <- trimws(as.character(gene_target$strand[1] %||% ""))
        candidate_ids <- as.character(genes_df$gene_id[idx_chr] %||% rep("", length(idx_chr)))
        candidate_strands <- as.character(genes_df$strand[idx_chr] %||% rep("", length(idx_chr)))
        if (nzchar(target_id)) {
            is_self <- same_coords & (
                trimws(candidate_ids) == target_id |
                    (nzchar(target_strand) & trimws(candidate_strands) == target_strand)
            )
        } else if (nzchar(target_strand)) {
            is_self <- same_coords & trimws(candidate_strands) == target_strand
        } else {
            is_self <- same_coords
        }
        keep_non_self <- !is_self
        idx_chr <- idx_chr[keep_non_self]
        start_vec_chr <- start_vec_chr[keep_non_self]
        end_vec_chr <- end_vec_chr[keep_non_self]
    }
    if (length(idx_chr) == 0) {
        return(list(
            upstream = empty_neighbor,
            downstream = empty_neighbor,
            overlapping = list(),
            flags = list(
                has_up = FALSE, has_down = FALSE,
                overlap_up = FALSE, overlap_down = FALSE,
                has_overlap = FALSE, overlap_count = 0L
            )
        ))
    }

    idx_up_non_overlap <- which(end_vec_chr < target_start)
    up_idx <- if (length(idx_up_non_overlap) > 0) {
        idx_chr[idx_up_non_overlap[which.max(end_vec_chr[idx_up_non_overlap])]]
    } else {
        NA_integer_
    }

    idx_down_non_overlap <- which(start_vec_chr > target_end)
    down_idx <- if (length(idx_down_non_overlap) > 0) {
        idx_chr[idx_down_non_overlap[which.min(start_vec_chr[idx_down_non_overlap])]]
    } else {
        NA_integer_
    }
    idx_overlap_local <- which(start_vec_chr <= target_end & end_vec_chr >= target_start)
    overlap_idx <- idx_chr[idx_overlap_local]

    build_neighbor <- function(idx_one, target_start_local, target_end_local) {
        if (!is.finite(idx_one) || is.na(idx_one) || idx_one < 1L || idx_one > nrow(genes_df)) {
            return(empty_neighbor)
        }
        nb_name <- as.character(genes_df$neighbor_name[idx_one] %||% "")
        nb_id <- as.character(genes_df$neighbor_id[idx_one] %||% "")
        nb_gene_id <- as.character(genes_df$gene_id[idx_one] %||% "")
        nb_label <- as.character(genes_df$neighbor_label[idx_one] %||% "")
        if (!nzchar(trimws(nb_label))) nb_label <- nb_name
        if (!nzchar(trimws(nb_label))) nb_label <- nb_id
        if (!nzchar(trimws(nb_label))) nb_label <- nb_gene_id
        if (!nzchar(trimws(nb_label))) nb_label <- "Unknown"
        if (!nzchar(trimws(nb_id))) nb_id <- nb_gene_id
        if (!nzchar(trimws(nb_name))) nb_name <- nb_label
        nb_start <- as.numeric(start_vec_full[idx_one])
        nb_end <- as.numeric(end_vec_full[idx_one])
        dist_val <- if (nb_end < target_start_local) {
            as.numeric(target_start_local - nb_end - 1)
        } else if (nb_start > target_end_local) {
            as.numeric(nb_start - target_end_local - 1)
        } else {
            -as.numeric(min(target_end_local, nb_end) - max(target_start_local, nb_start) + 1)
        }
        list(
            neighbor_id = nb_id,
            neighbor_name = nb_name,
            neighbor_label = nb_label,
            neighbor_start = nb_start,
            neighbor_end = nb_end,
            neighbor_chr = as.character(chr_vec[idx_one]),
            neighbor_strand = as.character(genes_df$strand[idx_one] %||% ""),
            dist_bp = dist_val
        )
    }

    upstream <- build_neighbor(up_idx, target_start, target_end)
    downstream <- build_neighbor(down_idx, target_start, target_end)
    overlapping <- lapply(
        overlap_idx,
        build_neighbor,
        target_start_local = target_start,
        target_end_local = target_end
    )

    list(
        upstream = upstream,
        downstream = downstream,
        overlapping = overlapping,
        flags = list(
            has_up = !is.na(upstream$dist_bp),
            has_down = !is.na(downstream$dist_bp),
            overlap_up = FALSE,
            overlap_down = FALSE,
            has_overlap = length(overlapping) > 0,
            overlap_count = as.integer(length(overlapping))
        )
    )
}

get_nearest_neighbors_from_light_index <- function(gene_target, genes_df) {
    empty_neighbor <- list(
        neighbor_id = NA_character_,
        neighbor_name = NA_character_,
        neighbor_label = NA_character_,
        neighbor_start = NA_real_,
        neighbor_end = NA_real_,
        neighbor_chr = NA_character_,
        neighbor_strand = NA_character_,
        dist_bp = NA_real_
    )

    empty_context <- list(
        upstream = empty_neighbor,
        downstream = empty_neighbor,
        overlapping = list(),
        flags = list(
            has_up = FALSE, has_down = FALSE,
            overlap_up = FALSE, overlap_down = FALSE,
            has_overlap = FALSE, overlap_count = 0L
        )
    )

    if (is.null(gene_target) || is.null(genes_df) || !is.data.frame(genes_df) || nrow(genes_df) == 0) {
        return(empty_context)
    }

    get_col <- function(df, primary, fallback = NULL, default = NULL) {
        if (primary %in% colnames(df)) return(df[[primary]])
        if (!is.null(fallback) && fallback %in% colnames(df)) return(df[[fallback]])
        default %||% rep(NA, nrow(df))
    }

    target_chr <- as.character(gene_target$chr[1] %||% "")
    target_start <- suppressWarnings(as.numeric(gene_target$start[1] %||% NA_real_))
    target_end <- suppressWarnings(as.numeric(gene_target$end[1] %||% NA_real_))
    if (!nzchar(target_chr) || !is.finite(target_start) || !is.finite(target_end)) {
        return(empty_context)
    }

    chr_vec <- as.character(get_col(genes_df, "seqid", "V1", ""))
    start_vec <- suppressWarnings(as.numeric(get_col(genes_df, "start", "V4", NA_real_)))
    end_vec <- suppressWarnings(as.numeric(get_col(genes_df, "end", "V5", NA_real_)))
    valid <- is.finite(start_vec) & is.finite(end_vec) & !is.na(chr_vec) & nzchar(chr_vec)
    idx_chr <- which(valid & chr_vec == target_chr)
    if (length(idx_chr) == 0) {
        return(empty_context)
    }

    start_chr <- start_vec[idx_chr]
    end_chr <- end_vec[idx_chr]
    same_coords <- start_chr == target_start & end_chr == target_end
    target_id <- trimws(as.character(gene_target$gene_id[1] %||% ""))
    target_strand <- trimws(as.character(gene_target$strand[1] %||% ""))
    attrs_vec_full <- as.character(get_col(genes_df, "attributes", "V9", ""))
    strand_vec_full <- as.character(get_col(genes_df, "strand", "V7", ""))
    same_local <- which(same_coords)
    is_self <- rep(FALSE, length(idx_chr))
    if (length(same_local) > 0 && nzchar(target_id)) {
        same_ids <- vapply(
            attrs_vec_full[idx_chr[same_local]],
            function(attr) sanitize_gene_display_name(extract_primary_gene_id(as.character(attr %||% ""))),
            character(1)
        )
        is_self[same_local] <- trimws(same_ids) == target_id |
            (nzchar(target_strand) & trimws(strand_vec_full[idx_chr[same_local]]) == target_strand)
    } else if (length(same_local) > 0 && nzchar(target_strand)) {
        is_self[same_local] <- trimws(strand_vec_full[idx_chr[same_local]]) == target_strand
    } else if (length(same_local) > 0) {
        is_self[same_local] <- TRUE
    }
    keep_non_self <- !is_self
    idx_chr <- idx_chr[keep_non_self]
    start_chr <- start_chr[keep_non_self]
    end_chr <- end_chr[keep_non_self]
    if (length(idx_chr) == 0) {
        return(empty_context)
    }

    idx_up_non_overlap <- which(end_chr < target_start)
    up_idx <- if (length(idx_up_non_overlap) > 0) {
        idx_chr[idx_up_non_overlap[which.max(end_chr[idx_up_non_overlap])]]
    } else {
        NA_integer_
    }

    idx_down_non_overlap <- which(start_chr > target_end)
    down_idx <- if (length(idx_down_non_overlap) > 0) {
        idx_chr[idx_down_non_overlap[which.min(start_chr[idx_down_non_overlap])]]
    } else {
        NA_integer_
    }
    idx_overlap_local <- which(start_chr <= target_end & end_chr >= target_start)
    overlap_idx <- idx_chr[idx_overlap_local]

    attrs_vec <- attrs_vec_full
    strand_vec <- strand_vec_full

    build_neighbor <- function(idx_one) {
        if (!is.finite(idx_one) || is.na(idx_one) || idx_one < 1L || idx_one > nrow(genes_df)) {
            return(empty_neighbor)
        }
        attr <- as.character(attrs_vec[idx_one] %||% "")
        nb_id <- sanitize_gene_display_name(extract_primary_gene_id(attr))
        nb_name <- sanitize_gene_display_name(extract_primary_gene_name(attr))
        nb_label <- nb_name %|||% nb_id %|||% "Unknown"
        nb_id <- nb_id %|||% nb_label
        nb_name <- nb_name %|||% nb_label
        nb_start <- suppressWarnings(as.numeric(start_vec[idx_one]))
        nb_end <- suppressWarnings(as.numeric(end_vec[idx_one]))
        dist_val <- if (nb_end < target_start) {
            as.numeric(target_start - nb_end - 1)
        } else if (nb_start > target_end) {
            as.numeric(nb_start - target_end - 1)
        } else {
            -as.numeric(min(target_end, nb_end) - max(target_start, nb_start) + 1)
        }
        list(
            neighbor_id = nb_id,
            neighbor_name = nb_name,
            neighbor_label = nb_label,
            neighbor_start = nb_start,
            neighbor_end = nb_end,
            neighbor_chr = as.character(chr_vec[idx_one] %||% target_chr),
            neighbor_strand = as.character(strand_vec[idx_one] %||% ""),
            dist_bp = dist_val
        )
    }

    upstream <- build_neighbor(up_idx)
    downstream <- build_neighbor(down_idx)
    overlapping <- lapply(overlap_idx, build_neighbor)
    list(
        upstream = upstream,
        downstream = downstream,
        overlapping = overlapping,
        flags = list(
            has_up = !is.na(upstream$dist_bp),
            has_down = !is.na(downstream$dist_bp),
            overlap_up = FALSE,
            overlap_down = FALSE,
            has_overlap = length(overlapping) > 0,
            overlap_count = as.integer(length(overlapping))
        )
    )
}

format_distance <- function(d_bp) {
    if (is.null(d_bp) || is.na(d_bp) || !is.finite(d_bp)) {
        return(list(label_short = "N/A", label_exact = "N/A"))
    }

    d_num <- as.numeric(d_bp)
    d_abs <- abs(d_num)

    human <- if (d_abs >= 1e6) {
        sprintf("%.1f Mb", d_abs / 1e6)
    } else if (d_abs >= 1e3) {
        sprintf("%.1f kb", d_abs / 1e3)
    } else {
        sprintf("%d bp", as.integer(round(d_abs)))
    }
    human <- sub("\\.0\\s", " ", human)

    short <- if (d_num < 0) sprintf("overlap (%s)", human) else human
    exact <- sprintf("%d bp", as.integer(round(d_num)))

    list(label_short = short, label_exact = exact)
}

log_scale_position <- function(d_bp, d_min = 10, d_max = 1e6) {
    if (is.null(d_bp) || is.na(d_bp) || !is.finite(d_bp)) {
        return(NA_real_)
    }
    d_min <- max(1, as.numeric(d_min))
    d_max <- max(d_min + 1, as.numeric(d_max))
    d_abs <- abs(as.numeric(d_bp))
    d_clamped <- min(max(d_abs, d_min), d_max)
    s <- (log10(d_clamped) - log10(d_min)) / (log10(d_max) - log10(d_min))
    min(max(s, 0), 1)
}

get_neighbor_context_for_target <- function(annotation_file_path, gene_target) {
    if (is.null(annotation_file_path) || !nzchar(annotation_file_path) || is.null(gene_target)) {
        return(NULL)
    }

    chr_txt <- trimws(as.character(gene_target$chr[1] %||% ""))
    start_num <- suppressWarnings(as.numeric(gene_target$start[1] %||% NA_real_))
    end_num <- suppressWarnings(as.numeric(gene_target$end[1] %||% NA_real_))
    gid_txt <- trimws(as.character(gene_target$gene_id[1] %||% ""))
    if (!nzchar(chr_txt) || !is.finite(start_num) || !is.finite(end_num)) {
        return(NULL)
    }
    key <- paste(
        normalizePath(as.character(annotation_file_path), winslash = "/", mustWork = FALSE),
        chr_txt,
        as.integer(round(start_num)),
        as.integer(round(end_num)),
        gid_txt,
        sep = "::"
    )
    if (exists(key, envir = .neighbor_context_cache, inherits = FALSE)) {
        return(get(key, envir = .neighbor_context_cache, inherits = FALSE))
    }

    idx_light <- tryCatch(build_gff_gene_light_index(annotation_file_path), error = function(e) NULL)
    if (is.list(idx_light) && is.data.frame(idx_light$genes_df) && nrow(idx_light$genes_df) > 0) {
        out <- get_nearest_neighbors_from_light_index(gene_target, idx_light$genes_df)
        assign(key, out, envir = .neighbor_context_cache)
        trim_cache_env(.neighbor_context_cache, max_size = 5000L)
        return(out)
    }

    genes_df <- get_genes_table_from_annotation(annotation_file_path)
    if (nrow(genes_df) == 0) {
        return(NULL)
    }
    chr_index <- get_genes_chr_index_from_annotation(annotation_file_path, genes_df = genes_df)
    out <- get_nearest_neighbors(gene_target, genes_df, chr_index = chr_index)
    assign(key, out, envir = .neighbor_context_cache)
    trim_cache_env(.neighbor_context_cache, max_size = 5000L)
    out
}

compute_neighbor_context_from_plot_data <- function(annotation_file_path, plot_data, fallback_gene_id = "") {
    if (is.null(annotation_file_path) || !nzchar(annotation_file_path) ||
        is.null(plot_data) || !is.data.frame(plot_data) || nrow(plot_data) == 0) {
        return(NULL)
    }
    row_types <- tolower(trimws(as.character(plot_data$V3 %||% rep("", nrow(plot_data)))))
    gene_rows <- plot_data[row_types == "gene", , drop = FALSE]
    span_df <- if (nrow(gene_rows) > 0) gene_rows else plot_data
    tgt_chr <- as.character(span_df$V1[1] %||% plot_data$V1[1] %||% NA_character_)
    tgt_start <- suppressWarnings(as.numeric(min(span_df$V4, na.rm = TRUE)))
    tgt_end <- suppressWarnings(as.numeric(max(span_df$V5, na.rm = TRUE)))
    if (!is.finite(tgt_start) || !is.finite(tgt_end) || tgt_end < tgt_start) {
        return(NULL)
    }
    tgt_attr <- as.character(span_df$V9[1] %||% plot_data$V9[1] %||% "")
    gid <- trimws(as.character(fallback_gene_id %||% ""))
    if (!nzchar(gid)) {
        gid <- extract_primary_gene_id(tgt_attr)
    }
    target_gene <- data.frame(
        gene_id = gid,
        chr = tgt_chr,
        start = tgt_start,
        end = tgt_end,
        strand = as.character(span_df$V7[1] %||% plot_data$V7[1] %||% NA_character_),
        stringsAsFactors = FALSE
    )
    get_neighbor_context_for_target(annotation_file_path, target_gene)
}

# ═══════════════════════════════════════════════════════════════════════════════
# Protein-guided exon correspondence for "Comparative Aligned" view
# ─────────────────────────────────────────────────────────────────────────────
# Replaces the naive index-based ribbon pairing with biologically correct
# homology blocks derived from pairwise protein alignment (BLOSUM62).
# Pipeline: CDS extraction → translation → NW alignment (protein) →
#           codon→CDS-feature mapping → visual exon overlap → event classification
# ═══════════════════════════════════════════════════════════════════════════════

get_aligned_dna_substitution_matrix <- function() {
    # pwalign does not resolve "EDNAFULL" by name; use an explicit
    # IUPAC-aware nucleotide matrix with the same match/mismatch backbone.
    pwalign::nucleotideSubstitutionMatrix(match = 5, mismatch = -4, baseOnly = FALSE)
}

# Build a position-to-CDS-feature-index map.
# Returns an integer vector where element i = CDS-feature index (1-based)
# that owns nucleotide position i of the concatenated CDS.
# starts / ends: numeric vectors in transcription order (5'→3').
build_pos_to_exon_map <- function(starts, ends) {
    lens <- as.integer(round(ends - starts + 1L))
    lens[lens < 1L] <- 0L
    total_len <- sum(lens)
    if (total_len <= 0L) return(integer(0L))
    pos_map <- integer(total_len)
    cursor <- 1L
    for (i in seq_along(lens)) {
        if (lens[i] == 0L) next
        seg_end <- cursor + lens[i] - 1L
        pos_map[cursor:seg_end] <- i
        cursor <- seg_end + 1L
    }
    pos_map
}

# Compute protein-guided exon correspondence between two transcripts.
#
# Parameters:
#   df1, df2   – full track data.frames (aligned-view format, with columns:
#                plot_group, feature_type, xstart, xend, strand_val, genome_path, seqid)
#   df1e, df2e – visual exon rows of df1/df2 (sorted by rel_start, left-to-right)
#   min_codons – minimum aligned codons to call a correspondence (default 15)
#
# Returns a data.frame with columns:
#   vis_exon_A, vis_exon_B  – 1-based row indices into df1e / df2e
#   n_codons                 – number of aligned codons in this block
#   identity_pct             – % identical amino acids within block
#   overall_pid              – overall protein % identity for the pair
#   event_type               – "1:1", "1:n", "n:1", "partial"
# Returns NULL on any failure (no CDS, no FASTA, alignment error).
compute_protein_guided_correspondence <- function(df1, df2, df1e, df2e,
                                                  min_codons = 15L,
                                                  mode = "protein") {
    # mode: "protein" (BLOSUM62, recommended), "cds" (DNA NW on CDS),
    #       "exon" (DNA NW on full spliced exon, includes UTRs)
    mode <- tolower(trimws(mode %||% "protein"))
    if (!mode %in% c("protein", "cds", "exon")) mode <- "protein"

    # ── safety checks ────────────────────────────────────────────────────────
    if (is.null(df1) || is.null(df2) || is.null(df1e) || is.null(df2e)) return(NULL)
    if (nrow(df1e) == 0L || nrow(df2e) == 0L) return(NULL)
    if (!requireNamespace("Biostrings", quietly = TRUE)) return(NULL)
    if (!requireNamespace("pwalign",    quietly = TRUE)) return(NULL)

    # ── internal helpers ──────────────────────────────────────────────────────
    first_val <- function(df, ...) {
        for (nm in c(...)) {
            if (nm %in% names(df)) {
                v <- as.character(df[[nm]])
                v <- v[!is.na(v) & nzchar(v)]
                if (length(v) > 0L) return(v[1L])
            }
        }
        NULL
    }

    get_cds_rows <- function(df) {
        pg <- tolower(trimws(as.character(df$plot_group   %||% "")))
        ft <- tolower(trimws(as.character(df$feature_type %||% "")))
        cds <- df[(pg == "cds" | ft == "cds"), , drop = FALSE]
        if (nrow(cds) == 0L) return(NULL)
        cds$xstart <- suppressWarnings(as.numeric(cds$xstart))
        cds$xend   <- suppressWarnings(as.numeric(cds$xend))
        cds <- cds[is.finite(cds$xstart) & is.finite(cds$xend) &
                       cds$xend > cds$xstart, , drop = FALSE]
        if (nrow(cds) == 0L) NULL else cds
    }

    apply_phase <- function(seq_txt, cds_sorted) {
        # Remove non-coding prefix indicated by phase of first CDS feature
        ph_raw <- suppressWarnings(as.integer(
            (cds_sorted$phase %||% cds_sorted$V8 %||% 0L)[1L]
        ))
        ph <- if (!is.na(ph_raw) && ph_raw %in% c(1L, 2L)) ph_raw else 0L
        if (ph > 0L && nchar(seq_txt) > ph) substring(seq_txt, ph + 1L) else seq_txt
    }

    trim_to_codon <- function(s) {
        r <- nchar(s) %% 3L
        if (r > 0L) substr(s, 1L, nchar(s) - r) else s
    }

    translate_safe <- function(s) {
        tryCatch({
            dna <- Biostrings::DNAString(toupper(s))
            aa  <- as.character(Biostrings::translate(dna, if.fuzzy.codon = "solve"))
            sub("\\*$", "", aa)          # strip terminal stop
        }, error = function(e) NULL)
    }

    # ── genome paths & seqids (needed by all modes) ──────────────────────────
    str1 <- first_val(df1, "strand_val", "strand") %||% "+"
    str2 <- first_val(df2, "strand_val", "strand") %||% "+"
    gp1  <- first_val(df1, "genome_path")
    gp2  <- first_val(df2, "genome_path")
    sid1 <- first_val(df1, "seqid", "V1")
    sid2 <- first_val(df2, "seqid", "V1")
    if (is.null(gp1) || is.null(gp2) || is.null(sid1) || is.null(sid2)) return(NULL)

    # ── feature-index → visual exon index (by genomic overlap) ───────────────
    map_cds_to_visual <- function(feat_sorted, df_vis) {
        vis_xs <- suppressWarnings(as.numeric(df_vis$xstart))
        vis_xe <- suppressWarnings(as.numeric(df_vis$xend))
        f_xs   <- as.numeric(feat_sorted$xstart)
        f_xe   <- as.numeric(feat_sorted$xend)
        mapping <- integer(nrow(feat_sorted))
        for (i in seq_len(nrow(feat_sorted))) {
            ov <- which(is.finite(vis_xs) & vis_xs <= f_xe[i] &
                            is.finite(vis_xe) & vis_xe >= f_xs[i])
            mapping[i] <- if (length(ov) > 0L) ov[1L] else NA_integer_
        }
        mapping
    }

    compute_feature_units_by_visual <- function(feature_df, vis_map, n_vis, aligned_stride = 1L) {
        out_units <- rep(NA_real_, as.integer(n_vis %||% 0L))
        if (is.null(feature_df) || !is.data.frame(feature_df) || nrow(feature_df) == 0L ||
            length(vis_map) == 0L || length(out_units) == 0L) {
            return(out_units)
        }
        feat_start <- suppressWarnings(as.numeric(feature_df$xstart))
        feat_end <- suppressWarnings(as.numeric(feature_df$xend))
        feat_len_nt <- pmax(0, abs(feat_end - feat_start) + 1)
        accum_nt <- rep(0, length(out_units))
        seen <- rep(FALSE, length(out_units))
        for (i in seq_len(min(length(vis_map), length(feat_len_nt)))) {
            vi <- suppressWarnings(as.integer(vis_map[[i]]))
            if (!is.finite(vi) || vi < 1L || vi > length(accum_nt)) next
            if (!is.finite(feat_len_nt[[i]]) || feat_len_nt[[i]] <= 0) next
            accum_nt[[vi]] <- accum_nt[[vi]] + feat_len_nt[[i]]
            seen[[vi]] <- TRUE
        }
        divisor <- if (isTRUE(as.integer(aligned_stride %||% 1L) == 3L)) 3 else 1
        out_units[seen] <- accum_nt[seen] / divisor
        out_units
    }

    # ── MODE DISPATCH: sequence extraction + alignment ────────────────────────
    # Sets: aln, overall_pid, pm1, pm2, n_src1, n_src2, vis_map1, vis_map2, stride
    aln <- NULL; overall_pid <- NA_real_
    pm1 <- integer(0L); pm2 <- integer(0L)
    n_src1 <- 0L; n_src2 <- 0L
    vis_map1 <- integer(0L); vis_map2 <- integer(0L)
    stride <- 1L   # nucleotides per alignment column (1 for DNA, 3 for protein)

    if (mode %in% c("protein", "cds")) {
        # ── CDS-based modes: extract coding sequences ─────────────────────────
        cds1 <- get_cds_rows(df1); cds2 <- get_cds_rows(df2)
        if (is.null(cds1) || is.null(cds2)) return(NULL)
        cds1 <- if (str1 == "-") cds1[order(-cds1$xstart), , drop = FALSE] else
                                  cds1[order( cds1$xstart), , drop = FALSE]
        cds2 <- if (str2 == "-") cds2[order(-cds2$xstart), , drop = FALSE] else
                                  cds2[order( cds2$xstart), , drop = FALSE]

        seq1 <- tryCatch(extract_spliced_exon_sequence(gp1, sid1, cds1, strand = str1), error = function(e) NULL)
        seq2 <- tryCatch(extract_spliced_exon_sequence(gp2, sid2, cds2, strand = str2), error = function(e) NULL)
        if (is.null(seq1) || is.null(seq2) || !nzchar(seq1) || !nzchar(seq2)) return(NULL)

        seq1 <- trim_to_codon(apply_phase(seq1, cds1))
        seq2 <- trim_to_codon(apply_phase(seq2, cds2))
        if (nchar(seq1) < 3L || nchar(seq2) < 3L) return(NULL)

        if (mode == "protein") {
            # ── Protein alignment (BLOSUM62, NW global) ───────────────────────
            aa1 <- translate_safe(seq1); aa2 <- translate_safe(seq2)
            if (is.null(aa1) || is.null(aa2) || !nzchar(aa1) || !nzchar(aa2)) return(NULL)
            aln <- tryCatch(
                pwalign::pairwiseAlignment(
                    Biostrings::AAString(aa1), Biostrings::AAString(aa2),
                    type = "global", substitutionMatrix = "BLOSUM62",
                    gapOpening = -10, gapExtension = -0.5),
                error = function(e) NULL)
            stride <- 3L  # 1 amino acid = 3 CDS nucleotides
        } else {
            # ── CDS nucleotide alignment (IUPAC-aware DNA NW global) ─────────
            # Uses the same match/mismatch backbone as EDNAFULL (+5 / -4).
            aln <- tryCatch(
                pwalign::pairwiseAlignment(
                    Biostrings::DNAString(toupper(seq1)),
                    Biostrings::DNAString(toupper(seq2)),
                    type = "global", substitutionMatrix = get_aligned_dna_substitution_matrix(),
                    gapOpening = -10, gapExtension = -0.5),
                error = function(e) NULL)
            stride <- 1L  # 1 nucleotide per alignment column
        }
        if (is.null(aln)) return(NULL)
        pm1      <- build_pos_to_exon_map(cds1$xstart, cds1$xend)
        pm2      <- build_pos_to_exon_map(cds2$xstart, cds2$xend)
        n_src1   <- nrow(cds1)
        n_src2   <- nrow(cds2)
        vis_map1 <- map_cds_to_visual(cds1, df1e)
        vis_map2 <- map_cds_to_visual(cds2, df2e)
        feature_units1 <- compute_feature_units_by_visual(cds1, vis_map1, nrow(df1e), aligned_stride = stride)
        feature_units2 <- compute_feature_units_by_visual(cds2, vis_map2, nrow(df2e), aligned_stride = stride)

    } else {
        # ── Exon mode: full spliced exon sequences (CDS + UTRs, DNA NW) ───────
        # Scientific note: useful for studying UTR conservation and regulatory elements.
        # Signal-to-noise decreases for divergent species (> ~100 Mya).
        sort_vis <- function(df, strand) {
            xs <- suppressWarnings(as.numeric(df$xstart))
            if (strand == "-") df[order(-xs), , drop = FALSE] else df[order(xs), , drop = FALSE]
        }
        exs1 <- sort_vis(df1e, str1); exs2 <- sort_vis(df2e, str2)

        seq1 <- tryCatch(extract_spliced_exon_sequence(gp1, sid1, exs1, strand = str1), error = function(e) NULL)
        seq2 <- tryCatch(extract_spliced_exon_sequence(gp2, sid2, exs2, strand = str2), error = function(e) NULL)
        if (is.null(seq1) || is.null(seq2) || !nzchar(seq1) || !nzchar(seq2)) return(NULL)
        if (nchar(seq1) < 10L || nchar(seq2) < 10L) return(NULL)

        aln <- tryCatch(
            pwalign::pairwiseAlignment(
                Biostrings::DNAString(toupper(seq1)),
                Biostrings::DNAString(toupper(seq2)),
                type = "global", substitutionMatrix = get_aligned_dna_substitution_matrix(),
                gapOpening = -10, gapExtension = -0.5),
            error = function(e) NULL)
        if (is.null(aln)) return(NULL)
        stride   <- 1L
        pm1      <- build_pos_to_exon_map(exs1$xstart, exs1$xend)
        pm2      <- build_pos_to_exon_map(exs2$xstart, exs2$xend)
        n_src1   <- nrow(exs1)
        n_src2   <- nrow(exs2)
        vis_map1 <- map_cds_to_visual(exs1, df1e)
        vis_map2 <- map_cds_to_visual(exs2, df2e)
        feature_units1 <- compute_feature_units_by_visual(exs1, vis_map1, nrow(df1e), aligned_stride = stride)
        feature_units2 <- compute_feature_units_by_visual(exs2, vis_map2, nrow(df2e), aligned_stride = stride)
    }

    overall_pid <- tryCatch(pwalign::pid(aln), error = function(e) NA_real_)

    # ── walk alignment columns: accumulate match counts per visual exon pair ──
    aln1_chars <- strsplit(as.character(pwalign::alignedPattern(aln)), "")[[1L]]
    aln2_chars <- strsplit(as.character(pwalign::alignedSubject(aln)),  "")[[1L]]
    n_cols <- length(aln1_chars)

    n_vis1 <- nrow(df1e); n_vis2 <- nrow(df2e)
    M_count <- matrix(0L, nrow = n_vis1, ncol = n_vis2)  # aligned units
    M_match <- matrix(0L, nrow = n_vis1, ncol = n_vis2)  # identical units

    pos1 <- 0L; pos2 <- 0L
    for (col_i in seq_len(n_cols)) {
        ch1 <- aln1_chars[col_i]; ch2 <- aln2_chars[col_i]
        if (ch1 != "-") pos1 <- pos1 + 1L
        if (ch2 != "-") pos2 <- pos2 + 1L
        if (ch1 == "-" || ch2 == "-") next

        nt1 <- (pos1 - 1L) * stride + 1L   # nucleotide position in source sequence
        nt2 <- (pos2 - 1L) * stride + 1L

        cds_i1 <- if (nt1 <= length(pm1)) pm1[nt1] else n_src1
        cds_i2 <- if (nt2 <= length(pm2)) pm2[nt2] else n_src2

        vi1 <- if (cds_i1 >= 1L && cds_i1 <= length(vis_map1)) vis_map1[cds_i1] else NA_integer_
        vi2 <- if (cds_i2 >= 1L && cds_i2 <= length(vis_map2)) vis_map2[cds_i2] else NA_integer_

        if (!is.na(vi1) && !is.na(vi2) &&
                vi1 >= 1L && vi1 <= n_vis1 &&
                vi2 >= 1L && vi2 <= n_vis2) {
            M_count[vi1, vi2] <- M_count[vi1, vi2] + 1L
            if (ch1 == ch2) M_match[vi1, vi2] <- M_match[vi1, vi2] + 1L
        }
    }

    # ── classify correspondences ──────────────────────────────────────────────
    # min_codons threshold in "residues" (amino acids for protein, nucleotides for DNA).
    # For DNA modes, scale up ×3 to match biological unit (15 codons = 45 nucleotides).
    min_units <- as.integer(min_codons) * if (stride == 3L) 1L else 3L
    mc    <- min_units
    M_sig <- M_count >= mc

    events <- list()
    for (i in seq_len(n_vis1)) {
        j_hits <- which(M_sig[i, ])
        if (length(j_hits) == 0L) next
        for (jj in j_hits) {
            i_hits <- which(M_sig[, jj])
            etype <- if      (length(j_hits) == 1L && length(i_hits) == 1L) "1:1"
                     else if (length(j_hits) >  1L && length(i_hits) == 1L) "1:n"
                     else if (length(j_hits) == 1L && length(i_hits) >  1L) "n:1"
                     else                                                     "partial"
            nc      <- M_count[i, jj]
            nm_     <- M_match[i, jj]
            pid_blk <- if (nc > 0L) round(100 * nm_ / nc, 1) else NA_real_
            events[[length(events) + 1L]] <- data.frame(
                vis_exon_A   = i,
                vis_exon_B   = jj,
                n_codons     = nc,
                aligned_units = nc,
                feature_units_A = suppressWarnings(as.numeric(feature_units1[[i]] %||% NA_real_)),
                feature_units_B = suppressWarnings(as.numeric(feature_units2[[jj]] %||% NA_real_)),
                identity_pct = pid_blk,
                overall_pid  = overall_pid,
                event_type   = etype,
                stringsAsFactors = FALSE
            )
        }
    }

    if (length(events) == 0L) return(NULL)
    do.call(rbind,  events)
}

# ─────────────────────────────────────────────────────────────────────────────
# Split ribbon geometry for 1:n and n:1 events
# ─────────────────────────────────────────────────────────────────────────────
# When one exon maps to multiple exons (fusion/fission), we proportionally
# divide the ribbon width on the multi-exon side based on aligned residue counts.
# This converts "one wide ribbon to each B exon" into "each ribbon occupies its
# proportional slice of A's width", making the split visually explicit.
#
# Parameters:
#   ribbon_pairs – data.frame from compute_protein_guided_correspondence()
#   df1e, df2e   – visual exon data frames (sorted by rel_start)
#
# Returns ribbon_pairs with added columns x1L_adj, x1R_adj, x2L_adj, x2R_adj
# (adjusted ribbon edge coordinates for bezier drawing).
add_split_geometry <- function(ribbon_pairs, df1e, df2e) {
    if (is.null(ribbon_pairs) || nrow(ribbon_pairs) == 0L) return(ribbon_pairs)

    ribbon_pairs$x1L_adj <- NA_real_
    ribbon_pairs$x1R_adj <- NA_real_
    ribbon_pairs$x2L_adj <- NA_real_
    ribbon_pairs$x2R_adj <- NA_real_

    # ── split A side (handles 1:n fusion: exon A → multiple exons B) ─────────
    for (ea in unique(ribbon_pairs$vis_exon_A)) {
        rows_a  <- which(ribbon_pairs$vis_exon_A == ea)
        if (ea < 1L || ea > nrow(df1e)) next
        ex      <- df1e[ea, , drop = FALSE]
        x1L_orig <- suppressWarnings(as.numeric(ex$rel_start[1]))
        x1R_orig <- suppressWarnings(as.numeric(ex$rel_end[1]))
        if (!is.finite(x1L_orig) || !is.finite(x1R_orig)) next
        width_a <- x1R_orig - x1L_orig

        feature_units_a <- suppressWarnings(as.numeric(ribbon_pairs$feature_units_A[rows_a][1] %||% NA_real_))
        aligned_units_a <- suppressWarnings(as.numeric(ribbon_pairs$aligned_units[rows_a] %||% ribbon_pairs$n_codons[rows_a]))
        aligned_units_a <- aligned_units_a[is.finite(aligned_units_a) & aligned_units_a > 0]
        occupied_width_a <- width_a
        left_edge_a <- x1L_orig
        if (is.finite(feature_units_a) && feature_units_a > 0 && length(aligned_units_a) > 0L) {
            aligned_total_a <- sum(aligned_units_a, na.rm = TRUE)
            share_a <- max(0, min(1, aligned_total_a / feature_units_a))
            occupied_width_a <- width_a * share_a
            left_edge_a <- x1L_orig
        }

        if (length(rows_a) == 1L) {
            ribbon_pairs$x1L_adj[rows_a] <- left_edge_a
            ribbon_pairs$x1R_adj[rows_a] <- left_edge_a + occupied_width_a
        } else {
            # Proportional split: rows sorted by vis_exon_B (left to right on B)
            rows_a_sorted <- rows_a[order(ribbon_pairs$vis_exon_B[rows_a])]
            nc_vals  <- pmax(as.numeric(ribbon_pairs$aligned_units[rows_a_sorted] %||% ribbon_pairs$n_codons[rows_a_sorted]), 1)
            nc_total <- sum(nc_vals)
            fracs    <- cumsum(nc_vals / nc_total)
            ribbon_pairs$x1L_adj[rows_a_sorted[1]] <- left_edge_a
            ribbon_pairs$x1R_adj[rows_a_sorted[1]] <- left_edge_a + occupied_width_a * fracs[1]
            for (r in seq(2L, length(rows_a_sorted))) {
                ribbon_pairs$x1L_adj[rows_a_sorted[r]] <- left_edge_a + occupied_width_a * fracs[r - 1L]
                ribbon_pairs$x1R_adj[rows_a_sorted[r]] <- left_edge_a + occupied_width_a * fracs[r]
            }
        }
    }

    # ── split B side (handles n:1 fission: multiple exons A → exon B) ────────
    for (eb in unique(ribbon_pairs$vis_exon_B)) {
        rows_b  <- which(ribbon_pairs$vis_exon_B == eb)
        if (eb < 1L || eb > nrow(df2e)) next
        ex      <- df2e[eb, , drop = FALSE]
        x2L_orig <- suppressWarnings(as.numeric(ex$rel_start[1]))
        x2R_orig <- suppressWarnings(as.numeric(ex$rel_end[1]))
        if (!is.finite(x2L_orig) || !is.finite(x2R_orig)) next
        width_b <- x2R_orig - x2L_orig

        feature_units_b <- suppressWarnings(as.numeric(ribbon_pairs$feature_units_B[rows_b][1] %||% NA_real_))
        aligned_units_b <- suppressWarnings(as.numeric(ribbon_pairs$aligned_units[rows_b] %||% ribbon_pairs$n_codons[rows_b]))
        aligned_units_b <- aligned_units_b[is.finite(aligned_units_b) & aligned_units_b > 0]
        occupied_width_b <- width_b
        left_edge_b <- x2L_orig
        if (is.finite(feature_units_b) && feature_units_b > 0 && length(aligned_units_b) > 0L) {
            aligned_total_b <- sum(aligned_units_b, na.rm = TRUE)
            share_b <- max(0, min(1, aligned_total_b / feature_units_b))
            occupied_width_b <- width_b * share_b
            left_edge_b <- x2L_orig
        }

        if (length(rows_b) == 1L) {
            ribbon_pairs$x2L_adj[rows_b] <- left_edge_b
            ribbon_pairs$x2R_adj[rows_b] <- left_edge_b + occupied_width_b
        } else {
            # Proportional split: rows sorted by vis_exon_A (left to right on A)
            rows_b_sorted <- rows_b[order(ribbon_pairs$vis_exon_A[rows_b])]
            nc_vals  <- pmax(as.numeric(ribbon_pairs$aligned_units[rows_b_sorted] %||% ribbon_pairs$n_codons[rows_b_sorted]), 1)
            nc_total <- sum(nc_vals)
            fracs    <- cumsum(nc_vals / nc_total)
            ribbon_pairs$x2L_adj[rows_b_sorted[1]] <- left_edge_b
            ribbon_pairs$x2R_adj[rows_b_sorted[1]] <- left_edge_b + occupied_width_b * fracs[1]
            for (r in seq(2L, length(rows_b_sorted))) {
                ribbon_pairs$x2L_adj[rows_b_sorted[r]] <- left_edge_b + occupied_width_b * fracs[r - 1L]
                ribbon_pairs$x2R_adj[rows_b_sorted[r]] <- left_edge_b + occupied_width_b * fracs[r]
            }
        }
    }

    ribbon_pairs
}

compact_ribbon_span <- function(x_left, x_right, event_type = "1:1", identity_pct = NA_real_, guided = TRUE) {
    xl <- suppressWarnings(as.numeric(x_left))
    xr <- suppressWarnings(as.numeric(x_right))
    if (!is.finite(xl) || !is.finite(xr)) {
        return(c(NA_real_, NA_real_))
    }
    if (xr < xl) {
        tmp <- xl
        xl <- xr
        xr <- tmp
    }
    width <- xr - xl
    if (!is.finite(width) || width <= 0) {
        return(c(xl, xr))
    }

    et <- trimws(as.character(event_type %||% "1:1"))
    frac <- switch(et,
        "1:n" = 0.66,
        "n:1" = 0.66,
        "partial" = 0.44,
        "fallback" = 0.34,
        0.54
    )

    pid <- suppressWarnings(as.numeric(identity_pct))
    if (isTRUE(guided) && is.finite(pid)) {
        frac <- frac + ((pid - 50) / 100) * 0.18
    }
    frac <- max(0.22, min(0.82, frac))
    new_width <- width * frac
    pad <- (width - new_width) / 2
    c(xl + pad, xr - pad)
}

# Build a shared exon-alignment layout across ordered transcript tracks.
# Each exon receives aligned_start/aligned_end coordinates in a common space
# driven by pairwise correspondence between adjacent tracks.
build_aligned_exon_layout <- function(exon_tracks, corr_pairs,
                                      gap_fraction = 0.08,
                                      min_width = 1) {
    if (is.null(exon_tracks) || length(exon_tracks) == 0L) {
        return(list())
    }

    exon_lengths <- function(df) {
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
            return(numeric(0))
        }
        len <- suppressWarnings(as.numeric(df$largo %||% NA_real_))
        if (length(len) != nrow(df)) len <- rep(NA_real_, nrow(df))
        bad <- !is.finite(len) | len <= 0
        if (any(bad)) {
            xs <- suppressWarnings(as.numeric(df$xstart))
            xe <- suppressWarnings(as.numeric(df$xend))
            len[bad] <- abs(xe[bad] - xs[bad]) + 1
        }
        len[!is.finite(len) | len <= 0] <- 1
        len
    }

    all_lengths <- unlist(lapply(exon_tracks, exon_lengths), use.names = FALSE)
    valid_lengths <- all_lengths[is.finite(all_lengths) & all_lengths > 0]
    med_len <- suppressWarnings(stats::median(valid_lengths, na.rm = TRUE))
    if (!is.finite(med_len) || med_len <= 0) med_len <- 100
    gap <- med_len * as.numeric(gap_fraction)
    if (!is.finite(gap) || gap < 1) gap <- 1

    scaled_widths <- function(df) {
        len <- exon_lengths(df)
        widths <- len
        widths[!is.finite(widths)] <- min_width
        pmax(min_width, widths)
    }

    layouts <- vector("list", length(exon_tracks))
    first_df <- exon_tracks[[1L]]
    if (is.null(first_df) || !is.data.frame(first_df) || nrow(first_df) == 0L) {
        return(layouts)
    }
    first_w <- scaled_widths(first_df)
    first_start <- c(0, head(cumsum(first_w + gap), -1L))
    first_end <- first_start + first_w
    first_layout <- first_df
    first_layout$aligned_start <- first_start
    first_layout$aligned_end <- first_end
    first_layout$aligned_width <- first_w
    first_layout$aligned_mid <- (first_start + first_end) / 2
    layouts[[1L]] <- first_layout

    if (length(exon_tracks) == 1L) {
        return(layouts)
    }

    for (track_idx in 2:length(exon_tracks)) {
        prev_layout <- layouts[[track_idx - 1L]]
        cur_df <- exon_tracks[[track_idx]]
        if (is.null(cur_df) || !is.data.frame(cur_df) || nrow(cur_df) == 0L) {
            layouts[[track_idx]] <- cur_df
            next
        }
        cur_w <- scaled_widths(cur_df)
        n_cur <- nrow(cur_df)

        corr <- corr_pairs[[track_idx - 1L]]
        if (is.null(corr) || !is.data.frame(corr) || nrow(corr) == 0L) {
            fallback_start <- c(0, head(cumsum(cur_w + gap), -1L))
            fallback_end <- fallback_start + cur_w
            cur_layout <- cur_df
            cur_layout$aligned_start <- fallback_start
            cur_layout$aligned_end <- fallback_end
            cur_layout$aligned_width <- cur_w
            cur_layout$aligned_mid <- (fallback_start + fallback_end) / 2
            layouts[[track_idx]] <- cur_layout
            next
        }

        corr <- corr[
            is.finite(as.numeric(corr$vis_exon_A)) &
                is.finite(as.numeric(corr$vis_exon_B)),
            , drop = FALSE
        ]
        if (nrow(corr) == 0L) {
            fallback_start <- c(0, head(cumsum(cur_w + gap), -1L))
            fallback_end <- fallback_start + cur_w
            cur_layout <- cur_df
            cur_layout$aligned_start <- fallback_start
            cur_layout$aligned_end <- fallback_end
            cur_layout$aligned_width <- cur_w
            cur_layout$aligned_mid <- (fallback_start + fallback_end) / 2
            layouts[[track_idx]] <- cur_layout
            next
        }

        corr$vis_exon_A <- as.integer(corr$vis_exon_A)
        corr$vis_exon_B <- as.integer(corr$vis_exon_B)
        corr <- corr[
            corr$vis_exon_A >= 1L & corr$vis_exon_A <= nrow(prev_layout) &
                corr$vis_exon_B >= 1L & corr$vis_exon_B <= n_cur,
            , drop = FALSE
        ]
        if (nrow(corr) == 0L) {
            fallback_start <- c(0, head(cumsum(cur_w + gap), -1L))
            fallback_end <- fallback_start + cur_w
            cur_layout <- cur_df
            cur_layout$aligned_start <- fallback_start
            cur_layout$aligned_end <- fallback_end
            cur_layout$aligned_width <- cur_w
            cur_layout$aligned_mid <- (fallback_start + fallback_end) / 2
            layouts[[track_idx]] <- cur_layout
            next
        }

        desired_mid <- rep(NA_real_, n_cur)
        desired_start <- rep(NA_real_, n_cur)
        desired_width <- as.numeric(cur_w)
        anchor_start <- rep(NA_real_, n_cur)
        anchor_end <- rep(NA_real_, n_cur)

        for (cur_idx in seq_len(n_cur)) {
            rows_cur <- which(corr$vis_exon_B == cur_idx)
            if (length(rows_cur) == 0L) next
            prev_idx <- sort(unique(corr$vis_exon_A[rows_cur]))
            target_start <- suppressWarnings(min(prev_layout$aligned_start[prev_idx], na.rm = TRUE))
            target_end <- suppressWarnings(max(prev_layout$aligned_end[prev_idx], na.rm = TRUE))
            if (!is.finite(target_start) || !is.finite(target_end) || target_end <= target_start) next
            anchor_start[cur_idx] <- target_start
            anchor_end[cur_idx] <- target_end
            desired_mid[cur_idx] <- (target_start + target_end) / 2
        }

        shared_key <- ifelse(
            is.finite(anchor_start) & is.finite(anchor_end),
            paste(round(anchor_start, 6), round(anchor_end, 6), sep = "|"),
            paste0("no_anchor_", seq_len(n_cur))
        )
        shared_groups <- split(seq_len(n_cur), shared_key)
        for (grp in shared_groups) {
            if (length(grp) <= 1L) next
            if (!all(is.finite(anchor_start[grp])) || !all(is.finite(anchor_end[grp]))) next
            grp <- grp[order(grp)]
            span_start <- anchor_start[grp[1L]]
            span_end <- anchor_end[grp[1L]]
            span_mid <- (span_start + span_end) / 2
            total_width <- sum(desired_width[grp]) + gap * (length(grp) - 1L)
            run_start <- span_mid - total_width / 2
            for (ii in seq_along(grp)) {
                idx_cur <- grp[ii]
                desired_start[idx_cur] <- run_start
                desired_mid[idx_cur] <- run_start + desired_width[idx_cur] / 2
                run_start <- run_start + desired_width[idx_cur] + gap
            }
        }

        anchored_idx <- which(is.finite(desired_mid))
        if (length(anchored_idx) == 0L) {
            run_start <- 0
            for (cur_idx in seq_len(n_cur)) {
                desired_start[cur_idx] <- run_start
                desired_mid[cur_idx] <- run_start + desired_width[cur_idx] / 2
                run_start <- run_start + desired_width[cur_idx] + gap
            }
        } else {
            first_anchor <- anchored_idx[1L]
            if (first_anchor > 1L) {
                run_mid <- desired_mid[first_anchor]
                for (cur_idx in seq(from = first_anchor - 1L, to = 1L, by = -1L)) {
                    run_mid <- run_mid - (desired_width[cur_idx + 1L] / 2) - gap - (desired_width[cur_idx] / 2)
                    desired_mid[cur_idx] <- run_mid
                }
            }
            if (length(anchored_idx) > 1L) {
                for (ii in seq_len(length(anchored_idx) - 1L)) {
                    left_idx <- anchored_idx[ii]
                    right_idx <- anchored_idx[ii + 1L]
                    if (right_idx - left_idx <= 1L) next
                    mids <- seq(
                        from = desired_mid[left_idx],
                        to = desired_mid[right_idx],
                        length.out = right_idx - left_idx + 1L
                    )
                    for (cur_idx in seq.int(left_idx + 1L, right_idx - 1L)) {
                        desired_mid[cur_idx] <- mids[cur_idx - left_idx + 1L]
                    }
                }
            }
            last_anchor <- anchored_idx[length(anchored_idx)]
            if (last_anchor < n_cur) {
                run_mid <- desired_mid[last_anchor]
                for (cur_idx in seq.int(last_anchor + 1L, n_cur)) {
                    run_mid <- run_mid + (desired_width[cur_idx - 1L] / 2) + gap + (desired_width[cur_idx] / 2)
                    desired_mid[cur_idx] <- run_mid
                }
            }
        }

        desired_start[!is.finite(desired_start)] <- desired_mid[!is.finite(desired_start)] - desired_width[!is.finite(desired_start)] / 2
        if (!is.finite(desired_start[1L])) desired_start[1L] <- 0

        aligned_start <- desired_start
        aligned_end <- aligned_start + desired_width
        for (cur_idx in seq_len(n_cur)) {
            if (cur_idx == 1L) next
            min_start <- aligned_end[cur_idx - 1L] + gap
            if (!is.finite(aligned_start[cur_idx]) || aligned_start[cur_idx] < min_start) {
                aligned_start[cur_idx] <- min_start
                aligned_end[cur_idx] <- aligned_start[cur_idx] + desired_width[cur_idx]
            }
        }
        x_shift <- suppressWarnings(min(aligned_start, na.rm = TRUE))
        if (is.finite(x_shift) && x_shift < 0) {
            aligned_start <- aligned_start - x_shift
            aligned_end <- aligned_end - x_shift
        }

        cur_layout <- cur_df
        cur_layout$aligned_start <- aligned_start
        cur_layout$aligned_end <- aligned_end
        cur_layout$aligned_width <- desired_width
        cur_layout$aligned_mid <- (aligned_start + aligned_end) / 2
        layouts[[track_idx]] <- cur_layout
    }

    layouts
}

# Project arbitrary transcript features into aligned exon space.
# Features overlapping aligned exons are interpolated proportionally within
# the exon's aligned interval while preserving the exon order layout.
project_features_to_aligned_exon_layout <- function(track_df, exon_layout_df) {
    if (is.null(track_df) || !is.data.frame(track_df) || nrow(track_df) == 0L) {
        return(track_df)
    }
    out <- track_df
    if (is.null(exon_layout_df) || !is.data.frame(exon_layout_df) || nrow(exon_layout_df) == 0L) {
        out$disp_start <- suppressWarnings(as.numeric(out$rel_start))
        out$disp_end <- suppressWarnings(as.numeric(out$rel_end))
        return(out)
    }

    ex_xs <- suppressWarnings(as.numeric(exon_layout_df$xstart))
    ex_xe <- suppressWarnings(as.numeric(exon_layout_df$xend))
    ex_ds <- suppressWarnings(as.numeric(exon_layout_df$aligned_start))
    ex_de <- suppressWarnings(as.numeric(exon_layout_df$aligned_end))

    map_feature <- function(xs, xe) {
        x1 <- suppressWarnings(as.numeric(xs))
        x2 <- suppressWarnings(as.numeric(xe))
        if (!is.finite(x1) || !is.finite(x2)) {
            return(c(NA_real_, NA_real_))
        }
        f_start <- min(x1, x2)
        f_end <- max(x1, x2)
        ov_idx <- which(is.finite(ex_xs) & is.finite(ex_xe) & ex_xs <= f_end & ex_xe >= f_start)
        if (length(ov_idx) == 0L) {
            return(c(NA_real_, NA_real_))
        }

        mapped_start <- numeric(0)
        mapped_end <- numeric(0)
        for (ov in ov_idx) {
            ov_start <- max(f_start, ex_xs[ov])
            ov_end <- min(f_end, ex_xe[ov])
            exon_span <- ex_xe[ov] - ex_xs[ov]
            disp_span <- ex_de[ov] - ex_ds[ov]
            if (!is.finite(exon_span) || exon_span <= 0 || !is.finite(disp_span)) {
                mapped_start <- c(mapped_start, ex_ds[ov])
                mapped_end <- c(mapped_end, ex_de[ov])
            } else {
                frac_start <- (ov_start - ex_xs[ov]) / exon_span
                frac_end <- (ov_end - ex_xs[ov]) / exon_span
                mapped_start <- c(mapped_start, ex_ds[ov] + frac_start * disp_span)
                mapped_end <- c(mapped_end, ex_ds[ov] + frac_end * disp_span)
            }
        }
        c(min(mapped_start, na.rm = TRUE), max(mapped_end, na.rm = TRUE))
    }

    mapped <- t(vapply(seq_len(nrow(out)), function(i) map_feature(out$xstart[i], out$xend[i]), numeric(2)))
    out$disp_start <- mapped[, 1]
    out$disp_end <- mapped[, 2]

    fallback <- !is.finite(out$disp_start) | !is.finite(out$disp_end)
    if (any(fallback)) {
        out$disp_start[fallback] <- suppressWarnings(as.numeric(out$rel_start[fallback]))
        out$disp_end[fallback] <- suppressWarnings(as.numeric(out$rel_end[fallback]))
    }
    out$disp_start <- pmin(out$disp_start, out$disp_end)
    out$disp_end <- pmax(out$disp_start, out$disp_end)
    out
}

# Build a pairwise-aligned display without changing exon widths.
# Each track keeps its native rel_start/rel_end span; only a horizontal offset
# is estimated from the exon correspondences against the previous track.
build_pairwise_aligned_offsets <- function(exon_tracks, corr_pairs) {
    n_tracks <- length(exon_tracks %||% list())
    if (n_tracks == 0L) {
        return(list(offsets = numeric(0), layouts = list()))
    }

    offsets <- rep(0, n_tracks)
    layouts <- vector("list", n_tracks)

    make_layout <- function(df, offset = 0) {
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
            return(df)
        }
        out <- df
        rs <- suppressWarnings(as.numeric(out$rel_start))
        re <- suppressWarnings(as.numeric(out$rel_end))
        out$aligned_start <- rs + offset
        out$aligned_end <- re + offset
        out$aligned_width <- abs(out$aligned_end - out$aligned_start)
        out$aligned_mid <- (out$aligned_start + out$aligned_end) / 2
        out
    }

    layouts[[1L]] <- make_layout(exon_tracks[[1L]], 0)

    if (n_tracks == 1L) {
        return(list(offsets = offsets, layouts = layouts))
    }

    for (track_idx in 2:n_tracks) {
        prev_df <- exon_tracks[[track_idx - 1L]]
        cur_df <- exon_tracks[[track_idx]]
        corr <- corr_pairs[[track_idx - 1L]]

        shift_val <- offsets[track_idx - 1L]
        if (!is.null(prev_df) && is.data.frame(prev_df) && nrow(prev_df) > 0L &&
            !is.null(cur_df) && is.data.frame(cur_df) && nrow(cur_df) > 0L &&
            !is.null(corr) && is.data.frame(corr) && nrow(corr) > 0L) {

            corr$vis_exon_A <- suppressWarnings(as.integer(corr$vis_exon_A))
            corr$vis_exon_B <- suppressWarnings(as.integer(corr$vis_exon_B))
            corr <- corr[
                is.finite(corr$vis_exon_A) & is.finite(corr$vis_exon_B) &
                    corr$vis_exon_A >= 1L & corr$vis_exon_A <= nrow(prev_df) &
                    corr$vis_exon_B >= 1L & corr$vis_exon_B <= nrow(cur_df),
                , drop = FALSE
            ]

            if (nrow(corr) > 0L) {
                prev_mid <- (suppressWarnings(as.numeric(prev_df$rel_start)) +
                    suppressWarnings(as.numeric(prev_df$rel_end))) / 2 + offsets[track_idx - 1L]
                cur_mid <- (suppressWarnings(as.numeric(cur_df$rel_start)) +
                    suppressWarnings(as.numeric(cur_df$rel_end))) / 2
                deltas <- prev_mid[corr$vis_exon_A] - cur_mid[corr$vis_exon_B]
                deltas <- deltas[is.finite(deltas)]
                if (length(deltas) > 0L) {
                    shift_val <- stats::median(deltas, na.rm = TRUE)
                }
            }
        }

        offsets[track_idx] <- shift_val
        layouts[[track_idx]] <- make_layout(cur_df, shift_val)
    }

    list(offsets = offsets, layouts = layouts)
}

normalize_aligned_track_for_mode <- function(track_df, mode = "protein") {
    if (is.null(track_df) || !is.data.frame(track_df) || nrow(track_df) == 0L) {
        return(list(track_df = track_df, anchor_df = NULL, geometry_mode = "exon"))
    }

    mode_txt <- tolower(trimws(as.character(mode %||% "protein")))
    if (!mode_txt %in% c("protein", "cds", "exon")) mode_txt <- "protein"

    out <- track_df
    get_num_col <- function(df, primary, fallback = NULL) {
        if (!is.null(primary) && primary %in% names(df)) {
            return(suppressWarnings(as.numeric(df[[primary]])))
        }
        if (!is.null(fallback) && fallback %in% names(df)) {
            return(suppressWarnings(as.numeric(df[[fallback]])))
        }
        rep(NA_real_, nrow(df))
    }
    plot_group <- if ("plot_group" %in% names(out)) {
        tolower(trimws(as.character(out$plot_group)))
    } else if ("feature_type" %in% names(out)) {
        feature_type_norm <- tolower(trimws(as.character(out$feature_type)))
        ifelse(feature_type_norm == "gene", "gene",
            ifelse(feature_type_norm == "exon", "exon",
                ifelse(feature_type_norm == "cds", "cds",
                    ifelse(grepl("utr", feature_type_norm), "utr",
                        ifelse(feature_type_norm %in% c("start_codon", "stop_codon"), "codon", "other")
                    )
                )
            )
        )
    } else {
        rep("other", nrow(out))
    }
    out$plot_group <- plot_group
    wants_cds <- mode_txt %in% c("protein", "cds")
    has_cds <- any(plot_group == "cds", na.rm = TRUE)
    geometry_mode <- if (isTRUE(wants_cds) && isTRUE(has_cds)) "cds" else "exon"

    start_col <- if (identical(geometry_mode, "cds") && "rel_start_cds" %in% names(out)) "rel_start_cds" else
        if ("rel_start_exon" %in% names(out)) "rel_start_exon" else "rel_start"
    end_col <- if (identical(geometry_mode, "cds") && "rel_end_cds" %in% names(out)) "rel_end_cds" else
        if ("rel_end_exon" %in% names(out)) "rel_end_exon" else "rel_end"

    out$rel_start <- get_num_col(out, start_col, "rel_start")
    out$rel_end <- get_num_col(out, end_col, "rel_end")

    fallback <- !is.finite(out$rel_start) | !is.finite(out$rel_end)
    if (any(fallback)) {
        base_start <- get_num_col(out, "rel_start_exon", "rel_start")
        base_end <- get_num_col(out, "rel_end_exon", "rel_end")
        out$rel_start[fallback] <- base_start[fallback]
        out$rel_end[fallback] <- base_end[fallback]
    }
    second_fallback <- !is.finite(out$rel_start) | !is.finite(out$rel_end)
    if (any(second_fallback)) {
        raw_start <- get_num_col(out, "xstart", NULL)
        raw_end <- get_num_col(out, "xend", NULL)
        out$rel_start[second_fallback] <- raw_start[second_fallback]
        out$rel_end[second_fallback] <- raw_end[second_fallback]
    }
    out$rel_start <- pmin(out$rel_start, out$rel_end)
    out$rel_end <- pmax(out$rel_start, out$rel_end)

    anchor_group <- if (identical(geometry_mode, "cds")) "cds" else "exon"
    anchor_df <- out[plot_group == anchor_group, , drop = FALSE]
    if (nrow(anchor_df) == 0L && identical(anchor_group, "cds")) {
        geometry_mode <- "exon"
        out$rel_start <- get_num_col(out, "rel_start_exon", "rel_start")
        out$rel_end <- get_num_col(out, "rel_end_exon", "rel_end")
        anchor_df <- out[plot_group == "exon", , drop = FALSE]
    }
    if (nrow(anchor_df) == 0L) {
        anchor_df <- out[plot_group %in% c("exon", "cds"), , drop = FALSE]
    }
    if (nrow(anchor_df) == 0L) {
        anchor_df <- out[is.finite(out$rel_start) & is.finite(out$rel_end), , drop = FALSE]
    }
    if (nrow(anchor_df) > 0L) {
        anchor_df <- anchor_df[order(anchor_df$rel_start), , drop = FALSE]
    } else {
        anchor_df <- NULL
    }

    out$aligned_geometry_mode <- rep(geometry_mode, nrow(out))
    out$aligned_anchor_label <- rep(if (identical(geometry_mode, "cds")) "CDS block" else "exon", nrow(out))

    list(
        track_df = out,
        anchor_df = anchor_df,
        geometry_mode = geometry_mode
    )
}

project_features_with_track_offset <- function(track_df, x_offset = 0, rel_start_col = "rel_start", rel_end_col = "rel_end") {
    if (is.null(track_df) || !is.data.frame(track_df) || nrow(track_df) == 0L) {
        return(track_df)
    }
    out <- track_df
    rs <- suppressWarnings(as.numeric(out[[rel_start_col]]))
    re <- suppressWarnings(as.numeric(out[[rel_end_col]]))
    out$disp_start <- rs + as.numeric(x_offset %||% 0)
    out$disp_end <- re + as.numeric(x_offset %||% 0)
    out$track_offset <- rep(as.numeric(x_offset %||% 0), nrow(out))
    out
}
