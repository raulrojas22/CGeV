# global.R
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggiraph)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(scales)
library(shiny)
library(bslib)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(vroom)
library(future)
library(promises)
library(httr2)
library(visNetwork)

load_project_env_file <- function(path) {
    if (!file.exists(path)) {
        return(invisible(FALSE))
    }

    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    lines <- trimws(lines)
    lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
    if (!length(lines)) {
        return(invisible(TRUE))
    }

    for (line in lines) {
        eq_pos <- regexpr("=", line, fixed = TRUE)[1]
        if (!is.finite(eq_pos) || eq_pos < 2) {
            next
        }

        key <- trimws(substr(line, 1, eq_pos - 1))
        value <- trimws(substr(line, eq_pos + 1, nchar(line)))
        if (!nzchar(key) || nzchar(Sys.getenv(key, unset = ""))) {
            next
        }

        if (
            nchar(value) >= 2 &&
            substr(value, 1, 1) %in% c("\"", "'") &&
            substr(value, nchar(value), nchar(value)) == substr(value, 1, 1)
        ) {
            value <- substr(value, 2, nchar(value) - 1)
        }

        do.call(Sys.setenv, stats::setNames(list(value), key))
    }

    invisible(TRUE)
}

load_project_env_file(".Renviron")
load_project_env_file(".env")

cgv_release_version <- Sys.getenv("CGV_RELEASE_VERSION", unset = "1.1.0")

compute_app_asset_version <- function() {
    env_version <- trimws(Sys.getenv("APP_ASSET_VERSION", unset = ""))
    if (nzchar(env_version)) {
        return(env_version)
    }

    asset_paths <- unique(c(
        "global.R",
        "ui.R",
        "server.R",
        "www/css/custom.scss",
        list.files("www", recursive = TRUE, full.names = TRUE)
    ))
    asset_paths <- asset_paths[file.exists(asset_paths)]

    if (!length(asset_paths)) {
        return(format(Sys.time(), "%Y%m%d%H%M%S"))
    }

    info <- file.info(asset_paths)
    mtimes <- info$mtime
    sizes <- suppressWarnings(as.numeric(info$size))
    latest_mtime <- suppressWarnings(max(mtimes, na.rm = TRUE))
    if (!inherits(latest_mtime, "POSIXct") || is.na(latest_mtime)) {
        latest_mtime <- Sys.time()
    }

    size_tag <- as.integer(sum(sizes, na.rm = TRUE) %% 1000000)
    sprintf("%s-%06d", format(latest_mtime, "%Y%m%d%H%M%S"), size_tag)
}

app_asset_version <- compute_app_asset_version()

# ShinyProxy gives every spawned session a different URL prefix.  When nginx
# publishes the exact /app/www tree for an immutable image, opt selected CGV
# assets into that stable, cross-session namespace.  The strict release check
# deliberately makes this a no-op for Desktop, direct Shiny, and partially
# configured deployments.
compute_app_static_base_url <- function(
    asset_version = app_asset_version,
    candidate = Sys.getenv("APP_STATIC_BASE_URL", unset = "")
) {
    candidate <- trimws(candidate)
    expected <- sprintf("/cgv-static/%s", asset_version)
    if (
        grepl("^/cgv-static/[a-f0-9]{64}$", candidate) &&
        identical(candidate, expected)
    ) {
        candidate
    } else {
        ""
    }
}

app_static_base_url <- compute_app_static_base_url()

static_asset_path <- function(path) {
    path_txt <- as.character(path %||% "")
    if (!length(path_txt) || is.na(path_txt[[1L]])) {
        return("")
    }
    path_txt <- path_txt[[1L]]
    if (!nzchar(path_txt) || !nzchar(app_static_base_url)) {
        return(path_txt)
    }

    unsafe_path <- startsWith(path_txt, "/") ||
        grepl("^[A-Za-z][A-Za-z0-9+.-]*:", path_txt) ||
        grepl("\\\\", path_txt, perl = TRUE) ||
        grepl("%2[eEfF]|%5[cC]", path_txt, perl = TRUE) ||
        grepl("[\r\n]", path_txt, perl = TRUE) ||
        grepl("(^|[/\\\\])[.][.]([/\\\\]|$)", path_txt, perl = TRUE) ||
        grepl("[/\\\\]$", path_txt, perl = TRUE)
    if (unsafe_path) {
        return(path_txt)
    }

    paste0(app_static_base_url, "/", path_txt)
}

# Expose the current build identifier through a tiny static JSON resource.
# The browser version probe only needs this value; fetching and parsing the
# complete Shiny page adds avoidable network, HTML parsing, and UI construction
# work.  A process-local directory also keeps concurrent app versions isolated
# during rolling deployments.
.cgv_runtime_metadata_dir <- file.path(
    tempdir(),
    sprintf("cgv-runtime-metadata-%s", Sys.getpid())
)
dir.create(.cgv_runtime_metadata_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(
    jsonlite::toJSON(
        list(
            version = app_asset_version,
            release = cgv_release_version
        ),
        auto_unbox = TRUE
    ),
    file.path(.cgv_runtime_metadata_dir, "version.json"),
    useBytes = TRUE
)
if ("cgv-meta" %in% names(shiny::resourcePaths())) {
    shiny::removeResourcePath("cgv-meta")
}
shiny::addResourcePath("cgv-meta", .cgv_runtime_metadata_dir)

versioned_asset_path <- function(path) {
    path_txt <- as.character(path %||% "")
    if (!nzchar(path_txt)) {
        return("")
    }
    path_txt <- static_asset_path(path_txt)
    sep <- if (grepl("?", path_txt, fixed = TRUE)) "&" else "?"
    paste0(path_txt, sep, "av=", app_asset_version)
}

# Debug logs disabled by default; enable only when needed.
if (is.na(Sys.getenv("APP_DEBUG_LOGS", unset = NA_character_))) {
    Sys.setenv(APP_DEBUG_LOGS = "0")
}
if (is.na(Sys.getenv("APP_PERF_TIMING", unset = NA_character_))) {
    Sys.setenv(APP_PERF_TIMING = "0")
}
if (is.na(Sys.getenv("APP_FUTURE_MODE", unset = NA_character_))) {
    Sys.setenv(APP_FUTURE_MODE = "multisession")
}
if (is.na(Sys.getenv("APP_SHINY_JSON_DIGITS", unset = NA_character_))) {
    Sys.setenv(APP_SHINY_JSON_DIGITS = "12")
}
if (is.na(Sys.getenv("APP_GIRAFE_COMPACT_SVG", unset = NA_character_))) {
    Sys.setenv(APP_GIRAFE_COMPACT_SVG = "1")
}
if (is.na(Sys.getenv("APP_GIRAFE_SVG_DECIMALS", unset = NA_character_))) {
    Sys.setenv(APP_GIRAFE_SVG_DECIMALS = "2")
}
if (is.na(Sys.getenv("APP_ALIGNED_RIBBON_POINTS", unset = NA_character_))) {
    Sys.setenv(APP_ALIGNED_RIBBON_POINTS = "25")
}
if (is.na(Sys.getenv("APP_MULTIPIP_VISUAL_MERGE_FROM", unset = NA_character_))) {
    Sys.setenv(APP_MULTIPIP_VISUAL_MERGE_FROM = "350")
}

configure_app_transport_options <- function() {
    options(shiny.minified = TRUE)

    raw_digits <- tolower(trimws(as.character(Sys.getenv("APP_SHINY_JSON_DIGITS", "12") %||% "12")))
    if (raw_digits %in% c("na", "max", "full", "none")) {
        options(shiny.json.digits = NA)
        return(invisible(NULL))
    }

    digits <- suppressWarnings(as.integer(raw_digits))
    if (!is.finite(digits) || is.na(digits) || digits < 7L) {
        digits <- 12L
    }
    if (digits > 16L) {
        digits <- 16L
    }
    options(shiny.json.digits = I(digits))
    invisible(NULL)
}

configure_app_transport_options()

# ── Helper: info-tooltip for chart containers ────────────────────────
chart_info_tip <- function(body_html) {
    tags$div(
        class = "chart-info-tip",
        tags$i(class = "fa-solid fa-circle-info"),
        tags$div(class = "chart-info-content", HTML(body_html))
    )
}

# ── Helper: SVG export button for chart containers ─────────────────────
chart_export_svg_btn <- function(container_id, filename, label = "SVG") {
    tags$div(
        class = "chart-export-tip",
        tags$button(
            type = "button",
            class = "btn btn-sm btn-export-svg chart-export-btn",
            onclick = sprintf("exportSVG('%s', '%s');", container_id, filename),
            icon("download"),
            label
        )
    )
}

# Configurar plan para future (asincronía) con modo acotado y configurable.
configure_app_future_plan <- function() {
    mode <- tolower(trimws(as.character(Sys.getenv("APP_FUTURE_MODE", "multisession") %||% "multisession")))
    if (!mode %in% c("sequential", "multisession")) {
        mode <- "multisession"
    }
    Sys.setenv(APP_FUTURE_MODE = mode)

    if (identical(mode, "multisession")) {
        detected <- suppressWarnings(as.integer(parallel::detectCores(logical = FALSE)))
        if (!is.finite(detected) || is.na(detected) || detected < 1L) {
            detected <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
        }
        available <- tryCatch(
            {
                if (requireNamespace("parallelly", quietly = TRUE)) {
                    suppressWarnings(as.integer(parallelly::availableCores(omit = 0L)[1L]))
                } else {
                    detected
                }
            },
            error = function(e) detected
        )
        if (!is.finite(available) || is.na(available) || available < 1L) {
            available <- detected
        }
        if (!is.finite(detected) || is.na(detected) || detected < 1L) {
            detected <- available
        }
        if (!is.finite(available) || is.na(available) || available < 1L) {
            available <- 2L
        }
        default_workers <- max(1L, min(2L, available))
        raw_workers <- trimws(as.character(Sys.getenv("APP_FUTURE_WORKERS", unset = "") %||% ""))
        has_explicit_workers <- nzchar(raw_workers)
        if (!has_explicit_workers) {
            raw_workers <- as.character(default_workers)
        }
        workers <- suppressWarnings(as.integer(raw_workers))
        if (!is.finite(workers) || is.na(workers) || workers < 1L) {
            workers <- default_workers
        }
        # Respect an explicit APP_FUTURE_WORKERS value. In some desktop/container
        # launches parallelly::availableCores() reports 1 despite the host having
        # more cores, which silently collapses Cross-Species lookups to a single
        # worker.
        max_workers <- if (isTRUE(has_explicit_workers)) {
            max(1L, workers)
        } else {
            max(1L, available)
        }
        if (workers > max_workers) {
            message(sprintf(
                "[Future] Capping APP_FUTURE_WORKERS from %d to %d available core(s).",
                workers,
                max_workers
            ))
            workers <- max_workers
        }
        if (isTRUE(has_explicit_workers) && workers > available) {
            options(parallelly.maxWorkers.localhost = Inf)
            message(sprintf(
                "[Future] Respecting explicit APP_FUTURE_WORKERS=%d although availableCores() reported %d.",
                workers,
                available
            ))
        }
        workers_arg <- if (identical(as.integer(workers), 1L)) I(as.integer(workers)) else as.integer(workers)
        # APP_MEMORY_CACHE_BUDGET_MB is a total web/container allowance. Set the
        # effective main + worker process count before workers are created so the
        # same value is inherited by every persistent multisession process.
        Sys.setenv(APP_MEMORY_CACHE_PROCESS_COUNT = as.character(workers + 1L))
        tryCatch(
            future::plan(future::multisession, workers = workers_arg),
            error = function(e) {
                fallback_workers <- max(1L, min(default_workers, available))
                message(sprintf(
                    "[Future] Could not start %d multisession worker(s): %s. Falling back to %d worker(s).",
                    workers,
                    as.character(e$message %||% "unknown error"),
                    fallback_workers
                ))
                fallback_arg <- if (identical(as.integer(fallback_workers), 1L)) I(as.integer(fallback_workers)) else as.integer(fallback_workers)
                Sys.setenv(APP_MEMORY_CACHE_PROCESS_COUNT = as.character(fallback_workers + 1L))
                future::plan(future::multisession, workers = fallback_arg)
            }
        )
    } else {
        Sys.setenv(APP_MEMORY_CACHE_PROCESS_COUNT = "1")
        future::plan(future::sequential)
    }

    invisible(NULL)
}

configure_app_future_plan()

# Suppress harmless base-R serialization warnings that fire when future's
# serializedSize() encounters objects referencing an attached package namespace.
# visNetwork IS loaded in every session; the warning is purely precautionary.
globalCallingHandlers(
    warning = function(w) {
        if (grepl("may not be available when loading", conditionMessage(w),
                  fixed = TRUE)) {
            invokeRestart("muffleWarning")
        }
    }
)

# Tamaño máximo de archivo 500MB
options(shiny.maxRequestSize = 500 * 1024^2)

# ==============================================================================
# SOLUCIÓN AL ERROR DE FUTURE: AISLAMIENTO DE ENTORNO
# ==============================================================================
# Creamos un entorno limpio que hereda del entorno de paquetes (search path).
# Esto permite que las funciones vean las librerías cargadas, pero NO vean
# los datos pesados del GlobalEnv de la app.
sys.source("R/compiled_runtime.R", envir = environment())
lib_env <- new.env(parent = parent.env(.GlobalEnv))

# Cargamos los scripts DENTRO de este entorno limpio usando sys.source
if (file.exists("R/alias_resolution.R")) {
    cgv_source_runtime("R/alias_resolution.R", envir = lib_env)
}
cgv_source_runtime("R/utils.R", envir = lib_env)
cgv_source_runtime("R/shared_gene_cache.R", envir = lib_env)
cgv_source_runtime("R/feedback_delivery.R", envir = lib_env)
cgv_source_runtime("R/background_report_jobs.R", envir = lib_env)
cgv_source_runtime("R/modules.R", envir = lib_env)
cgv_source_runtime("R/server_cache_warm.R", envir = lib_env)
cgv_source_runtime("R/server_go_domain.R", envir = lib_env)
cgv_source_runtime("R/string_cache.R", envir = lib_env)
cgv_source_runtime("R/string_annotation.R", envir = lib_env)
cgv_source_runtime("R/string_worker.R", envir = lib_env)
cgv_source_runtime("R/server_analytics_domain.R", envir = lib_env)
cgv_source_runtime("R/server_autocomplete_domain.R", envir = lib_env)
cgv_source_runtime("R/server_plot_lifecycle_domain.R", envir = lib_env)
cgv_source_runtime("R/server_state_helpers_domain.R", envir = lib_env)
cgv_source_runtime("R/server_source_history_domain.R", envir = lib_env)
cgv_source_runtime("R/server_popup_status_domain.R", envir = lib_env)
cgv_source_runtime("R/server_session_snapshot_domain.R", envir = lib_env)
cgv_source_runtime("R/server_shared_analysis_domain.R", envir = lib_env)
cgv_source_runtime("R/server_ncbi_download_domain.R", envir = lib_env)

# Cargamos la librería de búsqueda optimizada si existe
if (file.exists("R/gene_search_lib.R")) {
    cgv_source_runtime("R/gene_search_lib.R", envir = lib_env)
} else if (file.exists("../gene_aliases.R")) {
    # Soporte legacy por si acaso
    cgv_source_runtime("../gene_aliases.R", envir = lib_env)
}

# Limpiamos si ya estaba adjunto (para evitar duplicados al recargar en RStudio)
# The sharing domain needs the app's asset URL helper. Export the closure
# explicitly; its global environment retains the current release settings.
lib_env$versioned_asset_path <- versioned_asset_path

if ("app_libraries" %in% search()) {
    detach("app_libraries")
}

# Adjuntamos el entorno al search path.
# Ahora la UI y el Server pueden encontrar las funciones (plotUIHomologous, etc.),
# pero cuando 'future' las exporte, solo exportará este entorno ligero (<1MB)
# en lugar de los 870MB del entorno global.
attach(lib_env, name = "app_libraries")

# Direct/native Shiny deployments can serve the same immutable report files
# without starting a second Shiny session. ShinyProxy production still serves
# this prefix directly from Nginx so report reads do not consume containers.
.cgv_shared_reports_path <- cgv_reports_root(".")
dir.create(.cgv_shared_reports_path, recursive = TRUE, showWarnings = FALSE)
if ("share" %in% names(shiny::resourcePaths())) {
    shiny::removeResourcePath("share")
}
shiny::addResourcePath("share", .cgv_shared_reports_path)
rm(.cgv_shared_reports_path)

# Tema personalizado mejorado
theme_custom <- bs_theme(
    bg = "#F8F9FA", # Fondo de la aplicación
    fg = "#343A40", # Color de texto
    primary = "#2C3E50", # Color del encabezado principal
    secondary = "#6C757D", # Color secundario
    base_font = font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
    "navbar-bg" = "#2C3E50", # Color de fondo de la barra de navegación
    "navbar-light-brand-color" = "#FFFFFF", # Color del texto del título
    "navbar-light-color" = "#FFFFFF", # Color de los enlaces
    "navbar-light-hover-color" = "#18BC9C", # Color hover de los enlaces
    "navbar-light-active-color" = "#18BC9C", # Color activo de los enlaces
    # Personalización adicional para botones y elementos interactivos
    "btn-primary" = "#007BFF",
    "btn-success" = "#28a745",
    "input-border-color" = "#ced4da",
    "border-radius" = ".25rem"
)

# Cargar CSS pre-compilado si existe y está al día; si no, compilar SCSS en caliente.
.cgv_compiled_css_path <- file.path("www", "css", "cgv_compiled.css")
.cgv_scss_path <- file.path("www", "css", "custom.scss")
.cgv_use_compiled <- FALSE
if (file.exists(.cgv_compiled_css_path) && file.exists(.cgv_scss_path)) {
    .cgv_css_mtime <- file.info(.cgv_compiled_css_path)$mtime
    .cgv_scss_mtime <- file.info(.cgv_scss_path)$mtime
    if (!is.na(.cgv_css_mtime) && !is.na(.cgv_scss_mtime) && .cgv_css_mtime >= .cgv_scss_mtime) {
        .cgv_use_compiled <- TRUE
    }
}
if (.cgv_use_compiled) {
    theme_custom <- theme_custom %>% bs_add_rules(readLines(.cgv_compiled_css_path, warn = FALSE))
} else {
    theme_custom <- theme_custom %>% bs_add_rules(sass::sass_file(.cgv_scss_path))
}
rm(.cgv_compiled_css_path, .cgv_scss_path, .cgv_use_compiled, .cgv_css_mtime, .cgv_scss_mtime)
