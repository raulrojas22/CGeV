#!/usr/bin/env Rscript

source(file.path("R", "utils.R"))
source(file.path("R", "feedback_delivery.R"))
source(file.path("R", "background_report_jobs.R"))

assert <- function(ok, message) {
    if (!isTRUE(ok)) stop(message, call. = FALSE)
}

test_root <- tempfile("cgv-background-reports-")
dir.create(test_root, recursive = TRUE)
on.exit(unlink(test_root, recursive = TRUE, force = TRUE), add = TRUE)

old_cache <- Sys.getenv("CGV_CACHE_DIR", unset = "")
old_enabled <- Sys.getenv("APP_BACKGROUND_REPORTS_ENABLED", unset = "")
old_global_lastz <- Sys.getenv("APP_LASTZ_GLOBAL_WORKERS", unset = "")
on.exit({
    if (nzchar(old_cache)) Sys.setenv(CGV_CACHE_DIR = old_cache) else Sys.unsetenv("CGV_CACHE_DIR")
    if (nzchar(old_enabled)) Sys.setenv(APP_BACKGROUND_REPORTS_ENABLED = old_enabled) else Sys.unsetenv("APP_BACKGROUND_REPORTS_ENABLED")
    if (nzchar(old_global_lastz)) Sys.setenv(APP_LASTZ_GLOBAL_WORKERS = old_global_lastz) else Sys.unsetenv("APP_LASTZ_GLOBAL_WORKERS")
}, add = TRUE)
Sys.setenv(
    CGV_CACHE_DIR = file.path(test_root, "cache"),
    APP_BACKGROUND_REPORTS_ENABLED = "1",
    APP_LASTZ_GLOBAL_WORKERS = "1"
)

slot_result <- cgv_with_global_lastz_slot(function() 42L, base_dir = test_root)
assert(identical(slot_result, 42L), "global LASTZ semaphore returns the protected result")
slot_root <- file.path(Sys.getenv("CGV_CACHE_DIR"), "lastz_global_slots")
assert(length(list.dirs(slot_root, recursive = FALSE)) == 0L, "global LASTZ slot is released after execution")

if (.Platform$OS.type == "unix") {
    Sys.setenv(APP_LASTZ_GLOBAL_WORKERS = "2")
    concurrent_started <- proc.time()[["elapsed"]]
    concurrent_jobs <- lapply(seq_len(2L), function(job_id) {
        parallel::mcparallel(cgv_with_global_lastz_slot(function() {
            Sys.sleep(1)
            job_id
        }, base_dir = test_root))
    })
    concurrent_results <- unname(unlist(parallel::mccollect(concurrent_jobs), use.names = FALSE))
    concurrent_elapsed <- proc.time()[["elapsed"]] - concurrent_started
    assert(identical(sort(as.integer(concurrent_results)), c(1L, 2L)), "two global LASTZ slots return both protected results")
    assert(concurrent_elapsed < 1.8, "two configured global LASTZ slots execute concurrently")
    assert(length(list.dirs(slot_root, recursive = FALSE)) == 0L, "both global LASTZ slots are released after concurrent execution")
    Sys.setenv(APP_LASTZ_GLOBAL_WORKERS = "1")
}

preloaded_ref <- list(name = "reference.fa", relative_path = "genomes/reference.fa", available = TRUE)
snapshot <- list(
    schema_version = 2L,
    app = list(global_search_query = "TP53"),
    homologous = list(plots = list()),
    orthologous = list(plots = list(list(
        id = "1",
        plot_data = data.frame(feature = "gene"),
        annotation_source = preloaded_ref,
        genome_source = preloaded_ref
    )))
)

queued <- cgv_enqueue_background_report(
    snapshot,
    "scientist@example.org",
    options = list(capture_mode = "complete", run_lastz = TRUE),
    base_dir = test_root
)
assert(grepl("^[a-f0-9]{32}$", queued$id), "queue returns a private job id")
assert(identical(queued$state, "queued"), "new report enters queued state")

claimed <- cgv_claim_next_background_report(test_root)
assert(is.list(claimed) && identical(claimed$job$id, queued$id), "worker claims the queued job")
assert(identical(claimed$job$state, "running"), "claimed job enters running state")
assert(is.null(cgv_claim_next_background_report(test_root)), "a claimed job cannot be claimed twice")

report_result <- list(
    url = "https://cgv.example/share/secret/index.html",
    expires_at = "2026-08-08T12:00:00-0400"
)
ready <- cgv_update_background_report(queued$id, state = "ready", result = report_result, base_dir = test_root)
assert(identical(ready$result$url, report_result$url), "published URL is persisted")

config <- list(
    api_key = "test-key",
    from_email = "CGeV Reports <reports@cgvapp.com>",
    reply_to = "cgvviewer@gmail.com",
    logo_path = "",
    public_url = "https://cgv.example"
)
delivery <- cgv_report_send_email(
    ready,
    success = TRUE,
    config = config,
    transport = function(request) list(status = 200L, body = '{"id":"report_email_123"}')
)
assert(isTRUE(delivery$ok), "result email accepts a successful Resend response")
assert(identical(delivery$provider_id, "report_email_123"), "provider id is retained")
assert(isTRUE(cgv_report_delivery_preflight(config)$ok), "valid report delivery configuration passes preflight")
missing_key_config <- config
missing_key_config$api_key <- ""
assert(identical(cgv_report_delivery_preflight(missing_key_config)$state, "not_configured"), "missing report API key fails preflight")
html <- cgv_report_email_html(ready, success = TRUE, config = config)
assert(grepl(report_result$url, html, fixed = TRUE), "result email contains the secret report URL")
assert(grepl("interactive CGeV report", html, fixed = TRUE), "result email uses branded interactive-report copy")

alignment_ready <- ready
alignment_ready$options <- list(
    request_kind = "alignment",
    alignment_mode = "blocks",
    include_multi_gene = FALSE,
    include_cross_species = TRUE,
    run_lastz = TRUE
)
alignment_html <- cgv_report_email_html(alignment_ready, success = TRUE, config = config)
assert(grepl("complete CGeV alignment report", alignment_html, fixed = TRUE), "alignment delivery has purpose-specific branded copy")
assert(grepl("Cross-Species", alignment_html, fixed = TRUE), "alignment delivery identifies its workflow")
assert(grepl("LASTZ blocks and MultiPIP", alignment_html, fixed = TRUE), "alignment delivery identifies both completed alignment views")
assert(identical(cgv_report_email_subject(alignment_ready), "Your CGeV alignment report is ready"), "alignment delivery uses a clear subject")

empty_env <- function(name, unset = "") unset
default_report_config <- cgv_report_delivery_config(getenv = empty_env)
assert(identical(default_report_config$from_email, "CGeV Reports <reports@cgvapp.com>"), "reports use the verified cgvapp.com sender by default")
assert(identical(default_report_config$reply_to, "cgvviewer@gmail.com"), "report replies return to the CGeV Gmail inbox")

completed <- cgv_update_background_report(queued$id, state = "completed", delivery = delivery, base_dir = test_root)
assert(identical(completed$state, "completed"), "delivered report enters completed state")
cgv_finish_background_report_marker(queued$id, test_root)
assert(!file.exists(claimed$marker), "running marker is removed after completion")

private_snapshot <- snapshot
private_snapshot$orthologous$plots[[1L]]$genome_source <- list(name = "upload.fa", relative_path = "")
private_error <- tryCatch({
    cgv_enqueue_background_report(private_snapshot, "scientist@example.org", base_dir = test_root)
    ""
}, error = function(e) conditionMessage(e))
assert(grepl("preloaded CGeV data", private_error, fixed = TRUE), "private uploads are rejected before queueing")

domain_text <- paste(readLines(file.path("R", "server_shared_analysis_domain.R"), warn = FALSE), collapse = "\n")
worker_text <- paste(readLines(file.path("scripts", "background_report_worker.R"), warn = FALSE), collapse = "\n")
chrome_verifier_text <- paste(readLines(file.path("scripts", "verify_headless_chrome.R"), warn = FALSE), collapse = "\n")
compose_text <- paste(readLines(file.path("deploy", "docker-compose.shinyproxy.yml"), warn = FALSE), collapse = "\n")
shinyproxy_text <- paste(readLines(file.path("deploy", "shinyproxy", "application.yml"), warn = FALSE), collapse = "\n")
colors_deploy_text <- paste(readLines(file.path("deploy", "deploy-colors-shinyproxy.sh"), warn = FALSE), collapse = "\n")
browser_text <- paste(readLines(file.path("www", "js", "reproducible_report.js"), warn = FALSE), collapse = "\n")
server_text <- paste(readLines("server.R", warn = FALSE), collapse = "\n")
dependencies_text <- paste(readLines(file.path("deploy", "docker", "Dockerfile.dependencies"), warn = FALSE), collapse = "\n")
assert(grepl('choiceValues = c("session", "email")', domain_text, fixed = TRUE), "Share exposes foreground and email delivery choices")
for (button_id in c(
    "email_homo_pip_report",
    "email_homo_multipip_report",
    "email_ortho_pip_report",
    "email_ortho_multipip_report"
)) {
    assert(grepl(button_id, domain_text, fixed = TRUE), paste("domain observes", button_id))
    assert(grepl(button_id, server_text, fixed = TRUE), paste("alignment UI exposes", button_id))
}
assert(grepl('request_kind = "alignment"', domain_text, fixed = TRUE), "alignment email requests are labelled for branded delivery")
assert(grepl('run_lastz = TRUE', domain_text, fixed = TRUE), "alignment email requests always run LASTZ in the worker")
assert(!grepl('lastz_contexts <- c(lastz_contexts, "homo")', domain_text, fixed = TRUE), "Multi-Gene reports never schedule LASTZ")
assert(grepl("CGV_BACKGROUND_REPORT_JOB_PATH", worker_text, fixed = TRUE), "worker launches an isolated snapshot renderer")
assert(grepl("resolve_worker_chrome", worker_text, fixed = TRUE), "worker validates its headless browser before accepting jobs")
assert(grepl("chromote::set_chrome_args", worker_text, fixed = TRUE), "worker configures Chrome through the supported chromote API")
assert(grepl('"--no-sandbox"', worker_text, fixed = TRUE), "worker disables Chrome's inner sandbox inside the hardened container")
assert(grepl('file.path(base_dir, "scripts", "verify_headless_chrome.R")', worker_text, fixed = TRUE), "worker runs its Chrome preflight in an isolated short-lived process")
assert(grepl("processx::run", worker_text, fixed = TRUE), "worker waits for and reaps the isolated Chrome verifier")
preflight_text <- sub("^[\\s\\S]*verify_worker_chrome <- function", "verify_worker_chrome <- function", worker_text, perl = TRUE)
preflight_end <- regexpr("\npoll_seconds <-", preflight_text, fixed = TRUE)[[1L]]
assert(preflight_end > 0L, "worker preflight function has a bounded static test region")
preflight_text <- substr(preflight_text, 1L, preflight_end - 1L)
assert(!grepl("ChromoteSession$new", preflight_text, fixed = TRUE), "long-lived worker never starts a persistent chromote event loop during preflight")
assert(grepl("ChromoteSession$new", chrome_verifier_text, fixed = TRUE), "isolated verifier starts a real chromote session")
headless_preflight_call <- regexpr("headless_product <- verify_worker_chrome()", worker_text, fixed = TRUE)[[1L]]
ready_marker_write <- regexpr("writeLines(c(", worker_text, fixed = TRUE)[[1L]]
assert(headless_preflight_call > 0L && ready_marker_write > headless_preflight_call, "worker starts headless Chrome before publishing its ready marker")
assert(grepl('paste0("browser=", headless_product)', worker_text, fixed = TRUE), "worker ready marker records the verified browser product")
assert(grepl("google-chrome-stable_current_amd64.deb", dependencies_text, fixed = TRUE), "dependency image installs a container-native headless browser")
assert(grepl("background-report-worker:", compose_text, fixed = TRUE), "ShinyProxy compose runs the detached report worker")
assert(grepl("init: true", compose_text, fixed = TRUE), "report worker uses an init process to reap browser children")
assert(grepl("worker.ready", compose_text, fixed = TRUE), "report worker health depends on successful delivery preflight")
assert(grepl("FEEDBACK_RESEND_API_KEY", shinyproxy_text, fixed = TRUE), "ShinyProxy sessions inherit the Resend delivery secret")
assert(grepl("REPORT_RESEND_API_KEY", shinyproxy_text, fixed = TRUE), "ShinyProxy sessions inherit an optional report-specific secret")
assert(grepl('CHROMOTE_CHROME: "/usr/bin/google-chrome"', compose_text, fixed = TRUE), "worker uses the packaged Google Chrome binary")
assert(grepl('CGV_PUBLIC_BASE_URL: "${CGV_PUBLIC_BASE_URL:-https://cgev.mobilomics.org}"', compose_text, fixed = TRUE), "worker always publishes an externally reachable report URL")
assert(grepl("cgv-background-report-worker", colors_deploy_text, fixed = TRUE), "Colors deploy starts the detached report worker")
assert(grepl("CGV_PUBLIC_BASE_URL='https://${PUBLIC_HOSTNAME}'", colors_deploy_text, fixed = TRUE), "Colors worker publishes its real public hostname")
assert(grepl("APP_BACKGROUND_REPORTS_ENABLED", colors_deploy_text, fixed = TRUE), "Colors sessions receive the background-report feature flag")
assert(grepl("REPORT_FROM_EMAIL", colors_deploy_text, fixed = TRUE), "Colors worker receives the branded report sender")
assert(grepl("requireNamespace('chromote'", colors_deploy_text, fixed = TRUE), "Colors deploy rebuilds dependencies when the headless runtime is absent")
assert(grepl("/usr/bin/google-chrome", colors_deploy_text, fixed = TRUE), "Colors validates the exact headless browser used by the worker")
assert(grepl("--init", colors_deploy_text, fixed = TRUE), "Colors worker uses an init process to reap browser children")
assert(grepl("APP_LASTZ_GLOBAL_WORKERS='${APP_LASTZ_GLOBAL_WORKERS}'", colors_deploy_text, fixed = TRUE), "Colors worker and public sessions share the configured LASTZ capacity")
assert(grepl('COLORS_INLINE_FAST_SEQUENCE_PREFETCH="${COLORS_INLINE_FAST_SEQUENCE_PREFETCH:-1}"', colors_deploy_text, fixed = TRUE), "Colors keeps the measured inline-prefetch baseline by default")
assert(grepl('SP_INLINE_FAST_SEQUENCE_PREFETCH:${COLORS_INLINE_FAST_SEQUENCE_PREFETCH}', colors_deploy_text, fixed = TRUE), "Colors writes inline-prefetch tuning into ShinyProxy sessions")
for (fixed_profile in c(
    'COLORS_ORTHO_SUSPEND_HIDDEN="1"',
    'COLORS_HOMO_DEFER_SEQUENCE="${COLORS_HOMO_DEFER_SEQUENCE:-0}"',
    'COLORS_ORTHO_DEFER_SEQUENCE="${COLORS_ORTHO_DEFER_SEQUENCE:-0}"',
    'COLORS_FOOTER_DEFER_SEQUENCE="${COLORS_FOOTER_DEFER_SEQUENCE:-0}"',
    'COLORS_DEFER_FEATURE_GC="${COLORS_DEFER_FEATURE_GC:-0}"',
    'COLORS_HOMO_RENDER_CHUNK_SIZE="1"',
    'COLORS_HOMO_AUTO_RENDER_DELAY_MS="120"',
    'COLORS_ORTHO_RENDER_CHUNK_SIZE="1"',
    'COLORS_ORTHO_AUTO_RENDER_MORE="1"',
    'COLORS_ORTHO_AUTO_RENDER_DELAY_MS="120"',
    'COLORS_HOMO_INITIAL_VISIBLE="1"',
    'COLORS_ORTHO_INITIAL_VISIBLE="1"',
    'COLORS_ISOFORM_RENDER_BATCH_SIZE="1"',
    'COLORS_ISOFORM_RENDER_BATCH_DELAY_MS="120"',
    'COLORS_ORTHO_SERVER_RENDER_NUDGE="0"'
)) {
    assert(grepl(fixed_profile, colors_deploy_text, fixed = TRUE), paste("Colors preserves progressive defaults:", fixed_profile))
}
assert(!grepl('SP_HOMO_DEFER_SEQUENCE:', colors_deploy_text, fixed = TRUE), "Colors does not allow stale SP_* values to reactivate deferred sequence work")
assert(grepl('falló la materialización del perfil progresivo en application.yml', colors_deploy_text, fixed = TRUE), "Colors validates progressive profile materialization")
assert(grepl("^browser=Chrome/", colors_deploy_text, fixed = TRUE), "Colors check requires a durable successful headless browser marker")
assert(!grepl("--chmod=F600", colors_deploy_text, fixed = TRUE), "Colors secret sync stays compatible with macOS openrsync")
assert(grepl("--chmod=Fu=rw,Fgo=", colors_deploy_text, fixed = TRUE), "Colors secret sync preserves mode 0600 portably")
assert(grepl("cgv:background-report-bootstrap", browser_text, fixed = TRUE), "headless browser can trigger the existing report capture")
assert(grepl("Queue email report", browser_text, fixed = TRUE), "Share labels the email queue action explicitly")

message("All background report job tests passed.")
