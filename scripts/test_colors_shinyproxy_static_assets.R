#!/usr/bin/env Rscript

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

deploy <- read_text(file.path("deploy", "deploy-colors-shinyproxy.sh"))
deploy_guide <- read_text(file.path("docs", "deployment", "COLORS_SHINYPROXY_DEPLOY.md"))
perf_workflow <- read_text(file.path("docs", "deployment", "FLUJO_LOGS_RENDIMIENTO.md"))

check_pos <- regexpr('if [[ "$MODE" == "check" ]]; then', deploy, fixed = TRUE)[1]
tests_pos <- regexpr('if [[ "$SKIP_TESTS" == "0" ]]; then', deploy, fixed = TRUE)[1]
assert(check_pos > 0 && tests_pos > check_pos,
       "Colors check-mode guard block is missing or out of order.")
tests_end_pos <- regexpr(
  'command -v Rscript >/dev/null 2>&1 || die "Rscript no está disponible para calcular el CSP del reporte"',
  deploy,
  fixed = TRUE
)[1]
assert(tests_end_pos > tests_pos, "Colors local test block has no detectable end.")
tests_text <- substr(deploy, tests_pos, tests_end_pos - 1L)
assert(grepl('command -v node >/dev/null 2>&1 || die "node no está disponible para ejecutar las pruebas JavaScript"', tests_text, fixed = TRUE) &&
         grepl("node tests/js/test_plot_paint_gate.js", tests_text, fixed = TRUE),
       "Colors deploy must require Node and execute the first-paint JavaScript regression in [1/7].")
check_text <- substr(deploy, check_pos, tests_pos - 1L)
check_revision_pos <- regexpr(
  'if [[ "$ACTIVE_IMAGE_REVISION" != "$SOURCE_REV" ]]; then',
  check_text,
  fixed = TRUE
)[1]
check_delegate_pos <- regexpr('CHECK_DELEGATE="$(', check_text, fixed = TRUE)[1]
check_policy_pos <- regexpr('BROKER_POLICY="$(', check_text, fixed = TRUE)[1]
check_application_perf_pos <- regexpr('APPLICATION_PERF_TIMING="$(', check_text, fixed = TRUE)[1]
check_delegate_perf_pos <- regexpr('DELEGATE_PERF_TIMING="$(', check_text, fixed = TRUE)[1]
check_success_pos <- regexpr("CHECK OK:", check_text, fixed = TRUE)[1]
assert(all(c(check_revision_pos, check_delegate_pos, check_policy_pos,
             check_application_perf_pos, check_delegate_perf_pos, check_success_pos) > 0) &&
         check_revision_pos < check_delegate_pos &&
         check_delegate_pos < check_policy_pos &&
         check_policy_pos < check_application_perf_pos &&
         check_application_perf_pos < check_delegate_perf_pos &&
         check_delegate_perf_pos < check_success_pos,
       "Colors check must reject revision drift before delegate and policy validation.")
assert(grepl(
  "podman image inspect '${CURRENT_IMAGE}' --format '{{ index .Labels \\\"org.opencontainers.image.revision\\\" }}'",
  check_text,
  fixed = TRUE
), "Colors check must read the immutable image revision label.")
assert(grepl("Colors saludable pero no ejecuta el commit local", check_text, fixed = TRUE),
       "Colors check must clearly distinguish health from running the local commit.")
assert(grepl("--filter name=sp-container- --filter status=running", check_text, fixed = TRUE) &&
         grepl("[ \\\"\\$candidate_image\\\" = '${CURRENT_IMAGE}' ]", check_text, fixed = TRUE),
       "Colors check must wait for a running delegate carrying the active image exactly.")
assert(grepl('[[ "$BROKER_POLICY" == "0" || "$BROKER_POLICY" == "1" ]]', check_text, fixed = TRUE) &&
         grepl('[[ "$DELEGATE_POLICY" == "0" || "$DELEGATE_POLICY" == "1" ]]', check_text, fixed = TRUE) &&
         grepl('[[ "$DELEGATE_POLICY" == "$BROKER_POLICY" ]]', check_text, fixed = TRUE),
       "Colors check must require valid and identical broker/delegate orthology policy.")
assert(grepl('[[ "$APPLICATION_PERF_TIMING" == "0" || "$APPLICATION_PERF_TIMING" == "1" ]]', check_text, fixed = TRUE) &&
         grepl('[[ "$DELEGATE_PERF_TIMING" == "0" || "$DELEGATE_PERF_TIMING" == "1" ]]', check_text, fixed = TRUE) &&
         grepl('[[ "$DELEGATE_PERF_TIMING" == "$APPLICATION_PERF_TIMING" ]]', check_text, fixed = TRUE),
       "Colors check must report valid telemetry agreed by application.yml and the delegate.")
assert(!grepl("BROKER_PERF_TIMING", check_text, fixed = TRUE) &&
         !grepl("COLORS_PERF_TIMING", check_text, fixed = TRUE),
       "Colors check must inspect effective telemetry without assuming broker env or the local deploy default.")

eager_profile <- c(
  APP_ORTHO_SUSPEND_HIDDEN = "1",
  APP_HOMO_DEFER_SEQUENCE = "0",
  APP_ORTHO_DEFER_SEQUENCE = "0",
  APP_FOOTER_DEFER_SEQUENCE = "0",
  APP_DEFER_FEATURE_GC = "0",
  APP_HOMO_RENDER_CHUNK_SIZE = "1",
  APP_HOMO_AUTO_RENDER_DELAY_MS = "120",
  APP_ORTHO_RENDER_CHUNK_SIZE = "1",
  APP_ORTHO_AUTO_RENDER_MORE = "1",
  APP_ORTHO_AUTO_RENDER_DELAY_MS = "120",
  APP_HOMO_INITIAL_VISIBLE = "1",
  APP_ORTHO_INITIAL_VISIBLE = "1",
  APP_ISOFORM_RENDER_BATCH_SIZE = "1",
  APP_ISOFORM_RENDER_BATCH_DELAY_MS = "120",
  APP_ORTHO_SERVER_RENDER_NUDGE = "0"
)
for (env_key in names(eager_profile)) {
  constant_name <- paste0("COLORS_", sub("^APP_", "", env_key))
  expected_constant <- sprintf('%s="%s"', constant_name, eager_profile[[env_key]])
  if (env_key %in% c("APP_HOMO_DEFER_SEQUENCE", "APP_ORTHO_DEFER_SEQUENCE",
                     "APP_FOOTER_DEFER_SEQUENCE", "APP_DEFER_FEATURE_GC")) {
    expected_constant <- sprintf('%s="${%s:-0}"', constant_name, constant_name)
  }
  assert(grepl(expected_constant, deploy, fixed = TRUE),
         paste("Colors must preserve progressive profile default:", expected_constant))
  expected_check <- sprintf("check_eager_profile_value %s \"$%s\"", env_key, constant_name)
  assert(grepl(expected_check, check_text, fixed = TRUE),
         paste("Colors check must validate effective progressive value:", env_key))
  assert(grepl(paste0("'", env_key, "=${", constant_name, "}'"), deploy, fixed = TRUE),
         paste("Static release guard must validate progressive value:", env_key))
}
assert(grepl('APPLICATION_EAGER_PROFILE="$(', check_text, fixed = TRUE) &&
         grepl('DELEGATE_ENV="$(', check_text, fixed = TRUE),
       "Colors check must compare application.yml and live delegate eager values.")

wait_pos <- regexpr("wait_for_release() {", deploy, fixed = TRUE)[1]
verify_pos <- regexpr("verify_static_release() {", deploy, fixed = TRUE)[1]
rollback_pos <- regexpr("rollback_release() {", deploy, fixed = TRUE)[1]
assert(all(c(wait_pos, verify_pos, rollback_pos) > 0) && wait_pos < verify_pos && verify_pos < rollback_pos,
       "Colors release readiness functions are incomplete or out of order.")
wait_text <- substr(deploy, wait_pos, verify_pos - 1L)
verify_text <- substr(deploy, verify_pos, rollback_pos - 1L)

assert(grepl("--filter name=sp-container- --filter status=running", wait_text, fixed = TRUE),
       "Colors readiness must search for a running ShinyProxy delegate.")
assert(grepl("candidate_image=\\$(podman inspect \\\"\\$candidate\\\" --format '{{.ImageName}}'", wait_text, fixed = TRUE),
       "Colors readiness must inspect the delegate image exactly.")
assert(grepl("[ \\\"\\$candidate_image\\\" = '${expected_image}' ]", wait_text, fixed = TRUE),
       "Colors readiness must require the expected image on the delegate.")
assert(!grepl("podman ps --filter ancestor='${expected_image}' -q | wc -l", wait_text, fixed = TRUE),
       "A background worker must not satisfy public-app readiness.")
assert(grepl("READINESS_GUARD_FAILED:", wait_text, fixed = TRUE),
       "Colors readiness failures must identify the failed guard.")
assert(grepl("for i in \\$(seq 1 15)", verify_text, fixed = TRUE) &&
         grepl("STATIC_GUARD_FAILED: delegate-missing", verify_text, fixed = TRUE),
       "Static verification must coalesce delegate startup and report a missing delegate explicitly.")
assert(grepl('verify_static_release "$STATIC_REVISION" "$NEW_IMAGE"', deploy, fixed = TRUE),
       "Static verification must select a delegate from the new image only.")
assert(grepl("application_perf=\\$(sed -n", verify_text, fixed = TRUE) &&
         grepl("STATIC_GUARD_FAILED: application-perf-timing", verify_text, fixed = TRUE) &&
         grepl("STATIC_GUARD_FAILED: delegate-perf-timing", verify_text, fixed = TRUE) &&
         grepl("STATIC_GUARD_FAILED: perf-timing-mismatch", verify_text, fixed = TRUE) &&
         !grepl("broker_perf=", verify_text, fixed = TRUE),
       "Static verification must compare effective delegate telemetry with active application.yml, not broker env.")

select_expected_delegate <- function(container_names, container_images, expected_image) {
  matches <- container_names[
    startsWith(container_names, "sp-container-") & container_images == expected_image
  ]
  if (length(matches)) matches[[1L]] else NA_character_
}
expected_image_fixture <- "localhost/cgv:release-new"
worker_only_name <- "cgv-background-report-worker"
worker_only_image <- expected_image_fixture
assert(is.na(select_expected_delegate(worker_only_name, worker_only_image, expected_image_fixture)),
       "Regression: a ready worker without a delegate must remain not-ready.")
assert(identical(
  select_expected_delegate(
    c(worker_only_name, "sp-container-old", "sp-container-new"),
    c(worker_only_image, "localhost/cgv:release-old", expected_image_fixture),
    expected_image_fixture
  ),
  "sp-container-new"
), "Regression: readiness must select only the delegate carrying the expected image.")

build_pos <- regexpr("podman build --pull=never", deploy, fixed = TRUE)[1]
digest_pos <- regexpr("podman image inspect --format '{{.Id}}' '${NEW_IMAGE}'", deploy, fixed = TRUE)[1]
publish_pos <- regexpr("CGV_PUBLISH_STATIC_ASSETS=1", deploy, fixed = TRUE)[1]
candidate_pos <- regexpr("build_nginx_static_candidate.py", deploy, fixed = TRUE)[1]
broker_candidate_pos <- regexpr("build_colors_shinyproxy_candidates.py", deploy, fixed = TRUE)[1]
nginx_test_pos <- regexpr("--entrypoint /usr/sbin/nginx", deploy, fixed = TRUE)[1]
compose_syntax_pos <- regexpr("podman-compose -f '${COLORS_COMPOSE_CANDIDATE}' config", deploy, fixed = TRUE)[1]
compose_validate_pos <- regexpr("podman-compose --env-file", deploy, fixed = TRUE)[1]
application_move_pos <- regexpr("mv '${COLORS_APPLICATION_CANDIDATE}' '${APP_DIR}/shinyproxy/application.yml'", deploy, fixed = TRUE)[1]
compose_move_pos <- regexpr("mv '${COLORS_COMPOSE_CANDIDATE}' '${COMPOSE_FILE}'", deploy, fixed = TRUE)[1]
nginx_move_pos <- regexpr("mv '${COLORS_NGINX_CANDIDATE}' '${COLORS_NGINX_CONFIG}'", deploy, fixed = TRUE)[1]
cutover_search_pos <- min(application_move_pos, compose_move_pos, nginx_move_pos)
cutover_teardown_relative <- regexpr(
  "${teardown_stack_command}",
  substring(deploy, cutover_search_pos),
  fixed = TRUE
)[1]
cutover_teardown_pos <- if (cutover_teardown_relative > 0) {
  cutover_search_pos + cutover_teardown_relative - 1
} else {
  -1
}

assert(all(c(build_pos, digest_pos, publish_pos, candidate_pos, nginx_test_pos,
             broker_candidate_pos, compose_validate_pos, application_move_pos,
             compose_syntax_pos, compose_move_pos, nginx_move_pos,
             cutover_teardown_pos) > 0),
       "Colors immutable-static deployment sequence is incomplete.")
assert(build_pos < digest_pos && digest_pos < publish_pos,
       "Colors must derive the exact image Id after build and before publication.")
assert(broker_candidate_pos < compose_syntax_pos && compose_syntax_pos < candidate_pos &&
         candidate_pos < nginx_test_pos &&
         nginx_test_pos < build_pos,
       "Server-owned candidates and nginx -t must complete before the expensive image build.")
assert(nginx_test_pos < nginx_move_pos,
       "The server-owned nginx candidate must pass nginx -t before atomic replacement.")
assert(broker_candidate_pos < compose_validate_pos &&
         compose_validate_pos < application_move_pos &&
         compose_validate_pos < compose_move_pos,
       "ShinyProxy candidates must be built and validated before atomic replacement.")
assert(application_move_pos < cutover_teardown_pos && compose_move_pos < cutover_teardown_pos,
       "Server-owned ShinyProxy candidates must move immediately before cutover.")

assert(grepl('COLORS_PERF_TIMING="${COLORS_PERF_TIMING:-0}"', deploy, fixed = TRUE),
       "Colors performance telemetry must default off.")
assert(grepl('[[ "$COLORS_PERF_TIMING" == "0" || "$COLORS_PERF_TIMING" == "1" ]]', deploy, fixed = TRUE),
       "Colors performance telemetry must reject values other than 0 or 1.")
assert(grepl('APP_PERF_TIMING: \\"${COLORS_PERF_TIMING}\\"', deploy, fixed = TRUE),
       "The server-owned application candidate must receive the selected telemetry mode.")
assert(!grepl('APP_PERF_TIMING: \\"1\\"', deploy, fixed = TRUE),
       "Colors deploy must not force performance telemetry on.")
assert(grepl('Captura:    desactivada', deploy, fixed = TRUE) &&
         grepl('Captura:    activa (${PERF_RUN_LABEL})', deploy, fixed = TRUE) &&
         grepl('Captura:    auditando release activa', deploy, fixed = TRUE),
       "The banner must distinguish normal operation, controlled capture, and check mode.")
assert(grepl("COLORS_PERF_TIMING=1 PERF_RUN_LABEL=despues_colors_01", perf_workflow, fixed = TRUE) &&
         grepl("COLORS_PERF_TIMING=0 ./deploy/deploy-colors-shinyproxy.sh", perf_workflow, fixed = TRUE) &&
         grepl("APP_PERF_TIMING=0", deploy_guide, fixed = TRUE) &&
         grepl("perf=0", deploy_guide, fixed = TRUE),
       "Colors documentation must show explicit capture enablement, shutdown, and effective check state.")

candidate_policy_default_pos <- regexpr(
  "  orthology_policy=0\n  if [ \\\"\\$policy_count\\\" = 1 ]; then",
  deploy,
  fixed = TRUE
)[1]
candidate_policy_arg_pos <- regexpr(
  "--orthology-policy \\\"\\$orthology_policy\\\"",
  deploy,
  fixed = TRUE
)[1]
assert(candidate_policy_default_pos > 0 && candidate_policy_arg_pos > candidate_policy_default_pos,
       "A missing Colors .env policy must resolve to literal 0 before candidate generation.")

cutover_start <- regexpr('echo "[6/7] Cambiando producción', deploy, fixed = TRUE)[1]
assert(cutover_start > 0, "Colors cutover block is missing.")
cutover_text <- substring(deploy, cutover_start)
cutover_policy_default_pos <- regexpr("  orthology_policy=0", cutover_text, fixed = TRUE)[1]
cutover_env_copy_pos <- regexpr("  cp -p .env \\\"\\$env_candidate\\\"", cutover_text, fixed = TRUE)[1]
cutover_policy_append_pos <- regexpr(
  "APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=\\$orthology_policy\\\" >> \\\"\\$env_candidate",
  cutover_text,
  fixed = TRUE
)[1]
cutover_policy_agreement_pos <- regexpr(
  "test \\\"\\$application_policy\\\" = \\\"\\$orthology_policy\\\"",
  cutover_text,
  fixed = TRUE
)[1]
cutover_compose_config_pos <- regexpr("podman-compose --env-file", cutover_text, fixed = TRUE)[1]
cutover_first_move_pos <- regexpr("mv '${COLORS_APPLICATION_CANDIDATE}'", cutover_text, fixed = TRUE)[1]
cutover_teardown_check_pos <- regexpr("${teardown_stack_command}", cutover_text, fixed = TRUE)[1]
assert(all(c(cutover_policy_default_pos, cutover_env_copy_pos, cutover_policy_append_pos,
             cutover_policy_agreement_pos, cutover_compose_config_pos,
             cutover_first_move_pos, cutover_teardown_check_pos) > 0) &&
         cutover_policy_default_pos < cutover_env_copy_pos &&
         cutover_env_copy_pos < cutover_policy_append_pos &&
         cutover_policy_append_pos < cutover_policy_agreement_pos &&
         cutover_policy_agreement_pos < cutover_compose_config_pos &&
         cutover_compose_config_pos < cutover_first_move_pos &&
         cutover_first_move_pos < cutover_teardown_check_pos,
       "Colors must materialize and verify the literal policy before Compose validation and teardown.")

resolve_policy_fixture <- function(env_lines) {
  values <- grep("^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=", env_lines, value = TRUE)
  if (!length(values)) return("0")
  if (length(values) != 1L || !grepl("^[01]$", sub("^[^=]*=", "", values))) return(NA_character_)
  sub("^[^=]*=", "", values)
}
assert(identical(resolve_policy_fixture(c("CGV_IMAGE=release")), "0"),
       "Regression: an absent policy must materialize as 0.")
assert(identical(resolve_policy_fixture(c("APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=1")), "1"),
       "Regression: an explicit strict policy must survive as 1.")
assert(is.na(resolve_policy_fixture(c("APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=2"))),
       "Regression: invalid policy values must fail closed.")

rollback_text <- substring(deploy, rollback_pos, cutover_start - 1L)
rollback_env_pos <- regexpr("cp -p '${BACKUP_DIR}/app.env' .env", rollback_text, fixed = TRUE)[1]
rollback_compose_pos <- regexpr(
  "cp -p '${BACKUP_DIR}/docker-compose.shinyproxy.colors.yml' docker-compose.shinyproxy.colors.yml",
  rollback_text,
  fixed = TRUE
)[1]
rollback_application_pos <- regexpr(
  "cp -p '${BACKUP_DIR}/application.yml' shinyproxy/application.yml",
  rollback_text,
  fixed = TRUE
)[1]
rollback_teardown_pos <- regexpr("${teardown_stack_command}", rollback_text, fixed = TRUE)[1]
rollback_up_pos <- regexpr("podman-compose -f docker-compose.shinyproxy.colors.yml up -d", rollback_text, fixed = TRUE)[1]
assert(all(c(rollback_env_pos, rollback_compose_pos, rollback_application_pos,
             rollback_teardown_pos, rollback_up_pos) > 0) &&
         max(rollback_env_pos, rollback_compose_pos, rollback_application_pos) < rollback_teardown_pos &&
         rollback_teardown_pos < rollback_up_pos,
       "Rollback must restore .env, Compose, and application.yml before restarting the old stack.")

required <- c(
  "CGV_STATIC_REVISION='${STATIC_REVISION}'",
  "APP_ASSET_VERSION=${STATIC_REVISION}",
  "APP_STATIC_BASE_URL=/cgv-static/${STATIC_REVISION}",
  "APP_ASSET_VERSION: \\\"\\${APP_ASSET_VERSION:}\\\"",
  "APP_STATIC_BASE_URL: \\\"\\${APP_STATIC_BASE_URL:}\\\"",
  "APP_ASSET_VERSION: \\\"\\${APP_ASSET_VERSION:-}\\\"",
  "APP_STATIC_BASE_URL: \\\"\\${APP_STATIC_BASE_URL:-}\\\"",
  "COLORS_PERF_TIMING=1",
  "COLORS_PERF_TIMING debe ser 0 o 1",
  "APP_PERF_TIMING: \\\"${COLORS_PERF_TIMING}\\\"",
  "--orthology-policy \\\"\\$orthology_policy\\\"",
  "APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=\\$orthology_policy",
  "application_policy=\\$(sed -n",
  "compose_policy=\\$(sed -n",
  "! grep -Fq '\\${APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY'",
  "COLORS_ENV_CANDIDATE",
  "rm -f '${COLORS_APPLICATION_CANDIDATE}' '${COLORS_COMPOSE_CANDIDATE}' '${COLORS_NGINX_CANDIDATE}' '${COLORS_ENV_CANDIDATE}'",
  "Colors no debe usar container-env-file",
  "no se pudieron generar los candidatos server-owned seguros",
  "falló la preparación del application.yml server-owned",
  "falló la materialización del perfil progresivo en application.yml",
  "los candidatos server-owned no superaron la validación final",
  "no se pudo completar el respaldo previo al deploy",
  "falló el prewarm o la publicación del snapshot estático",
  "el candidato nginx no superó la validación",
  "broker-feedback-secret",
  "POLICY_GUARD_FAILED: env-duplicate-orthology-policy",
  "POLICY_GUARD_FAILED: env-invalid-orthology-policy",
  "STATIC_GUARD_FAILED: broker-orthology-policy",
  "STATIC_GUARD_FAILED: delegate-orthology-policy",
  "STATIC_GUARD_FAILED: orthology-policy-mismatch",
  "STATIC_GUARD_FAILED: application-perf-timing",
  "STATIC_GUARD_FAILED: delegate-perf-timing",
  "STATIC_GUARD_FAILED: perf-timing-mismatch",
  "fail_guard broker-orthology-policy",
  "fail_guard delegate-orthology-policy",
  "fail_guard orthology-policy-mismatch",
  "fail_guard application-perf-timing",
  "fail_guard delegate-perf-timing",
  "fail_guard perf-timing-mismatch",
  "la sesión pública recibió FEEDBACK_RESEND_API_KEY",
  "/srv/cgv-cache",
  "static_assets/manifests/${STATIC_REVISION}.sha256",
  "static_assets/releases/${STATIC_REVISION}",
  "cp -p '${BACKUP_DIR}/app.env' .env",
  "cp -p '${BACKUP_DIR}/cgv-shinyproxy-colors.conf' deploy/nginx/cgv-shinyproxy-colors.conf",
  "verify_static_asset_http.py",
  "APP_STATIC_BASE_URL=/cgv-static/${expected_revision}",
  "REPORT_SCRIPT_CSP_HASH",
  "--report-script-hash '${REPORT_SCRIPT_CSP_HASH}'",
  "--tmpfs /var/cache/nginx:rw,nosuid,nodev,noexec,size=32m,mode=1777",
  "--tmpfs /run:rw,nosuid,nodev,noexec,size=8m,mode=1777",
  "title: CGeV - Comparative Gene Viewer",
  "display-name: CGeV - Comparative Gene Viewer"
)
assert(all(vapply(required, grepl, logical(1), x = deploy, fixed = TRUE)),
       "One or more Colors static-delivery guards are missing.")

assert(!grepl("rm -rf.*static_assets", deploy, perl = TRUE),
       "Colors must retain old immutable releases for rollback.")
assert(!grepl("sed .*docker-compose.shinyproxy.colors", deploy, perl = TRUE),
       "The application deploy must not edit the server-owned Colors compose file in place.")
assert(!grepl(":/opt/shinyproxy/env/cgv.env", deploy, fixed = TRUE),
       "Colors must never mount the full .env file into ShinyProxy.")
assert(!grepl("FEEDBACK_FROM_EMAIL:", deploy, fixed = TRUE),
       "Public app sessions must not receive feedback sender configuration.")
assert(!grepl("REPORT_FROM_EMAIL:", deploy, fixed = TRUE),
       "Public app sessions must not receive report sender configuration.")
assert(!grepl("--tmpfs /var/run:", deploy, fixed = TRUE),
       "The nginx syntax check must mount /run, where nginx.pid is created.")
assert(!grepl("'\\${APP_DIR}/shinyproxy/application.yml'", deploy, fixed = TRUE),
       "Colors final guards must expand APP_DIR instead of sending it literally over SSH.")
assert(!grepl("=\\${COLORS_", deploy, fixed = TRUE),
       "Colors final guards must compare expanded eager-profile values, not literal variable names.")
assert(grepl("s|^        CGV_PUBLIC_BASE_URL:.*|        CGV_PUBLIC_BASE_URL:", deploy, fixed = TRUE),
       "Colors deploy must migrate the server-owned public base URL to the configured hostname.")

message("Colors ShinyProxy immutable-static deployment contract is guarded.")
