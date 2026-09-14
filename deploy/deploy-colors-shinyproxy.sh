#!/usr/bin/env bash
# Safe CGV release deployment for Colors.
#
# This script preserves ShinyProxy, the socket proxy, the server-owned Compose
# file, and persistent biological data. It builds a versioned CGV image and
# prepares one canonical marked location in the backed-up server-owned nginx
# config via candidate + nginx -t + atomic rename before switching releases.
set -euo pipefail

DEPLOY_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_DIR="$(cd -- "${DEPLOY_DIR}/.." && pwd)"
REMOTE_TARGET="${REMOTE_TARGET:-colors}"
REMOTE_PATH="${REMOTE_PATH:-/home/rarojas/cgv}"
APP_DIR="${REMOTE_PATH}/app"
PUBLIC_HOSTNAME="${PUBLIC_HOSTNAME:-cgev.mobilomics.org}"
COMPOSE_FILE="${APP_DIR}/docker-compose.shinyproxy.colors.yml"
COLORS_NGINX_CONFIG="${APP_DIR}/deploy/nginx/cgv-shinyproxy-colors.conf"
BACKGROUND_WORKER_NAME="cgv-background-report-worker"
COLORS_CONTAINER_MEMORY="${COLORS_CONTAINER_MEMORY:-5g}"
BACKGROUND_REPORT_MEMORY="${BACKGROUND_REPORT_MEMORY:-4g}"
APP_LASTZ_GLOBAL_WORKERS="${APP_LASTZ_GLOBAL_WORKERS:-2}"
COLORS_INLINE_FAST_SEQUENCE_PREFETCH="${COLORS_INLINE_FAST_SEQUENCE_PREFETCH:-1}"
# Colors intentionally uses one fixed progressive rendering profile. These values are
# materialized as literals below so stale SP_* or server-owned values cannot
# silently reactivate bulk card rendering.
COLORS_ORTHO_SUSPEND_HIDDEN="1"
COLORS_HOMO_DEFER_SEQUENCE="0"
COLORS_ORTHO_DEFER_SEQUENCE="0"
COLORS_FOOTER_DEFER_SEQUENCE="0"
COLORS_DEFER_FEATURE_GC="0"
COLORS_HOMO_RENDER_CHUNK_SIZE="1"
COLORS_HOMO_AUTO_RENDER_DELAY_MS="120"
COLORS_ORTHO_RENDER_CHUNK_SIZE="1"
COLORS_ORTHO_AUTO_RENDER_MORE="1"
COLORS_ORTHO_AUTO_RENDER_DELAY_MS="120"
COLORS_HOMO_INITIAL_VISIBLE="1"
COLORS_ORTHO_INITIAL_VISIBLE="1"
COLORS_ISOFORM_RENDER_BATCH_SIZE="1"
COLORS_ISOFORM_RENDER_BATCH_DELAY_MS="120"
COLORS_ORTHO_SERVER_RENDER_NUDGE="0"
LOCAL_EMAIL_ENV="${SCRIPT_DIR}/.env.local"
REMOTE_EMAIL_ENV="${APP_DIR}/.env.background-reports"
EMAIL_ENV_STAGING=""
COLORS_APPLICATION_CANDIDATE=""
COLORS_COMPOSE_CANDIDATE=""
COLORS_NGINX_CANDIDATE=""
COLORS_ENV_CANDIDATE=""
CGV_DEPS_IMAGE="${CGV_DEPS_IMAGE:-cgv-deps:1.0.0}"
SHINYPROXY_IMAGE="${SHINYPROXY_IMAGE:-docker.io/openanalytics/shinyproxy:3.2.4@sha256:281dfddd3c8c54ea2dfa74390480d0f7769b53fd0bbef6d57f272574fd10fa3c}"
SHINYPROXY_DIGEST="${SHINYPROXY_IMAGE##*@}"
SHINYPROXY_RUNTIME_IMAGE="docker.io/openanalytics/shinyproxy@${SHINYPROXY_DIGEST}"
REBUILD_R_DEPS="${REBUILD_R_DEPS:-0}"
COLORS_PERF_TIMING="${COLORS_PERF_TIMING:-0}"
PERF_RUN_LABEL="${PERF_RUN_LABEL:-manual}"
MODE="deploy"
SKIP_TESTS=0
SSH_SOCK="/tmp/cgv-colors-deploy-$$.sock"

usage() {
  cat <<'USAGE'
Uso:
  ./deploy/deploy-colors-shinyproxy.sh [--check] [--skip-tests]

Opciones:
  --check       Audita Git y la infraestructura remota sin cambiar Colors.
  --skip-tests  Omite las pruebas R. Úsalo sólo ante una emergencia conocida.
  -h, --help    Muestra esta ayuda.

Variables opcionales:
  REMOTE_TARGET=colors
  REMOTE_PATH=/home/rarojas/cgv
  CGV_DEPS_IMAGE=cgv-deps:1.0.0
  REBUILD_R_DEPS=1   Reconstruye explícitamente la base de dependencias R.
  COLORS_CONTAINER_MEMORY=5g  Límite por contenedor; medir pico + margen antes de cambiarlo.
  BACKGROUND_REPORT_MEMORY=4g
  APP_LASTZ_GLOBAL_WORKERS=2
  COLORS_INLINE_FAST_SEQUENCE_PREFETCH=1
  COLORS_PERF_TIMING=1  Activa una captura de telemetría controlada (por defecto: 0).
  PERF_RUN_LABEL=antes_colors_01  Etiqueta usada cuando COLORS_PERF_TIMING=1.

Perfil de render progresivo fijo en Colors:
  hidden=1, homo-seq=0, ortho-seq=0, footer-seq=0, gc=0,
  homo/ortho chunk=1, auto-render=1, auto-delay=120ms,
  homo/ortho-initial=1, isoform batch=1/120ms,
  server-render-nudge=0.

El deploy normal exige que Git esté limpio y crea una imagen inmutable con
la forma localhost/cgv:release-<commit>-<fecha UTC>.
USAGE
}

die() {
  echo "ERROR: $*" >&2
  exit 1
}

for arg in "$@"; do
  case "$arg" in
    --check) MODE="check" ;;
    --skip-tests) SKIP_TESTS=1 ;;
    -h|--help) usage; exit 0 ;;
    *) die "opción desconocida: ${arg}" ;;
  esac
done

[[ "$REBUILD_R_DEPS" == "0" || "$REBUILD_R_DEPS" == "1" ]] || \
  die "REBUILD_R_DEPS debe ser 0 o 1"
[[ "$COLORS_PERF_TIMING" == "0" || "$COLORS_PERF_TIMING" == "1" ]] || \
  die "COLORS_PERF_TIMING debe ser 0 o 1"
for tuning_value in \
  "$COLORS_INLINE_FAST_SEQUENCE_PREFETCH" \
  "$COLORS_ORTHO_SUSPEND_HIDDEN" \
  "$COLORS_HOMO_DEFER_SEQUENCE" \
  "$COLORS_ORTHO_DEFER_SEQUENCE" \
  "$COLORS_FOOTER_DEFER_SEQUENCE" \
  "$COLORS_ORTHO_AUTO_RENDER_MORE" \
  "$COLORS_ORTHO_SERVER_RENDER_NUDGE" \
  "$COLORS_DEFER_FEATURE_GC"; do
  [[ "$tuning_value" == "0" || "$tuning_value" == "1" ]] || \
    die "Los flags del perfil de render de Colors deben ser 0 o 1"
done
[[ "$COLORS_HOMO_AUTO_RENDER_DELAY_MS" == "120" && "$COLORS_ORTHO_AUTO_RENDER_DELAY_MS" == "120" && "$COLORS_ISOFORM_RENDER_BATCH_DELAY_MS" == "120" ]] || \
  die "El perfil progresivo de Colors exige delays de 120 ms"
[[ "$COLORS_HOMO_RENDER_CHUNK_SIZE" == "1" && "$COLORS_ORTHO_RENDER_CHUNK_SIZE" == "1" && "$COLORS_HOMO_INITIAL_VISIBLE" == "1" && "$COLORS_ORTHO_INITIAL_VISIBLE" == "1" && "$COLORS_ISOFORM_RENDER_BATCH_SIZE" == "1" ]] || \
  die "El perfil progresivo de Colors exige lotes e initial-visible de 1"
[[ "$PERF_RUN_LABEL" =~ ^[A-Za-z0-9._-]+$ ]] || \
  die "PERF_RUN_LABEL solo puede contener letras, numeros, punto, guion y guion bajo"
[[ "$COLORS_CONTAINER_MEMORY" =~ ^[1-9][0-9]*[mMgG]$ ]] || \
  die "COLORS_CONTAINER_MEMORY debe usar un valor como 2048m o 2g"
case "$COLORS_CONTAINER_MEMORY" in
  *[gG]) COLORS_CONTAINER_MEMORY_BYTES=$(( ${COLORS_CONTAINER_MEMORY%?} * 1024 * 1024 * 1024 )) ;;
  *[mM]) COLORS_CONTAINER_MEMORY_BYTES=$(( ${COLORS_CONTAINER_MEMORY%?} * 1024 * 1024 )) ;;
esac
[[ "$BACKGROUND_REPORT_MEMORY" =~ ^[1-9][0-9]*[mMgG]$ ]] || \
  die "BACKGROUND_REPORT_MEMORY debe usar un valor como 2048m o 4g"
[[ "$APP_LASTZ_GLOBAL_WORKERS" =~ ^[1-9][0-9]*$ ]] && \
  (( APP_LASTZ_GLOBAL_WORKERS <= 16 )) || \
  die "APP_LASTZ_GLOBAL_WORKERS debe ser un entero entre 1 y 16"
[[ "$PUBLIC_HOSTNAME" =~ ^[A-Za-z0-9.-]+$ ]] || \
  die "PUBLIC_HOSTNAME no es válido"

for command_name in curl git ssh rsync shasum python3; do
  command -v "$command_name" >/dev/null 2>&1 || die "falta el comando local '${command_name}'"
done

[[ -f "$LOCAL_EMAIL_ENV" ]] || \
  die "falta ${LOCAL_EMAIL_ENV}; se necesita para enviar reportes desde Colors"
for email_key in FEEDBACK_RESEND_API_KEY FEEDBACK_TO_EMAIL FEEDBACK_FROM_EMAIL; do
  grep -Eq "^${email_key}=.+$" "$LOCAL_EMAIL_ENV" || \
    die "falta ${email_key} en ${LOCAL_EMAIL_ENV}"
done

git -C "$SCRIPT_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1 || \
  die "${SCRIPT_DIR} no es un repositorio Git"

SOURCE_REV="$(git -C "$SCRIPT_DIR" rev-parse --short=12 HEAD)"
SOURCE_BRANCH="$(git -C "$SCRIPT_DIR" branch --show-current)"
SOURCE_BRANCH="${SOURCE_BRANCH:-detached}"
DEPLOY_CHANGES="$(
  git -C "$SCRIPT_DIR" status --porcelain=v1 --untracked-files=all |
    grep -vE '^\?\? \.codex_work/' || true
)"

if [[ -n "$DEPLOY_CHANGES" ]]; then
  if [[ "$MODE" == "deploy" ]]; then
    echo "El árbol de trabajo contiene cambios no confirmados:" >&2
    printf '%s\n' "$DEPLOY_CHANGES" >&2
    die "haz commit de los cambios que deseas publicar antes del deploy"
  fi
  echo "AVISO: Git no está limpio; --check continuará sin desplegar."
fi

cleanup() {
  if [[ -S "$SSH_SOCK" ]]; then
    if [[ -n "$COLORS_APPLICATION_CANDIDATE" && -n "$COLORS_COMPOSE_CANDIDATE" && -n "$COLORS_NGINX_CANDIDATE" && -n "$COLORS_ENV_CANDIDATE" ]]; then
      ssh -o BatchMode=yes -S "$SSH_SOCK" "$REMOTE_TARGET" \
        "rm -f '${COLORS_APPLICATION_CANDIDATE}' '${COLORS_COMPOSE_CANDIDATE}' '${COLORS_NGINX_CANDIDATE}' '${COLORS_ENV_CANDIDATE}'" \
        >/dev/null 2>&1 || true
    fi
    ssh -S "$SSH_SOCK" -O exit "$REMOTE_TARGET" >/dev/null 2>&1 || true
  fi
  rm -f "$SSH_SOCK"
  if [[ -n "$EMAIL_ENV_STAGING" ]]; then
    rm -f "$EMAIL_ENV_STAGING"
  fi
}
trap cleanup EXIT

rssh() {
  ssh -o BatchMode=yes -S "$SSH_SOCK" "$REMOTE_TARGET" "$@"
}

echo "============================================"
echo "  CGeV Colors — deploy seguro de aplicación"
echo "  Modo:       ${MODE}"
echo "  Fuente:     ${SOURCE_BRANCH}@${SOURCE_REV}"
echo "  Destino:    ${REMOTE_TARGET}:${APP_DIR}"
echo "  URL:        https://${PUBLIC_HOSTNAME}"
case "$MODE" in
  check)
    echo "  Captura:    auditando release activa"
    ;;
  *)
    if [[ "$COLORS_PERF_TIMING" == "1" ]]; then
      echo "  Captura:    activa (${PERF_RUN_LABEL})"
    else
      echo "  Captura:    desactivada"
    fi
    ;;
esac
echo "============================================"
echo ""

ssh -o BatchMode=yes -o ConnectTimeout=10 -o ControlMaster=yes \
  -o ControlPersist=60 -S "$SSH_SOCK" -fN "$REMOTE_TARGET"

echo "[preflight] Auditando infraestructura segura en Colors..."
rssh "command -v podman >/dev/null && command -v podman-compose >/dev/null && command -v python3 >/dev/null" || \
  die "Colors no tiene disponibles podman, podman-compose o python3"
rssh "podman info >/dev/null" || die "Podman no responde en Colors"
rssh "test -s '${COMPOSE_FILE}' && test -s '${APP_DIR}/shinyproxy/application.yml' && test -s '${APP_DIR}/.env' && test -s '${COLORS_NGINX_CONFIG}'" || \
  die "falta un archivo server-owned requerido (Compose, application.yml, .env o nginx)"
rssh "grep -q 'docker-socket-proxy' '${COMPOSE_FILE}'" || \
  die "Compose no declara docker-socket-proxy"
rssh "grep -q 'openanalytics/shinyproxy:3.2.4' '${COMPOSE_FILE}'" || \
  die "Compose no fija ShinyProxy 3.2.4"
rssh "grep -q 'url: http://docker-socket-proxy:2375' '${APP_DIR}/shinyproxy/application.yml'" || \
  die "application.yml no usa el socket proxy aislado"
if rssh "grep -q '^[[:space:]]*container-env-file:' '${APP_DIR}/shinyproxy/application.yml'"; then
  die "Colors no debe usar container-env-file: .env contiene secretos; se exige allow-list explícita"
fi
rssh "podman container exists cgv-docker-socket-proxy && podman container exists cgv-shinyproxy && podman container exists cgv-nginx" || \
  die "faltan contenedores base de Colors"

ACTUAL_SP_IMAGE="$(rssh "podman inspect cgv-shinyproxy --format '{{.ImageName}}'")" || \
  die "no se pudo inspeccionar la imagen de ShinyProxy"
[[ "$ACTUAL_SP_IMAGE" == "$SHINYPROXY_IMAGE" || "$ACTUAL_SP_IMAGE" == "$SHINYPROXY_RUNTIME_IMAGE" ]] || \
  die "Colors usa '${ACTUAL_SP_IMAGE}', no el ShinyProxy 3.2.4 fijado"

PROXY_MOUNTS="$(rssh "podman inspect cgv-shinyproxy --format '{{range .Mounts}}{{println .Destination}}{{end}}'")" || \
  die "no se pudieron inspeccionar los mounts de ShinyProxy"
if grep -qx '/var/run/docker.sock' <<<"$PROXY_MOUNTS"; then
  die "ShinyProxy monta directamente el socket; se aborta antes de tocar producción"
fi
if grep -qx '/opt/shinyproxy/env/cgv.env' <<<"$PROXY_MOUNTS"; then
  die "ShinyProxy monta .env completo; se aborta para no exponer secretos a sesiones públicas"
fi
PROXY_ENV="$(rssh "podman inspect cgv-shinyproxy --format '{{range .Config.Env}}{{println .}}{{end}}'")" || \
  die "no se pudo inspeccionar el entorno de ShinyProxy"
if grep -q '^FEEDBACK_RESEND_API_KEY=' <<<"$PROXY_ENV"; then
  die "ShinyProxy recibió FEEDBACK_RESEND_API_KEY; el secreto debe limitarse al worker"
fi

NGINX_MOUNTS="$(rssh "podman inspect cgv-nginx --format '{{range .Mounts}}{{println .Destination}}{{end}}'")" || \
  die "no se pudieron inspeccionar los mounts de nginx"
grep -qx '/srv/cgv-cache' <<<"$NGINX_MOUNTS" || \
  die "nginx no monta el cache persistente en /srv/cgv-cache"
NGINX_RUNTIME_IMAGE="$(rssh "podman inspect cgv-nginx --format '{{.ImageName}}'")" || \
  die "no se pudo inspeccionar la imagen de nginx"

SOCKET_STATE="$(rssh "podman inspect cgv-docker-socket-proxy --format '{{.State.Health.Status}}'")"
[[ "$SOCKET_STATE" == "healthy" ]] || die "docker-socket-proxy no está saludable: ${SOCKET_STATE}"

CONTROL_INTERNAL="$(rssh "podman network inspect sp-control --format '{{.Internal}}'")"
[[ "$CONTROL_INTERNAL" == "true" ]] || die "la red sp-control no está marcada como interna"

CURRENT_IMAGE="$(
  rssh "podman inspect cgv-shinyproxy --format '{{range .Config.Env}}{{println .}}{{end}}'" |
    sed -n 's/^CGV_IMAGE=//p' | tail -1
)"
[[ "$CURRENT_IMAGE" == localhost/cgv:release-* ]] || \
  die "la imagen activa no es una release versionada: '${CURRENT_IMAGE:-vacía}'"
rssh "podman image exists '${CURRENT_IMAGE}'"
ENV_IMAGE="$(rssh "sed -n 's/^CGV_IMAGE=//p' '${APP_DIR}/.env' | tail -1")"
[[ "$ENV_IMAGE" == "$CURRENT_IMAGE" ]] || \
  die "la imagen de .env (${ENV_IMAGE:-vacía}) no coincide con la imagen activa (${CURRENT_IMAGE})"
rssh "for d in annotations genomes go_annotations data cache; do test -d '${APP_DIR}'/\"\$d\" || exit 1; done; test -s '${APP_DIR}/annotations/registry.tsv'"

read -r PROXY_CODE NGINX_CODE < <(
  rssh "python3 - <<'PY'
import http.client
codes=[]
for port in (18081, 3838):
    try:
        c=http.client.HTTPConnection('127.0.0.1', port, timeout=8)
        c.request('HEAD', '/')
        codes.append(str(c.getresponse().status))
        c.close()
    except Exception:
        codes.append('000')
print(' '.join(codes))
PY"
)
[[ "$PROXY_CODE" =~ ^[23][0-9][0-9]$ ]] || die "ShinyProxy no responde correctamente: HTTP ${PROXY_CODE}"
[[ "$NGINX_CODE" =~ ^[23][0-9][0-9]$ ]] || die "nginx no responde correctamente: HTTP ${NGINX_CODE}"

echo "  ShinyProxy: 3.2.4 fijado"
echo "  Socket API: ${SOCKET_STATE}, aislado en sp-control"
echo "  Imagen CGV activa: ${CURRENT_IMAGE}"
echo "  HTTP interno: proxy=${PROXY_CODE}, nginx=${NGINX_CODE}"

if [[ "$MODE" == "check" ]]; then
  ACTIVE_IMAGE_REVISION="$(
    rssh "podman image inspect '${CURRENT_IMAGE}' --format '{{ index .Labels \"org.opencontainers.image.revision\" }}'" 2>/dev/null || true
  )"
  if [[ "$ACTIVE_IMAGE_REVISION" != "$SOURCE_REV" ]]; then
    die "Colors saludable pero no ejecuta el commit local ${SOURCE_REV}; la imagen activa ${CURRENT_IMAGE} declara ${ACTIVE_IMAGE_REVISION:-sin-label}. Despliega el commit confirmado antes de aceptar CHECK OK"
  fi

  CHECK_DELEGATE="$(
    rssh "set -e
      for i in \$(seq 1 15); do
        for candidate in \$(podman ps --filter name=sp-container- --filter status=running --format '{{.Names}}' 2>/dev/null || true); do
          candidate_image=\$(podman inspect \"\$candidate\" --format '{{.ImageName}}' 2>/dev/null || true)
          if [ \"\$candidate_image\" = '${CURRENT_IMAGE}' ]; then
            printf '%s\n' \"\$candidate\"
            exit 0
          fi
        done
        sleep 1
      done
      exit 1
    " || true
  )"
  [[ -n "$CHECK_DELEGATE" ]] || \
    die "Colors ejecuta ${CURRENT_IMAGE}, pero no hay una sesión pública activa de esa release"

  BROKER_POLICY="$(sed -n 's/^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=//p' <<<"$PROXY_ENV")"
  DELEGATE_POLICY="$(
    rssh "podman inspect '${CHECK_DELEGATE}' --format '{{range .Config.Env}}{{println .}}{{end}}'" |
      sed -n 's/^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=//p'
  )"
  [[ "$BROKER_POLICY" == "0" || "$BROKER_POLICY" == "1" ]] || \
    die "la release activa no declara una política ortológica válida en ShinyProxy"
  [[ "$DELEGATE_POLICY" == "0" || "$DELEGATE_POLICY" == "1" ]] || \
    die "la sesión pública ${CHECK_DELEGATE} no recibió una política ortológica válida"
  [[ "$DELEGATE_POLICY" == "$BROKER_POLICY" ]] || \
    die "la política ortológica de la sesión (${DELEGATE_POLICY}) no coincide con ShinyProxy (${BROKER_POLICY})"

  APPLICATION_PERF_TIMING="$(
    rssh "sed -n 's/^        APP_PERF_TIMING: \"\([01]\)\"$/\1/p' '${APP_DIR}/shinyproxy/application.yml'"
  )"
  DELEGATE_PERF_TIMING="$(
    rssh "podman inspect '${CHECK_DELEGATE}' --format '{{range .Config.Env}}{{println .}}{{end}}'" |
      sed -n 's/^APP_PERF_TIMING=//p'
  )"
  [[ "$APPLICATION_PERF_TIMING" == "0" || "$APPLICATION_PERF_TIMING" == "1" ]] || \
    die "el application.yml activo no declara exactamente un APP_PERF_TIMING válido"
  [[ "$DELEGATE_PERF_TIMING" == "0" || "$DELEGATE_PERF_TIMING" == "1" ]] || \
    die "la sesión pública ${CHECK_DELEGATE} no recibió APP_PERF_TIMING válido"
  [[ "$DELEGATE_PERF_TIMING" == "$APPLICATION_PERF_TIMING" ]] || \
    die "APP_PERF_TIMING de la sesión (${DELEGATE_PERF_TIMING}) no coincide con application.yml (${APPLICATION_PERF_TIMING})"

  APPLICATION_EAGER_PROFILE="$(
    rssh "sed -n -E 's/^        (APP_ORTHO_SUSPEND_HIDDEN|APP_HOMO_DEFER_SEQUENCE|APP_ORTHO_DEFER_SEQUENCE|APP_FOOTER_DEFER_SEQUENCE|APP_DEFER_FEATURE_GC|APP_HOMO_RENDER_CHUNK_SIZE|APP_HOMO_AUTO_RENDER_DELAY_MS|APP_ORTHO_RENDER_CHUNK_SIZE|APP_ORTHO_AUTO_RENDER_MORE|APP_ORTHO_AUTO_RENDER_DELAY_MS|APP_HOMO_INITIAL_VISIBLE|APP_ORTHO_INITIAL_VISIBLE|APP_ISOFORM_RENDER_BATCH_SIZE|APP_ISOFORM_RENDER_BATCH_DELAY_MS|APP_ORTHO_SERVER_RENDER_NUDGE): \"([^\"]+)\"$/\1=\2/p' '${APP_DIR}/shinyproxy/application.yml'"
  )"
  DELEGATE_ENV="$(
    rssh "podman inspect '${CHECK_DELEGATE}' --format '{{range .Config.Env}}{{println .}}{{end}}'"
  )"
  check_eager_profile_value() {
    local env_key="$1"
    local expected_value="$2"
    local application_count application_value delegate_count delegate_value
    application_count="$(printf '%s\n' "$APPLICATION_EAGER_PROFILE" | grep -c "^${env_key}=" || true)"
    application_value="$(printf '%s\n' "$APPLICATION_EAGER_PROFILE" | sed -n "s/^${env_key}=//p")"
    delegate_count="$(printf '%s\n' "$DELEGATE_ENV" | grep -c "^${env_key}=" || true)"
    delegate_value="$(printf '%s\n' "$DELEGATE_ENV" | sed -n "s/^${env_key}=//p")"
    [[ "$application_count" == "1" && "$application_value" == "$expected_value" ]] || \
      die "application.yml no materializa ${env_key}=${expected_value} exactamente una vez"
    [[ "$delegate_count" == "1" && "$delegate_value" == "$expected_value" ]] || \
      die "la sesión pública ${CHECK_DELEGATE} no recibió ${env_key}=${expected_value} exactamente una vez"
  }
  check_eager_profile_value APP_ORTHO_SUSPEND_HIDDEN "$COLORS_ORTHO_SUSPEND_HIDDEN"
  check_eager_profile_value APP_HOMO_DEFER_SEQUENCE "$COLORS_HOMO_DEFER_SEQUENCE"
  check_eager_profile_value APP_ORTHO_DEFER_SEQUENCE "$COLORS_ORTHO_DEFER_SEQUENCE"
  check_eager_profile_value APP_FOOTER_DEFER_SEQUENCE "$COLORS_FOOTER_DEFER_SEQUENCE"
  check_eager_profile_value APP_DEFER_FEATURE_GC "$COLORS_DEFER_FEATURE_GC"
  check_eager_profile_value APP_HOMO_RENDER_CHUNK_SIZE "$COLORS_HOMO_RENDER_CHUNK_SIZE"
  check_eager_profile_value APP_HOMO_AUTO_RENDER_DELAY_MS "$COLORS_HOMO_AUTO_RENDER_DELAY_MS"
  check_eager_profile_value APP_ORTHO_RENDER_CHUNK_SIZE "$COLORS_ORTHO_RENDER_CHUNK_SIZE"
  check_eager_profile_value APP_ORTHO_AUTO_RENDER_MORE "$COLORS_ORTHO_AUTO_RENDER_MORE"
  check_eager_profile_value APP_ORTHO_AUTO_RENDER_DELAY_MS "$COLORS_ORTHO_AUTO_RENDER_DELAY_MS"
  check_eager_profile_value APP_HOMO_INITIAL_VISIBLE "$COLORS_HOMO_INITIAL_VISIBLE"
  check_eager_profile_value APP_ORTHO_INITIAL_VISIBLE "$COLORS_ORTHO_INITIAL_VISIBLE"
  check_eager_profile_value APP_ISOFORM_RENDER_BATCH_SIZE "$COLORS_ISOFORM_RENDER_BATCH_SIZE"
  check_eager_profile_value APP_ISOFORM_RENDER_BATCH_DELAY_MS "$COLORS_ISOFORM_RENDER_BATCH_DELAY_MS"
  check_eager_profile_value APP_ORTHO_SERVER_RENDER_NUDGE "$COLORS_ORTHO_SERVER_RENDER_NUDGE"

  DELEGATE_MEMORY="$(rssh "podman inspect '${CHECK_DELEGATE}' --format '{{.HostConfig.Memory}}'")"
  [[ "$DELEGATE_MEMORY" == "$COLORS_CONTAINER_MEMORY_BYTES" ]] || \
    die "límite de memoria efectivo ${DELEGATE_MEMORY}, esperado ${COLORS_CONTAINER_MEMORY_BYTES} bytes"
  DELEGATE_COMMAND="$(rssh "podman inspect '${CHECK_DELEGATE}' --format '{{range .Config.Cmd}}{{println .}}{{end}}'")"
  [[ "$DELEGATE_COMMAND" == $'bash\n/app/deploy/docker/run-app.sh' ]] || \
    die "la sesión pública no ejecuta el comando de arranque vigente"

  WORKER_STATE="$(rssh "podman inspect '${BACKGROUND_WORKER_NAME}' --format '{{.State.Status}}' 2>/dev/null || true")"
  WORKER_IMAGE="$(rssh "podman inspect '${BACKGROUND_WORKER_NAME}' --format '{{.ImageName}}' 2>/dev/null || true")"
  [[ "$WORKER_STATE" == "running" ]] || die "el worker de reportes no está activo: ${WORKER_STATE:-ausente}"
  [[ "$WORKER_IMAGE" == "$CURRENT_IMAGE" ]] || die "el worker usa ${WORKER_IMAGE:-ninguna}, no ${CURRENT_IMAGE}"
  rssh "podman exec '${BACKGROUND_WORKER_NAME}' test -s /app/cache/background_reports/worker.ready" || \
    die "el worker no completó el preflight headless; los reportes por correo fallarán"
  rssh "podman exec '${BACKGROUND_WORKER_NAME}' grep -q '^browser=Chrome/' /app/cache/background_reports/worker.ready" || \
    die "el worker no registró un arranque headless correcto en su marcador"
  echo "  Commit:     ${ACTIVE_IMAGE_REVISION}, coincide con la fuente local"
  echo "  Sesión:     ${CHECK_DELEGATE}, policy=${BROKER_POLICY}, perf=${APPLICATION_PERF_TIMING}"
  echo "  Progressive: hidden=${COLORS_ORTHO_SUSPEND_HIDDEN}, homo-seq=${COLORS_HOMO_DEFER_SEQUENCE}, ortho-seq=${COLORS_ORTHO_DEFER_SEQUENCE}, footer-seq=${COLORS_FOOTER_DEFER_SEQUENCE}, gc=${COLORS_DEFER_FEATURE_GC}, homo/ortho-chunk=${COLORS_HOMO_RENDER_CHUNK_SIZE}/${COLORS_ORTHO_RENDER_CHUNK_SIZE}, auto=${COLORS_ORTHO_AUTO_RENDER_MORE}, delays=${COLORS_HOMO_AUTO_RENDER_DELAY_MS}/${COLORS_ORTHO_AUTO_RENDER_DELAY_MS}ms, homo/ortho-initial=${COLORS_HOMO_INITIAL_VISIBLE}/${COLORS_ORTHO_INITIAL_VISIBLE}, isoform=${COLORS_ISOFORM_RENDER_BATCH_SIZE}/${COLORS_ISOFORM_RENDER_BATCH_DELAY_MS}ms, nudge=${COLORS_ORTHO_SERVER_RENDER_NUDGE}"
  echo "  Worker:     ${WORKER_STATE}, preflight headless OK"
  echo ""
  echo "CHECK OK: no se realizaron cambios en Colors."
  exit 0
fi

if [[ "$SKIP_TESTS" == "0" ]]; then
  command -v Rscript >/dev/null 2>&1 || die "Rscript no está disponible para ejecutar las pruebas"
  command -v node >/dev/null 2>&1 || die "node no está disponible para ejecutar las pruebas JavaScript"
  echo ""
  echo "[1/7] Validando sintaxis y pruebas R..."
  (
    cd "$SCRIPT_DIR"
    Rscript -e "invisible(parse(file='global.R')); invisible(parse(file='ui.R')); invisible(parse(file='server.R'))"
    Rscript -e "if (!requireNamespace('testthat', quietly=TRUE)) stop('testthat no está instalado'); testthat::test_dir('tests/testthat', reporter='summary')"
    Rscript scripts/test_background_report_jobs.R
    Rscript scripts/test_renderer_prewarm_scheduling.R
    Rscript scripts/test_plot_paint_timing_static.R
    Rscript scripts/test_render_lazy_defaults.R
    Rscript scripts/test_gene_catalog_feature_flag.R
    Rscript scripts/test_gene_catalog_search.R
    node tests/js/test_plot_paint_gate.js
    Rscript scripts/test_colors_shinyproxy_static_assets.R
    python3 -B scripts/test_colors_shinyproxy_candidates.py
    python3 -B scripts/test_colors_release_sync.py
  )
else
  echo ""
  echo "[1/7] Pruebas omitidas explícitamente con --skip-tests."
fi

command -v Rscript >/dev/null 2>&1 || die "Rscript no está disponible para calcular el CSP del reporte"
REPORT_SCRIPT_CSP_HASH="$(
  cd "$SCRIPT_DIR"
  Rscript -e "source('R/server_shared_analysis_domain.R'); cat(cgv_report_script_csp_hash())"
)"
[[ "$REPORT_SCRIPT_CSP_HASH" =~ ^sha256-[A-Za-z0-9+/]{43}=$ ]] || \
  die "el hash CSP del reporte no tiene el formato sha256 esperado: ${REPORT_SCRIPT_CSP_HASH}"

DEPLOY_UTC="$(date -u +%Y%m%dT%H%M%SZ)"
NEW_IMAGE="${CGV_IMAGE:-localhost/cgv:release-${SOURCE_REV}-${DEPLOY_UTC}}"
[[ "$NEW_IMAGE" == localhost/cgv:release-* ]] || \
  die "CGV_IMAGE debe usar una etiqueta versionada localhost/cgv:release-*"
[[ "$NEW_IMAGE" != "$CURRENT_IMAGE" ]] || die "la nueva imagen coincide con la release activa"
BACKUP_DIR="${REMOTE_PATH}/rollback/${DEPLOY_UTC}-pre-app-deploy"
COLORS_NGINX_CANDIDATE="${COLORS_NGINX_CONFIG}.candidate-${DEPLOY_UTC}"
COLORS_APPLICATION_CANDIDATE="${APP_DIR}/shinyproxy/application.yml.candidate-${DEPLOY_UTC}"
COLORS_COMPOSE_CANDIDATE="${COMPOSE_FILE}.candidate-${DEPLOY_UTC}"
COLORS_ENV_CANDIDATE="${APP_DIR}/.env.release-${DEPLOY_UTC}"

echo ""
echo "[2/7] Respaldando configuración y estado actual..."
rssh "set -e
  mkdir -p '${BACKUP_DIR}'
  cp -p '${APP_DIR}/.env' '${BACKUP_DIR}/app.env'
  cp -p '${COMPOSE_FILE}' '${BACKUP_DIR}/docker-compose.shinyproxy.colors.yml'
  cp -p '${APP_DIR}/shinyproxy/application.yml' '${BACKUP_DIR}/application.yml'
  cp -p '${APP_DIR}/deploy/nginx/cgv-shinyproxy-colors.conf' '${BACKUP_DIR}/cgv-shinyproxy-colors.conf'
  if [ -f '${REMOTE_EMAIL_ENV}' ]; then
    cp -p '${REMOTE_EMAIL_ENV}' '${BACKUP_DIR}/background-report-email.env'
  else
    : > '${BACKUP_DIR}/background-report-email.absent'
  fi
  podman inspect cgv-docker-socket-proxy cgv-shinyproxy cgv-nginx > '${BACKUP_DIR}/infrastructure.inspect.json'
  if podman container exists '${BACKGROUND_WORKER_NAME}'; then
    podman inspect '${BACKGROUND_WORKER_NAME}' > '${BACKUP_DIR}/background-worker.inspect.json'
  fi
  podman ps -a --no-trunc > '${BACKUP_DIR}/podman-ps.txt'
" || die "no se pudo completar el respaldo previo al deploy"

echo ""
echo "[3/7] Sincronizando sólo el código de aplicación..."
rsync -az --delete-delay --itemize-changes \
  -e "ssh -S $SSH_SOCK" \
  --exclude='/.git' \
  --exclude='/.env' --exclude='/.env.*' --exclude='/.Renviron' --exclude='/.cgv-compiled' \
  --exclude='/.codex_work' --exclude='/.codex_backups' --exclude='/.claude' --exclude='/.qodo' \
  --exclude='/.Rproj.user' --exclude='/.Rhistory' --exclude='/.Rapp.history' \
  --exclude='/annotations' --exclude='/genomes' --exclude='/go_annotations' \
  --exclude='/data' --exclude='/cache' --exclude='/ncbi_downloads' \
  --exclude='/build_sources' --exclude='/outputs' --exclude='/logs' --exclude='/tmp' \
  --exclude='/desktop' --exclude='/INSTALABLES-FINALES' \
  --exclude='/node_modules' --exclude='/paper' \
  --exclude='/deploy/deploy-colors-shinyproxy.sh' \
  --exclude='/deploy/docker-compose.shinyproxy.yml' \
  --exclude='/docker-compose.shinyproxy.colors.yml' \
  --exclude='/deploy/shinyproxy/application.yml' --exclude='/shinyproxy' \
  --exclude='/deploy/nginx/cgv-shinyproxy.conf' \
  --exclude='/deploy/nginx/cgv-shinyproxy-colors.conf' \
  "${SCRIPT_DIR}/" "${REMOTE_TARGET}:${APP_DIR}/"

# Copy only the allow-listed mail variables. The minimal file is never tracked,
# is mode 0600 on Colors, and is mounted only into the background worker.
EMAIL_ENV_STAGING="$(mktemp "${TMPDIR:-/tmp}/cgv-colors-report-email.XXXXXX")"
awk '
  BEGIN { single_quote = sprintf("%c", 39) }
  /^(FEEDBACK_RESEND_API_KEY|FEEDBACK_TO_EMAIL|FEEDBACK_FROM_EMAIL|REPORT_FROM_EMAIL|REPORT_REPLY_TO_EMAIL|REPORT_LOGO_PATH)=/ {
    separator = index($0, "=")
    key = substr($0, 1, separator - 1)
    value = substr($0, separator + 1)
    first = substr(value, 1, 1)
    last = substr(value, length(value), 1)
    if (length(value) >= 2 && ((first == "\"" && last == "\"") || (first == single_quote && last == single_quote))) {
      value = substr(value, 2, length(value) - 2)
    }
    print key "=" value
  }
' "$LOCAL_EMAIL_ENV" > "$EMAIL_ENV_STAGING"
chmod 600 "$EMAIL_ENV_STAGING"
rsync -az --chmod=Fu=rw,Fgo= \
  -e "ssh -S $SSH_SOCK" \
  "$EMAIL_ENV_STAGING" "${REMOTE_TARGET}:${REMOTE_EMAIL_ENV}"
rssh "chmod 600 '${REMOTE_EMAIL_ENV}' && test \"\$(stat -c '%a' '${REMOTE_EMAIL_ENV}')\" = 600" || \
  die "no se pudo asegurar el archivo privado del worker de reportes"

echo "  Preparando candidatos server-owned con allow-list de assets..."
rssh "set -e
  cd '${APP_DIR}'
  policy_count=\$(grep -c '^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=' '${APP_DIR}/.env' || true)
  if [ \"\$policy_count\" -gt 1 ]; then
    echo 'POLICY_GUARD_FAILED: env-duplicate-orthology-policy' >&2
    exit 1
  fi
  orthology_policy=0
  if [ \"\$policy_count\" = 1 ]; then
    if ! grep -Eq '^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=[01]$' '${APP_DIR}/.env'; then
      echo 'POLICY_GUARD_FAILED: env-invalid-orthology-policy' >&2
      exit 1
    fi
    orthology_policy=\$(sed -n 's/^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=//p' '${APP_DIR}/.env')
  fi
  python3 -B scripts/build_colors_shinyproxy_candidates.py \
    --application '${APP_DIR}/shinyproxy/application.yml' \
    --container-memory-limit '${COLORS_CONTAINER_MEMORY}' \
    --application-output '${COLORS_APPLICATION_CANDIDATE}' \
    --compose '${COMPOSE_FILE}' \
    --compose-output '${COLORS_COMPOSE_CANDIDATE}' \
    --orthology-policy \"\$orthology_policy\"
  test -s '${COLORS_APPLICATION_CANDIDATE}'
  test -s '${COLORS_COMPOSE_CANDIDATE}'
" || die "no se pudieron generar los candidatos server-owned seguros"

# Colors keeps its hardened ShinyProxy configuration on the server. Add only
# the allow-listed flags required by background reports and manual performance
# capture; the complete file was backed up above and is restored verbatim by
# rollback_release().
rssh "set -e
  config='${COLORS_APPLICATION_CANDIDATE}'
  sed -i -E 's#^  title:.*#  title: CGeV - Comparative Gene Viewer#' \"\$config\"
  sed -i -E 's#^      display-name:.*#      display-name: CGeV - Comparative Gene Viewer#' \"\$config\"
  if ! grep -q 'APP_BACKGROUND_REPORTS_ENABLED:' \"\$config\"; then
    staged=\"\${config}.background.\$\$\"
    awk '
      { print }
      /APP_PERF_TIMING:/ {
        print \"        APP_BACKGROUND_REPORTS_ENABLED: \\\"\${SP_BACKGROUND_REPORTS_ENABLED:1}\\\"\"
        print \"        APP_LASTZ_GLOBAL_WORKERS: \\\"\${SP_LASTZ_GLOBAL_WORKERS:1}\\\"\"
        print \"        APP_LASTZ_GLOBAL_QUEUE_WAIT_SECONDS: \\\"\${SP_LASTZ_GLOBAL_QUEUE_WAIT_SECONDS:1800}\\\"\"
      }
    ' \"\$config\" > \"\$staged\"
    chmod --reference=\"\$config\" \"\$staged\"
    mv \"\$staged\" \"\$config\"
  fi
  if ! grep -q 'APP_PERF_LOG_DIR:' \"\$config\"; then
    staged=\"\${config}.perf.\$\$\"
    awk '
      { print }
      /APP_PERF_TIMING:/ {
        print \"        APP_PERF_LOG_DIR: \\\"/app/cache/perf_runs\\\"\"
        print \"        APP_PERF_RUN_LABEL: \\\"manual\\\"\"
        print \"        APP_BUILD_REVISION: \\\"unknown\\\"\"
      }
    ' \"\$config\" > \"\$staged\"
    chmod --reference=\"\$config\" \"\$staged\"
    mv \"\$staged\" \"\$config\"
  fi
  if ! grep -q 'APP_PERF_RUN_LABEL:' \"\$config\"; then
    sed -i '/APP_PERF_LOG_DIR:/a\        APP_PERF_RUN_LABEL: \"manual\"' \"\$config\"
  fi
  if ! grep -q 'APP_BUILD_REVISION:' \"\$config\"; then
    sed -i '/APP_PERF_RUN_LABEL:/a\        APP_BUILD_REVISION: \"unknown\"' \"\$config\"
  fi
  sed -i -E 's|^        APP_PERF_TIMING:.*|        APP_PERF_TIMING: \"${COLORS_PERF_TIMING}\"|' \"\$config\"
  sed -i -E 's|^        APP_PERF_RUN_LABEL:.*|        APP_PERF_RUN_LABEL: \"${PERF_RUN_LABEL}\"|' \"\$config\"
  sed -i -E 's|^        APP_BUILD_REVISION:.*|        APP_BUILD_REVISION: \"${NEW_IMAGE}\"|' \"\$config\"
  if grep -q '^        CGV_PUBLIC_BASE_URL:' \"\$config\"; then
    sed -i -E 's|^        CGV_PUBLIC_BASE_URL:.*|        CGV_PUBLIC_BASE_URL: \"https://${PUBLIC_HOSTNAME}\"|' \"\$config\"
  else
    sed -i '/APP_BUILD_REVISION:/a\        CGV_PUBLIC_BASE_URL: \"https://${PUBLIC_HOSTNAME}\"' \"\$config\"
  fi
  grep -q 'APP_BACKGROUND_REPORTS_ENABLED: \"\${SP_BACKGROUND_REPORTS_ENABLED:1}\"' \"\$config\"
  grep -q 'APP_LASTZ_GLOBAL_WORKERS:' \"\$config\"
  test \"\$(grep -c '^        APP_PERF_TIMING: \"${COLORS_PERF_TIMING}\"$' \"\$config\")\" = 1
  grep -q 'APP_PERF_LOG_DIR: \"/app/cache/perf_runs\"' \"\$config\"
  grep -q 'APP_PERF_RUN_LABEL: \"${PERF_RUN_LABEL}\"' \"\$config\"
  grep -q 'APP_BUILD_REVISION: \"${NEW_IMAGE}\"' \"\$config\"
  test \"\$(grep -c '^        CGV_PUBLIC_BASE_URL: \"https://${PUBLIC_HOSTNAME}\"$' \"\$config\")\" = 1
  grep -Fq 'title: CGeV - Comparative Gene Viewer' \"\$config\"
  grep -Fq 'display-name: CGeV - Comparative Gene Viewer' \"\$config\"
" || die "falló la preparación del application.yml server-owned"

rssh "set -e
  config='${COLORS_APPLICATION_CANDIDATE}'
  if grep -q '^        APP_INLINE_FAST_SEQUENCE_PREFETCH:' \"\$config\"; then
    sed -i -E 's|^        APP_INLINE_FAST_SEQUENCE_PREFETCH:.*|        APP_INLINE_FAST_SEQUENCE_PREFETCH: \"\${SP_INLINE_FAST_SEQUENCE_PREFETCH:${COLORS_INLINE_FAST_SEQUENCE_PREFETCH}}\"|' \"\$config\"
  else
    sed -i '/APP_PERF_TIMING:/a\        APP_INLINE_FAST_SEQUENCE_PREFETCH: \"\${SP_INLINE_FAST_SEQUENCE_PREFETCH:${COLORS_INLINE_FAST_SEQUENCE_PREFETCH}}\"' \"\$config\"
  fi
  eager_staged=\"\${config}.eager.\$\$\"
  awk '
    /^        (APP_ORTHO_SUSPEND_HIDDEN|APP_HOMO_DEFER_SEQUENCE|APP_ORTHO_DEFER_SEQUENCE|APP_FOOTER_DEFER_SEQUENCE|APP_DEFER_FEATURE_GC|APP_HOMO_RENDER_CHUNK_SIZE|APP_HOMO_AUTO_RENDER_DELAY_MS|APP_ORTHO_RENDER_CHUNK_SIZE|APP_ORTHO_AUTO_RENDER_MORE|APP_ORTHO_AUTO_RENDER_DELAY_MS|APP_HOMO_INITIAL_VISIBLE|APP_ORTHO_INITIAL_VISIBLE|APP_ISOFORM_RENDER_BATCH_SIZE|APP_ISOFORM_RENDER_BATCH_DELAY_MS|APP_ORTHO_SERVER_RENDER_NUDGE):/ { next }
    { print }
    /^        APP_INLINE_FAST_SEQUENCE_PREFETCH:/ && !inserted {
      print \"        APP_ORTHO_SUSPEND_HIDDEN: \\\"${COLORS_ORTHO_SUSPEND_HIDDEN}\\\"\"
      print \"        APP_HOMO_DEFER_SEQUENCE: \\\"${COLORS_HOMO_DEFER_SEQUENCE}\\\"\"
      print \"        APP_ORTHO_DEFER_SEQUENCE: \\\"${COLORS_ORTHO_DEFER_SEQUENCE}\\\"\"
      print \"        APP_FOOTER_DEFER_SEQUENCE: \\\"${COLORS_FOOTER_DEFER_SEQUENCE}\\\"\"
      print \"        APP_DEFER_FEATURE_GC: \\\"${COLORS_DEFER_FEATURE_GC}\\\"\"
      print \"        APP_HOMO_RENDER_CHUNK_SIZE: \\\"${COLORS_HOMO_RENDER_CHUNK_SIZE}\\\"\"
      print \"        APP_HOMO_AUTO_RENDER_DELAY_MS: \\\"${COLORS_HOMO_AUTO_RENDER_DELAY_MS}\\\"\"
      print \"        APP_ORTHO_RENDER_CHUNK_SIZE: \\\"${COLORS_ORTHO_RENDER_CHUNK_SIZE}\\\"\"
      print \"        APP_ORTHO_AUTO_RENDER_MORE: \\\"${COLORS_ORTHO_AUTO_RENDER_MORE}\\\"\"
      print \"        APP_ORTHO_AUTO_RENDER_DELAY_MS: \\\"${COLORS_ORTHO_AUTO_RENDER_DELAY_MS}\\\"\"
      print \"        APP_HOMO_INITIAL_VISIBLE: \\\"${COLORS_HOMO_INITIAL_VISIBLE}\\\"\"
      print \"        APP_ORTHO_INITIAL_VISIBLE: \\\"${COLORS_ORTHO_INITIAL_VISIBLE}\\\"\"
      print \"        APP_ISOFORM_RENDER_BATCH_SIZE: \\\"${COLORS_ISOFORM_RENDER_BATCH_SIZE}\\\"\"
      print \"        APP_ISOFORM_RENDER_BATCH_DELAY_MS: \\\"${COLORS_ISOFORM_RENDER_BATCH_DELAY_MS}\\\"\"
      print \"        APP_ORTHO_SERVER_RENDER_NUDGE: \\\"${COLORS_ORTHO_SERVER_RENDER_NUDGE}\\\"\"
      inserted=1
    }
    END { if (!inserted) exit 42 }
  ' \"\$config\" > \"\$eager_staged\"
  chmod --reference=\"\$config\" \"\$eager_staged\"
  mv \"\$eager_staged\" \"\$config\"
  grep -q 'APP_INLINE_FAST_SEQUENCE_PREFETCH: \"\${SP_INLINE_FAST_SEQUENCE_PREFETCH:${COLORS_INLINE_FAST_SEQUENCE_PREFETCH}}\"' \"\$config\"
  test \"\$(grep -c '^        APP_ORTHO_SUSPEND_HIDDEN:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_HOMO_DEFER_SEQUENCE:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_ORTHO_DEFER_SEQUENCE:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_FOOTER_DEFER_SEQUENCE:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_DEFER_FEATURE_GC:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_HOMO_RENDER_CHUNK_SIZE:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_HOMO_AUTO_RENDER_DELAY_MS:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_ORTHO_RENDER_CHUNK_SIZE:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_ORTHO_AUTO_RENDER_MORE:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_ORTHO_AUTO_RENDER_DELAY_MS:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_HOMO_INITIAL_VISIBLE:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_ORTHO_INITIAL_VISIBLE:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_ISOFORM_RENDER_BATCH_SIZE:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_ISOFORM_RENDER_BATCH_DELAY_MS:' \"\$config\")\" = 1
  test \"\$(grep -c '^        APP_ORTHO_SERVER_RENDER_NUDGE:' \"\$config\")\" = 1
  grep -Fqx '        APP_ORTHO_SUSPEND_HIDDEN: \"${COLORS_ORTHO_SUSPEND_HIDDEN}\"' \"\$config\"
  grep -Fqx '        APP_HOMO_DEFER_SEQUENCE: \"${COLORS_HOMO_DEFER_SEQUENCE}\"' \"\$config\"
  grep -Fqx '        APP_ORTHO_DEFER_SEQUENCE: \"${COLORS_ORTHO_DEFER_SEQUENCE}\"' \"\$config\"
  grep -Fqx '        APP_FOOTER_DEFER_SEQUENCE: \"${COLORS_FOOTER_DEFER_SEQUENCE}\"' \"\$config\"
  grep -Fqx '        APP_DEFER_FEATURE_GC: \"${COLORS_DEFER_FEATURE_GC}\"' \"\$config\"
  grep -Fqx '        APP_HOMO_RENDER_CHUNK_SIZE: \"${COLORS_HOMO_RENDER_CHUNK_SIZE}\"' \"\$config\"
  grep -Fqx '        APP_HOMO_AUTO_RENDER_DELAY_MS: \"${COLORS_HOMO_AUTO_RENDER_DELAY_MS}\"' \"\$config\"
  grep -Fqx '        APP_ORTHO_RENDER_CHUNK_SIZE: \"${COLORS_ORTHO_RENDER_CHUNK_SIZE}\"' \"\$config\"
  grep -Fqx '        APP_ORTHO_AUTO_RENDER_MORE: \"${COLORS_ORTHO_AUTO_RENDER_MORE}\"' \"\$config\"
  grep -Fqx '        APP_ORTHO_AUTO_RENDER_DELAY_MS: \"${COLORS_ORTHO_AUTO_RENDER_DELAY_MS}\"' \"\$config\"
  grep -Fqx '        APP_HOMO_INITIAL_VISIBLE: \"${COLORS_HOMO_INITIAL_VISIBLE}\"' \"\$config\"
  grep -Fqx '        APP_ORTHO_INITIAL_VISIBLE: \"${COLORS_ORTHO_INITIAL_VISIBLE}\"' \"\$config\"
  grep -Fqx '        APP_ISOFORM_RENDER_BATCH_SIZE: \"${COLORS_ISOFORM_RENDER_BATCH_SIZE}\"' \"\$config\"
  grep -Fqx '        APP_ISOFORM_RENDER_BATCH_DELAY_MS: \"${COLORS_ISOFORM_RENDER_BATCH_DELAY_MS}\"' \"\$config\"
  grep -Fqx '        APP_ORTHO_SERVER_RENDER_NUDGE: \"${COLORS_ORTHO_SERVER_RENDER_NUDGE}\"' \"\$config\"
" || die "falló la materialización del perfil progresivo en application.yml"

rssh "set -e
  config='${COLORS_APPLICATION_CANDIDATE}'
  sed -i -E 's|^        APP_LASTZ_GLOBAL_WORKERS:.*|        APP_LASTZ_GLOBAL_WORKERS: \"\${SP_LASTZ_GLOBAL_WORKERS:${APP_LASTZ_GLOBAL_WORKERS}}\"|' \"\$config\"
  grep -q 'APP_LASTZ_GLOBAL_WORKERS: \"\${SP_LASTZ_GLOBAL_WORKERS:${APP_LASTZ_GLOBAL_WORKERS}}\"' \"\$config\"
" || die "falló la configuración LASTZ en application.yml"

rssh "set -e
  test \"\$(grep -c '^        APP_ASSET_VERSION: \"\${APP_ASSET_VERSION:}\"$' '${COLORS_APPLICATION_CANDIDATE}')\" = 1
  test \"\$(grep -c '^        APP_STATIC_BASE_URL: \"\${APP_STATIC_BASE_URL:}\"$' '${COLORS_APPLICATION_CANDIDATE}')\" = 1
  test \"\$(grep -c '^      APP_ASSET_VERSION: \"\${APP_ASSET_VERSION:-}\"$' '${COLORS_COMPOSE_CANDIDATE}')\" = 1
  test \"\$(grep -c '^      APP_STATIC_BASE_URL: \"\${APP_STATIC_BASE_URL:-}\"$' '${COLORS_COMPOSE_CANDIDATE}')\" = 1
  policy_count=\$(grep -c '^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=' '${APP_DIR}/.env' || true)
  if [ \"\$policy_count\" -gt 1 ]; then
    echo 'POLICY_GUARD_FAILED: env-duplicate-orthology-policy' >&2
    exit 1
  fi
  if [ \"\$policy_count\" = 1 ] && ! grep -Eq '^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=[01]$' '${APP_DIR}/.env'; then
    echo 'POLICY_GUARD_FAILED: env-invalid-orthology-policy' >&2
    exit 1
  fi
  orthology_policy=0
  if [ \"\$policy_count\" = 1 ]; then
    orthology_policy=\$(sed -n 's/^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=//p' '${APP_DIR}/.env')
  fi
  application_policy=\$(sed -n 's/^        APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: \"\([01]\)\"$/\1/p' '${COLORS_APPLICATION_CANDIDATE}')
  compose_policy=\$(sed -n 's/^      APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: \"\([01]\)\"$/\1/p' '${COLORS_COMPOSE_CANDIDATE}')
  test \"\$application_policy\" = \"\$orthology_policy\"
  test \"\$compose_policy\" = \"\$orthology_policy\"
  ! grep -Fq '\${APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY' '${COLORS_APPLICATION_CANDIDATE}'
  ! grep -Fq '\${APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY' '${COLORS_COMPOSE_CANDIDATE}'
  ! grep -q '^[[:space:]]*container-env-file:' '${COLORS_APPLICATION_CANDIDATE}'
  ! grep -q '^[[:space:]]*env_file:' '${COLORS_COMPOSE_CANDIDATE}'
  ! grep -q '/opt/shinyproxy/env/cgv.env' '${COLORS_COMPOSE_CANDIDATE}'
  cd '${APP_DIR}'
  podman-compose -f '${COLORS_COMPOSE_CANDIDATE}' config >/dev/null
" || die "los candidatos server-owned no superaron la validación final"

echo "  Preparando candidato nginx server-owned y validando sintaxis..."
rssh "set -e
  cd '${APP_DIR}'
  python3 scripts/build_nginx_static_candidate.py \
    --config '${COLORS_NGINX_CONFIG}' \
    --snippet '${APP_DIR}/deploy/nginx/cgv-static-assets.location.conf' \
    --output '${COLORS_NGINX_CANDIDATE}' \
    --report-script-hash '${REPORT_SCRIPT_CSP_HASH}'
  grep -q '^    # BEGIN CGV IMMUTABLE STATIC v1$' '${COLORS_NGINX_CANDIDATE}'
  grep -q 'alias /srv/cgv-cache/static_assets/releases/;' '${COLORS_NGINX_CANDIDATE}'
  grep -Fq '${REPORT_SCRIPT_CSP_HASH}' '${COLORS_NGINX_CANDIDATE}'
  podman run --rm \
    --name 'cgv-nginx-config-test-${DEPLOY_UTC}' \
    --user 101:101 \
    --read-only \
    --network sp-net \
    --security-opt no-new-privileges \
    --cap-drop all \
    --tmpfs /var/cache/nginx:rw,nosuid,nodev,noexec,size=32m,mode=1777 \
    --tmpfs /run:rw,nosuid,nodev,noexec,size=8m,mode=1777 \
    --tmpfs /tmp:rw,nosuid,nodev,noexec,size=8m \
    --volume '${COLORS_NGINX_CANDIDATE}:/etc/nginx/conf.d/default.conf:ro' \
    --volume '${APP_DIR}/cache:/srv/cgv-cache:ro' \
    --entrypoint /usr/sbin/nginx \
    '${NGINX_RUNTIME_IMAGE}' -t
" || die "el candidato nginx no superó la validación"

echo ""
echo "[4/7] Construyendo imagen inmutable ${NEW_IMAGE}..."
DEPS_HAVE_REPORT_RUNTIME=0
if rssh "podman image exists '${CGV_DEPS_IMAGE}' && podman run --rm --entrypoint /usr/bin/google-chrome '${CGV_DEPS_IMAGE}' --version >/dev/null && podman run --rm --entrypoint Rscript '${CGV_DEPS_IMAGE}' -e \"quit(status=if(requireNamespace('chromote', quietly=TRUE)) 0 else 1)\" >/dev/null"; then
  DEPS_HAVE_REPORT_RUNTIME=1
fi
if [[ "$REBUILD_R_DEPS" == "1" || "$DEPS_HAVE_REPORT_RUNTIME" == "0" ]]; then
  if [[ "$DEPS_HAVE_REPORT_RUNTIME" == "0" ]]; then
    echo "  La base no contiene Google Chrome/chromote; se reconstruirá automáticamente."
  fi
  rssh "cd '${APP_DIR}' && podman build --pull=never -t '${CGV_DEPS_IMAGE}' -f deploy/docker/Dockerfile.dependencies ."
else
  echo "  Google Chrome y chromote ya están disponibles en ${CGV_DEPS_IMAGE}."
fi
rssh "podman run --rm --entrypoint /usr/bin/google-chrome '${CGV_DEPS_IMAGE}' --version >/dev/null"
rssh "cd '${APP_DIR}' && podman build --pull=never \
  --build-arg CGV_DEPS_IMAGE='${CGV_DEPS_IMAGE}' \
  --label org.opencontainers.image.revision='${SOURCE_REV}' \
  --label org.opencontainers.image.created='${DEPLOY_UTC}' \
  -t '${NEW_IMAGE}' -f Dockerfile ."

STATIC_IMAGE_ID="$(rssh "podman image inspect --format '{{.Id}}' '${NEW_IMAGE}'" | tr -d '\r\n')"
STATIC_REVISION="${STATIC_IMAGE_ID#sha256:}"
[[ "$STATIC_REVISION" =~ ^[a-f0-9]{64}$ ]] || \
  die "Podman no devolvió un Id sha256 válido para ${NEW_IMAGE}: ${STATIC_IMAGE_ID}"

LOCAL_HASHES="$(
  cd "$SCRIPT_DIR"
  shasum -a 256 \
    ui.R server.R global.R \
    R/background_report_jobs.R scripts/background_report_worker.R scripts/verify_headless_chrome.R \
    www/js/reproducible_report.js \
    www/home_preview_cgv.html www/css/cgv_compiled.css |
    awk '{print $1}' | paste -sd: -
)"
REMOTE_HASHES="$(
  rssh "podman run --rm --user 10001:10001 --entrypoint sha256sum '${NEW_IMAGE}' \
    /app/ui.R /app/server.R /app/global.R \
    /app/R/background_report_jobs.R /app/scripts/background_report_worker.R /app/scripts/verify_headless_chrome.R \
    /app/www/js/reproducible_report.js \
    /app/www/home_preview_cgv.html /app/www/css/cgv_compiled.css" |
    awk '{print $1}' | paste -sd: -
)"
[[ "$LOCAL_HASHES" == "$REMOTE_HASHES" ]] || die "los hashes de la imagen no coinciden con el commit local"
echo "  Hashes app/worker/reporte/home/CSS y lectura con UID 10001: OK"
rssh "podman run --rm --network none --entrypoint sh '${NEW_IMAGE}' -c \"test ! -e /app/.env && test ! -e /app/.env.background-reports && test ! -e /app/.Renviron\"" || \
  die "la imagen contiene un archivo de entorno reservado al servidor"


# Exercise Shiny's real loader in the image that will be published. Separate
# processes avoid reusing global state or a manifest cache across modes.
for runtime_mode in 0 1; do
  runtime_args=""
  [[ "$runtime_mode" == "0" ]] || runtime_args="--require-compiled"
  rssh "podman run --rm --network none --user 10001:10001 \
    -e CGV_CACHE_DIR=/tmp/cgv-runtime-check -e APP_COMPILED_RUNTIME=${runtime_mode} \
    --entrypoint Rscript '${NEW_IMAGE}' scripts/test_shiny_runtime_loading.R ${runtime_args}" || \
    die "la imagen no supera la carga real de Shiny (APP_COMPILED_RUNTIME=${runtime_mode})"
done

rssh "cd '${APP_DIR}' && python3 -B scripts/verify_colors_image_startup.py \
  --application '${COLORS_APPLICATION_CANDIDATE}' --image '${NEW_IMAGE}' \
  --memory-limit '${COLORS_CONTAINER_MEMORY}'" || \
  die "la imagen no arranca con el comando efectivo de ShinyProxy; producción no se cambia"

echo ""
echo "[5/7] Precalentando índices y preparando permisos persistentes..."
rssh "set -e
  cd '${APP_DIR}'
  DOCKER_BIN=podman \
  CGV_IMAGE='${NEW_IMAGE}' \
  CGV_PUBLISH_STATIC_ASSETS=1 \
  CGV_STATIC_REVISION='${STATIC_REVISION}' \
  CGV_ANNOTATIONS_DIR='${APP_DIR}/annotations' \
  CGV_GENOMES_DIR='${APP_DIR}/genomes' \
  CGV_GO_ANNOTATIONS_DIR='${APP_DIR}/go_annotations' \
  CGV_DATA_DIR='${APP_DIR}/data' \
  CGV_CACHE_DIR='${APP_DIR}/cache' \
  bash deploy/docker/setup-prewarm.sh
  cache_dir=\$(readlink -f '${APP_DIR}/cache')
  ncbi_dir=\$(readlink -f '${REMOTE_PATH}/ncbi_downloads')
  mkdir -p \"\$cache_dir\" \"\$ncbi_dir\"
  podman unshare chown -R 10001:10001 \"\$cache_dir\" \"\$ncbi_dir\"
  podman unshare chown -R 0:0 \"\$cache_dir/static_assets\"
  test -s \"\$cache_dir/static_assets/manifests/${STATIC_REVISION}.sha256\"
  test -d \"\$cache_dir/static_assets/releases/${STATIC_REVISION}\"
" || die "falló el prewarm o la publicación del snapshot estático"

teardown_stack_command="
  podman rm -f ${BACKGROUND_WORKER_NAME} >/dev/null 2>&1 || true
  podman stop cgv-shinyproxy >/dev/null 2>&1 || true
  session_ids=\$(podman ps -aq --filter name=sp-container- 2>/dev/null || true)
  if [ -n \"\$session_ids\" ]; then podman rm -f \$session_ids >/dev/null; fi
  podman pod rm -f pod_app >/dev/null 2>&1 || true
  podman rm -f cgv-shinyproxy cgv-nginx cgv-docker-socket-proxy >/dev/null 2>&1 || true
  podman network rm sp-control >/dev/null 2>&1 || true
  podman network rm sp-net >/dev/null 2>&1 || true
"

start_background_worker() {
  local expected_image="$1"
  rssh "set -e
    podman rm -f '${BACKGROUND_WORKER_NAME}' >/dev/null 2>&1 || true
    rm -f '${APP_DIR}/cache/background_reports/worker.ready'
    podman run -d \\
      --name '${BACKGROUND_WORKER_NAME}' \\
      --restart unless-stopped \\
      --init \\
      --no-healthcheck \\
      --user 10001:10001 \\
      --network sp-net \\
      --read-only \\
      --security-opt no-new-privileges \\
      --cap-drop all \\
      --memory '${BACKGROUND_REPORT_MEMORY}' \\
      --pids-limit 512 \\
      --shm-size 512m \\
      --tmpfs /tmp:rw,nosuid,nodev,noexec,size=1g,mode=1777 \\
      --env-file '${REMOTE_EMAIL_ENV}' \\
      --env APP_DIR=/app \\
      --env HOME=/tmp/cgv-worker \\
      --env CGV_DATA_ROOT=/app \\
      --env CGV_CACHE_DIR=/app/cache \\
      --env CGV_NCBI_DOWNLOADS_DIR=/app/ncbi_downloads \\
      --env CGV_PUBLIC_BASE_URL='https://${PUBLIC_HOSTNAME}' \\
      --env APP_SHARED_REPORTS_ENABLED=1 \\
      --env APP_BACKGROUND_REPORTS_ENABLED=1 \\
      --env APP_BACKGROUND_REPORT_POLL_SECONDS=3 \\
      --env APP_BACKGROUND_REPORT_TIMEOUT_MINUTES=30 \\
      --env APP_BACKGROUND_REPORT_FUTURE_WORKERS=2 \\
      --env APP_BACKGROUND_REPORT_LASTZ_WORKERS=1 \\
      --env APP_LASTZ_GLOBAL_WORKERS='${APP_LASTZ_GLOBAL_WORKERS}' \\
      --env APP_LASTZ_GLOBAL_QUEUE_WAIT_SECONDS=1800 \\
      --env APP_PREWARM_ON_START=0 \\
      --env APP_PREWARM_BLOCK_START=0 \\
      --env APP_SESSION_METRICS=0 \\
      --env CHROMOTE_CHROME=/usr/bin/google-chrome \\
      --volume '${APP_DIR}/annotations:/app/annotations:ro' \\
      --volume '${APP_DIR}/genomes:/app/genomes:ro' \\
      --volume '${APP_DIR}/go_annotations:/app/go_annotations:ro' \\
      --volume '${APP_DIR}/data:/app/data:ro' \\
      --volume '${APP_DIR}/cache:/app/cache' \\
      --volume '${REMOTE_PATH}/ncbi_downloads:/app/ncbi_downloads' \\
      '${expected_image}' \\
      Rscript /app/scripts/background_report_worker.R >/dev/null
  "
}

wait_for_release() {
  local expected_image="$1"
  local require_worker="${2:-1}"
  rssh "set -e
    for i in \$(seq 1 120); do
      socket_state=\$(podman inspect cgv-docker-socket-proxy --format '{{.State.Health.Status}}' 2>/dev/null || true)
      delegate=''
      for candidate in \$(podman ps --filter name=sp-container- --filter status=running --format '{{.Names}}' 2>/dev/null || true); do
        candidate_image=\$(podman inspect \"\$candidate\" --format '{{.ImageName}}' 2>/dev/null || true)
        if [ \"\$candidate_image\" = '${expected_image}' ]; then
          delegate=\"\$candidate\"
          break
        fi
      done
      worker_state=\$(podman inspect '${BACKGROUND_WORKER_NAME}' --format '{{.State.Status}}' 2>/dev/null || true)
      worker_image=\$(podman inspect '${BACKGROUND_WORKER_NAME}' --format '{{.ImageName}}' 2>/dev/null || true)
      codes=\$(python3 - <<'PY'
import http.client
out=[]
for port in (18081, 3838):
    try:
        c=http.client.HTTPConnection('127.0.0.1', port, timeout=3)
        c.request('HEAD', '/')
        out.append(str(c.getresponse().status))
        c.close()
    except Exception:
        out.append('000')
print(' '.join(out))
PY
)
      proxy_code=\$(printf '%s' \"\$codes\" | awk '{print \$1}')
      nginx_code=\$(printf '%s' \"\$codes\" | awk '{print \$2}')
      worker_ok=0
      if [ '${require_worker}' = 0 ]; then
        worker_ok=1
      elif [ \"\$worker_state\" = running ] && [ \"\$worker_image\" = '${expected_image}' ] && \
           podman logs --tail 80 '${BACKGROUND_WORKER_NAME}' 2>&1 | grep -q '\[background-report-worker\] ready'; then
        if podman exec '${BACKGROUND_WORKER_NAME}' grep -q 'worker.ready' /app/scripts/background_report_worker.R 2>/dev/null; then
          if podman exec '${BACKGROUND_WORKER_NAME}' test -s /app/cache/background_reports/worker.ready 2>/dev/null; then worker_ok=1; fi
        else
          worker_ok=1
        fi
      fi
      if [ \"\$socket_state\" = healthy ] && [ \"\$worker_ok\" = 1 ] && printf '%s' \"\$proxy_code\" | grep -Eq '^[23][0-9][0-9]$' && printf '%s' \"\$nginx_code\" | grep -Eq '^[23][0-9][0-9]$' && [ -n \"\$delegate\" ]; then
        echo \"ready: socket=\$socket_state proxy=\$proxy_code nginx=\$nginx_code delegate=\$delegate worker=\${worker_state:-not-required} wait=\${i}s\"
        exit 0
      fi
      sleep 1
    done
    echo \"READINESS_GUARD_FAILED: expected_image=${expected_image} delegate=\${delegate:-missing} socket=\${socket_state:-missing} proxy=\${proxy_code:-000} nginx=\${nginx_code:-000} worker=\${worker_state:-missing} worker_ok=\${worker_ok:-0}\" >&2
    podman ps -a --format '{{.Names}}|{{.Image}}|{{.Status}}|{{.Ports}}' >&2
    podman logs --tail 120 cgv-shinyproxy >&2 || true
    podman logs --tail 120 '${BACKGROUND_WORKER_NAME}' >&2 || true
    exit 1
  "
}

verify_static_release() {
  local expected_revision="$1"
  local expected_image="$2"
  rssh "set -e
    cd '${APP_DIR}'
    python3 scripts/verify_static_asset_http.py \
      --base-url http://127.0.0.1:3838 \
      --revision '${expected_revision}'
    delegate=''
    for i in \$(seq 1 15); do
      for candidate in \$(podman ps --filter name=sp-container- --filter status=running --format '{{.Names}}' 2>/dev/null || true); do
        candidate_image=\$(podman inspect \"\$candidate\" --format '{{.ImageName}}' 2>/dev/null || true)
        if [ \"\$candidate_image\" = '${expected_image}' ]; then
          delegate=\"\$candidate\"
          break
        fi
      done
      [ -n \"\$delegate\" ] && break
      sleep 1
    done
    if [ -z \"\$delegate\" ]; then
      echo 'STATIC_GUARD_FAILED: delegate-missing expected_image=${expected_image}' >&2
      exit 1
    fi
    if ! podman inspect \"\$delegate\" --format '{{range .Config.Env}}{{println .}}{{end}}' | \
         grep -qx 'APP_ASSET_VERSION=${expected_revision}'; then
      echo 'STATIC_GUARD_FAILED: delegate-asset-version' >&2
      exit 1
    fi
    if ! podman inspect \"\$delegate\" --format '{{range .Config.Env}}{{println .}}{{end}}' | \
         grep -qx 'APP_STATIC_BASE_URL=/cgv-static/${expected_revision}'; then
      echo 'STATIC_GUARD_FAILED: delegate-static-base' >&2
      exit 1
    fi
    delegate_memory=\$(podman inspect \"\$delegate\" --format '{{.HostConfig.Memory}}')
    if [ \"\$delegate_memory\" != '${COLORS_CONTAINER_MEMORY_BYTES}' ]; then
      echo 'MEMORY_GUARD_FAILED: delegate-memory-limit' >&2
      exit 1
    fi
    delegate_command=\$(podman inspect \"\$delegate\" --format '{{json .Config.Cmd}}')
    if ! printf '%s' \"\$delegate_command\" | python3 -c 'import json,sys; sys.exit(json.load(sys.stdin) != [\"bash\", \"/app/deploy/docker/run-app.sh\"])'; then
      echo 'STARTUP_GUARD_FAILED: delegate-command' >&2
      exit 1
    fi
    broker_policy=\$(podman inspect cgv-shinyproxy --format '{{range .Config.Env}}{{println .}}{{end}}' | sed -n 's/^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=//p')
    delegate_policy=\$(podman inspect \"\$delegate\" --format '{{range .Config.Env}}{{println .}}{{end}}' | sed -n 's/^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=//p')
    case \"\$broker_policy\" in
      0|1) ;;
      *) echo 'STATIC_GUARD_FAILED: broker-orthology-policy' >&2; exit 1 ;;
    esac
    case \"\$delegate_policy\" in
      0|1) ;;
      *) echo 'STATIC_GUARD_FAILED: delegate-orthology-policy' >&2; exit 1 ;;
    esac
    if [ \"\$delegate_policy\" != \"\$broker_policy\" ]; then
      echo 'STATIC_GUARD_FAILED: orthology-policy-mismatch' >&2
      exit 1
    fi
    application_perf=\$(sed -n 's/^        APP_PERF_TIMING: \"\([01]\)\"$/\1/p' '${APP_DIR}/shinyproxy/application.yml')
    delegate_perf=\$(podman inspect \"\$delegate\" --format '{{range .Config.Env}}{{println .}}{{end}}' | sed -n 's/^APP_PERF_TIMING=//p')
    if [ \"\$application_perf\" != '${COLORS_PERF_TIMING}' ]; then
      echo 'STATIC_GUARD_FAILED: application-perf-timing' >&2
      exit 1
    fi
    case \"\$delegate_perf\" in
      0|1) ;;
      *) echo 'STATIC_GUARD_FAILED: delegate-perf-timing' >&2; exit 1 ;;
    esac
    if [ \"\$delegate_perf\" != \"\$application_perf\" ]; then
      echo 'STATIC_GUARD_FAILED: perf-timing-mismatch' >&2
      exit 1
    fi
    application_eager=\$(sed -n -E 's/^        (APP_ORTHO_SUSPEND_HIDDEN|APP_HOMO_DEFER_SEQUENCE|APP_ORTHO_DEFER_SEQUENCE|APP_FOOTER_DEFER_SEQUENCE|APP_DEFER_FEATURE_GC|APP_HOMO_RENDER_CHUNK_SIZE|APP_HOMO_AUTO_RENDER_DELAY_MS|APP_ORTHO_RENDER_CHUNK_SIZE|APP_ORTHO_AUTO_RENDER_MORE|APP_ORTHO_AUTO_RENDER_DELAY_MS|APP_HOMO_INITIAL_VISIBLE|APP_ORTHO_INITIAL_VISIBLE|APP_ISOFORM_RENDER_BATCH_SIZE|APP_ISOFORM_RENDER_BATCH_DELAY_MS|APP_ORTHO_SERVER_RENDER_NUDGE): \"([^\"]+)\"$/\1=\2/p' '${APP_DIR}/shinyproxy/application.yml')
    delegate_env=\$(podman inspect \"\$delegate\" --format '{{range .Config.Env}}{{println .}}{{end}}')
    for expected_env in \
      'APP_ORTHO_SUSPEND_HIDDEN=${COLORS_ORTHO_SUSPEND_HIDDEN}' \
      'APP_HOMO_DEFER_SEQUENCE=${COLORS_HOMO_DEFER_SEQUENCE}' \
      'APP_ORTHO_DEFER_SEQUENCE=${COLORS_ORTHO_DEFER_SEQUENCE}' \
      'APP_FOOTER_DEFER_SEQUENCE=${COLORS_FOOTER_DEFER_SEQUENCE}' \
      'APP_DEFER_FEATURE_GC=${COLORS_DEFER_FEATURE_GC}' \
      'APP_HOMO_RENDER_CHUNK_SIZE=${COLORS_HOMO_RENDER_CHUNK_SIZE}' \
      'APP_HOMO_AUTO_RENDER_DELAY_MS=${COLORS_HOMO_AUTO_RENDER_DELAY_MS}' \
      'APP_ORTHO_RENDER_CHUNK_SIZE=${COLORS_ORTHO_RENDER_CHUNK_SIZE}' \
      'APP_ORTHO_AUTO_RENDER_MORE=${COLORS_ORTHO_AUTO_RENDER_MORE}' \
      'APP_ORTHO_AUTO_RENDER_DELAY_MS=${COLORS_ORTHO_AUTO_RENDER_DELAY_MS}' \
      'APP_HOMO_INITIAL_VISIBLE=${COLORS_HOMO_INITIAL_VISIBLE}' \
      'APP_ORTHO_INITIAL_VISIBLE=${COLORS_ORTHO_INITIAL_VISIBLE}' \
      'APP_ISOFORM_RENDER_BATCH_SIZE=${COLORS_ISOFORM_RENDER_BATCH_SIZE}' \
      'APP_ISOFORM_RENDER_BATCH_DELAY_MS=${COLORS_ISOFORM_RENDER_BATCH_DELAY_MS}' \
      'APP_ORTHO_SERVER_RENDER_NUDGE=${COLORS_ORTHO_SERVER_RENDER_NUDGE}'; do
      env_key=\${expected_env%%=*}
      expected_value=\${expected_env#*=}
      application_count=\$(printf '%s\n' \"\$application_eager\" | grep -c \"^\${env_key}=\" || true)
      application_value=\$(printf '%s\n' \"\$application_eager\" | sed -n \"s/^\${env_key}=//p\")
      delegate_count=\$(printf '%s\n' \"\$delegate_env\" | grep -c \"^\${env_key}=\" || true)
      delegate_value=\$(printf '%s\n' \"\$delegate_env\" | sed -n \"s/^\${env_key}=//p\")
      if [ \"\$application_count\" != 1 ] || [ \"\$application_value\" != \"\$expected_value\" ] || \
         [ \"\$delegate_count\" != 1 ] || [ \"\$delegate_value\" != \"\$expected_value\" ]; then
        echo \"STATIC_GUARD_FAILED: eager-profile-\${env_key}\" >&2
        exit 1
      fi
    done
    if printf '%s\n' \"\$delegate_env\" | grep -q '^FEEDBACK_RESEND_API_KEY='; then
      echo 'la sesión pública recibió FEEDBACK_RESEND_API_KEY' >&2
      exit 1
    fi
  "
}

rollback_release() {
  echo "ROLLBACK: restaurando ${CURRENT_IMAGE}..." >&2
  if ! rssh "set -e
    cd '${APP_DIR}'
    cp -p '${BACKUP_DIR}/app.env' .env
    cp -p '${BACKUP_DIR}/docker-compose.shinyproxy.colors.yml' docker-compose.shinyproxy.colors.yml
    cp -p '${BACKUP_DIR}/application.yml' shinyproxy/application.yml
    cp -p '${BACKUP_DIR}/cgv-shinyproxy-colors.conf' deploy/nginx/cgv-shinyproxy-colors.conf
    if [ -f '${BACKUP_DIR}/background-report-email.env' ]; then
      cp -p '${BACKUP_DIR}/background-report-email.env' '${REMOTE_EMAIL_ENV}'
    else
      rm -f '${REMOTE_EMAIL_ENV}'
    fi
    ${teardown_stack_command}
    CGV_IMAGE='${CURRENT_IMAGE}' SHINYPROXY_IMAGE='${SHINYPROXY_IMAGE}' \
      podman-compose -f docker-compose.shinyproxy.colors.yml up -d
  "; then
    return 1
  fi
  start_background_worker "$CURRENT_IMAGE" && wait_for_release "$CURRENT_IMAGE" 1
}

echo ""
echo "[6/7] Cambiando producción a ${NEW_IMAGE}..."
set +e
rssh "set -e
  cd '${APP_DIR}'
  grep -q '^CGV_IMAGE=' .env
  test \"\$(grep -c '^CGV_IMAGE=' .env)\" = 1
  test \"\$(grep -c '^APP_ASSET_VERSION=' .env || true)\" -le 1
  test \"\$(grep -c '^APP_STATIC_BASE_URL=' .env || true)\" -le 1
  policy_count=\$(grep -c '^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=' .env || true)
  if [ \"\$policy_count\" -gt 1 ]; then
    echo 'POLICY_GUARD_FAILED: env-duplicate-orthology-policy' >&2
    exit 1
  fi
  if [ \"\$policy_count\" = 1 ] && ! grep -Eq '^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=[01]$' .env; then
    echo 'POLICY_GUARD_FAILED: env-invalid-orthology-policy' >&2
    exit 1
  fi
  orthology_policy=0
  if [ \"\$policy_count\" = 1 ]; then
    orthology_policy=\$(sed -n 's/^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=//p' .env)
  fi
  env_candidate='${COLORS_ENV_CANDIDATE}'
  cp -p .env \"\$env_candidate\"
  sed -i -E 's|^CGV_IMAGE=.*|CGV_IMAGE=${NEW_IMAGE}|' \"\$env_candidate\"
  if grep -q '^APP_ASSET_VERSION=' \"\$env_candidate\"; then
    sed -i -E 's|^APP_ASSET_VERSION=.*|APP_ASSET_VERSION=${STATIC_REVISION}|' \"\$env_candidate\"
  else
    printf '%s\n' 'APP_ASSET_VERSION=${STATIC_REVISION}' >> \"\$env_candidate\"
  fi
  if grep -q '^APP_STATIC_BASE_URL=' \"\$env_candidate\"; then
    sed -i -E 's|^APP_STATIC_BASE_URL=.*|APP_STATIC_BASE_URL=/cgv-static/${STATIC_REVISION}|' \"\$env_candidate\"
  else
    printf '%s\n' 'APP_STATIC_BASE_URL=/cgv-static/${STATIC_REVISION}' >> \"\$env_candidate\"
  fi
  if grep -q '^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=' \"\$env_candidate\"; then
    sed -i -E \"s|^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=.*|APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=\$orthology_policy|\" \"\$env_candidate\"
  else
    printf '%s\n' \"APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=\$orthology_policy\" >> \"\$env_candidate\"
  fi
  grep -qx 'APP_ASSET_VERSION=${STATIC_REVISION}' \"\$env_candidate\"
  grep -qx 'APP_STATIC_BASE_URL=/cgv-static/${STATIC_REVISION}' \"\$env_candidate\"
  grep -qx \"APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=\$orthology_policy\" \"\$env_candidate\"
  if grep -q '^SP_CONTAINER_MEMORY=' \"\$env_candidate\"; then
    sed -i -E 's|^SP_CONTAINER_MEMORY=.*|SP_CONTAINER_MEMORY=${COLORS_CONTAINER_MEMORY}|' \"\$env_candidate\"
  else
    printf '%s\n' 'SP_CONTAINER_MEMORY=${COLORS_CONTAINER_MEMORY}' >> \"\$env_candidate\"
  fi
  test -s '${COLORS_APPLICATION_CANDIDATE}'
  test -s '${COLORS_COMPOSE_CANDIDATE}'
  test -s '${COLORS_NGINX_CANDIDATE}'
  application_policy=\$(sed -n 's/^        APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: \"\([01]\)\"$/\1/p' '${COLORS_APPLICATION_CANDIDATE}')
  compose_policy=\$(sed -n 's/^      APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY: \"\([01]\)\"$/\1/p' '${COLORS_COMPOSE_CANDIDATE}')
  test \"\$application_policy\" = \"\$orthology_policy\"
  test \"\$compose_policy\" = \"\$orthology_policy\"
  ! grep -Fq '\${APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY' '${COLORS_APPLICATION_CANDIDATE}'
  ! grep -Fq '\${APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY' '${COLORS_COMPOSE_CANDIDATE}'
  podman-compose --env-file \"\$env_candidate\" -f '${COLORS_COMPOSE_CANDIDATE}' config >/dev/null
  mv '${COLORS_APPLICATION_CANDIDATE}' '${APP_DIR}/shinyproxy/application.yml'
  mv '${COLORS_COMPOSE_CANDIDATE}' '${COMPOSE_FILE}'
  mv '${COLORS_NGINX_CANDIDATE}' '${COLORS_NGINX_CONFIG}'
  mv \"\$env_candidate\" .env
  ${teardown_stack_command}
  CGV_IMAGE='${NEW_IMAGE}' SHINYPROXY_IMAGE='${SHINYPROXY_IMAGE}' \
    podman-compose -f docker-compose.shinyproxy.colors.yml up -d
"
CUTOVER_STATUS=$?
if [[ "$CUTOVER_STATUS" -eq 0 ]]; then
  start_background_worker "$NEW_IMAGE"
  CUTOVER_STATUS=$?
fi
set -e

VALIDATION_GUARD=""
if [[ "$CUTOVER_STATUS" -ne 0 ]]; then
  VALIDATION_GUARD="cutover"
elif ! wait_for_release "$NEW_IMAGE" 1; then
  VALIDATION_GUARD="release-readiness"
elif ! verify_static_release "$STATIC_REVISION" "$NEW_IMAGE"; then
  VALIDATION_GUARD="static-release"
fi

if [[ -n "$VALIDATION_GUARD" ]]; then
  echo "ERROR: la nueva release no superó la guarda ${VALIDATION_GUARD}; se inicia rollback." >&2
  if rollback_release; then
    die "deploy revertido correctamente a ${CURRENT_IMAGE}"
  fi
  die "deploy y rollback fallaron; respaldo disponible en ${BACKUP_DIR}"
fi

echo ""
echo "[7/7] Verificación final..."
set +e
rssh "set -e
  fail_guard() { echo \"FINAL_GUARD_FAILED: \$1\" >&2; exit 1; }
  test \"\$(podman inspect cgv-shinyproxy --format '{{.ImageName}}')\" = '${SHINYPROXY_RUNTIME_IMAGE}' || fail_guard shinyproxy-image
  if podman inspect cgv-shinyproxy --format '{{range .Mounts}}{{println .Destination}}{{end}}' | grep -qx '/var/run/docker.sock'; then fail_guard direct-docker-socket; fi
  if podman inspect cgv-shinyproxy --format '{{range .Mounts}}{{println .Destination}}{{end}}' | grep -qx '/opt/shinyproxy/env/cgv.env'; then fail_guard full-env-mount; fi
  broker_env=\$(podman inspect cgv-shinyproxy --format '{{range .Config.Env}}{{println .}}{{end}}')
  printf '%s\n' \"\$broker_env\" | grep -qx 'APP_ASSET_VERSION=${STATIC_REVISION}' || fail_guard broker-asset-version
  printf '%s\n' \"\$broker_env\" | grep -qx 'APP_STATIC_BASE_URL=/cgv-static/${STATIC_REVISION}' || fail_guard broker-static-base
  broker_policy=\$(printf '%s\n' \"\$broker_env\" | sed -n 's/^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=//p')
  case \"\$broker_policy\" in 0|1) ;; *) fail_guard broker-orthology-policy ;; esac
  if printf '%s\n' \"\$broker_env\" | grep -q '^FEEDBACK_RESEND_API_KEY='; then fail_guard broker-feedback-secret; fi
  test \"\$(podman network inspect sp-control --format '{{.Internal}}')\" = true || fail_guard sp-control-not-internal
  legacy=\$(podman ps --filter ancestor=localhost/cgv:1.0.0 -q | wc -l | tr -d ' ')
  test \"\$legacy\" = 0 || fail_guard legacy-cgv-container
  podman logs --since 5m cgv-shinyproxy 2>&1 | grep -q 'Started ShinyProxy 3.2.4' || fail_guard shinyproxy-start-log
  test \"\$(podman inspect '${BACKGROUND_WORKER_NAME}' --format '{{.State.Status}}')\" = running || fail_guard worker-not-running
  test \"\$(podman inspect '${BACKGROUND_WORKER_NAME}' --format '{{.ImageName}}')\" = '${NEW_IMAGE}' || fail_guard worker-image
  podman logs --tail 80 '${BACKGROUND_WORKER_NAME}' 2>&1 | grep -q '\[background-report-worker\] ready' || fail_guard worker-ready-log
  podman exec '${BACKGROUND_WORKER_NAME}' test -s /app/cache/background_reports/worker.ready || fail_guard worker-ready-marker
  podman exec '${BACKGROUND_WORKER_NAME}' grep -q '^browser=Chrome/' /app/cache/background_reports/worker.ready || fail_guard worker-browser-marker
  delegate=''
  for candidate in \$(podman ps --filter name=sp-container- --filter status=running --format '{{.Names}}' 2>/dev/null || true); do
    candidate_image=\$(podman inspect \"\$candidate\" --format '{{.ImageName}}' 2>/dev/null || true)
    if [ \"\$candidate_image\" = '${NEW_IMAGE}' ]; then delegate=\"\$candidate\"; break; fi
  done
  test -n \"\$delegate\" || fail_guard delegate-missing
  delegate_policy=\$(podman inspect \"\$delegate\" --format '{{range .Config.Env}}{{println .}}{{end}}' | sed -n 's/^APP_ORTHO_REQUIRE_VERIFIED_ORTHOLOGY=//p')
  case \"\$delegate_policy\" in 0|1) ;; *) fail_guard delegate-orthology-policy ;; esac
  test \"\$delegate_policy\" = \"\$broker_policy\" || fail_guard orthology-policy-mismatch
  application_perf=\$(sed -n 's/^        APP_PERF_TIMING: \"\([01]\)\"$/\1/p' '${APP_DIR}/shinyproxy/application.yml')
  test \"\$application_perf\" = '${COLORS_PERF_TIMING}' || fail_guard application-perf-timing
  delegate_perf=\$(podman inspect \"\$delegate\" --format '{{range .Config.Env}}{{println .}}{{end}}' | sed -n 's/^APP_PERF_TIMING=//p')
  case \"\$delegate_perf\" in 0|1) ;; *) fail_guard delegate-perf-timing ;; esac
  test \"\$delegate_perf\" = \"\$application_perf\" || fail_guard perf-timing-mismatch
  application_eager=\$(sed -n -E 's/^        (APP_ORTHO_SUSPEND_HIDDEN|APP_HOMO_DEFER_SEQUENCE|APP_ORTHO_DEFER_SEQUENCE|APP_FOOTER_DEFER_SEQUENCE|APP_DEFER_FEATURE_GC|APP_HOMO_RENDER_CHUNK_SIZE|APP_HOMO_AUTO_RENDER_DELAY_MS|APP_ORTHO_RENDER_CHUNK_SIZE|APP_ORTHO_AUTO_RENDER_MORE|APP_ORTHO_AUTO_RENDER_DELAY_MS|APP_HOMO_INITIAL_VISIBLE|APP_ORTHO_INITIAL_VISIBLE|APP_ISOFORM_RENDER_BATCH_SIZE|APP_ISOFORM_RENDER_BATCH_DELAY_MS|APP_ORTHO_SERVER_RENDER_NUDGE): \"([^\"]+)\"$/\1=\2/p' '${APP_DIR}/shinyproxy/application.yml')
  delegate_env=\$(podman inspect \"\$delegate\" --format '{{range .Config.Env}}{{println .}}{{end}}')
  for expected_env in \
    'APP_ORTHO_SUSPEND_HIDDEN=${COLORS_ORTHO_SUSPEND_HIDDEN}' \
    'APP_HOMO_DEFER_SEQUENCE=${COLORS_HOMO_DEFER_SEQUENCE}' \
    'APP_ORTHO_DEFER_SEQUENCE=${COLORS_ORTHO_DEFER_SEQUENCE}' \
    'APP_FOOTER_DEFER_SEQUENCE=${COLORS_FOOTER_DEFER_SEQUENCE}' \
    'APP_DEFER_FEATURE_GC=${COLORS_DEFER_FEATURE_GC}' \
    'APP_HOMO_RENDER_CHUNK_SIZE=${COLORS_HOMO_RENDER_CHUNK_SIZE}' \
    'APP_HOMO_AUTO_RENDER_DELAY_MS=${COLORS_HOMO_AUTO_RENDER_DELAY_MS}' \
    'APP_ORTHO_RENDER_CHUNK_SIZE=${COLORS_ORTHO_RENDER_CHUNK_SIZE}' \
    'APP_ORTHO_AUTO_RENDER_MORE=${COLORS_ORTHO_AUTO_RENDER_MORE}' \
    'APP_ORTHO_AUTO_RENDER_DELAY_MS=${COLORS_ORTHO_AUTO_RENDER_DELAY_MS}' \
    'APP_HOMO_INITIAL_VISIBLE=${COLORS_HOMO_INITIAL_VISIBLE}' \
    'APP_ORTHO_INITIAL_VISIBLE=${COLORS_ORTHO_INITIAL_VISIBLE}' \
    'APP_ISOFORM_RENDER_BATCH_SIZE=${COLORS_ISOFORM_RENDER_BATCH_SIZE}' \
    'APP_ISOFORM_RENDER_BATCH_DELAY_MS=${COLORS_ISOFORM_RENDER_BATCH_DELAY_MS}' \
    'APP_ORTHO_SERVER_RENDER_NUDGE=${COLORS_ORTHO_SERVER_RENDER_NUDGE}'; do
    env_key=\${expected_env%%=*}
    expected_value=\${expected_env#*=}
    application_count=\$(printf '%s\n' \"\$application_eager\" | grep -c \"^\${env_key}=\" || true)
    application_value=\$(printf '%s\n' \"\$application_eager\" | sed -n \"s/^\${env_key}=//p\")
    delegate_count=\$(printf '%s\n' \"\$delegate_env\" | grep -c \"^\${env_key}=\" || true)
    delegate_value=\$(printf '%s\n' \"\$delegate_env\" | sed -n \"s/^\${env_key}=//p\")
    test \"\$application_count\" = 1 || fail_guard \"eager-profile-application-duplicate-\${env_key}\"
    test \"\$application_value\" = \"\$expected_value\" || fail_guard \"eager-profile-application-value-\${env_key}\"
    test \"\$delegate_count\" = 1 || fail_guard \"eager-profile-delegate-duplicate-\${env_key}\"
    test \"\$delegate_value\" = \"\$expected_value\" || fail_guard \"eager-profile-delegate-value-\${env_key}\"
  done
  podman ps --format '{{.Names}}|{{.Image}}|{{.Status}}|{{.Networks}}'
"
FINAL_STATUS=$?
set -e

if [[ "$FINAL_STATUS" -ne 0 ]]; then
  echo "ERROR: falló una guarda final de seguridad; se inicia rollback." >&2
  if rollback_release; then
    die "deploy revertido correctamente a ${CURRENT_IMAGE}"
  fi
  die "falló la guarda final y también el rollback; usa ${BACKUP_DIR}"
fi

PUBLIC_CODE="000"
for public_attempt in 1 2 3 4 5 6; do
  PUBLIC_CODE="$(curl -sS -I --max-time 15 -o /dev/null -w '%{http_code}' "https://${PUBLIC_HOSTNAME}/" 2>/dev/null || true)"
  if [[ "$PUBLIC_CODE" =~ ^[23][0-9][0-9]$ ]]; then
    break
  fi
  sleep 5
done
if [[ ! "$PUBLIC_CODE" =~ ^[23][0-9][0-9]$ ]]; then
  echo "ERROR: la URL pública respondió HTTP ${PUBLIC_CODE:-000}; se inicia rollback." >&2
  if rollback_release; then
    die "deploy revertido correctamente a ${CURRENT_IMAGE}"
  fi
  die "falló la URL pública y también el rollback; usa ${BACKUP_DIR}"
fi
if ! python3 -B "${SCRIPT_DIR}/scripts/verify_static_asset_http.py" \
  --base-url "https://${PUBLIC_HOSTNAME}" \
  --revision "$STATIC_REVISION"; then
  echo "ERROR: los assets inmutables fallaron por la URL pública; se inicia rollback." >&2
  if rollback_release; then
    die "deploy revertido correctamente a ${CURRENT_IMAGE}"
  fi
  die "falló la entrega estática pública y también el rollback; usa ${BACKUP_DIR}"
fi

echo ""
echo "============================================"
echo "  Deploy completado"
echo "  Release:  ${NEW_IMAGE}"
echo "  Anterior: ${CURRENT_IMAGE}"
echo "  Respaldo: ${BACKUP_DIR}"
echo "  Público:  HTTP ${PUBLIC_CODE}"
echo "  Reportes: worker activo (${BACKGROUND_WORKER_NAME})"
echo "============================================"
