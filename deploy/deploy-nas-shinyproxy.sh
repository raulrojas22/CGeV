#!/usr/bin/env bash
# ============================================================
# deploy-nas-shinyproxy.sh — Despliega CGV con ShinyProxy en el NAS
# URL: https://cgvapp.com
# Soporta 3-5 usuarios simultaneos con contenedores por usuario.
# ShinyProxy queda sin login y limitado por SP_MAX_TOTAL_INSTANCES.
# ============================================================
set -euo pipefail

NAS_USER="${NAS_USER:-truenas_admin}"
NAS_HOST="${NAS_HOST:-192.168.1.200}"
NAS_PATH="${NAS_PATH:-/mnt/Datos4raro/cgv}"
DEPLOY_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_DIR="$(cd "${DEPLOY_DIR}/.." && pwd)"
LOCAL_APP="${LOCAL_APP:-${SCRIPT_DIR}}"
LOCAL_APP="${LOCAL_APP%/}/"
LOCAL_ENV_FILE="${LOCAL_APP}.env.local"
NAS_PUBLIC_HOSTNAME="${NAS_PUBLIC_HOSTNAME:-cgvapp.com}"
TUNNEL_NAME="cgv"
REMOTE_DOCKER="${REMOTE_DOCKER:-docker}"
NAS_APP_DIR="${NAS_PATH}/app"
local_env_value() {
  local key="$1"
  local fallback="$2"
  local env_file="${LOCAL_APP}/.env"
  local val=""
  if [[ -f "${env_file}" ]]; then
    val="$(grep -E "^${key}=" "${env_file}" | tail -1 | cut -d= -f2- || true)"
    val="${val%\"}"
    val="${val#\"}"
    val="${val%\'}"
    val="${val#\'}"
  fi
  if [[ -n "${val}" ]]; then
    printf '%s' "${val}"
  else
    printf '%s' "${fallback}"
  fi
}

ENV_CGV_IMAGE=""
if [[ -f "${LOCAL_APP}/.env" ]]; then
  ENV_CGV_IMAGE="$(grep -E '^CGV_IMAGE=' "${LOCAL_APP}/.env" | tail -1 | cut -d= -f2- || true)"
  ENV_CGV_IMAGE="${ENV_CGV_IMAGE%\"}"
  ENV_CGV_IMAGE="${ENV_CGV_IMAGE#\"}"
  ENV_CGV_IMAGE="${ENV_CGV_IMAGE%\'}"
  ENV_CGV_IMAGE="${ENV_CGV_IMAGE#\'}"
fi
CGV_IMAGE="${CGV_IMAGE:-${ENV_CGV_IMAGE:-cgv:1.0.0}}"
CGV_DEPS_IMAGE="${CGV_DEPS_IMAGE:-$(local_env_value CGV_DEPS_IMAGE cgv-deps:1.0.0)}"
REBUILD_R_DEPS="${REBUILD_R_DEPS:-0}"
CGV_NGINX_PORT="${CGV_NGINX_PORT:-$(local_env_value CGV_NGINX_PORT 18080)}"
PERF_RUN_LABEL="${PERF_RUN_LABEL:-manual}"
NAS_PERF_TIMING="${NAS_PERF_TIMING:-0}"

# Present complete cards progressively, with the same fixed profile as Colors.
# A stale remote .env must not restore bulk rendering or deferred information.
NAS_ORTHO_SUSPEND_HIDDEN="1"
NAS_HOMO_DEFER_SEQUENCE="0"
NAS_ORTHO_DEFER_SEQUENCE="0"
NAS_FOOTER_DEFER_SEQUENCE="0"
NAS_DEFER_FEATURE_GC="0"
NAS_HOMO_RENDER_CHUNK_SIZE="1"
NAS_HOMO_AUTO_RENDER_DELAY_MS="120"
NAS_ORTHO_RENDER_CHUNK_SIZE="1"
NAS_ORTHO_AUTO_RENDER_MORE="1"
NAS_ORTHO_AUTO_RENDER_DELAY_MS="120"
NAS_HOMO_INITIAL_VISIBLE="1"
NAS_ORTHO_INITIAL_VISIBLE="1"
NAS_ISOFORM_RENDER_BATCH_SIZE="1"
NAS_ISOFORM_RENDER_BATCH_DELAY_MS="120"
NAS_ORTHO_SERVER_RENDER_NUDGE="0"

SSH_SOCK="/tmp/deploy-nas-sp-ssh-$$"
ssh -o ServerAliveInterval=15 -o ServerAliveCountMax=4 -fNM -S "$SSH_SOCK" "${NAS_USER}@${NAS_HOST}"
trap 'ssh -S "$SSH_SOCK" -O exit "${NAS_USER}@${NAS_HOST}" 2>/dev/null' EXIT

nssh() {
  if [[ "${REMOTE_DOCKER}" == sudo* ]]; then
    ssh -tt -S "$SSH_SOCK" "${NAS_USER}@${NAS_HOST}" "$@"
  else
    ssh -S "$SSH_SOCK" "${NAS_USER}@${NAS_HOST}" "$@"
  fi
}

ensure_remote_docker_access() {
  if nssh "${REMOTE_DOCKER} info >/dev/null 2>&1"; then
    return 0
  fi
  if [[ "${REMOTE_DOCKER}" == "docker" ]] && nssh "docker info 2>&1 | grep -qi 'permission denied'"; then
    echo "  Docker requiere permisos elevados en el NAS; usando sudo docker."
    REMOTE_DOCKER="sudo docker"
    nssh "${REMOTE_DOCKER} info >/dev/null"
    return 0
  fi
  echo "ERROR: no se pudo acceder a Docker en el NAS con '${REMOTE_DOCKER}'." >&2
  exit 1
}

echo "============================================"
echo "  CGV ShinyProxy — Deploy to NAS"
echo "  https://${NAS_PUBLIC_HOSTNAME}"
echo "  Modo: multiusuario (1 contenedor/usuario)"
echo "  Telemetría: ${NAS_PERF_TIMING} (etiqueta=${PERF_RUN_LABEL})"
echo "  Render: tarjetas completas, lotes de 1, intervalo de 120 ms"
echo "  R dependencies: ${CGV_DEPS_IMAGE} (rebuild=${REBUILD_R_DEPS})"
echo "============================================"
echo ""

if [[ "${REBUILD_R_DEPS}" != "0" && "${REBUILD_R_DEPS}" != "1" ]]; then
  echo "ERROR: REBUILD_R_DEPS debe ser 0 o 1." >&2
  exit 1
fi
if [[ "${NAS_PERF_TIMING}" != "0" && "${NAS_PERF_TIMING}" != "1" ]]; then
  echo "ERROR: NAS_PERF_TIMING debe ser 0 o 1." >&2
  exit 1
fi
if ! [[ "${PERF_RUN_LABEL}" =~ ^[A-Za-z0-9._-]+$ ]]; then
  echo "ERROR: PERF_RUN_LABEL solo puede contener letras, numeros, punto, guion y guion bajo." >&2
  exit 1
fi
[[ "$NAS_PUBLIC_HOSTNAME" =~ ^[A-Za-z0-9.-]+$ ]] || exit 2

echo "Verificando acceso a Docker en el NAS..."
ensure_remote_docker_access
echo ""

# --- Paso 1: Sincronizar codigo ---
echo "[1/7] Sincronizando codigo al NAS..."
if [[ ! -f "${LOCAL_ENV_FILE}" ]] || ! grep -Eq '^FEEDBACK_RESEND_API_KEY=.+$' "${LOCAL_ENV_FILE}"; then
  echo "ERROR: falta FEEDBACK_RESEND_API_KEY en ${LOCAL_ENV_FILE}; el worker no podria enviar los reportes." >&2
  exit 1
fi
rsync -avz --progress --delete \
  -e "ssh -S $SSH_SOCK" \
  --exclude=annotations --exclude=genomes \
  --exclude=go_annotations --exclude=cache \
  --exclude=ncbi_downloads \
  --exclude=data/alias_index \
  --exclude='/data' --exclude='/www/screencasts' \
  --exclude='/outputs' --exclude='/logs' --exclude='/tmp' \
  --exclude='/desktop' --exclude='/INSTALABLES-FINALES' \
  --exclude='/.env' --exclude='/.env.*' --exclude='/.Renviron' \
  --exclude=.git --exclude=.claude \
  --exclude=node_modules \
  --exclude=.Rapp.history --exclude=.Rhistory \
  --exclude=.codex_backups --exclude=.qodo \
  --exclude='*.docx' --exclude='.env.local' \
  "$LOCAL_APP" \
  "${NAS_USER}@${NAS_HOST}:${NAS_APP_DIR}/"
rsync -az --chmod=Fu=rw,Fgo= \
  -e "ssh -S $SSH_SOCK" \
  "${LOCAL_ENV_FILE}" \
  "${NAS_USER}@${NAS_HOST}:${NAS_APP_DIR}/.env.local"
nssh "chmod 600 '${NAS_APP_DIR}/.env.local'"
nssh "cd '${NAS_APP_DIR}' && python3 -B scripts/verify_guide_assets.py"
echo ""

# --- Paso 1b: Verificar datos grandes en el NAS ---
echo "[1b/7] Verificando datos biologicos persistentes en el NAS..."
nssh "
  set -e
  mkdir -p ${NAS_PATH}
	  for d in annotations genomes go_annotations cache ncbi_downloads; do
	    app_dir='${NAS_APP_DIR}'/\$d
	    shared_dir='${NAS_PATH}'/\$d
	    if [ "\$d" = "ncbi_downloads" ]; then
	      mkdir -p "\$shared_dir"
	    fi
	    if [ ! -e \"\$app_dir\" ] && [ -e \"\$shared_dir\" ]; then
      ln -s \"\$shared_dir\" \"\$app_dir\"
      echo \"  Enlace creado: \$app_dir -> \$shared_dir\"
    elif [ -d \"\$shared_dir\" ] && [ ! -L \"\$app_dir\" ]; then
      needs_link=0
      if [ \"\$d\" = 'annotations' ] && [ -s \"\$shared_dir/registry.tsv\" ] && [ ! -s \"\$app_dir/registry.tsv\" ]; then
        needs_link=1
      elif [ \"\$d\" = 'genomes' ] && [ -s \"\$shared_dir/registry.tsv\" ] && [ ! -s \"\$app_dir/registry.tsv\" ]; then
        needs_link=1
      elif [ \"\$d\" = 'go_annotations' ] && [ -z \"\$(find \"\$app_dir\" -mindepth 1 -maxdepth 1 2>/dev/null | head -1)\" ]; then
        needs_link=1
      elif [ \"\$d\" = 'cache' ] && [ -d \"\$shared_dir/annotation_index\" ] && [ ! -d \"\$app_dir/annotation_index\" ]; then
        needs_link=1
      fi

      if [ \"\$needs_link\" = '1' ]; then
        if [ -z \"\$(find \"\$app_dir\" -mindepth 1 -maxdepth 1 2>/dev/null | head -1)\" ]; then
          rmdir \"\$app_dir\"
          ln -s \"\$shared_dir\" \"\$app_dir\"
          echo \"  Enlace reparado: \$app_dir -> \$shared_dir\"
        else
          echo \"ERROR: \$app_dir existe pero no apunta a los datos persistentes de \$shared_dir\" >&2
          echo \"Para reparar sin borrar datos, revisa en el NAS:\" >&2
          echo \"  ls -la \$app_dir \$shared_dir\" >&2
          echo \"Si \$app_dir esta vacio o incompleto, ejecuta:\" >&2
          echo \"  rmdir \$app_dir && ln -s \$shared_dir \$app_dir\" >&2
          exit 1
        fi
      fi
    fi
  done

  missing=''
  for d in annotations genomes go_annotations; do
    app_dir='${NAS_APP_DIR}'/\$d
    if [ ! -d \"\$app_dir\" ]; then
      missing=\"\$missing \$d\"
    fi
  done
  if [ -n \"\$missing\" ]; then
    echo \"ERROR: faltan directorios de datos en el NAS:\${missing}\" >&2
    echo \"\" >&2
    echo \"Copia los datos una sola vez desde tu Mac al NAS:\" >&2
    echo \"  rsync -avz --progress annotations genomes go_annotations ${NAS_USER}@${NAS_HOST}:${NAS_PATH}/\" >&2
    echo \"  rsync -avz --progress cache ${NAS_USER}@${NAS_HOST}:${NAS_PATH}/\" >&2
    echo \"\" >&2
    echo \"Luego vuelve a correr ./deploy/deploy-nas-shinyproxy.sh\" >&2
    exit 1
  fi

  if [ ! -s '${NAS_APP_DIR}/annotations/registry.tsv' ]; then
    echo \"ERROR: falta ${NAS_APP_DIR}/annotations/registry.tsv\" >&2
    echo \"Si tus datos estan en otro lugar, mueve o enlaza annotations/ hacia ${NAS_APP_DIR}/annotations.\" >&2
    exit 1
  fi
	  echo \"  Datos OK: annotations, genomes, go_annotations, ncbi_downloads\"
	"
echo ""

# --- Paso 3: Construir imagen CGV ---
echo ""
echo "[3/7] Preparando dependencias de R y construyendo '${CGV_IMAGE}' en el NAS..."
nssh "
  set -e
  cd ${NAS_APP_DIR}
  upsert_env() {
    key=\"\$1\"
    value=\"\$2\"
    if grep -q \"^\${key}=\" .env; then
      sed -i \"s#^\${key}=.*#\${key}=\${value}#\" .env
    else
      printf '%s=%s\n' \"\$key\" \"\$value\" >> .env
    fi
  }
  upsert_env CGV_IMAGE '${CGV_IMAGE}'
  upsert_env CGV_DEPS_IMAGE '${CGV_DEPS_IMAGE}'

  if [ '${REBUILD_R_DEPS}' = '1' ] ||
     ! ${REMOTE_DOCKER} image inspect '${CGV_DEPS_IMAGE}' >/dev/null 2>&1 ||
     ! ${REMOTE_DOCKER} run --rm --entrypoint /usr/bin/google-chrome '${CGV_DEPS_IMAGE}' --version >/dev/null 2>&1 ||
     ! ${REMOTE_DOCKER} run --rm '${CGV_DEPS_IMAGE}' Rscript -e 'library(chromote)' >/dev/null 2>&1; then
    echo '  Construyendo ${CGV_DEPS_IMAGE} (incluye Google Chrome/chromote para reportes en segundo plano)...'
    ${REMOTE_DOCKER} build --pull=false -t '${CGV_DEPS_IMAGE}' -f deploy/docker/Dockerfile.dependencies .
  else
    echo '  Reutilizando ${CGV_DEPS_IMAGE}; no se instalarán paquetes R.'
  fi
  ${REMOTE_DOCKER} run --rm --entrypoint /usr/bin/google-chrome '${CGV_DEPS_IMAGE}' --version >/dev/null
  CGV_IMAGE='${CGV_IMAGE}' CGV_DEPS_IMAGE='${CGV_DEPS_IMAGE}' ${REMOTE_DOCKER} compose build
  ${REMOTE_DOCKER} run --rm --entrypoint sh '${CGV_IMAGE}' -c 'cd /app && sha256sum -c deploy/guide-videos.sha256'
"

# --- Paso 4: Prewarming (indices SQLite y snapshot estatico inmutable) ---
echo ""
echo "[4/7] Ejecutando prewarming (indices SQLite, caches y snapshot estatico)..."
nssh "
  set -e
  cd ${NAS_APP_DIR}

  static_image_id=\$(${REMOTE_DOCKER} image inspect --format '{{.Id}}' '${CGV_IMAGE}' | tr -d '\r\n')
  static_revision=\${static_image_id#sha256:}
  if ! printf '%s\n' \"\$static_revision\" | grep -Eq '^[a-f0-9]{64}$'; then
    echo \"ERROR: Docker no devolvio un Id sha256 valido para ${CGV_IMAGE}: \$static_image_id\" >&2
    exit 1
  fi

  chmod +x deploy/docker/setup-prewarm.sh
  CGV_IMAGE='${CGV_IMAGE}' \
  CGV_PUBLISH_STATIC_ASSETS=1 \
  CGV_STATIC_REVISION=\"\$static_revision\" \
  CGV_ANNOTATIONS_DIR='${NAS_APP_DIR}/annotations' \
  CGV_GENOMES_DIR='${NAS_APP_DIR}/genomes' \
  CGV_GO_ANNOTATIONS_DIR='${NAS_APP_DIR}/go_annotations' \
  CGV_DATA_DIR='${NAS_APP_DIR}/data' \
  CGV_CACHE_DIR='${NAS_APP_DIR}/cache' \
  DOCKER_BIN='${REMOTE_DOCKER}' bash deploy/docker/setup-prewarm.sh

  static_release='${NAS_APP_DIR}/cache/static_assets/releases/'\"\$static_revision\"
  if [ ! -s \"\$static_release/healthz.txt\" ]; then
    echo \"ERROR: el snapshot estatico no contiene healthz.txt: \$static_release\" >&2
    exit 1
  fi

  upsert_env() {
    key=\"\$1\"
    value=\"\$2\"
    if grep -q \"^\${key}=\" .env; then
      sed -i \"s#^\${key}=.*#\${key}=\${value}#\" .env
    else
      printf '%s=%s\n' \"\$key\" \"\$value\" >> .env
    fi
  }
  upsert_env CGV_IMAGE '${CGV_IMAGE}'
  upsert_env APP_ASSET_VERSION \"\$static_revision\"
  upsert_env APP_STATIC_BASE_URL \"/cgv-static/\$static_revision\"

  test \"\$(grep '^CGV_IMAGE=' .env | tail -1)\" = \"CGV_IMAGE=${CGV_IMAGE}\"
  test \"\$(grep '^APP_ASSET_VERSION=' .env | tail -1)\" = \"APP_ASSET_VERSION=\$static_revision\"
  test \"\$(grep '^APP_STATIC_BASE_URL=' .env | tail -1)\" = \"APP_STATIC_BASE_URL=/cgv-static/\$static_revision\"
  echo \"  Snapshot estatico listo: \$static_revision\"
"

# --- Corte: candidato construido y prewarm verificado ---
echo "Deteniendo servicios anteriores para activar el candidato..."
nssh "
  echo '  Deteniendo contenedores anteriores...'
  ${REMOTE_DOCKER} stop cgv 2>/dev/null || echo '  (no habia cgv)'
  ${REMOTE_DOCKER} stop cgv-shinyproxy 2>/dev/null || echo '  (no habia shinyproxy)'
  ${REMOTE_DOCKER} stop cgv-nginx 2>/dev/null || echo '  (no habia nginx)'
  ${REMOTE_DOCKER} stop cgv-background-report-worker 2>/dev/null || echo '  (no habia worker de reportes)'
  ${REMOTE_DOCKER} rm cgv cgv-shinyproxy cgv-nginx cgv-background-report-worker 2>/dev/null || true
  stale=\$(${REMOTE_DOCKER} ps -aq --filter name=sp-container- 2>/dev/null || true)
  if [ -n \"\$stale\" ]; then
    echo '  Deteniendo contenedores CGV dinamicos de ShinyProxy...'
    ${REMOTE_DOCKER} stop \$stale >/dev/null 2>&1 || true
    ${REMOTE_DOCKER} rm \$stale >/dev/null 2>&1 || true
  fi
  echo '  Contenedores detenidos y eliminados.'
"

echo ""
echo "Verificando puerto web ${CGV_NGINX_PORT} en el NAS..."
nssh "
  port='${CGV_NGINX_PORT}'
  if command -v ss >/dev/null 2>&1 && ss -ltn 2>/dev/null | awk '{print \$4}' | grep -Eq '(^|:)'\${port}'$'; then
    echo \"ERROR: el puerto \${port} ya esta ocupado en el NAS.\" >&2
    echo \"Prueba otro puerto editando CGV_NGINX_PORT en ${NAS_APP_DIR}/.env o en tu .env local.\" >&2
    exit 1
  fi
  if ${REMOTE_DOCKER} ps --format '{{.Ports}}' | grep -Eq '0\\.0\\.0\\.0:'\"\${port}\"'->|:'\"\${port}\"'->'; then
    echo \"ERROR: Docker ya tiene un contenedor publicando el puerto \${port}.\" >&2
    ${REMOTE_DOCKER} ps --format 'table {{.Names}}\t{{.Ports}}' >&2
    exit 1
  fi
  echo \"  Puerto \${port} disponible.\"
"

# --- Paso 5: Iniciar ShinyProxy ---
echo ""
echo "[5/7] Iniciando ShinyProxy + nginx..."
nssh "
  set -e
  cd ${NAS_APP_DIR}
  for d in annotations genomes go_annotations data cache ncbi_downloads; do
    if [ ! -d '${NAS_APP_DIR}'/\"\$d\" ]; then
      echo \"ERROR: falta la ruta absoluta ${NAS_APP_DIR}/\$d requerida por ShinyProxy.\" >&2
      exit 1
    fi
  done

  echo '  Validando configuración nginx...'
  ${REMOTE_DOCKER} run --rm \
    -v '${NAS_APP_DIR}/deploy/nginx/cgv-shinyproxy.conf:/etc/nginx/conf.d/default.conf:ro' \
    docker.io/library/nginx:alpine nginx -t

  upsert_env() {
    key=\"\$1\"
    value=\"\$2\"
    if grep -q \"^\${key}=\" .env; then
      sed -i \"s#^\${key}=.*#\${key}=\${value}#\" .env
    else
      printf '%s=%s\n' \"\$key\" \"\$value\" >> .env
    fi
  }

  echo '  Fijando rutas absolutas del host NAS en .env...'
  upsert_env CGV_NGINX_PORT '${CGV_NGINX_PORT}'
  upsert_env CGV_PUBLIC_BASE_URL 'https://${NAS_PUBLIC_HOSTNAME}'
  upsert_env SP_ANNOTATIONS_DIR '${NAS_APP_DIR}/annotations'
  upsert_env SP_GENOMES_DIR '${NAS_APP_DIR}/genomes'
  upsert_env SP_GO_ANNOTATIONS_DIR '${NAS_APP_DIR}/go_annotations'
  upsert_env SP_DATA_DIR '${NAS_APP_DIR}/data'
  upsert_env SP_CACHE_DIR '${NAS_APP_DIR}/cache'
  upsert_env SP_NCBI_DOWNLOADS_DIR '${NAS_APP_DIR}/ncbi_downloads'
  upsert_env SP_APP_PERF_TIMING '${NAS_PERF_TIMING}'
  upsert_env SP_PERF_RUN_LABEL '${PERF_RUN_LABEL}'
  upsert_env SP_ORTHO_SUSPEND_HIDDEN '${NAS_ORTHO_SUSPEND_HIDDEN}'
  upsert_env SP_HOMO_DEFER_SEQUENCE '${NAS_HOMO_DEFER_SEQUENCE}'
  upsert_env SP_ORTHO_DEFER_SEQUENCE '${NAS_ORTHO_DEFER_SEQUENCE}'
  upsert_env SP_FOOTER_DEFER_SEQUENCE '${NAS_FOOTER_DEFER_SEQUENCE}'
  upsert_env SP_DEFER_FEATURE_GC '${NAS_DEFER_FEATURE_GC}'
  upsert_env SP_HOMO_RENDER_CHUNK_SIZE '${NAS_HOMO_RENDER_CHUNK_SIZE}'
  upsert_env SP_HOMO_AUTO_RENDER_DELAY_MS '${NAS_HOMO_AUTO_RENDER_DELAY_MS}'
  upsert_env SP_ISOFORM_RENDER_BATCH_SIZE '${NAS_ISOFORM_RENDER_BATCH_SIZE}'
  upsert_env SP_ISOFORM_RENDER_BATCH_DELAY_MS '${NAS_ISOFORM_RENDER_BATCH_DELAY_MS}'
  upsert_env SP_ORTHO_RENDER_CHUNK_SIZE '${NAS_ORTHO_RENDER_CHUNK_SIZE}'
  upsert_env SP_ORTHO_AUTO_RENDER_MORE '${NAS_ORTHO_AUTO_RENDER_MORE}'
  upsert_env SP_ORTHO_AUTO_RENDER_DELAY_MS '${NAS_ORTHO_AUTO_RENDER_DELAY_MS}'
  upsert_env SP_HOMO_INITIAL_VISIBLE '${NAS_HOMO_INITIAL_VISIBLE}'
  upsert_env SP_ORTHO_INITIAL_VISIBLE '${NAS_ORTHO_INITIAL_VISIBLE}'
  upsert_env SP_ORTHO_SERVER_RENDER_NUDGE '${NAS_ORTHO_SERVER_RENDER_NUDGE}'

  for expected_profile in \
    'SP_APP_PERF_TIMING=${NAS_PERF_TIMING}' \
    'SP_ORTHO_SUSPEND_HIDDEN=${NAS_ORTHO_SUSPEND_HIDDEN}' \
    'SP_HOMO_DEFER_SEQUENCE=${NAS_HOMO_DEFER_SEQUENCE}' \
    'SP_ORTHO_DEFER_SEQUENCE=${NAS_ORTHO_DEFER_SEQUENCE}' \
    'SP_FOOTER_DEFER_SEQUENCE=${NAS_FOOTER_DEFER_SEQUENCE}' \
    'SP_DEFER_FEATURE_GC=${NAS_DEFER_FEATURE_GC}' \
    'SP_HOMO_RENDER_CHUNK_SIZE=${NAS_HOMO_RENDER_CHUNK_SIZE}' \
    'SP_HOMO_AUTO_RENDER_DELAY_MS=${NAS_HOMO_AUTO_RENDER_DELAY_MS}' \
    'SP_ISOFORM_RENDER_BATCH_SIZE=${NAS_ISOFORM_RENDER_BATCH_SIZE}' \
    'SP_ISOFORM_RENDER_BATCH_DELAY_MS=${NAS_ISOFORM_RENDER_BATCH_DELAY_MS}' \
    'SP_ORTHO_RENDER_CHUNK_SIZE=${NAS_ORTHO_RENDER_CHUNK_SIZE}' \
    'SP_ORTHO_AUTO_RENDER_MORE=${NAS_ORTHO_AUTO_RENDER_MORE}' \
    'SP_ORTHO_AUTO_RENDER_DELAY_MS=${NAS_ORTHO_AUTO_RENDER_DELAY_MS}' \
    'SP_HOMO_INITIAL_VISIBLE=${NAS_HOMO_INITIAL_VISIBLE}' \
    'SP_ORTHO_INITIAL_VISIBLE=${NAS_ORTHO_INITIAL_VISIBLE}' \
    'SP_ORTHO_SERVER_RENDER_NUDGE=${NAS_ORTHO_SERVER_RENDER_NUDGE}'; do
    grep -Fqx "\$expected_profile" .env || {
      echo "ERROR: no se pudo fijar el perfil progresivo: \$expected_profile" >&2
      exit 1
    }
  done

  echo '  Iniciando servicios con rutas absolutas del host NAS...'
  CGV_IMAGE='${CGV_IMAGE}' ${REMOTE_DOCKER} compose --project-directory . -f deploy/docker-compose.shinyproxy.yml up -d
"

# --- Paso 6: Verificar salud ---
echo ""
echo "[6/7] Verificando servicios..."
echo "  Esperando ShinyProxy, nginx y un contenedor CGV real..."
nssh "
  set -e
  cd ${NAS_APP_DIR}
  static_image_id=\$(${REMOTE_DOCKER} image inspect --format '{{.Id}}' '${CGV_IMAGE}' | tr -d '\r\n')
  static_revision=\${static_image_id#sha256:}
  if ! printf '%s\n' \"\$static_revision\" | grep -Eq '^[a-f0-9]{64}$'; then
    echo \"ERROR: Docker no devolvio un Id sha256 valido para ${CGV_IMAGE}: \$static_image_id\" >&2
    exit 1
  fi
  asset_version=\$(grep '^APP_ASSET_VERSION=' .env | tail -1 | cut -d= -f2-)
  static_base_url=\$(grep '^APP_STATIC_BASE_URL=' .env | tail -1 | cut -d= -f2-)
  if [ \"\$asset_version\" != \"\$static_revision\" ] ||
     [ \"\$static_base_url\" != \"/cgv-static/\$static_revision\" ]; then
    echo 'ERROR: .env no identifica exactamente el snapshot de la imagen desplegada.' >&2
    echo \"  imagen=\$static_revision APP_ASSET_VERSION=\$asset_version APP_STATIC_BASE_URL=\$static_base_url\" >&2
    exit 1
  fi
  static_release='${NAS_APP_DIR}/cache/static_assets/releases/'\"\$static_revision\"
  if [ ! -s \"\$static_release/healthz.txt\" ]; then
    echo \"ERROR: falta healthz.txt en el snapshot estatico activo: \$static_release\" >&2
    exit 1
  fi

  MAX=180; ELAPSED=0; READY=0
  while [ \$ELAPSED -lt \$MAX ]; do
    delegate=\$(${REMOTE_DOCKER} ps --filter 'name=sp-container-' --filter status=running --format '{{.Names}}' 2>/dev/null | head -1 || true)
    app_code=000
    if [ -n \"\$delegate\" ] &&
       ${REMOTE_DOCKER} exec \"\$delegate\" sh -c 'wget -qO- http://127.0.0.1:3838/healthz.txt 2>/dev/null | grep -qx ok' >/dev/null 2>&1; then
      app_code=200
    fi
    shiny_code=\$(curl -sS -I -o /dev/null -w '%{http_code}' --max-time 5 http://127.0.0.1:8080 2>/dev/null || true)
    nginx_code=\$(curl -sS -I -o /dev/null -w '%{http_code}' --max-time 5 http://127.0.0.1:${CGV_NGINX_PORT} 2>/dev/null || true)
    report_worker=\$(${REMOTE_DOCKER} ps --filter 'name=cgv-background-report-worker' --filter status=running --format '{{.Names}}' 2>/dev/null | head -1 || true)
    if echo \"\$shiny_code\" | grep -Eq '^(200|302)$' &&
       echo \"\$nginx_code\" | grep -Eq '^(200|302)$' &&
       [ \"\$app_code\" = '200' ] && [ -n \"\$report_worker\" ]; then
      echo \"  Cadena local lista después de \${ELAPSED}s (ShinyProxy=\$shiny_code, nginx=\$nginx_code, app=\$delegate/\$app_code, worker=\$report_worker)\"
      READY=1
      break
    fi
    echo \"  Esperando... (\${ELAPSED}s/\${MAX}s; ShinyProxy=\${shiny_code:-000}, nginx=\${nginx_code:-000}, app=\${delegate:-pendiente}/\$app_code)\"
    sleep 5
    ELAPSED=\$((ELAPSED + 5))
  done

  if [ \"\$READY\" != '1' ]; then
    echo 'ERROR: la cadena ShinyProxy/nginx/CGV no quedó lista.' >&2
    echo '--- cgv-shinyproxy (últimas 80 líneas) ---' >&2
    ${REMOTE_DOCKER} logs --tail 80 cgv-shinyproxy >&2 2>&1 || true
    echo '--- cgv-nginx (últimas 80 líneas) ---' >&2
    ${REMOTE_DOCKER} logs --tail 80 cgv-nginx >&2 2>&1 || true
    exit 1
  fi

  delegate_image_id=\$(${REMOTE_DOCKER} inspect --format '{{.Image}}' \"\$delegate\" | tr -d '\r\n')
  delegate_revision=\${delegate_image_id#sha256:}
  if [ \"\$delegate_revision\" != \"\$static_revision\" ]; then
    echo 'ERROR: el contenedor CGV no usa la imagen exacta del snapshot estatico.' >&2
    echo \"  snapshot=\$static_revision app=\$delegate_revision\" >&2
    exit 1
  fi

  delegate_env=\$(${REMOTE_DOCKER} inspect \"\$delegate\" --format '{{range .Config.Env}}{{println .}}{{end}}')
  for expected_env in \
    'APP_PERF_TIMING=${NAS_PERF_TIMING}' \
    'APP_ORTHO_SUSPEND_HIDDEN=${NAS_ORTHO_SUSPEND_HIDDEN}' \
    'APP_HOMO_DEFER_SEQUENCE=${NAS_HOMO_DEFER_SEQUENCE}' \
    'APP_ORTHO_DEFER_SEQUENCE=${NAS_ORTHO_DEFER_SEQUENCE}' \
    'APP_FOOTER_DEFER_SEQUENCE=${NAS_FOOTER_DEFER_SEQUENCE}' \
    'APP_DEFER_FEATURE_GC=${NAS_DEFER_FEATURE_GC}' \
    'APP_HOMO_RENDER_CHUNK_SIZE=${NAS_HOMO_RENDER_CHUNK_SIZE}' \
    'APP_HOMO_AUTO_RENDER_DELAY_MS=${NAS_HOMO_AUTO_RENDER_DELAY_MS}' \
    'APP_ISOFORM_RENDER_BATCH_SIZE=${NAS_ISOFORM_RENDER_BATCH_SIZE}' \
    'APP_ISOFORM_RENDER_BATCH_DELAY_MS=${NAS_ISOFORM_RENDER_BATCH_DELAY_MS}' \
    'APP_ORTHO_RENDER_CHUNK_SIZE=${NAS_ORTHO_RENDER_CHUNK_SIZE}' \
    'APP_ORTHO_AUTO_RENDER_MORE=${NAS_ORTHO_AUTO_RENDER_MORE}' \
    'APP_ORTHO_AUTO_RENDER_DELAY_MS=${NAS_ORTHO_AUTO_RENDER_DELAY_MS}' \
    'APP_HOMO_INITIAL_VISIBLE=${NAS_HOMO_INITIAL_VISIBLE}' \
    'APP_ORTHO_INITIAL_VISIBLE=${NAS_ORTHO_INITIAL_VISIBLE}' \
    'APP_ORTHO_SERVER_RENDER_NUDGE=${NAS_ORTHO_SERVER_RENDER_NUDGE}'; do
    if ! printf '%s\n' \"\$delegate_env\" | grep -Fqx \"\$expected_env\"; then
      echo \"ERROR: el contenedor CGV no recibió el perfil esperado: \$expected_env\" >&2
      exit 1
    fi
  done
  echo '  Perfil progresivo confirmado: chunk=1, auto=1, initial=1/1, delay=120ms, nudge=0.'

  static_url='http://127.0.0.1:${CGV_NGINX_PORT}/cgv-static/'\"\$static_revision\"
  static_health_code=\$(curl -sS -o /dev/null -w '%{http_code}' --max-time 10 \"\$static_url/healthz.txt\" 2>/dev/null || true)
  if [ \"\$static_health_code\" != '200' ] ||
     ! curl -fsS --max-time 10 \"\$static_url/healthz.txt\" | grep -qx 'ok'; then
    echo \"ERROR: el healthz interno del snapshot respondio HTTP \${static_health_code:-000} o un cuerpo inesperado.\" >&2
    exit 1
  fi

  static_head=\$(curl -sS -I --max-time 10 \"\$static_url/healthz.txt\")
  if ! printf '%s\n' \"\$static_head\" | head -1 | grep -Eq '^HTTP/[0-9.]+ 200([[:space:]]|$)' ||
     ! printf '%s\n' \"\$static_head\" | tr -d '\r' | grep -Eiq '^Cache-Control:[[:space:]]*public,[[:space:]]*max-age=31536000,[[:space:]]*immutable[[:space:]]*$'; then
    echo 'ERROR: HEAD interno no confirmo HTTP 200 y Cache-Control inmutable.' >&2
    exit 1
  fi

  range_asset=\$(find \"\$static_release\" -type f \( -iname '*.mp4' -o -iname '*.pdf' \) -print | LC_ALL=C sort | head -1 || true)
  if [ -n \"\$range_asset\" ]; then
    range_relative=\${range_asset#\"\$static_release\"/}
    if ! printf '%s\n' \"\$range_relative\" | grep -Eq '^[A-Za-z0-9._/-]+$'; then
      echo \"ERROR: no es seguro construir la URL del asset de rango: \$range_relative\" >&2
      exit 1
    fi
    range_head=\$(curl -sS --max-time 15 -D - -o /dev/null -H 'Range: bytes=0-1' \"\$static_url/\$range_relative\")
    if ! printf '%s\n' \"\$range_head\" | head -1 | grep -Eq '^HTTP/[0-9.]+ 206([[:space:]]|$)' ||
       ! printf '%s\n' \"\$range_head\" | tr -d '\r' | grep -Eiq '^Content-Range:[[:space:]]*bytes[[:space:]]+0-1/[0-9]+[[:space:]]*$'; then
      echo \"ERROR: nginx no sirvio byte ranges para \$range_relative.\" >&2
      exit 1
    fi
    echo \"  Static smoke OK: healthz=200, cache inmutable, range=206 (\$range_relative)\"
  else
    echo '  Static smoke OK: healthz=200 y cache inmutable; no hay MP4/PDF para probar Range.'
  fi

  ${REMOTE_DOCKER} ps --filter name=cgv-nginx --filter name=cgv-shinyproxy --filter name=cgv-background-report-worker --format 'table {{.Names}}\t{{.Status}}\t{{.Ports}}'
"

# --- Paso 7: Supervisar Cloudflare tunnel ---
echo ""
echo "[7/7] Supervisando tunel Cloudflare (${TUNNEL_NAME})..."
nssh "
  set -e
  tunnel_config='/home/${NAS_USER}/.cloudflared/config.yml'
  if [ ! -f \"\$tunnel_config\" ]; then
    tunnel_config='/home/${NAS_USER}/.cloudflared/config.yaml'
  fi
  if [ ! -f \"\$tunnel_config\" ]; then
    echo 'ERROR: no se encontró config.yml/config.yaml de cloudflared.' >&2
    exit 1
  fi
  if grep -Eq 'service:[[:space:]]*http://(127[.]0[.]0[.]1|localhost):(3838|80)[[:space:]]*$' \"\$tunnel_config\" ||
     ! grep -Eq 'service:[[:space:]]*http://(127[.]0[.]0[.]1|localhost):${CGV_NGINX_PORT}[[:space:]]*$' \"\$tunnel_config\"; then
    echo \"ERROR: el túnel ${TUNNEL_NAME} no apunta a http://127.0.0.1:${CGV_NGINX_PORT}.\" >&2
    echo \"Corrige \$tunnel_config antes de relanzarlo.\" >&2
    exit 1
  fi

  NAS_USER='${NAS_USER}' NAS_PATH='${NAS_PATH}' TUNNEL_CONFIG=\"\$tunnel_config\" \
    bash '${NAS_APP_DIR}/deploy/setup-nas-tunnel.sh' --install

  MAX=60; ELAPSED=0; PUBLIC_OK=0
  while [ \$ELAPSED -lt \$MAX ]; do
    if ! systemctl --user is-active --quiet cgv-cloudflared.service; then
      echo 'ERROR: el túnel se detuvo durante el arranque.' >&2
      journalctl --user -u cgv-cloudflared.service -n 30 --no-pager >&2
      exit 1
    fi
    public_code=\$(curl -sS -I -o /dev/null -w '%{http_code}' --max-time 10 https://${NAS_PUBLIC_HOSTNAME}/ 2>/dev/null || true)
    if echo \"\$public_code\" | grep -Eq '^(200|301|302)$'; then
      echo \"  ${NAS_PUBLIC_HOSTNAME} responde HTTP \$public_code después de \${ELAPSED}s\"
      PUBLIC_OK=1
      break
    fi
    sleep 5
    ELAPSED=\$((ELAPSED + 5))
  done
  if [ \"\$PUBLIC_OK\" != '1' ]; then
    echo \"ERROR: ${NAS_PUBLIC_HOSTNAME} no quedó accesible (último HTTP: \${public_code:-000}).\" >&2
    journalctl --user -u cgv-cloudflared.service -n 50 --no-pager >&2
    exit 1
  fi
"

echo ""
echo "============================================"
echo "  ShinyProxy Deploy completado!"
echo ""
echo "  ShinyProxy:  http://127.0.0.1:8080 (interno del NAS)"
echo "  Proxy web:   http://${NAS_HOST}:${CGV_NGINX_PORT}"
echo "  Publico:     https://${NAS_PUBLIC_HOSTNAME}"
echo ""
echo "  Login:       desactivado (acceso directo, limitado por capacidad)"
echo ""
echo "  Usuarios simultaneos: $(local_env_value SP_MAX_TOTAL_INSTANCES 5) max (2GB RAM c/u por defecto)"
echo "============================================"
