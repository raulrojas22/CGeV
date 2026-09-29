#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR_DEFAULT="$(cd "${SCRIPT_DIR}/.." && pwd)"
APP_DIR="${APP_DIR:-${APP_DIR_DEFAULT}}"
export APP_DIR

load_env_file() {
  local file_path="$1"
  if [[ -f "${file_path}" ]]; then
    set -a
    # shellcheck disable=SC1090
    source "${file_path}"
    set +a
  fi
}

load_env_file "${APP_DIR}/.env"
load_env_file "${APP_DIR}/.env.local"

APP_HOST="${APP_HOST:-0.0.0.0}"
APP_PORT="${APP_PORT:-3838}"
APP_PREWARM_ON_START="${APP_PREWARM_ON_START:-0}"
APP_PREWARM_CLEAN="${APP_PREWARM_CLEAN:-0}"
APP_PREWARM_BLOCK_START="${APP_PREWARM_BLOCK_START:-0}"

cd "${APP_DIR}"
mkdir -p cache/work_sessions

for required_dir in annotations genomes go_annotations data/alias_index; do
  if [[ ! -d "${required_dir}" ]]; then
    echo "[cgv-native] warning: '${required_dir}' directory not found in ${APP_DIR}"
  fi
done

if [[ "${APP_PREWARM_ON_START}" == "1" ]]; then
  echo "[cgv-native] prewarm enabled (APP_PREWARM_ON_START=1)"
  prewarm_args=(scripts/precompute_preloaded_cache.R "--root=${APP_DIR}")
  if [[ "${APP_PREWARM_CLEAN}" == "1" ]]; then
    prewarm_args+=("--clean")
  fi

  if Rscript "${prewarm_args[@]}"; then
    echo "[cgv-native] prewarm completed"
  else
    echo "[cgv-native] warning: prewarm failed"
    if [[ "${APP_PREWARM_BLOCK_START}" == "1" ]]; then
      echo "[cgv-native] APP_PREWARM_BLOCK_START=1, aborting startup"
      exit 1
    fi
  fi
fi

echo "[cgv-native] starting Shiny app at ${APP_HOST}:${APP_PORT}"
exec Rscript -e "shiny::runApp('${APP_DIR}', host='${APP_HOST}', port=as.integer('${APP_PORT}'), launch.browser=FALSE)"
