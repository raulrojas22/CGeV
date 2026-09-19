#!/usr/bin/env bash
# Supervise only CGeV's locally managed tunnel, independently of other NAS apps.
set -euo pipefail
MODE="${1:---install}"
NAS_USER="${NAS_USER:-truenas_admin}"
NAS_PATH="${NAS_PATH:-/mnt/Datos4raro/cgv}"
TUNNEL_CONFIG="${TUNNEL_CONFIG:-/home/${NAS_USER}/.cloudflared/config.yml}"
TUNNEL_METRICS_PORT="${TUNNEL_METRICS_PORT:-20243}"
[[ "$MODE" == --install || "$MODE" == --boot ]] || exit 2
[[ "$NAS_USER" =~ ^[a-z_][a-z0-9_-]*$ ]] || exit 2
for path in "$NAS_PATH" "$TUNNEL_CONFIG"; do
  [[ "$path" =~ ^/[A-Za-z0-9_./-]+$ && "$path" != *..* ]] || exit 2
done
[[ "$TUNNEL_METRICS_PORT" =~ ^[0-9]+$ ]] &&
  (( TUNNEL_METRICS_PORT > 1024 && TUNNEL_METRICS_PORT < 65536 )) || exit 2
uid="$(id -u "$NAS_USER")"
if [[ "$(id -u)" == 0 ]]; then
  # TrueNAS POSTINIT runs as root. The connector itself always runs as its user.
  loginctl enable-linger "$NAS_USER"
  systemctl start "user@${uid}.service"
  exec runuser -u "$NAS_USER" -- env \
    XDG_RUNTIME_DIR="/run/user/${uid}" \
    DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/${uid}/bus" \
    NAS_USER="$NAS_USER" NAS_PATH="$NAS_PATH" TUNNEL_CONFIG="$TUNNEL_CONFIG" \
    TUNNEL_METRICS_PORT="$TUNNEL_METRICS_PORT" bash "$0" "$MODE"
fi
[[ "$(id -u)" == "$uid" ]] || { echo 'Run as the configured NAS user.' >&2; exit 1; }
export XDG_RUNTIME_DIR="/run/user/${uid}"
export DBUS_SESSION_BUS_ADDRESS="unix:path=${XDG_RUNTIME_DIR}/bus"
[[ -x "${NAS_PATH}/cloudflared" && -r "$TUNNEL_CONFIG" ]] || exit 1
"${NAS_PATH}/cloudflared" tunnel --config "$TUNNEL_CONFIG" ingress validate
loginctl enable-linger "$NAS_USER"

service_dir="${NAS_PATH}/services"
unit_dir="$(getent passwd "$NAS_USER" | cut -d: -f6)/.config/systemd/user"
mkdir -p "$service_dir" "$unit_dir"
persistent_script="${service_dir}/setup-nas-tunnel.sh"
if [[ "$(readlink -f "$0")" != "$persistent_script" ]]; then
  install -m 0755 "$0" "$persistent_script"
fi
unit="${service_dir}/cgv-cloudflared.service"
cat > "${unit}.candidate" <<EOF
[Unit]
Description=CGeV NAS Cloudflare tunnel
StartLimitIntervalSec=0

[Service]
Type=simple
ExecStart=${NAS_PATH}/cloudflared --no-autoupdate tunnel --config ${TUNNEL_CONFIG} --protocol http2 --metrics 127.0.0.1:${TUNNEL_METRICS_PORT} run cgv
Restart=always
RestartSec=5
TimeoutStopSec=30
NoNewPrivileges=true
UMask=0077

[Install]
WantedBy=default.target
EOF
# systemd requires the .service suffix when validating a standalone file.
verify_dir="$(mktemp -d)"
trap 'rm -rf "$verify_dir"' EXIT
cp "${unit}.candidate" "${verify_dir}/cgv-cloudflared.service"
systemd-analyze --user verify "${verify_dir}/cgv-cloudflared.service"
rm -r "$verify_dir"
trap - EXIT
changed=0
if ! cmp -s "${unit}.candidate" "${unit_dir}/cgv-cloudflared.service"; then changed=1; fi
mv "${unit}.candidate" "$unit"
install -m 0644 "$unit" "${unit_dir}/cgv-cloudflared.service"
systemctl --user daemon-reload
systemctl --user enable cgv-cloudflared.service
if [[ "$changed" == 1 ]] && systemctl --user is-active --quiet cgv-cloudflared.service; then
  systemctl --user restart cgv-cloudflared.service
else
  systemctl --user start cgv-cloudflared.service
fi
ready=0
for attempt in $(seq 1 30); do
  if systemctl --user is-active --quiet cgv-cloudflared.service &&
     curl -fsS --max-time 2 "http://127.0.0.1:${TUNNEL_METRICS_PORT}/ready" >/dev/null; then
    ready=1; break
  fi
  sleep 2
done
[[ "$ready" == 1 ]] || { echo 'Supervised connector not ready; legacy connector was not stopped.' >&2; exit 1; }

# Stop only the former manual connector after the supervised connector is ready.
python3 - "$NAS_PATH" "$TUNNEL_CONFIG" <<'PY'
from pathlib import Path
import os, signal, subprocess, sys
root, config = Path(sys.argv[1]), sys.argv[2]
current = int(subprocess.check_output(['systemctl', '--user', 'show', 'cgv-cloudflared.service', '-p', 'MainPID', '--value']))
pidfile = root / 'tunnel.pid'
if pidfile.exists():
    try:
        previous = int(pidfile.read_text().strip())
        proc = Path('/proc') / str(previous)
        args = (proc / 'cmdline').read_bytes().split(b'\0')
        if (previous != current and proc.stat().st_uid == os.getuid()
                and (proc / 'exe').resolve() == (root / 'cloudflared').resolve()
                and b'--config' in args and os.fsencode(config) in args and b'cgv' in args):
            os.kill(previous, signal.SIGTERM)
    except (ValueError, ProcessLookupError, FileNotFoundError):
        pass
pidfile.write_text(str(current) + '\n')
print('CGeV supervised connector ready; PID', current)
PY

if [[ "$MODE" == --install ]]; then
  boot_script="${service_dir}/cgv-tunnel-boot.sh"
  cat > "$boot_script" <<EOF
#!/usr/bin/env bash
exec env NAS_USER=${NAS_USER} NAS_PATH=${NAS_PATH} TUNNEL_CONFIG=${TUNNEL_CONFIG} TUNNEL_METRICS_PORT=${TUNNEL_METRICS_PORT} bash ${persistent_script} --boot
EOF
  chmod 0755 "$boot_script"
  python3 - "$boot_script" <<'PY'
import json, subprocess, sys
comment = 'CGeV cloudflared supervision'
def call(method, *args):
    return json.loads(subprocess.check_output(['midclt', 'call', method, *[json.dumps(a) for a in args]], text=True))
tasks = call('initshutdownscript.query', [['comment', '=', comment]])
if len(tasks) > 1:
    raise RuntimeError('Multiple CGeV startup tasks; refusing to choose one')
data = {'type': 'SCRIPT', 'script': sys.argv[1], 'when': 'POSTINIT', 'enabled': True,
        'timeout': 120, 'comment': comment}
if tasks:
    task = call('initshutdownscript.update', tasks[0]['id'], data)
else:
    task = call('initshutdownscript.create', data)
print('TrueNAS POSTINIT task:', task['id'])
PY
fi
systemctl --user show cgv-cloudflared.service -p ActiveState -p MainPID -p Restart -p NRestarts
loginctl show-user "$NAS_USER" -p Linger
