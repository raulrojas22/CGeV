# CGV ShinyProxy Deployment

CGV now has a multi-user deployment path:

```text
Cloudflare Tunnel -> nginx:18080 -> ShinyProxy:8080 -> one CGV container per browser session
```

ShinyProxy is configured with `authentication: none`, so capacity limits are the
main protection:

- `SP_MAX_TOTAL_INSTANCES=5`
- `SP_CONTAINER_MEMORY=2g`
- `SP_MAX_LIFETIME_MINUTES=120`
- `SP_HEARTBEAT_RATE_MS=30000`

The compose file binds ShinyProxy direct access to `127.0.0.1:8080`; public
traffic should go through nginx or Cloudflare on `CGV_NGINX_PORT` (`18080` by
default, to avoid conflicts with NAS/system port 80).

## Deploy

```bash
./deploy/deploy-nas-shinyproxy.sh
```

El deploy normal deja la telemetría desactivada y presenta tarjetas completas
progresivamente: una por lote, intervalo de 120 ms y composición/GC en la
primera pasada. Comparte el perfil de Colors, incluidas las isoformas, y no
activa un segundo `render nudge`. Para una captura diagnóstica puntual:

```bash
NAS_PERF_TIMING=1 PERF_RUN_LABEL=medicion_nas_01 ./deploy/deploy-nas-shinyproxy.sh
```

After the first deploy, Cloudflare Tunnel should point to:

```text
http://localhost:18080
```

El sitio del NAS es `https://cgvapp.com` (`NAS_PUBLIC_HOSTNAME` permite cambiarlo).
El despliegue valida ese origen y lo fija como base de los reportes compartidos.
Conserva el `.env` remoto, los índices de `data/` y los videos de
`www/screencasts/`. Antes de construir exige los 47 videos del manifiesto
`deploy/guide-videos.sha256`, con sus hashes correctos y sin archivos adicionales;
también comprueba los hashes dentro de la imagen. Si falta un video, hay que
reprovisionarlo desde el conjunto aprobado antes de desplegar. No se incluye
la carpeta de videos en Git. La construcción y el prewarm preceden al corte
de los contenedores anteriores; el túnel permanece activo.

### Supervisión del túnel en TrueNAS

`deploy/setup-nas-tunnel.sh --install`, ejecutado en el NAS como
`truenas_admin`, instala `cgv-cloudflared.service` en systemd de usuario con
`Restart=always` y habilita linger. Solo reemplaza el conector manual de CGeV
después de comprobar `/ready` en `127.0.0.1:20243`. No modifica otros túneles.
Guarda el instalador en `/mnt/Datos4raro/cgv/services/` y registra mediante
`midclt` una tarea POSTINIT llamada `CGeV cloudflared supervision`; esta restaura
la unidad de usuario al arrancar TrueNAS. El conector corre sin privilegios.

```bash
systemctl --user status cgv-cloudflared.service
journalctl --user -u cgv-cloudflared.service -n 50 --no-pager
curl -fsS http://127.0.0.1:20243/ready
```

El despliegue puede invocar el instalador repetidamente sin crear otra tarea
POSTINIT ni otro conector. La recuperación tras terminar el proceso se verificó
en el NAS; un reinicio completo del NAS sigue siendo una comprobación operativa
pendiente. ShinyProxy está fijado al digest de la imagen 3.2.4 comprobada en el NAS.

## Smoke Test

Open several independent/private browser windows:

```text
http://192.168.1.200:18080/
```

Then on the NAS:

```bash
docker ps --format 'table {{.Names}}\t{{.Image}}\t{{.Status}}\t{{.Ports}}'
docker stats --no-stream
docker logs cgv-shinyproxy --tail 100
```

Expected result: each active browser session has an isolated CGV app container;
closing tabs should release containers after the heartbeat timeout.

## Static shared reports

The ShinyProxy compose stack mounts `${SP_CACHE_DIR}` read-only into nginx.
Secret reports under `/share/<64-hex-token>/index.html` are therefore served
without allocating a ShinyProxy application container. The report route:

- disables access logging so bearer tokens are not retained;
- adds `noindex`, no-referrer, no-store, nosniff, and CSP headers;
- exposes only the report HTML/JSON and the optional reproducibility ZIP;
- leaves revocation metadata outside the public report directory.

The lightweight `report-cleaner` service runs every 15 minutes against the
shared cache. The NAS deploy sets `CGV_PUBLIC_BASE_URL` to
`https://${NAS_PUBLIC_HOSTNAME}` for links published by the report worker.

## Background report email worker

`deploy/docker-compose.shinyproxy.yml` also starts one
`cgv-background-report-worker`. When a user selects **Email me** in the Share
dialog, the app writes an immutable session snapshot under
`${SP_CACHE_DIR}/background_reports`. The worker claims jobs serially, restores
the snapshot in an internal CGV process, uses Chromium headless to run the
existing complete report capture, publishes through `/share/<token>`, and sends
the secret URL with the existing Resend branding.

The report sender inherits `FEEDBACK_RESEND_API_KEY`, `FEEDBACK_FROM_EMAIL`,
and `FEEDBACK_TO_EMAIL` by default. Thus it uses the verified CGV sender and
directs replies to the CGV Gmail inbox. Optional `REPORT_*` variables can
override that identity. Enable and tune the worker with:

```dotenv
APP_BACKGROUND_REPORTS_ENABLED=1
APP_BACKGROUND_REPORT_TIMEOUT_MINUTES=30
APP_BACKGROUND_REPORT_FUTURE_WORKERS=2
APP_BACKGROUND_REPORT_LASTZ_WORKERS=1
APP_LASTZ_GLOBAL_WORKERS=1
```

Chromium and the R package `chromote` live in `cgv-deps`; the deployment script
automatically rebuilds that dependency image when either is missing.
