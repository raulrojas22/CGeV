# Deploy seguro de CGV en Colors

Producción está publicada en:

```text
https://cgev.mobilomics.org
  -> 127.0.0.1:3838
  -> nginx sin privilegios
  -> ShinyProxy 3.2.4
  -> contenedor CGV versionado por release
```

El despliegue normal actualiza la aplicación sin reemplazar ShinyProxy, nginx,
`docker-socket-proxy`, las redes seguras ni los datos biológicos persistentes.
También mantiene el worker aislado de reportes por correo y añade únicamente
sus variables permitidas a la configuración de las sesiones CGV.

## Antes de desplegar

Comprueba la infraestructura sin realizar cambios:

```bash
./deploy/deploy-colors-shinyproxy.sh --check
```

El resultado esperado termina con:

```text
CHECK OK: no se realizaron cambios en Colors.
```

La línea `Sesión` informa además la telemetría efectiva (`perf=0` o `perf=1`)
que recibió el contenedor público. El check compara ese valor con el
`application.yml` activo; no lo deduce del valor por defecto del comando local.
La línea `Progressive` verifica también la información completa en cada tarjeta,
con lotes de una tarjeta e intervalos de 120 ms, incluidas las isoformas.
Las tarjetas aparecen en cuanto están listas y no difieren composición ni GC.

El script aborta si detecta cualquiera de estas condiciones:

- ShinyProxy no corresponde al digest fijado de la versión `3.2.4`;
- ShinyProxy monta directamente el socket Podman;
- `docker-socket-proxy` no está saludable;
- la red de control no está aislada;
- la imagen activa no es una release versionada;
- Git contiene cambios sin commit.

## Flujo habitual para un cambio pequeño

1. Modifica y prueba la aplicación localmente.
2. Revisa exactamente qué archivos cambiaron.
3. Crea un commit.
4. Ejecuta el deploy.

```bash
git status
Rscript -e "testthat::test_dir('tests/testthat', reporter='summary')"

git add ui.R www/css/custom.scss www/css/cgv_compiled.css
git commit -m "fix: describe el cambio"

./deploy/deploy-colors-shinyproxy.sh
```

No uses `--skip-tests` como flujo habitual. Existe únicamente para una
emergencia conocida:

```bash
./deploy/deploy-colors-shinyproxy.sh --skip-tests
```

## Telemetría de rendimiento

El deploy normal publica `APP_PERF_TIMING=0`, por lo que no mantiene la
instrumentación de captura activa para usuarios comunes. Para una medición
manual controlada, habilítala sólo en esa publicación y usa una etiqueta
segura:

```bash
COLORS_PERF_TIMING=1 PERF_RUN_LABEL=despues_colors_01 ./deploy/deploy-colors-shinyproxy.sh
```

Ambos valores permitidos son literales (`0` o `1`); cualquier otro valor hace
fallar el deploy antes de modificar Colors. Al terminar la medición, vuelve al
modo normal:

```bash
COLORS_PERF_TIMING=0 ./deploy/deploy-colors-shinyproxy.sh
```

## Qué hace el script

1. Audita la infraestructura remota en modo lectura.
2. Ejecuta parseo y pruebas `testthat` locales.
3. Respalda la configuración y el estado actual.
4. Sincroniza solamente código de aplicación, excluyendo secretos, datos,
   cachés y archivos de trabajo locales. La configuración de ShinyProxy se
   respalda y recibe las variables permitidas para reportes, el semáforo global
   de LASTZ y el perfil de render progresivo validado.
5. Construye una imagen inmutable como:

   ```text
   localhost/cgv:release-<commit>-<fecha-UTC>
   ```

6. Verifica que `ui.R`, `server.R` y `global.R` dentro de la imagen coincidan
   con los archivos locales.
7. Ejecuta el prewarm y cambia producción a la nueva imagen.
8. Inicia `cgv-background-report-worker` con el mismo release, sin puertos
   públicos, con usuario no privilegiado, sistema de archivos de sólo lectura,
   4 GB de memoria y acceso únicamente a los datasets y caché compartidos.
9. Comprueba socket proxy, HTTP interno, instancia CGV, worker y URL pública.

El perfil de render de Colors se escribe como valores literales para que una
configuración antigua del servidor no pueda reactivar las regresiones:

```text
APP_HOMO_INITIAL_VISIBLE=1
APP_ORTHO_INITIAL_VISIBLE=1
APP_HOMO_RENDER_CHUNK_SIZE=1
APP_HOMO_AUTO_RENDER_DELAY_MS=120
APP_ORTHO_RENDER_CHUNK_SIZE=1
APP_ORTHO_AUTO_RENDER_MORE=1
APP_ORTHO_AUTO_RENDER_DELAY_MS=120
APP_ISOFORM_RENDER_BATCH_SIZE=1
APP_ISOFORM_RENDER_BATCH_DELAY_MS=120
APP_ORTHO_SERVER_RENDER_NUDGE=0
APP_HOMO_DEFER_SEQUENCE=0
APP_ORTHO_DEFER_SEQUENCE=0
APP_FOOTER_DEFER_SEQUENCE=0
APP_DEFER_FEATURE_GC=0
APP_ORTHO_SUSPEND_HIDDEN=1
```

Las sesiones abiertas se cierran durante la conmutación. El tiempo de corte
normal es el necesario para reiniciar ShinyProxy y crear la primera instancia
CGV.

## Rollback automático

Si la nueva release no queda saludable o la URL pública no responde, el script
restaura la imagen anterior y la configuración respaldada.

Cada ejecución guarda su respaldo en:

```text
/home/rarojas/cgv/rollback/<fecha>-pre-app-deploy
```

La imagen nueva no se elimina automáticamente después de un fallo, para poder
inspeccionarla sin afectar la release restaurada.

## Dependencias R

Los cambios normales de R, JavaScript, CSS o contenido reutilizan:

```text
cgv-deps:1.0.0
```

El deploy comprueba que la base contenga Google Chrome headless utilizable y el
paquete R `chromote`; el paquete `chromium` de Ubuntu no se usa porque sólo
instala un lanzador de Snap que no funciona dentro del contenedor.
En la primera publicación del sistema de reportes la reconstruirá
automáticamente si faltan. Para forzar una reconstrucción posterior:

```bash
REBUILD_R_DEPS=1 ./deploy/deploy-colors-shinyproxy.sh
```

## Reportes interactivos por correo

Al escoger **Email me**, la sesión guarda un snapshot inmutable en el caché de
Colors. El worker restaura ese snapshot, genera el mismo reporte interactivo y
envía el enlace bajo `https://cgev.mobilomics.org/share/...` usando la
configuración existente de feedback. El usuario puede continuar trabajando o
cerrar la sesión; otro alineamiento crea un trabajo separado.

El worker se ejecuta en serie y comparte dos cupos globales de LASTZ con las
sesiones públicas (`APP_LASTZ_GLOBAL_WORKERS=2`). Esto permite dos alineamientos
simultáneos sin liberar una cantidad no acotada de procesos. El remitente
corresponde a `FEEDBACK_FROM_EMAIL` y las respuestas se dirigen a
`FEEDBACK_TO_EMAIL`.

Comprobación rápida después del deploy:

```bash
ssh colors 'podman ps --filter name=cgv-background-report-worker'
ssh colors 'podman logs --tail 40 cgv-background-report-worker'
```

## Datos persistentes

El script nunca sincroniza ni elimina estos directorios de Colors:

```text
/home/rarojas/cgv/app/annotations
/home/rarojas/cgv/app/genomes
/home/rarojas/cgv/app/go_annotations
/home/rarojas/cgv/app/data
/home/rarojas/cgv/app/cache
/home/rarojas/cgv/ncbi_downloads
```

## Acciones que no deben usarse para un cambio de página

No ejecutes manualmente:

```bash
podman-compose down
podman network rm -f sp-control
podman rm -f cgv-docker-socket-proxy
```

Tampoco vuelvas a configurar ShinyProxy `3.1.1` ni una imagen mutable
`cgv:1.0.0`. Para cambios normales, el único comando de producción es:

```bash
./deploy/deploy-colors-shinyproxy.sh
```
