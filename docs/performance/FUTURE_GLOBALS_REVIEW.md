# CGeV: globals explícitos para STRING, sequence prefetch y Literature

## Base y entrega local

- Base exacta: `b4b24a2717eba09e6dff34c5e639ac6dc81b5084`.
- Rama base disponible y validada: `codex/cgev-pr1-concurrency`.
- `git fetch` ejecutado antes de modificar; terminó con código 0.
- Rama nueva: `codex/cgev-future-globals`.
- Worktree nuevo: `/private/tmp/cgev-future-globals`.
- Checkout del usuario: `/Users/rarojas/Documents/A_FULLAPP`, sin modificaciones por esta tarea.
- Entrega local para revisión. No se ha publicado un PR remoto, realizado merge, deploy ni benchmark Colors.

## Commits de implementación

| SHA | Mensaje |
| --- | --- |
| `5073916f302ac0828fff4ae736ff5625c7f3cf95` | `perf(string): isolate Future globals from application environments` |
| `eb16f6a5cc81b6aeed55edcd30dae25f7f3d8e5f` | `perf(sequence): export prefetch data and code without module closures` |
| `72b561b9c12f21da075418b80a92736d724d8130` | `perf(literature): isolate search function and declare Future globals` |

Este informe se registra en un commit documental posterior.

## Archivos modificados respecto de la base

- `R/string_worker.R`
- `R/modules.R`
- `server.R`
- `scripts/test_string_future_globals.R`
- `scripts/test_sequence_prefetch_future_globals.R`
- `scripts/test_literature_future_globals.R`
- `docs/performance/FUTURE_GLOBALS_REVIEW.md`

## STRING

Antes: `future_promise({ string_resolve_and_fetch(query_payload, base_dir = ".") }, seed = TRUE)`; descubrimiento automático sobre funciones cuyo environment es `lib_env`.

Después: `string_future_worker(query_payload, cache_snapshot, base_dir = ".")`, con tres globals explícitos:

1. `string_future_worker`;
2. `query_payload`, sin modificar su estructura;
3. `cache_snapshot`: las tablas de resolución y Network de STRING.

`environment(string_future_worker)` es exactamente `baseenv()`. Los dos environments de caché tienen padre `emptyenv()`. El constructor `string_future_globals`, ligado a la librería, se ejecuta en el main process y **no** se exporta.

El worker crea un environment nuevo con padre `baseenv()` y carga los archivos existentes `R/utils.R`, `R/string_cache.R` y `R/string_worker.R`. Allí crea sus closures, instala las dos tablas recibidas y llama a la implementación original. `%>%` proviene del namespace de magrittr; los packages declarados son `httr2` y `magrittr`. Las dependencias opcionales existentes, como digest, siguen resolviéndose por namespace.

No se transportan las demás cachés de la aplicación. No se cambian implementación HTTP, retries, 404 de PR1, claves, TTLs, roles, Network, callbacks ni `seed = TRUE`. `resolve_missing`, `pendingStringPromises` y la ausencia de `screen_variants` en la key quedan intactos.

## Sequence prefetch homo y ortho

Antes: se exportaban las closures `run_sequence_prefetch()` y `run_prefetch_payload()`, ligadas a sus respectivos módulos Shiny.

Después: ambas rutas llaman a `sequence_prefetch_future_worker(prefetch_args, prefetch_code, prefetch_state)` con cuatro globals explícitos:

1. `sequence_prefetch_future_worker`;
2. `prefetch_args`: ruta de genoma, cromosoma, span GC, coordenadas de transcript, ID, exones, strand y flags; ortho añade ruta de anotación, gen objetivo y flag de vecinos;
3. `prefetch_code`: cuerpo original como objeto de lenguaje, sin closure y sin referencias de código fuente;
4. `prefetch_state`: límites configurados, contador de acceso y tablas de memoización que puede usar la ruta.

`environment(sequence_prefetch_future_worker)` es exactamente `baseenv()`. El constructor de globals inspecciona una lista fija de locales en el main process y no viaja al worker. No hay descubrimiento recursivo de globals en este constructor.

El worker carga `R/utils.R` en un environment nuevo con padre `baseenv()`, instala el estado explícito y evalúa el cuerpo original en un hijo que contiene únicamente los argumentos. Los helpers `extract_sequence_from_fasta`, `fetch_gene_data_sync` y `get_neighbor_context_for_target` se crean dentro del worker, ligados a ese environment nuevo; no se exportan closures de `lib_env`. Sus dependencias usan namespaces explícitos; `packages = character()` no implica eliminarlas.

Estado de caché seleccionado:

- Extracción habilitada: `.seq_extract_cache`.
- FASTA: fallback, headers, seqnames y resolución de seqnames.
- 2bit: información de secuencias e índice nativo; no las tablas exclusivas de FASTA.
- Secuencia de transcript habilitada: spliced sequence y composición.
- Solo ortho con vecinos habilitados: contexto de vecinos, índices light/por cromosoma, tabla de genes y metadatos de validación/mantenimiento de disco usados por esos helpers.

No se transportan handles FaFile/TwoBitFile: se abren localmente. Tampoco se exportan cachés ajenas como `.gff_cache` o `.orthologous_local_lookup_cache`. Las tablas seleccionadas conservan sus entradas y metadatos; no se vacían las cachés relevantes ni se introduce una caché compartida.

El cuerpo científico permanece idéntico. No cambian secuencias, coordenadas, strand, IDs, GC/composición, vecinos, callbacks ni manejo de errores. La rama inline conserva su closure original y el flag/default `APP_INLINE_FAST_SEQUENCE_PREFETCH`. Se conserva `seed = FALSE`.

## Literature

Antes: `search_papers_epmc(...)` se descubría automáticamente como closure del server.

Después: siete globals explícitos: `search_papers_epmc`, `gene_names`, `organism_str`, `org_aliases`, `page`, `page_size`, `sort_by`; package declarado `httr2`.

`environment(search_papers_epmc)` contiene únicamente `%||%` y tiene padre `baseenv()`. El helper `%||%` es una copia de la función existente con `environment() = baseenv()`. La función exportada no enlaza al server, a session ni a reactives.

El cuerpo de búsqueda no cambia: misma query, aliases, paginación, orden, número de papers, parsing y errores. Los callbacks/UI y la semántica previa de seed permanecen intactos. La prueba compara también el comportamiento actual ante HTTP errors; no intenta corregir problemas preexistentes de ese manejo.

## Medición pequeña

Tamaños en bytes de `serialize(globals, NULL)`, usando exclusivamente los fixtures offline de los tests. Cuentan bindings explícitos de primer nivel, no todas las entradas dentro de las tablas. No son mediciones del protocolo completo de Future, del tiempo de lanzamiento ni de la aplicación real. El JIT de R y el contenido de las tablas pueden afectar el tamaño.

| Ruta | Auto-globals antes | Globals después | Tamaño antes | Tamaño después, fixture |
| --- | --- | ---: | --- | ---: |
| STRING | NO MEDIDO | 3 | NO MEDIDO | 4.487 |
| Sequence homo | NO MEDIDO | 4 | NO MEDIDO | 8.207–13.424 |
| Sequence ortho | NO MEDIDO | 4 | NO MEDIDO | 14.689–18.677 |
| Literature | NO MEDIDO | 7 | NO MEDIDO | 47.764 |

Los tests añaden un objeto ajeno de 2 MiB al environment original y verifican que no se arrastra al worker. Para Literature se comprueba además que el único binding del environment es `%||%`; para los otros puntos de entrada se comprueba `baseenv()`.

La medición antes/después en aplicación real es **NO MEDIDO**. No se atribuyen los 60–70 segundos observados exclusivamente a serialización. La carga de los scripts ahora ocurre dentro del worker y sigue teniendo un costo. Las tablas relevantes aún pueden ser grandes; esta intervención elimina la captura incidental de los environments completos, sin prometer un tamaño constante.

## Validación dirigida

| Comando | Resultado |
| --- | --- |
| `Rscript scripts/test_string_http_errors.R` | PASS |
| `Rscript scripts/test_string_network_roles.R` | PASS; conserva el contraejemplo conocido de `resolve_missing=FALSE` |
| `Rscript scripts/test_string_future_globals.R` | PASS |
| `Rscript scripts/test_sequence_prefetch_future_globals.R` | PASS |
| `Rscript scripts/test_literature_future_globals.R` | PASS |
| `Rscript scripts/test_inline_fast_sequence_prefetch.R` | PASS |
| `Rscript scripts/test_selected_sequence_download.R` | PASS |
| `Rscript -e 'for (f in c("server.R", "R/modules.R", "R/string_worker.R", "scripts/test_string_future_globals.R", "scripts/test_sequence_prefetch_future_globals.R", "scripts/test_literature_future_globals.R")) {parse(f); cat(f, "parse-ok\n")}'` | PASS |
| `git diff --check b4b24a2717eba09e6dff34c5e639ac6dc81b5084` | PASS |

Los tests nuevos usan un único proceso hijo real con `multisession`, no emulación secuencial. Se requirió permiso de sandbox para el socket local. El retorno a `sequential` es únicamente limpieza del plan al terminar el test; no se cambia el plan de la aplicación.

STRING prueba resultado válido, payload, roles, caché caliente, not_found, HTTP 400/401/500/503 y Network 404, además de resolución vía `promises::future_promise`. Sequence compara exactamente los resultados originales (excepto tiempos) con FASTA/2bit, ambas hebras, exones, vecinos, memoización fría/caliente y flags deshabilitados. Literature compara cuerpo, query, paginación, parsing, entradas vacías y errores mediante HTTP simulado. No se consulta STRING ni Europe PMC reales.

## Límites y pendientes

- Los archivos R del mismo release deben seguir disponibles en el directorio de trabajo del worker, igual que los demás recursos relativos existentes de la aplicación.
- Las cachés no usadas se inicializan vacías dentro del worker al cargar utils. Las políticas, límites y algoritmos existentes no cambian, pero no se promete idéntica residencia/orden de evicción respecto de un worker que antes recibía todas las cachés ajenas del main process.
- No se ejecutaron benchmark Colors, carga, N=2/4/8, E2E largo ni deployment. La latencia real y el balance final inline/async requieren la medición posterior.
- Permanecen pendientes los Futures con auto-globals de GO, LASTZ, autocomplete, prewarm, ortho progressive/rescue y otros lookups. Ninguna de esas rutas fue modificada. En particular, se conserva el wrapper de LASTZ y su cola.
- No se crea package, servicio, Redis, scheduler, cola, single-flight ni warm-service. No se cambia el número de workers ni ningún default productivo.
