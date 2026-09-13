# Explicit Shiny runtime loading

`global.R` loads the application libraries in their required order through
`cgv_source_runtime()`, into `lib_env`, attached as `app_libraries`.
`ui.R` separately sources `R/ui_desktop_downloads.R` in its own environment.

`R/_disable_autoload.R` is Shiny's per-application opt-out for automatic helper
loading. It must ship with the application. Without it, Shiny sources `R/*.R`
again after `global.R` into its shared environment. The actual server then
resolves these second, uncompiled functions instead of the explicit runtime
bindings. This also creates a second set of library state and caches.

This opt-out applies both to bytecode loading and its ordinary source fallback.
It changes neither card scheduling nor the content required before a card is
complete. New R helpers must be added to an explicit loader; the inventory test
in `test-compiled-runtime.R` detects omissions.

For a real application check, run each command in a fresh R process from the
application root, using the same packages and artifacts as the image:

```sh
APP_COMPILED_RUNTIME=1 Rscript scripts/test_shiny_runtime_loading.R --require-compiled
APP_COMPILED_RUNTIME=0 Rscript scripts/test_shiny_runtime_loading.R
```

The script uses `shinyAppDir()`, its startup hook and server factory, checks every
library binding as resolved from the actual server closure, and checks bytecode
when valid compiled artifacts are available. `testServer()` is unsuitable for
this check because it rewrites the server body. Checking artifacts alone also
does not establish which functions the actual server uses.
