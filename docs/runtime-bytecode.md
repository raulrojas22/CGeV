# Compiled R runtime

The image build runs `Rscript scripts/compile_runtime.R /app/.cgv-compiled`. This compiles application expressions once in the same R image used at runtime. No session, genome, annotation, populated cache or result is serialized. Evaluating the expressions creates closures and caches in the current application environment.

`global.R` loads library sources through `cgv_source_runtime`; `server.R` returns the function provided by `cgv_runtime_server`. The latter must return the evaluated anonymous function itself. The ordinary R JIT remains enabled for other code and dependencies.

The loader checks the R version/platform, direct package versions, source checksums and artifact checksums. Missing, incompatible or damaged artifacts fall back to the source before evaluating any compiled expressions. Evaluation errors propagate normally and never cause source evaluation to run a second time.

Set `APP_COMPILED_RUNTIME=0` to compare the ordinary source route in the same image. `CGV_COMPILED_RUNTIME_DIR` can point to an alternative build output. Artifacts are ignored by Git and Docker's source context: always rebuild them inside the target image after changing source or dependencies.

For latency validation, use real Shiny sessions in a browser. `shiny::testServer` rewrites the server body for instrumentation, which discards precompiled server bytecode and invalidates startup comparisons. Compare complete cards, first and repeated searches, plus exact SVG output with only random IDs normalized. Keep the progressive card and complete information settings unchanged.
