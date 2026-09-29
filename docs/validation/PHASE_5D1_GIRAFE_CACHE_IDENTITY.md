# Phase 5D.1 — Girafe shared-cache identity

## Candidate and scope

Baseline: `d0a31682096f198278a11c0684251d2b8d205e97`.
Branch: `codex/phase-5d1-girafe-cache`.
Worktree: `/private/tmp/cgev-phase-5d1`.

The managed worktree tool returned “Git is unavailable”. The requested clean,
exact-baseline worktree was therefore created with `git worktree add`.
The dirty primary checkout was not edited. No production, Colors, deployment,
merge, configuration, service, or production-cache changes were made.

Application changes are confined to the Girafe key helper and its two render
call sites in `R/modules.R`. Scientific rendering and algorithms are unchanged.
The existing LRU, 48-entry default, and hit-time UID refresh are unchanged.

## Root cause and baseline oracle

Both search paths build a representative display label from the query and matched
annotation name, but use the resolved `matched_gene_id` for the scientific
signature. `create_gene_plot()` consumes the display name for the center label
and URL-encoded interactive payloads. It also embeds `plotIndex` as `plot_id` in
promoter/neighbor payloads. None of these presentation identities was in the old
key when a scientific signature was available.

The executable oracle renders real Girafe widgets for `P53 -> TP53` and `TP53`,
using the same scientific signature. With the exact baseline key functions,
both keys match and looking up B returns A's widget, including `plot_id=1` when
B needs `plot_id=2`. The initial no-neighbor fixture did not expose interactive
plot IDs; adding a real neighbor payload made that assertion meaningful.

## Exact old identity

The old key concatenates these values with `|`:

1. Plot context.
2. Scientific signature, or `plot_id:<fallback_id>`.
3. Formatted maximum gene length.
4. Visual mode.
5. Theme mode.
6. Colorblind flag.
7. Sequence length.
8. Neighbor-context presence flag.
9. Compact-feature key (interactivity plus GC span length or `eager`).
10. Normalized orientation.

The scientific signature contains annotation path, resolved gene, transcript,
chromosome, start, and end. It excludes file version and display identity.

## Exact new identity

SHA-256 of a structured serialized list, plus the existing readable orientation
suffix. Structured serialization avoids ambiguity from delimiters in values.

The list contains schema `girafe-final-v2`, plot context, stable scientific
signature, numeric maximum gene length, visual mode, theme mode, colorblind flag,
sequence length, neighbor presence, compact-feature key, normalized orientation,
file identities, and the exact deterministic renderer argument list.

The renderer argument list contains:

- `df`, `df_gene`, `df_transcript`;
- `current_transcript_length`, `length_difference`, `composicion_secuencia`;
- `gene_length_label`, `transcript_length_label`, `neighbor_context`;
- `visual_mode`, `width_svg`, `height_svg`, `organism_label`;
- `annotation_file_path`, `use_report_map`, `report_path`;
- `plot_id`, `plot_context`, `genome_fasta_path`;
- `is_dark_theme`, `is_colorblind_mode`, `gene_display_name`;
- `precomputed_genomic_span`, `model_cache_key`, `orientation_mode`.

The same list is passed to `create_gene_plot()` with `do.call`; only performance
timestamp `caller_started_at` is added after cache lookup. This keeps key inputs
and renderer inputs together. Raw display text is preserved conservatively:
semantically equivalent spellings may miss, but different labels cannot share.
No session ID is added. Plot ID is an additional SVG payload determinant, never
a replacement for the scientific signature. Equal inputs across sessions share.

Annotation, genome, and report paths each contribute canonical path, numeric
size, and numeric POSIX mtime. Serialization preserves the double's subsecond
precision without date formatting or integer truncation. Empty optional paths
are represented explicitly; specified files that cannot be statted bypass the
cache. This does not change the separate sequence or annotation caches.

Missing/blank/NA stable signatures and explicit `plot_id:` fallback signatures
produce an empty key. Both render modules disable cache lookup/storage for that
render, including their local widget memo. Repeated fallback renders leave the
global cache unchanged.

## Targeted results

All passed, in both source and compiled modes:

- Alias/canonical widgets have separate entries and correct labels.
- Encoded alias payloads cannot appear in B's widget.
- Plot IDs cannot cross over; equal plot IDs with different labels also isolate.
- Identical deterministic requests hit the shared entry and avoid the renderer.
- Hits refresh widget UIDs.
- Same-path size changes invalidate; isolated 0.125-second mtime changes invalidate.
- Repeated fallback module renders invoke the renderer and add zero shared entries.
- Both homologous and orthologous production modules work in independent
  `MockShinySession` domains within one PID. Closing A leaves B open and functional.
- LRU eviction and disabled-cache behavior retain their existing contracts.

This is a module/session prototype with synthetic resolved TP53 input and actual
Girafe SVGs. It does not run browser-driven alias lookup or production datasets.

## Timing

Small, local measurements, with warm libraries:

| Mode | Key (1,000 calls) | One render | Key with 100 kb span (100 calls) |
| --- | ---: | ---: | ---: |
| Source | 0.199 ms/call | 280 ms | 0.510 ms/call |
| Compiled | 0.191 ms/call | 295 ms | 0.540 ms/call |

The production-module oracle instruments the renderer and proves that an
identical request in a new session causes zero additional renders.

## Test inventory and limits

- Complete `tests/testthat`: **720 passed assertions; 0 failures, errors,
  warnings, or skips**.
- Targeted widget/session oracle: passed in source and compiled modes.
- Plot model helpers, genomic ruler, compact GC defer, plot scale recalculation,
  plot paint timing, zoom, lazy rendering, lookup rendering, renderer prewarm,
  shared analysis, and block-2 static scripts: passed.
- Actual Shiny source and compiled loading: passed, 727 explicit bindings each.
- Runtime compilation: 22 source files compiled successfully (ignored local output).
- A broad sweep attempted all 89 R/JS/Python `scripts/test*` files separately:
  initially 69 returned success and 20 failed. All 20 also failed in an untouched
  archive of the exact baseline. This sweep includes harnesses that are not
  standalone tests, so these totals must not be interpreted as 20 regressions.
- Five socket-blocked R scripts passed when localhost sockets were permitted:
  alias lookup, literature futures, sequence prefetch futures, STRING futures,
  STRING async screen. The initial alias worker-count warning was absent on retry.
- Geometry fixture generation passed with its required output argument (8 pairs).
  Thus 75 of the original 89 script entries have successful executions.
- One successful exit is an explicit skip: transcript composition needs a genome
  fixture. No warning diagnostics occurred in the targeted oracle or testthat run.
- Desktop tests: initial 21 passed / 7 failed, reproduced on baseline. Allowing
  local sockets yielded 22 passed / 6 failed; failures require unavailable `yazl`,
  `yauzl`, or `js-yaml`. Zero desktop skips. No dependencies were installed.
- Desktop package configuration and browser-side plot-paint gate scripts passed.

The 14 script entries without successful executions are:

- Missing dataset/index or related search fixtures: autocomplete sidecar,
  cross-species family suggestions, download lazy registration, gene plot model
  reuse, orthologous local cache oracle, partial gene suggestions, progressive
  gene cards.
- Cross-species orthology gate: baseline TRP1 ambiguity assertion fails.
- Gene footer reactivity: requires arguments and a preloaded human dataset.
- Gene geometry browser: requires an HTML/browser harness; Node has no `document`.
- Lightweight healthcheck and NAS ShinyProxy static assets: baseline static
  assertions fail.
- TwoBit single-span splice: `faToTwoBit` is unavailable.
- User manual integration: authoring PDF is unavailable.

No unrelated tests were edited. Shell deployment test scripts were not run.
Raw local logs are under `/private/tmp/girafe-*.log` and
`/private/tmp/girafe-suite/`; baseline comparison logs are under
`/private/tmp/girafe-baseline-suite/`.

## Reproduction

From the isolated worktree:

```sh
Rscript scripts/test_girafe_shared_cache_identity.R --baseline
APP_COMPILED_RUNTIME=0 Rscript scripts/test_girafe_shared_cache_identity.R
Rscript -e 'testthat::test_dir("tests/testthat", reporter="summary")'
Rscript scripts/compile_runtime.R
APP_COMPILED_RUNTIME=1 Rscript scripts/test_girafe_shared_cache_identity.R
APP_COMPILED_RUNTIME=0 Rscript scripts/test_shiny_runtime_loading.R
APP_COMPILED_RUNTIME=1 Rscript scripts/test_shiny_runtime_loading.R --require-compiled
```

## Decision

The identity fix meets the targeted safety and reuse checks. The full-suite pass
criterion is not met in this environment, so the candidate is retained only as
an unapproved review commit. Verdict: **NEEDS REVISION** (validation incomplete).

Next step: independently validate this commit in an isolated environment with
the required datasets, desktop dependencies, TwoBit utility, and browser harness;
resolve or explicitly disposition baseline failures, then rerun the full suite
and the two-session oracle before making a keep decision. Do not deploy or merge.

Remaining limits: size+mtime does not detect deliberately preserved metadata;
concurrent file replacement during rendering is outside the normal stable-input
contract. Hashing costs scale with render-input size; the small benchmark is not
a worst-case large-gene benchmark. No browser-driven end-to-end search was run.
