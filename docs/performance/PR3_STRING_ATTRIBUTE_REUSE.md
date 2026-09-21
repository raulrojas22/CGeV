# PR3: STRING annotation projection reuse

Base: `3a27a5e77409faf7dc4abfe1de5b21d3f3e05c1d`.
Branch: `codex/cgev-pr3-gff-attribute-reuse`.

## Scope and lifetime

`build_string_query_payload()` previously parsed all visible plot rows for
screen candidates, then parsed the selected plot rows again for query candidates,
on every open. The upstream gene index contains lookup maps, while transcript
splitting extracts selected identity/relationship fields. Neither retains the
complete STRING projection with the existing parser's semantics.

The revised lifecycle observer only prunes removed/empty plots. Establishing or
restoring active plots performs no STRING projection work. A session-owned state
materializes subsets on demand from the payload builder. It parses each row
once for the two extraction traversals, then discards those parsed lists. The
existing gene-name and fallback-ID helpers still parse the first gene row twice
per materialization; their behavior is unchanged. Retained output is only gene
name, fallback ID, screen candidates and query candidates, plus the source
snapshot used for exact equality checks. No genome-wide parse, persistent cache,
worker change, lookup change, API change or PR2 cache change is introduced.

The getter compares the full plot data, organism information and annotation path
with `identical()` before reusing a projection. This covers attributes, row order,
feature type, coordinates/region, species and source replacement. Annotation file
changes affect this projection when loaded into the plot state, exactly as they
affected the previous payload builder; the builder does not reread disk files.
Title, active plot ordering, taxid fallback resolution and theme are still read
live. Context and plot ID separate entries. Removed plots are pruned on reactive
refresh without preparing any projection; session termination releases the state.
An open before the observer flushes validates synchronously and cannot use stale
attributes.

The source is the existing R plot data frame, retained with R's copy-on-modify
semantics, not a deep copy of the genome. Target candidate duplicates are retained
until the original filtering/ordering/cap code in the payload builder. The two
extraction traversals preserve their independent error boundaries and partial
results on malformed fields.

## Validation

Run from the worktree root:

```sh
Rscript scripts/test_string_annotation_reuse.R
Rscript scripts/test_string_network_roles.R
Rscript scripts/test_string_http_errors.R
Rscript scripts/test_string_future_globals.R
Rscript scripts/test_perf_flag_independence.R
```

All passed locally with R 4.4.3. The future transport test required permission
for a local loopback socket. Tests use fixtures/mocked HTTP, not live STRING.

The new regression compares complete payloads with `identical()` against an
unchanged function frozen from the base commit. Coverage includes GFF/GTF,
URL-encoded delimiters and repeated decoding, duplicate keys/rows, Parent and
identity precedence, missing/malformed attributes, no gene row, title and TaxID
fallbacks, different species, the 64-candidate cap, and 40 deterministic mixed
fixtures. It tests parser/decoder counts, invalidation, isolation between sessions,
pruning, restored state and real Shiny observer scheduling.

After preparation, six consecutive payload builds on the fixture perform **zero
parser and zero decoder calls**. Preparation invokes the parser N + 2 times for
N rows (the two unchanged primary-gene helpers account for +2). These are call
counts, not BRCA1 performance or RAM benchmarks.

## Timing and independent acceptance

Existing APP_PERF_TIMING behavior and STRING markers remain. New STRING markers:
`payload_prepare_ms`, `annotation_prepare_ms`, `annotation_materialized`,
`annotation_reuse`. The STRING run starts before payload construction so the new
total includes that stage. Attribute timing is per plot, never per row, and is
skipped when timing is disabled.

With APP_PERF_TIMING enabled, independently compare BRCA1 and TP53 on the base and
this branch. Capture initial gene/plot establishment, first Network open, repeated
opens, a gene/region change and a species change. Compare the exact query payload
and worker snapshot, parser/decoder counts, main-thread time before worker launch,
end-to-end lookup-plus-Network time, and peak/retained RSS.

## Revision status: requested-plot-only scheduling is unresolved

Removing the eager observer fixes the no-Network case, but does not by itself
satisfy the requested first-click behavior. `collect_screen_records()` in the
original payload builder includes annotation-derived IDs from all visible plots.
A fixture demonstrates that adding `protein_id=UNIQUE_Y_ALIAS` only to plot Y
changes `screen_variants` in the original payload for unchanged plot X. Reading
only X cannot reproduce that payload unless those other IDs already exist in an
equivalent structured representation. No such complete upstream projection has
been established. The present payload path still requests the other projections
lazily; this is explicitly not claimed as the requested final revision.

The revised tests load `R/string_worker.R` and assert nonempty screen variants and
the X/Y dependency. V1's test harness omitted that source file, causing the
builder's caught missing-function error to return empty screen variants. Its
screen-variant parity coverage was therefore incomplete. Full parity still passes
with the real collector loaded.

New lifecycle tests establish and restore 368 fixture plots with zero parser and
decoder calls. Pruning also adds zero calls. Existing invalidation and reuse tests
pass after adjusting their scheduling expectations. These are fixture assertions,
not BRCA1 measurements.

A scope decision is pending: allow work on the screen-candidate dependency while
preserving the complete payload, or accept removal of eager preparation while
retaining the required cross-plot lazy sweep. Neither omitted screen aliases nor
renaming that sweep as requested-plot-only preparation is an acceptable shortcut.

**Limitations:** cold projection construction is still synchronous. The current
lazy screen dependency can still trigger many projections on the first Network
open. Peak/retained RAM and real BRCA1/TP53 latency require independent validation.
