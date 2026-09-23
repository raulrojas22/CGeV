# CGeV Phase 2A — STRING Main-Thread Extraction Implementation

Date: 2026-09-22. Experimental implementation for independent DeepSeek benchmarking.
No production deployment, merge, resource-limit change, or multi-user improvement claim.

## Baseline and isolation

GitHub `refs/heads/master` and the fetched `origin/master` both resolved to
`c5a667b74c9ccd0e1f4860fa7d7c9ac35292e3ee` before implementation; the Phase 1 baseline had not advanced.
Branch: `codex/cgev-phase2a-string-screen`.
Worktree: `/private/tmp/cgev-phase2a-string-screen`.
The original dirty checkout's status and binary diff were compared with their
pre-experiment copies. The explicitly protected untracked proposal's checksum
was also verified. None of those files is part of this change.

## Boundary and implementation

Before: `open_string_network` called `build_string_query_payload`, which called
`stringAnnotationState$screen` and `string_prepare_screen_rows` synchronously.
Only the subsequent STRING HTTP/cache worker was asynchronous.

Now the observer captures its request token and plot context, materializes the
selected plot's existing full annotation projection, and captures missing screen
batches. A `future_promise` on the existing pool runs the unchanged
`string_prepare_screen_rows`. Main checks relevance before merging, then resumes
the existing query builder, cache checks, HTTP future, display roles and widget.
A completely warm screen cache needs no new future.

Only two runtime files change:

- `R/string_annotation.R`: split the existing session screen operation into capture
  and merge; retain the synchronous method for existing callers; add the small
  `baseenv()`-bound worker and explicit-globals constructor. Empty cache-hit
  batches no longer invoke the preparation function in main.
- `server.R`: capture immutable attribute batches, submit and return, guard the
  completion, merge into the same session cache and resume the existing path.

Additional files: `scripts/test_string_screen_async.R` and this report.
Scientific function bodies (`string_prepare_screen_rows`,
`string_screen_ids_from_rows`, `string_prepare_gff_identifiers`) were compared
with the baseline and are unchanged. `R/utils.R` and its parser are unchanged.
Homologous/orthologous batching, row order, first-failure cutoff, candidate order,
cache equality/pruning and cache keys remain intact. No screen cache or in-flight
screen deduplication was added. Existing per-session HTTP promise reuse remains.

Worker input is a list of raw character-vector batches plus a timing flag.
Output is the existing prepared row lists, with optional timing metadata.
Explicit globals are exactly `string_screen_future_worker`, `screen_batches`,
and `timing`. Source snapshots, session state, merge plans, `lib_env`, input,
output, and caches remain in main. The worker sources the existing utilities and
annotation implementation into its own `baseenv()`-parented environment, following
the established STRING worker pattern.

Relevance requires an open session, the same request token, selected plot/context,
and identical active plot IDs, annotation data, organisms, annotation paths and
titles. Both screening and HTTP completions use this guard. The existing modal
close event invalidates the token. Stale work is discarded, not cancelled.
Launch failures and rejected workers use the existing STRING error widget;
there is no synchronous fallback. A rejected screen job never merges row results.

## Validation

Passed relevant existing scripts:

- `Rscript scripts/test_string_annotation_reuse.R`: payload/parser edge cases,
  session cache lifecycle, 368-plot lazy preparation, cross-plot display roles,
  and gated timing.
- `Rscript scripts/test_string_network_roles.R`
- `Rscript scripts/test_string_http_errors.R`
- `Rscript scripts/test_string_future_globals.R`
- `Rscript scripts/test_sequence_prefetch_future_globals.R`
- `Rscript scripts/test_literature_future_globals.R`

New regression: `Rscript scripts/test_string_screen_async.R`. It exercises a real
multisession worker, explicit FutureGlobals with an unrelated 10 MiB library
cache attached, row and complete-payload parity, and actual production Shiny
observer code. Cases cover success, modal close, data change, selected-plot
navigation, superseding request, worker failure, launch failure, and session
close, plus the transition into the HTTP promise (success, rejection, launch
failure and close while HTTP is pending). Assertions require no expensive screen computation in main and no stale
merge/render. Production widget construction is exercised on an offline graph.
The test accepts `CGEV_PHASE2A_FIXTURES` containing `ACTB.gff`, `TP53.gff`, and
`BRCA1.gff` for larger real-data cases; its default fixtures need no annotation
installation or network.

Real annotation validation used local NCBI GRCh38.p14 data: ACTB 13 rows,
TP53 501 rows, BRCA1 15,964 rows. Combined-plot complete payloads matched the
frozen pre-PR3 oracle (33/64/64 target candidates, respectively; 10,866 screen
variants each), including ordering. A separate final real-data run compared
against the verified Phase 2A baseline. Transcript-split observer payloads also
matched exactly for all three genes.

The prior independent parser fuzz script was rerun against this implementation:
40,000 cases, zero mismatches, including NA, encoded keys, extra equals signs,
GTF, malformed tokens, URL escapes and non-ASCII values. Expected malformed-input
warnings were emitted. Scientific function-body equality was checked separately.

Live STRING worker smoke tests with a temporary cache resolved:

| Gene | Selected STRING protein | Nodes | Edges |
|---|---|---:|---:|
| ACTB | 9606.ENSP00000494750 | 9 | 26 |
| TP53 | 9606.ENSP00000269305 | 9 | 25 |
| BRCA1 | 9606.ENSP00000418960 | 9 | 35 |

Each response produced a valid `visNetwork` widget and HTML export. Browser visual
inspection was blocked by the browser URL policy for local files; no workaround
was attempted. This is not a complete browser/end-to-end app acceptance test.

## Local measurement

macOS ARM64, R 4.4.3, future 1.68.0. One real multisession worker for the harness;
application worker configuration is unchanged. The harness executes the actual
baseline/candidate observer and query builder against real transcript splits,
with a fixed downstream cached graph and display roles to isolate screening.
It includes real widget construction, but excludes real disk/HTTP role-resolution
cost and other app observers. These are single samples, not capacity estimates.

| Gene | Transcript plots | Baseline observer blocking | Candidate observer blocking | Worker screen compute |
|---|---:|---:|---:|---:|
| ACTB | 1 | 94 ms | 48 ms | none; full selected projection supplies screen IDs |
| TP53 | 26 | 126 ms | 38 ms | 113 ms |
| BRCA1 | 368 | 2,311 ms | 55 ms | 2,343 ms |

The ACTB timing difference is subject to warmup/order noise; its one-plot case
has no screen work to offload.

For BRCA1 the baseline prepared **8,858** raw rows in main PID **77827**, taking
2,184 ms. The candidate performed zero screen-preparation calls in main and
prepared the rows in worker PID **77879**. Capture/submit was 54 ms; dispatch
wait 5.5 ms; return interval 71.9 ms; merge 44 ms; resumed payload assembly 60 ms.
Candidate completion took approximately 2.60 seconds in this harness: freeing
the event loop does not imply lower single-request completion latency.

BRCA1 transport measurements (five serialization samples):

| Item | Serialized bytes | Median serialization |
|---|---:|---:|
| Attribute batches | 2,138,117 | 1 ms |
| Prepared row lists | 2,368,295 | 5 ms |
| Entire explicit globals list | 2,139,566 | 1 ms |

`FutureGlobals` reported total size **2,139,500 bytes** and only the three declared
names. Serialized overhead beyond input is **1,449 bytes**. Adding an unrelated
10 MiB environment did not export it. These measured sizes differ from the Phase
1B report; no size reduction or parser optimization was attempted.

New timing is gated by `APP_PERF_TIMING`: `screen_capture_submit_ms`,
`screen_dispatch_wait_ms`, `screen_worker_compute_ms`, `screen_return_ms`,
`screen_merge_ms`, and worker PID. Dispatch includes queueing, transport and
future startup; return includes serialization, IPC and event-loop scheduling.
They are interval estimates, not pure queue/serialization measurements.
Existing timing remains; `screen_prepare_ms` for an async capture/merge includes
elapsed waiting, while `payload_prepare_ms` measures the resumed builder.
Serialization was measured separately in the harness, not added to production.

## Remaining synchronous work and limitations

Main still performs the selected-plot full projection
`string_prepare_gff_identifiers` (including scalar parsing/name/ID extraction),
snapshot capture and raw-row deduplication, merge row mapping/ID unions, query
assembly and same-taxid candidate collection. Main also retains
`string_try_cached_payload`, resolution cache/disk access, and
`string_apply_display_roles` on cache hits and HTTP completion. The existing
completion path can resolve missing display candidates synchronously. Widget
construction and unrelated plot/`safe_girafe` work remain synchronous.

Snapshots remain alive in the originating session until its promise completes.
Stale queued/running jobs consume their normal worker slot until completion;
there is no cancellation, scheduler or admission control. If underlying plot
state changes without another Network request, the discarded request does not
automatically restart; reopen Network. No claim is made about N-user latency,
worker saturation, production memory, throughput, or optimal worker count.
Independent DeepSeek benchmarking is still required.

**Did this move measured work out of the main Shiny event loop, or merely rearrange
code?** It moved the measured 8,858-row computation to a different R process,
with zero expensive preparation calls in main and a measured observer reduction
from 2,311 ms to 55 ms in the controlled local harness.

```text
BEFORE  MAIN: capture → screen compute → query/cache/roles → submit HTTP
        WORKER: HTTP → MAIN: roles + widget

AFTER   MAIN: capture → submit screen → return to event loop
        WORKER: unchanged screen compute
        MAIN: relevance check → merge → query/cache/roles → submit HTTP
        WORKER: HTTP → MAIN: relevance check → roles + widget
```
