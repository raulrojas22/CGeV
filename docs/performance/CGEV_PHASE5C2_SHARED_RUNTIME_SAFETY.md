# Phase 5C.2 — shared-runtime safety candidate

Baseline: `a7f0a99a98b4e8bfbeb5d67199d52bba6fccedca`.
Isolated branch: `codex/cgev-phase5c2-safety`.
Worktree: `/private/tmp/cgev-phase5c2-safety`.
The recorded pre-edit porcelain status is empty. The original dirty checkout
was not edited. No production deployment or configuration change is included.

## Traced failure

The original Phase 5C.1 fatal log has no stack. Replaying its `measure-reuse.cjs`
churn procedure against the baseline image with a diagnostic `later` trace
reproduced the same fatal error. One initial replay survived; the next three
search-active cycles failed. Diagnostic tracing observes errors without
catching or suppressing them and was removed before candidate validation.

Captured callback registration: `later::later(run_followup_queue, delay = 0.03)`.
Captured execution: `run_followup_queue()` →
`search_preparation_is_current(mode, token)` → `isolate(state$token())` →
`rv$get()` → destroyed-reactive error → `Execution halted`.
The exact reactive in this replay was `reactiveVal9bd0c7fa`, the homologous
`searchPreparationTokenHomo` selected by `preparation_state_for_mode()`.
A ended at 03:35:32.431955; the fatal callback ran at 03:35:32.616175
(Colors time, 2026-09-28), approximately 184 ms later.

Baseline source locations:

- `server.R:5745`: `schedule_fast_search_preparation()` owns the queue.
- `server.R:5777`: `run_followup_queue()` checks the generation token first.
- `server.R:5829`: it schedules another pass after 30 ms, including after the
  final item, so that pass can outlive the session.
- `server.R:5838`: initial follow-up scheduling; `server.R:5786`: retry scheduling.
- `server.R:5874`: `onFlushed` starts the deferred follow-up after preparation.
- `server.R:5737`: `search_preparation_is_current()` reads the session reactive.

This is application-session work, not a process task: its closure contains
session preparation state and generation checks even though part of its job
warms shared caches. Its `later` canceller was discarded. Shiny observer
auto-destruction does not cancel arbitrary application `later` callbacks.
The generation test cannot protect destruction because the test itself reads
a destroyed reactive. `session$isClosed()` is available without reading one.
The error wording says "module session" even for this top-level session state;
the captured stack does not implicate a plot module.

## Deferred-work taxonomy and scope

| Kind | Ownership | Observed patterns and handling |
|---|---|---|
| A. Server `later` callbacks | SESSION-OWNED | Preparation/retry/follow-up queues; cache, lookup-worker and renderer prewarm; metrics and index polls; registry refresh; batch/neighbor searches; rescue; progressive primary/isoform rendering; report bootstrap. All close over the originating session or its state. Use the centralized session scheduler. |
| A. Autocomplete timers | SESSION-OWNED | Quick scans/build queues publish to session-specific inputs and reactive caches. Use the same scheduler. |
| A. Report timers | SESSION-OWNED | Stage deadlines and the per-session cleanup loop. Cleanup does process-wide disk maintenance, but this loop is registered by a session and captures it. Cancel the loop when its owner ends. |
| A. Plot-module prefetch | SESSION-OWNED | Four existing timers in `R/modules.R` already check `module_destroyed` before reactive access; `onSessionEnded` sets it. Their separate module/card destruction behavior is unchanged. |
| B. Future computations | PROCESS-OWNED execution, session-originated inputs | Workers run pure lookup, sequence, STRING, alignment, indexing, or literature computations. Do not kill shared workers or change the global future plan. Already launched work may finish. |
| B/C. Future/promise continuations | SESSION-OWNED | 39 server, two autocomplete, and two report success/error continuations capture session state. Guard before entering their body. Keep the existing promise rejection behavior while alive. Plot-module continuations already have module-destruction guards and remain unchanged. |
| D. Observers/reactives | SESSION-OWNED | Registered in the Shiny session/module domain; Shiny owns observer destruction. Existing generation and request checks still apply while alive. No timer/observer redesign. |
| E. `onFlushed`, module teardown and downloads | SESSION-OWNED | Shiny-owned callbacks and session-bound handlers remain native; any application timer they create uses the guarded scheduler. |
| E. `future` work queues and library event-loop machinery | PROCESS-OWNED library machinery | Worker availability/promise settlement bookkeeping is unchanged. No package monkey patch is part of the candidate. |

No UNKNOWN application-owned registration was used as justification for a
change. This is a focused lifecycle inventory, not a new audit of global caches.

## Lifecycle mechanism

`make_session_deferred(session)` supplies `later` and `guard` functions.
The owner is explicit; it is not inferred from whichever reactive domain happens
to be current when a recursive callback reschedules itself.

- Register one teardown handler per helper instance, lazily when work is queued.
- Track only outstanding `later` cancellation handles. Remove each handle before
  running its callback, or when explicitly cancelled.
- On session end, mark a non-reactive flag, cancel queued timers, clear handles,
  and release the helper's session reference.
- Check the flag and `session$isClosed()` before entering any callback body.
- Run live callbacks in their owner's reactive domain. Return their values and
  let legitimate errors propagate; no new `tryCatch` suppression is used.
- Do not cancel shared workers. Promise callbacks become no-ops after their
  owner ends; existing in-flight promise references live until settlement.

A liveness check plus cancellation is sufficient for this failure. The existing
search generations still handle superseded work in a live session. Cancellation
alone would not cover future completions, and a reactive generation check alone
caused the captured fatal access. A global error handler would hide valid errors
and was rejected.

## Orthologous cache

The old key used `gff_cache_key(file)` and `normalize_gene_compact(gene)` but
stored the entire result, including detection, label, forced genome, and job
position. `gff_cache_key` already included annotation identity, size, rounded
mtime, and index version; it was not literally just a path. Missing context and
lossy gene normalization allowed incompatible results to share an entry.

The v2 key hashes a structured list using SHA-256:

- resolved annotation path;
- annotation size and full numeric modification time;
- GFF index format version;
- exact gene string (also returned in lookup metadata);
- forced genome string;
- file label (affects organism/alias context and returned metadata);
- explicit detection context (`job$det`, including the distinction from NULL).

Job position is not scientific identity: `file_idx` is rebound from the current
request on a hit. The current request's path spelling is also returned.
Identical scientific determinants share the process cache; session identity is
not included. The forced genome is a returned selector here, not a file read by
this lookup; its contents are consumed downstream. Other sequence/alias caches
are outside this change.

## Validation

Final measured results are recorded below. Completed churn and suite tests were not repeated after resuming.
Raw evidence is retained under `/private/tmp/cgev-phase5c2-evidence/` locally and
`/home/rarojas/cgv/benchmarks/phase5c2-20260928/` on Colors.

### Tests and environment limitations

- Local complete `testthat` run: **95 tests, 692 assertions, 24 files; 0 failures,
  0 warnings, 0 skips**. Of these, the new cache oracle has 39 assertions and
  the new lifecycle tests have 42. The other 611 existing assertions pass.
- The lifecycle tests cover cancellation of A without affecting B, live-domain
  and error behavior, late promise fulfillment/rejection, recursive scheduling,
  explicit cancellation, actual promise-pipe syntax, and collection of 20
  one-MiB payloads captured by cancelled long-delay callbacks.
- 22 targeted script checks pass across the local and isolated Linux runs:
  alias lookup, lookup observability, render lookup, verified orthology gate,
  cross-species notification/family behavior, download registration/selection/UI,
  reports, renderer prewarm, lazy rendering, autocomplete, sequence/literature/
  STRING futures, asynchronous STRING screen, compact GC rendering, memory
  budgets, progressive cards, footer reactivity, and the new real cache oracle.
- The real cache oracle executes the actual pipeline on human/mouse annotations.
  Repeated inputs return the cached value; forced-genome, label, detection, and
  annotation/species variants agree with fresh uncached results. Human results
  contain 501 rows; the mouse `Trp53` result contains 114 rows.
- Initial local data tests failed because a clean checkout has no downloaded
  reference data. The baseline checkout reproduced the local orthology-gate
  failure. With the existing reference data mounted read-only in the isolated
  image, the orthology gate, family suggestions, downloads, progressive cards,
  and footer tests all pass. No biological behavior was changed to make them pass.
- Initial future tests could not open localhost sockets under the sandbox;
  authorized reruns passed. Existing extracted-observer/scheduling test fixtures
  were updated to supply/recognize the new helper. The footer script's first
  prototype invocation lacked its required output argument; the corrected run
  passed. These initial failures remain in the raw evidence.
- **SKIPPED in the Linux image:** `testthat` is not installed there. The complete
  suite passed locally; no dependency was installed in production or the image.
- Local Shiny is 1.12.1; the actual prototype uses Shiny 1.14.0, later 1.4.8,
  promises 1.5.0. Runtime SHA-256 hashes of all four changed application files
  match the worktree, and the compiled-runtime manifest validates successfully.

The browser harness is the Phase 5C.1 harness with additional PID, memory,
post-teardown functional, and cross-species checks. Its final copy is retained
at `/private/tmp/cgev-phase5c2-evidence/measure-reuse.cjs`. The original teardown
schedule is preserved: concurrent human TP53/BRCA1 searches, stop A after the
first complete plot, check B after eight seconds. B then renders AMY1A as a fresh
functional probe. This uses a known fixture from the progressive-render tests.
Preliminary measurement failures (incorrect proxy-to-container mapping, a tunnel
lost during the pause, and an optional MYC strict-readiness timeout) are retained
separately and are not counted as successful acceptance cycles.


### Final isolated prototype results

ShinyProxy 3.2.4 used `allow-container-re-use: true`,
`minimum-seats-available: 1`, and `seats-per-container: 4`, on private port 28082
with separate networks, configuration, and copied prototype caches. Reference
annotations/genomes were mounted read-only. The diagnostic tracing was absent.
Candidate image: `localhost/cgv:phase5c2-candidate`, image ID
`2f9343c0e5c66b3dd65fe1aa9c24a652b13c414a010110bfdf086d818b85171c`.

- **Churn: 0/10 shared-runtime crashes; 10/10 innocent-session successes.**
  Each A teardown happened during the original search-active schedule. B kept
  its live socket and rendered a fresh AMY1A SVG with completed metrics afterward.
- All cycles used container
  `f4b2fc6cbd5f68918898cd0309ce94a74edbc40a1878e33c315041b881c6fae9`.
  Shared R host PID **3443809**, container PID **1**, start tick **98591449**
  stayed unchanged. Future workers **71 and 72** also stayed unchanged.
- Concurrent human TP53 / BRCA1 cards, plots, and notifications stayed with
  their respective sessions. No result/reactive crossover was observed.
- Final concurrent cross-species run: A rendered human TP53 and mouse Trp53;
  B rendered human BRCA1 and mouse Brca1. Both visible species cards in both
  sessions had an SVG and completed metrics. After A ended, B's socket and both
  rendered results survived in the same R process. Notifications named only
  the owning query. Cached orthologous lookup results were reused in this run.
- Real-annotation cache oracle: four context variants passed against fresh
  uncached pipeline results, in addition to the 39 adversarial unit assertions.
  This covers reuse of identical inputs and changes to forced genome, label,
  explicit detection context, and human/mouse annotation context.
- The optional MYC timeout was investigated: the browser displayed the expected
  “Possible Gene Matches” dialog, with exact MYC plus 24 additional matches,
  awaiting a selection. The harness incorrectly expected an immediate MYC plot.
  BRCA1 remained rendered; this was not a crash or failed lookup. No application
  behavior was changed for that diagnostic.
- An existing mouse chromosome display limitation was observed: the card says
  “Chr 77” for accession NC_000077. The chromosome formatter's parsed function
  body is identical to baseline; its fallback extracts accession digits when
  no mapping is available. This unrelated label issue was not changed. It is
  not evidence of cross-session cache contamination or a candidate regression.

### Warm startup

The five completed warm starts, using the Phase 5C.1 interactive-readiness
criterion, were **6.545, 7.785, 7.277, 5.294, and 6.145 seconds**.
All reused the same delegate. Median: **6.545 s**.
Compared with 6.50 s, the observed delta is **+0.045 s (+0.69%)**.
Compared with the historical cold median of 19.66 s, startup remains **66.71%
faster**. Five samples do not establish a statistically significant change;
the measured warm-start advantage remains substantial.

### Memory and retention

During the required ten-cycle churn, R RSS was **721.93 → 722.62 MiB**,
approximately **+0.69 MiB**, and stayed at 722.62 MiB for cycles 3–10.
The post-churn cgroup reading was approximately **837.37 MiB**. Worker RSS
remained 85,356 / 79,208 KiB. Session-end used-cell counts plateaued near
20.354 million. There was no monotonic growth across those repeated cycles.

The later cross-species workload loaded additional annotation/result data:
the reported process cache grew from **47.9 to 97.9 MiB**. Five subsequent warm
starts held R RSS at **844.6 MiB**, while session-end used-cell counts settled
near 23.368 million. The final cross-species run held RSS at 844.6 MiB. The
optional MYC family-dialog diagnostic ended at 875.7 MiB. After the long idle
pause, the final reading was **878.13 MiB R RSS / 992.52 MiB cgroup**, with the
same R and worker identities. This mixed-workload final value must not be
reported as the ten-cycle churn delta or proof of a lifecycle leak.

All **39 session starts had matching ends** in the final application log.
The helper has no global session registry, clears pending cancellations, and
releases its explicit session reference. Tests collected all 20 captured one-MiB
payloads after cancellation and verified zero pending handles. No obvious
lifecycle retention was introduced. This is not a full heap reachability proof;
in-flight promises can retain callbacks until settlement, and existing global
cache retention remains outside scope.

Final application and ShinyProxy logs have **zero** occurrences of
“Can't access reactive”, “Execution halted”, and “DelegateProxy crashed”, and
no warning/error/exception lines. Logs were saved before prototype cleanup.

## Candidate scope

Four application files changed (**200 insertions, 121 deletions**):

- `R/utils.R`: centralized session scheduler/guard and complete local cache key.
- `server.R`: guard the equivalent session-owned timers and continuations.
- `R/server_autocomplete_domain.R`: guard autocomplete deferred work.
- `R/server_shared_analysis_domain.R`: guard report deferred work.

Five existing test fixtures/expectations were updated:

- `scripts/test_autocomplete_quick_scan_smoke.R`
- `scripts/test_render_lazy_defaults.R`
- `scripts/test_renderer_prewarm_scheduling.R`
- `scripts/test_string_screen_async.R`
- `tests/testthat/test-first-paint-session-close.R`

Three new test files were added:

- `scripts/test_orthologous_local_cache_oracle.R`
- `tests/testthat/test-orthologous-local-cache-context.R`
- `tests/testthat/test-session-deferred.R`

Together with this report, the candidate contains **13 changed files**.
No production configuration, deployment script, module plotting implementation,
scientific search algorithm, or unrelated cache was changed.

## Evidence index

Local root: `/private/tmp/cgev-phase5c2-evidence/`.

| Evidence | File relative to evidence root |
|---|---|
| Exact baseline / clean worktree | `baseline-head.txt`, `baseline-status.txt` |
| Captured fatal registration and stack | `diagnostic-delegate.log` |
| Completed local suite | `testthat-final.log`, `testthat-results.rds` |
| Targeted scripts and real oracle | `script-test-final-status.json`, `remote/test-results/` |
| Ten accepted cycles | `final-churn/churn.json`, `churn-summary.json` |
| Final concurrent cross-species | `cross-species-final/cross-species.json` and screenshots |
| MYC timeout explanation | `myc-diagnostic/myc-diagnostic.json` and screenshot |
| Five warm starts | `warm-starts/run-warm5.json`, `warm-start-summary.json` |
| Memory and final fatal-log check | `memory-after-churn.txt`, `final-runtime-logs.txt`, `final-log-summary.json` |
| Tested source identity | `runtime-source-identity.txt`, `worktree-source-identity.txt` |
| Prototype cleanup / production liveness | `prototype-cleanup.txt`, `production-final-http.txt` |
| Original checkout final state | `original-final-head.txt`, `original-final-status.txt` |

Remote evidence and image build context remain under
`/home/rarojas/cgv/benchmarks/phase5c2-20260928/`. Only the isolated prototype
containers and networks were stopped after saving evidence.

## Acceptance and next step

All Phase 5C.2 KEEP criteria are met in the tested scenarios: relevant tests
pass; zero of ten search-active teardowns crash; innocent sessions, R, and
workers survive; A/B and cross-species isolation pass; cache oracles pass;
no scientific behavior regression was detected; the warm-start benefit remains;
and no obvious lifecycle leak was introduced. No remaining blocker was found
for this candidate. Independent validation is still required before considering
any production rollout. The existing chromosome-label limitation and known
out-of-scope global-cache, working-directory, and contention issues remain.

Next step: independently review the candidate commit and replay its targeted
cache/lifecycle and shared-runtime acceptance tests in a separate prototype.
Do not merge or deploy as part of Phase 5C.2.

**SAFE CANDIDATE — READY FOR INDEPENDENT VALIDATION**

- Production code modified: **NO**
- Production config modified: **NO**
- Production services restarted: **NO**
- Production containers manually stopped: **NO**
- Production cache intentionally modified: **NO**
- Deployment performed: **NO**
- Colors operational at end: **YES**

The original checkout was not edited; its HEAD and complete porcelain status
match the recorded initial dirty state. Its HEAD remains
`5510a256360f9596691dc74f3a5b92fe76d0923f`. The four production service container identities
remain unchanged. A production application delegate changed during the long
validation window; this task did not stop or restart it. Public production returned HTTP 302 at the homepage and
HTTP 200 for the existing static preview.
