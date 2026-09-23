# Phase 4A: demand-driven multi-transcript grouping

Required baseline: `a5ec791f9270de0b4f2ef202c98d473d6f744a3e` (PR #42).
Isolated branch: `codex/cgev-phase4a-demand-grouping`.
Worktree: `/private/tmp/cgev-phase4a-demand-grouping`.
No merge, deployment, or modification of PR #42.

## Dependency audit (recorded before implementation)

HMG is already a lazy Shiny reactive. Multiple readers within one valid reactive
cache generation do **not** independently rerun its body. Four measured executions
therefore require invalidation/recreation, not simply four call sites. The supplied
~1.1 s measurement is prior evidence, not a measurement made by this experiment.
Instrumented full-server baseline tracing is used below to distinguish these cases.

Every direct consumer, with baseline server.R lines:

| Line | Consumer | Need |
|---|---|---|
| 10613 | homoAlignedSelectedGroupKey | Selected/fallback Synteny group |
| 10627 | homoAlignedPlotIds | Ordered group plot IDs |
| 10636 | homo_visual_mode_ui | Always active; Synteny choice availability |
| 21530 | hydrate_homologous_isoform_cards | Expansion: find canonical anchor for a noncanonical seed |
| 22215 | build_homologous_lastz_card_ui | LASTZ selector; currently disabled modes |
| 22790 | homo_special_cards_ui | Guarded by aligned mode; Synteny card/selector |
| 23825 | homo_aligned_footer | Guarded by aligned mode; group label/track count |
| 23894 | build_homo_lastz_footer_ui | Disabled LASTZ footer paths |
| 25146 | resolve_isoform_expand_request | Requested expansion membership, order and anchor |
| 31930 | toolbar observer | Always active; enables Synteny workspace button |
| 31969 | visual-mode observer | Legacy LASTZ-mode redirect availability |
| 31988 | header-mode pick observer | User action; validates Synteny availability |
| 32052 | homo_header_mode_switch | Always active; Synteny button availability |
| 37784 | shared analysis availability callback | Report modal/open/capture demand |
| 37787 | shared analysis groups callback | Requested report group selections |

Indirect readers: homoAlignedPlotIds uses the selected-group reactive;
homoCanonicalReferencePlotId and homoLastzOrderedPlotIds use aligned IDs;
Synteny render and its bindEvent event expression read aligned IDs (the event
expression can run before the render body's mode guard). Shared-analysis domain
calls the injected callbacks when opening/configuring or capturing a report.
No Network/GO consumer. First gene-card creation and scientific footer do not
read HMG. Canonical pagination/load-more banner uses primaryPlotIdsHomologous,
not HMG; isoform expansion uses HMG as listed above.

Dependencies: ordered IDs from sortedPlotIdsHomologous; plotGeneMetaHomologous;
organismInfoHomologous; annotationPathsHomologous; titlesHomologous. Sorting
reads active IDs and homo_sort_mode; outside the lazy default load-order branch,
it also reads fileData, titles and organism information. Therefore sorting may
invalidate HMG even when the resulting ordered IDs are identical. Whole-map
metadata/title updates also invalidate HMG even for irrelevant entries/fields.
Gene search batch commits these maps before exposing active IDs; removal, clear,
restore, and card/module changes can update the same state.

The calculation is deterministic for these values and the existing pure title
and gene-key helpers. Output includes group key, ordered unique plot IDs, gene
and organism labels and transcript count label; transient row tx_label/total_tx
are not exported. No clock, random value, sequence lookup or external service
is used. A safe conservative equality boundary is the five full values above.
The minimal semantic projection is ordered active IDs, each eligible record's
`total_transcripts`, `query_gene`, `display_gene_name`, `matched_gene_name`,
`matched_gene_id`, organism name, annotation path and title. Retaining the full
five values avoids introducing a second interpretation of those fields.

Zero initial calculations would require changing the availability consumers or
adding a separate exact availability algorithm. That is larger than a narrow
reuse experiment. Preserve complete toolbar/card semantics and evaluate only
through actual reactive readers, with no eager observer or background work.

### Baseline trace and chosen boundary (before implementation)

The real BRCA1 full-server probe reproduced two nonempty body executions:
276 ms after search commit, 286 ms after card initialization. The second execution
had identical ordered IDs; changed keys in meta/org/ann/titles were exclusively
`1_c`. This canonical copy is not among the 368 active grouping IDs. The supplied
four-execution count is not yet reproduced in this headless probe; browser input
round trips may contribute additional invalidations. Do not equate four consumers
with four executions. The supported defect is redundant grouping after changes to
whole maps outside active IDs.

Chosen implementation: retain one successful result within the session's lazy HMG
reactive closure. Compare ordered active IDs plus those IDs' entries in each of
the four maps with `identical()`, without hashing. Missing entries remain NULL.
Compute through the original body only on mismatch; replace the single entry on
success. Empty IDs also replace the retained entry. No historical or process cache,
new observer, timer, future or speculative computation. Keep full per-ID records
rather than a hand-maintained list of individual metadata fields. This is a safe
conservative projection: nonactive entries cannot be read by the original body.
All relevant active-record changes and ordering changes force recomputation.
Organism/gene search inputs alone do not key the result: the actual loaded plots
are authoritative, including additive multi-gene/multi-organism states.

### Consumer dependency and safety classification

`R` means a reactive subscription; `I` means an isolated/event-handler read.
All consumers are left unchanged. Removing necessary subscriptions would leave
buttons, selected groups or tracks stale after search/removal/restore.

| Direct consumer | Execution / subscription | Is the dependency needed? Alternative and risk |
|---|---|---|
| selected-group key | Lazy reactive; R to HMG and selected input | Yes: removed selection must fall back to a valid group. isolate/eventReactive would stale the key. |
| aligned IDs | Lazy reactive; R to HMG and selected-group key | Yes: changed membership/order must propagate to alignment/reference tracks. |
| visual-mode UI | renderUI, suspendWhenHidden=FALSE; R | Yes for exact availability, even while hidden. A separate cheap exact predicate is possible but duplicates grouping identity logic; isolating would stale choices. |
| isoform hydration anchor | Function called during expansion; explicit isolate, I | Already demand-only. Needs a current snapshot; no persistent HMG subscription. |
| LASTZ card builder | Function called by guarded special-card UI; R if reached | Modes are excluded by allowed_special_modes='aligned'. No present speculative execution; preserve for compatibility. |
| special-card UI | renderUI; returns before HMG unless aligned; R when active | Yes: labels, choices and card presence must follow state. Existing guard is appropriate. |
| aligned footer | renderUI; mode guard plus hidden-output suspension; R when active | Yes for current gene/organism/track count. No safe unconditional isolate. |
| LASTZ footer builder | Function; render callers begin req(FALSE) | Unreachable in current UI; if enabled it needs current labels/reference. |
| isoform request resolver | Expansion/event path; explicit isolate, I | Already correctly demand-only; must read latest group/anchor/copy. |
| toolbar observer | Eager observer; R to IDs and HMG | Needed for current Synteny availability; hiding/suppressing could incorrectly enable or disable controls. Separate availability predicate would be a distinct larger change. |
| legacy visual-mode redirect | observeEvent(homo_visual_mode); I | Handler is already isolated by observeEvent; validates availability when invoked. |
| header-mode pick | observeEvent(homo_header_mode_pick), ignoreInit=TRUE; I | Already user-demand/event scoped. Could avoid read for compact picks, but always-active availability readers still require HMG. |
| header-mode switch | renderUI, suspendWhenHidden=FALSE; R | Required to reflect availability across changes; no unconditional isolate. |
| report availability callback | Called by show_share_analysis_dialog and report-capture event; I via event handlers | Not an initial global reactive subscriber. Reports need current availability when requested. |
| report group-options callback | Called only for requested multi-gene complete capture with Synteny available; I | Already demand-driven; omitting state could report old groups. |

Dependency graph:

```text
activePlotIds + homo_sort_mode
    + (non-load sort only: fileData, titles, organismInfo)
                    -> sortedPlotIdsHomologous --+
plotGeneMeta / organismInfo / annotationPaths / titles --+--> HMG
    (whole maps can change for nonactive 1_c)            |
                                                       +-> visual-mode UI
                                                       +-> toolbar enabled state
                                                       +-> header choices/pick/legacy redirect
                                                       +-> selected group -> aligned IDs
                                                       |    -> canonical reference / LASTZ ordering
                                                       |    -> alignment renderer bindEvent / tracks
                                                       +-> special-card + aligned/LASTZ footer
                                                       +-> expansion resolver/hydrator -> isoform cards
                                                       +-> requested report availability/group choices
```

This is broad upstream invalidation, not reactive recreation. HMG is constructed
once in the session server closure. Initial empty state costs essentially nothing.
Within an unchanged generation, many reads are cache hits. No independent grouping
reactives were found; all 15 consumers refer to the same object. `bindEvent`'s
aligned-ID event dependency can pull HMG before its rendering mode guard, but
removing it alone would not remove the always-active availability demand and could
miss changed tracks. Report options do not create an eager startup grouping chain.

Why not just use the existing keyed-map getter? Three maps support it, but
annotationPaths is a whole-map reactiveVal and sorted IDs can invalidate without
changing order. Changing the map factory or adding an eager equality observer
would broaden the change. The single previous active-state comparison handles
these sources without suppressing any original subscriptions. Only the expensive
calculation is skipped on an exact match; cheap projection may still reevaluate.

## Separate future phases (not implemented)

After this experiment, evaluate OpenTelemetry for Shiny; future_promise versus
mirai + ExtendedTask; nanonext serialization/peak memory; possibly crew as a
shared worker pool; lightweight multi-user sessions; shared data/cache architecture.
These are outside this branch. No scheduling, worker count, STRING, prewarm,
rendering, font, container or infrastructure changes are included.

## Validation results (2026-09-23)

The measured counter is **nonempty expensive grouping computations**, not calls to
HMG and not executions of the new cheap reactive wrapper. The wrapper remains
subscribed to the same upstream reactives and can still reevaluate after unrelated
map writes. Repeated consumers reuse Shiny's cache; equivalent active state also
reuses the previous grouping result after an invalidation.

Primary browser samples, milliseconds unless stated otherwise:

| Measurement | TP53 before | TP53 after | BRCA1 before | BRCA1 after |
|---|---:|---:|---:|---:|
| Grouping computations | 2 | 1 | 2 | 1 |
| Individual grouping wall times | 44 + 22 | 29 | 359 + 669 | 321 |
| Total grouping wall time | 66 | 29 | 1,028 | 321 |
| Search observer | 589 | 549 | 3,477 | 1,769 |
| Append END to render START | 172 | 157 | 1,971 | 1,248 |
| First plot ready (server) | 2,098 | 2,331 | 7,657 | 4,092 |
| First websocket message (client) | 0.6 | 0.9 | 5.9 | 0.7 |
| Complete card (client) | 2,273.3 | 2,490.9 | 8,105.6 | 4,513.2 |
| Maximum sampled event-loop lag | 2,088 | 2,335 | 7,654 | 4,170 |
| Main RSS at complete card (MiB) | 842.9 | 586.2 | 814.1 | 1,108.0 |

First plot ready uses the server search clock; websocket/complete-card timings use
the client's `generate1` event clock. Append gap is between existing backend log
markers. The 50 ms benchmark timer measures main-process scheduling delay; it is
not production instrumentation. A first websocket message need not contain the
completed plot. Complete-card events reported both sequence and metrics ready.

Worker snapshots were TP53 before 62.8/15.8 MiB, TP53 after 33.3/67.0 MiB,
BRCA1 after 15.8/15.8 MiB. A matching BRCA1-before worker snapshot was not retained.
These are snapshots, not peak-memory measurements. Main/worker RSS is affected by
GC, resident-page reclamation and process history; no memory improvement is claimed.
The guard retains one active-state projection and one group result per session.

These are exploratory individual runs, not randomized repetitions. Long session
interruptions and cache/process history differed; the after BRCA1 run followed
TP53 expansion and Synteny. The later baseline parity replay itself measured TP53
57 ms (2 computations), complete card 2,438.8 ms, and BRCA1 726 ms (2 computations),
complete card 5,218.4 ms. This variability prevents attributing the observed complete
card differences (+217.6 ms TP53; -3,592.4 ms BRCA1 in primary samples) to HMG alone.
A fresh final after-build BRCA1 replay again computed once (360 ms), but took
8,131.5 ms to complete the card, with a 4,219 ms observer and 7,544 ms maximum
loop lag. This is recorded as `final` in the evidence, not substituted for the
primary sample. No established end-to-end, event-loop or memory improvement is claimed. The
supported result is one redundant expensive grouping computation eliminated per
normal local search. The supplied four-evaluation search was not reproduced.

### Functional and scientific checks

- Production calculation AST from `norm_key_local` through returned groups is
  identical to the required baseline. No grouping semantics were rewritten.
- Saved real TP53 and BRCA1 group results match baseline exactly, including
  membership, ordering, keys, plot IDs, labels and counts. Regression fixtures use
  actual captured state as well as synthetic 26/368-transcript cases.
- First complete TP53 and BRCA1 cards have identical text and plot SVG after
  normalizing generated SVG IDs (and CSS declaration order). Sequence composition,
  metrics, transcript labels, coordinates and plotted scientific content match.
- TP53 expansion produces canonical card 1, alternative cards 2..26, then 1_c in
  both builds. All 27 card texts match. SVG content matches after normalizing IDs,
  CSS order and the rx/ry=3.4 decoration on three cards. That decoration is applied
  by the unchanged `www/js/rounded_rects.js`; raw expanded DOM markup is therefore
  not claimed byte-identical.
- TP53 Synteny has the same 26 tracks and identical normalized SVG and card text.
  Expansion, Synteny and report-preview interaction do not add after-build grouping
  computations for the stable state. TP53 and BRCA1 report-preview text matches exactly. The final BRCA1 report
  preview also adds zero grouping computations.
- Actual repeated BRCA1 search reports 368 duplicates, zero additions, zero new
  grouping computations (470 ms observer). It does not replace existing cards.
- Regression executes the real expansion resolver and alignment selection
  reactives, including all 368 BRCA1 IDs, canonical-copy position and group order.
  Canonical load-more pagination is independent of HMG and unchanged; no separate
  multi-gene pagination browser replay was performed.
- Regression covers active metadata/title/path/organism changes, reversed order,
  multi-gene/multi-organism groups, card removal/recreation, empty state,
  same-state repeat, rapid A->B->C before demand, singleton/no-Synteny, distinct
  sessions with identical IDs, and session closure. No stale results observed.
  Rapid navigation and organism switching are reactive-level tests, not a browser
  stress test. BRCA1's full 368-card DOM and 368-track Synteny rendering were not
  exhaustively replayed; exact downstream ID/order parity is automated.

### Automated tests

Passed:

```sh
CGEV_PHASE4A_FIXTURES=/private/tmp/cgev-phase4a-evidence/browser-before \
  Rscript scripts/test_hmg_demand_grouping.R
Rscript scripts/test_render_lazy_defaults.R
Rscript scripts/test_render_lookup_optimizations.R
Rscript scripts/test_shared_analysis_domain.R
```

The focused test deliberately creates three equivalent-state invalidations and
compares baseline 4 computations with after 1. This is a regression workload,
not the measured browser search count. Last real-BRCA1 regression run measured
1,089 ms versus 214 ms for that workload. Parse checks and `git diff --check` pass.

### Reproduction and evidence

`evidence/phase4a-hmg/measurements.json` preserves primary samples and the additional
baseline replay; `regression.log` preserves the focused test results. Full local
logs and real-state RDS fixtures are under `/private/tmp/cgev-phase4a-evidence`.
These temporary files are not required by the default regression test.

From the isolated worktree, run each variant with a separate output/cache directory:

```sh
CGV_DATA_ROOT=/path/to/reference-data Rscript scripts/benchmark_hmg_browser.R \
  before /tmp/hmg-before 7844
CGV_DATA_ROOT=/path/to/reference-data Rscript scripts/benchmark_hmg_browser.R \
  after /tmp/hmg-after 7844
```

Run one app at a time; use the normal browser UI to select Homo sapiens, search TP53,
then BRCA1, open transcript expansion, Synteny and the report preview. The script
reads baseline server.R directly from the exact base commit and injects benchmark
probes into a temporary copy. No application observability framework was added;
worker counts and optional analysis scheduling remain unchanged. Stop the test app
and its own workers afterward. Do not use a production cache/output directory.

### Recommendation

**KEEP EXPERIMENT** for independent DeepSeek A/B validation. One legitimate initial
computation remains for Synteny availability. BRCA1 still incurs about 321 ms of
synchronous grouping in the primary after sample, and substantial search/render
main-thread work remains. This branch does not investigate or optimize those
other costs. Repeat controlled, alternating runs before making a production
latency or memory claim. Preserve the isolated branch; do not merge or deploy.

## Changed files and safety

Only `server.R` changes production behavior. Added `scripts/test_hmg_demand_grouping.R`,
`scripts/benchmark_hmg_browser.R`, `scripts/hmg_browser_probe.R`, this audit, and
small evidence files under `docs/performance/evidence/phase4a-hmg/`. Benchmark-only
apps/workers and browser tabs were stopped/closed. The original checkout status
and SHA-256 hashes of its dirty/untracked files were checked against the saved
pre-experiment snapshot. No production services or configuration were changed.
