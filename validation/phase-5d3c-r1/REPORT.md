# Phase 5D.3C-R1 — scoped sequence identity reuse

## Verified parent and scope

Before editing, HEAD was exactly `ba5d9002d953502b97112c310bb44836aab808b0`, tree `9feb2acfa4184d0061a5d335e2999d6db7a0ad5f`; branch `phase-5d3c-sequence-identity`; status clean. The existing isolated clone `/private/tmp/cgev-phase-5d3c-20260929` was reused. Baseline/master remains `31debadccd9505f5ca7683445806b5632d9e5b13`.

The unchanged R1 implementation is commit `c3b4a049b873c92ab164a2d47398ff444d137f08`, tree `ed3510d276364af8500e4422ca69552b676bf021`, directly above ba5d900. This report now includes the independent adjudication in a separate documentation-only commit; the implementation candidate is not amended. Runtime edits are limited to `R/utils.R` and `R/modules.R`. Other additions are the focused R1 test and evidence in this directory. No changes to Colors, A_FULLAPP or master. The implementation close-out did not push or open a PR. Promotion preparation now authorizes a branch push and PR; merging and deployment remain excluded.

## Optimization design

The existing file identity, cache keys, per-path invalidation, stale-index rules, ctime checks, limits and LRU accounting remain unchanged.

1. Public sequence helpers accept optional `.sequence_context`. Without a valid context they call the existing `sequence_cache_validate`, preserving fresh filesystem validation for independent requests.
2. `sequence_operation_begin` creates a short-lived environment containing the prevalidated identity, input/canonical path association, working directory, runtime environment, owning call frame and its stack depth, and creating process ID.
3. A nested call may borrow it only while the owner is active on this process's stack, in the same runtime and PID, for the matching path. Relative input reuse also requires the same working directory.
4. The runtime's current per-path identity must still match the context. A reentrant independent call that advances the runtime to B makes the old A context unusable.
5. Invalid, expired, serialized, wrong-runtime, wrong-PID, wrong-path or superseded contexts fall back to ordinary fresh validation. A bare identity list is never sufficient to bypass a stat.
6. Only the owner performs `sequence_cache_finish` on exit. It revokes the context and releases owner/runtime references first, including on errors. No process-wide context registry is introduced.
7. Context is explicitly threaded through indexed/fallback FASTA helpers, metadata resolution, native/rtracklayer 2bit paths, spliced extraction and its exon loop, composition, `fetch_gene_data_sync`, and segmented downloads.
8. Both actual prefetch payloads in R/modules.R use `with_sequence_file_identity` to group their span/fetch work. Worker contexts are constructed locally after memo transport; an active context is never added to transported prefetch state.
9. Composition retains its fresh filesystem comparison before insertion on a miss, plus the supplied-splice identity comparison. 2bit sidecar write checks remain unchanged.

No TTL, session identifier, reactive/session state, broadcast purge, new dependency, whole-file hashing, broad cache redesign or unrelated 5D.3D/5D.4 work was introduced.

## Validation counts

One request after warmup, using the same operations as the committed 5D.3C request harness:

| Metric | ba5d900 TP53-like | R1 scoped TP53-like | ba5d900 BRCA1-like | R1 scoped BRCA1-like |
|---|---:|---:|---:|---:|
| Identity computations | 30 | 2 | 54 | 2 |
| file.info calls | 30 | 2 | 54 | 2 |
| Paths supplied to file.info | 90 | 6 | 162 | 6 |
| Entry validations | 15 | 1 | 27 | 1 |
| Exit validations | 15 | 1 | 27 | 1 |
| normalizePath calls | 46 | 18 | 82 | 30 |

Each identity still stats source + `.fai` + `.gzi` in one vectorized file.info call. These are R-level counts, not kernel syscall counts. File existence checks, normalization and backend reads may perform additional filesystem operations. Composition cache misses retain an additional insertion-time identity check, so the two-check result is specifically for the warmed logical workload.

**Independent calls intentionally do not share snapshots.** Running R1 through the original unscoped sequence of separate public calls still uses 28/52 identity computations (one nested FaFile pair is removed). The two-check result requires the explicit logical boundary, as integrated in actual prefetch/fetch/segmented operations. It does not claim that an entire UI card or separate reactive events share one snapshot. See `counts.tsv`.

## A. Codex original-host performance protocol and results

`benchmark-three-states.R` executes the existing committed `validation/phase-5d3c/benchmark.R` and `request-benchmark.R` harnesses. The timing reporter is replaced with five-batch median/min/max reporting. Request sample size remains 500 per batch after 10 warmups; direct/splice/composition microbenchmarks use 2,000 calls per batch after 20 warmups. Garbage collection precedes each timed batch.

The original request body is unchanged for baseline and ba5d900. For R1, the driver transforms only its invocation boundary: the same body is placed inside `with_sequence_file_identity`, and the three sequence calls receive `.sequence_context`. Coordinates, sequences, exon count, request order and backend choice are unchanged. The full span still exceeds the spliced span by one base, so this warmed workload includes one indexed scan on every repetition. It is not a pure memo-hit workload or an end-to-end UI benchmark.

Extra backend measurements use 500 calls per batch after 20 warmups. Indexed interval measures repeated indexed reads with a warm handle. Fallback interval removes only the exact interval entry before each call to exercise a warm full-record fallback; its measured cost includes that identical cache-drop operation and a base-sequence assertion in all three states.

The initial exploratory run overlapped other validation. Final measurements were serial, without concurrent tests or compilation. `performance-final.tsv` combines baseline and parent measurements from `performance-before-pid.tsv` with the final PID-guard R1 measurements in `performance-r1-final.tsv` (raw output: `performance-r1-final.txt`). Baseline and parent source were unchanged, so those measurements were not repeated. `performance-final.txt` is the earlier pre-PID serial log, not final R1 evidence. The exploratory measurements remain in `performance.tsv` for transparency.

These are host-specific Codex measurements, retained unchanged. The 41.1% / 38.4% recovery below was not independently reproduced and must not be generalized. All times are median milliseconds. Recovery is `(parent − R1) / parent`; negative means slower.

| Operation | Baseline | ba5d900 | Final R1 | Parent regression | R1 recovery | R1 overhead vs baseline |
|---|---:|---:|---:|---:|---:|---:|
| Direct interval warm hit | 0.0270 | 0.2165 | 0.2365 | +701.9% | -9.2% | +775.9% |
| Indexed interval scan | 2.0440 | 2.7860 | 2.5360 | +36.3% | +9.0% | +24.1% |
| Fallback record hit, interval evicted | 0.4300 | 1.0640 | 0.7940 | +147.4% | +25.4% | +84.7% |
| Spliced warm hit | 0.3395 | 0.6710 | 0.6975 | +97.6% | -3.9% | +105.4% |
| Composition warm hit | 0.5025 | 0.6915 | 0.7035 | +37.6% | -1.7% | +40.0% |
| TP53-like request | 4.8540 | 9.5120 | 5.6000 | +96.0% | +41.1% | +15.4% |
| BRCA1-like request | 7.5280 | 14.0540 | 8.6520 | +86.7% | +38.4% | +14.9% |

**Original-host observation:** On this host, R1 removes 3.912 ms (41.1%) from the parent TP53-like operation and 5.402 ms (38.4%) from BRCA1-like. Remaining baseline overhead is 0.746 ms and 1.124 ms, respectively. These synthetic 20 kb/11-exon and 80 kb/23-exon operations approximate the sequence portion of a card; they do not measure complete CGeV rendering, annotation work or network latency. Actual card savings depend on how much work is grouped in the integrated scope.

The removed cost is repeated identity construction/stat/path processing across nested calls and exons. One entry and one exit validation remain; cache lookup, backend reads and exon processing remain. Independent memo hits retain two stats and add context-lifetime checks, explaining their small R1 increase.

Before the PID guard, serial R1 request medians were 5.392/8.274 ms; final guard medians are 5.600/8.652 ms (+3.9%/+4.6%). Those separate runs are not a paired isolation experiment, so the entire difference cannot be attributed to the PID comparison. Counts remain exactly two, and the necessary guard does not erase the material recovery.

The complete samples, ranges and counts are retained in the TSV files. Single independent memo hits can have a small context-lifecycle overhead in R1: their two stats are intentionally preserved. The improvement comes from removing repeated path/stat validation inside explicitly grouped work, not from skipping independent-request validation. Existing parent measurements (4.616→9.074 ms TP53-like, 7.292→13.974 ms BRCA1-like) and the independent validator's different host timings remain valid historical measurements; the three-state table above is the current same-host comparison.

## B. Independent-validator paired measurements

The independent adjudication supplied for promotion on 2026-09-30 applies to exact implementation commit `c3b4a049b873c92ab164a2d47398ff444d137f08` and tree `ed3510d276364af8500e4422ca69552b676bf021`. The following are the validator's same-host paired medians, as reported by the user; they are separate from the Codex benchmark and are not a rerun performed for this documentation update.

| Scoped request | Baseline (us) | ba5d900 (us) | R1 (us) | Reduction vs ba5d900 | R1 overhead vs baseline |
|---|---:|---:|---:|---:|---:|
| TP53-like | 4470 | 5233 | 4854 | 379 us / 7.2% | 384 us / 8.6% |
| BRCA1-like | 7117 | 8326 | 7546 | 780 us / 9.4% | 429 us / 6.0% |

The independent validator confirmed the performance direction and mechanism, but did not reproduce the large absolute recovery in the Codex original-host measurements. The 41.1% / 38.4% figures are host-specific observations, not independently reproduced or generally reproducible results.

## C. Structural result and performance conclusion

The strongest portable evidence is the independently confirmed reduction from **30/54 to 2/2 identity computations and file.info calls** for the warmed scoped TP53/BRCA1 logical requests. Nested calls and exon loops reuse the same active operation identity; independent requests still validate freshly.

**Benefit confirmed; magnitude is filesystem/host dependent.** Keep R1. On the independent validator's host these scoped requests remain approximately 6–9% above baseline. Independent micro-calls remain intentionally more expensive because fresh validation is preserved. No claim is made that either host's absolute timings predict complete UI card latency.

## Independent validation adjudication

**R1 INDEPENDENT PASS — SAFE TO PROMOTE**

The independent validator confirmed:

- Persisted-cache and next-request scientific correctness are preserved; a stale generation cannot survive outer replacement detection.
- Expired and serialized contexts cannot bypass fresh validation, and independent requests freshly validate the filesystem.
- The PID guard rejects a context inherited by an actual fork. Removing that guard reproduced stale A in the fork negative control.
- Persistent multisession workers self-heal without restart or broadcast invalidation.
- Full suite: **1,288/1,288 expectations passed**.
- Scoped TP53/BRCA1 identity computations are **30/54 -> 2/2**.
- Request-level improvement versus ba5d900 was independently reproduced, with the host-dependent magnitude shown above.

## Scientific adversarial regression

All existing 21 Phase 5D.3C tests pass unchanged: 344 expectations. The known oracle remains A=`AAAATTTTAAAATTTT`, B=`CCCCGGGGCCCCGGGG`; exons 1–4 and 9–12; expected splice `AAAAAAAA`→`CCCCCCCC`; A/T/C/G counts 8/0/0/0→0/0/8/0.

| Case | R1 result |
|---|---|
| Atomic replacement | B bases and B composition |
| Same-inode rewrite | B bases and B composition |
| Same-size replacement | B bases and B composition |
| Preserved mtime, changed ctime | B; unchanged mtime and changed ctime asserted |
| Deletion/recreation | Empty on deletion; B after recreation |
| Repeated A/B cycles | Correct bases/counts on every cycle |
| Wider-span promotion/unseen region | New 5–8 interval is GGGG, not old TTTT |
| Positive/negative strand splice | Independent literal/reverse-complement oracles pass |
| Transcript composition | C=8 and correct denominator/length after replacement |
| Supplied stale splice | Unproven or old identity cannot poison B composition |
| Old FASTA index/layout | Correct sequential fallback, including fresh runtime; indexed use resumes after reindex |
| Header/seqname/index-only replacement | Fresh names and resolution |
| 2bit names/offsets/sidecar | Correct B bases; old sidecar rejected |
| Malformed/truncated FASTA | No old sequence returned |
| Unrelated genome | Remains warm; reader sentinel passes |
| Relative paths and symlinks | Existing canonical-path oracles pass |
| Copied worker memo state | Filesystem comparison invalidates A |

## Mid-request replacements and exact in-flight behavior

R1 tests use deterministic hooks with independent literal expected bases/counts. The scope is not a transactional file snapshot.

| Hook | In-flight caller receives | State inside operation | After outer exit | Next independent call |
|---|---|---|---|---|
| After outer snapshot, before warm nested extraction | `AAAAAAAA` | Existing warm A entries are still available under the A snapshot | All sequence/derived entries for path purged | `CCCCCCCC`, C=8 |
| Between two uncached exon reads | `AAAACCCC`, A=4/C=4 | Two interval entries; composition insertion refused by fresh stat | Zero sequence/derived entries for path | Full B and `CCCCCCCC`, C=8 |
| After extraction, before composition insertion (count hook) | A-derived counts A=8 | Composition not inserted; other extraction entries may exist | Zero sequence/derived entries for path | C=8 |
| Immediately before outer exit validation | A-derived counts A=8 | Entries, including composition, have already been inserted under the A snapshot | Exit sees B and purges them | Full B, splice B, C=8 |
| Actual wide transcript per-exon loop, replacement between reads | `AAAACCCC` | Two per-exon reads occur within one owner | Interval/splice state purged | `CCCCCCCC`, C=8 |

Additional R1 oracles verify expired context revocation, serialized active-context rejection, default independent calls inside an active scope, reentrant identity advancement, wrong-path rejection, cleanup on errors, and constant validation counts for 11/23 exons.

## Persistent workers

The final full suite explicitly ran with `CGV_SEQUENCE_WORKER_TEST=1` and `future::multisession(workers=I(1))`:

- Ordinary independent-call worker: PID **95318**, correct four A/B/A/B requests.
- New explicitly scoped worker: PID **95375**, correct four A/B/A/B requests.
- Each test asserts exactly one unique PID and checks both bases and counts on every request.
- No restart, broadcast purge or session-specific invalidation.

Actual prefetch future-global regression also passes for FASTA/2bit, both strands, and warm/cold transported memo state.

## PID ownership and actual fork

A fork inherits the parent's active stack and runtime pointers. The explicit `context$pid == Sys.getpid()` guard rejects that otherwise plausible capability before reuse. The final full suite includes this actual fork test; on Windows the fork-specific case is skipped, while ordinary/default and multisession paths still use the same guard and fresh-validation fallback.

Supplemental final-tree `fork-probe.R` recorded parent PID **31257**, child PID **31274**, inherited active context TRUE with owner PID 31257, match FALSE, and **two child identity computations / two file.info calls**. Child returned `CCCCCCCC`; no A splice was cached; its next independent request returned B and C=8. Parent exit left zero relevant entries; the parent's next request also returned B and C=8. The PID-only mutation fails this scientific oracle.

## Memory and lifetime

The unchanged large-fixture memory harness passes against R1: 12 A/B cycles of a 1,048,576-base FASTA and 524,288-base splice retain exactly **3,159,136 bytes** each cycle.

| Cache/state | Entries | Approximate retained bytes |
|---|---:|---:|
| Interval | 3 | 1,575,152 |
| Spliced | 1 | 525,552 |
| Fallback full record | 1 | 1,049,840 |
| Header | 1 | 1,992 |
| Index seqnames | 1 | 1,200 |
| Resolved seqname | 1 | 1,264 |
| Composition | 1 | 2,600 |
| File identity state | 1 | 1,536 |

The existing 24-cycle small fixture remains at 12,808 bytes. Twelve 2bit cycles retain counts 2/1/1/1/0/1 for interval/splice/seqinfo/native-index/handle/composition; bytes are 8,168 initially and 8,616 thereafter, exactly as the parent (accounting metadata initialization).

An additional 1,200 scoped operations (100 per replacement cycle) keep one entry each in interval/splice/fallback/composition/identity state, **7,968 bytes** for those five environments throughout. No cache value contains a request-context environment. Every retained test token is inactive with owner/runtime references NULL. This checks that context objects do not accumulate in runtime caches. PID ownership is one scalar on the temporary token, not a registry or process handle. Runtime caches retain no request context or live call/session/process object. Object-size totals are approximate retained binding sizes, not process RSS.

## Regression totals

- Existing invalidation: **21 tests / 344 passing expectations**.
- New R1: **11 tests / 69 passing expectations**, including the same-PID scoped worker.
- Existing sequence/GC: **4 tests / 38 passing expectations**.
- Native 2bit in a fresh process: **2 tests / 9 passing expectations**.
- Combined focused set: **38 tests / 460 passing expectations**, zero final failures/errors/skips.
- Full testthat suite: **154 tests / 1,288 passing expectations**, zero failures, errors, warnings or skips.
- Source loader: **740 bindings**, pass.
- Compilation: **22 runtime source files**, pass.
- Compiled loader with `--require-compiled`: **740 bindings**, pass.
- Additional R1 tests using verified compiled R/utils.R: **54 expectations**, no failures/errors; two worker/fork cases intentionally skipped in this extra compiled-only run; both ran explicitly in the final full source suite.
- Actual sequence-prefetch future-global script: pass.
- Selected-sequence download script: pass.

Harness/environment corrections are retained openly: an initial sandboxed full-suite attempt could not launch existing processx packaging operations; the full permitted run passed. An initial combined focused runner loaded rtracklayer before the native test's namespace-isolation assertion; running native tests in their required fresh process passed. These were harness/environment failures, not runtime changes. The final full suite also passes that assertion in normal suite order.

## Mutation / negative controls

1. Test-environment bypass of `sequence_cache_validate` invalidation and `sequence_cache_finish` purge: the new R1 tests produce **28 failed expectations**, 26 passes, no errors, two worker/fork skips. Failures include retained A entries and A bases/counts on the next B request, not only validation counters.
2. External source copy `/private/tmp/cgev-r1-provenance-mutant.R` removes only the supplied-splice provenance comparison: existing invalidation tests produce **2 scientific failures**, 333 passes, no errors, worker skipped. Both failures return A=8/C=0 where B requires A=0/C=8.

3. External PID-only mutation removes the process ownership comparison: **2 failed expectations**, 67 passes, no errors/skips. The actual fork then accepts the inherited context and returns `AAAAAAAA` after replacement with B.

The committed runtime contains none of these bypasses. Tests and raw output document all three controls.

## TOCTOU semantics and explicit answers

**Before:** ba5d900 performed entry/exit stats at each nested public consumer. A change could be discovered at the next nested boundary, and an in-flight read still lacked transactional snapshot guarantees.

**After:** an explicitly scoped operation shares the initial identity across nested/exon reads, with one owner exit check. Composition misses still perform their insertion-time check; sidecar writes retain their source check. A change between nested reads may therefore be detected later, at the owner exit. Old or mixed results can be returned by an operation overlapping a concurrent replacement, as the deterministic hooks show. This coarser detection timing is deliberate and must not be described as an atomic sequence snapshot.

- **Did a 5D.3C scientific guarantee weaken?** No persisted-cache or next-independent-request guarantee weakened in the tests. In-flight detection granularity is coarser; results during concurrent replacement are not guaranteed to match what ba5d900 would have returned. Neither version guaranteed a transactional snapshot.
- **Can an old generation remain cached after replacement is detected?** Not after the owning exit validation detects the change: all relevant path entries are purged before that operation returns. Intermediate entries may exist while an operation is still active. Composition insertion is refused when its fresh source check disagrees.
- **Does the next independent request revalidate?** Yes. Default calls validate; expired/copied/superseded capabilities cannot suppress validation. Supplying the still-active capability explicitly denotes participation in the same logical operation.
- **Are workers self-healing without broadcast?** Yes; both persistent-worker modes and transported memo tests pass.
- **Can another PID borrow a context?** No, including an actual fork inheriting an active stack.
- **Can expired or serialized contexts bypass validation?** No; active ownership/runtime/frame checks reject them.
- **Worth keeping?** Yes: the scoped workload uses two rather than 30/54 identity computations and the final measured logical-request latency improves substantially. Small independent-hit overhead remains explicit in the performance table.

Residual limits are inherited from ba5d900: indistinguishable size/mtime/ctime metadata, coarse/unavailable ctime, Windows creation-time semantics, NFS attribute caching and changes after the final stat. No hash/inode/content-transaction guarantee is added. Deployment-filesystem timing remains host-specific; independent revalidation of the exact implementation has now passed. An operation requiring a consistent snapshot while files are concurrently rewritten still needs coordinated publication or a stronger file-reading contract outside this revision.

## Evidence provenance and close-out

All final full-suite, loader, memory, mutation and final R1 benchmark evidence includes the PID guard. `focused-final.tsv` is extracted from the final full suite. Earlier individual R1 results (10/63 and 8/49), `full-sandbox.*`, and initial combined `focused.txt` are historical diagnostics, not final totals. `totals.txt` lists every retained result with its run name. TSV exports preserve test outcomes; binary RDS duplicates are omitted from the commit.

The prefetch and download scripts were run individually after resumption and both passed on the unchanged final runtime. Their earlier rejected compound command never executed and is not a test failure. They build synthetic fixtures; no production dataset was required. Compilation outputs remain ignored build artifacts and are not evidence substitutes for loader success.

## Blockers and next step

No implementation or independent-validation blocker remains. Preserve ba5d900 and c3b4a049 unchanged. Publish the branch and open a PR to master with this documentation-only adjudication, then inspect CI/check status. Do not merge or deploy as part of promotion preparation. No runtime or test change, and no expensive validation rerun, is required for this report-only update.

R1 INDEPENDENT PASS — SAFE TO PROMOTE
