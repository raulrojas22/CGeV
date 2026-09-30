# PR #54 follow-up: asymmetric FASTA companion indexes

## Baseline and scope

Verified clean checkout on branch `phase-5d3c-sequence-identity`, HEAD `5c7e094dea4cd7e8fc16573f1e7da8ffa67a0e23`, tree `8901373b8f1c18c1c2b105e2deecdfddb7d17ecd`. The original ba5d900/c3b4a049/5c7e094 history remains unchanged. Work used `/private/tmp/cgev-phase-5d3c-20260929`; Colors and A_FULLAPP were not touched.

Only runtime file changed: `R/utils.R`. No redesign of sequence identity, scoped operations, ownership/PID checks, composition provenance, workers, cache keys or unrelated caches.

## Defect reproduced before runtime editing

Real bgzip FASTA fixtures use installed Rsamtools 2.22.0 and independently generated literal sequence oracles, with multiple compressed blocks and different line wrapping. On the original PR head:

- Only `.gzi` regenerated: stale `.fai` rejected by age on this filesystem; correct fallback.
- Only `.fai` regenerated: stale `.gzi` trusted, indexed fetch failure and downstream composition error.
- Neither regenerated: rejected; correct fallback.
- Both regenerated: indexed reads correct.
- Case K, stale A `.fai` copied with fresh metadata beside B/fresh `.gzi`: wrong bases, splice and composition on repeated calls, also in a fresh runtime. Fourteen assertions failed for K.

`baseline.tsv/txt` retain the initial reproduction. Their preserved-mtime decision assertion initially expected indexed access; it was corrected to fallback because restoring source mtime updates source ctime after index publication. That provisional assertion is not a scientific defect. The stale-gzi and Case K failures above are scientific failures.

## Fix and rationale

`sequence_cache_validate` derives `.fai` and `.gzi` existence, unchanged identity, and age separately. Either existing companion remaining unchanged across a source replacement, or older than the source, blocks indexed access. The existing per-path purge removes indexed handles and sequence/derived state before installing the new identity.

Metadata cannot prove provenance: a copied old companion can have new timestamps. When an identity is first observed or changes and companions exist, otherwise eligible indexes must pass `sequence_fasta_companions_compatible`:

1. Create a temporary directory and private symlink to the current source.
2. Use the existing Rsamtools dependency to regenerate temporary indexes from that source.
3. Compare the complete `.fai` layout and `.gzi` block map with the supplied companions, in bounded 64 KiB chunks.
4. Remove the temporary directory and indexes on return/error. Published source and companions are not modified.
5. Any mismatch or inability to establish compatibility blocks indexed access and uses the existing streaming fallback.

This is deliberately stronger than a first-record check: a valid first record cannot certify later names, lengths, offsets, wrapping or BGZF block mappings. Regeneration is one full sequential source scan at a relevant identity transition, not whole-file hashing. It is not repeated on unchanged warm calls or nested exon reads. It also covers a fresh process with no prior source identity. No whole source copy is made. If symlinks, temporary storage or optional Rsamtools are unavailable, validation fails closed. Comparison is conservative: semantically equivalent but differently serialized indexes may be rejected safely.

The compatibility result uses the existing bounded per-path state, with no new retained index generations or registry. An identity state eviction can require validation again on the next observation.

Two `return(NULL)` branches in the indexed interval attempt previously returned from the entire extractor, preventing fallback. They now raise an error caught by that attempt's existing handler, allowing streaming fallback. The resolved name uses ordinary local assignment so fallback receives it without a runtime/global write. Successful indexed returns retain their previous behavior. No strand, exon-order, coordinate or backend-priority change.

## Regression matrix

| Case | Decision | Scientific result |
|---|---|---|
| A: new source + only new `.gzi` | Fallback | B interval, splice, composition; repeat correct |
| B: new source + only new `.fai` | Fallback | B interval, splice, composition; repeat correct |
| C: new source + both old companions | Fallback | B interval, splice, composition; repeat correct |
| D: new source + both regenerated | Indexed | B interval, splice, composition; repeat correct |
| E: Case K, copied old `.fai` with fresh metadata | Fallback | B interval, splice, composition; repeat correct |
| F: same compressed size, renamed contig | Indexed | Current contig and bases |
| F supplemental: same compressed size, changed exon base | Indexed | A then B interval/splice/composition, repeated |
| G: preserved source mtime, newer source ctime | Conservative fallback | B interval, splice, composition; repeat correct |
| Copied stale `.gzi` with fresh metadata | Fallback | B interval, splice, composition; repeat correct |
| Valid first record, bad later-record `.fai` offset | Fallback | Correct later-record interval/splice/composition |
| Unavailable indexed handle | Streaming fallback | Correct bases/splice/composition |
| Warm unchanged source | Reuse | No compatibility rescans |

Supplemental equal-size fixture: both compressed sources are 48,414 bytes, with changed base at position 70,001; 12 repeated bases/splice/composition assertions pass. A separate fresh R process (PID 86417) rejects Case K and passes repeated interval/splice/composition checks.

The companion persistent-worker test runs A/B/A/B with one PID, **86624** in the final full suite, including stale-gzi and K publications. Ordinary and scoped existing worker tests also pass with stable PIDs **86688** and **86730**, respectively. No restart or broadcast purge. Existing R1 fork/lifetime/mid-request cases pass unchanged.

Deterministic unit cases require no bgzip dependency: both companion ages, unchanged future-dated companions across source changes, failed compatibility, and one compatibility decision per identity. Real compressed tests skip only when optional Rsamtools is absent; worker tests additionally require local worker execution.

## Tests and loaders

Final tested runtime:

| Set | Tests | Passing expectations |
|---|---:|---:|
| New companion tests | 12 | 140 |
| Existing sequence identity | 21 | 346 |
| R1 scoped operation/fork | 11 | 69 |
| Sequence/GC | 4 | 38 |
| Native 2bit | 2 | 9 |
| Focused total, extracted from final full run | 50 | 602 |
| Full testthat | 166 | 1,430 |

Final full run: **zero failures, errors, warnings or skips**. `focused-final.tsv` and `full.tsv` record final outcomes. The initial focused companion run also passed 140 expectations; final full run includes the subsequent narrow indexed-attempt failure fix.

The old metadata test deliberately fabricated an incompatible `.fai` and expected its invented name to be trusted. It now builds valid indexes for normal refresh assertions and requires rejection/fallback for the invented index. This is the only edit to an existing test; it adds two assertions. An initial full run exposed these three obsolete expectations; the final run above passed after updating that fixture/expectation.

Source loader and required compiled loader both pass **741 bindings**. Runtime compilation passes for **22 source files**. No production fixture or deployment was needed. Tests and benchmark scripts parse successfully.

## Negative control

An external source copy restores only the old `sequence_cache_validate` from 5c7e094, neutralizing independent companion gating and layout compatibility. The working implementation remains unchanged. New tests produce **31 failed expectations**, 96 passes, zero errors, one intentionally skipped worker case. Case K alone produces **14 scientific/decision failures**, including wrong interval bases, splice and composition on repeats. The corrected failure-to-fallback path masks the former NULL-only symptom, but the wrong-base Case K oracle remains load-bearing.

`negative-control.R` reproduces this external mutation without editing the runtime file. Raw mutant results are retained.

## Performance and R1 counts

The existing scoped request workload is preserved (20 kb/11 exons and 80 kb/23 exons). Each state is warmed, then measured serially in five batches of 500 calls. Original sequential samples and a second serial alternating-batch comparison are both retained; the latter reduces host/time drift. No tests or compilation run concurrently with timing.

| Serial alternating batches | Parent 5c7e094 median | Candidate median | Observed difference |
|---|---:|---:|---:|
| TP53-like | 5.452 ms | 5.606 ms | +0.154 ms / +2.8% |
| BRCA1-like | 8.600 ms | 8.742 ms | +0.142 ms / +1.7% |

The earlier serial all-parent-then-all-candidate run was noisier: TP53 5.434 -> 6.034 ms (+11.0%), BRCA1 8.214 -> 9.392 ms (+14.3%). Its full samples remain in `performance.tsv`; the follow-up alternating batches remain in `performance-paired.tsv`. There is no claim of zero measured warm overhead or a portable percentage. The compatibility function is never invoked on those warm calls, so these timings do not measure repeated compatibility work. No performance-driven runtime optimization was made.

Both parent and candidate retain **2 identity computations, 2 file.info calls, 1 entry check and 1 exit check** per warmed TP53/BRCA1 operation. Candidate compatibility calls during each measured warm request: **zero**. `counts.tsv` records the instrumentation. This retains R1's structural reduction from the pre-R1 30/54 to 2/2.

Separate cold compatibility measurements: median **2 ms for 150,000 bases**, **32 ms for 5,000,000 bases**, five samples after one warmup on real bgzip sources. This cost scales with source size and filesystem; it must not be hidden or extrapolated as a whole-genome guarantee. A multi-GB genome can incur a noticeable first-observation/replacement cost. Obvious older/unchanged-companion failures bypass this rebuild and go directly to fallback. Unchanged source/companions do not rescan.

## Residual limitations and next step

Metadata identity still cannot detect an indistinguishable size/mtime/ctime replacement, and NFS caching/coarse or unavailable ctime retain their prior limits. Validation is not an atomic source/index snapshot: concurrent publication can affect an in-flight request; outer exit detection purges affected state and subsequent independent requests revalidate. Composition retains its insertion-time check. Coordinate-consistent reads during concurrent writes require a publication/snapshot contract outside this fix.

Cold validation intentionally scans the complete source to certify all companion mappings. It can repeat after actual identity changes or bounded provenance eviction, not on every read. Temporary-symlink restrictions or inability to rebuild indexes mean safe streaming fallback, which may be slower. Compressed fixtures exercise the existing `.gz` fallback path; this change does not expand compression-format support.

Create one narrow commit atop 5c7e094, push the existing branch to update PR #54, and wait for CI. Do not resolve review thread `PRRT_kwDORaFyks6nYDxD`, merge, deploy or automatically make a second fix if CI fails. Exact commit/tree and CI outcome are reported after publication.
