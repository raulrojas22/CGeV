# CGeV Phase 5D.3C — sequence cache identity

## Status and checkout

SAFE CANDIDATE — READY FOR INDEPENDENT VALIDATION

- Repository: raulrojas22/CGeV.
- Fetched master commit: `31debadccd9505f5ca7683445806b5632d9e5b13`.
- Verified master tree: `d62e00af9abc0e24781d0688325f426c28e76814`.
- Fresh clone: `/private/tmp/cgev-phase-5d3c-20260929`.
- Branch: `phase-5d3c-sequence-identity`.
- The final response records the candidate commit/tree, after this report is committed.
- Runtime changes: **R/utils.R** and **R/modules.R** only. One implementation commit includes tests and this evidence.
- No changes to A_FULLAPP, Colors, or the user's main checkout. No push, PR, merge, or deployment.

## Baseline reproduction

The fixtures are A=`AAAATTTTAAAATTTT`, B=`CCCCGGGGCCCCGGGG`, exons 1–4 and 9–12. Independent expected splices are `AAAAAAAA` and `CCCCCCCC`; expected A/T/C/G counts are 8/0/0/0 and 0/0/8/0. Oracles use literal sequences, substrings, character counting, and a separate reverse-complement construction.

Atomic replacement, same-inode truncating rewrite, same-size replacement, preserved-mtime replacement with changed ctime, and recreation all reproduced stale results. The indexed backend returned fresh B for direct exact extraction, but an unseen 5–8 subregion returned old `TTTT` through wider-span promotion. The fallback returned A even for the exact interval. Both returned the old splice and old composition, including composition under a changed file-version key. Deletion returned empty sequence in these fixtures, but did not eliminate stale state: recreation reused A.

The baseline persistent worker PID **66349** returned `AAAAAAAA` and A=8 both before and after replacement. No worker restart or cache export between those requests.

See `baseline-matrix.txt`, `baseline-probe.txt`, and `baseline-tests.txt`. Running the final new test file against baseline produced **129 failed expectations and 1 error**; the worker and new provenance-API test were skipped in that run. The separate baseline matrix exercised the worker.

## Cache inventory on the exact baseline

`P` means canonical sequence path; ranges are inclusive genomic coordinates. All listed environments are declared in R/utils.R. Scientific exposure means either bases or their selection/composition can be wrong.

| Cache/state | Baseline key | Backing files | Value | Callers and scientific exposure | Decision |
|---|---|---|---|---|---|
| `.seq_extract_cache` | P::seqid::start::end | FASTA or 2bit; FASTA index companions | Interval bases, including promoted spans | `extract_sequence_from_fasta`, splicing, module GC/render paths, selected-sequence downloads, `fetch_gene_data_sync`; direct wrong bases | Fix |
| `.spliced_seq_cache` | P::seqid::strand::normalized exon ranges | Sequence plus FASTA indexes | Spliced bases | `extract_spliced_exon_sequence`, fetch, composition, downloads; wrong strand-adjusted transcript | Fix |
| `.fasta_fallback_seq_cache` | P::resolved name | FASTA, including gzip fallback | Full sequence for one record | Sequential fallback extraction; wrong bases in any interval | Fix |
| `.fasta_header_cache` | P | FASTA | Full headers and chromosome/name map | `get_fasta_header_map` → resolution → extraction; wrong record selection | Fix |
| `.fasta_seqnames_cache` | P | FASTA `.fai` | Index record names | `get_fasta_index_seqnames`, resolution, server prewarm; wrong/missing record selection | Fix |
| `.fasta_resolved_seqname_cache` | P::requested seqid | FASTA headers and `.fai` | Resolved name or negative result | `resolve_seqname_in_fasta`, indexed and fallback extraction; wrong/empty sequence | Fix |
| `.fafile_handle_cache` | P | FASTA, `.fai`, `.gzi` when present | Rsamtools FaFile object | Direct/batched extraction and prewarm; handle itself observed fresh, but index may describe old layout | Invalidate with source/index state; no backend redesign |
| `.twobit_seqinfo_cache` | P | 2bit; validated seqnames sidecar | Seqnames | 2bit resolution/extraction; wrong record choice | Fix |
| `.twobit_native_index_cache` | P::size::formatted numeric mtime | 2bit | Names, offsets, endianness | Native 2bit extraction and splicing; stale offsets and names | Fix; use one current path entry |
| `.twobit_handle_cache` | P | 2bit | rtracklayer TwoBitFile | Non-native 2bit and wide-exon paths | Clear with directly dependent source state |
| `.transcript_composition_cache` | P||size:formatted mtime||seqid||strand||exons | Sequence through spliced input | Counts, denominator, length, formatted composition | Modules and `fetch_gene_data_sync`; versioned key can store A-derived counts for B | Fix; invalidate path and validate supplied-sequence provenance |
| 2bit seqnames disk sidecar | Digest of canonical dataset path, fixed schema | 2bit fingerprint + `.seqnames.rds` | Seqnames | Metadata cold load | Add exact numeric ctime/mtime validation and refuse writes from an older source identity |
| `.sequence_file_state` (new) | P | Sequence + `.fai` + `.gzi` | Current identity and stale-index flag only | All listed entry points and transported prefetch state | Bound to 200 paths; eviction purges that path first |

Extraction preference remains indexed FASTA, sequential FASTA fallback; native 2bit, rtracklayer, CLI fallback. Compact multi-exon transcripts retain the single-span path; wide transcripts retain existing batched/per-exon paths. Exon normalization, coordinates and reverse complementation were not edited. The tabix caches belong to annotation and were excluded. No alias, neighbor, report, or remote-service invalidation was changed.

## Design

A shared identity stores canonical path, numeric size, numeric mtime and numeric ctime. Numeric timestamps preserve the precision available from R/file.info without string formatting or integer truncation. Missing values become deterministic numeric NA values. A single `file.info` call covers the source and both FASTA index companions (three filesystem entries).

The identity is **not appended to large-value keys**. On an identity change, all memo entries belonging to that path are removed using the existing cache-drop accounting. Other paths remain warm. Composition and native 2bit index keys now retain only the current path generation. Existing LRU entry/byte limits remain intact. The small identity map has a 200-path bound and purges a victim's sequence caches before discarding its provenance.

Each public cache consumer validates before lookup. Exit checks purge results if the source/index identity changed during work. Composition additionally checks before insertion. The fetch path passes the identity captured before extracting its supplied splice; unproven or mismatching supplied sequence is re-extracted. This closes the A-splice/B-composition poisoning path while preserving supplied empty-transcript semantics for a matching identity.

An unchanged or older `.fai` after source replacement is not used. The existing sequential fallback reads the replacement until a fresh index appears. Missing indexes may still be built by the existing implementation. First observation also rejects indexes older than their FASTA, so provenance-map eviction does not rehabilitate a stale index. No index files are rewritten or deleted by invalidation itself.

Prefetch transports the small provenance environment with memo tables. A worker compares it against its own filesystem; unproven imported cache state is purged. Handles remain local. There are no session IDs, reactive objects, new dependencies, whole-file hashes, broadcast purges, or new runtime .GlobalEnv writes.

## Adversarial matrix

| Case | Candidate result |
|---|---|
| Unchanged file | Correct bases/counts; warm calls succeed with file-reader sentinel that rejects cold reads |
| Atomic A→B | Correct B, splice C×8, C=8, indexed and forced fallback |
| Same-inode rewrite | Correct B and derived results |
| Same-size replacement | Correct B and derived results; equal size asserted |
| Preserved mtime, changed ctime | Correct B; unchanged mtime and changed ctime asserted |
| Deletion | Empty interval/splice; composition known_total=0 |
| Recreation | Correct B, no retained A reuse |
| A/B/A/B cycles | 24 small FASTA cycles, 12 large FASTA cycles, 12 2bit cycles; bases/counts checked each cycle |
| Wider-span promotion | Old span removed; new 5–8 subregion is GGGG |
| Previously unseen region | Same independent GGGG oracle |
| Spliced transcript | Both strands checked against independent expected bases |
| Composition | Expected counts and length; supplied old splice rejected |
| Metadata | Header, name resolution, `.fai`-only replacement; 2bit names, native offsets and stale disk sidecar |
| Persistent worker | Four alternating replacements, same PID, expected bases/counts |
| Unrelated path | Reader sentinel proves unrelated cached genome stays warm |
| Symlink/relative paths | Correct canonical-path reuse and new target contents |
| Truncated/malformed FASTA | No old bases; requested interval/splice empty |
| Changed FASTA layout with old `.fai` | Correct fallback bases in warm and fresh environments; indexed backend resumes after reindex |
| Replacement during composition extraction | In-flight A result is not cached for B; next request returns C=8 |
| Sidecar write race | Old source identity cannot stamp old names onto new-file fingerprint |
| Copied worker memo state | Old serialized splice rejected using worker filesystem identity |

Final focused coverage: **21 tests, 344 passing expectations, zero failures/errors/skips**. Final persistent worker PID: **67915**, four A/B/A/B results, unchanged PID. The standalone candidate matrix also confirms A→B in PID **67654**. See `full-suite.txt`, `focused-final.txt`, and `candidate-matrix.txt`.

## Retained memory

`object.size` is summed over retained bindings, including cache metadata; shared R objects may be counted more than once. This is an approximate retained-object measurement, not process RSS.

For a 1,048,576-base FASTA and 524,288-base splice, all **12** A/B cycles retained exactly **3,159,136 bytes**. Counts were constant:

| State | Entries | Bytes |
|---|---:|---:|
| Interval | 3 | 1,575,152 |
| Spliced | 1 | 525,552 |
| Full fallback | 1 | 1,049,840 |
| Header | 1 | 1,992 |
| Index seqnames | 1 | 1,200 |
| Resolved name | 1 | 1,264 |
| Composition | 1 | 2,600 |
| Identity state | 1 | 1,536 |

Small FASTA tests retained 12,808 bytes throughout the final 24-cycle run. For 12 alternating 2bit replacements, interval/splice/seqinfo/native-index/handle/composition counts were **2/1/1/1/0/1** each cycle. Retained bytes were 8,168 initially and 8,616 thereafter (cache-accounting metadata initialization), with no generation growth.

Existing bounds remain: intervals 1,000 entries and 96 MiB (desktop 256 MiB); splices 1,200 and 64 MiB (desktop 192 MiB); fallback 8 entries/96 MiB and 5 million bases per cached record; composition 2,000/16 MiB; native index/handles 40 entries; FASTA metadata 200 entries, resolved names 5,000. Existing coordinated memory accounting remains in use. Path invalidation releases obsolete generations before these limits are reached.

## Performance

Local R timings, 2,000 warm calls per component; single-run means with millisecond elapsed-timer resolution. These are synthetic/local filesystem measurements, not production percentiles.

| Component | Baseline µs/call | Candidate µs/call |
|---|---:|---:|
| normalizePath | 11.5 | 12.5 |
| file.info, source | 73 | 71 |
| file.info, source + two companion paths | — | 69 |
| Identity construction from existing stat result | — | 3 |
| Full identity, including path/stat | — | 98 |
| Identity comparison | — | <0.5 |
| Warm identity validation | — | 99 |
| Sequence-cache lookup alone | 5.5 | 6 |
| Actual interval warm hit | 24.5 | 227 |
| Actual spliced warm hit | 318 | 661 |
| Actual composition warm hit | 465.5 | 703 |

Entry and exit identity checks explain most added warm-call time; raw lookup cost is unchanged. Cold nested entry points validate independently because they are also callable directly; exit checks are deliberate replacement-during-read protection. No unrelated optimization was performed.

Synthetic request benchmark: one span extraction, one splice, one composition, and one interval per exon, after warmup (500 requests):

- TP53-like, 20 kb/11 exons: **4.616 → 9.074 ms**, +4.458 ms.
- BRCA1-like, 80 kb/23 exons: **7.292 → 13.974 ms**, +6.682 ms.

### Exact workload and validation counts

These are 500 repetitions of a synthetic sequence workload after 10 warmups, not end-to-end UI card timings. Each repetition calls `extract_sequence_from_fasta(1, span)`, `extract_spliced_exon_sequence`, `get_transcript_composition_cached`, then `extract_sequence_from_fasta` separately for each of 11 or 23 exons. Each exon is 100 bases long. The final exon ends at span−1. Consequently the full 20 kb/80 kb interval is not covered by the cached splicing span: the existing indexed branch repeats one full-span scan, while the transcript, composition and exon intervals are warm. Calling the entire workload a pure cache-hit benchmark would be inaccurate. The separate microbenchmarks above do measure memo hits.

A close-out instrumentation check counted one request after three warmups, without rerunning the timing benchmark or changing runtime code:

| Workload | Entry validations | Exit validations | Identity computations / file.info calls | Paths supplied to those file.info calls | Nested FaFile lookups |
|---|---:|---:|---:|---:|---:|
| TP53-like | 15 | 15 | 30 | 90 | 1 |
| BRCA1-like | 27 | 27 | 54 | 162 | 1 |

Each identity uses one vectorized `file.info` call for three paths: source, `.fai`, `.gzi`. The path totals are R-level stat targets, not measured kernel syscall counts; normalizePath, file.exists and Rsamtools may perform additional filesystem operations. Counts follow `2 × (N exons + span + splice + composition + nested FaFile lookup)`, or `2 × (N+4)`.

**Identity is redundantly recomputed across exon-level calls and the nested FaFile call.** It is not threaded once across this logical request. Entry/exit checks intentionally bracket individual public consumers, but overlapping work repeats the same canonicalization and three-path stat. On cold paths there can be further nested validations. This is a remaining performance opportunity, not a hidden optimization already applied.

Stat and path normalization dominate the added latency: about 98 µs per full identity versus 3 µs for constructing its list and under 0.5 µs for comparison. The 30/54 full identities account for roughly 2.94/5.29 ms gross; remaining differences include guard overhead, repeated path operations, and ordinary benchmark noise. Baseline composition also performed a stat, so these are component attributions rather than an exact additive subtraction. Raw memo lookup remains about 6 µs.

For an actual CGeV card that performs this same set of sequence operations on this local filesystem, the estimate is **+4.458 ms for TP53-like work and +6.682 ms for BRCA1-like work**, approximately 97% and 92% more time in this synthetic sequence workload. These increases are not percentages of total card-render latency. A warm `fetch_gene_data_sync` using only the existing splice and composition hits has five identity computations (pre-splice provenance capture plus two per consumer); the component timings suggest about **0.68 ms added** for that smaller path. The application can also perform interval/GC requests, and actual request counts depend on the render/download path. No end-to-end real TP53 or BRCA1 card timing was measured. NFS or slower metadata access could increase the impact substantially.

**Performance decision: OPTIMIZATION WARRANTED BEFORE MERGE.** The absolute local increase is modest, but the near-doubling and 30/54 repeated identity computations warrant a separately reviewable reduction of redundant request/internal validation before merging for a shared runtime. Preserve this correctness candidate for independent validation first. No optimization was made during close-out; the recorded 4.616→9.074 ms and 7.292→13.974 ms figures are unchanged.

## Regression and negative controls

- Full testthat suite: **143 tests, 1,219 passing expectations, zero failures, errors, warnings or skips**. Includes existing sequence/GC, native 2bit, runtime-environment and compiled-runtime tests.
- Existing sequence/GC and native 2bit files also passed independently (38 + 9 expectations).
- Seven standalone scripts exited 0 directly; the transcript script initially reported a missing-fixture skip among those seven. The eighth, single-span 2bit, initially failed because repository-local `faToTwoBit` was absent.
- Both fixture-dependent standalone scripts then passed with a generated 4 kb 2bit genome and an explicitly documented temporary `rtracklayer`-based converter. The substitute tests sequence/splice behavior, not the UCSC executable. Temporary converter/genome were removed.
- Other direct standalone passes: 2bit sidecars, sequence prefetch future globals, selected-sequence downloads, sequence-download UI, inline prefetch, coordinated memory budgets.
- Source loader passed: all **736** explicit library bindings resolved.
- Compiled **22** runtime source files; compiled loader passed with `--require-compiled`, all **736** bindings resolved.
- Negative control removes identity invalidation/exit protection in the isolated test environment, leaving candidate extraction code in place: **126 failed expectations, 1 error**, 198 passing expectations, worker skipped. This includes failures of returned bases/counts, not merely keys.
- Baseline-source control: **129 failed expectations, 1 error**, 193 passing expectations, 2 skips (worker and new provenance API). Separate baseline worker evidence is above.

All commands and detailed results are retained alongside this report. `totals.txt` and per-test TSV files give machine-readable totals. Intentional control failures and the initial missing-fixture results remain clearly labeled.

## Limitations and blockers

- No blocking candidate test failures remain. The real production genome fixture and UCSC executable were unavailable; synthetic replacement coverage and the installed native/rtracklayer backends were used. Independent validation should repeat on the intended shared-runtime host and real datasets.
- Size/mtime/ctime cannot distinguish replacements whose observable metadata are all identical. Coarse timestamps, missing ctime, Windows creation-time semantics, network filesystem attribute caching, or unusual tools preserving all metadata limit detection. No content hash or inode check is claimed.
- Stat/read operations are not a transaction. Exit checks discard cache writes when a change is observed, but an in-flight returned value may precede or straddle concurrent replacement. A filesystem change after the final stat cannot be prevented by a cache identity. Coordinated atomic publication is still preferable. No linearizable snapshot guarantee is claimed.
- ctime-only metadata changes conservatively invalidate unchanged content. Old sidecars lacking ctime are rejected and rebuilt. Read-only stale FASTA indexes lead to sequential fallback, potentially slower until a valid index is supplied.
- Tests ran on this local macOS filesystem; they do not prove timestamp or attribute-cache behavior on the deployment filesystem. Performance estimates exclude network storage and application rendering.
- No broad architecture audit or Phase 5D.3D work was performed.

## Recommended next step

Independently validate this exact candidate commit/tree in a separate checkout on the intended persistent-R host. Re-run the focused oracle with `CGV_SEQUENCE_WORKER_TEST=1`, the full suite, and real-dataset replacement/performance checks. Review stale-index fallback and timestamp/TOCTOU limits, then address redundant validation in a separate measured change before merge. Do not push, merge or deploy as part of this phase.

## Close-out inspection

The resumed checkout had staged changes only, no unstaged edits and no implementation commit. The runtime diff remained confined to sequence identity/invalidation and prefetch provenance transport. Both runtime files and the new test file parsed successfully. No working-code edits or expensive validation reruns were made during close-out. The final response records post-commit SHA/tree, diffstat and status.

## Changed files

The complete committed file manifest follows.

```text
R/modules.R
R/utils.R
tests/testthat/test-sequence-cache-invalidation.R
validation/phase-5d3c/REPORT.md
validation/phase-5d3c/baseline-matrix.txt
validation/phase-5d3c/baseline-performance.txt
validation/phase-5d3c/baseline-probe.txt
validation/phase-5d3c/baseline-request-performance.txt
validation/phase-5d3c/baseline-test-results.tsv
validation/phase-5d3c/baseline-tests.txt
validation/phase-5d3c/benchmark.R
validation/phase-5d3c/candidate-matrix.txt
validation/phase-5d3c/candidate-performance.txt
validation/phase-5d3c/candidate-probe.txt
validation/phase-5d3c/candidate-request-performance.txt
validation/phase-5d3c/compile.txt
validation/phase-5d3c/compiled-loader.txt
validation/phase-5d3c/environment.txt
validation/phase-5d3c/existing-sequence-tests.txt
validation/phase-5d3c/focused-final.txt
validation/phase-5d3c/full-results.tsv
validation/phase-5d3c/full-suite.txt
validation/phase-5d3c/matrix.R
validation/phase-5d3c/memory.R
validation/phase-5d3c/memory.txt
validation/phase-5d3c/mutant-results.tsv
validation/phase-5d3c/mutant.txt
validation/phase-5d3c/probe.R
validation/phase-5d3c/regressions.R
validation/phase-5d3c/regressions.txt
validation/phase-5d3c/request-benchmark.R
validation/phase-5d3c/request-validation-counts.R
validation/phase-5d3c/request-validation-counts.txt
validation/phase-5d3c/source-loader.txt
validation/phase-5d3c/standalone-synthetic.R
validation/phase-5d3c/standalone-synthetic.txt
validation/phase-5d3c/test_coordinated_memory_cache_budget.R.log
validation/phase-5d3c/test_inline_fast_sequence_prefetch.R.log
validation/phase-5d3c/test_selected_sequence_download.R.log
validation/phase-5d3c/test_sequence_download_ui_static.R.log
validation/phase-5d3c/test_sequence_prefetch_future_globals.R.log
validation/phase-5d3c/test_transcript_composition_cache.R.log
validation/phase-5d3c/test_twobit_seqnames_sidecar.R.log
validation/phase-5d3c/test_twobit_single_span_splice.R.log
validation/phase-5d3c/totals.txt
validation/phase-5d3c/twobit-memory.R
validation/phase-5d3c/twobit-memory.txt
```
