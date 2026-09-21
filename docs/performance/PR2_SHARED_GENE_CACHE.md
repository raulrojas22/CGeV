# PR2: shared deterministic gene cache

Base: `origin/master` = `878c7a42c11cc2fe970f269d647ebd4e65530371`, merge of PR #38.
Branch: `codex/cgev-shared-gene-cache`. Isolated worktree:
`/Users/rarojas/Documents/A_FULLAPP/.codex_work/cgev-shared-gene-cache`.

## Scientific boundary (inspected before implementation)

- `split_gene_data_by_transcript(data)` traverses ID/Parent relationships, orders
  transcripts and returns named data frames with `cgev_transcript_meta` attributes.
  `compute_canonical_block_idx(blocks)` consumes those frames/attributes only.
  Persist exactly `list(blocks, canonical)` before canonical-first reordering.
  No alternate implementation of either scientific calculation was introduced.
- `build_transcript_metrics_payloads(rbind(blocks), blocks, ...)` consumes ordered
  blocks, representative/organism labels and chromosome-label helpers. Persist its
  existing list payload only. No duplicate copy of the input blocks in this entry.
  `format_org_name` captures the popup domain but its body only formats its argument
  with htmlEscape; it reads neither session nor reactive state.
- Chromosome labels currently are internal intermediate values, absent from the
  returned metrics payload. The key nevertheless includes effective labels and
  resolved assembly-report file identity, accounting for the existing process map
  caches and registry-based report resolution. No change to those caches/helpers.
- Timers are observational. IDs belonging to plots/sessions, session maps, closures,
  reactives, promises, external pointers and widgets are not stored. The whitelist
  accepts only unclassed atomic/list values and plain data frames, recursively
  checking attributes as well. Unsupported inputs bypass persistence; unsupported
  output is returned without storage.

## Key and invalidation

SHA-256 over R serialization v2 of kind, explicit schema `1L`, algorithm
`split-canonical-metrics-1`, normalized annotation path/size/numeric mtime, **all
actual gene input content** (or ordered blocks including metadata for metrics),
explicit parameters, R version, locale, formatting options and relevant package
versions. Gene stable IDs, coordinates, annotation attributes, transcript order
and any supplied build identifiers are represented by the complete input rows.
Organism labels are explicit. Metrics adds representative name, report-map flag,
report path/identity and effective chromosome labels.

The content fingerprint covers the annotation subset actually consumed, not the
whole annotation file. It therefore detects changed scientific inputs even when
size/mtime are preserved, without hashing a giant file per search. An unrelated
file change with identical consumed inputs cannot change these pure results.
It does not repair stale upstream annotation lookup caches. No genome/sequence
input participates in either cached calculation. Bump algorithm when any consumed
scientific or formatting helper changes; bump schema for storage format changes.
Missing digest/dependencies or unrepresentable keys bypass the cache.

## Storage and failures

`get_cgv_cache_root()/shared_gene/{split,metrics}/<sha256>.rds`, honoring existing
`CGV_CACHE_DIR` / `APP_CACHE_ROOT`. Tests override this with a temporary directory.
No cache artifacts committed, no hardcoded host/production path. DISK_SHARED
requires the existing root to be mounted/shared and writable by participating app
processes; a container-private root cannot provide cross-container hits.

Fast read → atomic mkdir per-key lock → re-check → original compute → saveRDS to
unique same-directory staging file → rename → release in on.exit. Readers validate
schema, key, plain types and payload checksum. No non-atomic copy fallback.
Cache read/write/rename failure returns/recomputes the original result; scientific
compute errors propagate. Canonical's existing error fallback to index 1 remains,
but a fallback caused by an error is never persisted.

Lock wait is bounded to 15 seconds. Timeout computes without publishing. Locks are
never stolen based on age, because a paused process on another host might still
own one. An orphan lock requires operator cleanup when known inactive; until then
requests fail open after the bounded wait. Slow overlapping requests may duplicate
compute after timeout. Filesystems must provide atomic mkdir and same-directory
rename (validated locally, not on production/NFS). No durability/fsync guarantee
is required for this disposable cache.

Seven-day read TTL and 128 completed entries per kind, following STRING's small
file-count policy; prune oldest after successful writes. No general manager or
hard total-byte quota. Staging files left by a killed process/orphan lock directories
are not pruned automatically. Real large-gene disk/RAM growth is still unmeasured.
Existing L1/session lookup paths and orthologous prepass reuse remain in place.
APP_PERF_TIMING adds shared cache state/lookup/compute markers. Existing scientific
phase timers occur on MISS; HIT skips those calculations.

## Directed validation

- `Rscript --vanilla scripts/test_shared_gene_cache.R`: exact `identical()` parity
  against unchanged original functions for MISS/HIT and serialization; positive/
  negative strands, 2/60 synthetic isoforms, GTF/no-CDS, no-transcript, shared-parent
  and reversed blocks; session ID rebinding outside storage; key isolation,
  schema/algorithm/content invalidation, corruption/checksum, TTL, lock timeout,
  rename failure, canonical error and cleanup. Small forked two-process test:
  identical values and one compute. Cross-process key equality checked.
- `Rscript --vanilla -e 'testthat::test_file("tests/testthat/test-orthologous-transcript-split-reuse.R", stop_on_failure=TRUE)'`:
  26 checks pass. Static call-site expectation updated for the wrapper.
- Parse changed R sources and `git diff --check`.

Measured serialized sizes (bytes; final fixtures include a MANE Select tag):

| Synthetic fixture | Split raw / gzip | Metrics raw / gzip |
| --- | ---: | ---: |
| 2 isoforms, 7 rows, + | 2,832 / 587 | 4,975 / 564 |
| 2 isoforms, 7 rows, − | 2,832 / 586 | 4,975 / 564 |
| 60 isoforms, 181 rows, + | 79,234 / 4,947 | 148,466 / 1,841 |
| 60 isoforms, 181 rows, − | 79,234 / 4,942 | 148,466 / 1,841 |

Sizes are the persisted scientific products (envelope adds small metadata).
No real BRCA1 size claimed. No full Shiny/browser session, cross-container filesystem,
BRCA1 R1/R2, CPU/RAM, large load or long profiling validation was performed.

**NOT RUN BY CODEX — independent benchmark pending.**
**NO DEPLOY / NO production changes. No merge.**
