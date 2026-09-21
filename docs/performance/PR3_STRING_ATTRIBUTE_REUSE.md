# PR3: lazy STRING query and lightweight cross-plot screen projection

Base: `3a27a5e77409faf7dc4abfe1de5b21d3f3e05c1d`.
Branch: `codex/cgev-pr3-gff-attribute-reuse`.

## Functional dependency and exact field requirements

Cross-plot matching is intentional. Network X uses identifiers from visible cards
Y/Z to mark related STRING nodes as `plotted` instead of `neighbor`. The matching,
role assignment and visual rendering code is unchanged. Query candidates belong
to X; screen candidates belong to every active, nonempty plot in both contexts,
with the original final TaxID filter, ordering and uniqueness rules preserved.

| Field/source in Y | Effect on screen_variants | Original vs revised extraction cost |
| --- | --- | --- |
| Card title, `Gene` field | Prepended as a candidate, except empty/NA/`Gene` | Same title extraction on each payload |
| `gene`, `gene_id`, `locus_tag`, `Name`, `protein_id`, `ID` in every V9 row | First value of each case-insensitive key; trimmed original and variant stripping `gene/rna/mrna/cds/transcript` plus `:` or `-` prefix | Full scalar parse/decode per row → selected fields in one batch of unique rows |
| All `Dbxref` and `db_xref` values | Split decoded commas; add full entry then suffix after first colon | Same order and token logic; selected values decoded in the batch |
| `Parent`, raw `gene_name`, `Alias`, `gene_synonym(s)`, `synonym`, `transcript_id`, other fields | No direct contribution to screen extraction; a transcript identifier contributes if stored under `ID`. Title may independently reflect a gene name | Not decoded in the guarded fast path; fallback still uses the original parser |
| Organism TaxID/name and annotation path | Same numeric TaxID resolution and final same-species filter | Unchanged resolver; no new disk reads |
| Active IDs, context, V3/nrow, raw V9 order | Original plot eligibility, ordering, row contributions and malformed-row behavior | Existing plot state; no new row selection |

The selected plot's full query additionally uses synonyms, primary gene-name/ID
precedence (including Parent fallback), sanitization and the original 64-candidate
cap. `string_prepare_gff_identifiers()` is unchanged in this revision.

## Upstream inspection

* `build_gff_gene_index()` / `build_gene_lookup_maps()` in R/utils.R retain maps
  for gene/CDS lookup rows. Their normalization and alias extraction differ from
  STRING; they do not retain every transcript/exon row's ordered screen tokens.
* `split_gene_data_by_transcript()` extracts ID/transcript_id and Parent to build
  relationships. Those temporary maps normalize prefixes/quotes and omit other
  screen fields; they are not an equivalent screen projection.
* `plotGeneMeta` carries matched/query/display gene names, matched ID, lookup/local
  aliases, region and canonical flags. Those gene-level metadata cannot reproduce
  all per-row IDs, proteins and DB cross-references in the original order.
* STRING resolution caches map submitted candidates to STRING IDs. They do not
  identify the currently active cards' complete candidate sets.

The exact source remains each existing plot's V9 vector. No lookup, split,
canonical state, metadata, PR2 cache or worker code is changed.

## Architecture and lifetime

There is no annotation work on search/card establishment or restore. The existing
session-owned state contains two projections, created only on a Network request:

1. **Full query:** only for the selected plot, using the unchanged materializer.
   It parses N rows plus the two existing primary-gene helper calls and retains
   the query result for reuse. Its screen IDs are reused directly when valid.
2. **Lightweight screen:** for other required active plots. The getter gathers
   only missing/changed subsets in each context, pools identical raw attribute
   strings, and processes each unique row once. It maps the resulting row IDs
   back to each plot in its original order, then discards the shared row batch.
   Retained data is only the compact plot screen IDs and source identity.

The screen fast path tokenizes ordinary ASCII GFF3 key=value rows, selects the
six scalar keys and two cross-reference keys, and calls the existing
`safe_url_decode()` once on the selected value vector. Scalar ASCII keys need no
URL decoding. All key/value and row order rules are retained. This is not merely
memoization of the original full parser: unneeded fields are not decoded, key
handling is batched, and unrelated full query projections are never built.

The fast path is deliberately conservative. GTF, encoded keys, extra equals,
non-ASCII text, malformed tokens, NUL/high-byte URL escapes use the unchanged
scalar parser per unique row. These rows retain original fallback behavior.
Per-row screen extraction records failure and partial IDs; mapping stops that
plot at its first failing row, exactly like the original surrounding tryCatch.

Full and screen results use exact source equality (plot data, organism and
annotation path), with separate context/plot keys. Source changes invalidate on
the next request; no observer needs to run first. Stale full results are released
when screen collection encounters a changed source, and a new full result releases
its obsolete lightweight entry. Title/theme/active order/TaxID resolution are
assembled live. The lifecycle observer only prunes removed/empty plots in both
stores. Session end releases the state; restoring plots does not prepare them.
No disk cache, genome-wide parsed state, workers or async scheduling are added.

## Corrected harness and validation

V1 omitted `R/string_worker.R` from its harness. Consequently the caught missing
screen collector returned empty screen variants, so its screen parity claim was
incomplete. The corrected harness permanently loads that file and asserts that
screen variants are nonempty. Full original-vs-candidate parity still passes with
that correction, including the actual cross-plot values.

All of these pass locally on R 4.4.3:

```sh
Rscript scripts/test_string_annotation_reuse.R
Rscript scripts/test_string_network_roles.R
Rscript scripts/test_string_http_errors.R
Rscript scripts/test_string_future_globals.R
Rscript scripts/test_perf_flag_independence.R
```

The future test needs permission for a local loopback socket. HTTP is mocked;
there are no real STRING requests or BRCA1 benchmarks. R syntax and diff checks
also pass.

Coverage includes complete payload identity against the frozen base function,
GFF/GTF, ordering, duplicate keys/rows, identity/Parent precedence, malformed and
missing attributes, TaxID fallbacks/species, title/theme changes and candidate
cap. Screen extraction is independently compared to the original nested function
for every byte URL escape, encoded keys, malformed tokens, raw invalid encoding
and 100 deterministic mixed-row sequences. Shiny tests cover no eager work,
source changes, opening before observer flush, pruning and restore.

An alias exclusive to Y changes X's screen variants without changing X's query
candidates. A cached alias mapping to TP53 makes that node `plotted` in X's graph;
removing Y or changing Y's TaxID makes it `neighbor`. Original and candidate graph
payloads/roles are identical, ignoring only the role timestamp. Matching and
color code are untouched.

### Synthetic work counts (not latency benchmarks)

368 fixture plots each contain three rows, including two shared rows:

| Operation | Full projections | Scalar parser calls | Decoder calls |
| --- | ---: | ---: | ---: |
| Establish/restore plots without Network | 0 | 0 | 0 |
| Original first Network X | N/A | 1109 | 7401 |
| Revised first Network X | 1 | 5 | 42 |
| Repeated unchanged Network X | 0 | 0 | 0 |
| First Network Y after X | 1 | 5 | 41 |
| Change only Y's alias, then Network X | 0 | 0 | 1 |

The first X screen batch processes 369 unique rows for 367 other plots rather
than 1101 row copies. Y's request reuses the other plots' screen state and adds only
its full projection. Changed-Y screen extraction processes three rows while X's
full query remains cached. The 368-card count is a synthetic scheduling fixture,
not real BRCA1 transcript data.

## Timing and independent A/B risks

Existing STRING timings remain. `payload_prepare_ms` measures total preparation;
`annotation_prepare_ms`, `annotation_materialized` and `annotation_reuse` refer to
full query projections. `screen_prepare_ms` and aggregate `screen_materialized`,
`screen_reused`, `screen_unique_rows`, `screen_fallback_rows` describe lightweight
screen work. No per-row logging; timing work is skipped when disabled.

Expected mechanism: no full preparation on establishment; only one full query
projection on first open; no scalar parsing on the ordinary GFF3 screen fast path;
one batch decode for shared screen rows; no repeated extraction for unchanged
plots. Cross-plot scanning of raw inputs is still necessary on a cold request.

Independent A/B must measure actual first-card/plot latency, first and repeated
Network latency, exact snapshots/roles and peak/retained RSS for BRCA1/TP53. This
implementation does not establish a real speedup or absence of a RAM regression.
Fallback-heavy GTF/non-ASCII/malformed annotations may still incur substantial
scalar parsing. Large numbers of unique screen IDs retain proportional memory;
the transient token/row batch also has a memory cost. Pooling is per context and
request, not a persistent genome index. These limits are explicit review targets.
