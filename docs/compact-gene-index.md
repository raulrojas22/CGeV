# Compact annotation lookup indexes

The main Shiny process has a coordinated memory cache budget (128 MiB under
Colors' current three-process configuration). The human and mouse legacy light
indexes total approximately 199 MiB; selecting both evicts the first, so the
first Cross-Species search reloads annotations that were just warmed.

The compact representation stores each alias map as tokens, cumulative range
ends and integer gene rows. Compact-normalized aliases reuse the normal map's
range only when the complete row vector is identical. A signed position vector
preserves original alias ordering and distinguishes collisions and compact-only
aliases. All annotation rows, metadata and gene-row order remain unchanged.
`gene_lookup_tokens()` and `gene_lookup_hits()` accept both representations.

Caches use the separate `gene_light_compact_v1` kind. The normal deployment
precompute step writes them before sessions start. Runtime accepts legacy caches
and converts them in memory if the new file is missing; it does not rewrite
legacy files during searches. Compact files use the annotation's canonical
path, size, modification time and existing index version in their filename.
They are not selected by the legacy-family fallback. Older application images
continue using their legacy cache files during rollback.

The change retains the coordinated memory limits and LRU eviction. Larger
organism selections can still exceed the budget and reload indexes. This is a
storage and lookup change; the card rendering and readiness rules are unchanged.
