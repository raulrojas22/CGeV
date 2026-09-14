# Colors release source and validation

`master` is the canonical source branch. Prepare changes in a clean
`codex/` worktree based on `master`, integrate through a pull request, and tag
only a tested release commit. Deploy from that exact clean checkout using
`bash deploy/deploy-colors-shinyproxy.sh`; never deploy the developer's dirty
working directory or move a release tag to a different commit.

## September 2026 integration

The previously deployed source was `895fa7f128837857ee563b950758c30025c0fde7`.
Its branch predates a public-history rewrite and the source-layout cleanup on
`master`. The integration replays the twelve optimization commits on
`a506e7edc75c25d1e76d29443f42427c72a9dd9e`, rather than merging unrelated
history and restoring removed private manuscript materials. Desktop dependency
updates, public repository cleanup and the `deploy/` layout are retained.

| Previously deployed commit | Integrated commit |
| --- | --- |
| c9a5aa2 | 59cb292 |
| e2a0699 | fd5c16d |
| 02ddfbd | bf0cbd7 |
| bfee96c | ca50e5c |
| 16a7e1f | 86b163e |
| cf84b79 | 97ca1ae |
| d6a61ee | d91b94f |
| 009ad92 | 9543f7e |
| 745381e | 1faa71c |
| defa2ff | af12e1e |
| eba0a6a | a13f582 |
| 895fa7f | ba83edd |

The compilation source list now includes `R/gene_search_lib.R`. Shiny's
real-loader test requires its compiled artifact, checks the server bytecode,
and verifies all explicit library bindings and their app-global dependencies.
CI runs both source and compiled modes in separate R processes, alongside the
R regression suite. Deployment repeats both real-loader checks inside the
exact candidate image before switching the public release.

## Server-owned configuration

Colors continues to mount `app/shinyproxy/application.yml` even though the
repository template is now `deploy/shinyproxy/application.yml`. Synchronization
must preserve the server directory; only the validated candidate replaces its
active file. Environment files, including `.env.background-reports`, must be
excluded from every image build context. They are supplied only to their
intended runtime process.

ShinyProxy's `container-cmd` overrides the image CMD. The candidate builder
migrates the known legacy `/app/docker/run-app.sh` command to
`["bash", "/app/deploy/docker/run-app.sh"]` and rejects unknown commands.
Before cutover, `verify_colors_image_startup.py` starts the exact candidate
with that command, UID 10001, no network, an ephemeral cache, and resource
limits, including the selected `COLORS_CONTAINER_MEMORY` cap. It must serve
`healthz.txt`; the temporary container is then removed.
Deployment and `--check` also verify the running delegate's command.
This covers configuration drift that a source/bytecode loader test cannot.

Guide videos in `www/screencasts/` are separately provisioned binary assets,
excluded from Git. Colors synchronization preserves that server directory even
when deploying from a clean worktree. `deploy/guide-videos.sha256` records their
approved bytes; the inventory must exactly match `guide_media_files` in `ui.R`.
The deploy checks every video before building, and again as UID 10001 inside
the candidate image before cutover. CI checks the inventory and rejection cases.

For a new server or missing assets, recover `www/screencasts/` from the last
verified release image or its immutable static snapshot, then run
`python3 scripts/verify_guide_assets.py` in the server app directory. Do not
substitute source recordings or unrelated files. A deliberate video update
requires updating the manifest and provisioning the matching bytes on Colors.

`COLORS_CONTAINER_MEMORY` selects the deployment's explicit app-container cap
(default `5g`). The candidate builder migrates the ignored `container-memory`
key to `container-memory-limit` only inside the CGeV spec, rejects ambiguous
keys, and leaves other applications untouched. Deployment and `--check`
validate the actual Podman `HostConfig.Memory`, not merely the YAML text.
Pass the same override to `--check` when using a non-default cap.

Size this cap from cgroup `memory.peak` during complete workflows, including
future workers, long genes, repeat searches and isoforms. The coordinated R
cache budget (384 MiB by default, divided among processes) is a separate
mechanism and does not bound total container RAM. A per-container cap also
does not guarantee that all allowed simultaneous sessions fit in host RAM;
measure concurrency separately before increasing pool or session capacity.

## Release evidence and rollback

Record the Git commit, annotated tag, immutable image ID/tag, active memory
limit, test results and public functional checks for every deployment. Preserve
the immediate rollback directory emitted by the deploy script. Its existing
rollback restores the previous image and the exact server-owned YAML/env files.
Do not regenerate or replace biological annotations to roll back application
code. Compact indexes are derived caches; see `../compact-gene-index.md`.

Public p95 requires enough independent sessions and must distinguish ready-pool
startup, container creation, first search and repetition. Functional checks and
a few memory stress cases are not a public p95 or a concurrency guarantee.
