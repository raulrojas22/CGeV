# CGeV performance and capacity — final technical report

This report closes the current CGeV performance-optimization and
controlled-concurrency characterization cycle. It is documentation only: no
application code, production configuration, deployment, cache, or ShinyProxy
setting was changed to produce it.

- Repository: `raulrojas22/CGeV`
- Validated production revision: `368872bae9b8833ffef02cdca3573bd202438485`
- Validated production image: `localhost/cgv:release-368872bae9b8-20260921T154656Z`
- Production endpoint: `https://cgev.mobilomics.org`
- Host: `colors` (production host; connection coordinates intentionally omitted)

**Strongest demonstrated statement:**

> **CGeV CONCURRENCY CHARACTERIZATION: PASS THROUGH 6 HEAVY**

This document distinguishes four kinds of statement throughout:

- **Measured** — a value read from a result file, log, or live production
  inspection.
- **Observed** — an architectural or operational behavior seen directly during
  testing.
- **Interpreted** — a conclusion supported by the measurements above.
- **Unknown** — not measured; explicitly flagged as future work.

Every numeric claim below is traceable to an artifact listed in section 13.

---

## 1. Scope and executive summary

The cycle closed by this report had this progression:

1. baseline / root-cause investigation of the fresh search and STRING Network
   path;
2. **FutureGlobals** — removal of large lazy-future closure/global
   serialization (PR #38);
3. **shared gene cache** — cross-session deterministic `split`/`metrics` disk
   cache (PR #39);
4. **STRING/GFF attribute reuse** — removal of pathological repeated GFF
   attribute parsing/decoding (PR #40);
5. independent A/B validation of each phase on an isolated container;
6. controlled production deployment of revision `368872bae9b8`;
7. controlled 1 / 2 / 4 / 6 heavy-user concurrency characterization on
   production.

The validated revision and image above are the ones measured end-to-end. No
claim is made for any other revision.

This report does **not** assert an arbitrary production user capacity such as
"supports 30 users." The demonstrated statement is bounded and deliberately
narrow:

> **PASS THROUGH 6 HEAVY**

"6 heavy" means **six independent user sessions, each executing the
synchronized adversarial BRCA1 workload including the STRING Network
analysis**, at the same time, on the production system. They were not six
idle, browsing, or lightly-loaded users. Section 5 gives the exact
per-stage latency, and section 8 explains why this is an adversarial upper
bound rather than a normal working load.

---

## 2. Production architecture under test

**Host (measured):** `colors`, 8 vCPU, 16 GiB RAM, ~15 GiB swap.

**Application stack (observed in production):**

- ShinyProxy `3.2.4` (pinned by digest in `deploy/docker-compose.shinyproxy.yml`);
- nginx reverse proxy / cache server;
- `cgv-background-report-worker` (long-lived report worker);
- delegate `sp-container-*` application containers managed by ShinyProxy;
- `cgv-docker-socket-proxy`;
- shared persistent cache mounted from the host.

**Runtime configuration (measured in production `.env` and container
inspection):**

| Setting | Value |
|---|---|
| per-app-container cgroup memory limit | `SP_CONTAINER_MEMORY=5g` (5,368,709,120 B = exactly 5 GiB) |
| ShinyProxy total instance ceiling | `SP_MAX_TOTAL_INSTANCES=6` |
| per-user instance ceiling | `SP_MAX_INSTANCES_PER_USER=2` |
| minimum standby seats | `SP_MINIMUM_SEATS_AVAILABLE=1` |
| container re-use | `SP_ALLOW_CONTAINER_RE_USE=false` |
| future workers per app container | `APP_FUTURE_WORKERS=2` |
| future plan | `multisession` |
| shared cache | container `/app/cache`, host-backed, read/write |

`APP_FUTURE_WORKERS=2` is confirmed both in the production environment and by
direct observation: each active app container ran two parallel future workers
(`parallel:::.slaveRSOCK`), so six active containers produced **12 future
workers** sharing the same 8 physical cores.

### ShinyProxy topology — corrected interpretation

An earlier interpretation suggested that multiple heavy user sessions might be
packed into a single app container. Final controlled testing **contradicts
that**:

> **Observed:** in every stage, each active delegate proxy container carried
> exactly one active user seat. No two users were ever co-located in one
> container. The "sharing" is a shared pool of single-seat containers, not
> in-container session packing.

`minimum-seats-available=1` keeps one **standby** container alive at all
times. As a result, raw container count is always one higher than the active
user count:

| Stage | Active heavy users | Standby | Total containers |
|---|---:|---:|---:|
| 3 | 4 | 1 | 5 |
| 4 | 6 | 1 | 7 |

Consequently, **container count alone does not equal active-user count.**
Container/seat cleanup and standing capacity must be read from ShinyProxy's
seat claim/release events, not from `podman ps`.

---

## 3. Optimization history

### Phase 1 — FutureGlobals (PR #38, merged as `878c7a4`)

**Root cause (measured):** `future_promise()` calls in the STRING, sequence
prefetch, and Literature paths were passing closures bound to the large
`lib_env` application environment. Future's automatic global discovery
serialized that closure environment, including caches and objects the worker
never used, synchronously in the main R process before the worker could
start.

**Corrective design (from `docs/performance/FUTURE_GLOBALS_REVIEW.md`):**
explicit, minimal worker globals **plus** isolation from the `lib_env`
closure. Each worker entry point (`string_future_worker`,
`sequence_prefetch_future_worker`, `search_papers_epmc`) is now bound to
`baseenv()`, receives only an explicit argument list, and loads the R sources
it needs inside the worker. This is not merely "declaring globals" — the
isolation of the function environments from the application closure is a
required half of the fix. A targeted test injects a foreign 2 MiB object into
the source environment and confirms it is not carried to the worker.

**Measured evidence (production and isolated benchmark):**

| Quantity | Before | After |
|---|---|---|
| `worker_launch_ms` (STRING) | ~66.6 s | ~35 ms |
| `card_module_init_ms` (sequence prefetch) | ~66–72 s | ~61–74 ms |

The prompt's `worker_launch_ms ≈ 66,641 ms` corresponds to the measured
"~66.6 s"; the prompt's `card_module_init_ms ≈ 61–151 ms` is broader than the
measured after-range of **~61–74 ms** reported by the validating session, and
this report uses the narrower measured range. See section 13 (Discrepancies).

*Evidence gap:* the specific worker-RSS before/after figures quoted in the
task prompt (~1.29 GB → ~119–140 MB) could **not** be recovered from any
repository or session artifact. A later independent measurement (PR #40)
reports worker RSS **~188 MB** and `worker_launch_ms ≈ 17–37 ms`; that is
documented below but is a different revision. The RSS before/after pair is
therefore marked **unknown** here rather than restated.

### Phase 2 — shared gene cache (PR #39, merged as `3a27a5e`)

**Design (from `docs/performance/PR2_SHARED_GENE_CACHE.md`):** a shared,
on-disk cache for the two deterministic, genome/sequence-independent
scientific products of a gene search:

- the `split` product (`list(blocks, canonical)` persisted before
  canonical-first reordering);
- the `metrics` payload from `build_transcript_metrics_payloads()`.

Key and integrity properties:

- **content-aware SHA-256 key** over R serialization v2 of kind, explicit
  schema, algorithm tag, normalized annotation path/size/mtime, the **complete
  consumed gene input content**, explicit parameters, R version, locale,
  formatting options, and relevant package versions;
- **split/canonical payload** and **metrics payload** stored separately as
  `.rds`;
- **atomic writes** via a unique same-directory staging file then `rename`;
- **per-key lock** using atomic `mkdir`, with a bounded 15 s wait;
- **corruption / failure handling:** readers validate schema, key, plain
  types, and payload checksum; read/write/rename failure recomputes and
  returns the original result; scientific compute errors propagate and are
  never persisted;
- **TTL / bounds:** 7-day read TTL and 128 completed entries per kind, pruned
  oldest-after-write;
- **scientific errors preserved:** canonical's existing fallback to index 1
  remains, but a fallback caused by an error is never cached.

**Measured benchmark (BRCA1, isolated container, `APP_PERF_TIMING=1`):** MISS
from `brca1_perf.log` / `pr2_perf.log`; HIT from `pr2_perf.log`:

| Marker | Cold / MISS | HIT |
|---|---|---|
| `split_transcripts_ms` | ~2.9–3.8 s | **0.361 s** |
| `metrics_payload_build_ms` | ~1.9–2.7 s | **0.701 s** |
| `plot_state_prepare_total_ms` | ~6.65–8.42 s | **2.886 s** |
| `search_finish_ms` | ~8.75–11.14 s | **4.981 s** |
| `client_click_to_card_complete_ms` | ~17.4–22.8 s | **13.664 s** |

The prompt's expected pair (split ~2.9 s → ~0.36 s; metrics ~1.9 s → ~0.70 s;
plot_state_prepare ~6.65 → ~2.89 s; search_finish ~8.75 → ~4.98 s; card ~17.5
→ ~13.8 s) is consistent with these exact values.

**Scientific parity and cache behavior (measured):**
`scripts/test_shared_gene_cache.R` verifies `identical()` parity against the
unchanged original functions for MISS and HIT, both strands, 2/60 synthetic
isoforms, GTF/no-CDS, no-transcript, shared-parent and reversed blocks; a
small forked two-process test confirms identical values with a single compute
and cross-process key equality. Exact-byte parity is asserted between
replicas by panel and phase.

### Phase 3 — STRING GFF attribute reuse (PR #40, merged as `368872b`)

**Root-cause investigation (measured by Rprof):** BRCA1 Network latency was
not primarily STRING HTTP latency (~0.40 s) nor FutureGlobals (worker launch
~40 ms, worker work ~2.4 s). Profiling `build_string_query_payload` identified
repeated GFF attribute parsing and URL decoding:

| Marker | BRCA1 | TP53 |
|---|---:|---:|
| gene GFF rows | 15,964 | — |
| mRNA | 368 | 124 |
| exons | 8,146 | 1,203 |
| `parse_gff_attributes()` calls | **17,135** | 618 |
| `safe_url_decode()` calls | **128,970** | — |
| unique parser inputs | 8,888 | — |
| exact duplicate inputs | 8,247 | — |

Main R was ~100% CPU for ~90 s while the STRING API itself answered in ~0.40 s.

**Rejected first implementation (engineering evidence):** an eager design that
materialized the full projection for all ~368 active plots on establishment.
This moved the cost earlier and produced **~95 s card latency**. It was
rejected and is not present in the final implementation. This is recorded
because it is a load-bearing part of why the final design is lazy.

**Final design (from `docs/performance/PR3_STRING_ATTRIBUTE_REUSE.md`):**

- lazy **full projection** for the selected gene only, on Network request;
- a lightweight **cross-plot screen projection** for other active plots;
- **pooled identical raw attribute strings** processed once per unique row;
- a **fast path** for ordinary ASCII GFF3 `key=value` attributes that selects
  the six scalar keys and two cross-reference keys and decodes only the
  selected value vector;
- **conservative fallback** to the unchanged scalar parser for GTF, encoded
  keys, extra equals, non-ASCII, malformed tokens, and NUL/high-byte escapes;
- **source invalidation** on annotation/plot changes, with separate
  context/plot keys;
- **no** genome-wide persistent parsed state, **no** disk cache, **no**
  workers/async scheduling added;
- **no** change to matching or role semantics (target / plotted / neighbor).

**Independent validation (measured):**

- **40,000 adversarial differential fuzz cases, 0 mismatches**;
- complete payload identity against the frozen pre-PR3 function
  (`tests/fixtures/string_query_payload_pre_pr3.R`);
- STRING role behavior preserved, including the `plotted → neighbor`
  transition when a Y-exclusive alias is removed or its TaxID changes;
- FutureGlobals regression checks remain green.

**Independent A/B (BRCA1, isolated, control vs candidate):**

| Metric | Control | Candidate |
|---|---:|---:|
| initial card | 23.39 / 15.19 s | 23.33 / 15.85 s |
| first Network canvas | 164.3 / 90.5 s | **108.7 / 35.5 s** |
| repeated Network canvas | 74.9 / 73.9 s | **11.2 / 11.2 s** |

Candidate cold `payload_prepare_ms ≈ 12.4 s`; warm `payload_prepare_ms ≈
0.69–0.73 s`. Only one full projection is materialized for the requested plot,
and no eager STRING work occurs before Network. BRCA1 main RSS: control ~850–900
MB, candidate ~792 MB; worker RSS essentially unchanged (~188 MB);
`worker_launch_ms ≈ 17–37 ms`.

**Known limitation (not resolved):** the first cold BRCA1 Network still
performs **~10.6 s** of synchronous lightweight screen preparation across 367
other plots / 8,858 unique rows. This is materially better than the original
path but is explicitly **not** claimed as resolved.

**Production deployment revision for this phase:**
`368872bae9b8833ffef02cdca3573bd202438485`.

---

## 4. Controlled concurrency methodology

**Workload driven per worker (measured by `cgev_worker.R`):**

1. fresh, independent headless-Chrome session;
2. select `Homo sapiens`;
3. BRCA1 search → wait for the card;
4. open STRING **Network** → wait for modal → wait for **Network canvas**;
5. close Network → open **Literature**;
6. brief hold, then release.

**Session establishment and barrier (observed):**

- session establishment was **staggered ~10 s per worker** (`--stagger`),
  because simultaneous cold-start of multiple headless Chrome processes caused
  a client/test-harness race where some browsers remained on the ShinyProxy
  `app_i` loading page. This is a **test-harness artifact and is not a CGeV
  capacity failure**;
- once a session fully loaded and organism selection completed, the worker
  wrote a **READY** marker and waited at a **common barrier**;
- the barrier was released for all workers only after all were READY, so the
  actual heavy BRCA1 phase genuinely overlapped;
- **absolute epoch timestamps** (`t_nav`, `t_ready`, `t_start`, `t_end`) prove
  overlap: barrier-release spread was **0.057 s** (4 users) and **0.079 s**
  (6 users);
- **distinct session tokens** confirmed independent sessions (4/4 and 6/6).

**Overlap windows (measured):** Stage 3 = **112.70 s**; Stage 4 = **165.33 s**.

**Safety and discipline (observed):**

- a production preflight computed usable capacity from ShinyProxy's net seat
  claims, so a standby container was never mistaken for a user;
- a failed session could no longer release the shared barrier; `run_stage.sh`
  aborts without releasing the barrier if any worker fails to become READY;
- no application source, deployment, configuration, cache, or ShinyProxy
  change was made;
- invalid Stage 3 attempts — workers stuck on the `app_i` loading page with
  `JSERR:Uncaught` — were **explicitly discarded** and are excluded from all
  results.

The harness results and derived summaries that substantiate the tables below are
committed under `docs/performance/evidence/concurrency-20260921/` (section 13);
the original harness scripts and raw sampler output were author-local and are
cited there as provenance only.

---

## 5. Final concurrency results

Median per-user latency, in seconds, from the validated result files:

| Heavy users | Card | Modal | Canvas | Literature | Total |
|---:|---:|---:|---:|---:|---:|
| 1 | 16.13 | 13.09 | 51.82 | 3.03 | 71.98 |
| 2 | 14.84 | 14.08 | 60.67 | 2.54 | 79.05 |
| 4 | 19.64 | 24.16 | 93.37 | 3.79 | 119.12 |
| 6 | 28.70 | 36.14 | 133.62 | 4.62 | 168.86 |

Degradation relative to Stage 1 (exact computed percentages; prompt values are
rounded):

| Heavy users | Card | Modal | Canvas | Literature | Total |
|---:|---:|---:|---:|---:|---:|
| 2 | −8.0% | +7.6% | +17.1% | −16.2% | +9.8% |
| 4 | +21.8% | +84.6% | +80.2% | +25.1% | +65.5% |
| 6 | +77.9% | +176.1% | +157.9% | +52.5% | +134.6% |

Aggregate throughput must be computed from the synchronized-batch **makespan**,
not from a per-user median: the median discards the slow tail, so
`users / median(total)` overstates batch throughput. Makespan is the interval
from the shared barrier release to the final worker completion.

| Stage | Users | Batch makespan (s) | Throughput (users/s) | Relative to Stage 1 | Makespan source |
|---:|---:|---:|---:|---:|---|
| 1 | 1 | 71.980 | 0.01389 | 1.00× | single worker `total_work_s` |
| 2 | 2 | 82.580 | 0.02422 | 1.74× | `max(total_work_s)` from the shared barrier |
| 3 | 4 | 129.310 | 0.03093 | 2.23× | epoch: `max(t_end) − min(t_start)` |
| 4 | 6 | 182.067 | 0.03295 | 2.37× | epoch: `max(t_end) − min(t_start)` |

**Makespan evidence note:** Stages 3 and 4 recorded absolute epoch timestamps
(`t_start_epoch` at barrier release, `t_end_epoch` at completion), so their
makespans are exact. Stage 1 is a single worker, whose `total_work_s` equals the
batch makespan. Stage 2 did **not** record absolute epoch timestamps; its
makespan is `max(total_work_s)`, which is exact only because both workers were
released from the **same shared barrier file**, so each `total_work_s` is
measured from that common release. This is stated explicitly rather than
reconstructed: an independent absolute-clock makespan for Stage 2 is not
recoverable from the evidence. The recomputation is reproducible via
`docs/performance/evidence/concurrency-20260921/analysis/makespan_throughput.py`.

The earlier median-derived indicator (`users / median(total)`: 0.0139 / 0.0253 /
0.0336 / 0.0355 users/s) is **not** aggregate throughput and is retained only as
a rough median-derived index; it is superseded by the makespan figures above.

**Interpreted:** with the correct makespan definition, throughput is still
monotonic but strongly sub-linear (1.00× → 1.74× → 2.23× → 2.37×). Doubling
from 2 to 4 users buys far less than the first doubling, and 4 to 6 adds little
aggregate throughput while inflating per-user latency by ~135%. The dominant
term is the Network canvas (section 6).

---

## 6. Resource scaling

Measured by the host sampler (`podman stats` + `/proc`, once per interval)
during each stage:

| Quantity | Stage 1 | Stage 2 | Stage 3 | Stage 4 |
|---|---:|---:|---:|---:|
| peak `load1` | 1.17 | 1.98 | **4.72** | **6.37** |
| peak aggregate app-container CPU | 191% | 345% | 459% | **628%** |
| peak single app-container CPU | 155% | 171% | 231% | 195% |
| minimum MemAvailable | 11.03 GB | 10.11 GB | 8.74 GB | **6.78 GB** |
| peak total app-container memory | 6.94 GB* | 3.30 GB | 4.84 GB | 7.75 GB* |
| largest single app-container | 5.50 GB* | 1.10 GB | 1.10 GB | 5.50 GB* |

\* The 5.50 GB peak is a **transient startup event** discussed below, not the
steady working set. Steady per-container memory during the heavy plateau was
~1.0–1.1 GiB. Stage 1/2 samples may include pre-existing standby containers.

**Other measured facts:**

- **OOMKilled=false**, **RestartCount=0** for every test container and for
  ShinyProxy, nginx, the background report worker, and the socket proxy;
- **essentially no swap pressure** (swap used ~768 KiB throughout);
- `APP_FUTURE_WORKERS=2` produced **12 future workers** across six active app
  containers during Stage 4, sharing the same 8 cores.

**Primary measured bottleneck: CPU.**

- Network canvas is approximately **80% of heavy-workload latency** and grows
  **51.82 → 60.67 → 93.37 → 133.62 s** (monotonic, near-linear with user
  count).
- At six users, the app alone consumed ~6 of 8 host cores (peak aggregate
  CPU 628%).
- **Memory was not the limiting resource** in this experiment: minimum
  MemAvailable stayed at 6.78 GB with zero swap pressure.

**Residual memory risk (observed, not an OOM):** one app container showed a
**transient memory event near the 5 GiB cgroup limit** (5,498 MB reported by
`podman stats`) during Stage 1 and Stage 4 startup. **No OOM kill occurred**
and the container immediately returned to its ~1.0–1.1 GiB steady state. This
requires separate future investigation (section 9) and is **not** described as
an OOM.

---

## 7. ShinyProxy operational findings

Observed during controlled testing:

- `minimum-seats-available=1` maintains a **standby seat/container**. This is
  what makes raw container count appear one higher than active-user count.
- Container/seat cleanup must therefore be interpreted from **seat
  claims/releases**, not container count alone. After the last seat of a
  delegate proxy is released, that proxy stops within seconds
  (`allow-container-re-use=false`); the long-lived artifact is the standby
  seat (observed to persist >16 minutes).
- **Temporary "Seat not immediately available" messages occurred during
  scale-up.** Later workers waited a further **~8–12 s** for container spin-up
  but all successfully obtained seats.
- At the actual configured capacity ceilings, new users may **wait or be
  refused** until capacity becomes available.

**On `SP_MAX_TOTAL_INSTANCES` semantics:** production evidence shows
`SP_MAX_TOTAL_INSTANCES=6`, yet a 6-heavy-user run drove **7 containers**
(6 active + 1 standby) and ShinyProxy did **not** refuse. What the production
evidence directly demonstrates is: (a) the variable does not simply mean
"maximum 6 users"; (b) a standby seat coexists with the active seats; and (c)
when seat/container capacity is actually exhausted, new arrivals wait or are
refused. Beyond that, the exact accounting of how the standby interacts with
the total-instance ceiling is **not** fully characterized by this evidence and
should not be generalized from the variable name alone.

---

## 8. Heavy versus realistic workload

A single-user **TP53 control** was measured in the same harness:

| Workload | Total | Canvas |
|---|---:|---:|
| TP53 (1 user) | ~24.71 s | ~11.58 s |
| BRCA1 (Stage 1) | 71.98 s | 51.82 s |

BRCA1 canvas is the dominant cost, and the synchronized six-user BRCA1 run is
deliberately **adversarial**: identical, simultaneous, network-heavy work.

**Therefore:**

- **PASS THROUGH 6 HEAVY does not mean only six users can be connected.** Idle,
  browsing, and lighter-gene sessions place far less contention on CPU.
- **It also does not prove any arbitrary larger concurrency** such as 20 or 30
  simultaneous users.
- A realistic population with browsing, idle sessions, smaller genes, and
  non-synchronized operations should produce lower contention — but **exact
  mixed-workload capacity remains unmeasured.**

---

## 9. Current bottlenecks and remaining risks

| Issue | Evidence | Current severity | Recommended future action |
|---|---|---|---|
| Network canvas CPU scaling | canvas 51.82→133.62 s across 1→6 users; ~80% of total; peak CPU 628% on 8 cores | High (dominant bottleneck) | Treat CPU/Network-canvas computation as the next optimization dimension if more heavy concurrency is required |
| Transient ~5 GiB startup memory event | one container reported 5,498 MB near the 5 GiB cap; no OOM kill; returns to ~1.0–1.1 GiB | Medium (residual, uncharacterized) | Investigate separately before changing any memory limit |
| ShinyProxy/container startup latency | "Seat not immediately available"; ~8–12 s spin-up wait during scale-up | Medium | Characterize spin-up cost; consider only if measured impact warrants |
| Configured capacity ceiling behavior | 6 heavy users drove 7 containers without refusal; refusals possible at true exhaustion | Medium | Document exact seat/instance accounting; validate ceiling behavior explicitly |
| Cold BRCA1 STRING screen preparation | ~10.6 s synchronous lightweight screen prep across 367 plots / 8,858 rows; warm `payload_prepare_ms` 0.69–0.73 s | Medium | Consider only if cold-Network latency becomes a product requirement |
| Mixed-workload capacity | unmeasured | Unknown | Required before any explicit user-capacity SLA or contractual claim |

Redis, microservices, or a warm-service tier are **not** established
requirements from this evidence. They may be considered later as possible
options only if simpler measured CPU optimizations prove insufficient.

---

## 10. Operational recommendations

Based strictly on the evidence above:

- **Keep the current production revision and configuration for now.**
- **No urgent PR4 is required** as a result of this characterization.
- **Treat CPU as the next optimization dimension** if additional heavy
  concurrency is required.
- **Investigate the transient ~5 GiB startup event separately** before changing
  memory limits.
- **Preserve the current rollback/deployment discipline.**
- **Use mixed-workload testing** before making any contractual or user-capacity
  claim.
- **Continue measuring PSS/USS/cgroup**, not RSS alone, for future memory
  capacity claims.

No immediate infrastructure change is recommended, because the evidence does
not require one.

---

## 11. Validated production state

Final cleanup and health checks (measured live on `colors` after testing):

- all test user sessions released; **active user seats = 0** (seat claims ==
  releases);
- one **standby seat** expected and present (`minimum-seats-available=1`),
  left untouched;
- **no orphan workers**; no dead/stopped app containers;
- **no local test Chrome/worker processes** remaining;
- ShinyProxy healthy; nginx healthy; background report worker healthy; socket
  proxy healthy;
- **zero relevant restarts / OOM** (`OOMKilled=false`, `RestartCount=0`);
- public production endpoint healthy (HTTP 302 → landing; `/app_direct/cgv`
  HTTP 200);
- production revision and image **unchanged**:
  - revision `368872bae9b8833ffef02cdca3573bd202438485`
  - image `localhost/cgv:release-368872bae9b8-20260921T154656Z`.

---

## 12. Final conclusion

> **CGeV CONCURRENCY CHARACTERIZATION: PASS THROUGH 6 HEAVY**

This establishes that the validated production revision and architecture
successfully sustained six simultaneous, independent, synchronized, adversarial
BRCA1 workloads — including STRING Network — with synchronized overlap
(barrier-release spread ≤0.08 s), all sessions completing, no OOM kills, no
restarts, and no swap pressure. It does **not** establish a general user
capacity, and it does **not** establish behavior at 8, 10, 20, or 30
simultaneous heavy users; nor does it measure mixed realistic workloads.

The current optimization cycle can be considered **technically closed pending
future workload requirements** — not because every possible optimization
opportunity is exhausted, but because the measured system meets the
demonstrated adversarial target and no measured evidence currently requires a
further change.

---

## 13. Evidence, artifacts, and discrepancies

### Primary artifacts

- Validated revision / merge commits: `368872b` (PR #40), `3a27a5e` (PR #39),
  `878c7a4` (PR #38), `894cc92` (FutureGlobals review doc commit).
- In-repo phase documentation: `docs/performance/FUTURE_GLOBALS_REVIEW.md`,
  `docs/performance/PR2_SHARED_GENE_CACHE.md`,
  `docs/performance/PR3_STRING_ATTRIBUTE_REUSE.md`.
- **Durable concurrency evidence (in this repository):**
  `docs/performance/evidence/concurrency-20260921/`
  - `results/stage{1,2,3,4}/result_*.json` — per-worker results (per-session
    identifier removed); source of the latency medians and the makespan
    calculation;
  - `resource-summary.json` — per-stage resource peaks derived from the host
    sampler;
  - `analysis/makespan_throughput.py` and its captured output
    `analysis/makespan_throughput.txt` — reproducible throughput calculation;
  - `README.md` — stage mapping, makespan definition, and reproducibility
    commands.
- **Provenance (historical, author-local, not durable):** the concurrency
  harness and raw sampler output originally lived under
  `/var/folders/yp/…/T/opencode/cgev-conc/` (`cgev_worker.R`, `run_stage.sh`,
  `sampler.sh`, `analyze.R`, `compare.py`, `overlap.py`, and the raw
  `samples.csv`). The compact, scrubbed subset committed above is the durable
  evidence of record; the temporary path is cited only as provenance.
- Isolated A/B and profiling logs (author-local, not committed):
  `brca1_perf.log`, `pr2_perf.log`, `pr3c.log`, `pp3_brca1.log`, and
  `pr1-iso/…`, originally under the same temporary work directory.
- PR3 profiling and A/B numbers (parse/decode call counts, 40,000-case fuzz,
  control-vs-candidate canvas) recovered from the validating session log
  (`…/.codex/sessions/2026/09/21/…jsonl`).
- Live production inspection of `colors`.

### Discrepancies with the task prompt

- `card_module_init_ms` after: prompt says ~61–151 ms; measured range reported
  by the validating session is **~61–74 ms**. This report uses the measured
  range.
- Worker RSS before/after (~1.29 GB → ~119–140 MB): **not recoverable** from
  any artifact. Marked unknown; a later-revision worker RSS of ~188 MB is
  documented for what it is.
- `worker_launch_ms` before: prompt's `66,641 ms` corresponds to the measured
  "~66.6 s"; the report uses "~66.6 s" with the exact millisecond form noted.
- Degradation percentages: prompt rounds; this report gives exact computed
  values.

### Evidence gaps / future work

- Exact FutureGlobals worker-RSS before/after.
- Mixed-workload capacity and any explicit user-capacity SLA.
- Precise `SP_MAX_TOTAL_INSTANCES` / standby-seat accounting at true
  exhaustion.
- The transient ~5 GiB startup memory event (no OOM; mechanism uncharacterized).
- p95 / tail latency: the concurrency stages report medians over a small
  number of sessions, not a p95 distribution.
- Stage 2 batch makespan: absolute epoch timestamps were not recorded for that
  stage, so its makespan is derived from the shared barrier release rather than
  an independent absolute clock.
