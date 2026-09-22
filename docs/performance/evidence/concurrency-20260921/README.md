# Controlled-concurrency evidence — 2026-09-21

Durable, compact evidence supporting the concurrency tables and calculations in
[`../../CGEV_PERFORMANCE_CAPACITY_FINAL.md`](../../CGEV_PERFORMANCE_CAPACITY_FINAL.md).

Revision under test: `368872bae9b8833ffef02cdca3573bd202438485`
Image: `localhost/cgv:release-368872bae9b8-20260921T154656Z`

## Layout

- `results/stage{1,2,3,4}/result_*.json` — per-worker results as written by the
  harness, with the per-session identifier (`session_token`) removed. These are
  the source of the per-stage latency medians and the makespan calculation.
- `resource-summary.json` — per-stage resource peaks derived from the host
  sampler (`podman stats` + `/proc`) reduced to compact form. Container names
  and host-local paths are intentionally excluded.
- `analysis/makespan_throughput.py` — self-contained recomputation of the
  synchronized-batch makespan and throughput from `results/`.
- `analysis/makespan_throughput.txt` — captured output of the above.

## Stage → user mapping

| Stage | Heavy users |
|---:|---:|
| 1 | 1 |
| 2 | 2 |
| 3 | 4 |
| 4 | 6 |

## Makespan definition

Batch throughput uses a synchronized-batch **makespan**, not a per-user median:

- **Stage 3 / Stage 4**: absolute epoch timestamps were recorded
  (`t_start_epoch` at barrier release, `t_end_epoch` at final completion);
  `makespan = max(t_end_epoch) − min(t_start_epoch)`.
- **Stage 1**: single worker; batch makespan equals that worker's
  `total_work_s` (elapsed from the shared barrier release).
- **Stage 2**: no absolute epoch timestamps were recorded. Both workers were
  released from the **same shared barrier file**, so each worker's
  `total_work_s` is elapsed from that common release; batch makespan =
  `max(total_work_s)`. This relies on the shared-barrier property, not on an
  absolute epoch.

`throughput = completed_users / makespan`.

## How to reproduce

```sh
python3 analysis/makespan_throughput.py
```

## Privacy / minimization

The committed results are scrubbed of the per-session identifier. Raw host
sampler CSVs, browser profiles, container names, and host-local filesystem
paths are not committed; only derived summaries are retained.
