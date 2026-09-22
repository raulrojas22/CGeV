#!/usr/bin/env python3
"""Recompute per-stage batch makespan and throughput from committed results.

Reads docs/performance/evidence/concurrency-20260921/results/stage<N>/result_*.json
and prints the synchronized-batch makespan and throughput for each stage.

Makespan definition:
  - Stage 3 and Stage 4 recorded absolute epoch timestamps
    (t_start_epoch at barrier release, t_end_epoch at final completion).
    makespan = max(t_end_epoch) - min(t_start_epoch).
  - Stage 1 is a single worker; its batch makespan equals that worker's
    total_work_s (elapsed from the shared barrier release to final completion).
  - Stage 2 did not record absolute epoch timestamps. Both workers were
    released from the SAME shared barrier file, so each worker's total_work_s
    is elapsed from that common release; batch makespan = max(total_work_s).
    This relies on the shared-barrier property, not on an absolute epoch.

throughput = completed_users / makespan
"""

import glob
import json
import os

HERE = os.path.dirname(os.path.abspath(__file__))
STAGES = (1, 2, 3, 4)


def load(stage):
    pat = os.path.join(HERE, "..", "results", f"stage{stage}", "result_*.json")
    out = []
    for f in sorted(glob.glob(pat)):
        with open(f) as fh:
            out.append(json.load(fh))
    return out


def makespan(stage, results):
    starts = [r.get("t_start_epoch") for r in results]
    ends = [r.get("t_end_epoch") for r in results]
    if all(v is not None for v in starts) and all(v is not None for v in ends):
        return max(ends) - min(starts), "epoch"
    # No absolute epoch: barrier is a single shared file, so each worker's
    # total_work_s is measured from that common release.
    return max(r["total_work_s"] for r in results), "shared-barrier total_work_s"


def main():
    base = None
    print(f"{'stage':>6} {'users':>6} {'makespan_s':>12} {'source':>28} "
          f"{'throughput_u/s':>16} {'relative':>9}")
    for stage in STAGES:
        results = load(stage)
        users = len(results)
        ms, source = makespan(stage, results)
        thr = users / ms
        if base is None:
            base = thr
        print(f"{stage:>6} {users:>6} {ms:>12.3f} {source:>28} "
              f"{thr:>16.5f} {thr / base:>8.2f}x")


if __name__ == "__main__":
    main()
