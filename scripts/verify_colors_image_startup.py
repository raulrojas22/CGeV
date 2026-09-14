#!/usr/bin/env python3
"""Boot the candidate using ShinyProxy's command before changing production."""

import argparse
import json
from pathlib import Path
import re
import subprocess
import time
import uuid

from build_colors_shinyproxy_candidates import (
    CandidateError, _block_end, _body, _indent, _key_matches, _prepare,
    build_application_candidate,
)


def candidate_command(text):
    # Validate the complete server-owned spec without printing its contents.
    build_application_candidate(text)
    lines = _prepare(text, "application.yml")
    spec = next(i for i, line in enumerate(lines)
                if re.fullmatch(r" *-\s+id\s*:\s*(?:cgv|'cgv'|\"cgv\")\s*(?:#.*)?", _body(line)))
    end = _block_end(lines, spec, _indent(lines[spec]))
    matches = _key_matches(lines[spec + 1:end], "container-cmd")
    if len(matches) != 1:
        raise CandidateError("candidate must explicitly set its launch command")
    command = json.loads(matches[0][1].group("value"))
    if command != ["bash", "/app/deploy/docker/run-app.sh"]:
        raise CandidateError("candidate still uses an obsolete launch command")
    return command


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--application", type=Path, required=True)
    parser.add_argument("--image", required=True)
    parser.add_argument("--memory-limit", required=True)
    args = parser.parse_args()
    if not re.fullmatch(r"[1-9][0-9]*[mMgG]", args.memory_limit):
        parser.error("--memory-limit must be positive MiB/GiB (e.g. 5g)")
    command = candidate_command(args.application.read_text())
    name = "cgv-startup-check-" + uuid.uuid4().hex[:12]

    def podman(*argv, **kwargs):
        return subprocess.run(["podman", *argv], text=True, timeout=30, **kwargs)

    try:
        podman("run", "-d", "--name", name, "--network", "none",
               "--user", "10001:10001", "--cpus", "1", "--memory", args.memory_limit,
               "--tmpfs", "/app/cache:mode=1777", "--entrypoint", command[0],
               args.image, *command[1:], check=True, stdout=subprocess.DEVNULL)
        deadline = time.monotonic() + 120
        while time.monotonic() < deadline:
            state = podman("inspect", name, "--format", "{{.State.Running}}",
                           check=True, capture_output=True).stdout.strip()
            if state != "true":
                raise RuntimeError("candidate launch process exited")
            health = podman("exec", name, "wget", "-qO-",
                            "http://127.0.0.1:3838/healthz.txt", capture_output=True)
            if health.returncode == 0 and health.stdout.strip() == "ok":
                print("Candidate startup: OK (effective ShinyProxy command, UID 10001, isolated cache)")
                return
            time.sleep(1)
        raise RuntimeError("candidate did not become ready in 120 seconds")
    except Exception:
        podman("logs", "--tail", "60", name, check=False)
        raise
    finally:
        podman("rm", "-f", name, check=False, stdout=subprocess.DEVNULL,
               stderr=subprocess.DEVNULL)


if __name__ == "__main__":
    main()
