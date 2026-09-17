#!/usr/bin/env python3
"""Validate the guide's versioned media inventory and deployed video bytes."""

import argparse
import hashlib
from pathlib import Path
import re


def verify(root: Path, catalog_only: bool = False) -> int:
    ui = (root / "ui.R").read_text()
    catalog = re.search(r"guide_media_files\s*<-\s*c\((.*?)\)\s*guide_media_map", ui, re.S)
    if not catalog:
        raise ValueError("guide media catalog not found in ui.R")
    names = re.findall(r'"([a-z0-9-]+\.mp4)"', catalog.group(1))
    if not names or len(set(names)) != len(names):
        raise ValueError("guide media catalog is empty or contains duplicates")
    expected = {"www/screencasts/" + name for name in names}
    hashes = {}
    for line in (root / "deploy/guide-videos.sha256").read_text().splitlines():
        match = re.fullmatch(r"([a-f0-9]{64})  (www/screencasts/[a-z0-9-]+\.mp4)", line)
        if not match or match[2] in hashes:
            raise ValueError("invalid or duplicate guide video manifest entry")
        hashes[match[2]] = match[1]
    if set(hashes) != expected:
        raise ValueError("guide video manifest does not match the UI catalog")
    if not catalog_only:
        deployed_dir = root / "www/screencasts"
        if deployed_dir.is_dir():
            for entry in sorted(deployed_dir.rglob("*")):
                if entry.is_symlink() or entry.is_file():
                    relative = "www/screencasts/" + entry.relative_to(deployed_dir).as_posix()
                    if relative not in hashes:
                        raise ValueError(f"unlisted guide media file: {relative}")
        for relative, expected_hash in hashes.items():
            path = root / relative
            if path.is_symlink() or not path.is_file():
                raise ValueError(f"missing or symlinked guide video: {relative}")
            digest = hashlib.sha256()
            with path.open("rb") as source:
                for block in iter(lambda: source.read(1024 * 1024), b""):
                    digest.update(block)
            if digest.hexdigest() != expected_hash:
                raise ValueError(f"guide video checksum mismatch: {relative}")
    return len(hashes)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path("."))
    parser.add_argument("--catalog-only", action="store_true")
    args = parser.parse_args()
    count = verify(args.root, args.catalog_only)
    print(f"Guide assets: OK ({count} videos; " + ("catalog" if args.catalog_only else "SHA-256 verified") + ")")
