#!/usr/bin/env python3
from pathlib import Path
import hashlib
import tempfile

from verify_guide_assets import verify

root = Path(__file__).resolve().parents[1]
assert verify(root, catalog_only=True) > 0
with tempfile.TemporaryDirectory() as tmp:
    fixture = Path(tmp)
    (fixture / "deploy").mkdir()
    (fixture / "www/screencasts").mkdir(parents=True)
    (fixture / "ui.R").write_text('guide_media_files <- c("guide-intro.mp4")\nguide_media_map <- NULL')
    media = fixture / "www/screencasts/guide-intro.mp4"
    manifest = fixture / "deploy/guide-videos.sha256"
    manifest.write_text(hashlib.sha256(b"original video").hexdigest() + "  www/screencasts/guide-intro.mp4\n")

    def rejected(expected):
        try:
            verify(fixture)
        except ValueError as error:
            assert expected in str(error), str(error)
        else:
            raise AssertionError("invalid guide media was accepted")

    rejected("missing")
    media.write_bytes(b"original video")
    assert verify(fixture) == 1
    media.write_bytes(b"damaged video")
    rejected("checksum")
    media.unlink()
    target = fixture / "elsewhere.mp4"
    target.write_bytes(b"original video")
    media.symlink_to(target)
    rejected("symlinked")
    manifest.write_text(manifest.read_text().splitlines(True)[0])
    media.unlink()
    media.write_bytes(b"original video")
    assert verify(fixture) == 1
    extra = fixture / "www/screencasts/guide-extra.mp4"
    extra.write_bytes(b"unlisted video")
    rejected("unlisted")
    extra.unlink()
    assert verify(fixture) == 1
    manifest.write_text(manifest.read_text() * 2)
    rejected("duplicate")
    manifest.write_text("")
    rejected("does not match")
print("PASS: guide inventory rejects missing, damaged, symlinked and unlisted media")
