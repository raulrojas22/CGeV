#!/usr/bin/env python3
"""Exercise NAS synchronization with missing local datasets and guide videos."""
from pathlib import Path
import os
import shlex
import subprocess
import tempfile

root = Path(__file__).resolve().parents[1]
script = (root / 'deploy/deploy-nas-shinyproxy.sh').read_text()
start = script.index('rsync -avz --progress --delete')
block = script[start:script.index('rsync -az --chmod', start)]
excludes = [word for word in shlex.split(block.replace('\\\n', ' '))
            if word.startswith('--exclude=')]
assert excludes
preserved = ['.env', '.env.background-reports', 'data/alias_index/human.sqlite',
             'data/other-index.rds', 'cache/index.rds', 'annotations/genome.gff',
             'www/screencasts/guide-intro.mp4']
with tempfile.TemporaryDirectory() as directory:
    src, dst = Path(directory) / 'src', Path(directory) / 'dst'
    src.mkdir(); dst.mkdir()
    (src / 'www').mkdir()
    (src / 'www/main.css').write_text('new style')
    (src / '.env').write_text('local settings must not replace remote settings')
    for name in preserved:
        path = dst / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text('server-owned original')
    (dst / 'obsolete-source.R').write_text('obsolete')
    subprocess.run(['rsync', '-a', '--delete', *excludes,
                    str(src) + '/', str(dst) + '/'], check=True)
    for name in preserved:
        assert (dst / name).read_text() == 'server-owned original', name
    assert (dst / 'www/main.css').read_text() == 'new style'
    assert not (dst / 'obsolete-source.R').exists()

    # Execute the real build block with a failed Docker build: the live env
    # must still reference the old image and its static snapshot.
    live_env = 'CGV_IMAGE=cgv:old\nAPP_ASSET_VERSION=old\n'
    (dst / '.env').write_text(live_env)
    docker = Path(directory) / 'docker-probe'
    docker.write_text('''#!/bin/sh
if [ "$1 $2" = "image inspect" ] && [ "$3" = "cgv:candidate" ]; then exit 1; fi
if [ "$1 $2" = "compose build" ]; then
  printf '%s' "$CGV_IMAGE" > "$NAS_APP_DIR/built-image"
  exit 23
fi
exit 0
''')
    docker.chmod(0o755)
    build = script[script.index('# --- Paso 3:'):script.index('# --- Paso 4:')]
    env = dict(os.environ, NAS_APP_DIR=str(dst), REMOTE_DOCKER=str(docker),
               CGV_IMAGE='cgv:candidate', CGV_DEPS_IMAGE='cgv-deps:probe', REBUILD_R_DEPS='0')
    result = subprocess.run(['bash', '-c', 'set -e; nssh() { bash -c "$1"; };\n' + build],
                            env=env, capture_output=True, text=True)
    assert result.returncode == 23, result.stderr
    assert (dst / 'built-image').read_text() == 'cgv:candidate'
    assert (dst / '.env').read_text() == live_env

# A failed media check or image build must happen before stopping live services.
assert script.index('scripts/verify_guide_assets.py') < script.index(' compose build')
assert script.index('sha256sum -c deploy/guide-videos.sha256') < script.index(' stop cgv ')
assert script.index('upsert_env CGV_IMAGE') > script.index(' stop cgv-shinyproxy ')
assert script.index('upsert_env APP_ASSET_VERSION') > script.index(' stop cgv-shinyproxy ')
assert 'cgv:release-${SOURCE_REV}-' in script
assert 'ENV_CGV_IMAGE' not in script
assert 'nohup' not in script
assert 'cgev.mobilomics.org' not in script
print('PASS: NAS sync preserves settings, indexes and videos; media checked before cutover')
