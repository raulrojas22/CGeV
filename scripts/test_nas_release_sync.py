#!/usr/bin/env python3
"""Exercise NAS synchronization with missing local datasets and guide videos."""
from pathlib import Path
import re
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

# A failed media check or image build must happen before stopping live services.
assert script.index('scripts/verify_guide_assets.py') < script.index(' compose build')
assert script.index('sha256sum -c deploy/guide-videos.sha256') < script.index(' stop cgv ')
assert 'nohup' not in script
assert 'cgev.mobilomics.org' not in script
print('PASS: NAS sync preserves settings, indexes and videos; media checked before cutover')
