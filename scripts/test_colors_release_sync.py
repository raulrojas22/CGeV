#!/usr/bin/env python3
"""Exercise the deploy rsync exclusions against a temporary server layout."""
from pathlib import Path
import re
import subprocess
import tempfile

root = Path(__file__).resolve().parents[1]
script = (root / 'deploy/deploy-colors-shinyproxy.sh').read_text()
start = script.index('rsync -az --delete-delay')
block = script[start:script.index('# Copy only the allow-listed mail variables.', start)]
excludes = re.findall(r"--exclude='([^']+)'", block)
assert excludes
preserved = [
    'shinyproxy/application.yml', 'deploy/nginx/cgv-shinyproxy-colors.conf',
    '.env', '.env.background-reports', 'annotations/genome.gff', 'cache/index.rds',
    'www/screencasts/guide-intro.mp4',
]
with tempfile.TemporaryDirectory() as directory:
    src, dst = Path(directory) / 'src', Path(directory) / 'dst'
    src.mkdir(); dst.mkdir()
    # The reorganized repository contains deploy/, but not the old shinyproxy/.
    (src / 'deploy/nginx').mkdir(parents=True)
    (src / 'www').mkdir()
    (src / 'www/home_preview_cgv.html').write_text('new tracked home page')
    for name in preserved:
        path = dst / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text('server-owned original')
    (src / 'global.R').write_text('new application source')
    (dst / 'obsolete-source.R').write_text('old application source')
    subprocess.run(['rsync', '-a', '--delete-delay',
                    *[f'--exclude={value}' for value in excludes],
                    str(src) + '/', str(dst) + '/'], check=True)
    for name in preserved:
        assert (dst / name).read_text() == 'server-owned original', name
    assert (dst / 'global.R').read_text() == 'new application source'
    assert (dst / 'www/home_preview_cgv.html').read_text() == 'new tracked home page'
    assert not (dst / 'obsolete-source.R').exists()
print('PASS: release synchronization preserves server configuration, env, data and cache')
