"""Install pinned native CUDA inference artifacts independently of any build.

Python 3.12+, standard library only. Models are shared across runtime releases.
Never starts/stops OreStudio or changes its configuration.
"""
import hashlib
import json
import os
from pathlib import Path
import shutil
import sys
import tarfile
import urllib.request
import zipfile

lock = json.loads((Path(__file__).resolve().parent.parent / 'inference-lock.json').read_text())
if len(sys.argv) != 2:
    raise SystemExit('Usage: python scripts/install-inference.py <independent-runtime-root>')
root = Path(sys.argv[1]).resolve()
source = Path(__file__).resolve().parents[3]
if root == source or source in root.parents:
    raise SystemExit('Runtime root must be outside the source checkout.')
root.mkdir(parents=True, exist_ok=True)
cache = root / 'downloads'
cache.mkdir(exist_ok=True)


def sha(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def download(url, name, expected):
    dest = cache / name
    if dest.exists() and sha(dest) == expected:
        return dest
    part = dest.with_name(dest.name + '.part')
    print(f'Downloading {name}', flush=True)
    with urllib.request.urlopen(url, timeout=60) as response, part.open('wb') as stream:
        shutil.copyfileobj(response, stream, 1024 * 1024)
    if sha(part) != expected:
        part.unlink()
        raise ValueError(f'Checksum mismatch: {name}')
    part.replace(dest)
    return dest


platform = 'win-cuda-12.4-x64.zip' if os.name == 'nt' else 'ubuntu-cuda-12.8-x64.tar.gz'
release = root / 'releases' / lock['release']
release.mkdir(parents=True, exist_ok=True)
for asset in lock['assets']:
    if not asset['name'].endswith(platform):
        continue
    archive = download(asset['url'], asset['name'], asset['sha256'])
    if archive.suffix == '.zip':
        with zipfile.ZipFile(archive) as z:
            for member in z.infolist():
                dest = (release / member.filename).resolve()
                if release.resolve() not in dest.parents:
                    raise ValueError('Unsafe archive path')
            z.extractall(release)
    else:
        with tarfile.open(archive) as t:
            t.extractall(release, filter='data')
model = lock['model']
url = f"https://huggingface.co/{model['repository']}/resolve/{model['revision']}/{model['file']}?download=true"
models = root / 'models'
models.mkdir(exist_ok=True)
target = models / model['file']
if not target.exists():
    downloaded = download(url, model['file'], model['sha256'])
    # Move rather than duplicate the large model on a disk-constrained host.
    downloaded.replace(target)
if sha(target) != model['sha256']:
    raise ValueError('Installed model checksum mismatch')
(root / 'inference-lock.json').write_text(json.dumps(lock, indent=2) + '\n')
print(json.dumps({'installed': str(release), 'model': str(target), 'started': False}), flush=True)
