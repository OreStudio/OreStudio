"""Install pinned inference files; never start services or change OreStudio."""
import argparse
import hashlib
import http.client
import json
import os
from pathlib import Path
import shutil
import tarfile
import time
import urllib.request
import zipfile


def sha(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def download(cache, url, name, expected):
    dest = cache / name
    if dest.exists() and sha(dest) == expected:
        return dest
    part = dest.with_name(dest.name + '.part')
    for attempt in range(3):
        try:
            print(f'Downloading {name}, attempt {attempt + 1}', flush=True)
            with urllib.request.urlopen(url, timeout=60) as response, part.open('wb') as stream:
                shutil.copyfileobj(response, stream, 1024 * 1024)
            if sha(part) != expected:
                raise ValueError(f'Checksum mismatch: {name}')
            part.replace(dest)
            return dest
        except (OSError, ValueError, http.client.IncompleteRead):
            part.unlink(missing_ok=True)
            if attempt == 2:
                raise
            time.sleep(1)


def extract(archive, release):
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


def install(root, cpu=False):
    lock = json.loads((Path(__file__).resolve().parent.parent / 'inference-lock.json').read_text())
    root = root.resolve()
    source = Path(__file__).resolve().parents[3]
    if root == source or source in root.parents:
        raise ValueError('Runtime root must be outside the source checkout.')
    backend = 'cpu' if cpu else 'cuda'
    installed_lock = root / 'inference-lock.json'
    if installed_lock.exists():
        installed_backend = json.loads(installed_lock.read_text()).get('backend', 'cuda')
        if installed_backend != backend:
            raise ValueError('CPU and CUDA deployments require separate runtime roots.')
    root.mkdir(parents=True, exist_ok=True)
    cache = root / 'downloads'
    cache.mkdir(exist_ok=True)
    if cpu:
        platform = 'win-cpu-x64.zip' if os.name == 'nt' else 'ubuntu-x64.tar.gz'
        assets = lock['cpuAssets']
    else:
        platform = 'win-cuda-12.4-x64.zip' if os.name == 'nt' else 'ubuntu-cuda-12.8-x64.tar.gz'
        assets = lock['assets']
    selected = [asset for asset in assets if asset['name'].endswith(platform)]
    if not selected:
        raise ValueError('No pinned inference archive for this platform.')
    release = root / 'releases' / (lock['release'] + ('-cpu' if cpu else ''))
    release.mkdir(parents=True, exist_ok=True)
    for asset in selected:
        extract(download(cache, asset['url'], asset['name'], asset['sha256']), release)
    model = lock['model']
    models = root / 'models'
    models.mkdir(exist_ok=True)
    target = models / model['file']
    if not target.exists():
        url = f"https://huggingface.co/{model['repository']}/resolve/{model['revision']}/{model['file']}?download=true"
        # Move the verified download; do not duplicate the large model.
        download(cache, url, model['file'], model['sha256']).replace(target)
    if sha(target) != model['sha256']:
        raise ValueError('Installed model checksum mismatch')
    if cpu:
        lock['backend'] = 'cpu'
    (root / 'inference-lock.json').write_text(json.dumps(lock, indent=2) + '\n')
    print(json.dumps({'installed': str(release), 'model': str(target), 'started': False}), flush=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('root', type=Path)
    parser.add_argument('--cpu', action='store_true', help='Use a separate CPU-only runtime root.')
    options = parser.parse_args()
    install(options.root, options.cpu)
