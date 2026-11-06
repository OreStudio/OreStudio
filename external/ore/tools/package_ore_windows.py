"""Create a flat, verified Windows ORE tarball; invoked by the PowerShell entry point."""
import argparse
from collections import deque
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import struct
import subprocess
import sys
import tarfile
import tempfile


def sha256(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2) + '\n', encoding='utf-8', newline='\n')


def require_x64(path):
    with path.open('rb') as stream:
        if stream.read(2) != b'MZ':
            raise ValueError(f'Not a Windows PE binary: {path}')
        stream.seek(0x3c)
        offset = struct.unpack('<I', stream.read(4))[0]
        stream.seek(offset)
        if stream.read(4) != b'PE\0\0' or struct.unpack('<H', stream.read(2))[0] != 0x8664:
            raise ValueError(f'Not an x64 Windows binary: {path}')


def imports(path):
    output = subprocess.check_output(['dumpbin.exe', '/nologo', '/dependents', str(path)],
                                     text=True, errors='replace')
    return sorted(set(re.findall(r'^\s+([\w.+-]+\.dll)\s*$', output, re.M | re.I)), key=str.lower)


def resolve_dependency(name, directories, system_dir):
    # API sets are OS loader contracts, not deployable DLLs.
    if name.lower().startswith(('api-ms-win-', 'ext-ms-win-')):
        return None
    for directory in directories:
        candidate = directory / name
        if candidate.is_file():
            return candidate
    # VC runtimes must be packaged from the redistributable tree, even when
    # the build host has a copy in System32. Never silently require its install.
    vc_runtime = name.lower().startswith(('vcruntime', 'msvcp', 'concrt', 'vcomp'))
    if not vc_runtime and (system_dir / name).is_file():
        return None
    raise FileNotFoundError(f'Unresolved runtime dependency: {name}')


def bundle_dependencies(executable, stage, directories, system_dir):
    pending = deque([executable])
    seen = set()
    host_dlls = set()
    while pending:
        binary = pending.popleft()
        require_x64(binary)
        for name in imports(binary):
            key = name.lower()
            if key in seen:
                continue
            seen.add(key)
            resolved = resolve_dependency(name, directories, system_dir)
            if resolved is None:
                host_dlls.add(name)
            else:
                require_x64(resolved)
                target = stage / name
                shutil.copy2(resolved, target)
                pending.append(target)
    return sorted(host_dlls, key=str.lower)


def extract_sample(sample, job):
    if sample.is_dir():
        shutil.copytree(sample, job, dirs_exist_ok=True)
    else:
        with tarfile.open(sample, 'r:gz') as archive:
            archive.extractall(job, filter='data')
    # A prior run's output must never make the self-test pass.
    shutil.rmtree(job / 'Output', ignore_errors=True)
    if not (job / 'Input/ore.xml').is_file():
        raise ValueError('The sample must contain Input/ore.xml at its root.')


def verify_archive(archive_path, scratch, sample, expected, rtol):
    extracted, job = scratch / 'extracted', scratch / 'job'
    with tarfile.open(archive_path, 'r:gz') as archive:
        archive.extractall(extracted, filter='data')
    manifest = json.loads((extracted / 'manifest.json').read_text(encoding='utf-8'))
    if manifest != {'executable': 'ore.exe', 'args': ['Input/ore.xml']}:
        raise ValueError('Unexpected runtime manifest or archive layout.')
    extract_sample(sample, job)
    environment = os.environ.copy()
    windows = Path(os.environ['SystemRoot'])
    environment['PATH'] = os.pathsep.join([str(windows / 'System32'), str(windows)])
    environment['TZ'] = 'UTC'
    log_path = scratch / 'engine.log'
    with log_path.open('w', encoding='utf-8') as log:
        result = subprocess.run([str(extracted / manifest['executable']), *manifest['args']],
                                cwd=job, env=environment, stdout=log, stderr=subprocess.STDOUT,
                                timeout=600)
    if result.returncode:
        raise RuntimeError(f'Packaged engine failed ({result.returncode}):\n{log_path.read_text(errors="replace")[-8000:]}')
    references = sorted(expected.glob('*.csv'))
    if not references:
        raise ValueError(f'No reference CSV files in {expected}')
    report = []
    comparator = Path(__file__).with_name('compare_csv.py')
    for reference in references:
        actual = job / 'Output' / reference.name
        if not actual.is_file():
            raise FileNotFoundError(f'Engine did not produce {actual.name}')
        result = subprocess.run([sys.executable, '-X', 'utf8', str(comparator),
                                 str(actual), str(reference), '--rtol', str(rtol)],
                                capture_output=True, text=True, encoding='utf-8')
        report.append(f'{reference.name}:\n{result.stdout}{result.stderr}')
        if result.returncode:
            raise RuntimeError('\n'.join(report))
    return '\n'.join(report), log_path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-dir', type=Path, required=True)
    parser.add_argument('--out-dir', type=Path, required=True)
    parser.add_argument('--version', required=True)
    parser.add_argument('--sample', type=Path, required=True)
    parser.add_argument('--expected-output', type=Path, required=True)
    parser.add_argument('--rtol', type=float, default=1e-6)
    args = parser.parse_args()
    if not re.fullmatch(r'[A-Za-z0-9][A-Za-z0-9.+_-]*', args.version):
        parser.error('Version must be a filename-safe version string.')
    if not 0 < args.rtol < 1:
        parser.error('rtol must be between zero and one.')
    build = args.build_dir.resolve()
    record = json.loads((build / 'orestudio-build.json').read_text(encoding='utf-8'))
    source = Path(next(x.split('=', 1)[1] for x in (build / 'CMakeCache.txt').read_text().splitlines()
                       if x.startswith('CMAKE_HOME_DIRECTORY:INTERNAL=')))
    commit = subprocess.check_output(['git', '-C', str(source), 'rev-parse', 'HEAD'], text=True).strip()
    dirty = subprocess.check_output(['git', '-C', str(source), 'status', '--porcelain',
                                     '--untracked-files=no', '--ignore-submodules=none'], text=True)
    if commit != record['engine_commit'] or dirty:
        raise ValueError('Engine source changed since the recorded build; rebuild before packaging.')
    executable = build / 'App/Release/ore.exe'
    dependencies = build / 'vcpkg_installed/x64-windows'
    redist_root = Path(os.environ['VCToolsRedistDir'])
    crt_directories = sorted((redist_root / 'x64').glob('Microsoft.VC*.CRT'))
    if not crt_directories:
        raise FileNotFoundError(f'No redistributable x64 VC runtime in {redist_root}')
    package_name = f'ore-{args.version}-x64-windows'
    out = args.out_dir.resolve()
    with tempfile.TemporaryDirectory(prefix='ore-windows-') as temp:
        scratch = Path(temp)
        stage = scratch / 'package'
        stage.mkdir()
        shutil.copy2(executable, stage / 'ore.exe')
        host_dlls = bundle_dependencies(executable, stage,
                                       [executable.parent, dependencies / 'bin', *crt_directories],
                                       Path(os.environ['SystemRoot']) / 'System32')
        write_json(stage / 'manifest.json', {'executable': 'ore.exe', 'args': ['Input/ore.xml']})
        licences = stage / 'licenses'
        licences.mkdir()
        shutil.copy2(source / 'license.txt', licences / 'ORE.txt')
        shutil.copy2(source / 'QuantLib/LICENSE.TXT', licences / 'QuantLib.txt')
        shutil.copy2(source / 'ThirdPartyLibs/rapidxml-1.13/license.txt', licences / 'RapidXML.txt')
        for copyright_file in sorted((dependencies / 'share').glob('*/copyright')):
            shutil.copy2(copyright_file, licences / f'{copyright_file.parent.name}.txt')
        # VS's distributable-list notice is distinct from the application licence.
        redist_list = Path(os.environ['VSINSTALLDIR']) / 'Licenses/1033/Redist.txt'
        if redist_list.is_file():
            shutil.copy2(redist_list, licences / 'Microsoft-VC-Redist.txt')
        record.update({
            'file': package_name + '.tar.gz', 'engine_version': args.version,
            'built_by': 'external/ore/tools/build_ore_windows.ps1',
            'packaged_by': 'external/ore/tools/package_ore_windows.ps1',
            'platform': 'x64-windows', 'host_system_dlls': host_dlls,
            'packaged_at': datetime.now(timezone.utc).isoformat(),
            'runtime_files': {p.name: sha256(p) for p in sorted(stage.iterdir()) if p.is_file()},
            'dependency_status_sha256': sha256(build / 'vcpkg_installed/vcpkg/status'),
            'verified_against': str(args.expected_output.resolve()), 'rtol': args.rtol,
        })
        write_json(stage / 'provenance.json', record)
        archive_path = scratch / (package_name + '.tar.gz')
        with tarfile.open(archive_path, 'w:gz') as archive:
            for path in sorted(stage.iterdir()):
                archive.add(path, arcname=path.name)
        report, engine_log = verify_archive(archive_path, scratch, args.sample.resolve(),
                                            args.expected_output.resolve(), args.rtol)
        record['sha256'] = sha256(archive_path)
        record['size_bytes'] = archive_path.stat().st_size
        out.mkdir(parents=True, exist_ok=True)
        destination = out / archive_path.name
        if destination.exists() and sha256(destination) != record['sha256']:
            raise FileExistsError(f'Refusing to overwrite existing package: {destination}; use a new version or output directory.')
        shutil.copy2(archive_path, destination)
        write_json(out / (package_name + '.provenance.json'), record)
        (out / (package_name + '.diff-report.txt')).write_text(
            f'Package: {package_name}\nEngine commit: {commit}\nSHA256: {record["sha256"]}\n'
            f'Extracted archive tested with PATH restricted to Windows system directories.\n'
            f'Tolerance: rtol={args.rtol}\n\n{report}', encoding='utf-8', newline='\n')
        (out / (package_name + '.engine.log')).write_text(
            engine_log.read_text(encoding='utf-8', errors='replace'),
            encoding='utf-8', newline='\n')
        print(f'Verified: {destination}\nSHA256: {record["sha256"]}\n{report}')


if __name__ == '__main__':
    main()
