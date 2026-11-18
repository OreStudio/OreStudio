"""Compute the libraries the service images share, and stage them once.

Example
-------
Given three services with these dependencies::

    svc_a needs {libores.service, libores.logging, libores.utility}
    svc_b needs {libores.service, libores.logging}
    svc_c needs {libores.service, libores.nats}

The *union* is {libores.service, libores.logging, libores.utility,
libores.nats}. One shared image carries that union, so each library is
stored and transferred once and each per-service image carries only its
own binary. An intersection instead pushes every library that two
services do not both need into the per-service images, which copies a
large library once per service that uses it.

Excluded from consideration
---------------------------
- ``linux-vdso.so.*`` — kernel virtual DSO, not a real file.
- ``ld-linux-x86-64.so.*`` — the dynamic linker itself, provided by the
  chainguard base image (glibc-dynamic).
- Any lib resolved to a path outside the staged lib directory (system libs
  such as libm, libc, libstdc++ — all provided by the chainguard base).
- ``statically linked`` binaries — they have no dynamic deps at all.
  Any service that is statically linked contributes zero libs; it does
  not reduce the common set (intersection of nothing is nothing).
"""

from __future__ import annotations

import os
import re
import subprocess
from collections import defaultdict
from pathlib import Path

# System libraries excluded from consideration because they are provided
# by the chainguard/glibc-dynamic base image (or the kernel).  We only
# manage our own ores.*.so.* libraries.
_SYSTEM_LIB_PATTERNS = [
    re.compile(r"^linux-vdso\.so\."),
    re.compile(r"^ld-linux-x86-64\.so\."),
]


def _is_system_lib(soname: str) -> bool:
    """Return True if *soname* is a system/kernel library we never stage."""
    for pat in _SYSTEM_LIB_PATTERNS:
        if pat.match(soname):
            return True
    return False


def _ldd_libs(binary: Path, lib_dir: Path) -> set[str]:
    """Return the set of library **sonames** that *binary* resolves to
    inside *lib_dir* (the staged lib directory).

    Runs ``ldd`` with ``LD_LIBRARY_PATH=<lib_dir>`` so that the linker
    can find the staged copies of our own libraries first.  Libraries
    resolved to paths outside *lib_dir* are treated as system libraries
    and excluded from the returned set.
    """
    env = {**os.environ, "LD_LIBRARY_PATH": str(lib_dir)}
    result = subprocess.run(
        ["ldd", str(binary)],
        capture_output=True, text=True, env=env,
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"ldd failed on {binary} (exit {result.returncode}): "
            f"{result.stderr.strip()}"
        )

    libs: set[str] = set()
    for line in result.stdout.splitlines():
        line = line.strip()
        if not line:
            continue

        # Statically linked binary — no dynamic deps.
        if "statically linked" in line:
            return set()

        # Expected format:  soname => /path/to/lib (address)
        # Unresolved:       soname => not found
        m = re.match(r"^\s*(\S+)\s+=>\s+(\S+)\s", line)
        if not m:
            continue

        soname, path = m.group(1), m.group(2)
        if path == "not":
            # Unresolved library — "soname => not found"
            continue

        if _is_system_lib(soname):
            continue

        # Only include libs that live inside our staged lib directory
        # (or the publish directory — same content).  This naturally
        # filters out libc, libm, libstdc++, libgcc_s, libpthread, …
        # Path containment, not string containment: a sibling dir that
        # shares the path as a prefix (e.g. .../lib-old) must not match.
        try:
            resolved = Path(path).resolve()
        except OSError:
            continue

        if resolved.is_relative_to(lib_dir.resolve()):
            libs.add(soname)

    return libs


def compute_shared_libraries(
    bin_dir: Path,
    lib_dir: Path,
    services: list[str],
) -> set[str]:
    """Return the libraries the shared image must carry.

    Parameters
    ----------
    bin_dir:
        Directory containing staged, stripped service binaries (e.g.
        ``build/docker-stage/bin``).
    lib_dir:
        Directory containing staged, stripped shared libraries.
    services:
        List of service binary names (e.g. ``["ores.iam.service", …]``).

    Returns
    -------
    set[str]
        The union of the SONAMEs the services resolve inside *lib_dir*.
        A statically linked service contributes nothing, which cannot
        remove a library another service needs.
    """
    shared: set[str] = set()
    for svc in services:
        binary = bin_dir / svc
        if not binary.is_file():
            raise FileNotFoundError(
                f"Service binary not found: {binary}"
            )
        shared |= _ldd_libs(binary, lib_dir)
    return shared


def stage_for_build(
    stage_dir: Path,
    bin_dir: Path,
    lib_dir: Path,
    services: list[str],
    shared_libs: set[str],
) -> None:
    """Stage files for the library and per-service image builds.

    Creates under *stage_dir*::

        libs/lib/         ← shared_libs (SONAME symlink + real file)
        services/<svc>/bin/<svc>       ← service binary
        services/<svc>/bin/version.manifest ← the build record it reads

    Each lib is copied as both the unversioned SONAME symlink **and** its
    real file (following the symlink), matching the convention in
    ``stage-runtime.sh``. The empty runtime directories that the base
    image carries are created by ``stage-runtime.sh``.

    A per-service image carries no library: everything the services load
    is in the shared image.
    """
    services_root = stage_dir / "services"
    (stage_dir / "libs" / "lib").mkdir(parents=True, exist_ok=True)

    # -- Shared libs ------------------------------------------------------
    for soname in sorted(shared_libs):
        src_symlink = lib_dir / soname
        if not src_symlink.is_symlink():
            # Some SONAMEs are the real file, not a symlink.
            _copy_file(src_symlink, stage_dir / "libs" / "lib" / soname)
            continue
        real = src_symlink.resolve()
        _copy_file(src_symlink, stage_dir / "libs" / "lib" / soname)
        _copy_file(real, stage_dir / "libs" / "lib" / real.name)

    # -- Per-service ------------------------------------------------------
    for svc in services:
        svc_dir = services_root / svc
        (svc_dir / "bin").mkdir(parents=True, exist_ok=True)
        _copy_file(bin_dir / svc, svc_dir / "bin" / svc)
        # The build record goes beside the binary that reads it. A service
        # resolves the manifest from its own executable path, so an image
        # without it reports an unknown commit.
        manifest = bin_dir / "version.manifest"
        if manifest.is_file():
            _copy_file(manifest, svc_dir / "bin" / "version.manifest")
        # Entrypoint symlink — relative, resolves inside /app/bin at
        # runtime.  COPY <dir>/ /app/bin/ preserves the symlink as-is.
        entrypoint = svc_dir / "bin" / "entrypoint"
        if entrypoint.exists():
            entrypoint.unlink()
        entrypoint.symlink_to(f"./{svc}")


def _copy_file(src: Path, dst: Path) -> None:
    """Copy *src* to *dst*, preserving symlinks and the executable bit.

    Creates the destination's parent directory if needed.
    """
    dst.parent.mkdir(parents=True, exist_ok=True)
    if src.is_symlink():
        # Preserve the symlink — read its target and create a matching one.
        target = os.readlink(str(src))
        dst.symlink_to(target)
    else:
        data = src.read_bytes()
        dst.write_bytes(data)
        st = src.stat()
        dst.chmod(st.st_mode)
