"""Compute common vs. per-service shared libraries via ldd intersection.

Example
-------
Given three services with these dependencies::

    svc_a needs {libores.service, libores.logging, libores.utility}
    svc_b needs {libores.service, libores.logging}
    svc_c needs {libores.service, libores.nats}

The *intersection* (common to all) is {libores.service}.
Per-service extras::

    svc_a → {libores.logging, libores.utility}
    svc_b → {libores.logging}
    svc_c → {libores.nats}

The base image carries common_libs; each per-service overlay carries its
binary plus its extra libs (if any).

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
        try:
            resolved = Path(path).resolve()
        except OSError:
            continue

        if str(lib_dir.resolve()) in str(resolved):
            libs.add(soname)

    return libs


def compute_partition(
    bin_dir: Path,
    lib_dir: Path,
    services: list[str],
) -> tuple[set[str], dict[str, set[str]]]:
    """Compute common and per-service library sets.

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
    (common_libs, service_extras)
        ``common_libs`` is the set of SONAMEs needed by **every** service
        (the intersection).  ``service_extras`` maps each service name to
        the set of SONAMEs it needs beyond the common set (may be empty).

        A service whose binary is missing from *bin_dir* (or is statically
        linked) contributes an empty set — it does not shrink the
        intersection (an empty set intersected with anything yields the
        empty set, so we skip it to preserve the meaningful intersection).
    """
    service_libs: dict[str, set[str]] = {}
    for svc in services:
        binary = bin_dir / svc
        if not binary.is_file():
            raise FileNotFoundError(
                f"Service binary not found: {binary}"
            )
        libs = _ldd_libs(binary, lib_dir)
        if libs:
            service_libs[svc] = libs

    if not service_libs:
        return set(), {}

    # Intersection of all non-empty lib sets.
    common = set.intersection(*service_libs.values()) if service_libs else set()

    extras: dict[str, set[str]] = {}
    for svc in services:
        libs = service_libs.get(svc, set())
        extra = libs - common
        if extra:
            extras[svc] = extra

    return common, extras


def stage_for_build(
    stage_dir: Path,
    bin_dir: Path,
    lib_dir: Path,
    services: list[str],
    common_libs: set[str],
    service_extras: dict[str, set[str]],
) -> None:
    """Stage files for the base and per-service image builds.

    Creates under *stage_dir*::

        base/lib/         ← common_libs (SONAME symlink + real file)
        base/log/         ← empty, mode 777
        base/run/         ← empty, mode 777
        base/storage/     ← empty, mode 777
        services/<svc>/bin/<svc>       ← service binary
        services/<svc>/lib/<soname>…   ← extra libs (if any)

    Each lib is copied as both the unversioned SONAME symlink **and** its
    real file (following the symlink), matching the convention in
    ``stage-runtime.sh``.
    """
    base_dir = stage_dir / "base"
    services_root = stage_dir / "services"

    # -- Base directories ------------------------------------------------
    (base_dir / "lib").mkdir(parents=True, exist_ok=True)
    for d in ("log", "run", "storage"):
        p = base_dir / d
        p.mkdir(parents=True, exist_ok=True)
        p.chmod(0o777)

    # -- Common libs ------------------------------------------------------
    for soname in sorted(common_libs):
        src_symlink = lib_dir / soname
        if not src_symlink.is_symlink():
            # Some SONAMEs are the real file, not a symlink.
            _copy_file(src_symlink, base_dir / "lib" / soname)
            continue
        real = src_symlink.resolve()
        _copy_file(src_symlink, base_dir / "lib" / soname)       # the symlink
        _copy_file(real, base_dir / "lib" / real.name)           # the real file

    # -- Per-service ------------------------------------------------------
    for svc in services:
        svc_dir = services_root / svc
        (svc_dir / "bin").mkdir(parents=True, exist_ok=True)
        # Service binary
        _copy_file(bin_dir / svc, svc_dir / "bin" / svc)
        # Entrypoint symlink — relative, resolves inside /app/bin at
        # runtime.  COPY <dir>/ /app/bin/ preserves the symlink as-is.
        entrypoint = svc_dir / "bin" / "entrypoint"
        if entrypoint.exists():
            entrypoint.unlink()
        entrypoint.symlink_to(f"./{svc}")
        # Extra libs
        extras = service_extras.get(svc, set())
        if extras:
            (svc_dir / "lib").mkdir(parents=True, exist_ok=True)
            for soname in sorted(extras):
                src_symlink = lib_dir / soname
                if not src_symlink.is_symlink():
                    _copy_file(src_symlink, svc_dir / "lib" / soname)
                    continue
                real = src_symlink.resolve()
                _copy_file(src_symlink, svc_dir / "lib" / soname)
                _copy_file(real, svc_dir / "lib" / real.name)


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
