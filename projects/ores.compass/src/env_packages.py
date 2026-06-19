"""compass env install-packages — install system packages for OreStudio.

Ported from build/scripts/install_debian_packages.sh; behaviour is preserved:
- Baseline packages (X11/GL headers, NATS, etc.) are always installed.
- --full-install adds the complete developer toolchain.
- --with-qt adds Qt6 dev packages (implied by --full-install).
- --with-valgrind adds Valgrind.
After a --full-install, setup_postgres_extensions.sh is run to add pg_cron,
timescaledb, and pgmq.
"""

import argparse
import subprocess
import sys
import time
from pathlib import Path

_BASELINE = [
    "autoconf",
    "autoconf-archive",
    "automake",
    "fontconfig",
    "nats-server",
    "freeglut3-dev",
    "libegl1",
    "libltdl-dev",
    "libtool",
    "^libxcb.*-dev",
    "libx11-xcb-dev",
    "libglu1-mesa-dev",
    "libxrender-dev",
    "libxi-dev",
    "libxkbcommon-dev",
    "libxkbcommon-x11-dev",
    "libegl1-mesa-dev",
    "libxcursor-dev",
    "libxinerama-dev",
    "mesa-common-dev",
    "pkg-config",
    "xorg-dev",
]

_FULL = [
    # Core build toolchain
    "build-essential",
    "gcc",
    "g++",
    "clang",
    "lld",
    # Build system
    "cmake",
    "ninja-build",
    # Version control + download tools
    "git",
    "curl",
    "wget",
    "ca-certificates",
    # Archive tools (required by vcpkg)
    "unzip",
    "zip",
    "tar",
    # Additional tools used by vcpkg ports
    "gperf",
    "nasm",
    # Python (used by validate_schemas.sh)
    "python3",
    "python3-pip",
    "python3-venv",
    # OpenSSL
    "openssl",
    "libssl-dev",
    # PostgreSQL
    "postgresql",
    "postgresql-client",
    "libpq-dev",
    # Memory analysis
    "valgrind",
]

_QT = [
    "qt6-base-dev",
    "qt6-tools-dev",
    "qt6-l10n-tools",
    "qt6-charts-dev",
    "qt6-svg-dev",
    "libqt6help6",
]

_MAX_RETRIES = 5
_RETRY_DELAY = 5


def install(project_root: Path, full: bool = False, with_qt: bool = False,
            with_valgrind: bool = False) -> int:
    """Install system packages; return exit code."""
    packages = list(_BASELINE)
    if full:
        packages.extend(_FULL)
        with_qt = True
    if with_qt:
        packages.extend(_QT)
    if with_valgrind and not full:
        packages.append("valgrind")

    delay = _RETRY_DELAY
    for attempt in range(1, _MAX_RETRIES + 1):
        print(f"Attempt {attempt} of {_MAX_RETRIES}...")
        rc = subprocess.run(
            ["sudo", "apt-get", "update", "-o", "Acquire::Retries=3"]
        ).returncode
        if rc == 0:
            rc = subprocess.run(
                ["sudo", "apt-get", "install", "-y", "-o", "Acquire::Retries=3"]
                + packages
            ).returncode
        if rc == 0:
            print("Package installation succeeded.")
            break
        if attempt == _MAX_RETRIES:
            print(f"Package installation failed after {_MAX_RETRIES} attempts.",
                  file=sys.stderr)
            return rc
        print(f"Package installation failed. Retrying in {delay}s...")
        time.sleep(delay)
        delay *= 2

    subprocess.run(["sudo", "apt-get", "clean"])
    subprocess.run(["sudo", "apt-get", "autoremove", "-y"])

    if not full:
        return 0

    ext_script = project_root / "build" / "scripts" / "setup_postgres_extensions.sh"
    if not ext_script.is_file():
        print(f"Warning: {ext_script} not found; skipping postgres extensions.",
              file=sys.stderr)
        return 0

    rc = subprocess.run([str(ext_script), "--configure", "--restart"]).returncode
    if rc != 0:
        return rc

    print("\n" + "=" * 71)
    print("  OreStudio developer environment installed successfully.")
    print("=" * 71)
    print("\n  Next step: compass env configure --preset <preset>")
    print("  Then:      compass db recreate -y")
    print("=" * 71)
    return 0


def run(argv: list, project_root: Path) -> int:
    """compass env install-packages entry point."""
    parser = argparse.ArgumentParser(
        prog="compass env install-packages",
        description="Install system packages required to build OreStudio on Debian/Ubuntu.")
    parser.add_argument("--full-install", action="store_true",
                        help="Full developer environment: compilers, cmake, postgres, "
                             "Qt6, valgrind, and postgres extensions. "
                             "Use this on a fresh Debian/Ubuntu box.")
    parser.add_argument("--with-qt", action="store_true",
                        help="Add Qt6 dev packages (implied by --full-install). "
                             "CI installs Qt via install-qt-action instead.")
    parser.add_argument("--with-valgrind", action="store_true",
                        help="Add Valgrind (without the rest of --full-install).")
    args = parser.parse_args(argv)
    return install(project_root,
                   full=args.full_install,
                   with_qt=args.with_qt,
                   with_valgrind=args.with_valgrind)
