"""compass image build — build the ORE Studio container image hierarchy.

Builds a shared base image plus thin per-service overlays, stripping
binaries on the host before podman ever sees them.  A separate step
that ``compass env deploy`` requires to have been run first.

Image model::

    ores-service-base (once, ~1.5 GB)
        Common shared libraries (ldd intersection), healthcheck probe,
        empty log/run/storage dirs.  docker/service-base.Dockerfile.

    Per-service overlays (N images, ~MB each)
        Service binary + entrypoint symlink, FROM ores-service-base.
        docker/service-runtime.Dockerfile.

    ores-nats
        Standalone NATS image.  docker/nats.Dockerfile.

    ores-compute-wrapper
        Standalone compute-node image.  docker/service-compute.Dockerfile.
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

# Image names (exported — env_deploy imports these).
IMAGE_BASE = "localhost/ores-service-base"
IMAGE_NATS = "localhost/ores-nats"
IMAGE_COMPUTE = "localhost/ores-compute-wrapper"


def _run(cmd, project_root, check=True, **kwargs):
    return subprocess.run(cmd, cwd=project_root, check=check, **kwargs)


def _version_tag(project_root: Path) -> str:
    cmake = project_root / "CMakeLists.txt"
    version = "0.0.0"
    if cmake.is_file():
        text = cmake.read_text(encoding="utf-8")
        m = re.search(r'project\(\s*OreStudio\s+VERSION\s+(\S+)\s', text)
        if m:
            version = m.group(1)
    result = subprocess.run(
        ["git", "rev-parse", "--short", "HEAD"],
        cwd=project_root, capture_output=True, text=True)
    commit = result.stdout.strip() if result.returncode == 0 else "unknown"
    return f"{version}-{commit}"


def _runtime_services(project_root: Path) -> list:
    from systemd_generate import load_service_registry  # noqa: PLC0415
    raw = load_service_registry(project_root)
    return [s["name"] for s in raw
            if s.get("enabled") and s["name"] != "ores.compute.wrapper"]


def build_base(project_root: Path, tag: str | None = None) -> str:
    """Build the shared base image.  Returns the tag."""
    if tag is None:
        tag = _version_tag(project_root)
    print(f"=== Building base image: {IMAGE_BASE}:{tag} ===")
    _run(["podman", "build", "--format", "docker",
          "-t", f"{IMAGE_BASE}:local",
          "-t", f"{IMAGE_BASE}:{tag}",
          "-f", "docker/service-base.Dockerfile", "."],
         project_root)
    return tag


def build_overlays(project_root: Path, tag: str | None = None,
                   services: list | None = None) -> str:
    """Build per-service overlay images (one per enabled service).

    Requires the base image to already exist.
    Returns the tag used.
    """
    if tag is None:
        tag = _version_tag(project_root)
    if services is None:
        services = _runtime_services(project_root)
    print(f"=== Building {len(services)} per-service overlays ===")
    for svc in services:
        image = f"localhost/{svc}"
        _run(["podman", "build", "--format", "docker",
              "--build-arg", f"SERVICE_NAME={svc}",
              "--build-arg", f"BASE_TAG={tag}",
              "-t", f"{image}:local",
              "-t", f"{image}:{tag}",
              "-f", "docker/service-runtime.Dockerfile", "."],
             project_root)
    return tag


def build_nats(project_root: Path, tag: str | None = None) -> str:
    if tag is None:
        tag = _version_tag(project_root)
    print(f"=== Building NATS image: {IMAGE_NATS}:{tag} ===")
    _run(["podman", "build", "--format", "docker",
          "-t", f"{IMAGE_NATS}:local",
          "-t", f"{IMAGE_NATS}:{tag}",
          "-f", "docker/nats.Dockerfile", "."],
         project_root)
    return tag


def build_compute(project_root: Path, tag: str | None = None) -> str:
    if tag is None:
        tag = _version_tag(project_root)
    print(f"=== Building compute image: {IMAGE_COMPUTE}:{tag} ===")
    _run(["podman", "build", "--format", "docker",
          "-t", f"{IMAGE_COMPUTE}:local",
          "-t", f"{IMAGE_COMPUTE}:{tag}",
          "-f", "docker/service-compute.Dockerfile", "."],
         project_root)
    return tag


def build_all(project_root: Path, tag: str | None = None) -> str:
    """Stage, strip, compute ldd partition, build base + overlays + NATS."""
    if tag is None:
        tag = _version_tag(project_root)
    print(f"=== Version tag: {tag} ===")

    # 1. Stage binaries (also strips them on the host).
    print("=== Staging binaries ===")
    _run(["bash", "docker/stage-runtime.sh"], project_root)

    # 2. Compute ldd intersection + stage for build.
    from library_partition import (  # noqa: PLC0415
        compute_partition, stage_for_build,
    )
    stage_dir = project_root / "build" / "docker-stage"
    services = _runtime_services(project_root)
    print(f"  Runtime services: {len(services)} (from service registry)")

    print("=== Computing library partition (ldd intersection) ===")
    common_libs, service_extras = compute_partition(
        stage_dir / "bin", stage_dir / "lib", services)
    print(f"  Common libs (base image)  : {len(common_libs)}")
    svc_with_extras = sum(1 for v in service_extras.values() if v)
    print(f"  Services with extra libs   : {svc_with_extras} / {len(services)}")

    print("=== Staging for build (base + per-service dirs) ===")
    stage_for_build(stage_dir, stage_dir / "bin", stage_dir / "lib",
                    services, common_libs, service_extras)

    # 3. Build base (once).
    build_base(project_root, tag)

    # 4. Build per-service overlays.
    build_overlays(project_root, tag, services)

    # 5. NATS.
    build_nats(project_root, tag)

    return tag


# --- Image existence checks (used by env_deploy) --------------------------

def check_images_exist(project_root: Path, role: str = "runtime") -> bool:
    """Return True if all expected images for *role* are present locally.

    Prints a helpful message on failure telling the user what to run.
    """
    tag = _version_tag(project_root)
    missing = missing_images(project_root, role, tag)
    if not missing:
        return True
    print(f"error: {len(missing)} container image(s) not found locally:",
          file=sys.stderr)
    for m in missing:
        print(f"  - {m}", file=sys.stderr)
    print(file=sys.stderr)
    print("Run this first:", file=sys.stderr)
    if role == "compute":
        print("  compass image build --compute", file=sys.stderr)
    else:
        print("  compass image build", file=sys.stderr)
    return False


def missing_images(project_root: Path, role: str, tag: str) -> list[str]:
    """Return the list of image refs that should exist but don't."""
    import json as _json
    missing = []
    if role == "compute":
        for img in [f"{IMAGE_COMPUTE}:{tag}"]:
            if not _image_exists(img):
                missing.append(img)
    else:
        for img in [f"{IMAGE_BASE}:{tag}"]:
            if not _image_exists(img):
                missing.append(img)
        services = _runtime_services(project_root)
        for svc in services:
            img = f"localhost/{svc}:{tag}"
            if not _image_exists(img):
                missing.append(img)
        if not _image_exists(f"{IMAGE_NATS}:{tag}"):
            missing.append(f"{IMAGE_NATS}:{tag}")
    return missing


def _image_exists(ref: str) -> bool:
    result = subprocess.run(
        ["podman", "image", "exists", ref],
        capture_output=True, text=True)
    return result.returncode == 0


# --- CLI ------------------------------------------------------------------

def run(argv: list[str], project_root: Path) -> int:
    ap = argparse.ArgumentParser(
        prog="compass image build",
        description="Build ORE Studio container images "
                    "(base + per-service overlays).")
    ap.add_argument("--base-only", action="store_true",
                    help="Build only the shared base image")
    ap.add_argument("--overlays-only", action="store_true",
                    help="Build only per-service overlays "
                         "(requires --base-tag if not 'local')")
    ap.add_argument("--nats", action="store_true",
                    help="Build only the NATS image")
    ap.add_argument("--compute", action="store_true",
                    help="Build the compute-wrapper image")
    ap.add_argument("--base-tag", type=str, default=None,
                    help="Tag of the base image to build FROM "
                         "(default: auto-detected version tag)")
    ap.add_argument("--tag", type=str, default=None,
                    help="Override the version tag (default: auto-detect)")
    args = ap.parse_args(argv)

    tag = args.tag  # may be None → auto-detect

    # --compute is standalone — no staging, no library partition.
    if args.compute:
        _run(["bash", "docker/stage-runtime.sh", "--service",
              "ores.compute.wrapper"], project_root)
        build_compute(project_root, tag)
        return 0

    # --nats is standalone.
    if args.nats:
        build_nats(project_root, tag)
        return 0

    # --overlays-only requires the base to exist.
    if args.overlays_only:
        build_overlays(project_root, tag)
        return 0

    # --base-only: stage + strip + build just the base.
    if args.base_only:
        _run(["bash", "docker/stage-runtime.sh"], project_root)
        services = _runtime_services(project_root)
        from library_partition import (  # noqa: PLC0415
            compute_partition, stage_for_build,
        )
        stage_dir = project_root / "build" / "docker-stage"
        common_libs, service_extras = compute_partition(
            stage_dir / "bin", stage_dir / "lib", services)
        stage_for_build(stage_dir, stage_dir / "bin", stage_dir / "lib",
                        services, common_libs, service_extras)
        build_base(project_root, tag)
        return 0

    # Default: full build — base + overlays + NATS.
    try:
        build_all(project_root, tag)
    except subprocess.CalledProcessError as e:
        print(f"error: {e} (exit code {e.returncode})", file=sys.stderr)
        return 1
    except (RuntimeError, FileNotFoundError) as e:
        print(f"error: {e}", file=sys.stderr)
        return 1
    return 0
