"""compass image build — build the ORE Studio container image hierarchy.

Builds a base image, one shared-library image, and thin per-service
images, stripping binaries on the host before podman ever sees them.  A
separate step that ``compass env deploy`` requires to have been run first.

Image model::

    ores-service-base (once, tens of MB)
        Healthcheck probe and the empty log/run/storage dirs.
        docker/service-base.Dockerfile.

    ores-service-libs (once, ~the size of every library a service loads)
        Every shared library any service needs, so each library is stored
        and transferred once.  docker/service-libs.Dockerfile.

    Per-service images (N images, a few MB each)
        Service binary, its build record and the entrypoint symlink,
        FROM ores-service-libs.  docker/service-runtime.Dockerfile.

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
IMAGE_LIBS = "localhost/ores-service-libs"
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


def runtime_services(project_root: Path) -> list:
    """Enabled services that have a compiled binary to put in an image.

    Node services are excluded: they ship a bundle, not a binary, and have
    no image yet (systemd_generate applies the same split).
    """
    from systemd_generate import load_service_registry  # noqa: PLC0415
    raw = load_service_registry(project_root)
    return [s["name"] for s in raw
            if s.get("enabled")
            and s["name"] != "ores.compute.wrapper"
            and s.get("runtime", "native") != "node"]


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


def build_libs(project_root: Path, tag: str | None = None,
               base_tag: str | None = None) -> str:
    """Build the shared-library image.  Returns the tag.

    Requires the base image to already exist.  *base_tag* is the tag of
    the base image to build FROM; it defaults to *tag*.
    """
    if tag is None:
        tag = _version_tag(project_root)
    if base_tag is None:
        base_tag = tag
    print(f"=== Building library image: {IMAGE_LIBS}:{tag} ===")
    _run(["podman", "build", "--format", "docker",
          "--build-arg", f"BASE_TAG={base_tag}",
          "-t", f"{IMAGE_LIBS}:local",
          "-t", f"{IMAGE_LIBS}:{tag}",
          "-f", "docker/service-libs.Dockerfile", "."],
         project_root)
    return tag


def build_overlays(project_root: Path, tag: str | None = None,
                   services: list | None = None,
                   base_tag: str | None = None) -> str:
    """Build the per-service images (one per enabled service).

    Requires the shared-library image to already exist.
    *base_tag* is the tag of the library image to build FROM; it defaults
    to *tag* (images are normally built on the library image built in the
    same run).  Returns the tag used for the images themselves.
    """
    if tag is None:
        tag = _version_tag(project_root)
    if base_tag is None:
        base_tag = tag
    if services is None:
        services = runtime_services(project_root)
    print(f"=== Building {len(services)} per-service images "
          f"(FROM {IMAGE_LIBS}:{base_tag}) ===")
    for svc in services:
        image = f"localhost/{svc}"
        _run(["podman", "build", "--format", "docker",
              "--build-arg", f"SERVICE_NAME={svc}",
              "--build-arg", f"BASE_TAG={base_tag}",
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
    services = runtime_services(project_root)
    print("=== Staging binaries ===")
    _run(["bash", "docker/stage-runtime.sh", "--services", " ".join(services)],
         project_root)

    # 2. Compute the shared libraries and stage for build.
    from library_partition import (  # noqa: PLC0415
        compute_shared_libraries, stage_for_build,
    )
    stage_dir = project_root / "build" / "docker-stage"
    print(f"  Runtime services: {len(services)} (from service registry)")

    print("=== Computing the shared libraries (ldd union) ===")
    shared_libs = compute_shared_libraries(
        stage_dir / "bin", stage_dir / "lib", services)
    print(f"  Shared libs (library image): {len(shared_libs)}")

    print("=== Staging for build (library + per-service dirs) ===")
    stage_for_build(stage_dir, stage_dir / "bin", stage_dir / "lib",
                    services, shared_libs)

    # 3. Build base (once), then the libraries (once).
    build_base(project_root, tag)
    build_libs(project_root, tag)

    # 4. Build the per-service images.
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
        for img in [f"{IMAGE_BASE}:{tag}", f"{IMAGE_LIBS}:{tag}"]:
            if not _image_exists(img):
                missing.append(img)
        services = runtime_services(project_root)
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
                    "(base + shared libraries + per-service images).")
    ap.add_argument("--base-only", action="store_true",
                    help="Build only the shared base and library images")
    ap.add_argument("--overlays-only", action="store_true",
                    help="Build only the per-service images "
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
        build_overlays(project_root, tag, base_tag=args.base_tag)
        return 0

    # --base-only: stage + strip + build just the base and library images.
    if args.base_only:
        services = runtime_services(project_root)
        _run(["bash", "docker/stage-runtime.sh", "--services",
              " ".join(services)], project_root)
        from library_partition import (  # noqa: PLC0415
            compute_shared_libraries, stage_for_build,
        )
        stage_dir = project_root / "build" / "docker-stage"
        shared_libs = compute_shared_libraries(
            stage_dir / "bin", stage_dir / "lib", services)
        stage_for_build(stage_dir, stage_dir / "bin", stage_dir / "lib",
                        services, shared_libs)
        build_base(project_root, tag)
        build_libs(project_root, tag)
        return 0

    # Default: full build — base + libraries + services + NATS.
    try:
        build_all(project_root, tag)
    except subprocess.CalledProcessError as e:
        print(f"error: {e} (exit code {e.returncode})", file=sys.stderr)
        return 1
    except (RuntimeError, FileNotFoundError) as e:
        print(f"error: {e}", file=sys.stderr)
        return 1
    return 0
