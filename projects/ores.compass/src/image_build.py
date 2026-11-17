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
import os
import re
import subprocess
import sys
from pathlib import Path

# Image names (exported — env_deploy imports these).
IMAGE_BASE = "localhost/ores-service-base"
IMAGE_LIBS = "localhost/ores-service-libs"
IMAGE_NATS = "localhost/ores-nats"
IMAGE_COMPUTE = "localhost/ores-compute-wrapper"
IMAGE_WEB = "localhost/ores-web"
IMAGE_SHELL = "localhost/ores-shell"

# The compute wrapper is a supervised binary but not a runtime service: it
# ships its own image, yet it shares the library image with the services and
# therefore is staged with them.
COMPUTE_SERVICE = "ores.compute.wrapper"

# The shell is not a supervised service but it is shipped as an image, and it
# links clients no service links (compute.client among them), so its closure
# has to join the shared library image or the shell image cannot start.
SHELL_SERVICE = "ores.shell"


def _run(cmd, project_root, check=True, **kwargs):
    return subprocess.run(cmd, cwd=project_root, check=check, **kwargs)

# The engine the build and publish commands run. podman is the default
# because the checkout uses it; the CI runner has docker, so the nightly
# publish job selects that instead.
_ENGINE = "podman"


def set_engine(engine: str) -> None:
    """Select the container engine: "podman" or "docker"."""
    global _ENGINE
    if engine not in ("podman", "docker"):
        raise ValueError(f"unknown container engine: {engine}")
    _ENGINE = engine


def _build_args(args: list[str]) -> list[str]:
    """The engine's build command, followed by *args*.

    podman needs --format docker because the default OCI format silently
    drops the Dockerfile's HEALTHCHECK; docker keeps it by default and
    rejects the flag.
    """
    cmd = [_ENGINE, "build"]
    if _ENGINE == "podman":
        cmd += ["--format", "docker"]
    return cmd + args



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


def web_services(project_root: Path) -> list:
    """Enabled services that run on Node instead of a compiled binary.

    They ship a bundle, so their images are built from the compiled
    TypeScript rather than from publish/.
    """
    from systemd_generate import load_service_registry  # noqa: PLC0415
    raw = load_service_registry(project_root)
    return [s["name"] for s in raw
            if s.get("enabled") and s.get("runtime") == "node"]


def stage_web(project_root: Path) -> bool:
    """Stage the web service's built output. Returns False when it is not
    built, so a checkout that never built the bundle still stages the
    compiled services rather than failing outright.
    """
    if not web_services(project_root):
        return False
    print("=== Staging the web service ===")
    result = _run(["bash", "docker/stage-web.sh"], project_root, check=False)
    if result.returncode != 0:
        print("  warning: web bundle not built, skipping the web image")
        return False
    return True


def build_web(project_root: Path, tag: str | None = None) -> str | None:
    """Build the web image, if the bundle was staged. Returns its tag."""
    if not (project_root / "build" / "docker-stage" / "web").is_dir():
        return None
    if tag is None:
        tag = _version_tag(project_root)
    print(f"=== Building web image: {IMAGE_WEB}:{tag} ===")
    _run(_build_args([
          "-t", f"{IMAGE_WEB}:local",
          "-t", f"{IMAGE_WEB}:{tag}",
          "-f", "docker/service-web.Dockerfile", "."]),
         project_root)
    return tag


def build_base(project_root: Path, tag: str | None = None) -> str:
    """Build the shared base image.  Returns the tag."""
    if tag is None:
        tag = _version_tag(project_root)
    print(f"=== Building base image: {IMAGE_BASE}:{tag} ===")
    _run(_build_args([
          "-t", f"{IMAGE_BASE}:local",
          "-t", f"{IMAGE_BASE}:{tag}",
          "-f", "docker/service-base.Dockerfile", "."]),
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
    _run(_build_args([
          "--build-arg", f"BASE_TAG={base_tag}",
          "-t", f"{IMAGE_LIBS}:local",
          "-t", f"{IMAGE_LIBS}:{tag}",
          "-f", "docker/service-libs.Dockerfile", "."]),
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
        _run(_build_args([
              "--build-arg", f"SERVICE_NAME={svc}",
              "--build-arg", f"BASE_TAG={base_tag}",
              "-t", f"{image}:local",
              "-t", f"{image}:{tag}",
              "-f", "docker/service-runtime.Dockerfile", "."]),
             project_root)
    return tag


def build_nats(project_root: Path, tag: str | None = None) -> str:
    if tag is None:
        tag = _version_tag(project_root)
    print(f"=== Building NATS image: {IMAGE_NATS}:{tag} ===")
    _run(_build_args([
          "-t", f"{IMAGE_NATS}:local",
          "-t", f"{IMAGE_NATS}:{tag}",
          "-f", "docker/nats.Dockerfile", "."]),
         project_root)
    return tag


def build_compute(project_root: Path, tag: str | None = None,
                  base_tag: str | None = None) -> str:
    """Build the compute-wrapper image. Requires the library image."""
    if tag is None:
        tag = _version_tag(project_root)
    if base_tag is None:
        base_tag = tag
    print(f"=== Building compute image: {IMAGE_COMPUTE}:{tag} ===")
    _run(_build_args([
          "--build-arg", f"SERVICE_NAME={COMPUTE_SERVICE}",
          "--build-arg", f"BASE_TAG={base_tag}",
          "-t", f"{IMAGE_COMPUTE}:local",
          "-t", f"{IMAGE_COMPUTE}:{tag}",
          "-f", "docker/service-compute.Dockerfile", "."]),
         project_root)
    return tag


def build_all(project_root: Path, tag: str | None = None,
              skip_stage: bool = False) -> str:
    """Stage, strip, compute ldd partition, build base + overlays + NATS."""
    if tag is None:
        tag = _version_tag(project_root)
    print(f"=== Version tag: {tag} ===")

    services = (runtime_services(project_root) if skip_stage
                else stage_only(project_root))

    # 3. Build base (once), then the libraries (once).
    build_base(project_root, tag)
    build_libs(project_root, tag)

    # 4. Build the compute image and the per-service images.
    build_compute(project_root, tag)
    build_overlays(project_root, tag, services)

    # 5. The web service, when its bundle was staged.
    build_web(project_root, tag)

    # 6. NATS.
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
        if web_services(project_root) and not _image_exists(
                f"{IMAGE_WEB}:{tag}"):
            missing.append(f"{IMAGE_WEB}:{tag}")
    return missing


def _image_exists(ref: str) -> bool:
    result = subprocess.run(
        [_ENGINE, "image", "exists", ref],
        capture_output=True, text=True)
    return result.returncode == 0



def stage_only(project_root: Path) -> list[str]:
    """Stage the binaries and libraries for the image builds. Returns the
    service list.

    The CI build runs inside the CI container, which has no container
    engine: it stages, hands the staged tree to a job that has an engine,
    and that job builds the images without staging again.
    """
    services = runtime_services(project_root)
    staged = services + [COMPUTE_SERVICE, SHELL_SERVICE]
    print("=== Staging binaries ===")
    _run(["bash", "docker/stage-runtime.sh", "--services", " ".join(staged)],
         project_root)
    from library_partition import (  # noqa: PLC0415
        compute_shared_libraries, stage_for_build,
    )
    stage_dir = project_root / "build" / "docker-stage"
    print(f"  Runtime services: {len(services)} (from service registry)")
    print("=== Computing the shared libraries (ldd union) ===")
    # The wrapper is staged with the services, so its libraries belong in the
    # shared image too: its image is the library image plus that binary.
    shared_libs = compute_shared_libraries(
        stage_dir / "bin", stage_dir / "lib", staged)
    print(f"  Shared libs (library image): {len(shared_libs)}")
    print("=== Staging for build (library + per-service dirs) ===")
    stage_for_build(stage_dir, stage_dir / "bin", stage_dir / "lib",
                    staged, shared_libs)
    stage_web(project_root)
    return services


def image_names(project_root: Path) -> list[str]:
    """Repository names of every image the runtime role runs.

    The registry name of an image is its local name without the
    localhost/ prefix, so a service called ores.iam.service is published as
    ores.iam.service.
    """
    names = ["ores-service-base", "ores-service-libs", "ores-nats",
             "ores-compute-wrapper", "ores-web", "ores-shell"]
    names += runtime_services(project_root)
    return names


def _published(ref: str) -> bool:
    """Whether the registry already holds *ref*.

    A tag that exists is never rebuilt, so a rerun after a partial push
    only fills the gaps and a nightly run that publishes the same commit
    twice costs nothing.
    """
    result = subprocess.run(
        [_ENGINE, "manifest", "inspect", ref],
        capture_output=True, text=True)
    return result.returncode == 0


def publish(project_root: Path, registry: str, tag: str | None = None,
            alias: str | None = None, dry_run: bool = False,
            force: bool = False) -> int:
    """Push every image to *registry* under *tag*, and under *alias* too.

    *registry* is a repository prefix such as ghcr.io/orestudio. The images
    must have been built locally for *tag* already. Returns the number of
    images pushed.
    """
    if tag is None:
        tag = _version_tag(project_root)
    pushed = 0
    skipped = 0
    for name in image_names(project_root):
        ref = f"{registry}/{name}:{tag}"
        if dry_run:
            print(f"  would publish {ref}" + (f" and {registry}/{name}:{alias}"
                                              if alias else ""))
            pushed += 1
            continue
        if not _image_exists(f"localhost/{name}:{tag}"):
            print(f"  {name} is not built locally, skipping it")
            skipped += 1
            continue
        if not force and _published(ref):
            print(f"  {ref} is already published")
            skipped += 1
            continue
        _run([_ENGINE, "tag", f"localhost/{name}:{tag}", ref], project_root)
        print(f"=== Pushing {ref} ===")
        _run([_ENGINE, "push", ref], project_root)
        if alias:
            alias_ref = f"{registry}/{name}:{alias}"
            _run([_ENGINE, "tag", ref, alias_ref], project_root)
            _run([_ENGINE, "push", alias_ref], project_root)
        pushed += 1
    print(f"Published {pushed} image(s); {skipped} were already present.")
    return pushed


# --- CLI ------------------------------------------------------------------

def run(argv: list[str], project_root: Path) -> int:
    command = "build"
    if argv and argv[0] in ("build", "stage", "publish"):
        command = argv[0]
        argv = argv[1:]

    ap = argparse.ArgumentParser(
        prog=f"compass image {command}",
        description="Build, stage, and publish the ORE Studio container "
                    "images (base + shared libraries + per-service images).")
    ap.add_argument("--engine", choices=("podman", "docker"), default="podman",
                    help="Container engine (default: podman)")
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
    ap.add_argument("--from-stage", action="store_true",
                    help="Build from an existing build/docker-stage tree "
                         "instead of staging again")
    ap.add_argument("--registry", type=str, default=None,
                    help="Registry prefix to publish to, e.g. "
                         "ghcr.io/orestudio (publish only)")
    ap.add_argument("--alias", type=str, default=None,
                    help="Extra tag to publish each image under (publish only)")
    ap.add_argument("--preset", type=str, default=None,
                    help="Build preset to stage from, e.g. "
                         "linux-clang-release-ninja (default: ORES_PRESET)")
    ap.add_argument("--print-tag", action="store_true",
                    help="Print the version-commit tag and exit")
    ap.add_argument("--dry-run", action="store_true",
                    help="Report what publish would push, and push nothing")
    ap.add_argument("--force", action="store_true",
                    help="Push a tag the registry already holds (after "
                         "rebuilding an image in place)")
    args = ap.parse_args(argv)

    # The caller's preset wins over the one in .env, so a lambda can build a
    # configuration the checkout is not on.
    if args.preset:
        os.environ["ORES_PRESET"] = args.preset
    if args.print_tag:
        print(_version_tag(project_root))
        return 0
    set_engine(args.engine)
    tag = args.tag  # may be None → auto-detect

    try:
        if command == "stage":
            stage_only(project_root)
            return 0

        if command == "publish":
            if not args.registry:
                print("error: publish needs --registry", file=sys.stderr)
                return 2
            if not args.dry_run:
                build_all(project_root, tag, skip_stage=args.from_stage)
            publish(project_root, args.registry, tag, args.alias,
                    dry_run=args.dry_run, force=args.force)
            return 0

        # --compute still needs the shared stage: its image is the library
        # image plus the wrapper binary.
        if args.compute:
            stage_only(project_root)
            build_base(project_root, tag)
            build_libs(project_root, tag)
            build_compute(project_root, tag)
            return 0

        # --nats is standalone.
        if args.nats:
            build_nats(project_root, tag)
            return 0

        # --overlays-only requires the library image to exist.
        if args.overlays_only:
            build_overlays(project_root, tag, base_tag=args.base_tag)
            return 0

        # --base-only: build the two shared images only.
        if args.base_only:
            stage_only(project_root)
            build_base(project_root, tag)
            build_libs(project_root, tag)
            return 0

        # Default: full build — base + libraries + services + NATS.
        build_all(project_root, tag, skip_stage=args.from_stage)
    except subprocess.CalledProcessError as e:
        print(f"error: {e} (exit code {e.returncode})", file=sys.stderr)
        return 1
    except (RuntimeError, FileNotFoundError) as e:
        print(f"error: {e}", file=sys.stderr)
        return 1
    return 0
