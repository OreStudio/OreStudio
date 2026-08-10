"""compass env deploy — build, stage, transfer, and deploy the ORE Studio
service runtime (or a compute node) to a named remote WSL host over SSH.

Replaces the manual podman build / save / ssh ... podman load / remote
run-pod.sh sequence used to deploy to Newton in sprint 24 (see the
offload-services-to-wsl-host story). Two deployment roles:

- runtime (default): the full service-runtime + NATS containers, run
  with --network=host (Postgres is native on the WSL host).
- compute: a *partial* environment — just the ores.compute.wrapper
  compute node, connecting outward to the serving environment's NATS
  core. No NATS server, no Postgres, no service set.

Image architecture
------------------
Instead of building N near-identical ~2.5 GB images (one per service,
differing only in the entrypoint symlink), the runtime role uses a
base + overlay model::

    ores-service-base (once, ~1.5 GB on wire)
        Common shared libraries (ldd intersection across all services),
        the healthcheck probe, and empty log/run/storage directories.
        Built from docker/service-base.Dockerfile.

    Per-service overlays (N images, ~MB each on wire)
        Just the service binary, its service-specific extra shared libs
        (if any), and an entrypoint symlink.  FROM ores-service-base.
        Built from docker/service-runtime.Dockerfile.

Binaries are stripped on the host (by stage-runtime.sh) BEFORE podman
sees them — no in-container strip stage, no redundant COPY of
unstripped data into the build context.

Transfer: the base image is saved/gzipped/scp'd/loaded once; per-service
overlays are scp'd as raw files and built remotely on top of the
already-loaded base.  The old `podman save | gzip | ssh load` pipeline
(which buffered heavily and could OOM) is replaced with discrete serial
steps (save to disk → gzip → scp → remote load).

Hosts are registered as named env files, .env.<host> at the repo root —
the host-registry concept compass already has (`compass ... --env
<host>` resolves to the same file, see how-do-i-use-named-env-files).
The profile is derived from docker/.env and gitignored (it carries the
same secrets). The remote-side run/stop logic lives in
docker/remote-run.sh and docker/remote-stop.sh, piped to the host via
`ssh <host> bash -s`.
"""

import argparse
import re
import shlex
import subprocess
import sys
from pathlib import Path

# Defaults used when the per-host profile does not override them.
DEFAULT_REMOTE_ROOT = "~/ores-deploy"
DEFAULT_REMOTE_DB_PORT = "5433"

# Image names — keep in sync with image_build.py (single source of truth).
IMAGE_NATS = "localhost/ores-nats"
IMAGE_COMPUTE = "localhost/ores-compute-wrapper"
IMAGE_BASE = "localhost/ores-service-base"

_COMPUTE_REQUIRED = (
    "ORES_COMPUTE_HOST_ID",
    "ORES_COMPUTE_TENANT_ID",
    "ORES_COMPUTE_NATS_URL",
    "ORES_COMPUTE_NATS_SUBJECT_PREFIX",
    "ORES_COMPUTE_NATS_WIRE_FORMAT",
    "ORES_COMPUTE_NATS_TLS_CA",
    "ORES_COMPUTE_NATS_TLS_CERT",
    "ORES_COMPUTE_NATS_TLS_KEY",
)


def _read_env(path: Path) -> dict:
    """Parse KEY=VALUE lines, stripping one pair of surrounding double
    quotes (docker/.env uses KEY="value" for values with escaped \n)."""
    result = {}
    try:
        text = path.read_text(encoding="utf-8")
    except FileNotFoundError:
        return result
    for line in text.splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        key = key.strip()
        if not re.match(r"^[A-Za-z_][A-Za-z0-9_]*$", key):
            continue
        value = value.strip()
        if len(value) >= 2 and value.startswith('"') and value.endswith('"'):
            value = value[1:-1]
        result[key] = value
    return result


def _run(cmd, project_root, check=True, **kwargs):
    """Run a local command from the project root, inheriting stdio."""
    return subprocess.run(cmd, cwd=project_root, check=check, **kwargs)


def _ssh(host, command, input_text=None, check=False, capture=True,
         **kwargs):
    """Run a command on the remote host; returns the CompletedProcess.

    Queries (HOME, reachability) capture output; script executions pass
    capture=False so their progress streams to the terminal.
    """
    if capture:
        kwargs.setdefault("capture_output", True)
    return subprocess.run(
        ["ssh", host, command],
        input=input_text,
        text=True,
        check=check,
        **kwargs,
    )


def _remote_script(host, script_text, env_vars):
    """Pipe a script to the remote host via `ssh <host> bash -s`, with
    the given variables as inline assignments on the command line."""
    assignments = " ".join(
        f"{k}={shlex.quote(str(v))}" for k, v in env_vars.items())
    print(f"  Piping remote script ({len(script_text)} bytes) to {host}")
    if env_vars:
        print(f"    Env vars: {' '.join(sorted(env_vars.keys()))}")
    cmd = f"{assignments} bash -s" if assignments else "bash -s"
    return _ssh(host, cmd, input_text=script_text, check=True,
                capture=False)


def _remote_root(host, root_value):
    """Expand a ~-prefixed remote root to the remote HOME (queried)."""
    if not root_value.startswith("~"):
        return root_value
    result = _ssh(host, "printf %s \"$HOME\"")
    home = (result.stdout or "").strip()
    if not home:
        raise RuntimeError(f"cannot resolve remote HOME via ssh {host}")
    return home + root_value[1:]


def _profile_path(project_root, host):
    # The named-env-file registry: `compass ... --env <host>` resolves to
    # the same file (see compass.py's _resolve_env_file).
    return project_root / f".env.{host}"


def _label(env: dict, project_root: Path) -> str:
    return env.get("ORES_CHECKOUT_LABEL", project_root.name)


def _version_tag(project_root: Path) -> str:
    """Return a short version-commit tag for container images (e.g.
    '0.0.25-ca621ac6d'). The version is read from CMakeLists.txt; the
    commit from `git rev-parse --short HEAD`."""
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
    """Return the list of enabled runtime service names from the service
    registry (the same model consumed by systemd_generate)."""
    from systemd_generate import load_service_registry  # noqa: PLC0415
    raw = load_service_registry(project_root)
    return [s["name"] for s in raw if s.get("enabled") and s["name"] != "ores.compute.wrapper"]


# --- Profile generation ---------------------------------------------------

def _strip_quotes(line: str) -> str:
    m = re.match(r'^([A-Za-z_][A-Za-z0-9_]*)=(.*)$', line)
    if not m:
        return line
    key, value = m.group(1), m.group(2)
    if len(value) >= 2 and value.startswith('"') and value.endswith('"'):
        inner = value[1:-1]
        if re.search(r"\s", inner):
            # Keep the quotes when the inner value contains whitespace: the
            # profile doubles as a bash-sourced env file on the remote
            # (remote-run.sh `source`s it), and an unquoted value with
            # spaces would be executed as a command ("PRIVATE: command not
            # found"). podman's --env-file passes the quotes through
            # literally, but the only such value in practice — the IAM JWT
            # PEM — is overridden via --env from the real PEM file on the
            # remote side, so keeping the quotes is safe for both consumers.
            return line
        return f"{key}={inner}"
    # If the value is unquoted but contains whitespace (e.g. the JWT PEM
    # in docker/.env, whose header "-----BEGIN PRIVATE KEY-----" has
    # spaces), add quotes so that bash sourcing does not interpret the
    # spaces as command separators.
    if re.search(r"\s", value):
        return f'{key}="{value}"'
    return line


def _generate_profile(project_root, profile_path, src_env_path, host,
                      remote_root, remote_db_port):
    """Derive the .env.<host> profile from docker/.env using the
    sprint-24 Newton session's rewrites: every *_DB_PORT matching the
    local ORES_DB_PORT becomes the remote port (Postgres is native on
    the WSL host at localhost:5433), and every absolute path under the
    checkout becomes $REMOTE_ROOT (the WSL host has no checkout of this
    repo). The remote configuration block is appended with the values
    in effect, so the file is inspectable/editable and self-contained —
    and usable by every other command via `--env <host>`."""
    src = src_env_path.read_text(encoding="utf-8")
    local_db_port = _read_env(src_env_path).get("ORES_DB_PORT", "5432")
    project_root_s = str(project_root)

    out = []
    for line in src.splitlines():
        line = _strip_quotes(line)
        if "=" in line and not line.startswith("#"):
            key, _, value = line.partition("=")
            if ((key == "PGPORT" or key.endswith("_DB_PORT"))
                    and value == local_db_port
                    and remote_db_port != local_db_port):
                line = f"{key}={remote_db_port}"
        if project_root_s in line:
            line = line.replace(project_root_s, remote_root)
        out.append(line)

    # PGPORT is libpq's own env var (psql, and every libpq-based client,
    # read it natively); the source .env only carries it when the DB
    # already lives off 5432 (see env_init.py). The remote profile must
    # carry it whenever the remote Postgres is off the default port —
    # this is what makes `compass db recreate -y -k --env <host>` target
    # the remote DB (the sprint-24 session added PGPORT=5433 by hand).
    if remote_db_port != "5432" and not any(
            line.startswith("PGPORT=") for line in out):
        out.append(f"PGPORT={remote_db_port}")

    out.extend([
        "",
        "# ---------------------------------------------------------------------------",
        "# Remote host configuration (read by `compass env deploy <host>`)",
        "# ---------------------------------------------------------------------------",
        f"ORES_REMOTE_HOST={host}",
        f"ORES_REMOTE_ROOT={remote_root}",
        f"ORES_REMOTE_DB_PORT={remote_db_port}",
    ])
    profile_path.write_text("\n".join(out) + "\n", encoding="utf-8")
    print(f"  Wrote host profile: {profile_path}")
    print("  (inspect and adjust ORES_REMOTE_HOST / ORES_REMOTE_ROOT before re-running)")


# --- Compute-node config --------------------------------------------------

def _write_compute_env(project_root, profile_env, label, remote_root):
    """Build the compute node's env file (app-prefixed
    ORES_COMPUTE_WRAPPER_NATS_* block — the wrapper parser reads its env
    via make_mapper("COMPUTE_WRAPPER"), and the TLS trio is deliberately
    not in the shared-domain fallback, so it must be app-prefixed) and
    stage the serving environment's client certs. Returns (env_path,
    keys_stage_dir)."""
    stage = project_root / "build" / "docker-stage"
    stage.mkdir(parents=True, exist_ok=True)

    # Named compute/keys (not compute-keys) so a recursive scp of the
    # stage dir into the remote compute/ parent creates exactly the
    # compute/keys path that compute.env and remote-run.sh reference.
    keys_stage = stage / "compute" / "keys"
    keys_stage.mkdir(parents=True, exist_ok=True)
    tls_bases = {}
    for tls in ("CA", "CERT", "KEY"):
        src = Path(profile_env[f"ORES_COMPUTE_NATS_TLS_{tls}"])
        if not src.is_file():
            raise RuntimeError(
                f"compute TLS file not found: {src} "
                "(set ORES_COMPUTE_NATS_TLS_{tls} in the host profile)")
        base = src.name
        (keys_stage / base).write_bytes(src.read_bytes())
        tls_bases[tls] = base

    env_path = stage / "compute.env"
    lines = [
        f"ORES_COMPUTE_WRAPPER_NATS_URL={profile_env['ORES_COMPUTE_NATS_URL']}",
        f"ORES_COMPUTE_WRAPPER_NATS_SUBJECT_PREFIX={profile_env['ORES_COMPUTE_NATS_SUBJECT_PREFIX']}",
        f"ORES_COMPUTE_WRAPPER_NATS_WIRE_FORMAT={profile_env['ORES_COMPUTE_NATS_WIRE_FORMAT']}",
        f"ORES_COMPUTE_WRAPPER_NATS_TLS_CA={remote_root}/compute/keys/{tls_bases['CA']}",
        f"ORES_COMPUTE_WRAPPER_NATS_TLS_CERT={remote_root}/compute/keys/{tls_bases['CERT']}",
        f"ORES_COMPUTE_WRAPPER_NATS_TLS_KEY={remote_root}/compute/keys/{tls_bases['KEY']}",
        f"ORES_COMPUTE_WRAPPER_HOST_ID={profile_env['ORES_COMPUTE_HOST_ID']}",
        f"ORES_COMPUTE_WRAPPER_TENANT_ID={profile_env['ORES_COMPUTE_TENANT_ID']}",
        f"ORES_COMPUTE_LABEL={label}",
    ]
    if profile_env.get("ORES_COMPUTE_HTTP_BASE_URL"):
        lines.append(
            f"ORES_COMPUTE_WRAPPER_HTTP_BASE_URL={profile_env['ORES_COMPUTE_HTTP_BASE_URL']}")
    env_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(f"  Wrote compute env: {env_path}")
    return env_path, keys_stage


# --- Transfer helpers -----------------------------------------------------

def _stream_images(project_root, host, images):
    """Transfer images to the remote host.

    Two-phase strategy (base + overlay model):
    1. Base image: save → gzip → scp → remote load (once, large).
    2. Per-service overlays: scp staging dirs + build remotely on top of
       the already-loaded base (each overlay is just the binary — a few
       MB — not a full image tarball).
    """
    stage = project_root / "build" / "docker-stage" / "transfer"
    stage.mkdir(parents=True, exist_ok=True)
    remote_tmp = f"/tmp/ores-deploy-{host}"

    # Separate the base image from per-service overlays and standalone
    # images (NATS, compute-wrapper — not part of the overlay system).
    base_images = [i for i in images if "ores-service-base" in i]
    nats_images = [i for i in images if "ores-nats" in i]
    compute_images = [i for i in images if "ores-compute-wrapper" in i]
    # Everything else is a per-service overlay (localhost/<service_name>).
    overlay_images = [i for i in images
                      if i not in base_images + nats_images + compute_images]

    # -- Phase 1: Transfer base image (once) ------------------------------
    for img in base_images:
        _transfer_one_image(project_root, host, img, stage, remote_tmp)

    # -- Phase 2: Transfer standalone images (NATS, compute) --------------
    for img in nats_images + compute_images:
        _transfer_one_image(project_root, host, img, stage, remote_tmp)

    # -- Phase 3: Remote-build per-service overlays -----------------------
    if overlay_images:
        tag = _version_tag(project_root)
        services_stage = project_root / "build" / "docker-stage" / "services"
        dlf = project_root / "docker" / "service-runtime.Dockerfile"

        # Mirror the local directory layout on the remote so the
        # Dockerfile's relative COPY paths resolve correctly:
        #   build/docker-stage/services/<svc>/bin/…
        remote_build_context = f"{remote_tmp}/build-context"
        remote_services_dir = f"{remote_build_context}/build/docker-stage/services"

        print(f"  Transferring per-service staging tree to {host} ...")
        _ssh(host, f"mkdir -p {shlex.quote(remote_services_dir)}", check=True)
        _scp(project_root, host, services_stage,
             f"{remote_build_context}/build/docker-stage/", recursive=True)
        _ssh(host, f"mkdir -p {shlex.quote(remote_build_context + '/docker')}",
             check=True)
        _scp(project_root, host, dlf,
             f"{remote_build_context}/docker/service-runtime.Dockerfile")

        # Build each overlay remotely — FROM the already-loaded base image.
        print(f"  Building {len(overlay_images)} overlays remotely on {host} ...")
        svc_names = " ".join(
            img.split("/")[-1].split(":")[0] for img in overlay_images)
        build_script = (
            f"set -euo pipefail; "
            f"DOCKERFILE={shlex.quote(remote_build_context + '/docker/service-runtime.Dockerfile')}; "
            f"CTX={shlex.quote(remote_build_context)}; "
            f"TAG={shlex.quote(tag)}; "
            f"echo 'Building per-service overlays...'; "
            f"cd \"$CTX\"; "
            f"for svc in {svc_names}; do "
            f"  echo \"  $svc\"; "
            f"  podman build --format docker "
            f"    --build-arg SERVICE_NAME=\"$svc\" "
            f"    --build-arg BASE_TAG=\"$TAG\" "
            f"    -t \"localhost/$svc:$TAG\" "
            f"    -t \"localhost/$svc:local\" "
            f"    -f \"$DOCKERFILE\" .; "
            f"done; "
            f"echo 'Overlay builds complete.'"
        )
        _ssh(host, f"bash -c {shlex.quote(build_script)}",
             check=True, capture=False)
        # Clean up the remote staging area.
        _ssh(host, f"rm -rf {shlex.quote(remote_build_context)}", check=True)


def _transfer_one_image(project_root, host, img, stage, remote_tmp):
    """Transfer a single image to the remote host via discrete serial
    steps (save → compress → scp → remote-load → cleanup)."""
    print(f"  Transferring image: {img}")
    safe_name = img.replace("/", "_").replace(":", "-")
    tar_path = stage / f"{safe_name}.tar"
    gz_path = stage / f"{safe_name}.tar.gz"

    # 1. Save to a local tarball (disk, not pipe — avoids OOM).
    print(f"    Saving to {tar_path.name} ...")
    _run(["podman", "save", "-o", str(tar_path), img], project_root)
    sz = tar_path.stat().st_size
    print(f"    Saved: {sz / 1024 / 1024:.0f} MiB")

    # 2. Compress (gzip -1 = fastest, ~70-80% ratio on container layers).
    print(f"    Compressing ...")
    _run(["gzip", "-1", "-f", str(tar_path)], project_root)
    gz_sz = gz_path.stat().st_size
    print(f"    Compressed: {gz_sz / 1024 / 1024:.0f} MiB "
          f"({gz_sz / sz * 100:.0f}% of original)")

    # 3. Transfer via scp (its own flow control, no pipe).
    _ssh(host, f"mkdir -p {shlex.quote(remote_tmp)}", check=True)
    _scp(project_root, host, gz_path, remote_tmp)

    # 4. Load on the remote host from disk.
    print(f"    Loading on {host} ...")
    remote_gz = f"{remote_tmp}/{gz_path.name}"
    _ssh(host, f"podman load -i {shlex.quote(remote_gz)}",
         check=True, capture=False)
    _ssh(host, f"rm -f {shlex.quote(remote_gz)}", check=True)

    # 5. Clean up local staging file.
    gz_path.unlink()
    print(f"    Transfer complete: {img}")


def _scp(project_root, host, local, remote, recursive=False):
    print(f"  scp: {local} -> {host}:{remote}")
    flags = "-q" + (" -r" if recursive else "")
    _run(["scp", *flags.split(), str(local), f"{host}:{remote}"],
         project_root, check=True)
    print(f"    scp done: {local}")


# --- Pipeline stages ------------------------------------------------------

def _preflight(project_root, profile_path, role, env):
    if not (project_root / ".env").is_file():
        raise RuntimeError(".env not found — run: compass env configure")
    if not (project_root / "docker" / ".env").is_file():
        print("=== Generating docker/.env ===")
        _run(["bash", "docker/generate-env.sh"], project_root)
    if role == "runtime":
        label = _label(env, project_root)
        nats_conf = project_root / "build" / "config" / f"nats-{label}.conf"
        if not nats_conf.is_file():
            raise RuntimeError(
                f"{nats_conf} not found — run: compass nats init")
        publish = project_root / "build" / "output" / "linux-clang-debug-make" / "publish"
        if not publish.is_dir():
            raise RuntimeError(
                f"{publish} not found — run: compass build first")
        keys_dir = project_root / "build" / "keys" / "nats"
        if not keys_dir.is_dir():
            raise RuntimeError(
                f"{keys_dir} not found — run: compass env configure "
                "(generates the NATS certs)")


def _transfer_runtime(project_root, host, remote_root, profile_path,
                      label):
    print("=== Transferring to the remote host ===")
    keys_dir = project_root / "build" / "keys" / "nats"
    jwt_key = project_root / "build" / "keys" / "iam-rsa-private.pem"
    nats_conf = project_root / "build" / "config" / f"nats-{label}.conf"

    # Render a path-rewritten copy of the NATS config for the remote
    # (the remote has no checkout; every /mnt/development/... path in it
    # must point at $REMOTE_ROOT).
    stage_conf = project_root / "build" / "docker-stage" / f"nats-{label}.conf"
    stage_conf.parent.mkdir(parents=True, exist_ok=True)
    rewritten = nats_conf.read_text(encoding="utf-8").replace(
        str(project_root), remote_root)
    stage_conf.write_text(rewritten, encoding="utf-8")

    # scp -r <dir> <existing-target> creates <target>/<dir> (nesting) rather
    # than merging contents. Remove the remote keys/nats directory and let
    # scp re-create it by targeting its parent directory.
    _ssh(host, f"rm -rf {shlex.quote(remote_root)}/build/keys/nats "
               f"&& mkdir -p {shlex.quote(remote_root)}/build/keys "
               f"{shlex.quote(remote_root)}/build/config "
               f"{shlex.quote(remote_root)}/docker", check=True)
    _scp(project_root, host, keys_dir, f"{remote_root}/build/keys/",
         recursive=True)
    if jwt_key.is_file():
        _scp(project_root, host, jwt_key, f"{remote_root}/build/keys/")
    else:
        print("  Warning: iam-rsa-private.pem not found — services will "
              "start without a JWT signing key")
    _scp(project_root, host, stage_conf,
         f"{remote_root}/build/config/nats-{label}.conf")
    _scp(project_root, host, profile_path, f"{remote_root}/docker/.env")


def _transfer_compute(project_root, host, remote_root, env_path, keys_stage):
    print("=== Transferring to the remote host ===")
    # scp -r <dir> <existing-target> creates <target>/<dir> (nesting) rather
    # than merging contents — the same trap _transfer_runtime avoids. Remove
    # the remote compute/keys directory and let scp re-create it by targeting
    # its parent (compute.env references compute/keys/<file> directly).
    _ssh(host, f"rm -rf {shlex.quote(remote_root)}/compute/keys "
               f"&& mkdir -p {shlex.quote(remote_root)}/compute", check=True)
    _scp(project_root, host, keys_stage, f"{remote_root}/compute/",
         recursive=True)
    _scp(project_root, host, env_path, f"{remote_root}/compute/compute.env")


def _deploy_runtime(project_root, host, profile_env, remote_root):
    label = _label(profile_env, project_root)
    tag = _version_tag(project_root)
    services = _runtime_services(project_root)
    print(f"=== Deploying runtime to {host} (label '{label}', tag '{tag}') ===")
    print(f"  Services: {len(services)}")
    # Space-separated list for remote-run.sh to loop over.
    services_arg = " ".join(services)
    script = (project_root / "docker" / "remote-run.sh").read_text(
        encoding="utf-8")
    _remote_script(host, script,
                   {"REMOTE_ROOT": remote_root, "ROLE": "runtime",
                    "IMAGE_TAG": tag, "SERVICES": services_arg})


def _deploy_compute(project_root, host, profile_env, remote_root):
    tag = _version_tag(project_root)
    print(f"=== Deploying compute node to {host} (tag '{tag}') ===")
    script = (project_root / "docker" / "remote-run.sh").read_text(
        encoding="utf-8")
    _remote_script(host, script,
                   {"REMOTE_ROOT": remote_root, "ROLE": "compute",
                    "IMAGE_TAG": tag})


def _stop(project_root, host, profile_env, remote_root, role, purge):
    script = (project_root / "docker" / "remote-stop.sh").read_text(
        encoding="utf-8")
    env_vars = {"REMOTE_ROOT": remote_root, "ROLE": role}
    if purge:
        env_vars["PURGE"] = "1"
    _remote_script(host, script, env_vars)


def _missing_compute_keys(profile_env: dict) -> list:
    """Compute-profile keys that must be present for --role compute."""
    return [k for k in _COMPUTE_REQUIRED if not profile_env.get(k)]


def _setup_host(project_root, host):
    script = """set -euo pipefail
export XDG_RUNTIME_DIR="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
if ! command -v podman >/dev/null 2>&1; then
    echo "Error: podman is not installed on the remote host." >&2
    echo "Install it first, e.g. (Debian/Ubuntu WSL):  sudo apt install podman" >&2
    exit 1
fi
echo "=== podman on remote ==="
podman --version
echo "=== enabling linger (keeps the user session alive for rootless podman) ==="
loginctl enable-linger "$(id -un)" 2>/dev/null || echo "  (enable-linger skipped — no permission; run it yourself if services die after logout)"
echo "=== starting user dbus socket (sprint-24 Newton finding) ==="
systemctl --user start dbus.socket 2>/dev/null || true
echo "Done — host is ready for \\`compass env deploy <host>\\`."
"""
    _ssh(host, "bash -s", input_text=script, check=True, capture=False)


# --- Commands -------------------------------------------------------------

def _cmd_list(project_root):
    profiles = sorted(p for p in project_root.glob(".env.*")
                      if p.name not in (".env.old",) and not p.name.endswith(".old"))
    if not profiles:
        print("No host profiles found (.env.<host> at the repo root).")
        print("Deploy to a host to generate its profile: compass env deploy <host>")
        return 0
    print("Host profiles (also targetable via `compass ... --env <host>`):")
    for p in profiles:
        env = _read_env(p)
        alias = p.name[len(".env."):]
        host = env.get("ORES_REMOTE_HOST", "(alias)")
        root = env.get("ORES_REMOTE_ROOT", DEFAULT_REMOTE_ROOT)
        print(f"  {alias:<16} host={host:<24} root={root}")
    return 0


def _cmd_deploy(argv, project_root):
    ap = argparse.ArgumentParser(
        prog="compass env deploy <host>",
        description="Build, stage, transfer, and deploy the service "
                    "runtime (or a compute node) to a remote WSL host "
                    "over SSH. Idempotent: re-running safely picks up a "
                    "new build without manual cleanup.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    ap.add_argument("host", help="Host alias (docker/.env.<host> profile)")
    ap.add_argument("--role", choices=("runtime", "compute"),
                    default="runtime",
                    help="runtime = full service/DB environment; "
                         "compute = partial environment with just the "
                         "ores.compute.wrapper node")
    ap.add_argument("--stop", action="store_true",
                    help="Stop the remote containers instead of deploying")
    ap.add_argument("--purge", action="store_true",
                    help="With --stop: also remove the staged certs volume")
    ap.add_argument("--setup-host", action="store_true",
                    help="One-time remote host prep (podman present, "
                         "linger, dbus.socket) and exit")
    args = ap.parse_args(argv)

    profile_path = _profile_path(project_root, args.host)

    # Existing profile? Its remote config keys are authoritative; for a
    # missing profile, generate one from docker/.env with defaults.
    if profile_path.is_file():
        print(f"Using existing host profile: {profile_path}")
        profile_env = _read_env(profile_path)
    else:
        src_env = project_root / "docker" / ".env"
        if not src_env.is_file():
            # The profile derives from docker/.env (the podman-safe,
            # quote-stripped copy), which derives from .env.
            if not (project_root / ".env").is_file():
                raise RuntimeError(".env not found — run: compass env configure")
            print("=== Generating docker/.env ===")
            _run(["bash", "docker/generate-env.sh"], project_root)
        print(f"=== Generating host profile {profile_path} ===")
        _generate_profile(project_root, profile_path, src_env, args.host,
                          DEFAULT_REMOTE_ROOT, DEFAULT_REMOTE_DB_PORT)
        profile_env = _read_env(profile_path)

    # The remote host may differ from the alias (ssh-config name vs
    # actual target); everything remote goes through it.
    remote_host = profile_env.get("ORES_REMOTE_HOST", args.host)
    print(f"=== Checking ssh {remote_host} ===")
    _ssh(remote_host, "true", check=True)

    # Expand a ~-prefixed remote root against the remote HOME and pin
    # the expanded form back into the profile: the profile is what gets
    # scp'd to the remote and consumed by podman --env-file / the C++
    # parsers, neither of which expand ~.
    remote_root_value = profile_env.get("ORES_REMOTE_ROOT",
                                         DEFAULT_REMOTE_ROOT)
    remote_root = _remote_root(remote_host, remote_root_value)
    if remote_root != remote_root_value:
        text = profile_path.read_text(encoding="utf-8").replace(
            remote_root_value, remote_root)
        profile_path.write_text(text, encoding="utf-8")
        profile_env = _read_env(profile_path)
        print(f"  Pinned expanded remote root into {profile_path}: "
              f"{remote_root}")

    if args.setup_host:
        _setup_host(project_root, remote_host)
        return 0

    if args.stop:
        _stop(project_root, remote_host, profile_env, remote_root,
              args.role, args.purge)
        return 0

    _preflight(project_root, profile_path, args.role, profile_env)

    # Image build is a separate step — require it to have been run first.
    import image_build  # noqa: PLC0415
    if not image_build.check_images_exist(project_root, args.role):
        return 1

    if args.role == "compute":
        missing = _missing_compute_keys(profile_env)
        if missing:
            raise RuntimeError(
                f"compute role requires {', '.join(missing)} in "
                f"{profile_path} — add them (from the serving "
                "environment's checkout) and re-run")
        tag = _version_tag(project_root)
        _stream_images(project_root, remote_host,
                       [f"{IMAGE_COMPUTE}:{tag}"])
        env_path, keys_stage = _write_compute_env(
            project_root, profile_env,
            profile_env.get("ORES_COMPUTE_LABEL",
                            _label(profile_env, project_root)),
            remote_root)
        _transfer_compute(project_root, remote_host, remote_root,
                          env_path, keys_stage)
        _deploy_compute(project_root, remote_host, profile_env, remote_root)
        return 0

    tag = _version_tag(project_root)
    svc_images = [f"localhost/{s}:{tag}"
                  for s in _runtime_services(project_root)]
    _stream_images(project_root, remote_host,
                   [f"{IMAGE_BASE}:{tag}", *svc_images,
                    f"{IMAGE_NATS}:{tag}"])
    _transfer_runtime(project_root, remote_host, remote_root,
                      profile_path, _label(profile_env, project_root))
    _deploy_runtime(project_root, remote_host, profile_env, remote_root)
    return 0


def run(argv, project_root: Path) -> int:
    if argv and argv[0] == "list":
        return _cmd_list(project_root)
    try:
        return _cmd_deploy(argv, project_root)
    except subprocess.CalledProcessError as e:
        print(f"error: {e} (exit code {e.returncode})", file=sys.stderr)
        return 1
    except RuntimeError as e:
        print(f"error: {e}", file=sys.stderr)
        return 1
