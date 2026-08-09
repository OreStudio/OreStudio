"""compass nats ensure — Bring the local NATS server to a correct state.

Composes the existing NATS building blocks so a session never starts
against stale infrastructure:

    1. certificates — nats_certs.generate() (idempotent: skips existing
       keys unless --force)
    2. config       — nats_init.generate() (render build/config/nats-<label>.conf)
    3. restart      — systemctl --user restart nats-server-<label>.service
    4. probe        — wait for the NATS INFO handshake on the client port

The restart is the piece nothing else does: the Go nats-server loads
certs at startup and does NOT pick up a regenerated CA/leaf by itself,
so every regeneration of build/keys/nats/ must be followed by a restart
or every mTLS client fails with "SSL Error" / "bad record MAC" (see
doc/knowledge/architecture/nats_certificates.org, "Rotation and
re-deploy").

The server is managed as the per-environment systemd user unit
nats-server-<label>.service (systemd_generate keeps the env name
verbatim, e.g. nats-server-brave_hopper.service; some older units use
dashes — both conventions are matched).  If the unit does not exist
the command reports how to start the server instead of guessing.
"""

import argparse
import socket
import subprocess
import sys
import time
from pathlib import Path

import nats_certs
import nats_init


def _load_env(project_root: Path) -> dict:
    env = nats_init._load_dotenv(project_root / ".env")
    return env


def _find_unit(label: str) -> str:
    """The systemd user unit for the env's NATS server.

    systemd_generate names units '{service}-{env_name}' with the env
    name verbatim (nats-server-brave_hopper.service), but some older
    units were created with dashes (nats-server-festive-dijkstra.service)
    — match either convention against the installed unit list.
    """
    candidates = [
        f"nats-server-{label}.service",
        f"nats-server-{label.replace('_', '-')}.service",
    ]
    proc = subprocess.run(
        ["systemctl", "--user", "list-unit-files"],
        capture_output=True, text=True, check=False)
    for unit in candidates:
        if unit in proc.stdout:
            return unit
    return candidates[0]  # canonical name, for the error message


def _restart(label: str) -> int:
    unit = _find_unit(label)
    proc = subprocess.run(
        ["systemctl", "--user", "list-unit-files", unit],
        capture_output=True, text=True, check=False)
    if unit not in proc.stdout:
        print(f"Error: no systemd user unit '{unit}' for environment '{label}'.", file=sys.stderr)
        print("  Start the server manually with:", file=sys.stderr)
        print(f"    nats-server --config build/config/nats-{label}.conf", file=sys.stderr)
        return 1

    print(f"=== Restarting {unit} ===")
    proc = subprocess.run(
        ["systemctl", "--user", "restart", unit],
        capture_output=True, text=True, check=False)
    if proc.returncode != 0:
        print(f"Error: systemctl --user restart {unit} failed:", file=sys.stderr)
        print(proc.stderr.strip() or proc.stdout.strip(), file=sys.stderr)
        return 1
    print(f"  Restarted: {unit}")
    return 0


def _probe(port: int, timeout_seconds: int = 15) -> int:
    """Wait for the server and confirm it speaks NATS (INFO handshake line)."""
    deadline = time.monotonic() + timeout_seconds
    while time.monotonic() < deadline:
        try:
            with socket.create_connection(("localhost", port), timeout=2) as s:
                info = s.recv(1024).decode(errors="replace")
            if "INFO" in info:
                tls = "tls_required" in info
                print(f"  NATS     : INFO probe ok on localhost:{port}"
                      f" (tls_required={tls})")
                return 0
        except OSError:
            pass
        time.sleep(1)
    print(f"Error: NATS server not answering on localhost:{port} after "
          f"{timeout_seconds}s", file=sys.stderr)
    return 1


def run(argv, project_root: Path) -> int:
    ap = argparse.ArgumentParser(
        prog="compass nats ensure",
        description="Bring the local NATS server to a correct state: "
                    "certs present (idempotent), config rendered, server "
                    "restarted, INFO probe confirms it is up.")
    ap.add_argument("--force", action="store_true",
                    help="Regenerate all certificates (--force on nats certs)")
    args = ap.parse_args(argv)

    env = _load_env(project_root)
    label = env.get("ORES_CHECKOUT_LABEL", project_root.name)
    try:
        nats_port = int(env.get("ORES_NATS_PORT", "4222"))
    except ValueError:
        print(f"Error: invalid ORES_NATS_PORT in .env: {env.get('ORES_NATS_PORT')}",
              file=sys.stderr)
        return 1

    print(f"=== NATS ensure for environment '{label}' ===")
    print(f"  Port: {nats_port}")

    # 1. Certificates (idempotent; --force to rotate).
    print("=== Certificates ===")
    nats_certs.generate(project_root, force=args.force)

    # 2. Config + JetStream store dir.
    print("=== Config ===")
    nats_init.generate(
        project_root, label,
        nats_port,
        int(env.get("ORES_NATS_MONITOR_PORT", "8222")),
        Path(env.get("ORES_NATS_STORE_DIR",
                     str(project_root / "build" / "nats" / label / "jetstream"))))

    # 3. Restart the server so it loads the current certs.
    rc = _restart(label)
    if rc != 0:
        return rc

    # 4. Verify.
    return _probe(nats_port)
