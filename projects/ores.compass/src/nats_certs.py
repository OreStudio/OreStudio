"""compass nats certs — Generate CA + per-service mTLS certificates for NATS.

Python port of the former build/scripts/generate_nats_certs.sh.

Creates build/keys/nats/ with:
    ca.key, ca.crt              — Internal CA (1 year validity)
    nats-server.key/crt         — NATS broker cert (SAN: localhost + <hostname>)
    <service>.key/crt           — One client cert per service (90-day validity)

Existing files are NOT overwritten unless --force is given.  Use --force in CI
(ephemeral keys); omit in dev (stable keys).

Requires: openssl in PATH.
"""

import argparse
import subprocess
import tempfile
from pathlib import Path

_SERVICES = [
    "ores.controller.service",
    "ores.iam.service",
    "ores.refdata.service",
    "ores.workspace.service",
    "ores.dq.service",
    "ores.variability.service",
    "ores.assets.service",
    "ores.synthetic.service",
    "ores.scheduler.service",
    "ores.reporting.service",
    "ores.telemetry.service",
    "ores.trading.service",
    "ores.marketdata.service",
    "ores.compute.service",
    "ores.ore.service",
    "ores.workflow.service",
    "ores.http.server",
    "ores.wt.service",
    "ores.qt.client",
    "ores.compute.wrapper",
    "ores.analytics.service",
]

_CA_DAYS = 365
_LEAF_DAYS = 90


def _openssl(*args):
    subprocess.run(["openssl", *args], check=True)


def _skip(path: Path, force: bool) -> bool:
    """Return True (and print a skip notice) when the file already exists and force is off."""
    if path.is_file() and not force:
        print(f"  (exists, skipping) {path}")
        return True
    return False


def generate(checkout_root: Path, *, force: bool = False, hostname: str = "localhost") -> None:
    """Generate all NATS mTLS certificates under build/keys/nats/.

    Idempotent: skips files that already exist unless *force* is True.
    """
    keys_dir = checkout_root / "build" / "keys" / "nats"
    keys_dir.mkdir(parents=True, exist_ok=True)

    ca_key = keys_dir / "ca.key"
    ca_crt = keys_dir / "ca.crt"

    # --- CA ---
    print("==> CA")
    if not _skip(ca_key, force):
        _openssl("ecparam", "-name", "prime256v1", "-genkey", "-noout",
                 "-out", str(ca_key))
        _openssl("req", "-new", "-x509",
                 "-key", str(ca_key),
                 "-out", str(ca_crt),
                 "-days", str(_CA_DAYS),
                 "-subj", "/CN=ores-nats-ca/O=ORE Studio")
        print(f"  Generated CA: {ca_crt}")

    # --- NATS server certificate ---
    print("==> NATS server certificate")
    server_key = keys_dir / "nats-server.key"
    server_crt = keys_dir / "nats-server.crt"
    server_csr = keys_dir / "nats-server.csr"
    if not _skip(server_key, force):
        _openssl("ecparam", "-name", "prime256v1", "-genkey", "-noout",
                 "-out", str(server_key))
        _openssl("req", "-new",
                 "-key", str(server_key),
                 "-out", str(server_csr),
                 "-subj", "/CN=nats-server/O=ORE Studio")
        # Write the SAN extension to a temp file so the path is always a real
        # filesystem path (process substitution via <(...) is not portable).
        san = f"subjectAltName=DNS:localhost,DNS:{hostname},IP:127.0.0.1"
        with tempfile.NamedTemporaryFile(mode="w", suffix=".cnf", delete=False) as tf:
            tf.write(san + "\n")
            san_file = Path(tf.name)
        try:
            _openssl("x509", "-req",
                     "-in", str(server_csr),
                     "-CA", str(ca_crt),
                     "-CAkey", str(ca_key),
                     "-CAcreateserial",
                     "-out", str(server_crt),
                     "-days", str(_LEAF_DAYS),
                     "-extfile", str(san_file))
        finally:
            san_file.unlink(missing_ok=True)
            server_csr.unlink(missing_ok=True)
        print(f"  Generated: {server_crt} (SAN: localhost, {hostname})")

    # --- Service client certificates ---
    print("==> Service client certificates")
    for svc in _SERVICES:
        svc_key = keys_dir / f"{svc}.key"
        svc_crt = keys_dir / f"{svc}.crt"
        svc_csr = keys_dir / f"{svc}.csr"
        if not _skip(svc_key, force):
            _openssl("ecparam", "-name", "prime256v1", "-genkey", "-noout",
                     "-out", str(svc_key))
            _openssl("req", "-new",
                     "-key", str(svc_key),
                     "-out", str(svc_csr),
                     "-subj", f"/CN={svc}/O=ORE Studio")
            _openssl("x509", "-req",
                     "-in", str(svc_csr),
                     "-CA", str(ca_crt),
                     "-CAkey", str(ca_key),
                     "-CAcreateserial",
                     "-out", str(svc_crt),
                     "-days", str(_LEAF_DAYS))
            svc_csr.unlink(missing_ok=True)
            print(f"  Generated: {svc_crt}")

    (keys_dir / "ca.srl").unlink(missing_ok=True)
    print(f"\nDone. Certificates written to {keys_dir}")
    print("Rotation: re-run with --force to regenerate all certificates.")


def run(argv, project_root: Path) -> int:
    parser = argparse.ArgumentParser(
        prog="compass nats certs",
        description="Generate CA + per-service mTLS certificates for NATS.")
    parser.add_argument("--force", action="store_true",
                        help="Overwrite existing certificates.")
    parser.add_argument("--hostname", default="localhost",
                        help="Extra hostname for the NATS server SAN (default: localhost).")
    args = parser.parse_args(argv)
    generate(project_root, force=args.force, hostname=args.hostname)
    return 0
