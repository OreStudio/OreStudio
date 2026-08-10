"""compass nats certs — Generate CA + per-service mTLS certificates for NATS.

Creates build/keys/nats/ with:
    ca.key, ca.crt              — Internal CA (1 year validity)
    nats-server.key/crt         — NATS broker cert (SAN: localhost + <hostname>
                                  + the host's own non-loopback IPv4 addresses)
    <service>.key/crt           — One client cert per service (90-day validity)

Existing files are NOT overwritten unless --force is given.  Use --force in CI
(ephemeral keys); omit in dev (stable keys).

Requires: openssl in PATH (and iproute2's `ip` for address discovery; falls
back to gethostname resolution when `ip` is absent).
"""

import argparse
import ipaddress
import socket
import subprocess
import tempfile
from pathlib import Path

_SERVICES = [
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


def _local_ipv4_addresses() -> list[str]:
    """Non-loopback IPv4 addresses assigned to this host, best effort.

    The NATS broker cert must carry the serving host's own addresses as
    IP SANs: remote clients (e.g. a compute node deployed to another box)
    connect to the broker by IP, and RFC 6125 requires the connecting
    address to appear as an "IP Address" SAN entry, never a DNS entry.
    """
    addrs: set[str] = set()
    proc = subprocess.run(["ip", "-4", "-o", "addr", "show"],
                          capture_output=True, text=True, check=False)
    if proc.returncode == 0:
        for line in proc.stdout.splitlines():
            parts = line.split()
            if len(parts) >= 4 and parts[2] == "inet":
                ip = parts[3].split("/")[0]
                if not ip.startswith("127.") and not ip.startswith("169.254."):
                    addrs.add(ip)
    if not addrs:
        # Fallback: whatever gethostname resolves to (no `ip` available).
        try:
            for res in socket.getaddrinfo(socket.gethostname(), None,
                                          socket.AF_INET):
                addrs.add(res[4][0])
        except OSError:
            pass
        addrs.discard("127.0.0.1")
    return sorted(addrs)


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
        # --hostname may be a literal IP (e.g. a remote deploy host's
        # address) rather than a DNS name -- TLS clients verifying a
        # connection made to an IP target check the SAN's "IP Address"
        # entries, not "DNS" entries (RFC 6125/2818), so a DNS-typed SAN
        # containing dotted-decimal text does not satisfy that check and
        # the handshake fails with a generic SSL error, not a clear
        # hostname-mismatch message.
        try:
            ipaddress.ip_address(hostname)
            extra_san = f"IP:{hostname}"
        except ValueError:
            extra_san = f"DNS:{hostname}"
        san_entries = ["DNS:localhost", extra_san, "IP:127.0.0.1"]
        # Resolve once: the SAN list and the log line below must show the
        # same address set -- a second subprocess call could race a DHCP
        # renewal and print a different set than went into the cert.
        local_ips = _local_ipv4_addresses()
        for ip in local_ips:
            entry = f"IP:{ip}"
            if entry not in san_entries:
                san_entries.append(entry)
        san = "subjectAltName=" + ",".join(san_entries)
        san_file = None
        try:
            with tempfile.NamedTemporaryFile(mode="w", suffix=".cnf", delete=False) as tf:
                tf.write(san + "\n")
                san_file = Path(tf.name)
            _openssl("x509", "-req",
                     "-in", str(server_csr),
                     "-CA", str(ca_crt),
                     "-CAkey", str(ca_key),
                     "-CAcreateserial",
                     "-out", str(server_crt),
                     "-days", str(_LEAF_DAYS),
                     "-extfile", str(san_file))
        finally:
            if san_file is not None:
                san_file.unlink(missing_ok=True)
            server_csr.unlink(missing_ok=True)
        print(f"  Generated: {server_crt} "
              f"(SAN: localhost, {hostname}, {', '.join(local_ips)})")

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
