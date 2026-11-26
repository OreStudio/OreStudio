"""The vault's two keys, and the transport key pair the gateway uses.

The vault cannot start without a key encryption key, which seals every stored
value, and a transport key pair, which is how a caller seals a value to it. Both
are generated once per checkout, under ``build/keys``, and never by a build: a
key a build could mint is a key a rebuild would replace, orphaning every value
the vault already holds.

They are handed to the unit as systemd credentials rather than through the
environment file, because the environment file is written by the same
generator that would then hold the key, and a key beside what it protects
protects nothing.

The gateway's pair is generated here too. It is the consumer that resolves a
provider's secret at the token exchange and answers a person's reveal, so it
needs a pair of its own, and generating both in one place keeps the two from
being provisioned by different steps.
"""

from __future__ import annotations

import subprocess
from pathlib import Path

_KEK = "vault.kek"
_TRANSPORT = "vault.transport.key"
_TRANSPORT_PUBLIC = "vault.transport.pub"
_GATEWAY = "gateway.transport.key"
_GATEWAY_PUBLIC = "gateway.transport.pub"

_KEY_LEN = 32


def _openssl(*args: str) -> bytes:
    """Runs openssl and returns its standard output."""
    return subprocess.run(
        ["openssl", *args], check=True, capture_output=True
    ).stdout


def _raw_x25519(path: Path, public: bool) -> bytes:
    """The raw 32-byte X25519 half of a generated private key.

    openssl emits DER, whose last 32 bytes are the key itself for both the
    private and the public form. Slicing is what keeps this free of a
    dependency on a crypto library the project does not otherwise carry.
    """
    args = ["pkey", "-in", str(path)]
    if public:
        args += ["-pubout"]
    args += ["-outform", "DER"]
    raw = _openssl(*args)
    if len(raw) < _KEY_LEN:
        raise RuntimeError(f"openssl produced {len(raw)} bytes for {path}, not a key")
    return raw[-_KEY_LEN:]


def _generate_pair(keys_dir: Path, private_name: str, public_name: str,
                   announce) -> None:
    """Generates one X25519 pair, writing the raw halves."""
    private = keys_dir / private_name
    public = keys_dir / public_name
    if private.is_file() and public.is_file():
        return

    pem = keys_dir / f".{private_name}.pem"
    announce(f"Generating {private_name} and {public_name}...")
    _openssl("genpkey", "-algorithm", "X25519", "-out", str(pem))
    try:
        private.write_bytes(_raw_x25519(pem, public=False))
        public.write_bytes(_raw_x25519(pem, public=True))
    finally:
        pem.unlink(missing_ok=True)
    private.chmod(0o600)
    public.chmod(0o644)
    announce(f"  Written to {private} and {public}")


def ensure(checkout_root: Path, announce=print) -> dict[str, Path]:
    """Generates whatever is missing and answers every key's path.

    Idempotent: a key that is already present is left exactly as it is, so a
    restart or a deploy never replaces the key that protects the stored values.
    """
    keys_dir = checkout_root / "build" / "keys"
    keys_dir.mkdir(parents=True, exist_ok=True)

    kek = keys_dir / _KEK
    if not kek.is_file():
        announce("Generating the vault's key encryption key...")
        _openssl("rand", "-out", str(kek), str(_KEY_LEN))
        kek.chmod(0o600)
        announce(f"  Written to {kek}")

    _generate_pair(keys_dir, _TRANSPORT, _TRANSPORT_PUBLIC, announce)
    _generate_pair(keys_dir, _GATEWAY, _GATEWAY_PUBLIC, announce)

    return {
        "kek": kek,
        "transport": keys_dir / _TRANSPORT,
        "transport_public": keys_dir / _TRANSPORT_PUBLIC,
        "gateway": keys_dir / _GATEWAY,
        "gateway_public": keys_dir / _GATEWAY_PUBLIC,
    }
