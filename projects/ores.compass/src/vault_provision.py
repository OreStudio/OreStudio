# -*- coding: utf-8 -*-
"""Provision the deployment's own custody material into the vault.

A service's database password must live in the vault before the service can ask
for it, and asking is what the vault authenticates. The circle is broken where
the keys already are: `ores.vault.service --provision` reads one document from a
pipe and writes it with the key encryption key it holds. This module builds that
document and hands it over.

Two things are written per database service:

- a consumer, named by the service account the registry states, carrying the
  public half of a key pair this module generates under ``build/keys``;
- the password the environment file already holds, owned by that same account,
  so the vault knows who may resolve it.

The private half stays in the checkout, as ``build/keys/<psql_var>.vault.key``,
and a unit is given it when the service starts resolving at boot. Nothing here
changes the database: the roles already hold the passwords this reads.

The vault is skipped. It is the one process that cannot ask itself, so its own
password stays a credential file and never enters its own store, in the same way
its keys do not.
"""

from __future__ import annotations

import base64
import json
import os
import subprocess
import sys
from pathlib import Path

import vault_credentials
from service_registry import load_service_registry

# The service whose password never enters the vault, because it would have to
# answer its own request. IAM is provisioned here like any other service, even
# though its unit does not resolve yet: it has to start before the vault, because
# the vault fetches its token keys from IAM, so the value waits in the vault for
# the day that order changes. systemd_generate holds the other half of that fact.
SELF_PROVISIONING = {"vault_service"}

# The vault's key encryption key, transport key and the version that says which
# key a write uses. The service reads them from here, and so does provisioning.
KEYS_DIRECTORY = "build/keys"

# How a service's own key pair is named under the keys directory.
KEY_SUFFIX = ".vault.key"
PUBLIC_SUFFIX = ".vault.pub"


def consumer_key_paths(keys_dir: Path, psql_var: str) -> tuple[Path, Path]:
    """The private and public halves of one consumer's key pair."""
    return (keys_dir / f"{psql_var}{KEY_SUFFIX}", keys_dir / f"{psql_var}{PUBLIC_SUFFIX}")


def ensure_consumer_key(keys_dir: Path, psql_var: str, announce) -> Path:
    """Generates the consumer's key pair when it is absent, and answers the public half.

    The pair is generated once and kept: a copy that a run replaced would have to
    be re-registered with the vault, and the value already stored for it would be
    sealed to a key nobody holds.
    """
    private, public = consumer_key_paths(keys_dir, psql_var)
    vault_credentials._generate_pair(keys_dir, private.name, public.name, announce)
    return public


def build_document(services, env: dict, keys_dir: Path, announce) -> tuple[dict, list[str]]:
    """The document the vault writes, and the services it could not describe.

    A service is left out when the environment file holds no password for it,
    because provisioning a value that is not the role's password would lock the
    service out of its own database.
    """
    consumers: list[dict] = []
    secrets: list[dict] = []
    skipped: list[str] = []

    for service in services:
        psql_var = service.get("psql_var", "")
        if not psql_var or psql_var in SELF_PROVISIONING:
            continue

        env_key = service.get("env_key", "")
        account = service.get("email", "")
        password = env.get(f"ORES_{env_key}_DB_PASSWORD", "")
        if not env_key or not account or not password:
            skipped.append(service["name"])
            continue

        public = ensure_consumer_key(keys_dir, psql_var, announce)
        consumers.append({
            "name": account,
            "actor": account,
            "public_key": base64.b64encode(public.read_bytes()).decode("ascii"),
        })
        secrets.append({
            "name": f"db.{psql_var}.password",
            "owner": account,
            "value": password,
        })

    return {"consumers": consumers, "secrets": secrets}, skipped


def _child_environment(env: dict, keys_dir: Path) -> dict:
    """The environment the vault's own process runs with.

    It is the file's, because the unit's is drawn from the same file, plus the
    directory the platform states its credentials in — which on this host is the
    checkout's own key directory, the deployment's master copy.
    """
    child = os.environ.copy()
    child.update(env)
    child["CREDENTIALS_DIRECTORY"] = str(keys_dir)
    return child


def run(argv, project_root: Path, env_file: Path | None = None) -> int:
    import argparse

    from compass_db import load_env

    ap = argparse.ArgumentParser(
        prog="compass vault",
        description="Write the deployment's own secrets into the vault: one "
                    "consumer and one password per database service, before "
                    "any service holds a token.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    pv = sub.add_parser("provision",
                        help="Write each database service's password and consumer "
                             "key into the vault")
    pv.add_argument("--dry-run", action="store_true",
                    help="Print the document the vault would be handed, with "
                         "each value's length rather than the value")
    pv.add_argument("--preset", default=None,
                    help="CMake preset holding the vault binary (default: "
                         "ORES_PRESET from .env)")
    pv.add_argument("--keys-dir", default=None,
                    help=f"Where the key pairs live (default: {KEYS_DIRECTORY})")

    args = ap.parse_args(argv)
    env = load_env(project_root, env_file)
    keys_dir = Path(args.keys_dir) if args.keys_dir else project_root / KEYS_DIRECTORY
    keys_dir.mkdir(parents=True, exist_ok=True)

    def announce(message):
        print(message)

    services = load_service_registry(project_root)
    document, skipped = build_document(services, env, keys_dir, announce)
    for name in skipped:
        print(f"  skip    {name}: the environment file holds no password for it")

    if not document["consumers"]:
        print("error: nothing to provision — the environment file holds no "
              "service password", file=sys.stderr)
        return 1

    if args.dry_run:
        shown = {
            "consumers": document["consumers"],
            "secrets": [{"name": s["name"], "owner": s["owner"],
                         "value": f"<{len(s['value'])} characters>"}
                        for s in document["secrets"]],
        }
        print(json.dumps(shown, indent=2))
        return 0

    preset = args.preset or env.get("ORES_PRESET", "")
    if not preset:
        print("error: no preset — pass --preset <preset> or set ORES_PRESET "
              "via compass env configure", file=sys.stderr)
        return 1
    binary = project_root / "build/output" / preset / "publish/bin/ores.vault.service"
    if not binary.is_file():
        print(f"error: no vault binary at {binary}", file=sys.stderr)
        print("       build it with: ./compass.sh build ores.vault.service",
              file=sys.stderr)
        return 1

    print(f"  vault   {len(document['consumers'])} consumers, "
          f"{len(document['secrets'])} secrets")
    run_vault = subprocess.run(
        [str(binary), "--provision", "-", "--log-level", "warn"],
        input=json.dumps(document), text=True, capture_output=True,
        env=_child_environment(env, keys_dir))
    for line in run_vault.stdout.splitlines():
        print(f"  wrote   {line}")
    if run_vault.returncode != 0:
        for line in run_vault.stderr.splitlines():
            print(f"  failed  {line}", file=sys.stderr)
        return run_vault.returncode
    return 0
