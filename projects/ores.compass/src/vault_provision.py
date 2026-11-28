"""Provision the deployment's own custody material, and rotate it.

A service cannot ask for the database password it needs to start, because asking
is what needs the password. The circle is broken where the keys already are:
`ores.vault.service --provision` reads one document from a pipe and writes it
with the key encryption key it holds. This module builds that document and hands
it over, and it makes the values it writes.

One value, four homes, one act. A service proves the same password to its
database role and to IAM, which keeps a hash of it, and the transaction pooler
authenticates it from the verifier the server derived. So a run invents a
password per service and writes it to the role, to the IAM credential, to the
verifier the pooler reads under ``build/keys``, and to the vault. A value that
reaches three of those four is a service nobody can sign in, which is why they
are not separate steps.

Because a run invents every value, running it rotates every password: the units
and the pooler read theirs at start, so a run is followed by a restart of the
fleet. Rotation is what a second run is for; provisioning a new service and
rotating an old one are the same act.

The two services that cannot ask keep a credential file instead of a vault entry:
the vault, which would have to answer its own request, and IAM, which has to
exist before the vault does.

Provisioning acts as the database's own administrator, because a role cannot set
its own password to a value it does not know. It therefore needs PGPASSWORD, and
the deployment's shell account is not involved.
"""

from __future__ import annotations

import base64
import json
import os
import secrets
import subprocess
import sys
from pathlib import Path

import vault_credentials
from service_registry import SELF_PROVISIONING
from service_registry import load_service_registry

# The services whose password never enters the vault: they cannot ask for it, so
# each gets a credential file instead.

# The vault's key encryption key, transport key and the version that says which
# key a write uses. The service reads them from here, and so does provisioning.
KEYS_DIRECTORY = "build/keys"

# How a service's own key pair is named under the keys directory.
KEY_SUFFIX = ".vault.key"

# What the pooler authenticates a service with: the verifier the server derived,
# which is not a password and is useless without one.
VERIFIER_SUFFIX = "db-verifier"
PUBLIC_SUFFIX = ".vault.pub"


# The credential a unit that cannot ask reads its own password from, which is
# the name the runner looks for in the directory the deployment places.
PASSWORD_CREDENTIAL = "db-password"


def write_password_credential(keys_dir: Path, psql_var: str, password: str) -> Path:
    """Places the password of a service that cannot ask the vault for it.

    Only the two that have to start before the vault answers reach this: the
    vault itself, and IAM, which the vault waits for. The file is read-only to
    its owner, and the unit is given it as a credential rather than through the
    environment file, which no longer carries a value.
    """
    path = keys_dir / f"{psql_var}.{PASSWORD_CREDENTIAL}"
    path.write_text(password, encoding="utf-8")
    path.chmod(0o600)
    return path


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


def invent_password() -> str:
    """A database password, made by the step that puts it in both places.

    The environment file names the vault's entry and not its value, so the value
    has to be made where it can reach every place that checks it: the role, the
    IAM credential that the same value signs a service in with, and the vault.
    """
    return secrets.token_urlsafe(24)


def build_document(services, keys_dir: Path, announce) -> tuple[dict, list[dict]]:
    """The document the vault writes, and the roles it belongs with.

    Each service gets a fresh password, so a run rotates them all. Rotation and
    provisioning are one act on purpose: two steps would leave the vault, the
    role and the IAM credential disagreeing if one of them failed, and the next
    run is what repairs any of them.

    The second list is what the caller needs to set the same values on the
    database roles, which is the half this module cannot do itself.
    """
    consumers: list[dict] = []
    secrets: list[dict] = []
    roles: list[dict] = []

    for service in services:
        psql_var = service.get("psql_var", "")
        env_key = service.get("env_key", "")
        account = service.get("email", "")
        if not psql_var or not env_key or not account:
            continue

        password = invent_password()
        roles.append({
            "name": account,
            "psql_var": psql_var,
            "user": f"{{ORES_{env_key}_DB_USER}}",
            "username": f"{{ORES_{env_key}_DB_USER}}",
            "value": password,
        })

        if psql_var in SELF_PROVISIONING:
            # A service that cannot ask the vault still needs a value, and the
            # environment file no longer carries one. It goes where the
            # deployment places credentials, by the name the runner looks for.
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

    return {"consumers": consumers, "secrets": secrets}, roles


def apply_roles(env: dict, roles: list[dict]) -> None:
    """Puts one value into every place that checks it, for every role.

    A service proves the same value twice: it logs in to its database role, and
    it signs in to IAM, which compares it against the account's stored hash. The
    two must move together, so they move in one statement list. The pooler also
    needs the value, but only as the verifier the server derived from it, which
    is read back after the rotation and written where the pooler looks.
    """
    host = env.get("ORES_DB_HOST", "localhost")
    database = env.get("ORES_TEST_DB_DATABASE", "")
    password = env.get("PGPASSWORD", "")
    if not password:
        print("error: PGPASSWORD is set neither in the environment nor in .env",
              file=sys.stderr)
        raise SystemExit(1)

    child = os.environ.copy()
    child.update(env)
    child["PGPASSWORD"] = password

    def psql(*args):
        done = subprocess.run(
            ["psql", "-h", host, "-U", "postgres", "-d", database, "-v", "ON_ERROR_STOP=1"]
            + list(args), text=True, capture_output=True, env=child)
        if done.returncode != 0:
            print(f"error: {done.stderr.strip()}", file=sys.stderr)
            raise SystemExit(done.returncode)
        return done.stdout

    accounts = {
        line.strip() for line in
        psql("-t", "-A", "-c",
             "select username from ores_iam_accounts_tbl "
             "where valid_to = ores_utility_infinity_timestamp_fn()").splitlines()
        if line.strip()
    }

    statements = []
    for role in roles:
        user = env.get(role["user"].strip("{}"), "")
        if not user:
            continue
        value = role["value"].replace("'", "''")
        statements.append(f"ALTER ROLE \"{user}\" WITH PASSWORD '{value}';")
        if user in accounts:
            statements.append(
                f"select ores_iam_service_account_credential_set_fn('{user}', '{value}');")
    if statements:
        psql("-q", "-c", "\n".join(statements))
    print(f"  roles   {len(statements)} statement(s) applied")

    # The verifier the server derived, which is what the pooler authenticates a
    # client with. It is not a password: without the plaintext it is useless.
    users = [env.get(r["user"].strip("{}"), "") for r in roles]
    users = [u for u in users if u]
    if not users:
        return
    listed = ", ".join(f"'{u}'" for u in users)
    rows = psql("-t", "-A", "-F", "|", "-c",
                f"select usename, passwd from pg_shadow where usename in ({listed})")
    verifiers = {}
    for line in rows.splitlines():
        if "|" in line:
            name, verifier = line.split("|", 1)
            verifiers[name.strip()] = verifier.strip()
    print(f"  pooler  {len(verifiers)} verifier(s) read")
    return verifiers


def _child_environment(env: dict, keys_dir: Path) -> dict:
    """The environment the vault's own process runs with.

    It is the file's, because the unit's is drawn from the same file, plus the
    directory the platform states its credentials in — which on this host is the
    checkout's own key directory, the deployment's master copy.
    """
    child = os.environ.copy()
    child.update(env)
    child["CREDENTIALS_DIRECTORY"] = str(keys_dir)

    # PostgreSQL itself, not the pooler: a run rewrites the passwords the
    # pooler's user list was built from, so a pooler asked during the run would
    # refuse the password this run just invented. The restart that follows
    # rebuilds that list.
    child["ORES_VAULT_SERVICE_DB_HOST"] = env.get("ORES_DB_HOST", "localhost")
    child["ORES_VAULT_SERVICE_DB_PORT"] = env.get("PGPORT") or "5432"

    # The vault's own password is not in the environment file any more: it is the
    # file the deployment places, and the unit reads it as a credential. This
    # invocation is not the unit, so it is handed the same value the way the
    # option mapper reads it.
    own = keys_dir / "vault_service.db-password"
    if own.is_file() and not child.get("ORES_VAULT_SERVICE_DB_PASSWORD"):
        child["ORES_VAULT_SERVICE_DB_PASSWORD"] = own.read_text(encoding="utf-8").strip()
    return child


def restart_pooler(env: dict) -> None:
    """Rebuilds the pooler's user list from the passwords this run invented.

    The pooler authenticates each service with the verifier the server holds, so
    a list built before the run names the passwords the run replaced. Restarting
    it is the step that makes the new ones usable, and it belongs here rather
    than in the operator's hands: nothing else knows the values just changed.
    A host that runs no pooler has no unit to restart, which is not a failure.
    """
    label = env.get("ORES_ENV_NAME", "")
    if not label:
        return

    import systemctl_bus

    systemctl_bus.adopt_transport_setting(env)
    unit = f"ores-pooler-{label}.service"
    done = systemctl_bus.run(["--user", "restart", unit],
                             capture_output=True, text=True, check=False)
    if done.returncode == 0:
        print("  pooler  restarted on the new passwords")
    else:
        print("  pooler  not restarted; start it before the services that "
              "resolve at start", file=sys.stderr)


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
    document, roles = build_document(services, keys_dir, announce)

    if not document["consumers"]:
        print("error: nothing to provision — the registry names no database "
              "service", file=sys.stderr)
        return 1

    if args.dry_run:
        shown = {
            "consumers": document["consumers"],
            "secrets": [{"name": s["name"], "owner": s["owner"],
                         "value": f"<{len(s['value'])} characters>"}
                        for s in document["secrets"]],
            "roles": [{"user": r["user"], "value": f"<{len(r['value'])} characters>"}
                      for r in roles],
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

    # The roles and the IAM credentials first, in one statement list, because a
    # vault that holds a value the role does not is a service that resolves a
    # password the database refuses.
    verifiers = apply_roles(env, roles) or {}
    for role in roles:
        user = env.get(role["user"].strip("{}"), "")
        if role["psql_var"] in SELF_PROVISIONING:
            write_password_credential(keys_dir, role["psql_var"], role["value"])
        if user in verifiers:
            path = keys_dir / f"{role['psql_var']}.{VERIFIER_SUFFIX}"
            path.write_text(verifiers[user], encoding="utf-8")
            path.chmod(0o600)

    print(f"  keys    {len(verifiers)} verifier(s) and the two placed passwords")
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

    restart_pooler(env)
    return 0
