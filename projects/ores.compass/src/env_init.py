"""compass env init — initialise the checkout environment.

Generates a .env file at the checkout root with all required credentials for
running ORE Studio services locally and in CI. Ported from
build/scripts/init-environment.sh; behaviour is preserved:

- Existing secrets are reused on re-runs; only missing variables are generated.
- --enable-logging / --disable-logging toggle test logging without touching
  any other variable.
- The NATS domain service list is read from the authored service registry
  (projects/ores.codegen/models/services/ores_services_service_registry.json),
  the same source that generates projects/ores.sql/service_vars.sh.

Dependencies: python3, openssl, and the sibling shell helpers
generate_nats_certs.sh / init-nats.sh (invoked for cert + NATS setup; these
move under `compass nats` in a later migration task).
"""

import argparse
import json
import os
import re
import secrets
import string
import subprocess
import sys
import uuid
from datetime import datetime, timezone
from pathlib import Path

# The canonical .env-format version is the highest row in the org-mode log
# at doc/knowledge/architecture/env_format_version_log.org — read by both
# `compass env init` (writes ORES_ENV_VERSION) and the version check.
# `compass env new-version` appends a row; no version constant lives in code.
def _env_version_doc(project_root: Path) -> Path:
    return (project_root / "doc" / "knowledge" / "architecture" /
            "env_format_version_log.org")


def _parse_version_rows(doc: Path):
    """Parse the version table: returns [(version:int, date:str, desc:str), ...]."""
    rows = []
    for line in doc.read_text().splitlines():
        s = line.strip()
        if not s.startswith("|"):
            continue
        cells = [c.strip() for c in s.strip("|").split("|")]
        if cells and cells[0].isdigit():
            rows.append((int(cells[0]),
                         cells[1] if len(cells) > 1 else "",
                         cells[2] if len(cells) > 2 else ""))
    return rows


def current_version(project_root: Path) -> int:
    """Highest version in the log — the canonical .env-format version."""
    rows = _parse_version_rows(_env_version_doc(project_root))
    if not rows:
        raise ValueError("no versions found in env_format_version_log.org")
    return max(r[0] for r in rows)


_PW_ALPHABET = string.ascii_letters + string.digits


def _gen_password() -> str:
    """32-char alphanumeric secret (matches the shell generator's shape)."""
    return "".join(secrets.choice(_PW_ALPHABET) for _ in range(32))


def _gen_uuid() -> str:
    return str(uuid.uuid4())


def _read_env(env_file: Path) -> dict:
    """Parse an existing .env into {KEY: value} (first occurrence wins)."""
    values: dict = {}
    if not env_file.is_file():
        return values
    for line in env_file.read_text().splitlines():
        if not line or line.lstrip().startswith("#"):
            continue
        if "=" not in line:
            continue
        key, _, val = line.partition("=")
        if key and key not in values:
            values[key] = val
    return values


def _get_or_gen(existing: dict, key: str) -> str:
    val = existing.get(key)
    return val if val else _gen_password()


def _get_or_gen_uuid(existing: dict, key: str) -> str:
    val = existing.get(key)
    return val if val else _gen_uuid()


def _upper(component: str) -> str:
    """Mirror tr '[:lower:]-' '[:upper:]_': uppercase, hyphens to underscores."""
    return component.upper().replace("-", "_")


def _service_names(registry: Path, excludes=("wt",)) -> list:
    data = json.loads(registry.read_text())
    names = [s["name"] for s in data["service_registry"]["services"]]
    return [n for n in names if n not in excludes]


def _logging_only(env_file: Path, op: str, level: str) -> int:
    """--enable-logging / --disable-logging: touch only the logging vars."""
    if not env_file.is_file():
        print(f"Error: {env_file} does not exist. Run 'compass env init' first.",
              file=sys.stderr)
        return 1
    kept = []
    for line in env_file.read_text().splitlines():
        if line.startswith("ORES_TEST_LOG_") or line.startswith("# Test logging"):
            continue
        kept.append(line)
    while kept and kept[-1].strip() == "":
        kept.pop()
    out = "\n".join(kept) + "\n"
    if op == "enable":
        out += (f"\n# Test logging\nORES_TEST_LOG_ENABLED=true\n"
                f"ORES_TEST_LOG_LEVEL={level}\nORES_TEST_LOG_CONSOLE=true\n")
        print(f"Test logging enabled (level={level}).")
    else:
        print("Test logging disabled.")
    env_file.write_text(out)
    env_file.chmod(0o600)
    print("Re-run 'cmake --preset <preset>' to pick up the change.")
    return 0


def new_version(project_root: Path, description: str) -> int:
    """compass env new-version — append a new .env-format version row.

    Computes the next version (current + 1), appends a dated row to the
    version log table with the given description, and writes it back. Run
    this after a change that alters the .env (a new service in the registry,
    a new/renamed variable); then re-run 'compass env init' so existing
    checkouts' version check flags the change.
    """
    doc = _env_version_doc(project_root)
    if not doc.is_file():
        print(f"Error: env version log not found: {doc}", file=sys.stderr)
        return 1
    lines = doc.read_text().splitlines()
    table_idx = [i for i, ln in enumerate(lines) if ln.strip().startswith("|")]
    if not table_idx:
        print(f"Error: no version table found in {doc}", file=sys.stderr)
        return 1
    nxt = current_version(project_root) + 1
    date = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    row = f"| {nxt} | {date} | {description} |"
    lines.insert(table_idx[-1] + 1, row)
    doc.write_text("\n".join(lines) + "\n")
    print(f"Recorded .env-format version {nxt} ({date}): {description}")
    print("Now re-run 'compass env init --preset <preset> -y' to write the new "
          "version into .env (older checkouts will then fail the version check).")
    return 0


def diff(project_root: Path) -> int:
    """compass env diff — unified diff between .env.old and .env.

    Developer tool; output contains secrets in plain text. Exit codes match
    diff(1): 0 identical, 1 differ, 2 on error (missing files).
    """
    env_file = project_root / ".env"
    env_old = project_root / ".env.old"
    if not env_old.is_file():
        print("No .env.old found — run 'compass env init' at least twice to "
              "generate a diff.", file=sys.stderr)
        return 2
    if not env_file.is_file():
        print("No .env found.", file=sys.stderr)
        return 2
    proc = subprocess.run(["diff", "-u", str(env_old), str(env_file)])
    # diff returns 1 when files differ; surface that, but never treat it as a
    # hard failure of the command itself.
    return 0 if proc.returncode in (0, 1) else proc.returncode


_SECRET_RE = re.compile(r"PASSWORD|SECRET|JWT_PRIVATE_KEY")
_SERVICE_VAR_RE = re.compile(r"^ORES_([A-Z0-9]+)_SERVICE_DB_(USER|PASSWORD|DATABASE)$")


def _is_secret(key: str) -> bool:
    return key == "PGPASSWORD" or bool(_SECRET_RE.search(key))


def _mask(key: str, value: str, show_secrets: bool) -> str:
    if show_secrets or not _is_secret(key):
        return value
    return f"<hidden:{len(value)} chars>" if value else "<empty>"


def list_env(project_root: Path, show_secrets: bool) -> int:
    """compass env list — show .env variables grouped, secrets masked by default.

    Parses the .env's own comment-delimited sections as group headings, and
    sub-groups the per-service credentials block by service name.
    """
    env_file = project_root / ".env"
    if not env_file.is_file():
        print(f"No .env found at {env_file}. Run 'compass env init' first.",
              file=sys.stderr)
        return 2

    lines = env_file.read_text().splitlines()
    is_rule = lambda s: s.strip().startswith("# ---")
    sections = []          # [(title, [(key, value)])]
    title = "General"
    rows = []
    pending = None
    for idx, line in enumerate(lines):
        if is_rule(line):
            continue
        if line.lstrip().startswith("#"):
            if idx > 0 and is_rule(lines[idx - 1]):
                pending = line.lstrip("# ").rstrip()
            continue
        if "=" not in line:
            continue
        if pending is not None:
            if rows:
                sections.append((title, rows))
            title, rows, pending = pending, [], None
        key, _, value = line.partition("=")
        rows.append((key, value))
    if rows:
        sections.append((title, rows))

    hidden = 0
    for title, rows in sections:
        print(f"\n\033[1m{title}\033[0m" if sys.stdout.isatty() else f"\n{title}")
        is_service_block = any(_SERVICE_VAR_RE.match(k) for k, _ in rows)
        last_svc = None
        for key, value in rows:
            m = _SERVICE_VAR_RE.match(key)
            if is_service_block and m and m.group(1) != last_svc:
                last_svc = m.group(1)
                print(f"  [{last_svc.lower()}]")
            shown = _mask(key, value, show_secrets)
            if _is_secret(key) and not show_secrets:
                hidden += 1
            indent = "    " if (is_service_block and m) else "  "
            print(f"{indent}{key} = {shown}")

    if hidden and not show_secrets:
        print(f"\n{hidden} secret value(s) hidden — pass --show-secrets to reveal.")
    return 0


def run(argv, project_root: Path) -> int:
    parser = argparse.ArgumentParser(
        prog="compass env init",
        description="Initialise the checkout environment (.env + NATS certs + IAM key).")
    parser.add_argument("-y", "--yes", action="store_true",
                        help="Skip the overwrite confirmation prompt")
    parser.add_argument("--preset",
                        help="Build preset name (e.g. linux-clang-debug-ninja)")
    parser.add_argument("--enable-logging", nargs="?", const="debug", metavar="LEVEL",
                        help="Enable test logging (trace/debug/info/warn/error; default debug)")
    parser.add_argument("--disable-logging", action="store_true",
                        help="Disable test logging")
    parser.add_argument("--with-diff", action="store_true",
                        help="After writing, show the unified diff of .env.old vs .env")
    args = parser.parse_args(argv)

    checkout_root = project_root
    env_file = checkout_root / ".env"
    script_dir = checkout_root / "build" / "scripts"

    # Logging-only mode — short-circuit before any credential work.
    if args.enable_logging is not None:
        return _logging_only(env_file, "enable", args.enable_logging)
    if args.disable_logging:
        return _logging_only(env_file, "disable", "debug")

    preset = args.preset or ""
    if not preset and not os.environ.get("CI"):
        print("Error: --preset is required.\n"
              "  Example: compass env init --preset linux-clang-debug-ninja",
              file=sys.stderr)
        return 1

    # Identity from the checkout directory name (OreStudio.<label>).
    dir_name = checkout_root.name
    label = dir_name[len("OreStudio."):] if dir_name.startswith("OreStudio.") else dir_name

    db_name = os.environ.get("ORES_DATABASE_NAME", f"ores_dev_{label}")
    nats_prefix = os.environ.get("ORES_NATS_SUBJECT_PREFIX", f"ores.dev.{label}")

    m = re.search(r"(\d*)$", label)
    label_suffix = m.group(1) if m else ""
    if label_suffix:
        nats_port = 42220 + int(label_suffix)
        nats_monitor_port = 8220 + int(label_suffix)
    else:
        nats_port = 42229
        nats_monitor_port = 8229
    nats_url = f"nats://localhost:{nats_port}"
    nats_monitor_url = f"http://localhost:{nats_monitor_port}"

    base_port = {"remote": 50000, "local1": 51000, "local2": 52000,
                 "local3": 53000, "local4": 54000, "local5": 55000}.get(label, 51000)
    if "release" in preset:
        http_port = base_port + 1
        wt_port = base_port + 3
    else:
        http_port = base_port + 0
        wt_port = base_port + 2
    # Site preview port: preset-independent — 8000 + numeric suffix, else 8000.
    site_port = 8000 + int(label_suffix) if label_suffix else 8000

    nats_store_dir = checkout_root / "build" / "nats" / label / "jetstream"
    nats_certs_dir = checkout_root / "build" / "keys" / "nats"

    # NATS mTLS certificates (idempotent; the script preserves existing files).
    print("--- NATS certificates ---")
    subprocess.run([str(script_dir / "generate_nats_certs.sh")], check=True)

    nats_tls_ca = nats_tls_cert = nats_tls_key = ""
    if (nats_certs_dir / "ca.crt").is_file():
        nats_tls_ca = str(nats_certs_dir / "ca.crt")
        nats_tls_cert = str(nats_certs_dir / "ores.qt.client.crt")
        nats_tls_key = str(nats_certs_dir / "ores.qt.client.key")

    label_lower = label.lower().replace(".", "_").replace("-", "_")

    # IAM RSA signing key (preset-independent).
    keys_dir = checkout_root / "build" / "keys"
    iam_key = keys_dir / "iam-rsa-private.pem"
    if not iam_key.is_file():
        print("Generating IAM RSA-2048 signing key...")
        keys_dir.mkdir(parents=True, exist_ok=True)
        subprocess.run(["openssl", "genrsa", "-out", str(iam_key), "2048"], check=True)
        iam_key.chmod(0o600)
        print(f"  Written to {iam_key}")

    existing = _read_env(env_file)

    # Overwrite guard.
    if env_file.is_file() and not args.yes:
        ans = input(f".env already exists at {env_file}.\n"
                    "Existing passwords will be reused.\nContinue? [y/N] ")
        if ans not in ("y", "Y"):
            print("Aborted.")
            return 1

    # Postgres superuser password: env var > existing > prompt.
    if os.environ.get("PGPASSWORD"):
        pgpassword = os.environ["PGPASSWORD"]
    elif existing.get("PGPASSWORD"):
        pgpassword = existing["PGPASSWORD"]
        print("Reusing existing PGPASSWORD.")
    else:
        import getpass
        pgpassword = getpass.getpass("Enter the postgres superuser password: ")
        if not pgpassword:
            print("Error: postgres password cannot be empty.", file=sys.stderr)
            return 1

    registry = (checkout_root / "projects" / "ores.codegen" / "models" /
                "services" / "ores_services_service_registry.json")
    if not registry.is_file():
        print(f"Error: service registry not found: {registry}", file=sys.stderr)
        return 1
    service_components = _service_names(registry)
    if not service_components:
        print(f"Error: no services read from registry {registry}", file=sys.stderr)
        return 1
    print(f"Detected services: {' '.join(service_components)}")

    # Role / user names — always derived from the label, never read from .env.
    owner_role = f"ores_{label_lower}_owner"
    rw_role = f"ores_{label_lower}_rw"
    ro_role = f"ores_{label_lower}_ro"
    service_role = f"ores_{label_lower}_service"
    ddl_user = f"ores_{label_lower}_ddl_user"
    cli_user = f"ores_{label_lower}_cli_user"
    wt_user = f"ores_{label_lower}_wt_user"
    http_user = f"ores_{label_lower}_http_user"
    shell_user = f"ores_{label_lower}_shell_user"
    compute_wrapper_user = f"ores_{label_lower}_compute_wrapper_user"
    readonly_user = f"ores_{label_lower}_readonly_user"
    test_ddl_user = f"ores_{label_lower}_test_ddl_user"
    test_dml_user = f"ores_{label_lower}_test_dml_user"

    db_host = existing.get("ORES_DB_HOST") or "localhost"

    print("Resolving passwords...")
    ddl_pw = _get_or_gen(existing, "ORES_DB_DDL_PASSWORD")
    cli_pw = _get_or_gen(existing, "ORES_DB_CLI_PASSWORD")
    wt_pw = _get_or_gen(existing, "ORES_DB_WT_PASSWORD")
    http_pw = _get_or_gen(existing, "ORES_DB_HTTP_PASSWORD")
    shell_pw = _get_or_gen(existing, "ORES_DB_SHELL_PASSWORD")
    readonly_pw = _get_or_gen(existing, "ORES_DB_READONLY_PASSWORD")
    test_ddl_pw = _get_or_gen(existing, "ORES_TEST_DB_DDL_PASSWORD")
    test_pw = _get_or_gen(existing, "ORES_TEST_DB_PASSWORD")
    http_jwt_secret = _get_or_gen(existing, "ORES_HTTP_SERVER_JWT_SECRET")

    service_pw = {c: _get_or_gen(existing, f"ORES_{_upper(c)}_SERVICE_DB_PASSWORD")
                  for c in service_components}
    grid_node_ids = [_get_or_gen_uuid(existing, f"ORES_GRID_NODE_{n}_HOST_ID")
                     for n in range(1, 6)]
    print("All credentials resolved.")

    # PEM as a single line with literal \n separators (awk '{printf "%s\\n",$0}').
    jwt_key_oneline = "".join(l + "\\n" for l in iam_key.read_text().splitlines())

    if env_file.is_file():
        (env_file.parent / ".env.old").write_text(env_file.read_text())
        print("Backed up existing .env to .env.old")

    env_version = current_version(checkout_root)
    print(f"Writing {env_file}...")
    ts = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC")
    out = []
    out.append(f"""\
# ORE Studio Environment: {label}
# Generated by compass env init on {ts}
# DO NOT COMMIT — this file contains secrets.

# ---------------------------------------------------------------------------
# .env format version — checked by projects/ores.sql/utility/validate_env_version.sh
# at the start of recreate_database.sh.  If this file is out of date, re-run:
#   compass env init --preset <preset> -y
# ---------------------------------------------------------------------------
ORES_ENV_VERSION={env_version}

# ---------------------------------------------------------------------------
# Checkout identity
# ---------------------------------------------------------------------------
ORES_CHECKOUT_LABEL={label}
""")
    if preset:
        out.append(f"ORES_PRESET={preset}\n")
    out.append(f"""\
ORES_DATABASE_NAME={db_name}

# ---------------------------------------------------------------------------
# NATS (per-environment: port derived from label suffix)
# Run build/scripts/init-nats.sh to generate the server config and store dir.
# ---------------------------------------------------------------------------
ORES_HTTP_PORT={http_port}
ORES_CONTROLLER_SERVICE_HTTP_PORT={http_port}
ORES_CONTROLLER_SERVICE_WT_PORT={wt_port}
ORES_SITE_PORT={site_port}
ORES_NATS_PORT={nats_port}
ORES_NATS_URL={nats_url}
ORES_NATS_MONITOR_PORT={nats_monitor_port}
ORES_NATS_MONITOR_URL={nats_monitor_url}
ORES_NATS_SUBJECT_PREFIX={nats_prefix}
ORES_NATS_STORE_DIR={nats_store_dir}
# mTLS: auto-enabled when certificates exist in build/keys/nats/.
# Run build/scripts/generate_nats_certs.sh to generate them.
# ORES_NATS_TLS_CERT/KEY are used by the Qt desktop client.
ORES_NATS_TLS_CA={nats_tls_ca}
ORES_NATS_TLS_CERT={nats_tls_cert}
ORES_NATS_TLS_KEY={nats_tls_key}

# ---------------------------------------------------------------------------
# Database admin (postgres superuser — for recreate_database.sh and psql)
# ---------------------------------------------------------------------------
PGPASSWORD={pgpassword}
ORES_DB_HOST={db_host}
ORES_TEST_DB_DATABASE={db_name}
ORES_TEST_DB_HOST={db_host}

# ---------------------------------------------------------------------------
# Database roles and users (env-prefixed for isolation between environments)
# ---------------------------------------------------------------------------
ORES_DB_OWNER_ROLE={owner_role}
ORES_DB_RW_ROLE={rw_role}
ORES_DB_RO_ROLE={ro_role}
ORES_DB_SERVICE_ROLE={service_role}
ORES_DB_DDL_USER={ddl_user}
ORES_DB_CLI_USER={cli_user}
ORES_DB_WT_USER={wt_user}
ORES_DB_HTTP_USER={http_user}
ORES_DB_SHELL_USER={shell_user}
ORES_DB_READONLY_USER={readonly_user}
ORES_TEST_DB_DDL_USER={test_ddl_user}

# ---------------------------------------------------------------------------
# Script / DDL passwords (used by recreate_database.sh)
# ---------------------------------------------------------------------------
ORES_DB_DDL_PASSWORD={ddl_pw}
ORES_DB_CLI_PASSWORD={cli_pw}
ORES_DB_WT_PASSWORD={wt_pw}
ORES_DB_HTTP_PASSWORD={http_pw}
ORES_DB_SHELL_PASSWORD={shell_pw}
ORES_DB_READONLY_PASSWORD={readonly_pw}

# ---------------------------------------------------------------------------
# Test connection credentials (read by recreate_database.sh and C++ tests)
# ---------------------------------------------------------------------------
ORES_TEST_DB_USER={test_dml_user}
ORES_TEST_DB_PASSWORD={test_pw}
ORES_TEST_DB_DDL_PASSWORD={test_ddl_pw}

""")

    # NATS service DB credentials.
    out.append("\n# ---------------------------------------------------------------------------\n")
    out.append("# NATS service DB credentials (read by C++ make_mapper)\n")
    out.append("# ---------------------------------------------------------------------------\n")
    for c in service_components:
        up = _upper(c)
        out.append("\n")
        out.append(f"ORES_{up}_SERVICE_DB_USER=ores_{label_lower}_{c}_service\n")
        out.append(f"ORES_{up}_SERVICE_DB_PASSWORD={service_pw[c]}\n")
        out.append(f"ORES_{up}_SERVICE_DB_DATABASE={db_name}\n")
    out.append(f"""
# ---------------------------------------------------------------------------
# CLI DB credentials (read by C++ make_mapper("CLI"))
# ---------------------------------------------------------------------------
ORES_CLI_DB_USER={cli_user}
ORES_CLI_DB_PASSWORD={cli_pw}
ORES_CLI_DB_DATABASE={db_name}

# ---------------------------------------------------------------------------
# Shell DB credentials (read by C++ make_mapper("SHELL"))
# ---------------------------------------------------------------------------
ORES_SHELL_DB_USER={shell_user}
ORES_SHELL_DB_PASSWORD={shell_pw}
ORES_SHELL_DB_DATABASE={db_name}

# ---------------------------------------------------------------------------
# Shell NATS credentials (read by C++ make_mapper("SHELL"))
# ---------------------------------------------------------------------------
ORES_SHELL_NATS_URL={nats_url}
ORES_SHELL_NATS_SUBJECT_PREFIX={nats_prefix}
ORES_SHELL_NATS_TLS_CA={nats_tls_ca}
ORES_SHELL_NATS_TLS_CERT={nats_tls_cert}
ORES_SHELL_NATS_TLS_KEY={nats_tls_key}

# ---------------------------------------------------------------------------
# HTTP server DB credentials (read by C++ make_mapper("HTTP_SERVER"))
# ---------------------------------------------------------------------------
ORES_HTTP_SERVER_DB_USER={http_user}
ORES_HTTP_SERVER_DB_PASSWORD={http_pw}
ORES_HTTP_SERVER_DB_DATABASE={db_name}
ORES_HTTP_SERVER_JWT_SECRET={http_jwt_secret}

# ---------------------------------------------------------------------------
# WT service DB credentials (read by C++ make_mapper("WT"))
# ---------------------------------------------------------------------------
ORES_WT_DB_USER={wt_user}
ORES_WT_DB_PASSWORD={wt_pw}
ORES_WT_DB_DATABASE={db_name}

# ---------------------------------------------------------------------------
# Compute Wrapper IAM service account name (no DB connection — NATS/TLS only)
# ---------------------------------------------------------------------------
ORES_DB_COMPUTE_WRAPPER_USER={compute_wrapper_user}

# ---------------------------------------------------------------------------
# IAM JWT signing key (PEM encoded as single line, \\n = newline)
# Note: excluded from GITHUB_ENV export since services don't run in CI.
# ---------------------------------------------------------------------------
ORES_IAM_SERVICE_JWT_PRIVATE_KEY="{jwt_key_oneline}"

# ---------------------------------------------------------------------------
# Compute wrapper node host IDs (one per test node)
# Each ID must match a host record in the compute.hosts table.
# Re-run recreate_database.sh after regenerating to keep IDs in sync.
# ---------------------------------------------------------------------------
""")
    for i, gid in enumerate(grid_node_ids, start=1):
        out.append(f"ORES_GRID_NODE_{i}_HOST_ID={gid}\n")

    # Preserve any previously-set logging vars.
    print("Checking for existing logging configuration...")
    log_level = existing.get("ORES_TEST_LOG_LEVEL")
    if log_level:
        print(f"Preserving test logging (level={log_level})")
        log_console = existing.get("ORES_TEST_LOG_CONSOLE") or "true"
        out.append(f"\n# Test logging\nORES_TEST_LOG_LEVEL={log_level}\n"
                   f"ORES_TEST_LOG_CONSOLE={log_console}\n")
    else:
        print("No test logging configuration found; skipping.")

    print("Setting file permissions...")
    env_file.write_text("".join(out))
    env_file.chmod(0o600)
    print("Done.")

    # NATS setup — per-environment server config + JetStream store dir.
    print("\n--- NATS setup ---")
    subprocess.run([str(script_dir / "init-nats.sh")], check=True)

    print(f"\n=== Environment initialised for '{label}' "
          f"(db: {db_name}, NATS port: {nats_port}) ===\n")
    print("Next steps:")
    print("  1. Create the database (first time only):")
    print("       ./projects/ores.sql/recreate_database.sh -y\n")
    print("  2. Start NATS:")
    print(f"       nats-server --config build/config/nats-{label}.conf\n")
    print("  3. In Emacs, set up SQL connections:")
    print("       M-x ores-db/setup-connections\n")
    print("  4. Start services via prodigy — they read ORES_PRESET and "
          "credentials from .env.\n")
    print("Logging:")
    print("  Enable:  compass env init --enable-logging [level]")
    print("  Disable: compass env init --disable-logging")
    print("  (Re-run 'cmake --preset <preset>' after toggling logging)")

    if args.with_diff:
        print("\n--- .env.old → .env diff ---")
        diff(checkout_root)

    return 0
