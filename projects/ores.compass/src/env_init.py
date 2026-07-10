"""compass env configure — initialise the checkout environment.

Generates a .env file at the checkout root with all required credentials for
running ORE Studio services locally and in CI. Ported from
build/scripts/init-environment.sh; behaviour is preserved:

- Existing secrets are reused on re-runs; only missing variables are generated.
- --enable-logging / --disable-logging toggle test logging without touching
  any other variable.
- The NATS domain service list is read from the authored service registry
  (projects/ores.codegen/models/services/ores_services_service_registry.json),
  the same source that generates projects/ores.sql/service_vars.sh.

Dependencies: python3, openssl.  NATS cert generation is handled by the
nats_certs module (compass nats certs); NATS config + store setup is handled
by the nats_init module (compass nats init).
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
# `compass env configure` (writes ORES_ENV_VERSION) and the version check.
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


# All ports for an environment derive from a single base_port (spaced
# BASE_PORT_STEP apart), so every port an environment owns is identifiable at a
# glance from its base. These offsets are the single source of truth for that
# layout; keep them in sync with the services and the Qt client.
#
# Kept below the kernel's ephemeral port range (see
# /proc/sys/net/ipv4/ip_local_port_range, typically 32768-60999) so a fixed
# listener (e.g. the NATS monitor port) can never collide with an unrelated
# process's outbound connection being handed the same port as its OS-assigned
# ephemeral source port. BASE_PORT_START used to be 50000, squarely inside
# that range; existing environments keep their old ports via ORES_BASE_PORT
# in .env until migrated by hand.
BASE_PORT_START = 20000
BASE_PORT_STEP = 1000
HTTP_PORT_OFFSET_DEBUG = 0
HTTP_PORT_OFFSET_RELEASE = 1
WT_PORT_OFFSET_DEBUG = 2
WT_PORT_OFFSET_RELEASE = 3
SITE_PORT_OFFSET = 4
NATS_PORT_OFFSET = 5
NATS_MONITOR_PORT_OFFSET = 6


def _scan_ports(parent_dir: Path) -> tuple[int, int, int]:
    """Scan sibling worktrees for used base ports; return (base_port, nats_port, nats_monitor_port).

    All ports for an environment derive from a single base_port (spaced
    BASE_PORT_STEP apart) so every port owned by an environment is identifiable
    at a glance via the *_PORT_OFFSET constants: http, wt, site, NATS client,
    NATS monitor. Scans both ores_dev_* and legacy OreStudio.* directories for
    ORES_BASE_PORT so a stray legacy checkout can't collide silently.
    """
    used_base: set = set()
    for pattern in ("ores_dev_*/.env", "OreStudio.*/.env"):
        for env_file in parent_dir.glob(pattern):
            d = _read_env(env_file)
            if d.get("ORES_BASE_PORT"):
                try:
                    used_base.add(int(d["ORES_BASE_PORT"]))
                except ValueError:
                    pass
    base_port = BASE_PORT_START
    while base_port in used_base:
        base_port += BASE_PORT_STEP
    nats_port = base_port + NATS_PORT_OFFSET
    nats_monitor_port = base_port + NATS_MONITOR_PORT_OFFSET
    return base_port, nats_port, nats_monitor_port


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
    """Service names from the org-mode service registry: each service is a
    top-level heading (=* <name>=); level-2 headings (DML/Select prefixes)
    are ignored."""
    names = []
    for line in registry.read_text().splitlines():
        m = re.match(r"^\* (\S+)\s*$", line)
        if m and m.group(1) not in excludes:
            names.append(m.group(1))
    return names


def _logging_only(env_file: Path, op: str, level: str) -> int:
    """--enable-logging / --disable-logging: touch only the logging vars."""
    if not env_file.is_file():
        print(f"Error: {env_file} does not exist. Run 'compass env configure' first.",
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
    print("Picked up automatically on the next build/test run "
          "(.env is a CMake configure dependency).")
    return 0


def new_version(project_root: Path, description: str) -> int:
    """compass env new-version — append a new .env-format version row.

    Computes the next version (current + 1), appends a dated row to the
    version log table with the given description, and writes it back. Run
    this after a change that alters the .env (a new service in the registry,
    a new/renamed variable); then re-run 'compass env configure' so existing
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
    print("Now re-run 'compass env configure --preset <preset> -y' to write the new "
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
        print("No .env.old found — run 'compass env configure' at least twice to "
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
        print(f"No .env found at {env_file}. Run 'compass env configure' first.",
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


def _run_install_packages(checkout_root: Path, provision_type: str,
                          force: bool, skip: bool) -> int:
    """Offer to run compass env install-packages based on provision_type.

    force=True  → install without prompting (--install-packages).
    skip=True   → skip entirely (--skip-packages).
    Otherwise   → prompt on interactive TTY; print instructions otherwise.
    Full envs get --with-qt; light envs get baseline only.
    """
    import env_packages

    with_qt = (provision_type == "full")
    flag_desc = "--with-qt" if with_qt else "baseline only"
    manual_cmd = ("compass env install-packages --with-qt" if with_qt
                  else "compass env install-packages")

    if skip:
        print(f"\nPackage installation skipped. To install later:\n  {manual_cmd}")
        return 0

    if force:
        print(f"\n--- Installing system packages ({flag_desc}) ---")
        return env_packages.install(checkout_root, with_qt=with_qt)

    if sys.stdin.isatty():
        ans = input(f"\nInstall system packages now? ({flag_desc}, requires sudo) [y/N] ")
        if ans in ("y", "Y"):
            print(f"--- Installing system packages ({flag_desc}) ---")
            return env_packages.install(checkout_root, with_qt=with_qt)
        print(f"Skipped. To install later:\n  {manual_cmd}")
    else:
        print(f"\nTo install system packages ({flag_desc}):\n  {manual_cmd}")

    return 0


def run(argv, project_root: Path) -> int:
    parser = argparse.ArgumentParser(
        prog="compass env configure",
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
    parser.add_argument("--compiler-cache", choices=("sccache", "ccache"),
                        help="Compiler cache to use for local builds (default: sccache, "
                             "or whatever is already in .env). CI always uses sccache "
                             "regardless of this setting.")
    pkg_grp = parser.add_mutually_exclusive_group()
    pkg_grp.add_argument("--install-packages", action="store_true",
                         help="Run compass env install-packages without prompting (requires sudo)")
    pkg_grp.add_argument("--skip-packages", action="store_true",
                         help="Skip the system package installation step")
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
              "  Example: compass env configure --preset linux-clang-debug-ninja",
              file=sys.stderr)
        return 1

    # Read existing .env early so ORES_ENV_NAME and pre-assigned ports can be
    # picked up before the derived values are computed.
    existing = _read_env(env_file)

    # Identity from the checkout directory name.
    # New style: ores_dev_festive_hawking → label = festive_hawking
    # Legacy style: OreStudio.local1 → label = local1
    dir_name = checkout_root.name
    if dir_name.startswith("ores_dev_"):
        label = dir_name[len("ores_dev_"):]
    elif dir_name.startswith("OreStudio."):
        label = dir_name[len("OreStudio."):]
    else:
        label = dir_name

    # ORES_ENV_NAME: explicit value set by `compass env provision`; falls back to label.
    env_name = existing.get("ORES_ENV_NAME") or label
    provision_type = existing.get("ORES_PROVISION_TYPE", "full")
    # Underscored form — safe for DB object names and NATS paths.
    label_lower = env_name.lower().replace(".", "_").replace("-", "_")

    # DB name: prefer explicit existing value (set by compass env provision or
    # a manual override), then derive from label_lower (fixes hyphen bug for
    # adjective-noun names like festive-hawking → ores_dev_festive_hawking).
    db_name = (os.environ.get("ORES_DATABASE_NAME")
               or existing.get("ORES_DATABASE_NAME")
               or f"ores_dev_{label_lower}")

    # NATS subject prefix: hyphens become dots (e.g. festive-hawking → ores.dev.festive.hawking).
    nats_prefix = (os.environ.get("ORES_NATS_SUBJECT_PREFIX")
                   or f"ores.dev.{env_name.replace('-', '.')}")

    # Ports: scan sibling environments to find the next free base_port slot,
    # then override with any value already in .env (pre-assigned by env
    # provision or a prior configure run). Scanning handles fresh clones that
    # bypass env provision. NATS ports are always re-derived from base_port
    # (like http_port/wt_port/site_port below) rather than read back from
    # .env, so a checkout still on the old independent 42221xxx/8221xxx
    # scheme actually migrates on its next `env configure` run.
    base_port, nats_port, nats_monitor_port = _scan_ports(checkout_root.parent)
    if existing.get("ORES_BASE_PORT"):
        try:
            base_port = int(existing["ORES_BASE_PORT"])
        except ValueError:
            print("Warning: invalid ORES_BASE_PORT in .env, using scanned value.")
    nats_port = base_port + NATS_PORT_OFFSET
    nats_monitor_port = base_port + NATS_MONITOR_PORT_OFFSET

    if "release" in preset:
        http_port = base_port + HTTP_PORT_OFFSET_RELEASE
        wt_port = base_port + WT_PORT_OFFSET_RELEASE
    else:
        http_port = base_port + HTTP_PORT_OFFSET_DEBUG
        wt_port = base_port + WT_PORT_OFFSET_DEBUG
    site_port = base_port + SITE_PORT_OFFSET

    nats_url = f"nats://localhost:{nats_port}"
    nats_monitor_url = f"http://localhost:{nats_monitor_port}"
    # Underscored label for filesystem paths — keeps a single canonical
    # nats-<label>.conf / build/nats/<label>/ regardless of whether
    # ORES_ENV_NAME uses hyphens (e.g. festive-hawking).
    nats_store_dir = checkout_root / "build" / "nats" / label_lower / "jetstream"
    nats_certs_dir = checkout_root / "build" / "keys" / "nats"

    # NATS mTLS certificates (idempotent; skips files that already exist).
    print("--- NATS certificates ---")
    import nats_certs
    nats_certs.generate(checkout_root)

    nats_tls_ca = nats_tls_cert = nats_tls_key = ""
    if (nats_certs_dir / "ca.crt").is_file():
        nats_tls_ca = str(nats_certs_dir / "ca.crt")
        nats_tls_cert = str(nats_certs_dir / "ores.qt.client.crt")
        nats_tls_key = str(nats_certs_dir / "ores.qt.client.key")

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
    env_type = existing.get("ORES_ENV_TYPE", "development")

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

    registry = (checkout_root / "projects" / "modeling" /
                "service_registry.org")
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

    # sccache config: dir and size are shared across all worktrees (one
    # server, one cache), so every checkout should agree on the same values.
    sccache_dir = (existing.get("SCCACHE_DIR")
                   or str(Path.home() / ".cache" / "sccache"))
    sccache_cache_size = existing.get("SCCACHE_CACHE_SIZE") or "50G"

    # SSH agent directory (optional): compass.sh exports SSH_AUTH_SOCK from
    # the sole socket in this directory when the calling environment does not
    # provide a live one (e.g. sandboxed LLM sessions).
    ssh_agent_dir = (existing.get("ORES_SSH_AGENT_DIR")
                     or str(Path.home() / ".ssh" / "agent"))

    # Build parallelism: per-machine tuning, not per-checkout, so always
    # preserve whatever is already in .env; only default it on first write.
    cmake_build_parallel_level = existing.get("CMAKE_BUILD_PARALLEL_LEVEL") or "2"

    # Compiler cache: local dev choice between sccache/ccache, read by
    # build/scripts/compiler_cache_wrapper.sh. CI never reads this (it sets
    # SCCACHE_DIR directly and doesn't run `compass env configure`), so
    # defaulting to sccache here has no effect on CI behaviour.
    compiler_cache = (args.compiler_cache
                       or existing.get("ORES_COMPILER_CACHE")
                       or "sccache")

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
# ORE Studio Environment: {env_name}
# Generated by compass env configure on {ts}
# DO NOT COMMIT — this file contains secrets.

# ---------------------------------------------------------------------------
# .env format version — checked by projects/ores.sql/utility/validate_env_version.sh
# at the start of compass db recreate.  If this file is out of date, re-run:
#   compass env configure --preset <preset> -y
# ---------------------------------------------------------------------------
ORES_ENV_VERSION={env_version}

# ---------------------------------------------------------------------------
# Checkout identity
# ---------------------------------------------------------------------------
ORES_ENV_NAME={env_name}
ORES_PROVISION_TYPE={provision_type}
ORES_CHECKOUT_LABEL={label_lower}
ORES_ENV_TYPE={env_type}
ORES_BASE_PORT={base_port}
""")
    if preset:
        out.append(f"ORES_PRESET={preset}\n")
    out.append(f"""\
ORES_DATABASE_NAME={db_name}

# ---------------------------------------------------------------------------
# sccache: cache dir + size are shared across all worktrees — one server, one
# cache. build/scripts/sccache_wrapper.sh reads these and exports them before
# the server first starts, so a change only takes effect on the next build
# after a server restart:
#   sccache --stop-server
# Change here and re-run compass env configure everywhere to keep worktrees
# in sync, or edit directly.
# ---------------------------------------------------------------------------
SCCACHE_DIR={sccache_dir}
SCCACHE_CACHE_SIZE={sccache_cache_size}

# ---------------------------------------------------------------------------
# SSH agent directory (optional) — compass.sh exports SSH_AUTH_SOCK from the
# sole socket in this directory when the calling environment does not provide
# a live one (e.g. sandboxed LLM sessions).
# ---------------------------------------------------------------------------
ORES_SSH_AGENT_DIR={ssh_agent_dir}

# ---------------------------------------------------------------------------
# Build parallelism (rotational-disk / sccache contention keeps this low on
# this machine). Read by `compass build` as the default -j when --jobs is
# not passed explicitly.
# ---------------------------------------------------------------------------
CMAKE_BUILD_PARALLEL_LEVEL={cmake_build_parallel_level}

# ---------------------------------------------------------------------------
# Compiler cache for local builds (sccache or ccache). Read by
# build/scripts/compiler_cache_wrapper.sh. Change with:
#   compass env configure --compiler-cache {{sccache,ccache}}
# ---------------------------------------------------------------------------
ORES_COMPILER_CACHE={compiler_cache}

# ---------------------------------------------------------------------------
# NATS (per-environment: assigned by compass env create; preserved on re-run)
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
# Run `compass nats certs` to generate them.
# ORES_NATS_TLS_CERT/KEY are used by the Qt desktop client.
ORES_NATS_TLS_CA={nats_tls_ca}
ORES_NATS_TLS_CERT={nats_tls_cert}
ORES_NATS_TLS_KEY={nats_tls_key}

# ---------------------------------------------------------------------------
# Database admin (postgres superuser — for compass db recreate and psql)
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
# Script / DDL passwords (used by compass db recreate)
# ---------------------------------------------------------------------------
ORES_DB_DDL_PASSWORD={ddl_pw}
ORES_DB_CLI_PASSWORD={cli_pw}
ORES_DB_WT_PASSWORD={wt_pw}
ORES_DB_HTTP_PASSWORD={http_pw}
ORES_DB_SHELL_PASSWORD={shell_pw}
ORES_DB_READONLY_PASSWORD={readonly_pw}

# ---------------------------------------------------------------------------
# Test connection credentials (read by compass db recreate and C++ tests)
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
    # Front-end client apps that read their config from the environment via the
    # C++ make_mapper("<APP>") convention. `uses_db` marks whether the app opens
    # a direct database connection. NATS-only clients (the shell, like the
    # compute wrapper) must NOT get DB creds: ores.shell registers no db-*
    # options, so boost::program_options parse_environment would reject
    # ORES_SHELL_DB_* as unrecognised options. This is the per-client flag — the
    # client analogue of the backend service registry, which only catalogues
    # DB-writing NATS services.
    client_apps = [
        {"mapper": "CLI", "user": cli_user, "pw": cli_pw, "uses_db": True},
        {"mapper": "SHELL", "user": shell_user, "pw": shell_pw, "uses_db": False},
        {"mapper": "HTTP_SERVER", "user": http_user, "pw": http_pw, "uses_db": True},
        {"mapper": "WT", "user": wt_user, "pw": wt_pw, "uses_db": True},
    ]
    for app in client_apps:
        if not app["uses_db"]:
            continue
        m = app["mapper"]
        out.append(
            "\n# ---------------------------------------------------------------------------\n"
            f'# {m} DB credentials (read by C++ make_mapper("{m}"))\n'
            "# ---------------------------------------------------------------------------\n"
            f"ORES_{m}_DB_USER={app['user']}\n"
            f"ORES_{m}_DB_PASSWORD={app['pw']}\n"
            f"ORES_{m}_DB_DATABASE={db_name}\n")

    out.append(f"""
# ---------------------------------------------------------------------------
# Shell NATS credentials (read by C++ make_mapper("SHELL"))
# Note: the shell is a pure NATS client and has NO DB credentials (uses_db=False
# above) — it reaches data only through the backend services over NATS.
# ---------------------------------------------------------------------------
ORES_SHELL_NATS_URL={nats_url}
ORES_SHELL_NATS_SUBJECT_PREFIX={nats_prefix}
ORES_SHELL_NATS_TLS_CA={nats_tls_ca}
ORES_SHELL_NATS_TLS_CERT={nats_tls_cert}
ORES_SHELL_NATS_TLS_KEY={nats_tls_key}

# ---------------------------------------------------------------------------
# HTTP server JWT secret (read by C++ make_mapper("HTTP_SERVER"))
# ---------------------------------------------------------------------------
ORES_HTTP_SERVER_JWT_SECRET={http_jwt_secret}

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
# Re-run compass db recreate after regenerating to keep IDs in sync.
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
    # Use label_lower (underscored) so this matches `compass nats init`,
    # which keys off ORES_CHECKOUT_LABEL — one canonical nats-<label>.conf.
    print("\n--- NATS setup ---")
    import nats_init
    nats_init.generate(checkout_root, label_lower, nats_port, nats_monitor_port, nats_store_dir)

    print(f"\n=== Environment initialised for '{env_name}' "
          f"(db: {db_name}, NATS port: {nats_port}) ===\n")
    print("Next steps:")
    print("  1. Create the database (first time only):")
    print("       ./projects/ores.compass/compass.sh db recreate -y\n")
    print("  2. Start NATS:")
    print(f"       nats-server --config build/config/nats-{label_lower}.conf\n")
    print("  3. In Emacs, set up SQL connections:")
    print("       M-x ores-db/setup-connections\n")
    print("  4. Start services via prodigy — they read ORES_PRESET and "
          "credentials from .env.\n")
    print("Logging:")
    print("  Enable:  compass env configure --enable-logging [level]")
    print("  Disable: compass env configure --disable-logging")
    print("  (Re-run 'cmake --preset <preset>' after toggling logging)")

    if args.with_diff:
        print("\n--- .env.old → .env diff ---")
        diff(checkout_root)

    pkg_rc = _run_install_packages(checkout_root, provision_type,
                                   force=args.install_packages,
                                   skip=args.skip_packages)
    if pkg_rc != 0:
        return pkg_rc

    return 0
