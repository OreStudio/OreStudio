"""compass env configure — initialise the checkout environment.

Generates a .env file at the checkout root with all required credentials for
running ORE Studio services locally and in CI. Ported from
build/scripts/init-environment.sh; behaviour is preserved:

- Existing secrets are reused on re-runs; only missing variables are generated.
- --enable-logging / --disable-logging toggle test logging without touching
  any other variable.
- The NATS domain service list is read from the authored service registry
  (projects/modeling/service_registry.org), the same source that generates
  projects/ores.sql/service_vars.sh.

Dependencies: python3, openssl.  NATS cert generation is handled by the
nats_certs module (compass nats certs); NATS config + store setup is handled
by the nats_init module (compass nats init).
"""

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

def current_version(project_root: Path) -> int:
    """The .env format's current version, which compass-ng reads from
    compass/checks/env_format.org."""
    exe = shutil.which("compass-ng")
    if exe is None:
        raise RuntimeError("compass-ng is not on the PATH; it holds the .env format versions")
    out = subprocess.run([exe, "-C", str(project_root), "env", "version"],
                         capture_output=True, text=True, check=True).stdout
    return int(out.strip().splitlines()[-1])


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
# layout; keep them in sync with the services.
#
# Kept below the kernel's ephemeral port range (see
# /proc/sys/net/ipv4/ip_local_port_range, typically 32768-60999) so a fixed
# listener (e.g. the NATS monitor port) can never collide with an unrelated
# process's outbound connection being handed the same port as its OS-assigned
# ephemeral source port. BASE_PORT_START used to be 50000, squarely inside
# that range; existing environments keep their old ports via ORES_BASE_PORT
# in .env until migrated by hand.
BASE_PORT_START = 20000
# 200, not 1000. Only 7 offsets are actually used (0..6, see below), so a
# 1000-wide slot spent 993 ports per environment on nothing and capped the
# machine at 13 environments between BASE_PORT_START and
# EPHEMERAL_PORT_FLOOR -- a real ceiling once the btrfs migration (story
# 03831B51) dropped the disk cost of a built worktree to ~4G and made disk
# stop being the limiting factor. 200 still leaves ~28x headroom over the 7
# ports an environment claims, so a new offset can be added without
# revisiting this, and yields 64 slots.
#
# Existing environments are unaffected: they keep their allocated base via
# ORES_BASE_PORT in .env, and _scan_ports only picks a step-aligned base for
# *new* ones. Bases already handed out on the old 1000 spacing (20000, 21000,
# ...) simply remain claimed; the next allocation fills 20200.
BASE_PORT_STEP = 200
# Conservative floor for the kernel's ephemeral port range: the default is
# usually 32768 (see /proc/sys/net/ipv4/ip_local_port_range), but some
# systems start lower, so _scan_ports refuses to hand out a base_port whose
# top offset would land at or past this floor rather than silently
# reproducing the exact collision BASE_PORT_START was moved to avoid.
EPHEMERAL_PORT_FLOOR = 32768
# The TypeScript web interface (ores.web). Offset 2 held the retired Wt
# client, so it is free again: the BFF listens here and serves both the HTTP
# API and the built browser bundle.
NATS_PORT_OFFSET = 5
NATS_MONITOR_PORT_OFFSET = 6

# The ores.web site config declares one entry per environment instance. The
# BFF resolves its entry by id, so a checkout binds its label to a declared
# id through ORES_WEB_ENV.
WEB_ENVIRONMENTS_RELATIVE_PATH = (Path("projects") / "ores.web" / "config"
                                  / "environments.json")
# Hosts that mean "this machine". Several instances of one environment share
# a port and subject prefix and differ only by host, so the local instance is
# the one a local checkout serves.
WEB_LOCAL_HOSTS = frozenset({"localhost", "127.0.0.1", "::1"})


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
        if base_port + NATS_MONITOR_PORT_OFFSET >= EPHEMERAL_PORT_FLOOR:
            raise RuntimeError(
                f"No free base_port below the ephemeral port floor "
                f"({EPHEMERAL_PORT_FLOOR}) — {len(used_base)} environments "
                f"already claim a base_port from {BASE_PORT_START}. Free up "
                f"a stale checkout's .env, or raise BASE_PORT_STEP's ceiling "
                f"deliberately rather than let this wrap into the kernel's "
                f"ephemeral range.")
    nats_port = base_port + NATS_PORT_OFFSET
    nats_monitor_port = base_port + NATS_MONITOR_PORT_OFFSET
    return base_port, nats_port, nats_monitor_port


def _env_value(existing: dict, key: str, fallback: str) -> str:
    """Value for a generated .env entry, in precedence order.

    The checkout's own .env wins. The process environment is a fallback only
    for a checkout that has no value yet (a fresh provision, or CI), because
    an ORES_* exported by another worktree's shell must never rewrite this
    checkout's identity: the database name and the NATS subject prefix decide
    which environment a command touches, so a stale exported value can point
    a command at another worktree's database."""
    return existing.get(key) or os.environ.get(key) or fallback


def _underscored(label: str) -> str:
    """Underscored form of an environment label.

    The one canonical spelling every name built from the label uses, for the
    database, the postgres roles, the NATS store directory, and the identifiers
    a declared environment is compared against. It matters because a
    provisioned label carries hyphens -- env_create's _NAME_RE requires one --
    while every declared ores.web id uses underscores."""
    return label.lower().replace(".", "_").replace("-", "_")


def _resolve_web_env_id(checkout_root: Path, nats_port: int,
                        subject_prefix: str, env_name: str) -> str | None:
    """Id of the ores.web environment instance this checkout serves.

    The BFF resolves its site configuration by id, and it falls back to
    ORES_ENV_NAME -- the checkout label, which is not necessarily an id the
    web config declares. So a checkout whose label differs from its declared
    id (e.g. a label 'curie' against a declared 'curie_local') cannot start the
    web service without this binding.

    Match on the NATS port and subject prefix, which is how a declared entry
    anchors itself to an environment and which compass itself assigned; prefer
    an entry whose id is the label, then the local instance, since instances of
    one environment share both values and differ only by host.

    Returns None when the web component is absent or no entry matches, so a
    checkout without the web interface still configures cleanly."""
    path = checkout_root / WEB_ENVIRONMENTS_RELATIVE_PATH
    try:
        declared = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None
    entries = declared.get("environments") or []
    candidates = [entry for entry in entries
                  if entry.get("port") == nats_port
                  and entry.get("subjectPrefix") == subject_prefix]
    label = _underscored(env_name)
    for entry in candidates:
        if entry.get("id") == label:
            return entry["id"]
    local = [entry for entry in candidates
             if entry.get("host") in WEB_LOCAL_HOSTS]
    resolved = local or candidates
    if len(resolved) != 1:
        return None
    return resolved[0].get("id")


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


def _run_install_packages(checkout_root: Path, force: bool, skip: bool) -> int:
    """Offer to run compass env install-packages.

    force=True  → install without prompting (--install-packages).
    skip=True   → skip entirely (--skip-packages).
    Otherwise   → prompt on interactive TTY; print instructions otherwise.
    """
    import env_packages

    manual_cmd = "compass env install-packages"

    if skip:
        print(f"\nPackage installation skipped. To install later:\n  {manual_cmd}")
        return 0

    if force:
        print("\n--- Installing system packages ---")
        return env_packages.install(checkout_root)

    if sys.stdin.isatty():
        ans = input("\nInstall system packages now? (requires sudo) [y/N] ")
        if ans in ("y", "Y"):
            print("--- Installing system packages ---")
            return env_packages.install(checkout_root)
        print(f"Skipped. To install later:\n  {manual_cmd}")
    else:
        print(f"\nTo install system packages:\n  {manual_cmd}")

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
                        help="Compiler cache to use for local builds (default: ccache, "
                             "or whatever is already in .env). CI always uses sccache "
                             "regardless of this setting.")
    parser.add_argument("--use-busctl", action=argparse.BooleanOptionalAction,
                        default=None,
                        help="Transport compass uses to drive the fleet: --use-busctl "
                             "(default) goes through busctl, which reaches the user "
                             "manager where plain systemctl --user cannot, as inside "
                             "the DSH sandbox. --no-use-busctl selects plain systemctl. "
                             "Without either flag an existing .env value is kept.")
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

    # Read existing .env early so ORES_ENV_NAME and pre-assigned ports can be
    # picked up before the derived values are computed, and so --preset can
    # default to whatever is already configured rather than forcing the
    # caller to repeat it every time.
    existing = _read_env(env_file)

    preset = args.preset or existing.get("ORES_PRESET", "")
    if not preset and not os.environ.get("CI"):
        print("Error: --preset is required.\n"
              "  Example: compass env configure --preset linux-clang-debug-ninja",
              file=sys.stderr)
        return 1

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
    label_lower = _underscored(env_name)

    # DB name: prefer explicit existing value (set by compass env provision or
    # a manual override), then derive from label_lower (fixes hyphen bug for
    # adjective-noun names like festive-hawking → ores_dev_festive_hawking).
    db_name = _env_value(existing, "ORES_DATABASE_NAME",
                         f"ores_dev_{label_lower}")

    # NATS subject prefix: hyphens become dots (e.g. festive-hawking → ores.dev.festive.hawking).
    nats_prefix = _env_value(existing, "ORES_NATS_SUBJECT_PREFIX",
                             f"ores.dev.{env_name.replace('-', '.')}")

    # NATS wire payload format: json or msgpack, decided once at process
    # startup (see ores::nats::wire_format). Preserve an existing choice on
    # re-run; default to msgpack for fresh environments.
    nats_wire_format = _env_value(existing, "ORES_NATS_WIRE_FORMAT", "msgpack")

    # Ports: scan sibling environments to find the next free base_port slot,
    # then override with any value already in .env (pre-assigned by env
    # provision or a prior configure run). Scanning handles fresh clones that
    # bypass env provision. NATS ports are always re-derived from base_port
    # (like http_port/site_port below) rather than read back from
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
        nats_tls_cert = str(nats_certs_dir / "ores.shell.crt")
        nats_tls_key = str(nats_certs_dir / "ores.shell.key")

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

    # Postgres superuser password: existing > env var > prompt. The checkout's
    # .env wins for the same reason as the identity values above; an exported
    # PGPASSWORD is only a fallback where the checkout has none yet.
    pgpassword = existing.get("PGPASSWORD") or os.environ.get("PGPASSWORD")
    if pgpassword:
        print("Reusing existing PGPASSWORD.")
    else:
        import getpass
        pgpassword = getpass.getpass("Enter the postgres superuser password: ")
        if not pgpassword:
            print("Error: postgres password cannot be empty.", file=sys.stderr)
            return 1

    # compass-ng writes .env from compass/checks/env_format.org: every variable,
    # its default and the format version. This keeps choosing the environment.
    exe = shutil.which("compass-ng")
    if exe is None:
        print("Error: compass-ng is not on the PATH; it writes .env. "
              "See compass bearings for how to install it.", file=sys.stderr)
        return 1
    import env_activity
    fresh = not env_file.is_file()
    web_env = _resolve_web_env_id(checkout_root, nats_port, nats_prefix, env_name) or ""
    inputs = {
        "env_name": env_name,
        "label": label_lower,
        "base_port": str(base_port),
        "http_port_offset": "1" if "release" in preset else "0",
        "preset": preset,
        "env_activity": (str(env_activity.current_activity(checkout_root))
                         if fresh else "0"),
        "nats_subject_prefix": f"ores.dev.{env_name.replace('-', '.')}",
        "nats_tls_ca": nats_tls_ca,
        "nats_tls_cert": nats_tls_cert,
        "nats_tls_key": nats_tls_key,
        "web_env": web_env,
        "db_port": existing.get("ORES_DB_PORT", ""),
    }
    cmd = [exe, "-C", str(checkout_root), "env", "generate"]
    for key, value in inputs.items():
        cmd += ["--input", f"{key}={value}"]
    if args.compiler_cache:
        cmd += ["--var", f"ORES_COMPILER_CACHE={args.compiler_cache}"]
    if args.use_busctl is not None:
        cmd += ["--var", f"ORES_USE_BUSCTL={'1' if args.use_busctl else '0'}"]
    # PGPASSWORD goes through the environment, so it never appears in ps.
    rc = subprocess.run(cmd, env=dict(os.environ, PGPASSWORD=pgpassword)).returncode
    if rc:
        return rc

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
    print("  2. In Emacs, set up SQL connections:")
    print("       M-x ores-db/setup-connections\n")
    print("  3. Start the service fleet via systemd:")
    print("       ./projects/ores.compass/compass.sh services start\n")
    print("     This brings up NATS and every service unit "
          "(ores-<env>.target); starting an\n"
          "     individual unit (systemctl --user start "
          f"ores.<svc>.service-{label_lower}) pulls\n"
          "     in NATS automatically via its Requires= dependency. "
          "They read ORES_PRESET\n"
          "     and credentials from .env.\n")
    print("Logging:")
    print("  Enable:  compass env configure --enable-logging [level]")
    print("  Disable: compass env configure --disable-logging")
    print("  (Re-run 'cmake --preset <preset>' after toggling logging)")

    if args.with_diff:
        print("\n--- .env.old → .env diff ---")
        diff(checkout_root)

    pkg_rc = _run_install_packages(checkout_root,
                                   force=args.install_packages,
                                   skip=args.skip_packages)
    if pkg_rc != 0:
        return pkg_rc

    return 0
