# -*- coding: utf-8 -*-
"""compass db — Provision pillar: database lifecycle.

Native port of the projects/ores.sql shell scripts:

    recreate_database.sh  ->  compass db recreate
    setup_database.sh     ->  compass db setup
    drop_database.sh      ->  compass db drop
    run_sql.sh            ->  compass db sql
    reset_system.sh       ->  compass db reset-system
    reset_tenant.sh       ->  compass db reset-tenant
    (new)                 ->  compass db status

The .sql files in projects/ores.sql/ remain the single source of truth
for schema and seed logic; this module owns only the orchestration the
shell scripts used to do (env plumbing, validation, sequenced psql
invocations).
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile
import time
from pathlib import Path

PROTECTED_DBS = ("postgres", "template0", "template1")


# --- environment ------------------------------------------------------------

def load_env(project_root: Path) -> dict:
    """Parse .env into a dict (KEY=VALUE lines; quotes stripped)."""
    env_file = project_root / ".env"
    if not env_file.exists():
        print(f"error: .env not found at {env_file}", file=sys.stderr)
        print("       run: ./projects/ores.compass/compass.sh env configure "
              "--preset <preset>", file=sys.stderr)
        sys.exit(1)
    env = {}
    for line in env_file.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        value = value.strip()
        if len(value) >= 2 and value[0] == value[-1] and value[0] in "\"'":
            value = value[1:-1]
        env[key.strip()] = value
    return env


def validate_env_version(project_root: Path, env: dict):
    """Port of utility/validate_env_version.sh."""
    import env_init
    required = int(env_init.current_version(project_root))
    current = int(env.get("ORES_ENV_VERSION", "0") or "0")
    if current > required:
        print(f"Warning: .env version {current} is newer than required "
              f"{required} — proceeding.", file=sys.stderr)
        return
    if current < required:
        preset = env.get("ORES_PRESET", "<preset>")
        print(f"""
=== ERROR: .env is out of date ===

  Your .env:  version {current}
  Required:   version {required}

See doc/knowledge/architecture/env_format_version_log.org for details.

Fix: re-run 'compass env configure' to regenerate .env:
  ./projects/ores.compass/compass.sh env configure --preset {preset} -y
""", file=sys.stderr)
        sys.exit(1)


def service_names(project_root: Path):
    """Parse SERVICE_NAMES from projects/ores.sql/service_vars.sh (generated)."""
    text = (project_root / "projects/ores.sql/service_vars.sh").read_text()
    m = re.search(r"SERVICE_NAMES=\((.*?)\)", text, re.S)
    if not m:
        print("error: could not parse SERVICE_NAMES from service_vars.sh",
              file=sys.stderr)
        sys.exit(1)
    names = []
    for line in m.group(1).splitlines():
        line = line.split("#")[0].strip()
        names.extend(line.split())
    return names


# --- psql helpers -----------------------------------------------------------

def _psql(env, *args, user="postgres", password=None, database=None,
          check=True, capture=False, cwd=None):
    cmd = ["psql", "-h", env.get("ORES_DB_HOST", "localhost"), "-U", user]
    if database:
        cmd += ["-d", database]
    cmd += list(args)
    e = os.environ.copy()
    e.update(env)
    if password is not None:
        e["PGPASSWORD"] = password
    # cwd matters: the .sql entry points use relative \i includes, so they
    # must run from projects/ores.sql (the shell scripts cd'd there).
    return subprocess.run(cmd, env=e, check=check, cwd=cwd,
                          capture_output=capture, text=True)


def check_connections(env, db_name, kill=False) -> bool:
    """Port of utility/check_db_connections.sh + kill_db_connections.sh."""
    pw = env.get("PGPASSWORD", "")
    if kill:
        _psql(env, "-At", "-c",
              f"SELECT pg_terminate_backend(pid) FROM pg_stat_activity "
              f"WHERE datname = '{db_name}' AND pid <> pg_backend_pid();",
              password=pw, check=False, capture=True)
        time.sleep(0.5)
    out = _psql(env, "-At", "-c",
                f"SELECT count(*) FROM pg_stat_activity WHERE datname = "
                f"'{db_name}' AND pid <> pg_backend_pid();",
                password=pw, check=False, capture=True)
    count = (out.stdout or "").strip()
    if not count:
        return True  # database may not exist — fine for recreation
    if int(count) > 0:
        print(f"\nERROR: there are {count} active connection(s) to "
              f"{db_name}.", file=sys.stderr)
        print("Hint: pass --kill (-k) to terminate them, or stop the "
              "services first (compass services stop).", file=sys.stderr)
        return False
    return True


def _confirm(prompt, assume_yes):
    if assume_yes:
        return True
    reply = input(f"{prompt} Type 'yes' to proceed: ")
    return reply.strip() == "yes"


def _svc_psql_args(env, names, with_passwords):
    args = []
    for svc in names:
        upper = svc.upper() + "_SERVICE"
        args += ["-v", f"{svc}_service_user={env.get(f'ORES_{upper}_DB_USER', '')}"]
        if with_passwords:
            args += ["-v",
                     f"{svc}_service_password={env.get(f'ORES_{upper}_DB_PASSWORD', '')}"]
    return args


def database_info(env):
    """Latest ores_database_info_tbl row, or None when unreachable.

    Returns {restored_at, schema_version, git_commit, git_date}. Used by
    compass bearings for the environment status; degrades silently."""
    db_name = env.get("ORES_TEST_DB_DATABASE", "")
    pw = env.get("PGPASSWORD", "")
    if not db_name or not pw:
        return None
    try:
        out = _psql(env, "-At", "-c",
                    "SELECT to_char(created_at, 'YYYY-MM-DD HH24:MI'), "
                    "schema_version, git_commit, git_date "
                    "FROM ores_database_info_tbl "
                    "ORDER BY created_at DESC LIMIT 1;",
                    password=pw, database=db_name, check=False, capture=True)
    except OSError:
        return None
    line = (out.stdout or "").strip()
    if out.returncode or not line:
        return None
    restored, schema, commit, git_date = (line.split("|") + ["", "", ""])[:4]
    return {"restored_at": restored, "schema_version": schema,
            "git_commit": commit, "git_date": git_date}


# --- subcommands ------------------------------------------------------------

def cmd_sql(project_root, env, args, passthrough):
    """Port of run_sql.sh."""
    if args.user == "postgres":
        user = "postgres"
        password = env.get("PGPASSWORD", "")
    else:  # ddl
        user = env.get("ORES_DB_DDL_USER", "")
        password = env.get("ORES_DB_DDL_PASSWORD", "")
        if not user or not password:
            print("error: ORES_DB_DDL_USER / ORES_DB_DDL_PASSWORD not set "
                  "in .env", file=sys.stderr)
            return 1
    if not password:
        print("error: PGPASSWORD not set and not found in .env",
              file=sys.stderr)
        return 1
    db_name = env.get("ORES_TEST_DB_DATABASE", "")
    if not db_name:
        print("error: ORES_TEST_DB_DATABASE not set in .env", file=sys.stderr)
        return 1
    host = env.get("ORES_TEST_DB_HOST", "localhost")
    e = os.environ.copy()
    e.update(env)
    e["PGPASSWORD"] = password
    return subprocess.run(
        ["psql", "-h", host, "-U", user, "-d", db_name] + passthrough,
        env=e).returncode


def cmd_drop(project_root, env, args):
    """Port of drop_database.sh."""
    db_name = args.database
    if db_name in PROTECTED_DBS:
        print(f"error: cannot drop protected database: {db_name}",
              file=sys.stderr)
        return 1
    pw = env.get("PGPASSWORD", "")
    if not pw:
        print("error: PGPASSWORD is required (set via .env)", file=sys.stderr)
        return 1
    out = _psql(env, "-At", "-c",
                f"SELECT 1 FROM pg_database WHERE datname = '{db_name}';",
                password=pw, check=False, capture=True)
    if not (out.stdout or "").strip():
        print(f"Database '{db_name}' does not exist.")
        return 0
    if not check_connections(env, db_name, kill=args.kill):
        return 1
    if not _confirm(f"WARNING: this will DROP database '{db_name}'.",
                    args.yes):
        print("Aborted.")
        return 1
    _psql(env, "-c", f'DROP DATABASE IF EXISTS "{db_name}";', password=pw)
    print(f"Dropped database '{db_name}'.")
    return 0


def cmd_setup(project_root, env, args):
    """Port of setup_database.sh: create db, schema, metadata."""
    sql_dir = project_root / "projects/ores.sql"
    db_name = args.database
    ddl_user = env.get("ORES_DB_DDL_USER", "")
    ddl_password = env.get("ORES_DB_DDL_PASSWORD", "")
    for var, val in [("ORES_DB_DDL_USER", ddl_user),
                     ("ORES_DB_DDL_PASSWORD", ddl_password)]:
        if not val:
            print(f"error: {var} must be set", file=sys.stderr)
            return 1
    roles = {name: env.get(f"ORES_DB_{name}", "")
             for name in ("OWNER_ROLE", "RW_ROLE", "RO_ROLE", "SERVICE_ROLE")}
    for name, val in roles.items():
        if not val:
            print(f"error: ORES_DB_{name} must be set", file=sys.stderr)
            return 1
    pw = env.get("PGPASSWORD", "")
    names = service_names(project_root)

    print(f"--- Phase 1: Creating database {db_name} ---")
    _psql(env, "--set", "ON_ERROR_STOP=on",
          "-v", f"db_name={db_name}",
          "-v", f"owner_role={roles['OWNER_ROLE']}",
          "-v", f"rw_role={roles['RW_ROLE']}",
          "-v", f"ro_role={roles['RO_ROLE']}",
          "-v", f"service_role={roles['SERVICE_ROLE']}",
          "-v", f"ddl_user={ddl_user}",
          "-f", "create_database.sql", password=pw, cwd=str(sql_dir))

    print("--- Phase 2: Setting up schema ---")
    skip = "on" if args.skip_validation else "off"
    schema_args = [
        "--set", "ON_ERROR_STOP=on",
        "-v", f"skip_validation={skip}",
        "-v", f"owner_role={roles['OWNER_ROLE']}",
        "-v", f"rw_role={roles['RW_ROLE']}",
        "-v", f"ro_role={roles['RO_ROLE']}",
        "-v", f"ddl_user={ddl_user}",
        "-v", f"cli_user={env.get('ORES_DB_CLI_USER', '')}",
        "-v", f"wt_user={env.get('ORES_DB_WT_USER', '')}",
        "-v", f"shell_user={env.get('ORES_DB_SHELL_USER', '')}",
        "-v", f"http_user={env.get('ORES_DB_HTTP_USER', '')}",
        "-v", f"compute_wrapper_user={env.get('ORES_DB_COMPUTE_WRAPPER_USER', '')}",
        "-v", f"test_ddl_user={env.get('ORES_TEST_DB_DDL_USER', '')}",
        "-v", f"test_dml_user={env.get('ORES_TEST_DB_USER', '')}",
    ] + _svc_psql_args(env, names, with_passwords=False) + [
        "-f", "setup_schema.sql",
    ]
    _psql(env, *schema_args, user=ddl_user, password=ddl_password,
          database=db_name, cwd=str(sql_dir))

    print("--- Phase 3: Populating database metadata ---")
    cmake = (project_root / "CMakeLists.txt").read_text()
    m = re.search(r"project\(OreStudio VERSION (\d+\.\d+\.\d+)", cmake)
    schema_version = m.group(1) if m else "0.0.0"
    build_env = env.get("ORES_BUILD_ENVIRONMENT", "local")

    def _git(*gargs, default=""):
        out = subprocess.run(["git", "-C", str(sql_dir)] + list(gargs),
                             capture_output=True, text=True)
        return out.stdout.strip() if out.returncode == 0 else default

    commit = _git("rev-parse", "--short", "HEAD", default="unknown")
    if _git("status", "--porcelain"):
        commit += "-dirty"
    git_date = _git("log", "-1", "--format=%ad",
                    "--date=format:%Y/%m/%d %H:%M:%S", "HEAD",
                    default="unknown")
    print(f"  Schema version:    {schema_version}")
    print(f"  Build environment: {build_env}")
    print(f"  Git commit:        {commit}")
    print(f"  Git date:          {git_date}")
    _psql(env, "--set", "ON_ERROR_STOP=on", "-c",
          f"INSERT INTO ores_database_info_tbl "
          f"(id, schema_version, build_environment, git_commit, git_date) "
          f"VALUES (gen_random_uuid(), '{schema_version}', '{build_env}', "
          f"'{commit}', '{git_date}');",
          password=pw, database=db_name)
    _VALID_ENV_TYPES = {"development", "staging", "production"}
    env_type = env.get("ORES_ENV_TYPE", "development")
    if env_type not in _VALID_ENV_TYPES:
        print(f"  Warning: unknown ORES_ENV_TYPE '{env_type}', defaulting to 'development'",
              file=sys.stderr)
        env_type = "development"
    _psql(env, "-c",
          f"ALTER DATABASE \"{db_name}\" SET ores.env_type = '{env_type}';",
          password=pw, database=db_name)
    print(f"  env_type: {env_type}")
    print(f"\nDatabase {db_name} is ready.")
    return 0


def cmd_recreate(project_root, env, args):
    """Port of recreate_database.sh."""
    sql_dir = project_root / "projects/ores.sql"
    db_name = args.database or env.get("ORES_DATABASE_NAME",
                                       "ores_frosty_leaf")
    names = service_names(project_root)

    required = ["ORES_DB_HOST", "PGPASSWORD", "ORES_DB_OWNER_ROLE",
                "ORES_DB_RW_ROLE", "ORES_DB_RO_ROLE", "ORES_DB_SERVICE_ROLE",
                "ORES_DB_DDL_USER", "ORES_DB_CLI_USER", "ORES_DB_WT_USER",
                "ORES_DB_SHELL_USER", "ORES_DB_HTTP_USER",
                "ORES_DB_READONLY_USER", "ORES_TEST_DB_DDL_USER",
                "ORES_TEST_DB_USER", "ORES_DB_DDL_PASSWORD",
                "ORES_DB_CLI_PASSWORD", "ORES_DB_WT_PASSWORD",
                "ORES_DB_SHELL_PASSWORD", "ORES_DB_HTTP_PASSWORD",
                "ORES_TEST_DB_DDL_PASSWORD", "ORES_TEST_DB_PASSWORD",
                "ORES_DB_READONLY_PASSWORD"]
    for svc in names:
        upper = svc.upper() + "_SERVICE"
        required += [f"ORES_{upper}_DB_USER", f"ORES_{upper}_DB_PASSWORD"]
    missing = [v for v in required if not env.get(v)]
    if missing:
        print("error: missing required environment variables:",
              file=sys.stderr)
        for v in missing:
            print(f"  - {v}", file=sys.stderr)
        print("\nRun ./projects/ores.compass/compass.sh env configure to "
              "generate a .env file.", file=sys.stderr)
        return 1

    start = time.time()
    print("=== ORE Studio Database Recreation ===")
    print(f"Database name: {db_name}")
    print(f"Start: {time.strftime('%Y-%m-%d %H:%M:%S')}\n")

    print("--- Checking for active connections ---")
    if not check_connections(env, db_name, kill=args.kill):
        return 1
    if not _confirm("WARNING: this will DROP all ORES databases and "
                    "recreate from scratch!", args.yes):
        print("Aborted.")
        return 1

    owner = env["ORES_DB_OWNER_ROLE"]
    env_label = re.sub(r"_owner$", "", re.sub(r"^ores_", "", owner))
    if not env_label:
        print(f"error: could not derive env_label from "
              f"ORES_DB_OWNER_ROLE: {owner}", file=sys.stderr)
        return 1
    pw = env["PGPASSWORD"]

    print(f"--- Dropping target database: {db_name} ---")
    _psql(env, "-c", f'DROP DATABASE IF EXISTS "{db_name}";', password=pw)

    _psql(env, "-v", f"env_label={env_label}",
          "-f", "drop_roles.sql", password=pw, cwd=str(sql_dir))

    print("--- Recreating roles and users ---")
    role_args = [
        "-f", "recreate_database.sql",
        "-v", f"owner_role={owner}",
        "-v", f"rw_role={env['ORES_DB_RW_ROLE']}",
        "-v", f"ro_role={env['ORES_DB_RO_ROLE']}",
        "-v", f"service_role={env['ORES_DB_SERVICE_ROLE']}",
        "-v", f"ddl_user={env['ORES_DB_DDL_USER']}",
        "-v", f"cli_user={env['ORES_DB_CLI_USER']}",
        "-v", f"wt_user={env['ORES_DB_WT_USER']}",
        "-v", f"shell_user={env['ORES_DB_SHELL_USER']}",
        "-v", f"http_user={env['ORES_DB_HTTP_USER']}",
        "-v", f"readonly_user={env['ORES_DB_READONLY_USER']}",
        "-v", f"test_ddl_user={env['ORES_TEST_DB_DDL_USER']}",
        "-v", f"test_dml_user={env['ORES_TEST_DB_USER']}",
        "-v", f"ddl_password={env['ORES_DB_DDL_PASSWORD']}",
        "-v", f"cli_password={env['ORES_DB_CLI_PASSWORD']}",
        "-v", f"wt_password={env['ORES_DB_WT_PASSWORD']}",
        "-v", f"shell_password={env['ORES_DB_SHELL_PASSWORD']}",
        "-v", f"http_password={env['ORES_DB_HTTP_PASSWORD']}",
        "-v", f"test_ddl_password={env['ORES_TEST_DB_DDL_PASSWORD']}",
        "-v", f"test_dml_password={env['ORES_TEST_DB_PASSWORD']}",
        "-v", f"ro_password={env['ORES_DB_READONLY_PASSWORD']}",
    ] + _svc_psql_args(env, names, with_passwords=True) + [
        "-v", f"db_name={db_name}",
    ]
    _psql(env, *role_args, password=pw, cwd=str(sql_dir))

    setup_ns = argparse.Namespace(database=db_name,
                                  skip_validation=args.no_sql_validation)
    rc = cmd_setup(project_root, env, setup_ns)
    if rc:
        return rc

    mins, secs = divmod(int(time.time() - start), 60)
    print("\n=== Database recreation complete ===")
    print(f"Duration: {mins}m {secs:02d}s")
    return 0


def _reset(project_root, env, sql_body, extra_args, label, warning,
           assume_yes):
    if not env.get("PGPASSWORD") or not env.get("ORES_TEST_DB_DATABASE"):
        print("error: PGPASSWORD and ORES_TEST_DB_DATABASE must be set "
              "(run compass env configure).", file=sys.stderr)
        return 1
    print(f"=== {label} ===")
    print(warning)
    if not _confirm("", assume_yes):
        print("Aborted.")
        return 1
    with tempfile.NamedTemporaryFile("w", suffix=".sql", delete=False) as f:
        f.write(sql_body)
        tmp = f.name
    try:
        ns = argparse.Namespace(user="postgres")
        rc = cmd_sql(project_root, env, ns, ["-f", tmp] + extra_args)
    finally:
        os.unlink(tmp)
    if rc == 0:
        print(f"\n=== {label} complete ===")
    return rc


def cmd_reset_system(project_root, env, args):
    """Port of reset_system.sh."""
    sql = ("\\set ON_ERROR_STOP on\n"
           "SELECT * FROM ores_iam_set_tenant_fn('system');\n"
           "SELECT ores_iam_reset_system_fn();\n")
    return _reset(project_root, env, sql, [],
                  "System bootstrap reset",
                  "This purges ALL non-system tenants, soft-deletes system "
                  "admin accounts,\nand re-enables bootstrap mode. The "
                  "SystemProvisionerWizard fires on next start.",
                  args.yes)


def cmd_reset_tenant(project_root, env, args):
    """Port of reset_tenant.sh."""
    sql = ("\\set ON_ERROR_STOP on\n"
           "SELECT * FROM ores_iam_set_tenant_fn('system');\n"
           "SELECT ores_iam_reset_tenant_bootstrap_fn(:'tenant_code');\n")
    return _reset(project_root, env, sql,
                  ["-v", f"tenant_code={args.tenant}"],
                  f"Tenant bootstrap reset: {args.tenant}",
                  "This resets the tenant's provisioned data and re-enables "
                  "its bootstrap mode;\nthe tenant record, admin account and "
                  "base reference data are preserved.",
                  args.yes)


# --- entry point ------------------------------------------------------------

def cmd_db_status(project_root, env):
    """Print a full database health overview."""
    import datetime

    db_name = env.get("ORES_TEST_DB_DATABASE", "")
    pw = env.get("PGPASSWORD", "")

    try:
        import ui
    except ImportError:
        ui = None

    def _header(text):
        if ui:
            return ui.header(text)
        return f"\033[1;36m{text}\033[0m"

    def _ycmd(text):
        if ui:
            return ui.ycmd(text)
        return f"\033[33m{text}\033[0m"

    def _cyan(text):
        if ui:
            return f"{ui.CYAN}{text}{ui.RESET}"
        return f"\033[36m{text}\033[0m"

    def _green(text):
        return f"\033[32m{text}\033[0m"

    def _yellow(text):
        return f"\033[33m{text}\033[0m"

    def _red(text):
        return f"\033[31m{text}\033[0m"

    print(_header("🗄️  compass db status"))
    print()

    if not db_name or not pw:
        print(_red("❌  DB credentials not configured (ORES_TEST_DB_DATABASE / PGPASSWORD missing)"))
        print(f"    {_ycmd('compass db recreate -y -k')}")
        return 1

    # ── Schema / restore info ─────────────────────────────────────────────────

    info = database_info(env)
    if not info:
        print(_red(f"❌  Database unreachable: {db_name}"))
        print(f"    {_ycmd('compass services stop && compass db recreate -y -k')}")
        return 1

    delta = None
    try:
        import subprocess as _sp
        head_ct = int(_sp.run(
            ["git", "log", "-1", "--format=%ct", "HEAD"],
            capture_output=True, text=True, cwd=str(project_root)
        ).stdout.strip() or "0")
        db_dt = datetime.datetime.strptime(info["git_date"], "%Y/%m/%d %H:%M:%S")
        delta = max(0, head_ct - int(db_dt.timestamp()))
    except (OSError, ValueError):
        pass

    if delta is None:
        drift_col, drift_label, drift_warn = "", "unknown", None
    elif delta >= 3 * 86400:
        days = delta // 86400
        drift_col, drift_label = _red, f"{days}d behind HEAD — stale"
        drift_warn = _red("⚠  Schema is stale") + f"  {_ycmd('compass services stop && compass db recreate -y -k')}"
    elif delta >= 86400:
        days = delta // 86400
        drift_col, drift_label = _yellow, f"{days}d behind HEAD — drifting"
        drift_warn = _yellow("⚠  Schema is drifting") + f"  {_ycmd('compass db recreate -y -k')}"
    else:
        drift_col, drift_label = _green, "current"
        drift_warn = None

    if callable(drift_col):
        drift_str = drift_col(drift_label)
    else:
        drift_str = drift_label

    print(f"📦  Schema")
    print()
    print(f"    version   : {_cyan(info['schema_version'])}")
    print(f"    restored  : {_cyan(info['restored_at'])}")
    print(f"    built from: {_cyan(info['git_commit'][:12] if info['git_commit'] else '?')}  ({info['git_date']})")
    print(f"    drift     : {drift_str}")
    if drift_warn:
        print(f"    {drift_warn}")
    print()

    def _query(sql):
        try:
            out = _psql(env, "-At", "-c", sql,
                        password=pw, database=db_name, check=False, capture=True)
            if out.returncode:
                return None
            return (out.stdout or "").strip()
        except OSError:
            return None

    # ── Bootstrap mode ────────────────────────────────────────────────────────

    boot_val = _query(
        "SELECT value FROM ores_variability_system_settings_tbl "
        "WHERE name = 'system.bootstrap_mode' "
        "AND valid_to = ores_utility_infinity_timestamp_fn() LIMIT 1;"
    )
    print(f"🔧  Bootstrap mode")
    print()
    if boot_val is None:
        print(f"    {_yellow('? (could not read)')}")
    elif boot_val.lower() == "true":
        print(f"    {_yellow('true')}  — system provisioning wizard not yet run")
        print(f"    {_ycmd('compass db setup')} or run the provisioning wizard")
    else:
        print(f"    {_green('false')}  — system provisioned")
    print()

    # ── Tenants ───────────────────────────────────────────────────────────────

    tenant_rows = _query(
        "SELECT code, status, to_char(valid_from,'YYYY-MM-DD') "
        "FROM ores_iam_tenants_tbl "
        "WHERE valid_to = ores_utility_infinity_timestamp_fn() "
        "AND id <> ores_utility_system_tenant_id_fn() "
        "ORDER BY valid_from;"
    )
    print(f"🏢  Tenants (non-system)")
    print()
    if not tenant_rows:
        print(f"    {_yellow('(none)')}")
    else:
        for row in tenant_rows.splitlines():
            parts = (row.split("|") + ["", ""])[:3]
            code, status, created = parts[0], parts[1], parts[2]
            status_str = _green(status) if status == "active" else _yellow(status)
            print(f"    {_cyan(code)}  [{status_str}]  created {created}")
    print()

    # ── Counts ────────────────────────────────────────────────────────────────

    print(f"📊  Data counts")
    print()

    def _count(sql, label, warn_zero=False):
        val = _query(sql)
        if val is None:
            print(f"    {label:<20} {_yellow('?')}")
            return
        n = int(val) if val.isdigit() else 0
        col = _yellow(val) if (warn_zero and n == 0) else _cyan(val)
        print(f"    {label:<20} {col}")

    _count(
        "SELECT count(*) FROM ores_refdata_parties_tbl "
        "WHERE valid_to = ores_utility_infinity_timestamp_fn();",
        "parties"
    )
    _count(
        "SELECT count(*) FROM ores_iam_accounts_tbl "
        "WHERE valid_to = ores_utility_infinity_timestamp_fn() "
        "AND account_type NOT IN ('service','algorithm','llm');",
        "user accounts"
    )
    _count(
        "SELECT count(*) FROM ores_refdata_currencies_tbl "
        "WHERE valid_to = ores_utility_infinity_timestamp_fn();",
        "currencies"
    )
    print()

    return 0


def run(argv, project_root: Path) -> int:
    ap = argparse.ArgumentParser(
        prog="compass db",
        description="Provision pillar: database lifecycle "
                    "(.sql sources stay in projects/ores.sql/).")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    sub.add_parser("status", help="Show database health overview: "
                              "schema version, drift, bootstrap state, tenants, counts")

    rc = sub.add_parser("recreate",
                        help="Drop and recreate the database, roles and "
                             "schema from scratch")
    rc.add_argument("-D", "--database", default=None,
                    help="Database name (default: ORES_DATABASE_NAME)")
    rc.add_argument("-y", "--yes", action="store_true",
                    help="Skip confirmation prompt")
    rc.add_argument("-k", "--kill", action="store_true",
                    help="Kill active connections first")
    rc.add_argument("--no-sql-validation", action="store_true",
                    help="Skip input validation in seed functions (faster)")

    st = sub.add_parser("setup", help="Create database, schema and metadata "
                                      "(roles must already exist)")
    st.add_argument("database", help="Database name")
    st.add_argument("--skip-validation", action="store_true")

    dr = sub.add_parser("drop", help="Drop a database safely")
    dr.add_argument("database", help="Database name")
    dr.add_argument("-y", "--yes", action="store_true")
    dr.add_argument("-k", "--kill", action="store_true")

    sq = sub.add_parser("sql", help="Run psql against the database "
                                    "(remaining args pass through)")
    sq.add_argument("-u", "--user", choices=["postgres", "ddl"],
                    default="postgres")

    rs = sub.add_parser("reset-system",
                        help="Return the system to pre-bootstrap state")
    rs.add_argument("-y", "--yes", action="store_true")

    rt = sub.add_parser("reset-tenant",
                        help="Reset a tenant's provisioned data and "
                             "re-enable its bootstrap")
    rt.add_argument("-t", "--tenant", required=True,
                    help="Tenant code (e.g. ores.dev.local3)")
    rt.add_argument("-y", "--yes", action="store_true")

    args, passthrough = ap.parse_known_args(argv)
    if passthrough and passthrough[0] == "--":
        passthrough = passthrough[1:]

    env = load_env(project_root)
    validate_env_version(project_root, env)

    if args.subcmd == "status":
        return cmd_db_status(project_root, env)
    if args.subcmd == "recreate":
        return cmd_recreate(project_root, env, args)
    if args.subcmd == "setup":
        return cmd_setup(project_root, env, args)
    if args.subcmd == "drop":
        return cmd_drop(project_root, env, args)
    if args.subcmd == "sql":
        return cmd_sql(project_root, env, args, passthrough)
    if args.subcmd == "reset-system":
        return cmd_reset_system(project_root, env, args)
    if args.subcmd == "reset-tenant":
        return cmd_reset_tenant(project_root, env, args)
    return 1
