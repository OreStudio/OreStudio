# -*- coding: utf-8 -*-
"""compass env status — this checkout's environment, as text or as JSON.

Three readers share one gather: the human verb, the JSON contract that the
DSH environment plugin reads, and the environment section of `compass
bearings`. They cannot disagree about an environment, because every rule
they depend on is called rather than copied:

    compass_db.database_info / schema_sync
        when the database was restored, and how its schema compares to HEAD
    compass_db.bootstrap_mode
        whether the provisioning wizard has run
    compass_db.level_of and its threshold constants
        a number turned into ok / warn / critical
    compass_services.gather_units
        every unit this environment runs, with its state and detail

Payload shape. The plugin's CONTRACT.md names these same fields.

    ok              bool
    generatedAt     ISO 8601, UTC
    env             name, label, preset, worktree, envVersion,
                    requiredEnvVersion, envStale, scope, slice,
                    activities[], vcpkgWarning
    database        reachable, reason, name, restoredAt, restoredAgeSeconds,
                    restoredAge, restoredLevel, schemaFingerprint,
                    expectedFingerprint, builtFrom, builtAt, driftLabel,
                    driftLevel,
                    bootstrapMode
    services        total, counts{running,starting,stopped,failed,missing},
                    nats{unit,label,state,detail}, units[], logDir
    health          level (ok|warn|critical), reasons[]
    remedies        startServices, stopServices, recreateDatabase,
                    configureEnv

The payload never carries an ANSI escape code, whatever the terminal is.
"""

import argparse
import datetime
import json
import re
import sys
from pathlib import Path

_ANSI = re.compile(r"\033\[[0-9;]*m")
_IS_TTY = sys.stdout.isatty()


def _plain(text):
    """The same string with terminal colour removed."""
    return _ANSI.sub("", str(text))


def _colour(code):
    return code if _IS_TTY else ""


def session_scope():
    """(scope, slice) for the calling process, or (None, None).

    Reads /proc/self/cgroup for the innermost scope unit and its parent
    slice. A session that runs under no named scope answers (None, None),
    which is a normal state rather than an error."""
    try:
        text = Path("/proc/self/cgroup").read_text(encoding="utf-8")
    except (OSError, UnicodeError):
        return None, None
    scope = None
    parent = None
    for part in reversed(text.strip().split("/")):
        if part.endswith(".scope") and scope is None:
            scope = part
        elif part.endswith(".slice") and parent is None:
            parent = part
        if scope is not None and parent is not None:
            break
    return scope, parent


def _age(seconds):
    """A compact age: 12m, 4h, 2d. Empty for an unknown age."""
    if seconds is None:
        return ""
    seconds = int(seconds)
    if seconds < 3600:
        return f"{seconds // 60}m"
    if seconds < 86400:
        return f"{seconds // 3600}h"
    return f"{seconds // 86400}d"


def _restored_age_seconds(restored_at):
    try:
        when = datetime.datetime.strptime(restored_at, "%Y-%m-%d %H:%M")
    except (ValueError, TypeError):
        return None
    return int((datetime.datetime.now() - when).total_seconds())


def gather(project_root, env, preset=None):
    """This checkout's environment as one dict. See the module docstring."""
    import compass as _c
    import compass_db as _cdb
    import compass_services as _csv
    import env_activity as _ea
    import env_init as _ei
    import systemctl_bus as _bus

    # Reads the user manager, so it adopts the checkout's transport itself.
    # compass's entry point already does this from .env, but a direct caller
    # would otherwise read every unit as `missing` rather than `stopped`.
    _bus.adopt_transport_setting(env)

    project_root = Path(project_root)
    preset = preset or env.get("ORES_PRESET", "")
    scope, slice_name = session_scope()

    # ── Identity ─────────────────────────────────────────────────────────────
    try:
        env_version = int(env.get("ORES_ENV_VERSION", "0") or "0")
    except ValueError:
        env_version = 0
    try:
        required = int(_ei.current_version(project_root))
    except Exception:
        required = None

    try:
        current = int(env.get("ORES_ENV_ACTIVITY", "0") or "0")
    except ValueError:
        current = 0
    activities = []
    try:
        for number, date, title, recipe_id in _ea.pending(project_root, current):
            activities.append({"number": number, "date": _plain(date),
                               "title": _plain(title),
                               "recipeId": _plain(recipe_id)})
    except Exception:
        pass

    vcpkg_warning = ""
    try:
        drift = _c.vcpkg_drift(project_root)
        if drift["error"] == "no-submodule":
            vcpkg_warning = ("vcpkg submodule not checked out — "
                             "git submodule update --init vcpkg")
        elif drift["error"] is None and drift["current"] != drift["expected"]:
            vcpkg_warning = (f"vcpkg is on {drift['current'][:9]}, main expects "
                             f"{drift['expected'][:9]} — git submodule update "
                             f"--init vcpkg")
    except Exception:
        pass

    identity = {
        "name": env.get("ORES_ENV_NAME", "") or project_root.name,
        "label": env.get("ORES_CHECKOUT_LABEL", ""),
        "preset": preset,
        "worktree": str(project_root),
        "envVersion": env_version,
        "requiredEnvVersion": required,
        "envStale": required is not None and env_version < required,
        "scope": scope or "",
        "slice": slice_name or "",
        "activities": activities,
        "vcpkgWarning": vcpkg_warning,
    }

    # ── Database ─────────────────────────────────────────────────────────────
    db_name = env.get("ORES_TEST_DB_DATABASE", "")
    info = _cdb.database_info(env)
    if not info:
        database = {"reachable": False, "name": db_name,
                    "reason": "credentials" if not (db_name and env.get("PGPASSWORD"))
                              else "unreachable"}
    else:
        expected, drift_label, drift_ansi, drift_warning = _cdb.schema_sync(
            project_root, info)
        age = _restored_age_seconds(info["restored_at"])
        database = {
            "reachable": True,
            "name": db_name,
            "restoredAt": info["restored_at"],
            "restoredAgeSeconds": age,
            "restoredAge": _age(age),
            "restoredLevel": _cdb.level_of(
                age / 3600 if age is not None else None,
                _cdb.RESTORED_WARN_HOURS, _cdb.RESTORED_CRITICAL_HOURS),
            "schemaFingerprint": info["schema_fingerprint"],
            "expectedFingerprint": expected,
            "builtFrom": info["git_commit"],
            "builtAt": info["git_date"],
            "driftLabel": _plain(drift_label),
            "driftLevel": {"\033[32m": "ok",
                           "\033[31m": "critical"}.get(drift_ansi, "unknown"),
            "warning": _plain(drift_warning) if drift_warning else "",
            "bootstrapMode": _cdb.bootstrap_mode(env),
        }

    # ── Services ─────────────────────────────────────────────────────────────
    try:
        ctx = _csv.Ctx(project_root, env, None)
        gathered = _csv.gather_units(ctx)
        services = {
            "total": gathered["service_total"],
            "counts": gathered["counts"],
            "nats": gathered["nats"],
            "units": gathered["units"],
            "logDir": str(ctx.log_dir),
        }
    except SystemExit:
        services = {"total": 0,
                    "counts": {"running": 0, "starting": 0, "stopped": 0,
                               "failed": 0, "missing": 0},
                    "nats": None, "units": [], "logDir": ""}

    # ── Health: the worst reason wins, and every reason is named ─────────────
    critical = []
    warnings = []
    if not database.get("reachable"):
        critical.append("the database does not answer")
    counts = services["counts"]
    if counts["failed"]:
        critical.append(f"{counts['failed']} service(s) failed")
    if counts["missing"]:
        critical.append(f"{counts['missing']} unit(s) are not deployed")
    if counts["stopped"]:
        warnings.append(f"{counts['stopped']} of {services['total']} services "
                        f"are stopped")
    if counts["starting"]:
        warnings.append(f"{counts['starting']} service(s) are starting")
    if database.get("reachable"):
        if database["driftLevel"] == "critical":
            critical.append(f"the schema is {database['driftLabel']}")
        if database["restoredLevel"] in ("warn", "critical"):
            warnings.append(f"the database was restored {database['restoredAge']} "
                            f"ago")
        if database["bootstrapMode"]:
            warnings.append("bootstrap mode is on: the provisioning wizard has "
                            "not run")
    if identity["envStale"]:
        warnings.append(f".env is stale (v{identity['envVersion']}, needs "
                        f"v{identity['requiredEnvVersion']})")
    if activities:
        plural = "y" if len(activities) == 1 else "ies"
        warnings.append(f"{len(activities)} environment activit{plural} "
                        f"outstanding")
    health = {"level": "critical" if critical else
                       "warn" if warnings else "ok",
              "reasons": critical + warnings}

    return {
        "ok": True,
        "generatedAt": datetime.datetime.now(datetime.timezone.utc)
            .strftime("%Y-%m-%dT%H:%M:%SZ"),
        "env": identity,
        "database": database,
        "services": services,
        "health": health,
        "remedies": {
            "startServices": "compass services start",
            "stopServices": "compass services stop",
            "recreateDatabase": "compass db recreate -y -k",
            "configureEnv": f"compass env configure --preset {preset} -y"
                            if preset else "compass env configure",
        },
    }


def render_lines(payload):
    """The environment block as terminal lines, the text bearings prints.

    Colour follows the terminal, and the JSON path never calls this."""
    import compass_db as _cdb

    green = _colour("\033[32m")
    yellow = _colour("\033[33m")
    red = _colour("\033[31m")
    reset = _colour("\033[0m")
    ycmd = lambda text: f"{yellow}{text}{reset}"  # noqa: E731

    identity = payload["env"]
    database = payload["database"]
    services = payload["services"]
    lines = []

    lines.append(f"  Preset   : {identity['preset']}  (label: "
                 f"{identity['label']}, env v{identity['envVersion']})")
    if identity["scope"]:
        slice_info = f"  (slice: {identity['slice']})" if identity["slice"] else ""
        lines.append(f"  Scope    : {identity['scope']}{slice_info}")
    else:
        lines.append("  Scope    : (no named systemd scope — running unscoped)")

    if identity["envStale"]:
        lines.append(f"  {yellow}⚠  .env is stale (v{identity['envVersion']}, "
                     f"need v{identity['requiredEnvVersion']}) — "
                     f"{ycmd('compass env configure --preset ' + identity['preset'] + ' -y')}"
                     f"{reset}")
    elif (identity["requiredEnvVersion"] is not None
          and identity["envVersion"] > identity["requiredEnvVersion"]):
        lines.append(f"  {yellow}⚠  .env version v{identity['envVersion']} is "
                     f"newer than required v{identity['requiredEnvVersion']} — "
                     f"proceeding.{reset}")
    if identity["activities"]:
        count = len(identity["activities"])
        plural = "y" if count == 1 else "ies"
        lines.append(f"  {yellow}⚠  {count} environment activit{plural} "
                     f"outstanding for this checkout:{reset}")
        for activity in identity["activities"]:
            lines.append(f"     {activity['number']}. {activity['title']}  —  "
                         f"compass show {activity['recipeId']}")
        lines.append(f"     Then: {ycmd('compass env activity ack ' + str(identity['activities'][-1]['number']))}")
    if identity["vcpkgWarning"]:
        lines.append(f"  {yellow}⚠  {identity['vcpkgWarning']}{reset}")

    if not database["reachable"]:
        lines.append(f"  Database : {red}unreachable{reset}  "
                     f"({ycmd('compass db recreate -y -k')})")
    else:
        chip = f" (schema {database['driftLabel']})"
        col = {"ok": green, "warn": yellow}.get(database["driftLevel"], red)
        lines.append(f"  Database : {col}restored {database['restoredAt']}"
                     f"{chip}{reset}")
        if database.get("warning"):
            warning_col = {"ok": green, "warn": yellow}.get(
                database["driftLevel"], red)
            lines.append(f"  {warning_col}{database['warning']}{reset}")

    counts = services["counts"]
    nats = services["nats"]["state"] if services["nats"] else "missing"
    all_up = counts["running"] and not counts["stopped"] and not counts["missing"]
    tone = green if all_up else yellow
    hint = ("" if counts["running"] or counts["starting"]
            else f"  ({ycmd('compass services start')})")
    lines.append(f"  Services : {tone}running={counts['running']} "
                 f"starting={counts['starting']} stopped={counts['stopped']} "
                 f"failed={counts['failed']} missing={counts['missing']}{reset}  "
                 f"(nats: {nats}){hint}")
    return lines


def run(argv, project_root):
    ap = argparse.ArgumentParser(
        prog="compass env status",
        description="This checkout's environment: identity, database, and "
                    "services. --json emits the contract the DSH environment "
                    "plugin reads.")
    ap.add_argument("--preset", help="CMake preset (default: ORES_PRESET)")
    ap.add_argument("--json", action="store_true",
                    help="Emit one JSON object instead of terminal text")
    args = ap.parse_args(argv)

    import compass_db
    env = compass_db.load_env(Path(project_root))
    payload = gather(project_root, env, args.preset)

    if args.json:
        print(json.dumps(payload, indent=2, sort_keys=False))
        return 0

    print("🌍  compass env status\n")
    for line in render_lines(payload):
        print(line)
    print(f"\n  Health   : {payload['health']['level']}")
    for reason in payload["health"]["reasons"]:
        print(f"     - {reason}")
    return 0
