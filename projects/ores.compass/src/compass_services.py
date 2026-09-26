# -*- coding: utf-8 -*-
"""compass services — Operate pillar.

Native port of the build/scripts service-lifecycle scripts:

    start-services.sh   ->  compass services start
    stop-services.sh    ->  compass services stop
    status-services.sh  ->  compass services status
    clear-logs.sh       ->  compass services clear-logs

`compass services start/stop/status` are systemd-backed: they
generate+deploy the concrete per-environment units (same code path as
`compass systemd generate`/`deploy`) and drive them via `systemctl
--user`, instead of spawning ores.controller.service to exec/cascade
its own children directly. ores.controller.service/process_supervisor
were decommissioned once systemd took over dependency-ordered startup
and readiness gating (see the split-services-into-per-service-
containers story). Each unit still execs the same binaries with the
same --log-directory args as before, so log-based readiness detection
(gather_counts/cmd_status) is unchanged; only process ownership
(PID-file bookkeeping -> systemd) and cascade-stop (controller ->
PartOf=<target>) moved.
"""

import argparse
import contextlib
import os
import subprocess
import sys
import time
from pathlib import Path

import systemctl_bus
import systemd_generate
from compass_db import load_env, validate_env_version


class _Tee:
    """Minimal stdout/stderr-like object writing to several streams at once."""

    def __init__(self, *streams):
        self.streams = streams

    def write(self, data):
        for s in self.streams:
            s.write(data)
            s.flush()
        return len(data)

    def flush(self):
        for s in self.streams:
            s.flush()


@contextlib.contextmanager
def _tee_to_file(log_path: Path):
    """Mirror everything printed inside the block to log_path as well as the
    console, so `tail -f log_path` from any shell shows exactly what a
    long-running compass command (services start, shell -f) is doing right
    now -- the same well-known-log-file pattern `compass build` uses.
    """
    log_path.parent.mkdir(parents=True, exist_ok=True)
    with open(log_path, "w") as f:
        old_out, old_err = sys.stdout, sys.stderr
        sys.stdout = _Tee(old_out, f)
        sys.stderr = _Tee(old_err, f)
        try:
            yield
        finally:
            sys.stdout, sys.stderr = old_out, old_err

# Port bases match ores-prodigy.el ores/port-bases.
PORT_BASES = {"remote": 50000, "local1": 51000, "local2": 52000,
              "local3": 53000, "local4": 54000, "local5": 55000}


# --- shared context ---------------------------------------------------------

class Ctx:
    def __init__(self, project_root: Path, env: dict, preset_arg):
        self.root = project_root
        self.env = env
        preset = preset_arg or env.get("ORES_PRESET", "")
        if not preset:
            print("error: no preset — pass --preset <preset> or set "
                  "ORES_PRESET via compass env configure", file=sys.stderr)
            sys.exit(1)
        if (preset_arg and env.get("ORES_PRESET")
                and env["ORES_PRESET"] != preset_arg):
            print(f"error: --preset '{preset_arg}' does not match "
                  f"ORES_PRESET='{env['ORES_PRESET']}' in .env",
                  file=sys.stderr)
            print(f"       run: ./projects/ores.compass/compass.sh env configure "
                  f"--preset {preset_arg}", file=sys.stderr)
            sys.exit(1)
        self.preset = preset
        self.build_dir = project_root / "build/output" / preset
        self.bin_dir = self.build_dir / "publish/bin"
        self.log_dir = self.build_dir / "publish/log"
        self.run_dir = self.build_dir / "publish/run"
        self.label = env.get("ORES_CHECKOUT_LABEL", "local1")
        self.env_name = env.get("ORES_ENV_NAME", "")
        self.target_name = (systemd_generate._unit_basename("ores", self.env_name)
                            + ".target" if self.env_name else "")
        self.nats_port = int(env.get("ORES_NATS_PORT", "4222"))
        self.nats_url = env.get("ORES_NATS_URL",
                                f"nats://localhost:{self.nats_port}")
        self.nats_prefix = env.get("ORES_NATS_SUBJECT_PREFIX",
                                   f"ores.dev.{self.label}")

def _wait_for_listen(port, timeout=60) -> bool:
    """ss-based LISTEN probe (TCP connect fails under mTLS)."""
    print(f"  wait    nats-server (port {port})", end="", flush=True)
    for i in range(timeout * 2):
        out = subprocess.run(["ss", "-tlnH", f"sport = :{port}"],
                             capture_output=True, text=True)
        if out.stdout.strip():
            print(" ... ready")
            return True
        time.sleep(0.5)
        if i % 4 == 3:
            print(".", end="", flush=True)
    print(" ... timeout (check nats-server logs)")
    return False


def _wait_for_log(ctx, name, pattern, timeout=120, log_basename=None) -> bool:
    log_file = ctx.log_dir / (log_basename or f"{name}.log")
    start_pos = log_file.stat().st_size if log_file.exists() else 0
    print(f"  wait    {name} ({pattern})", end="", flush=True)
    for i in range(timeout * 2):
        cur = log_file.stat().st_size if log_file.exists() else 0
        if cur < start_pos:  # log truncated by service startup
            start_pos = 0
        if log_file.exists():
            with open(log_file, "rb") as f:
                f.seek(start_pos)
                if pattern.encode() in f.read():
                    print(" ... done")
                    return True
        time.sleep(0.5)
        if i % 4 == 3:
            print(".", end="", flush=True)
    print(f" ... timeout (check {log_file})")
    return False


def _wait_for_logs(ctx, units, pattern, timeout=180, start_pos=None) -> dict:
    """Poll every (unit, log_basename) in `units` concurrently -- one
    shared timeout, not N sequential per-unit timeouts -- since systemd
    already started every unit's dependency-ordered chain in parallel
    when the target was started; a real deploy with 20+ services all
    connecting to the DB at once can legitimately take longer than any
    single unit's own budget to settle, so checking them one at a time
    with individual timeouts (as an earlier version of this function
    did) reported false timeouts for units that were still simply
    queued behind slower siblings, not actually stuck. Returns
    {unit: bool} once every unit is ready or the shared timeout expires.

    `start_pos`, if given, is a {unit: byte-offset} map captured by the
    caller *before* issuing `systemctl start` -- capturing it fresh in
    here (log_file.stat().st_size at call time) is too late: a fast
    unit can start, connect, and log "Service ready." before this
    function is even entered (e.g. while the caller is still waiting
    on something else, like the NATS port), so a size captured now
    would already be past that line and this function would wait
    forever for a *second* occurrence that never comes."""
    remaining = {unit: ctx.log_dir / log for unit, log in units}
    if start_pos is None:
        start_pos = {unit: (f.stat().st_size if f.exists() else 0)
                     for unit, f in remaining.items()}
    ready = {}
    print(f"  wait    {len(remaining)} unit(s) ({pattern})", end="", flush=True)
    for i in range(timeout * 2):
        for unit in list(remaining):
            log_file = remaining[unit]
            cur = log_file.stat().st_size if log_file.exists() else 0
            if cur < start_pos[unit]:  # log truncated by service restart
                start_pos[unit] = 0
            if not log_file.exists():
                continue
            with open(log_file, "rb") as f:
                f.seek(start_pos[unit])
                if pattern.encode() in f.read():
                    ready[unit] = True
                    del remaining[unit]
        if not remaining:
            print(" ... done")
            return ready
        time.sleep(0.5)
        if i % 4 == 3:
            print(".", end="", flush=True)
    print(f" ... timeout ({len(remaining)}/{len(ready) + len(remaining)} "
          f"still not ready: {', '.join(sorted(remaining))})")
    ready.update({unit: False for unit in remaining})
    return ready


def _log_contains(log_file: Path, pattern: str) -> bool:
    """Stream-search a log for PATTERN without loading it into memory."""
    if not log_file.exists():
        return False
    needle = pattern.encode()
    keep = len(needle) - 1
    tail = b""
    with open(log_file, "rb") as f:
        while True:
            chunk = f.read(1 << 20)
            if not chunk:
                return False
            if needle in tail + chunk:
                return True
            tail = chunk[-keep:] if keep else b""


def _log_last_line(log_file: Path) -> str:
    """Last line of a log by reading only its final chunk."""
    if not log_file.exists():
        return ""
    with open(log_file, "rb") as f:
        f.seek(0, 2)
        size = f.tell()
        f.seek(max(0, size - 1024))
        lines = f.read().splitlines()
    return lines[-1].decode("utf-8", errors="ignore") if lines else ""


def _service_units(ctx, only=None):
    """(unit, log_basename) for every concrete systemd unit this
    environment's target aggregates -- NOT including nats-server, which
    has no readiness log file under systemd (see _nats_unit) and is
    tracked separately. `unit` is the systemd unit basename (carries the
    -<env> suffix systemd_generate.py uses to keep this checkout's units
    distinct from a sibling checkout's on the same user session);
    `log_basename` is what the *binary itself* actually names its log
    file as, which is NOT unit-name-based -- it's always
    "<service_name>.<replica_index>.log" (replica_index 0 even for
    singletons, per the shared args_template), unaffected by env name
    since each checkout's build/output/<preset>/publish/log/ is already
    its own directory."""
    units = []
    services = systemd_generate.load_service_registry(ctx.root)
    for d in systemd_generate.fetch_service_definitions(services):
        if not d["enabled"]:
            continue
        if only is not None and d["service_name"] != only:
            continue
        base = systemd_generate._unit_basename(d["service_name"], ctx.env_name)
        if d["desired_replicas"] > 1:
            units += [(f"{base}-{r}", f"{d['service_name']}.{r}.log")
                      for r in range(1, d["desired_replicas"] + 1)]
        else:
            units.append((base, f"{d['service_name']}.0.log"))
    return units


def _registry_names(ctx):
    services = systemd_generate.load_service_registry(ctx.root)
    return [svc["name"] for svc in services]


def _resolve_service(ctx, selector):
    """Map what the user typed onto a registry service name.

    Accepts the registry name (ores.web.service), a component short name
    (web, ores.web, http.server), or a unit name already carrying this
    environment's suffix (ores.web.service-eager_maxwell)."""
    names = _registry_names(ctx)
    suffix = f"-{ctx.env_name}"
    stem = selector[: -len(suffix)] if selector.endswith(suffix) else selector
    if stem.endswith(".service"):
        stem = stem[: -len(".service")]
    for candidate in (stem, f"ores.{stem}"):
        for name in names:
            if name == candidate or name == f"{candidate}.service":
                return name
    raise KeyError(selector)


def _snapshot_logs(ctx, unit_pairs):
    """Log sizes before a start, so a readiness line already on disk is not
    mistaken for one this start produced."""
    return {unit: ((ctx.log_dir / log).stat().st_size
                   if (ctx.log_dir / log).exists() else 0)
            for unit, log in unit_pairs}


def _service_ready(ctx, unit_pairs, start_pos, timeout=300):
    ready = _wait_for_logs(ctx, unit_pairs, "Service ready", timeout=timeout,
                           start_pos=start_pos)
    # Requires= means a unit whose FIRST start attempt fails (e.g. it
    # briefly races a dependency) permanently fails that unit's start
    # job -- systemd does NOT re-trigger it once the dependency's own
    # Restart=always later succeeds. One reset-failed+start retry pass
    # for anything still not ready and in a failed/inactive systemd
    # state covers that race without masking a genuinely broken service
    # (which will just fail the retry too).
    broken = [unit for unit, ok in ready.items()
              if not ok and _unit_active_state(unit) in ("failed", "inactive")]
    if broken:
        print(f"[retry: {len(broken)} unit(s) failed their first start "
              f"attempt -- likely raced a dependency; retrying once]")
        for unit in broken:
            _systemctl(["reset-failed", f"{unit}.service"], check=False)
            _systemctl(["start", f"{unit}.service"], check=False)
        ready.update(_wait_for_logs(
            ctx, [(u, log) for u, log in unit_pairs if u in broken],
            "Service ready"))
    return all(ready.values())


def _nats_unit(ctx):
    return systemd_generate._unit_basename("nats-server", ctx.env_name)


def _systemctl(args, **kwargs):
    kwargs.setdefault("capture_output", True)
    kwargs.setdefault("text", True)
    return systemctl_bus.run(["--user"] + args, **kwargs)


def _unit_active_state(unit) -> str:
    """"active"/"inactive"/"failed"/"activating"/... , or "missing" if the
    unit isn't loaded at all (e.g. never deployed)."""
    out = _systemctl(["is-active", f"{unit}.service"], check=False)
    state = out.stdout.strip()
    return state if state else "missing"


def gather_counts(ctx):
    """Service state counts for status displays: dict of state -> count,
    plus nats state. Mirrors cmd_status's classification, driven by
    `systemctl --user is-active` instead of PID files."""
    counts = {"running": 0, "starting": 0, "stopped": 0, "missing": 0}

    def _classify(unit, log_basename):
        state = _unit_active_state(unit)
        if state == "missing":
            return "missing"
        if state != "active":
            return "stopped"
        return ("running" if _log_contains(ctx.log_dir / log_basename, "Service ready")
                else "starting")

    if not ctx.env_name:
        return {"nats": "missing", "counts": counts, "service_total": 0}

    # nats-server's systemd unit has no -l logfile flag (unlike the old
    # native-process launch) -- it logs to journald only -- so readiness
    # here is ActiveState alone, no log-content check available.
    nats_state = _unit_active_state(_nats_unit(ctx))
    nats = "missing" if nats_state == "missing" else (
        "running" if nats_state == "active" else "stopped")

    units = _service_units(ctx)
    for unit, log_basename in units:
        counts[_classify(unit, log_basename)] += 1
    return {"nats": nats, "counts": counts, "service_total": len(units)}


# --- subcommands ------------------------------------------------------------

def cmd_start(ctx, args):
    log_path = Path(f"/tmp/ores_{ctx.label}_services_start.log")
    print(f"📝 Progress log: {log_path} (tail -f to follow)")
    with _tee_to_file(log_path):
        rc = _cmd_start(ctx, args)
        if rc != 0 and not systemctl_bus.use_busctl():
            print(systemctl_bus.sandbox_hint(), file=sys.stderr)
        return rc


def _cmd_start(ctx, args):
    if not ctx.bin_dir.is_dir():
        print(f"error: binary directory not found: {ctx.bin_dir}",
              file=sys.stderr)
        print(f"       cmake --build --preset {ctx.preset}", file=sys.stderr)
        return 1
    if not ctx.env_name:
        print("error: ORES_ENV_NAME not set in .env", file=sys.stderr)
        return 1
    ctx.log_dir.mkdir(parents=True, exist_ok=True)
    ctx.run_dir.mkdir(parents=True, exist_ok=True)

    start_ts = time.time()
    if args.service:
        return _start_one(ctx, args, start_ts)

    print("Starting ORE Studio services")
    print(f"  Preset : {ctx.preset}")
    print(f"  NATS   : {ctx.nats_url} (prefix: {ctx.nats_prefix})")
    print()

    print("[Generate + deploy systemd units]")
    if systemd_generate.cmd_generate(ctx.root, ctx.env, None) != 0:
        return 1
    if systemd_generate.cmd_deploy(ctx.root, ctx.env, None) != 0:
        return 1
    print()

    units = _service_units(ctx)
    start_pos = _snapshot_logs(ctx, units)

    print(f"[systemctl --user start {ctx.target_name}]")
    result = _systemctl(["start", ctx.target_name], check=False)
    if result.returncode != 0:
        print(result.stderr, file=sys.stderr)
        return 1
    print()

    if not _wait_for_listen(ctx.nats_port):
        return 1

    # 300s: 20+ services all connecting to the DB at once (migrations,
    # schema checks) can genuinely take several minutes to all settle.
    ok = _service_ready(ctx, units, start_pos)

    print()
    print(f"Logs     : {ctx.log_dir}")
    print(f"Stop     : compass services stop")
    print(f"Time     : {int(time.time() - start_ts)}s")
    return 0 if ok else 1


def _resolve_service_or_report(ctx, selector):
    """(name, unit pairs) for a selector, or (None, None) after printing a
    readable error naming what the registry does hold."""
    try:
        name = _resolve_service(ctx, selector)
    except KeyError:
        print(f"error: no service '{selector}' in the registry.",
              file=sys.stderr)
        print("       Known services: " + ", ".join(_registry_names(ctx)),
              file=sys.stderr)
        return None, None
    units = _service_units(ctx, only=name)
    if not units:
        print(f"error: '{name}' is disabled in the registry.",
              file=sys.stderr)
        return None, None
    return name, units


def _start_one(ctx, args, start_ts):
    """Start one registry service, leaving the rest of the fleet alone. Its
    Requires= dependencies come up with it if they were down."""
    name, units = _resolve_service_or_report(ctx, args.service)
    if name is None:
        return 1

    print(f"Starting {name}")
    print(f"  Preset : {ctx.preset}")
    print()

    print("[Generate + deploy systemd units]")
    if systemd_generate.cmd_generate(ctx.root, ctx.env, None) != 0:
        return 1
    if systemd_generate.cmd_deploy(ctx.root, ctx.env, None) != 0:
        return 1
    print()

    start_pos = _snapshot_logs(ctx, units)
    print(f"[systemctl --user start {name}]")
    for unit, _log in units:
        result = _systemctl(["start", f"{unit}.service"], check=False)
        if result.returncode != 0:
            print(result.stderr, file=sys.stderr)
            return 1
    print()

    if not _wait_for_listen(ctx.nats_port):
        return 1

    ok = _service_ready(ctx, units, start_pos)
    print()
    print(f"Logs     : {ctx.log_dir}")
    print(f"Time     : {int(time.time() - start_ts)}s")
    return 0 if ok else 1


def cmd_stop(ctx, args):
    print(f"Stopping ORE Studio services ({ctx.preset})\n")
    if not ctx.env_name:
        print("error: ORES_ENV_NAME not set in .env", file=sys.stderr)
        return 1

    if args.service:
        return _stop_one(ctx, args)

    print(f"[systemctl --user stop {ctx.target_name}]")
    # PartOf=<target> on every generated unit means stopping the target
    # cascades to nats-server plus every service, the same one-command
    # cascade ores.controller.service used to perform itself.
    result = _systemctl(["stop", ctx.target_name], check=False)
    if result.returncode != 0 and "not loaded" not in (result.stderr or ""):
        print(result.stderr, file=sys.stderr)
        return 1

    print(f"Stopped  : {ctx.target_name} (and every unit it aggregates).")
    return 0


def _stop_one(ctx, args):
    """Stop one registry service, leaving the rest of the fleet alone."""
    name, units = _resolve_service_or_report(ctx, args.service)
    if name is None:
        return 1

    print(f"[systemctl --user stop {name}]")
    for unit, _log in units:
        result = _systemctl(["stop", f"{unit}.service"], check=False)
        if result.returncode != 0 and "not loaded" not in (result.stderr or ""):
            print(result.stderr, file=sys.stderr)
            return 1

    print(f"Stopped  : {name}.")
    return 0


def cmd_restart(ctx, args):
    print(f"Restarting ORE Studio services ({ctx.preset})\n")
    if not ctx.env_name:
        print("error: ORES_ENV_NAME not set in .env", file=sys.stderr)
        return 1

    # Regenerate first: a unit's ExecStart is built from .env (the log level
    # above all), so restarting units this deployment has stopped generating
    # would silently run the old settings. Deploy syncs and reloads only when
    # something actually changed.
    print("[Generate + deploy systemd units]")
    if systemd_generate.cmd_generate(ctx.root, ctx.env, None) != 0:
        return 1
    if systemd_generate.cmd_deploy(ctx.root, ctx.env, None) != 0:
        return 1
    print()

    if args.service:
        name, units = _resolve_service_or_report(ctx, args.service)
        if name is None:
            return 1
        start_pos = _snapshot_logs(ctx, units)
        print(f"[systemctl --user restart {name}]")
        for unit, _log in units:
            result = _systemctl(["restart", f"{unit}.service"], check=False)
            if result.returncode != 0:
                print(result.stderr, file=sys.stderr)
                return 1
        return 0 if _service_ready(ctx, units, start_pos) else 1

    units = _service_units(ctx)
    start_pos = _snapshot_logs(ctx, units)
    print(f"[systemctl --user restart {ctx.target_name}]")
    result = _systemctl(["restart", ctx.target_name], check=False)
    if result.returncode != 0 and "not loaded" not in (result.stderr or ""):
        print(result.stderr, file=sys.stderr)
        return 1
    return 0 if _service_ready(ctx, units, start_pos) else 1


def cmd_status(ctx, args):
    print(f"ORE Studio service status ({ctx.preset})\n")
    print(f"  {'STATUS':<10} {'SERVICE':<40} DETAIL")
    print(f"  {'-' * 10} {'-' * 40} ------")
    running = starting = stopped = missing = 0

    if not ctx.env_name:
        print("error: ORES_ENV_NAME not set in .env", file=sys.stderr)
        return 1

    def _check(unit, log_file=None):
        nonlocal running, starting, stopped, missing
        state = _unit_active_state(unit)
        if state == "missing":
            print(f"  {'missing':<10} {unit:<40} (unit not loaded)")
            missing += 1
            return
        if state != "active":
            print(f"  {'stopped':<10} {unit:<40} ({state})")
            stopped += 1
            return
        # nats-server's systemd unit has no log file (journald only).
        if log_file is None:
            print(f"  {'running':<10} {unit:<40} (active)")
            running += 1
            return
        if _log_contains(log_file, "Service ready"):
            print(f"  {'running':<10} {unit:<40} (active)")
            running += 1
        else:
            last = _log_last_line(log_file).split('\"] ')[-1][:60]
            print(f"  {'starting':<10} {unit:<40}  "
                  f"{f'({last})' if last else ''}")
            starting += 1

    if args.service:
        name, units = _resolve_service_or_report(ctx, args.service)
        if name is None:
            return 1
        for unit, log_basename in units:
            _check(unit, ctx.log_dir / log_basename)
    else:
        _check(_nats_unit(ctx))
        for unit, log_basename in _service_units(ctx):
            _check(unit, ctx.log_dir / log_basename)

    print(f"\nservices: running={running}  starting={starting}  "
          f"stopped={stopped}  missing={missing}")
    print(f"\nLogs : {ctx.log_dir}")
    return 0


def _claude_slice_name(ctx) -> str:
    import compass_claude
    return compass_claude._slice_name(ctx.env_name)


def _claude_slice_path(ctx) -> str:
    """Absolute cgroupfs path (unified hierarchy) for this environment's
    Claude slice -- systemd-cgtop takes real paths, not unit names, unlike
    systemd-cgls's --user-unit. app-claude-<env>.slice nests INSIDE the
    static app-claude.slice parent (systemd's dash-hierarchy convention,
    see compass_claude.py's own docstring) -- both segments are required,
    not just the leaf."""
    uid = os.getuid()
    return (f"/user.slice/user-{uid}.slice/user@{uid}.service/app.slice/"
            f"app-claude.slice/{_claude_slice_name(ctx)}")


def _slice_is_loaded(slice_name) -> bool:
    """Whether a (possibly transient, e.g. app-claude-<env>.slice) slice
    unit is currently loaded. Unlike _unit_active_state, does NOT append
    .service -- slice names already carry .slice, and systemd garbage-
    collects a dash-truncated-drop-in-backed transient slice entirely
    once its last leaf scope exits, so an environment with no active
    Claude session genuinely has no such unit for cgls/cgtop to find."""
    out = _systemctl(["is-active", slice_name], check=False)
    return out.stdout.strip() == "active"


def cmd_tree(ctx, args):
    """WS-4: process tree for this environment -- the Claude session slice
    (per-environment since WS-1) plus every unit this environment's fleet
    aggregates. Fleet service units have no per-environment slice of their
    own yet (only Claude sessions and, once WS-7's remainder lands,
    builds do) -- each unit's own cgroup is shown individually instead."""
    if not ctx.env_name:
        print("error: ORES_ENV_NAME not set in .env", file=sys.stderr)
        return 1
    units = [_nats_unit(ctx)] + [u for u, _ in _service_units(ctx)]
    loaded = [u for u in units if _unit_active_state(u) != "missing"]
    user_units = []
    slice_name = _claude_slice_name(ctx)
    if _slice_is_loaded(slice_name):
        user_units.append(f"--user-unit={slice_name}")
    user_units += [f"--user-unit={u}.service" for u in loaded]
    if not user_units:
        print(f"Nothing loaded for environment '{ctx.env_name}' "
              f"(no active Claude session, no fleet units running).")
        return 0
    result = subprocess.run(["systemd-cgls"] + user_units + args.extra,
                            check=False)
    return result.returncode


def cmd_top(ctx, args):
    """WS-4: systemd-cgtop filtered to this environment's Claude slice --
    the only per-environment cgroup that exists today (see cmd_tree's
    docstring on fleet services not having one yet)."""
    if not ctx.env_name:
        print("error: ORES_ENV_NAME not set in .env", file=sys.stderr)
        return 1
    result = subprocess.run(["systemd-cgtop"] + args.extra + [_claude_slice_path(ctx)],
                            check=False)
    return result.returncode


def cmd_clear_logs(ctx, args):
    if not ctx.log_dir.is_dir():
        print(f"Nothing to clear: log directory does not exist "
              f"({ctx.log_dir}).")
        return 0
    files = list(ctx.log_dir.glob("*.log")) + list(ctx.log_dir.glob("*.err"))
    if not files:
        print(f"No log files under {ctx.log_dir}.")
        return 0
    for f in files:
        f.unlink(missing_ok=True)
    print(f"Cleared {len(files)} log file(s) from {ctx.log_dir} "
          f"({ctx.preset}).")
    return 0


# --- entry points -----------------------------------------------------------

def _common(parser):
    parser.add_argument("--preset", default=None,
                        help="CMake preset (default: ORES_PRESET from .env)")
    systemctl_bus.add_busctl_argument(parser)


def _service_argument(parser):
    parser.add_argument(
        "service", nargs="?", default=None,
        help="Act on one registry service instead of the whole fleet. "
             "Accepts the registry name (ores.web.service), a short name "
             "(web), or the full unit name (ores.web.service-<env>).")


def run(argv, project_root: Path, env_file: Path | None = None) -> int:
    ap = argparse.ArgumentParser(
        prog="compass services",
        description="Operate pillar: service lifecycle, generated+deployed "
                    "as systemd units and driven via `systemctl --user`.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    st = sub.add_parser("start", help="Generate+deploy systemd units, then "
                                      "systemctl --user start the fleet. The "
                                      "compiled services' log level comes from "
                                      "ORES_SERVICE_LOG_LEVEL in .env.")
    _common(st)
    _service_argument(st)

    sp = sub.add_parser("stop", help="systemctl --user stop the fleet "
                                     "(cascades via PartOf=)")
    _common(sp)
    _service_argument(sp)

    sr = sub.add_parser("restart", help="systemctl --user restart the fleet, "
                                        "or just one service. Regenerates the "
                                        "units first, so it also applies "
                                        "ORES_SERVICE_LOG_LEVEL from .env.")
    _common(sr)
    _service_argument(sr)

    su = sub.add_parser("status", help="Per-unit status from systemctl "
                                       "plus readiness log lines")
    _common(su)
    _service_argument(su)

    tr = sub.add_parser("tree", help="Process tree for this environment: "
                                     "Claude session slice + fleet units "
                                     "(systemd-cgls)")
    _common(tr)
    tr.add_argument("extra", nargs=argparse.REMAINDER,
                    help="Extra flags forwarded verbatim to systemd-cgls "
                         "(e.g. -a, -l)")

    tp = sub.add_parser("top", help="Live resource usage for this "
                                    "environment's Claude slice "
                                    "(systemd-cgtop)")
    _common(tp)
    tp.add_argument("extra", nargs=argparse.REMAINDER,
                    help="Extra flags forwarded verbatim to systemd-cgtop "
                         "(e.g. -1, -b, -m)")

    cl = sub.add_parser("clear-logs", help="Delete all *.log / *.err under "
                                           "the preset's log directory")
    _common(cl)

    args = ap.parse_args(argv)
    env = load_env(project_root, env_file)
    systemctl_bus.adopt_transport_setting(env, getattr(args, "use_busctl", False))
    validate_env_version(project_root, env)
    ctx = Ctx(project_root, env, args.preset)

    return {"start": cmd_start, "stop": cmd_stop, "status": cmd_status,
            "restart": cmd_restart, "tree": cmd_tree, "top": cmd_top,
            "clear-logs": cmd_clear_logs}[args.subcmd](ctx, args)
