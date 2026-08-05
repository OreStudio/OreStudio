# -*- coding: utf-8 -*-
"""compass services / compass client — Operate pillar.

Native port of the build/scripts service-lifecycle scripts:

    start-services.sh   ->  compass services start
    stop-services.sh    ->  compass services stop
    status-services.sh  ->  compass services status
    clear-logs.sh       ->  compass services clear-logs
    start-client.sh     ->  compass client start

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
PartOf=<target>) moved. The Qt client is unaffected -- it was never
part of the controller's cascade and keeps its own PID-file-based
launch/stop path.
"""

import argparse
import contextlib
import os
import signal
import subprocess
import sys
import time
from pathlib import Path

import systemd_generate
from compass_db import load_env, validate_env_version

CLIENT_COLOURS = {"red": "F44336", "green": "4CAF50", "blue": "2196F3"}


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

    def child_env(self):
        e = os.environ.copy()
        e.update(self.env)
        # Real newlines for the JWT key, not the two-character '\n'.
        key_file = self.root / "build/keys/iam-rsa-private.pem"
        if key_file.exists():
            e["ORES_IAM_SERVICE_JWT_PRIVATE_KEY"] = key_file.read_text()
        e["WT_RESOURCES_DIR"] = str(
            self.build_dir / "vcpkg_installed/x64-linux/share/Wt/resources")
        return e


def _pid_alive(pid) -> bool:
    try:
        os.kill(int(pid), 0)
        return True
    except (OSError, ValueError):
        return False


def _read_pid(pid_file: Path):
    try:
        return int(pid_file.read_text().strip())
    except (OSError, ValueError):
        return None


def _launch(ctx, name, binary, args, pid_name=None):
    """Start a detached process from bin_dir; record its PID file. Used only
    by the Qt client path -- service lifecycle is systemd-managed."""
    pid_name = pid_name or name
    pid_file = ctx.run_dir / f"{pid_name}.pid"
    pid = _read_pid(pid_file)
    if pid and _pid_alive(pid):
        print(f"  skip    {pid_name:<38} PID {pid}")
        return pid
    proc = subprocess.Popen([f"./{binary}"] + args, cwd=str(ctx.bin_dir),
                            env=ctx.child_env(),
                            stdout=subprocess.DEVNULL,
                            stderr=subprocess.DEVNULL,
                            start_new_session=True)
    pid_file.write_text(f"{proc.pid}\n")
    print(f"  start   {pid_name:<38} PID {proc.pid}")
    return proc.pid


def _terminate(name, pid_file, grace) -> str:
    """SIGTERM (then SIGKILL after `grace` seconds) the PID in `pid_file`.
    Used only by the Qt client path."""
    pid = _read_pid(pid_file)
    if pid is None:
        print(f"  skip    {name} (no PID file — already stopped)")
        return "gone"
    pid_file.unlink(missing_ok=True)
    if not _pid_alive(pid):
        print(f"  gone    {name:<38} PID {pid}")
        return "gone"
    os.kill(pid, signal.SIGTERM)
    print(f"  stop    {name:<38} PID {pid}")
    print(f"Waiting for {name} to exit...", end="", flush=True)
    for _ in range(grace * 2):
        if not _pid_alive(pid):
            break
        time.sleep(0.5)
        print(".", end="", flush=True)
    print(" done")
    if _pid_alive(pid):
        os.kill(pid, signal.SIGKILL)
        print(f"  killed  PID {pid} (did not exit within grace period)")
    print()
    return "stopped"


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


def _service_units(ctx):
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
    for d in systemd_generate.fetch_service_definitions(ctx.env):
        if not d["enabled"]:
            continue
        base = systemd_generate._unit_basename(d["service_name"], ctx.env_name)
        if d["desired_replicas"] > 1:
            units += [(f"{base}-{r}", f"{d['service_name']}.{r}.log")
                      for r in range(1, d["desired_replicas"] + 1)]
        else:
            units.append((base, f"{d['service_name']}.0.log"))
    return units


def _nats_unit(ctx):
    return systemd_generate._unit_basename("nats-server", ctx.env_name)


def _systemctl(args, **kwargs):
    kwargs.setdefault("capture_output", True)
    kwargs.setdefault("text", True)
    return subprocess.run(["systemctl", "--user"] + args, **kwargs)


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


def client_status(ctx):
    """List of (instance_name, pid) for running Qt clients."""
    clients = []
    if ctx.run_dir.is_dir():
        for pid_file in sorted(ctx.run_dir.glob("ores.qt*.pid")):
            pid = _read_pid(pid_file)
            if pid and _pid_alive(pid):
                clients.append((pid_file.stem, pid))
    return clients


# --- subcommands ------------------------------------------------------------

def cmd_start(ctx, args):
    log_path = Path(f"/tmp/ores_{ctx.label}_services_start.log")
    print(f"📝 Progress log: {log_path} (tail -f to follow)")
    with _tee_to_file(log_path):
        return _cmd_start(ctx, args)


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
    # Captured *before* `systemctl start` so a fast unit that logs
    # "Service ready." before we ever get around to calling
    # _wait_for_logs (e.g. while still waiting on the NATS port below)
    # doesn't have its readiness line skipped by a start_pos taken too
    # late -- see _wait_for_logs's own docstring.
    start_pos = {unit: ((ctx.log_dir / log).stat().st_size
                        if (ctx.log_dir / log).exists() else 0)
                for unit, log in units}

    print(f"[systemctl --user start {ctx.target_name}]")
    result = _systemctl(["start", ctx.target_name], check=False)
    if result.returncode != 0:
        print(result.stderr, file=sys.stderr)
        return 1
    print()

    if not _wait_for_listen(ctx.nats_port):
        return 1

    # 300s: 20+ services all connecting to the DB at once (migrations,
    # schema checks) can genuinely take several minutes to all settle,
    # observed empirically -- the old controller-based start gave a
    # single combined "All services started" wait of 120s for the same
    # fleet, but that was one signal from one process; here every unit
    # is checked independently and the slowest one sets the bar.
    ready = _wait_for_logs(ctx, units, "Service ready", timeout=300,
                           start_pos=start_pos)
    # Requires= means a unit whose FIRST start attempt fails (e.g. it
    # briefly races a dependency) permanently fails that unit's start
    # job -- systemd does NOT re-trigger it once the dependency's own
    # Restart=always later succeeds. One reset-failed+start retry pass
    # for anything still not ready and in a failed/inactive systemd
    # state covers that race without masking a genuinely broken service
    # (which will just fail the retry too).
    retry = [unit for unit, ok in ready.items()
             if not ok and _unit_active_state(unit) in ("failed", "inactive")]
    if retry:
        print(f"[retry: {len(retry)} unit(s) failed their first start "
              f"attempt -- likely raced a dependency; retrying once]")
        for unit in retry:
            _systemctl(["reset-failed", f"{unit}.service"], check=False)
            _systemctl(["start", f"{unit}.service"], check=False)
        retry_units = [(u, log) for u, log in units if u in retry]
        ready.update(_wait_for_logs(ctx, retry_units, "Service ready"))

    ok = all(ready.values())
    print()
    print(f"Logs     : {ctx.log_dir}")
    print(f"Stop     : compass services stop")
    print(f"Time     : {int(time.time() - start_ts)}s")
    return 0 if ok else 1


def cmd_stop(ctx, args):
    print(f"Stopping ORE Studio services ({ctx.preset})\n")
    if not ctx.env_name:
        print("error: ORES_ENV_NAME not set in .env", file=sys.stderr)
        return 1

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
    systemd-cgls's --user-unit."""
    uid = os.getuid()
    return (f"/user.slice/user-{uid}.slice/user@{uid}.service/app.slice/"
            f"{_claude_slice_name(ctx)}")


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
    user_units = [f"--user-unit={_claude_slice_name(ctx)}"]
    user_units += [f"--user-unit={u}.service" for u in loaded]
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


def _client_colour(colour_arg):
    """Resolve --colour to (hex, tag), or ("", "") if unset. Returns None on
    an invalid value (caller prints the error and exits)."""
    if not colour_arg:
        return "", ""
    c = colour_arg.lower()
    if c in CLIENT_COLOURS:
        return CLIENT_COLOURS[c], c
    if len(c) == 6 and all(ch in "0123456789abcdef" for ch in c):
        return c.upper(), c
    return None


def cmd_client_start(ctx, args):
    if not (ctx.bin_dir / "ores.qt").exists():
        print(f"error: ores.qt not found in {ctx.bin_dir}", file=sys.stderr)
        print(f"       cmake --build --preset {ctx.preset}", file=sys.stderr)
        return 1
    ctx.run_dir.mkdir(parents=True, exist_ok=True)

    resolved = _client_colour(args.colour)
    if resolved is None:
        print(f"error: unknown colour '{args.colour}' — use red, green, "
              f"blue, or a 6-digit hex value", file=sys.stderr)
        return 1
    colour_hex, colour_tag = resolved

    pid_name = f"ores.qt.{colour_tag}" if colour_tag else "ores.qt"
    log_file = f"{pid_name}.log"
    client_args = ["--log-enabled", "--log-level", args.log_level,
                   "--log-directory", "../log", "--log-filename", log_file]
    # The colour is only a window marker; the display name is always
    # ORES_CHECKOUT_LABEL — so the status bar shows which checkout (e.g.
    # local2) this client is bound to. Not overridable: a mismatched name
    # makes the client unidentifiable in the fleet/status view.
    if ctx.label:
        client_args += ["--instance-name", ctx.label]
    if colour_hex:
        client_args += ["--instance-color", colour_hex]
    if args.open_scenario:
        client_args += ["--open-scenario", args.open_scenario]
    if args.master_password:
        client_args += ["--master-password", args.master_password]

    _launch(ctx, pid_name, "ores.qt", client_args)
    print(f"\nLogs : {ctx.log_dir / log_file}")
    return 0


def cmd_client_stop(ctx, args):
    resolved = _client_colour(args.colour)
    if resolved is None:
        print(f"error: unknown colour '{args.colour}' — use red, green, "
              f"blue, or a 6-digit hex value", file=sys.stderr)
        return 1
    _, colour_tag = resolved
    pid_name = f"ores.qt.{colour_tag}" if colour_tag else "ores.qt"
    print(f"Stopping {pid_name} ({ctx.preset})\n")
    _terminate(pid_name, ctx.run_dir / f"{pid_name}.pid", grace=10)
    return 0


# --- entry points -----------------------------------------------------------

def _common(parser):
    parser.add_argument("--preset", default=None,
                        help="CMake preset (default: ORES_PRESET from .env)")


def run(argv, project_root: Path) -> int:
    ap = argparse.ArgumentParser(
        prog="compass services",
        description="Operate pillar: service lifecycle, generated+deployed "
                    "as systemd units and driven via `systemctl --user`.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    st = sub.add_parser("start", help="Generate+deploy systemd units, then "
                                      "systemctl --user start the fleet")
    _common(st)
    st.add_argument("--log-level", default="trace")

    sp = sub.add_parser("stop", help="systemctl --user stop the fleet "
                                     "(cascades via PartOf=)")
    _common(sp)

    su = sub.add_parser("status", help="Per-unit status from systemctl "
                                       "plus readiness log lines")
    _common(su)

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
    env = load_env(project_root)
    validate_env_version(project_root, env)
    ctx = Ctx(project_root, env, args.preset)

    return {"start": cmd_start, "stop": cmd_stop, "status": cmd_status,
            "tree": cmd_tree, "top": cmd_top,
            "clear-logs": cmd_clear_logs}[args.subcmd](ctx, args)


def run_client(argv, project_root: Path) -> int:
    ap = argparse.ArgumentParser(
        prog="compass client",
        description="Operate pillar: launch/stop the Qt client, detached.")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    st = sub.add_parser("start", help="Launch the Qt client detached")
    _common(st)
    st.add_argument("--colour", "--color", default=None,
                    help="red, green, blue, or 6-digit hex — instance accent")
    st.add_argument("--log-level", default="debug")
    st.add_argument("--open-scenario", default=None,
                    help="Path to a test_scenario .org doc to open in the "
                         "Scenario Runner on startup (System > Testing)")
    st.add_argument("--master-password", default=None,
                    help="Connections.db master password, passed through to "
                         "--master-password. Falls back to "
                         "ORES_CONNECTIONS_MASTER_PASSWORD in .env if not set.")

    sp = sub.add_parser("stop", help="Stop a running Qt client instance")
    _common(sp)
    sp.add_argument("--colour", "--color", default=None,
                    help="Must match the --colour the instance was started "
                         "with (default: the uncoloured instance)")

    args = ap.parse_args(argv)
    env = load_env(project_root)
    validate_env_version(project_root, env)
    ctx = Ctx(project_root, env, args.preset)
    return {"start": cmd_client_start, "stop": cmd_client_stop}[args.subcmd](ctx, args)
