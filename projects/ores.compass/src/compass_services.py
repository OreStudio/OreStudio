# -*- coding: utf-8 -*-
"""compass services / compass client — Operate pillar.

Native port of the build/scripts service-lifecycle scripts:

    start-services.sh   ->  compass services start
    stop-services.sh    ->  compass services stop
    status-services.sh  ->  compass services status
    clear-logs.sh       ->  compass services clear-logs
    start-client.sh     ->  compass client start

The controller spawns IAM and every domain service; this module only
manages nats-server, the controller, and the Qt client, plus PID-file
hygiene for anything else found under publish/run/.
"""

import argparse
import contextlib
import os
import shutil
import signal
import subprocess
import sys
import time
from pathlib import Path

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
        self.nats_port = int(env.get("ORES_NATS_PORT", "4222"))
        self.nats_url = env.get("ORES_NATS_URL",
                                f"nats://localhost:{self.nats_port}")
        self.nats_prefix = env.get("ORES_NATS_SUBJECT_PREFIX",
                                   f"ores.dev.{self.label}")
        self.nats_config = project_root / f"build/config/nats-{self.label}.conf"
        self.nats_pid_file = (project_root / "build/nats" / self.label
                              / "nats-server.pid")
        self.keys_dir = project_root / "build/keys/nats"
        self.tls = bool(env.get("ORES_NATS_TLS_CA", ""))

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
    """Start a detached process from bin_dir; record its PID file."""
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


def _tls_args(ctx, cert_name):
    if not ctx.tls:
        return []
    return ["--nats-tls-ca", str(ctx.keys_dir / "ca.crt"),
            "--nats-tls-cert", str(ctx.keys_dir / f"{cert_name}.crt"),
            "--nats-tls-key", str(ctx.keys_dir / f"{cert_name}.key")]


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


def _wait_for_log(ctx, name, pattern, timeout=120, suffix=".log") -> bool:
    log_file = ctx.log_dir / f"{name}{suffix}"
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


def gather_counts(ctx):
    """Service state counts for status displays: dict of state -> count,
    plus nats state. Mirrors cmd_status's classification."""
    counts = {"running": 0, "starting": 0, "stopped": 0, "missing": 0}
    _ready = _log_contains

    def _classify(svc):
        pid = _read_pid(ctx.run_dir / f"{svc}.pid")
        if pid is None:
            return "missing"
        if not _pid_alive(pid):
            return "stopped"
        log_file = ctx.log_dir / f"{svc}.log"
        if not log_file.exists() and (ctx.log_dir / f"{svc}.0.log").exists():
            log_file = ctx.log_dir / f"{svc}.0.log"
        return "running" if _ready(log_file, "Service ready") else "starting"

    nats_pid = _read_pid(ctx.nats_pid_file)
    if nats_pid is None:
        nats = "missing"
    elif not _pid_alive(nats_pid):
        nats = "stopped"
    elif _ready(ctx.log_dir / "nats-server.log", "Server is ready"):
        nats = "running"
    else:
        nats = "starting"

    seen = set()
    if ctx.run_dir.is_dir():
        for pid_file in sorted(ctx.run_dir.glob("*.pid")):
            svc = pid_file.stem
            if svc.startswith("ores.qt"):
                continue  # the client is reported separately
            seen.add(svc)
            counts[_classify(svc)] += 1
    return {"nats": nats, "counts": counts, "service_total": len(seen)}


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
    ctx.log_dir.mkdir(parents=True, exist_ok=True)
    ctx.run_dir.mkdir(parents=True, exist_ok=True)

    nats_bin = None
    for candidate in ([shutil.which("nats-server")] +
                      ["/usr/sbin/nats-server", "/sbin/nats-server",
                       "/usr/local/sbin/nats-server",
                       "/usr/local/bin/nats-server"]):
        if candidate and os.access(candidate, os.X_OK):
            nats_bin = candidate
            break
    if not nats_bin:
        print("error: nats-server not found (tried PATH, /usr/sbin, /sbin, "
              "/usr/local/sbin)", file=sys.stderr)
        return 1

    start_ts = time.time()
    print("Starting ORE Studio services")
    print(f"  Preset : {ctx.preset}")
    print(f"  NATS   : {ctx.nats_url} (prefix: {ctx.nats_prefix}, "
          f"mTLS: {'enabled' if ctx.tls else 'disabled'})")
    print()

    print("[NATS server]")
    nats_pid = _read_pid(ctx.nats_pid_file)
    if nats_pid and _pid_alive(nats_pid):
        print(f"  skip    {'nats-server':<38} PID {nats_pid}")
    else:
        if not ctx.nats_config.exists():
            print(f"  error: NATS config not found: {ctx.nats_config}")
            print("         run: ./projects/ores.compass/compass.sh nats init")
            return 1
        ctx.nats_pid_file.parent.mkdir(parents=True, exist_ok=True)
        proc = subprocess.Popen(
            [nats_bin, "--config", str(ctx.nats_config),
             "-l", str(ctx.log_dir / "nats-server.log")],
            env=ctx.child_env(), start_new_session=True)
        ctx.nats_pid_file.write_text(f"{proc.pid}\n")
        print(f"  start   {'nats-server':<38} PID {proc.pid}")
    if not _wait_for_listen(ctx.nats_port):
        return 1
    print()

    print("[Controller]")
    _launch(ctx, "ores.controller.service", "ores.controller.service",
            ["--log-enabled", "--log-level", args.log_level,
             "--log-directory", "../log",
             "--nats-url", ctx.nats_url,
             "--nats-subject-prefix", ctx.nats_prefix]
            + _tls_args(ctx, "ores.controller.service"))
    ok = _wait_for_log(ctx, "ores.controller.service",
                       "All services started", 120)
    print()
    print(f"Logs     : {ctx.log_dir}")
    print(f"Stop     : compass services stop")
    print(f"Time     : {int(time.time() - start_ts)}s")
    return 0 if ok else 1


def _terminate(name, pid_file, grace) -> str:
    """SIGTERM (then SIGKILL after `grace` seconds) the PID in `pid_file`.

    Returns "stopped", "gone" (already dead / no PID file), for callers that
    tally results across several processes.
    """
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


def _find_orphans(ctx):
    """Scan the OS process table for anything belonging to this environment
    that outlived `cmd_stop` — e.g. a domain service the controller failed
    to cascade-kill (controller died before reaching stop_all(), or a
    prior session's process was never tracked by a PID file at all).

    Matches conservatively, scoped to *this* environment only, so a
    sibling worktree's fleet (different --nats-subject-prefix / NATS
    config) is never touched:
      - nats-server: command line contains this environment's NATS config
        path (ctx.nats_config).
      - every other service: carries this environment's
        --nats-subject-prefix (unique per checkout by construction — it
        embeds ORES_CHECKOUT_LABEL). Matched on the prefix alone, not on
        bin_dir: `ps` reports processes by the relative argv0 they were
        launched with (`./ores.foo.service`, since _launch() sets
        cwd=bin_dir), so bin_dir never actually appears in the command
        line — matching on it silently matches nothing. The Qt client is
        excluded — its lifecycle is managed by `compass client stop`,
        not `services stop`.

    Returns a list of (pid, cmdline) tuples.
    """
    try:
        out = subprocess.run(["ps", "-eo", "pid,args"],
                             capture_output=True, text=True, check=True)
    except (OSError, subprocess.CalledProcessError):
        return []

    nats_marker = str(ctx.nats_config)
    prefix_marker = f"--nats-subject-prefix {ctx.nats_prefix}"
    orphans = []
    for line in out.stdout.splitlines()[1:]:
        line = line.strip()
        if not line:
            continue
        pid_str, _, cmd = line.partition(" ")
        try:
            pid = int(pid_str)
        except ValueError:
            continue
        if not _pid_alive(pid):
            continue
        is_nats = "nats-server" in cmd and nats_marker in cmd
        is_service = prefix_marker in cmd and "ores.qt" not in cmd.split()[0]
        if is_nats or is_service:
            orphans.append((pid, cmd))
    return orphans


def _verify_no_orphans(ctx):
    """Run after cmd_stop's own termination sequence: catch anything that
    outlived it (controller-cascade failures, or processes that were never
    tracked by a PID file). SIGKILLs whatever it finds and reports it —
    there is no graceful path left to offer something `cmd_stop` already
    gave two SIGTERM rounds to reach."""
    orphans = _find_orphans(ctx)
    if not orphans:
        return 0
    print(f"\n⚠ WARNING: {len(orphans)} stray process(es) survived stop — "
          f"this means the controller failed to cascade-stop one or more "
          f"children (or a process from an earlier session was never "
          f"tracked by a PID file at all). Killing them now, but this is a "
          f"symptom worth investigating, not a routine cleanup:")
    for pid, cmd in orphans:
        binary = cmd.split()[0].split("/")[-1]
        try:
            os.kill(pid, signal.SIGKILL)
            print(f"  ⚠ killed  {binary:<38} PID {pid} (orphaned — untracked, "
                  f"or the controller failed to stop it)")
        except OSError as e:
            print(f"  ⚠ warn    {binary:<38} PID {pid}: could not kill ({e})")
    print("⚠ If this recurs, check ores.controller.service's own logs from "
          "the prior session for why stop_all() did not run or did not "
          "finish.")
    return len(orphans)


def cmd_stop(ctx, args):
    print(f"Stopping ORE Studio services ({ctx.preset})\n")
    stopped = gone = 0

    def _term(name, pid_file, grace):
        nonlocal stopped, gone
        if _terminate(name, pid_file, grace) == "stopped":
            stopped += 1
        else:
            gone += 1

    print("[Controller]")
    # The controller must stop all its children before it exits: 30 s grace.
    _term("ores.controller.service",
          ctx.run_dir / "ores.controller.service.pid", 30)

    # Clear stale PID files for anything already dead.
    if ctx.run_dir.is_dir():
        for f in ctx.run_dir.glob("*.pid"):
            pid = _read_pid(f)
            if pid is None or not _pid_alive(pid):
                f.unlink(missing_ok=True)

    print("[NATS server]")
    _term("nats-server", ctx.nats_pid_file, 10)

    orphan_count = _verify_no_orphans(ctx)

    if stopped + gone == 0 and orphan_count == 0:
        print(f"No services found for preset '{ctx.preset}'.")
        return 0
    print(f"Stopped  : {stopped} service(s), {gone} already gone"
          f"{f', {orphan_count} orphan(s) killed' if orphan_count else ''}.")
    return 0


def cmd_status(ctx, args):
    print(f"ORE Studio service status ({ctx.preset})\n")
    print(f"  {'STATUS':<10} {'SERVICE':<40} DETAIL")
    print(f"  {'-' * 10} {'-' * 40} ------")
    running = starting = stopped = missing = 0
    _ready = _log_contains

    nats_pid = _read_pid(ctx.nats_pid_file)
    nats_log = ctx.log_dir / "nats-server.log"
    if nats_pid is None:
        print(f"  {'missing':<10} {'nats-server':<40} (no PID file)")
    elif not _pid_alive(nats_pid):
        print(f"  {'stopped':<10} {'nats-server':<40} PID {nats_pid} (dead)")
    elif _ready(nats_log, "Server is ready"):
        print(f"  {'running':<10} {'nats-server':<40} PID {nats_pid}  "
              f"port {ctx.nats_port}")
    else:
        print(f"  {'starting':<10} {'nats-server':<40} PID {nats_pid}")

    def _check(svc):
        nonlocal running, starting, stopped, missing
        pid_file = ctx.run_dir / f"{svc}.pid"
        # Domain services log per-instance (name.0.log); the controller and
        # singletons log to name.log. Prefer whichever exists.
        log_file = ctx.log_dir / f"{svc}.log"
        if not log_file.exists() and (ctx.log_dir / f"{svc}.0.log").exists():
            log_file = ctx.log_dir / f"{svc}.0.log"
        pid = _read_pid(pid_file)
        if pid is None:
            print(f"  {'missing':<10} {svc:<40} (no PID file)")
            missing += 1
            return
        if not _pid_alive(pid):
            print(f"  {'stopped':<10} {svc:<40} PID {pid} (dead)")
            stopped += 1
            return
        if _ready(log_file, "Service ready"):
            print(f"  {'running':<10} {svc:<40} PID {pid}")
            running += 1
        else:
            last = _log_last_line(log_file).split('\"] ')[-1][:60]
            print(f"  {'starting':<10} {svc:<40} PID {pid}  "
                  f"{f'({last})' if last else ''}")
            starting += 1

    _check("ores.controller.service")
    if ctx.run_dir.is_dir():
        for pid_file in sorted(ctx.run_dir.glob("*.pid")):
            svc = pid_file.stem
            if svc != "ores.controller.service":
                _check(svc)

    print(f"\nservices: running={running}  starting={starting}  "
          f"stopped={stopped}  missing={missing}")
    print(f"\nLogs : {ctx.log_dir}")
    return 0


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
        description="Operate pillar: service lifecycle (nats-server + "
                    "controller; the controller spawns the domain services).")
    sub = ap.add_subparsers(dest="subcmd", required=True)

    st = sub.add_parser("start", help="Start nats-server and the controller "
                                      "(which spawns all services)")
    _common(st)
    st.add_argument("--log-level", default="trace")

    sp = sub.add_parser("stop", help="Stop the controller (and its children) "
                                     "then nats-server")
    _common(sp)

    su = sub.add_parser("status", help="Per-service status from PID files "
                                       "and readiness log lines")
    _common(su)

    cl = sub.add_parser("clear-logs", help="Delete all *.log / *.err under "
                                           "the preset's log directory")
    _common(cl)

    args = ap.parse_args(argv)
    env = load_env(project_root)
    validate_env_version(project_root, env)
    ctx = Ctx(project_root, env, args.preset)

    return {"start": cmd_start, "stop": cmd_stop, "status": cmd_status,
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
