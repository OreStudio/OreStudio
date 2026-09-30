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


# The journal is the only log source. Every unit logs to the console and
# systemd captures it, so one interface reaches the whole fleet, including
# nats-server, which never wrote a file.
JOURNAL_LINES = 400
JOURNAL_TIMEOUT_S = 20


def _journal_lines(unit, lines=JOURNAL_LINES):
    """The most recent journal lines for one unit, oldest first.

    `-o cat` drops the journal's own prefix, so a caller sees what the
    service wrote. A journal that cannot be read and a service that has
    written nothing both come back empty; a caller reports the count rather
    than guessing which it was."""
    cmd = ["journalctl", "--user", "-u", f"{unit}.service", "--no-pager",
           "-n", str(lines), "-o", "cat"]
    try:
        out = subprocess.run(cmd, capture_output=True, text=True,
                             timeout=JOURNAL_TIMEOUT_S)
    except (OSError, subprocess.TimeoutExpired):
        return []
    if out.returncode:
        return []
    return out.stdout.splitlines()


def _journal_contains(unit, pattern: str) -> bool:
    """Whether the unit's recent journal output carries PATTERN."""
    return any(pattern in line for line in _journal_lines(unit))


def _journal_last_line(unit) -> str:
    """The unit's last journal line, trimmed to the message.

    A service logs a JSON envelope, so the interesting part is what follows
    the closing bracket of the envelope."""
    found = _journal_lines(unit, lines=1)
    if not found:
        return ""
    return found[-1].split('\"] ')[-1].strip()[:120]


def _short_name(service_name):
    """The name a person uses for a registry service: `ores.web.service`
    reads as `web`, and `ores.http.server` as `http.server`."""
    name = service_name
    if name.startswith("ores."):
        name = name[len("ores."):]
    if name.endswith(".service"):
        name = name[: -len(".service")]
    return name


def _unit_rows(ctx, only=None):
    """Every concrete systemd unit this environment's target aggregates,
    as a dict. nats-server is NOT included: it has no readiness log file
    under systemd (see _nats_unit) and is tracked separately.

    `unit` is the systemd unit basename, and it carries the -<env> suffix
    systemd_generate.py uses to keep this checkout's units distinct from a
    sibling checkout's on the same user session. `log` is what the *binary
    itself* names its log file, which is NOT unit-name-based: it is always
    "<service_name>.<replica_index>.log", with replica_index 0 even for
    singletons, per the shared args_template. The environment name does not
    enter it, because each checkout's build/output/<preset>/publish/log/ is
    already its own directory.

    `replica` is 0 for a service that runs one copy, and 1..N otherwise.
    `label` is what the view shows."""
    rows = []
    services = systemd_generate.load_service_registry(ctx.root)
    for d in systemd_generate.fetch_service_definitions(services):
        if not d["enabled"]:
            continue
        if only is not None and d["service_name"] != only:
            continue
        base = systemd_generate._unit_basename(d["service_name"], ctx.env_name)
        short = _short_name(d["service_name"])
        if d["desired_replicas"] > 1:
            rows += [{"unit": f"{base}-{r}",
                      "service": d["service_name"],
                      "replica": r,
                      "label": f"{short}-{r}",
                      "log": f"{d['service_name']}.{r}.log",
                      "runtime": d.get("runtime", "native")}
                     for r in range(1, d["desired_replicas"] + 1)]
        else:
            rows.append({"unit": base, "service": d["service_name"],
                         "replica": 0, "label": short,
                         "log": f"{d['service_name']}.0.log",
                         "runtime": d.get("runtime", "native")})
    return rows


def _service_units(ctx, only=None):
    """(unit, log_basename) pairs, the shape the start and stop paths use."""
    return [(row["unit"], row["log"]) for row in _unit_rows(ctx, only=only)]


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


def _service_ready(ctx, unit_pairs, timeout=300):
    """Wait until every unit is active.

    ActiveState is the readiness signal for the compiled services, because
    they are Type=notify: systemd reports the unit active only once the
    service itself called sd_notify(READY=1). There is no log to scan any
    more, and scanning one would only restate that. nats-server and
    ores.web are Type=simple; systemd reports them active when the process
    is up, and nats blocks on its own port check before its start job
    completes."""
    pending = {unit for unit, _ in unit_pairs}
    ready = {}
    print(f"  wait    {len(pending)} unit(s) (active)", end="", flush=True)
    deadline = time.time() + timeout
    while pending and time.time() < deadline:
        for unit in sorted(pending):
            if _unit_active_state(unit) == "active":
                ready[unit] = True
                pending.discard(unit)
        if pending:
            print(".", end="", flush=True)
            time.sleep(0.5)
    if ready:
        print(" ... done" if not pending else "", end="")
    if pending:
        print(f" ... timeout ({len(pending)} still not active: "
              f"{', '.join(sorted(pending))})")
    else:
        print()
    ready.update({unit: False for unit in pending})
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
    broken_pairs = [(u, log) for u, log in unit_pairs if u in broken]
    if broken_pairs:
        return _service_ready(ctx, broken_pairs, timeout=120)
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


# The five states the fleet reports. `stopped` is a unit the manager knows
# and is not running, which is usually an operator's choice. `failed` is a
# unit the manager tried to run and could not, and it is separate because a
# crashed service must not read as a deliberate shutdown. `missing` is a
# unit the manager has never heard of, which means this environment's units
# were never deployed.
SERVICE_STATES = ("running", "starting", "stopped", "failed", "missing")


def classify_unit(ctx, unit, runtime="native"):
    """(state, detail) for one unit.

    `running` means the unit is active. For the compiled services that is
    the whole rule, because they are Type=notify: systemd reports the unit
    active only after the service called sd_notify(READY=1). `ores.web` is
    Type=simple and cannot declare itself, so its readiness line is read
    from the journal. nats-server is reported on ActiveState alone, as it
    always was; its unit's port check is what the start path waits on."""
    active = _unit_active_state(unit)
    if active == "missing":
        return "missing", "unit not loaded"
    if active == "failed":
        return "failed", _journal_last_line(unit) or "failed"
    if active == "active":
        if runtime != "node":
            return "running", "active"
        if _journal_contains(unit, "Service ready"):
            return "running", "active"
        return "starting", _journal_last_line(unit)
    if active == "activating":
        return "starting", _journal_last_line(unit) or active
    return "stopped", active


def gather_units(ctx):
    """Every unit this environment runs, with its state and detail.

    One pass, and the only place a unit is classified. The status table,
    the counts line, and the JSON contract all read this, so they cannot
    disagree about whether the same unit is up.

    Returns {"nats": row|None, "units": [row], "counts": {state: n}}. The
    counts cover the service units only; nats-server is reported on its own
    because it has no readiness log and so is not classified the same way.
    """
    counts = {state: 0 for state in SERVICE_STATES}
    rows = []
    nats = None

    if not ctx.env_name:
        return {"nats": None, "units": [], "counts": counts,
                "service_total": 0}

    # nats-server's systemd unit has no -l logfile flag (unlike the old
    # native-process launch), so readiness is ActiveState alone.
    nats_unit = _nats_unit(ctx)
    state, detail = classify_unit(ctx, nats_unit)
    nats = {"unit": nats_unit, "service": "", "replica": 0,
            "label": "nats-server", "log": "", "runtime": "native",
            "state": state, "detail": detail}

    for row in _unit_rows(ctx):
        state, detail = classify_unit(ctx, row["unit"], row["runtime"])
        counts[state] += 1
        rows.append({**row, "state": state, "detail": detail})

    return {"nats": nats, "units": rows, "counts": counts,
            "service_total": len(rows)}


def gather_counts(ctx):
    """Service state counts for status displays: dict of state -> count,
    plus nats state. Reads the same classification as the status table."""
    gathered = gather_units(ctx)
    return {"nats": gathered["nats"]["state"] if gathered["nats"] else "missing",
            "counts": gathered["counts"],
            "service_total": gathered["service_total"]}


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
    ok = _service_ready(ctx, units)

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

    print(f"[systemctl --user start {name}]")
    for unit, _log in units:
        result = _systemctl(["start", f"{unit}.service"], check=False)
        if result.returncode != 0:
            print(result.stderr, file=sys.stderr)
            return 1
    print()

    if not _wait_for_listen(ctx.nats_port):
        return 1

    ok = _service_ready(ctx, units)
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
            print(f"[systemctl --user restart {name}]")
        for unit, _log in units:
            result = _systemctl(["restart", f"{unit}.service"], check=False)
            if result.returncode != 0:
                print(result.stderr, file=sys.stderr)
                return 1
        return 0 if _service_ready(ctx, units) else 1

    units = _service_units(ctx)
    print(f"[systemctl --user restart {ctx.target_name}]")
    result = _systemctl(["restart", ctx.target_name], check=False)
    if result.returncode != 0 and "not loaded" not in (result.stderr or ""):
        print(result.stderr, file=sys.stderr)
        return 1
    return 0 if _service_ready(ctx, units) else 1


def cmd_status(ctx, args):
    print(f"ORE Studio service status ({ctx.preset})\n")
    print(f"  {'STATUS':<10} {'SERVICE':<40} DETAIL")
    print(f"  {'-' * 10} {'-' * 40} ------")

    if not ctx.env_name:
        print("error: ORES_ENV_NAME not set in .env", file=sys.stderr)
        return 1

    counts = {state: 0 for state in SERVICE_STATES}

    def _check(unit, log_basename=None):
        state, detail = classify_unit(ctx, unit, log_basename)
        print(f"  {state:<10} {unit:<40} {f'({detail})' if detail else ''}")
        counts[state] += 1

    nats = None
    if args.service:
        name, units = _resolve_service_or_report(ctx, args.service)
        if name is None:
            return 1
        for unit, log_basename in units:
            _check(unit, log_basename)
    else:
        # nats-server is reported on its own line below, because it has no
        # readiness log and so is not classified the way a service is.
        nats, _ = classify_unit(ctx, _nats_unit(ctx))
        for unit, log_basename in _service_units(ctx):
            _check(unit, log_basename)

    print("\nservices: " + "  ".join(f"{s}={counts[s]}"
                                     for s in SERVICE_STATES))
    if nats is not None:
        print(f"nats    : {nats}")
    print("\nJournal: journalctl --user -u <service>-" + ctx.env_name
          + ".service   (or: journalctl --user -u 'ores*-" + ctx.env_name + "*')")
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
    systemctl_bus.adopt_transport_setting(env)
    validate_env_version(project_root, env)
    ctx = Ctx(project_root, env, args.preset)

    return {"start": cmd_start, "stop": cmd_stop, "status": cmd_status,
            "restart": cmd_restart, "tree": cmd_tree, "top": cmd_top,
            "clear-logs": cmd_clear_logs}[args.subcmd](ctx, args)
