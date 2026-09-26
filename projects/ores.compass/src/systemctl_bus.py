"""systemctl-shaped front end for the user systemd manager, over busctl.

compass drives the fleet with `systemctl --user`. Inside a bubblewrap
sandbox the user manager refuses that client: the sandbox's user namespace
leaves the peer credentials unrepresentable (SO_PEERCRED reports pid 0), so
the manager drops the connection while sd-bus authenticates. The same
manager answers busctl over the D-Bus session bus.

This module keeps systemctl's command shape and its CompletedProcess
result, so a caller does not change, and adds what busctl needs instead:
method calls, a wait for the verbs systemctl blocks on, unit-name escaping
for object paths, and the text the listing verbs print.

Select it with ORES_USE_BUSCTL=1: the checkout's .env sets the default for
every compass command, and the same variable in the process environment
overrides the file for one command, either way.

Two deliberate departures from systemctl are documented where they happen:
a unit the manager does not know prints nothing rather than "inactive",
and show-environment is a reachability probe because systemd 261 dropped
the method.
"""

import fnmatch
import json
import os
import subprocess
import time

MANAGER = "org.freedesktop.systemd1"
MANAGER_PATH = "/org/freedesktop/systemd1"
MANAGER_IFACE = "org.freedesktop.systemd1.Manager"
UNIT_IFACE = "org.freedesktop.systemd1.Unit"
SERVICE_IFACE = "org.freedesktop.systemd1.Service"
JOB_IFACE = "org.freedesktop.systemd1.Job"

# systemctl prints this when the user-scope bus is unreachable. It is the
# one failure the busctl path exists to route around, so it is matched
# rather than passed through silently.
CONNECT_FAILURE_MARKER = "Failed to connect to user scope bus"

# systemd's default job timeout is 90s per unit; a target that wants many
# units can legitimately outlast that, so allow more before giving up. The
# wait is bounded rather than infinite so a wedged job cannot hang compass.
DEFAULT_JOB_TIMEOUT = 300.0
JOB_POLL_INTERVAL = 0.2

# ListUnits returns one struct of ten fields per unit.
_LIST_UNITS_FIELDS = 10

_use_busctl = None
_hint_printed = False

# The spellings compass accepts for a true value. Kept in one place so the
# .env value and the process environment cannot drift apart.
_TRUTHY = ("1", "true", "yes", "on")


def _truthy(value: object) -> bool:
    return str(value if value is not None else "").strip().lower() in _TRUTHY


def set_use_busctl(enabled) -> None:
    """Set the transport choice. None means no opinion."""
    global _use_busctl
    _use_busctl = None if enabled is None else bool(enabled)


def adopt_transport_setting(env: dict) -> None:
    """Resolve the transport choice from the environment, then the .env.

    compass reads .env into a dict and never writes os.environ, so a value
    that lives only in the file is invisible to use_busctl(). Every caller
    that has the dict must hand it over, or the file's choice does nothing.

    The process environment wins in both directions, so an operator can
    switch the bus off for one command as well as on. An empty value at
    either level means no opinion, not "off", so a stray empty variable in
    a shell cannot silently mask the checkout's choice.
    """
    raw = os.environ.get("ORES_USE_BUSCTL", "").strip()
    if not raw:
        raw = str(env.get("ORES_USE_BUSCTL", "")).strip()
    if not raw:
        return
    set_use_busctl(_truthy(raw))


def use_busctl() -> bool:
    """Whether systemctl calls go through busctl."""
    if _use_busctl is not None:
        return _use_busctl
    return _truthy(os.environ.get("ORES_USE_BUSCTL", ""))


def _escape_unit(name: str) -> str:
    """systemd's bus_path_escape: keep ASCII alphanumerics, hex the rest."""
    out = []
    for ch in name:
        if ("a" <= ch <= "z") or ("A" <= ch <= "Z") or ("0" <= ch <= "9"):
            out.append(ch)
        else:
            out.append("_%02x" % ord(ch))
    return "".join(out)


def _unit_path(unit: str) -> str:
    return f"{MANAGER_PATH}/unit/{_escape_unit(unit)}"


def _tokens(text: str) -> list:
    """Every value busctl printed, quoted or bare.

    busctl quotes strings, object paths and signatures, but prints an
    integer or a boolean bare, so a struct mixes the two: ListUnits' job_id
    is a bare number sitting between two quoted fields. Grouping on quoted
    values alone would silently shift every record by one field."""
    tokens = []
    index = 0
    length = len(text)
    while index < length:
        ch = text[index]
        if ch == '"':
            index += 1
            buffer = []
            while index < length and text[index] != '"':
                if text[index] == "\\" and index + 1 < length:
                    buffer.append(text[index + 1])
                    index += 2
                else:
                    buffer.append(text[index])
                    index += 1
            index += 1
            tokens.append("".join(buffer))
        elif ch.isspace():
            index += 1
        else:
            start = index
            while index < length and not text[index].isspace() \
                    and text[index] != '"':
                index += 1
            tokens.append(text[start:index])
    return tokens


def _parse_values(text: str) -> list:
    """Every double-quoted value on the line, unescaped.

    Used for single-value results (a property, a method's object path),
    where the only things on the line are the type letter and the value."""
    values = []
    index = 0
    length = len(text)
    while index < length:
        if text[index] != '"':
            index += 1
            continue
        index += 1
        buffer = []
        while index < length and text[index] != '"':
            if text[index] == "\\" and index + 1 < length:
                buffer.append(text[index + 1])
                index += 2
            else:
                buffer.append(text[index])
                index += 1
        index += 1
        values.append("".join(buffer))
    return values


def _array_values(text: str) -> list:
    """Values of an array result, with busctl's 'a(...) N' header removed.

    busctl prints the header on the same line as the values when the array
    is small, so the header is recognised by shape rather than by line."""
    tokens = _tokens(text)
    if tokens and "(" in tokens[0] and tokens[0].endswith(")"):
        return tokens[2:]
    return tokens


def _completed(argv, returncode, stdout="", stderr=""):
    return subprocess.CompletedProcess(argv, returncode, stdout, stderr)


def _finish(result, kwargs):
    """Apply the caller's stream and check semantics to a result."""
    if kwargs.get("stdout") is subprocess.DEVNULL:
        result.stdout = None
    if kwargs.get("stderr") is subprocess.DEVNULL:
        result.stderr = None
    if kwargs.get("check") and result.returncode != 0:
        raise subprocess.CalledProcessError(
            result.returncode, result.args, output=result.stdout,
            stderr=result.stderr)
    return result


def _busctl(*args, timeout=30):
    """Run one busctl invocation against the user bus."""
    try:
        return subprocess.run(
            ["busctl", "--user", *args],
            capture_output=True, text=True, timeout=timeout, check=False)
    except FileNotFoundError:
        return _completed(["busctl"], 127, "",
                          "busctl not found on PATH\n")
    except subprocess.TimeoutExpired:
        return _completed(["busctl", *args], 124, "",
                          "busctl timed out\n")


def _get_property(object_path, interface, name):
    """One property, or None when the object or the property is absent.

    A job object is collected the moment its job finishes, so None here
    legitimately means "gone", which is how the job wait detects
    completion as well as failure."""
    proc = _busctl("get-property", MANAGER, object_path, interface, name)
    if proc.returncode != 0:
        return None
    values = _parse_values(proc.stdout)
    return values[0] if values else None


def _method(method, signature, *args, timeout=30):
    call = ["call", MANAGER, MANAGER_PATH, MANAGER_IFACE, method]
    # A no-argument method takes no signature argument at all; passing an
    # empty string makes busctl reject the call.
    if signature:
        call.append(signature)
    call.extend(args)
    proc = _busctl(*call, timeout=timeout)
    if proc.returncode != 0:
        return proc.returncode, "", proc.stderr or proc.stdout
    return 0, proc.stdout, ""


def _wait_for_job(job_path, unit, timeout):
    """Block until the job finishes, as `systemctl start` does.

    busctl returns a job object path as soon as the job is enqueued, so the
    wait has to be explicit. The job vanishes when it completes, so a
    failed property read means "finished", and the unit's own state is what
    decides success."""
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        state = _get_property(job_path, JOB_IFACE, "State")
        if state is None or state == "done":
            return True
        time.sleep(JOB_POLL_INTERVAL)
    return False


def _action(verb, unit, timeout):
    method = {"start": "StartUnit", "stop": "StopUnit",
              "restart": "RestartUnit"}[verb]
    code, text, error = _method(method, "ss", unit, "replace",
                                timeout=int(timeout))
    if code != 0:
        return _completed(["systemctl", "--user", verb, unit], code, "",
                          error)

    jobs = _parse_values(text)
    if jobs and not _wait_for_job(jobs[0], unit, timeout):
        return _completed(
            ["systemctl", "--user", verb, unit], 1, "",
            f"Timed out waiting for {verb} {unit} to finish.\n")

    active = _get_property(_unit_path(unit), UNIT_IFACE, "ActiveState")
    if verb == "stop":
        ok = active in ("inactive", "failed") or active is None
    else:
        ok = active == "active"
        if not ok:
            # A Type=oneshot service is "inactive" once its commands
            # succeeded, and only Result distinguishes that from a failure.
            result = _get_property(_unit_path(unit), SERVICE_IFACE, "Result")
            ok = result == "success"
    if ok:
        return _completed(["systemctl", "--user", verb, unit], 0, "", "")
    return _completed(
        ["systemctl", "--user", verb, unit], 1, "",
        f"Job for {unit} failed: the unit is {active or 'unknown'}.\n")


def _is_active(unit):
    load = _get_property(_unit_path(unit), UNIT_IFACE, "LoadState")
    if load is None or load == "not-found":
        # Deliberately silent: compass reads an empty answer as "missing",
        # which is what an undeployed unit is. systemctl would say
        # "inactive" here and lose that distinction.
        return _completed(["systemctl", "--user", "is-active", unit], 3, "", "")
    state = _get_property(_unit_path(unit), UNIT_IFACE, "ActiveState") or ""
    returncode = 0 if state == "active" else 3
    return _completed(["systemctl", "--user", "is-active", unit], returncode,
                      f"{state}\n" if state else "", "")


def _reload():
    code, _text, error = _method("Reload", "")
    return _completed(["systemctl", "--user", "daemon-reload"], code, "", error)


def _reset_failed(units):
    if not units:
        code, _text, error = _method("ResetFailed", "")
        return _completed(["systemctl", "--user", "reset-failed"], code, "",
                          error)
    for unit in units:
        code, _text, error = _method("ResetFailedUnit", "s", unit)
        if code != 0:
            return _completed(["systemctl", "--user", "reset-failed", unit],
                              code, "", error)
    return _completed(["systemctl", "--user", "reset-failed"], 0, "", "")


def _list_unit_files(patterns):
    # One unfiltered call, filtered here: ListUnitFilesByPatterns takes two
    # arrays of the same type, and busctl cannot tell where the first ends.
    code, text, error = _method("ListUnitFiles", "")
    if code != 0:
        return _completed(["systemctl", "--user", "list-unit-files"], code,
                          "", error)
    values = _array_values(text)
    lines = []
    for index in range(0, len(values) - 1, 2):
        name = os.path.basename(values[index])
        if patterns and not any(fnmatch.fnmatch(name, p) for p in patterns):
            continue
        lines.append(f"{name:<48} {values[index + 1]}")
    return _completed(["systemctl", "--user", "list-unit-files"], 0,
                      "\n".join(lines) + ("\n" if lines else ""), "")


def _list_units(args):
    code, text, error = _method("ListUnits", "")
    if code != 0:
        return _completed(["systemctl", "--user", "list-units"], code, "",
                          error)
    values = _array_values(text)

    unit_type = None
    as_json = False
    for index, arg in enumerate(args):
        if arg.startswith("--type="):
            unit_type = arg.split("=", 1)[1]
        elif arg == "--type" and index + 1 < len(args):
            unit_type = args[index + 1]
        elif arg == "--output=json":
            as_json = True

    # ListUnits returns a(ssssssouso): id, description, load, active, sub,
    # following, object path, job id, job type, job path.
    entries = []
    for index in range(0, len(values) - (_LIST_UNITS_FIELDS - 1),
                       _LIST_UNITS_FIELDS):
        name = values[index]
        if unit_type and not name.endswith("." + unit_type):
            continue
        entries.append({
            "unit": name,
            "load": values[index + 2],
            "active": values[index + 3],
            "sub": values[index + 4],
            "description": values[index + 1],
        })

    if as_json:
        return _completed(["systemctl", "--user", "list-units"], 0,
                          json.dumps(entries) + "\n", "")
    lines = [f"{e['unit']:<48} {e['load']:<8} {e['active']:<8} {e['sub']}"
             for e in entries]
    return _completed(["systemctl", "--user", "list-units"], 0,
                      "\n".join(lines) + ("\n" if lines else ""), "")


def _show_environment():
    """Reachability probe, not an environment dump.

    systemd 261 removed Manager.ShowEnvironment, and systemctl's own
    show-environment is listed in its help but has nothing to call. The
    only caller treats success as "the user manager is reachable", so that
    is what this reports."""
    code, _values, error = _method("GetUnit", "s", "default.target")
    return _completed(["systemctl", "--user", "show-environment"], code, "",
                      error)


def _dispatch(args, timeout):
    if not args:
        return _completed(["systemctl", "--user"], 1, "",
                          "no systemctl verb given\n")
    verb, rest = args[0], list(args[1:])
    if verb == "is-active":
        return _is_active(rest[0]) if rest else _completed(
            ["systemctl", "--user", verb], 1, "", "no unit given\n")
    if verb in ("start", "stop", "restart"):
        if not rest:
            return _completed(["systemctl", "--user", verb], 1, "",
                              "no unit given\n")
        return _action(verb, rest[0], timeout)
    if verb == "daemon-reload":
        return _reload()
    if verb == "reset-failed":
        return _reset_failed(rest)
    if verb == "list-unit-files":
        return _list_unit_files(rest)
    if verb == "list-units":
        return _list_units(rest)
    if verb == "show-environment":
        return _show_environment()
    return _completed(
        ["systemctl", "--user", *args], 2, "",
        f"the busctl transport does not implement '{verb}' yet; "
        f"run it with systemctl directly.\n")


def run(args, **kwargs):
    """Run one systemctl --user command, by either transport.

    `args` are the arguments after the executable, with or without a
    leading --user. Returns a CompletedProcess whose stdout, stderr and
    returncode match what systemctl would have produced, so callers that
    parse the text keep working."""
    argv = ["systemctl", "--user"] + [a for a in args if a != "--user"]
    if not use_busctl():
        return subprocess.run(argv, **kwargs)

    timeout = float(kwargs.get("timeout") or DEFAULT_JOB_TIMEOUT)
    result = _dispatch([a for a in args if a != "--user"], timeout)
    result.args = argv
    return _finish(result, kwargs)


def sandbox_hint() -> str:
    """The suggestion to print when a systemctl call could not connect."""
    return ("  If you are running inside a sandbox, set ORES_USE_BUSCTL=1 in "
            ".env, or prefix the command with ORES_USE_BUSCTL=1.")
