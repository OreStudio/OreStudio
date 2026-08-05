"""compass claude -- launch Claude Code inside its own systemd --user scope.

WHY THIS EXISTS

Emacs runs as PID 1 of emacs.service, and forked children inherit their
parent's cgroup. So every Claude session, build and LSP server launched from
Emacs is accounted for inside emacs.service. systemd-oomd sees one enormous
blob and, under memory pressure, kills the entire unit -- editor included.

Running Claude in a transient scope under its own slice makes it a sibling of
emacs.service under app.slice rather than a child, so oomd evaluates it as its
own kill candidate and takes out the actual offender instead of the editor.
A scope contains all descendants, so anything Claude spawns is isolated too.

Each session actually lands in a per-environment child slice,
app-claude-<env>.slice (app-claude.slice itself, see systemd/app-claude.slice
next to this file, turns on accounting only, no caps). The per-environment
slice carries the real limits, from the app-claude-.slice.d/50-limits.conf
drop-in next to this file: MemoryHigh/MemoryMax/MemorySwapMax/CPUWeight. That
is the difference between isolation (which slice a session is a sibling of)
and containment (a memory ceiling that makes the cgroup-local OOM fire before
kernel-wide OOM does) -- see systemd-resource-management-plan.org for why
both matter.

This is opt-in: it changes nothing for anyone invoking the regular `claude`
binary directly. Use `compass claude` (or `compass.sh claude`) when you want
the scoped launch; every checkout already carries compass.sh and the slice
unit source, so there is no separate ~/.local/bin install step to bootstrap
on a new machine -- the unit is deployed on first use.

Inspect live accounting with: systemd-cgtop --user
"""

import filecmp
import os
import re
import shutil
import subprocess
from pathlib import Path

_SLICE_ROOT = "app-claude"
_SLICE_NAME = f"{_SLICE_ROOT}.slice"
_SYSTEMD_SRC_DIR = Path(__file__).resolve().parent / "systemd"
_SLICE_SRC = _SYSTEMD_SRC_DIR / _SLICE_NAME
_LIMITS_DROPIN_REL = Path(f"{_SLICE_ROOT}-.slice.d") / "50-limits.conf"
_LIMITS_DROPIN_SRC = _SYSTEMD_SRC_DIR / _LIMITS_DROPIN_REL


def _slice_name(env_name: str) -> str:
    """The per-environment slice a Claude session for env_name lands in.

    systemd creates app-claude-<env>.slice (and its app-claude.slice
    parent) implicitly from this name -- no per-environment unit file is
    needed, only the dash-truncated drop-in at
    app-claude-.slice.d/50-limits.conf (see _ensure_slice_deployed).
    """
    return f"{_SLICE_ROOT}-{env_name}.slice"


def run(argv, project_root=None) -> int:
    real = shutil.which("claude")
    if real is None:
        print("compass claude: cannot find the `claude` binary on PATH",
              flush=True)
        return 127

    if _has_user_systemd():
        _ensure_slice_deployed()
        env_name = _env_name(project_root)
        cmd = [
            "systemd-run", "--user", "--scope", "-q", "--collect",
            f"--unit=claude-{env_name}-{os.getpid()}",
            f"--slice={_slice_name(env_name)}",
            f"--description=Claude Code session ({env_name}, pid {os.getpid()})",
            real, *argv,
        ]
        os.execvp(cmd[0], cmd)

    print("compass claude: no user systemd manager; running unscoped",
          flush=True)
    os.execvp(real, [real, *argv])


def _env_name(project_root) -> str:
    """Resolve the environment's identity for use in a systemd unit name.

    Reads ORES_ENV_NAME from the checkout's .env, falling back to the
    checkout directory's own name, then sanitises it to the characters
    systemd unit names allow.
    """
    name = None
    if project_root is not None:
        env_file = Path(project_root) / ".env"
        if env_file.is_file():
            for line in env_file.read_text(encoding="utf-8").splitlines():
                if line.lstrip().startswith("ORES_ENV_NAME="):
                    name = line.partition("=")[2].strip().strip("'\"")
                    break
        if not name:
            name = Path(project_root).name

    name = name or "unknown"
    name = re.sub(r"[^A-Za-z0-9_.-]", "_", name)
    return name or "unknown"


def _has_user_systemd() -> bool:
    if shutil.which("systemd-run") is None:
        return False
    try:
        subprocess.run(
            ["systemctl", "--user", "show-environment"],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)
        return True
    except (subprocess.CalledProcessError, OSError):
        return False


def _ensure_slice_deployed() -> None:
    """Copy every checked-in unit/drop-in this module owns to
    ~/.config/systemd/user/ if missing or stale, and daemon-reload once if
    anything changed so systemd-run sees it immediately.

    Manifest of (source, destination-relative-path) pairs -- add future
    app-claude-.slice.d/ drop-ins here rather than writing a new one-off
    deploy function.
    """
    dest_dir = Path.home() / ".config" / "systemd" / "user"
    manifest = [
        (_SLICE_SRC, Path(_SLICE_NAME)),
        (_LIMITS_DROPIN_SRC, _LIMITS_DROPIN_REL),
    ]

    changed = False
    for src, dest_rel in manifest:
        dest = dest_dir / dest_rel
        if dest.exists() and filecmp.cmp(src, dest, shallow=False):
            continue
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(src, dest)
        changed = True

    if changed:
        subprocess.run(["systemctl", "--user", "daemon-reload"],
                        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                        check=False)
