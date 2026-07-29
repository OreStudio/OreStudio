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

The slice itself (app-claude.slice, see systemd/app-claude.slice next to this
file) turns on accounting only -- no MemoryHigh/MemoryMax -- this is
isolation, not a resource cap; the OOM killer still does its job at the finer
per-scope granularity.

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

_SLICE_NAME = "app-claude.slice"
_SLICE_SRC = Path(__file__).resolve().parent / "systemd" / _SLICE_NAME


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
            f"--slice={_SLICE_NAME}",
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
    """Copy the checked-in slice unit to ~/.config/systemd/user/ if it is
    missing or stale, and daemon-reload so systemd-run sees it immediately.
    """
    dest_dir = Path.home() / ".config" / "systemd" / "user"
    dest = dest_dir / _SLICE_NAME

    if dest.exists() and filecmp.cmp(_SLICE_SRC, dest, shallow=False):
        return

    dest_dir.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(_SLICE_SRC, dest)
    subprocess.run(["systemctl", "--user", "daemon-reload"],
                    stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                    check=False)
