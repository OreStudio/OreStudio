#!/usr/bin/env python3
"""No file that git would commit holds a public source secret from .env.

A public source's key or password, such as ORES_MARKETDATA_BANXICO_API_KEY,
lives only in the checkout's .env, which git ignores. A provider row names the
variable as env:NAME, never its value. This check reads the values from .env and
searches every tracked file, and every untracked file that git does not ignore,
for each of them. A checkout without .env, or with no such variable, passes. A
user name is not a secret, and it is often the owner's address, which the
licence headers hold, so it is not searched.

It searches the files as they are now, not the history: a key that an earlier
commit holds needs a rewrite of history and a new key, which no check can give.

The report names the variable and the file, never the value. The value goes to
git on its standard input, so no process listing shows it.
"""
import re
import subprocess
import sys
from pathlib import Path

CREDENTIAL = re.compile(r"^ORES_MARKETDATA_(?!SERVICE_)[A-Z0-9_]+_(API_KEY|PASSWORD)$")
# A value shorter than this is too common to search for without false reports.
MIN_LENGTH = 8


def credentials(env_text: str) -> dict[str, str]:
    """The credential variables of a .env text and their values."""
    found: dict[str, str] = {}
    for line in env_text.splitlines():
        if not line or line.lstrip().startswith("#") or "=" not in line:
            continue
        name, _, value = line.partition("=")
        name = name.strip()
        value = value.strip().strip('"').strip("'")
        if CREDENTIAL.match(name) and len(value) >= MIN_LENGTH:
            found.setdefault(name, value)
    return found


def files_holding(root: Path, value: str) -> list[str]:
    """The files git would commit that hold the value."""
    run = subprocess.run(
        ["git", "grep", "--untracked", "-l", "-F", "-z", "-f", "-"],
        cwd=root, input=value.encode() + b"\n", capture_output=True)
    if run.returncode not in (0, 1):
        raise RuntimeError(run.stderr.decode(errors="replace"))
    return [p for p in run.stdout.decode(errors="replace").split("\0") if p]


def main(root: Path) -> int:
    env = root / ".env"
    if not env.is_file():
        print("no .env in this checkout")
        return 0
    found = credentials(env.read_text(errors="replace"))
    leaks = [(name, path) for name, value in sorted(found.items())
             for path in files_holding(root, value)]
    for name, path in leaks:
        print(f"{path}: holds the value of {name}")
    if leaks:
        print(f"{len(leaks)} file(s) hold a credential: take it out, and name it as env:NAME")
        return 1
    print(f"no file holds any of {len(found)} credential(s) of .env")
    return 0


if __name__ == "__main__":
    sys.exit(main(Path(__file__).resolve().parents[2]))
