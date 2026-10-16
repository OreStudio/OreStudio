#!/usr/bin/env python3
"""Syntax-check every changed translation unit using the build's own flags.

The full build serialises on a worktree lock and takes hours under load. The
changed files are a known, small set, so check exactly those against
compile_commands.json first. This proves each changed TU compiles; it does not
link, and it says nothing about untouched TUs.

    python3 .audit/check_changed_tus.py
"""
import json
import shlex
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DB = ROOT / "build/output/linux-clang-release-ninja/compile_commands.json"


def changed_cpp(base="origin/main"):
    """Changed .cpp files, committed or not.

    Measured against a base ref rather than against the working tree alone. The
    first version read `git status`, which sees nothing once the work is
    committed, so it reported "no changed .cpp files" instead of checking them.
    """
    files = set()
    for args in (
        ["git", "diff", "--name-only", "--diff-filter=ACMR", f"{base}...HEAD"],
        ["git", "diff", "--name-only", "--diff-filter=ACMR"],
        ["git", "ls-files", "--others", "--exclude-standard"],
    ):
        out = subprocess.run(args, cwd=ROOT, capture_output=True, text=True, check=False).stdout
        files.update(line.strip() for line in out.splitlines() if line.strip())

    found = []
    for name in sorted(files):
        if not name.endswith(".cpp"):
            continue
        path = (ROOT / name).resolve()
        if path.exists():
            found.append(str(path))
    return found


def syntax_only_argv(command: str):
    """Drop -c and -o, which clang rejects as unused next to -fsyntax-only."""
    argv = shlex.split(command)
    out = []
    skip_next = False
    for arg in argv:
        if skip_next:
            skip_next = False
            continue
        if arg == "-o":
            skip_next = True
            continue
        if arg == "-c":
            continue
        out.append(arg)
    return out + ["-fsyntax-only"]


def main():
    db = json.loads(DB.read_text())
    by_file = {e["file"]: e for e in db}
    targets = changed_cpp()
    if not targets:
        print("no changed .cpp files")
        return 0

    failed = []
    for path in targets:
        entry = by_file.get(path)
        if entry is None:
            print(f"SKIP  {path} (not in compile_commands.json)")
            continue
        res = subprocess.run(
            syntax_only_argv(entry["command"]),
            cwd=entry["directory"],
            capture_output=True,
            text=True,
        )
        name = str(Path(path).relative_to(ROOT))
        if res.returncode == 0:
            print(f"OK    {name}")
        else:
            print(f"FAIL  {name}")
            sys.stdout.write(res.stderr[:8000])
            failed.append(name)

    print()
    if failed:
        print(f"FAIL: {len(failed)} of {len(targets)} changed TU(s) do not compile.")
        return 1
    print(f"OK: all {len(targets)} changed translation units compile.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
