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


def changed_cpp():
    out = subprocess.run(
        ["git", "status", "--porcelain"], cwd=ROOT, capture_output=True, text=True, check=True
    ).stdout
    files = []
    for line in out.splitlines():
        path = line[3:].strip()
        if path.endswith(".cpp"):
            files.append(str((ROOT / path).resolve()))
    return files


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
