#!/usr/bin/env python3
"""Run a component's generated shell recipes against the live fleet.

V04 of the Component Clean Standard asks that every generated shell command
runs against the live fleet and answers, and that any command which cannot run
is recorded with its reason.  This script is that check.

The recipes are generated one-liners under
projects/ores.shell/scripts/library/<group>, one directory per entity.  Each is
replayed in its own shell process, behind a sign-in preamble, so one failing
recipe cannot hide the verdict of the next.

Usage:
    1. Bootstrap the local administrator once, in bootstrap mode:

        printf 'bootstrap create-initial-admin admin <password> <email>\n' \
            | ores.shell

    2. Export the same credential and run this script, naming the recipe groups
       the component owns:

        export V04_PRINCIPAL=admin V04_PASSWORD=<password>
        projects/ores.codegen/scripts/check_shell_recipes.py system_settings

    With no group named it replays the assets groups, which is what it did
    before it took an argument.

A destructive recipe is named in --skip and recorded NOT_REPLAYED rather than
run.  A component whose sign-in is a hand-written command names it in
--login-command; the default is the top-level 'login'.

Writes a TSV and a per-recipe raw log under --out-dir, which defaults to
.audit/clean-shell-recipes/. The .audit tree is not committed, so rerun this
script to regenerate the evidence.
"""

from __future__ import annotations

import argparse
import os
import subprocess
import tempfile
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]
SHELL = REPO / "build/output/linux-clang-debug-make/publish/bin/ores.shell"
LIBRARY = REPO / "projects/ores.shell/scripts/library"
# The groups the script replays when the caller names none: the component this
# lever was first written for.
DEFAULT_GROUPS = ("images", "image_tags", "tags")
PREFIX = "projects/ores.shell/scripts/library"
DEFAULT_OUT_DIR = REPO / ".audit/clean-shell-recipes"

# A command that names a verb the shell does not know is a wiring defect.  Any
# other refusal is the command answering, which is what V04 asks for.
NOT_WIRED = "Wrong command"

# An aborted script is not an answer.  The shell stops a script at its first
# failing command, so everything after it never ran: counting an abort as an
# answer hides both the failing preamble and the command that never executed.
ABORTED = "Script aborted"

# A recipe that must not be run against the fleet says so.  Skipping is a
# verdict, not a silence: the row is written with its reason.
NOT_REPLAYED = "NOT_REPLAYED"


def recipe_paths(groups: tuple[str, ...], skip: set[str]) -> list[Path]:
    found: list[Path] = []
    for group in groups:
        directory = LIBRARY / group
        if not directory.is_dir():
            raise SystemExit(f"no recipe group at {directory}")
        found.extend(sorted(p for p in directory.glob("*.ores") if p.stem not in skip))
    return found


def body_of(recipe: Path) -> str:
    """Return the recipe's command lines, without its comments."""
    lines = []
    for line in recipe.read_text().splitlines():
        stripped = line.strip()
        if stripped and not stripped.startswith("#"):
            lines.append(stripped)
    return "\n".join(lines)


def classify(recipe: Path, transcript: str) -> tuple[str, str]:
    """Return (status, reason) for one recipe transcript.

    The shell prints a sign-in preamble before the REPL banner, and its own
    auto-login attempt fails there because this script signs in explicitly.
    Only lines after the banner describe the recipe.
    """
    command = body_of(recipe).splitlines()[0] if body_of(recipe) else ""
    lines = transcript.splitlines()
    banner = next((i for i, ln in enumerate(lines) if ln.startswith("Type 'help'")), -1)
    outcome = lines[banner + 1 :] if banner >= 0 else lines
    for line in outcome:
        if NOT_WIRED in line:
            return "NOT_WIRED", line.strip()[:200]
    # An abort is checked before a refusal, because the shell prints both and the
    # abort is the one that says the command did not finish.
    for line in outcome:
        if ABORTED in line:
            return "ABORTED", line.strip()[:200]
    refusals = [ln.strip() for ln in outcome if "\u2717" in ln]
    if refusals:
        return "ANSWERED_WITH_ERROR", refusals[-1][:200]
    if "Traceback" in transcript or "Segmentation fault" in transcript:
        return "CRASHED", command[:200]
    return "ANSWERED", "ok"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "groups",
        nargs="*",
        default=list(DEFAULT_GROUPS),
        help="recipe library directories to replay "
             f"(default: {' '.join(DEFAULT_GROUPS)})",
    )
    parser.add_argument(
        "--out-dir",
        default=str(DEFAULT_OUT_DIR),
        help="where the TSV and the per-recipe logs go "
             f"(default: {DEFAULT_OUT_DIR.relative_to(REPO)})",
    )
    parser.add_argument(
        "--skip",
        default="",
        help="comma-separated recipe stems to record as NOT_REPLAYED, such as a "
             "destructive command the fleet must not be asked to run",
    )
    parser.add_argument(
        "--login-command",
        default="login",
        help="the shell command that signs in, without its arguments; pass the "
             "component's own when sign-in is a hand-written command, for "
             "example 'accounts login'",
    )
    args = parser.parse_args()

    out_dir = Path(args.out_dir)
    if not out_dir.is_absolute():
        out_dir = REPO / out_dir
    out_dir = out_dir.resolve()
    log_dir = out_dir / "logs"
    tsv = out_dir / "recipes.tsv"
    skip = {s.strip() for s in args.skip.split(",") if s.strip()}

    principal = os.environ.get("V04_PRINCIPAL")
    password = os.environ.get("V04_PASSWORD")
    if not principal or not password:
        print("V04_PRINCIPAL and V04_PASSWORD must be set", file=sys.stderr)
        return 2
    if not SHELL.exists():
        print(f"shell binary not found: {SHELL}", file=sys.stderr)
        return 2

    log_dir.mkdir(parents=True, exist_ok=True)
    recipes = recipe_paths(tuple(args.groups), skip)
    rows: list[tuple[str, str, str, str]] = []

    for recipe in recipes:
        command = body_of(recipe)
        # The password is quoted: the shell's tokeniser refuses a bare token
        # that carries punctuation, and a generated credential may.
        quoted_password = "'" + password.replace("'", "'\\''") + "'"
        script = f"{args.login_command} {principal} {quoted_password}\n{command}\n"
        transcript = ""
        with tempfile.NamedTemporaryFile("w", suffix=".ores", delete=False) as handle:
            handle.write(script)
            script_path = handle.name
        try:
            completed = subprocess.run(
                [str(SHELL), "--load", script_path],
                capture_output=True,
                text=True,
                timeout=120,
                env={**os.environ, "HOME": os.environ.get("HOME", "/root")},
                cwd=str(REPO),
            )
            transcript = completed.stdout + completed.stderr
        except subprocess.TimeoutExpired:
            transcript = "\u2717 timed out after 120s"
        finally:
            os.unlink(script_path)
        status, reason = classify(recipe, transcript)
        rel = f"{PREFIX}/{recipe.parent.name}/{recipe.name}"
        (log_dir / f"{recipe.name}.log").write_text(transcript)
        rows.append((rel, command, status, reason))
        print(f"{status:20} {recipe.name:32} {reason[:90]}")

    for stem in sorted(skip):
        rows.append((f"{PREFIX}/*/{stem}.ores", "", NOT_REPLAYED,
                     "not replayed: named in --skip"))
        print(f"{NOT_REPLAYED:20} {stem:32} not replayed: named in --skip")

    with tsv.open("w") as handle:
        handle.write("recipe\tcommand\tstatus\treason\n")
        for rel, command, status, reason in rows:
            handle.write(f"{rel}\t{command}\t{status}\t{reason}\n")

    tally: dict[str, int] = {}
    for _, _, status, _ in rows:
        tally[status] = tally.get(status, 0) + 1
    print(f"\n{len(rows)} recipes: " + ", ".join(f"{k}={v}" for k, v in sorted(tally.items())))
    print(f"evidence: {tsv.relative_to(REPO)}")
    failing = ("NOT_WIRED", "ABORTED", "CRASHED")
    return 1 if any(tally.get(k) for k in failing) else 0


if __name__ == "__main__":
    raise SystemExit(main())
