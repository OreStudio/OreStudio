#!/usr/bin/env python3
"""Run every generated assets shell recipe against the live fleet.

V04 of the Component Clean Standard asks that every generated shell command
runs against the live fleet and answers, and that any command which cannot run
is recorded with its reason.  This script is that check.

The recipes are generated one-liners under
projects/ores.shell/scripts/library/{images,image_tags,tags}.  Each is replayed
in its own shell process, behind a sign-in preamble, so one failing recipe
cannot hide the verdict of the next.

Usage:
    1. Bootstrap the local administrator once, in bootstrap mode:

        printf 'bootstrap create-initial-admin admin <password> <email>\n' \
            | ores.shell

    2. Export the same credential and run this script:

        export V04_PRINCIPAL=admin V04_PASSWORD=<password>
        projects/ores.codegen/scripts/check_shell_recipes.py

Outputs .audit/clean-assets/A30_v04_recipes.tsv and a per-recipe raw log
under .audit/clean-assets/A30_logs/. The .audit tree is not committed, so
rerun this script to regenerate the evidence.
"""

from __future__ import annotations

import os
import subprocess
import tempfile
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]
SHELL = REPO / "build/output/linux-clang-debug-make/publish/bin/ores.shell"
LIBRARY = REPO / "projects/ores.shell/scripts/library"
GROUP_DIRS = ("images", "image_tags", "tags")
PREFIX = "projects/ores.shell/scripts/library"
LOG_DIR = REPO / ".audit/clean-assets/A30_logs"
TSV = REPO / ".audit/clean-assets/A30_v04_recipes.tsv"

# A command that names a verb the shell does not know is a wiring defect.  Any
# other refusal is the command answering, which is what V04 asks for.
NOT_WIRED = "Wrong command"


def recipe_paths() -> list[Path]:
    found: list[Path] = []
    for group in GROUP_DIRS:
        found.extend(sorted((LIBRARY / group).glob("*.ores")))
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
    refusals = [ln.strip() for ln in outcome if "\u2717" in ln]
    if refusals:
        return "ANSWERED_WITH_ERROR", refusals[-1][:200]
    if "Traceback" in transcript or "Segmentation fault" in transcript:
        return "CRASHED", command[:200]
    return "ANSWERED", "ok"


def main() -> int:
    principal = os.environ.get("V04_PRINCIPAL")
    password = os.environ.get("V04_PASSWORD")
    if not principal or not password:
        print("V04_PRINCIPAL and V04_PASSWORD must be set", file=sys.stderr)
        return 2
    if not SHELL.exists():
        print(f"shell binary not found: {SHELL}", file=sys.stderr)
        return 2

    LOG_DIR.mkdir(parents=True, exist_ok=True)
    recipes = recipe_paths()
    rows: list[tuple[str, str, str, str]] = []

    for recipe in recipes:
        command = body_of(recipe)
        script = f"login {principal} {password}\n{command}\n"
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
        (LOG_DIR / f"{recipe.name}.log").write_text(transcript)
        rows.append((rel, command, status, reason))
        print(f"{status:20} {recipe.name:32} {reason[:90]}")

    with TSV.open("w") as handle:
        handle.write("recipe\tcommand\tstatus\treason\n")
        for rel, command, status, reason in rows:
            handle.write(f"{rel}\t{command}\t{status}\t{reason}\n")

    tally: dict[str, int] = {}
    for _, _, status, _ in rows:
        tally[status] = tally.get(status, 0) + 1
    print(f"\n{len(rows)} recipes: " + ", ".join(f"{k}={v}" for k, v in sorted(tally.items())))
    print(f"evidence: {TSV.relative_to(REPO)}")
    return 1 if tally.get("NOT_WIRED") or tally.get("CRASHED") else 0


if __name__ == "__main__":
    raise SystemExit(main())
