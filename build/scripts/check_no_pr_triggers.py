#!/usr/bin/env python3
"""Fail when a workflow triggers on a pull request.

GitHub runs no pull-request check. The twelve that did were retired, and the
local runner — =compass check= — is the gate now. A workflow that grows the
trigger back reintroduces the wait the retirement removed, and it comes back
silently: nothing else reads the workflow files, so the first symptom is a
check appearing on somebody's pull request.

Only the plain trigger counts. =pull_request_review= and
=pull_request_review_comment= are different events, still used by the
on-demand =@claude= review, and a commented-out line is not a trigger.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = REPO_ROOT / ".github" / "workflows"

# Two spaces of indentation under "on:", the key alone on its line.
TRIGGER = re.compile(r"^  pull_request:\s*$")


def offending_lines(text: str) -> list[tuple[int, str]]:
    """The line numbers and text of every pull_request trigger in a workflow."""
    found: list[tuple[int, str]] = []
    for number, line in enumerate(text.splitlines(), start=1):
        if line.lstrip().startswith("#"):
            continue
        if TRIGGER.match(line):
            found.append((number, line.rstrip()))
    return found


def main() -> int:
    files = sorted({*WORKFLOWS.glob("*.yml"), *WORKFLOWS.glob("*.yaml")})
    if not files:
        print(f"❌ no workflow files under {WORKFLOWS}", file=sys.stderr)
        return 1

    violations: list[str] = []
    for path in files:
        for number, line in offending_lines(path.read_text()):
            violations.append(f"  {path.relative_to(REPO_ROOT)}:{number}: {line}")

    if violations:
        print(f"❌ {len(violations)} workflow trigger(s) on a pull request:")
        print("\n".join(violations))
        print(
            "\nGitHub runs no pull-request check: the local runner is the gate.\n"
            "Drop the pull_request trigger and keep push to main and dispatch."
        )
        return 1

    print(f"✅ no workflow triggers on a pull request ({len(files)} files)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
