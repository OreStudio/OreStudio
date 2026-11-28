#!/usr/bin/env python3
"""Check the shape of the CI YAML that GitHub alone would reject.

A composite action is a list of steps that run inside somebody else's job, and
three of its rules are broken silently:

- every step that runs a command must state its own shell, because the caller's
  `defaults.run.shell` does not reach into an action;
- every local `uses:` must name a directory that holds an action.yml, or a
  workflow that declares `workflow_call`;
- the job that calls a local action must check the repository out first, and
  without a condition, because a local action is read from the workspace and a
  skipped checkout leaves it missing;
- a step states only the keys a composite accepts, because the platform refuses
  the whole file over any other, which reaches the run as a job that failed
  without starting.

None of these fails the pull request, or the merge: all fail hours later, in the
nightly, on the platform that happens to take the branch. This is the check
that reads the YAML instead, written with the standard library alone so that it
runs under either interpreter the gate has.

The scan is deliberately shallow. It reads the step boundaries of the file, the
keys each step states, the ids it declares and the local paths it calls; it is
not a YAML parser, and it does not try to be one.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
ACTIONS_DIR = REPO_ROOT / ".github" / "actions"
WORKFLOWS_DIR = REPO_ROOT / ".github" / "workflows"

STEP = re.compile(r"^(?P<indent> *)- (?P<rest>.*)$")
KEY = re.compile(r"^(?P<indent> *)(?P<key>[A-Za-z_][A-Za-z0-9_-]*):(?P<value>.*)$")
LOCAL_ACTION = re.compile(r"\./\.github/actions/([A-Za-z0-9_.-]+)")
LOCAL_WORKFLOW = re.compile(r"\./\.github/workflows/([A-Za-z0-9_.-]+)")
STEP_REFERENCE = re.compile(r"steps\.([A-Za-z0-9_-]+)\.outputs")

# The keys a step of a composite action may state. The list is short because the
# platform rejects the file outright on any other, which reads as a job that
# failed without running: timeout-minutes is the one that costs a rebuild to
# learn, and the leg states its own cap with timeout(1) instead.
COMPOSITE_STEP_KEYS = frozenset({
    "name", "id", "if", "uses", "run", "shell", "env", "with",
    "working-directory",
})


def display(path: Path) -> str:
    """The path as the report names it: relative when the tree holds it."""
    try:
        return str(path.relative_to(REPO_ROOT))
    except ValueError:
        return str(path)


def steps_of(text: str) -> list[list[str]]:
    """The lines of each step, from its leading dash to the next one.

    A step starts at a dash that is indented under `steps:`; everything more
    indented belongs to it. The dash sits at the same indent for every step in
    a list, so the first dash of the file sets the depth to expect.
    """
    steps: list[list[str]] = []
    dash_indent: int | None = None
    current: list[str] = []
    for line in text.splitlines():
        match = STEP.match(line)
        if match and (dash_indent is None or len(match.group("indent")) == dash_indent):
            if current:
                steps.append(current)
            dash_indent = len(match.group("indent"))
            current = [line]
            continue
        if current and line.strip():
            current.append(line)
        elif current:
            current.append(line)
    if current:
        steps.append(current)
    return steps


def step_keys(step: list[str]) -> dict[str, str]:
    """The keys a step states directly, at the indent its dash introduced.

    A key nested under `with:` belongs to that step too, and is kept apart by
    the indent it sits at, so `with: {run: …}` cannot pass for a run step.
    """
    dash = STEP.match(step[0])
    assert dash is not None
    first = dash.group("rest").strip()
    keys: dict[str, str] = {}
    key_indent: int | None = None
    inline = KEY.match(" " * (len(dash.group("indent")) + 2) + first)
    if inline:
        keys[inline.group("key")] = inline.group("value").strip()
        key_indent = len(dash.group("indent")) + 2
    for line in step[1:]:
        match = KEY.match(line)
        if not match:
            continue
        indent = len(match.group("indent"))
        if key_indent is None and indent > len(dash.group("indent")):
            key_indent = indent
        if key_indent is not None and indent == key_indent:
            keys[match.group("key")] = match.group("value").strip()
    return keys


def check_action(path: Path) -> list[str]:
    """Every rule a composite action's own file must satisfy."""
    findings: list[str] = []
    text = path.read_text()
    relative = display(path)

    if "using: composite" not in text:
        findings.append(
            f"{relative}: runs.using is not composite, so the file is not an"
            " action the workflows can call")

    steps = steps_of(text)
    if not steps:
        findings.append(f"{relative}: no steps")

    ids = set()
    for number, step in enumerate(steps, start=1):
        keys = step_keys(step)
        if "run" in keys and "shell" not in keys:
            findings.append(
                f"{relative}: step {number} runs a command but states no shell;"
                " the caller's defaults do not reach into an action")
        unknown = set(keys) - COMPOSITE_STEP_KEYS
        if unknown:
            findings.append(
                f"{relative}: step {number} states {sorted(unknown)}, which a"
                " composite action does not accept")
        if "id" in keys:
            ids.add(keys["id"])
        uses = keys.get("uses", "")
        action = LOCAL_ACTION.search(uses)
        if action:
            target = ACTIONS_DIR / action.group(1) / "action.yml"
            if not target.is_file():
                findings.append(
                    f"{relative}: step {number} calls {uses}, which is not an"
                    " action in this repository")
        workflow = LOCAL_WORKFLOW.search(uses)
        if workflow and not (WORKFLOWS_DIR / workflow.group(1)).is_file():
            findings.append(
                f"{relative}: step {number} calls {uses}, which does not exist")

    for referenced in sorted(set(STEP_REFERENCE.findall(text))):
        if referenced not in ids:
            findings.append(
                f"{relative}: an output reads steps.{referenced}, which is not"
                " an id any step in this file declares")
    return findings


def job_blocks(text: str) -> list[tuple[str, str]]:
    """Each job of a workflow, as its name and its lines."""
    jobs: list[tuple[str, list[str]]] = []
    for line in text.splitlines():
        match = re.match(r"^  ([A-Za-z0-9_-]+):\s*$", line)
        if match:
            jobs.append((match.group(1), [line]))
        elif jobs:
            jobs[-1][1].append(line)
    return [(name, "\n".join(lines)) for name, lines in jobs]


def check_workflow(path: Path) -> list[str]:
    """The local paths a workflow calls, and the workflow it calls as a job."""
    findings: list[str] = []
    text = path.read_text()
    relative = display(path)

    for action in sorted(set(LOCAL_ACTION.findall(text))):
        target = ACTIONS_DIR / action / "action.yml"
        if not target.is_file():
            findings.append(
                f"{relative}: calls ./.github/actions/{action}, which is not an"
                " action in this repository")

    for workflow in sorted(set(LOCAL_WORKFLOW.findall(text))):
        target = WORKFLOWS_DIR / workflow
        if not target.is_file():
            findings.append(
                f"{relative}: calls ./.github/workflows/{workflow}, which does"
                " not exist")
        elif "workflow_call" not in target.read_text():
            findings.append(
                f"{relative}: calls ./.github/workflows/{workflow}, which does"
                " not declare workflow_call")

    for name, body in job_blocks(text):
        if "./.github/actions/" not in body:
            continue
        checkouts = [step for step in steps_of(body)
                     if "actions/checkout@" in "".join(step)]
        if not checkouts:
            findings.append(
                f"{relative}: job {name} calls a local action but never checks"
                " the repository out, so the action cannot be read")
            continue
        for step in checkouts:
            if "if" in step_keys(step):
                findings.append(
                    f"{relative}: job {name} checks out under a condition, and"
                    " a skipped checkout leaves a local action unreadable")
    return findings


def main() -> int:
    findings: list[str] = []
    for path in sorted(ACTIONS_DIR.glob("*/action.yml")):
        findings.extend(check_action(path))
    for path in sorted(WORKFLOWS_DIR.glob("*.yml")):
        findings.extend(check_workflow(path))

    for finding in findings:
        print(finding, file=sys.stderr)
    if findings:
        print(f"\n{len(findings)} problem(s) in the CI YAML.", file=sys.stderr)
        return 1
    print("The CI YAML names only what exists, and every command states its shell.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
