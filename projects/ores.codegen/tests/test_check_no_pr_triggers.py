"""Tests for build/scripts/check_no_pr_triggers.py.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_no_pr_triggers.py

GitHub runs no pull-request check, and the only thing keeping that true is
that no workflow declares the trigger. The guard reads the workflow files, so
these cases are the contract: the plain trigger fails, and the review events
the @claude bot still uses do not.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "check_no_pr_triggers",
    REPO_ROOT / "build" / "scripts" / "check_no_pr_triggers.py")
check_no_pr_triggers = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = check_no_pr_triggers
SPEC.loader.exec_module(check_no_pr_triggers)


def test_the_plain_trigger_is_found():
    text = "on:\n  push:\n    branches: [main]\n  pull_request:\n    branches: [main]\n"
    assert [line for _, line in check_no_pr_triggers.offending_lines(text)] == [
        "  pull_request:"
    ]


def test_a_commented_out_trigger_is_not_found():
    text = "on:\n  # pull_request:\n  #   branches: [main]\n  workflow_dispatch:\n"
    assert check_no_pr_triggers.offending_lines(text) == []


def test_the_review_events_are_not_the_trigger():
    text = (
        "on:\n"
        "  pull_request_review:\n"
        "    types: [submitted]\n"
        "  pull_request_review_comment:\n"
        "    types: [created]\n"
    )
    assert check_no_pr_triggers.offending_lines(text) == []


def test_a_deeper_key_is_not_the_trigger():
    # A job- or step-level key that merely mentions the name must not fire.
    text = "jobs:\n  x:\n      pull_request:\n"
    assert check_no_pr_triggers.offending_lines(text) == []


def test_the_committed_workflows_declare_no_pull_request_trigger():
    files = sorted({
        *check_no_pr_triggers.WORKFLOWS.glob("*.yml"),
        *check_no_pr_triggers.WORKFLOWS.glob("*.yaml"),
    })
    assert files, "no workflow files found"
    offenders = []
    for path in files:
        for number, line in check_no_pr_triggers.offending_lines(path.read_text()):
            offenders.append(f"{path.name}:{number}: {line}")
    assert not offenders, (
        "a workflow triggers on a pull request; GitHub runs no such check: "
        + ", ".join(offenders)
    )


def test_the_command_succeeds_on_this_tree(capsys):
    assert check_no_pr_triggers.main() == 0
    assert "no workflow triggers on a pull request" in capsys.readouterr().out
