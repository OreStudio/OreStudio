"""Tests for build/scripts/check_ci_actions.py.

Run::

    python3 -m pytest build/scripts/tests/test_check_ci_actions.py

The check protects two rules that the platform enforces silently and late: a
composite step must state its own shell, and a local call must name something
that exists. These cases pin the two findings, and pin that the repository as it
stands passes them.
"""
import importlib.util
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "check_ci_actions",
    REPO_ROOT / "build" / "scripts" / "check_ci_actions.py")
check_ci_actions = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = check_ci_actions
SPEC.loader.exec_module(check_ci_actions)


def write_action(tmp_path: Path, body: str) -> Path:
    path = tmp_path / "action.yml"
    path.write_text(body)
    return path


PREAMBLE = """---
name: An action
description: An action the tests make up.

runs:
  using: composite
  steps:
"""


def test_a_run_step_without_a_shell_is_a_finding(tmp_path):
    path = write_action(tmp_path, PREAMBLE + """    - name: Build
      run: |
        echo hello
""")

    findings = check_ci_actions.check_action(path)

    assert len(findings) == 1
    assert "states no shell" in findings[0]


def test_a_run_step_with_a_shell_passes(tmp_path):
    path = write_action(tmp_path, PREAMBLE + """    - name: Build
      shell: bash
      run: |
        echo hello
""")

    assert check_ci_actions.check_action(path) == []


def test_a_uses_step_needs_no_shell(tmp_path):
    path = write_action(tmp_path, PREAMBLE + """    - name: Checkout
      uses: actions/checkout@v7
      with:
        fetch-depth: 0
""")

    assert check_ci_actions.check_action(path) == []


def test_a_local_call_that_names_no_action_is_a_finding(tmp_path):
    path = write_action(tmp_path, PREAMBLE + """    - uses: ./.github/actions/no-such-action
""")

    findings = check_ci_actions.check_action(path)

    assert len(findings) == 1
    assert "no-such-action" in findings[0]


def test_a_key_nested_under_with_is_not_the_step_key(tmp_path):
    path = write_action(tmp_path, PREAMBLE + """    - name: Build
      shell: bash
      with:
        run: not a command
""")

    assert check_ci_actions.check_action(path) == []


def test_an_output_that_reads_an_undeclared_step_is_a_finding(tmp_path):
    path = write_action(tmp_path, """---
name: An action
description: An action the tests make up.
outputs:
  answer:
    description: The answer.
    value: ${{ steps.nowhere.outputs.answer }}

runs:
  using: composite
  steps:
    - name: Answer
      id: answer
      shell: bash
      run: echo "answer=42" >> "$GITHUB_OUTPUT"
""")

    findings = check_ci_actions.check_action(path)

    assert len(findings) == 1
    assert "steps.nowhere" in findings[0]


def test_an_output_that_reads_a_declared_step_passes(tmp_path):
    path = write_action(tmp_path, """---
name: An action
description: An action the tests make up.
outputs:
  answer:
    description: The answer.
    value: ${{ steps.answer.outputs.answer }}

runs:
  using: composite
  steps:
    - name: Answer
      id: answer
      shell: bash
      run: echo "answer=42" >> "$GITHUB_OUTPUT"
""")

    assert check_ci_actions.check_action(path) == []


def test_a_workflow_that_calls_a_missing_workflow_is_a_finding(tmp_path):
    path = tmp_path / "caller.yml"
    path.write_text("""---
jobs:
  attach:
    uses: ./.github/workflows/no-such-workflow.yml
""")

    findings = check_ci_actions.check_workflow(path)

    assert len(findings) == 1
    assert "no-such-workflow" in findings[0]


def test_the_repository_actions_and_workflows_pass():
    findings = []
    for path in sorted(check_ci_actions.ACTIONS_DIR.glob("*/action.yml")):
        findings.extend(check_ci_actions.check_action(path))
    for path in sorted(check_ci_actions.WORKFLOWS_DIR.glob("*.yml")):
        findings.extend(check_ci_actions.check_workflow(path))

    assert findings == []


def test_every_composite_step_states_its_shell():
    composites = list(check_ci_actions.ACTIONS_DIR.glob("*/action.yml"))
    assert composites, "the tree has no composite actions to check"

    for path in composites:
        for step in check_ci_actions.steps_of(path.read_text()):
            keys = check_ci_actions.step_keys(step)
            if "run" in keys:
                assert "shell" in keys, f"{path.name} step {step[0]!r}"
