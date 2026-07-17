"""
Tests for the piped/redirected-compass PreToolUse hook.

Run with:  python -m pytest projects/ores.compass/tests/test_deny_piped_compass_hook.py -v
No live database or file system access required.
"""

import io
import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import deny_piped_compass_hook as hook


def run(command, tool_name="Bash", monkeypatch=None):
    """Drive hook.main() with a synthetic PreToolUse payload on stdin,
    returning (exit_code, stderr_text)."""
    payload = {"tool_name": tool_name, "tool_input": {"command": command}}
    import json
    monkeypatch.setattr(sys, "stdin", io.StringIO(json.dumps(payload)))
    stderr = io.StringIO()
    monkeypatch.setattr(sys, "stderr", stderr)
    return hook.main(), stderr.getvalue()


def test_bare_compass_allowed(monkeypatch):
    code, _ = run("./compass.sh build --direct site", monkeypatch=monkeypatch)
    assert code == 0


def test_piped_compass_denied(monkeypatch):
    code, msg = run("./compass.sh build --direct site 2>&1 | tail -30",
                     monkeypatch=monkeypatch)
    assert code == 2
    assert "Never pipe or redirect" in msg


def test_redirected_compass_denied(monkeypatch):
    code, _ = run("./compass.sh build > out.log", monkeypatch=monkeypatch)
    assert code == 2


def test_logical_or_compass_allowed(monkeypatch):
    """`||` is error handling, not a pipe -- must not be denied."""
    code, _ = run("./compass.sh build --direct site || echo failed",
                   monkeypatch=monkeypatch)
    assert code == 0


def test_all_four_invocation_forms_denied_when_piped(monkeypatch):
    for prefix in (
        "./compass.sh",
        "compass.sh",
        "./projects/ores.compass/compass.sh",
        "projects/ores.compass/compass.sh",
    ):
        code, _ = run(f"{prefix} build | tail", monkeypatch=monkeypatch)
        assert code == 2, prefix


def test_unrelated_pipe_allowed(monkeypatch):
    code, _ = run("git log --oneline | head -5", monkeypatch=monkeypatch)
    assert code == 0


def test_unrelated_logical_or_allowed(monkeypatch):
    code, _ = run("git fetch || echo failed", monkeypatch=monkeypatch)
    assert code == 0


def test_non_bash_tool_allowed(monkeypatch):
    code, _ = run("./compass.sh build | tail", tool_name="Read",
                  monkeypatch=monkeypatch)
    assert code == 0


def test_non_dict_json_does_not_crash(monkeypatch):
    monkeypatch.setattr(sys, "stdin", io.StringIO("[1, 2, 3]"))
    assert hook.main() == 0


def test_null_json_does_not_crash(monkeypatch):
    monkeypatch.setattr(sys, "stdin", io.StringIO("null"))
    assert hook.main() == 0


def test_malformed_json_does_not_crash(monkeypatch):
    monkeypatch.setattr(sys, "stdin", io.StringIO("not json"))
    assert hook.main() == 0
