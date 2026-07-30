"""
Tests for the unscoped-find PreToolUse hook.

Run with:  python -m pytest projects/ores.compass/tests/test_deny_unscoped_find_hook.py -v
No live database or file system access required.
"""

import io
import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import deny_unscoped_find_hook as hook


def run(command, tool_name="Bash", monkeypatch=None):
    """Drive hook.main() with a synthetic PreToolUse payload on stdin,
    returning (exit_code, stderr_text)."""
    payload = {"tool_name": tool_name, "tool_input": {"command": command}}
    import json
    monkeypatch.setattr(sys, "stdin", io.StringIO(json.dumps(payload)))
    stderr = io.StringIO()
    monkeypatch.setattr(sys, "stderr", stderr)
    return hook.main(), stderr.getvalue()


def test_find_root_denied(monkeypatch):
    code, msg = run('find / -iname "aonia.hpp"', monkeypatch=monkeypatch)
    assert code == 2
    assert "Unscoped `find`" in msg


def test_find_home_denied(monkeypatch):
    code, _ = run("find /home -iname '*.hpp'", monkeypatch=monkeypatch)
    assert code == 2


def test_find_relative_path_allowed(monkeypatch):
    code, _ = run("find . -iname foo.hpp", monkeypatch=monkeypatch)
    assert code == 0


def test_find_project_relative_allowed(monkeypatch):
    code, _ = run("find projects/ores.sql -iname '*.sql'",
                   monkeypatch=monkeypatch)
    assert code == 0


def test_find_narrower_absolute_path_allowed(monkeypatch):
    code, _ = run("find /home/marco/Development/ORE/Engine -iname '*.hpp'",
                   monkeypatch=monkeypatch)
    assert code == 0


def test_find_with_flags_before_path_denied(monkeypatch):
    code, _ = run("find -L / -iname foo.hpp", monkeypatch=monkeypatch)
    assert code == 2


def test_find_after_chain_operator_denied(monkeypatch):
    code, _ = run("cd /tmp && find / -iname foo.hpp", monkeypatch=monkeypatch)
    assert code == 2


def test_find_as_prose_word_in_unrelated_command_allowed(monkeypatch):
    """"find" as an ordinary English word inside an unrelated command's
    argument (e.g. a --description string) must not be mistaken for a
    find invocation just because it's preceded by whitespace."""
    code, _ = run(
        'compass add task --description "surfaced when an unscoped '
        'find / scanned the whole machine"',
        monkeypatch=monkeypatch)
    assert code == 0


def test_unrelated_command_allowed(monkeypatch):
    code, _ = run("git log --oneline -5", monkeypatch=monkeypatch)
    assert code == 0


def test_non_bash_tool_allowed(monkeypatch):
    code, _ = run("find / -iname foo.hpp", tool_name="Read",
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
