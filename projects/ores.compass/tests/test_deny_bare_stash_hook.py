"""
Tests for the bare-stash PreToolUse hook.

Run with:  python -m pytest projects/ores.compass/tests/test_deny_bare_stash_hook.py -v
No live database required.
"""

import io
import json
import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import deny_bare_stash_hook as hook


def run(command, monkeypatch, tool_name="Bash"):
    payload = {"tool_name": tool_name, "tool_input": {"command": command}}
    monkeypatch.setattr(sys, "stdin", io.StringIO(json.dumps(payload)))
    return hook.main()


def test_bare_stash_is_denied(monkeypatch):
    assert run("git stash", monkeypatch) == 2


def test_bare_pop_is_denied(monkeypatch):
    assert run("git stash pop", monkeypatch) == 2


def test_pop_with_a_ref_is_still_denied(monkeypatch):
    """A ref does not help: the entry it names may be another session's."""
    assert run("git stash pop stash@{0}", monkeypatch) == 2


def test_save_without_a_message_is_denied(monkeypatch):
    assert run("git stash save", monkeypatch) == 2


def test_tagged_push_is_allowed(monkeypatch):
    assert run('git stash push -u -m "wip-bright-faraday"', monkeypatch) == 0


def test_apply_against_an_explicit_entry_is_allowed(monkeypatch):
    assert run("git stash apply 8f2a1c3", monkeypatch) == 0


def test_reads_are_allowed(monkeypatch):
    assert run("git stash list", monkeypatch) == 0
    assert run("git stash show -p", monkeypatch) == 0


def test_drop_is_allowed(monkeypatch):
    assert run("git stash drop stash@{2}", monkeypatch) == 0


def test_denial_survives_a_compound_command(monkeypatch):
    assert run("cd /tmp && git stash", monkeypatch) == 2


def test_unrelated_commands_pass(monkeypatch):
    assert run("git status", monkeypatch) == 0


def test_the_words_inside_a_message_are_not_a_command(monkeypatch):
    """The first pattern denied this, which blocked writing about the
    rule while explaining it."""
    assert run("echo git stash is shared", monkeypatch) == 0


def test_push_with_flags_but_no_message_is_denied(monkeypatch):
    assert run("git stash push -u", monkeypatch) == 2


def test_other_tools_are_ignored(monkeypatch):
    assert run("git stash", monkeypatch, tool_name="Read") == 0


def test_malformed_payload_does_not_crash(monkeypatch):
    monkeypatch.setattr(sys, "stdin", io.StringIO("not json"))
    assert hook.main() == 0
