"""
Tests for the skill-selection PreToolUse hook.

Run with:  python -m pytest projects/ores.compass/tests/test_record_skill_selection_hook.py -v
No live database required.
"""

import io
import json
import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_skills
import record_skill_selection_hook as hook


def run(payload, monkeypatch, tmp_path):
    monkeypatch.setattr(compass_skills, "log_path",
                        lambda _root: tmp_path / "skill_selections.jsonl")
    monkeypatch.setattr(sys, "stdin", io.StringIO(json.dumps(payload)))
    return hook.main()


def events(tmp_path):
    path = tmp_path / "skill_selections.jsonl"
    if not path.exists():
        return []
    return [json.loads(line) for line in path.read_text().splitlines() if line]


def test_skill_call_is_recorded(monkeypatch, tmp_path):
    assert run({"tool_name": "Skill", "session_id": "s1", "cwd": str(tmp_path),
                "tool_input": {"skill": "compass-pr-merge"}}, monkeypatch, tmp_path) == 0
    recorded = events(tmp_path)
    assert len(recorded) == 1
    assert recorded[0]["kind"] == "selection"
    assert recorded[0]["skill"] == "compass-pr-merge"
    assert recorded[0]["session"] == "s1"


def test_other_tools_are_ignored(monkeypatch, tmp_path):
    assert run({"tool_name": "Bash", "tool_input": {"command": "ls"}},
               monkeypatch, tmp_path) == 0
    assert events(tmp_path) == []


def test_malformed_payload_does_not_crash(monkeypatch, tmp_path):
    monkeypatch.setattr(sys, "stdin", io.StringIO("not json"))
    assert hook.main() == 0


def test_recording_failure_never_blocks_the_tool(monkeypatch, tmp_path):
    def explode(*_args, **_kwargs):
        raise OSError("log is unwritable")
    monkeypatch.setattr(compass_skills, "record_selection", explode)
    monkeypatch.setattr(sys, "stdin", io.StringIO(json.dumps(
        {"tool_name": "Skill", "cwd": str(tmp_path),
         "tool_input": {"skill": "compass-pr-merge"}})))
    assert hook.main() == 0
