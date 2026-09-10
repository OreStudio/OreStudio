"""
Tests for the skill-selection instrument.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_skills.py -v
No live database required.
"""

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_skills as skills


def use_log(monkeypatch, tmp_path):
    log = tmp_path / "skill_selections.jsonl"
    monkeypatch.setattr(skills, "log_path", lambda _root: log)
    return log


def test_selection_records_the_attenuation_level(monkeypatch, tmp_path):
    use_log(monkeypatch, tmp_path)
    monkeypatch.setenv("ORES_SKILL_LEVEL", "s1")

    event = skills.record_selection(tmp_path, "pr-merge", "s-1")

    assert event["level"] == "s1"
    assert event["skill"] == "pr-merge"
    assert isinstance(event["offered"], int)


def test_selection_without_a_level_is_unattenuated(monkeypatch, tmp_path):
    use_log(monkeypatch, tmp_path)
    monkeypatch.delenv("ORES_SKILL_LEVEL", raising=False)

    assert skills.record_selection(tmp_path, "pr-merge", "s-1")["level"] is None


def test_correct_marks_the_most_recent_selection(monkeypatch, tmp_path):
    use_log(monkeypatch, tmp_path)
    monkeypatch.delenv("ORES_SKILL_LEVEL", raising=False)
    skills.record_selection(tmp_path, "pr-raise", "s-1")
    second = skills.record_selection(tmp_path, "pr-merge", "s-1")

    assert skills.cmd_correct(["--cause", "description"], tmp_path) == 0

    corrections = [e for e in skills.read_events(tmp_path)
                   if e["kind"] == "correction"]
    assert len(corrections) == 1
    assert corrections[0]["selection"] == second["id"]
    assert corrections[0]["cause"] == "description"


def test_correct_refuses_an_empty_log(monkeypatch, tmp_path):
    use_log(monkeypatch, tmp_path)
    assert skills.cmd_correct(["--cause", "level"], tmp_path) == 1


def test_summary_splits_the_correction_rate_by_level(monkeypatch, tmp_path):
    use_log(monkeypatch, tmp_path)
    monkeypatch.delenv("ORES_SKILL_LEVEL", raising=False)
    plain = skills.record_selection(tmp_path, "pr-raise", "s-1")
    monkeypatch.setenv("ORES_SKILL_LEVEL", "s1")
    skills.record_selection(tmp_path, "pr-merge", "s-2")
    skills.cmd_correct(["--cause", "catalogue-size",
                        "--selection", plain["id"]], tmp_path)

    s = skills.summarise(skills.read_events(tmp_path))

    assert s["by_level"] == {"unattenuated": 1, "s1": 1}
    assert s["corrected_by_level"]["unattenuated"] == 1
    assert s["corrected_by_level"]["s1"] == 0
    assert s["causes"]["catalogue-size"] == 1


def test_two_corrections_on_one_selection_count_once(monkeypatch, tmp_path):
    use_log(monkeypatch, tmp_path)
    monkeypatch.delenv("ORES_SKILL_LEVEL", raising=False)
    skills.record_selection(tmp_path, "pr-raise", "s-1")
    skills.cmd_correct(["--cause", "description"], tmp_path)
    skills.cmd_correct(["--cause", "level"], tmp_path)

    s = skills.summarise(skills.read_events(tmp_path))

    assert len(s["corrections"]) == 2
    assert s["corrected"] == 1


def test_window_excludes_older_events(monkeypatch, tmp_path):
    events = [{"kind": "selection", "id": "old", "ts": "2020-01-01T00:00:00Z"},
              {"kind": "selection", "id": "new", "ts": skills.now_iso()}]

    kept = skills.within_window(events, skills.parse_duration("1d"))

    assert [e["id"] for e in kept] == ["new"]


def test_unreadable_log_reads_as_empty(monkeypatch, tmp_path):
    use_log(monkeypatch, tmp_path)
    assert skills.read_events(tmp_path) == []


def test_malformed_lines_are_skipped(monkeypatch, tmp_path):
    log = use_log(monkeypatch, tmp_path)
    log.write_text('{"kind": "selection", "id": "a", "ts": "x"}\nnot json\n')

    assert [e["id"] for e in skills.read_events(tmp_path)] == ["a"]


def test_report_runs_on_an_empty_log(monkeypatch, tmp_path, capsys):
    use_log(monkeypatch, tmp_path)

    assert skills.cmd_report([], tmp_path) == 0
    assert "No selections recorded yet" in capsys.readouterr().out


def test_report_rejects_an_unparsable_window(monkeypatch, tmp_path):
    use_log(monkeypatch, tmp_path)
    assert skills.cmd_report(["--since", "yesterday"], tmp_path) == 1


def test_log_is_shared_across_worktrees(tmp_path):
    """The log lives in the git common dir, so every worktree appends to one
    file rather than restarting the baseline per checkout."""
    path = skills.log_path(Path(__file__).resolve().parent)
    assert path.name == skills.LOG_NAME
    assert path.parent.name.endswith(".git")
