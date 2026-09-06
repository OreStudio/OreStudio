"""
Tests for the compass pr create body template.

Run with:  python -m pytest projects/ores.compass/tests/test_compass_pr.py -v
No live database or GitHub access required; the refusal paths are
exercised before any subprocess runs.
"""

import argparse
import sys
from pathlib import Path

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass_pr  # noqa: E402


def _body(**overrides):
    args = dict(
        summary="The summary",
        change_bullets=["First change", "Second change"],
        story_title="Story title",
        story_id="11111111-1111-1111-1111-111111111111",
        story_url="https://example.test/story.html",
        task_title="Task title",
        task_id="22222222-2222-2222-2222-222222222222",
        task_url="https://example.test/task.html",
        environment="brave_hopper",
        testing_plan="plan prose",
        testing_evidence="evidence prose",
        testing_limitations="limitations prose",
    )
    args.update(overrides)
    return compass_pr._canonical_body(**args)


def test_canonical_body_section_order():
    body = _body()
    marks = ["## Summary", "## Changes", "## Traceability",
             "| Environment", "## Testing", "Limitations.",
             "🤖 Generated with [Claude Code]"]
    positions = [body.index(m) for m in marks]
    assert positions == sorted(positions), marks
    assert body.startswith("## Summary\n\nThe summary\n")
    assert body.endswith("🤖 Generated with [Claude Code]"
                         "(https://claude.com/claude-code)")


def test_canonical_body_changes_bullets():
    body = _body()
    assert "- First change\n- Second change\n" in body


def test_canonical_body_testing_paragraphs():
    body = _body()
    assert "## Testing\n\n" in body
    assert "Plan. plan prose" in body
    assert "Evidence. evidence prose" in body
    assert "Limitations. limitations prose" in body


def test_canonical_body_traceability_rows():
    body = _body()
    assert "| Story | [Story title](https://example.test/story.html) | " \
           "11111111-1111-1111-1111-111111111111 |" in body
    assert "| Task | [Task title](https://example.test/task.html) | " \
           "22222222-2222-2222-2222-222222222222 |" in body
    assert "| Environment | brave_hopper | |" in body


def _run_create(argv):
    return compass_pr.run(["create", *argv], Path("."))


def test_create_refuses_missing_testing_flags(monkeypatch, capsys):
    monkeypatch.setattr(compass_pr, "_current_branch",
                        lambda project_root: "feature/something")
    rc = _run_create(["--title", "[compass] x", "--summary", "s"])
    assert rc == 1
    err = capsys.readouterr().err
    assert "--testing-plan" in err
    assert "--testing-evidence" in err
    assert "--testing-limitations" in err


def test_create_refuses_naming_only_missing_flags(monkeypatch, capsys):
    monkeypatch.setattr(compass_pr, "_current_branch",
                        lambda project_root: "feature/something")
    rc = _run_create(["--title", "[compass] x",
                      "--testing-plan", "p", "--testing-evidence", "e"])
    assert rc == 1
    err = capsys.readouterr().err
    assert "--testing-limitations" in err
    assert "--testing-plan" not in err
    assert "--testing-evidence" not in err


def test_create_refuses_blank_testing_flags(monkeypatch, capsys):
    monkeypatch.setattr(compass_pr, "_current_branch",
                        lambda project_root: "feature/something")
    rc = _run_create(["--title", "[compass] x",
                      "--testing-plan", " ", "--testing-evidence", "  ",
                      "--testing-limitations", "l"])
    assert rc == 1
    err = capsys.readouterr().err
    assert "--testing-plan" in err
    assert "--testing-evidence" in err
    assert "--testing-limitations" not in err


def test_create_refuses_missing_title(monkeypatch, capsys):
    import pytest

    monkeypatch.setattr(compass_pr, "_current_branch",
                        lambda project_root: "feature/something")
    with pytest.raises(SystemExit) as exc:
        _run_create(["--summary", "s"])
    assert exc.value.code == 2
    err = capsys.readouterr().err
    assert "--title" in err


def test_missing_testing_flags_reports_each_blank_field():
    ns = argparse.Namespace(testing_plan="", testing_evidence=" ",
                            testing_limitations="\t")
    assert compass_pr._missing_testing_flags(ns) == [
        "--testing-plan", "--testing-evidence", "--testing-limitations"]


def test_missing_testing_flags_empty_when_all_present():
    ns = argparse.Namespace(testing_plan="p", testing_evidence="e",
                            testing_limitations="l")
    assert compass_pr._missing_testing_flags(ns) == []
