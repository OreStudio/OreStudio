"""compass task move must remove the * Tasks row, not any row that links the task.

A story's Status Next cell may name the task it hands over to, with an org
link. The removal used to scan the whole file for a table row holding that
link and take the first one, so the Next cell was extracted instead of the
Tasks row: the source story kept the row, the target story gained a
two-cell Status fragment in its Tasks table, and the source lost its Next
cell. The scan is now scoped to the * Tasks table.

Run with:  python -m pytest projects/ores.compass/tests/test_task_move_row_scope.py -v
No live database required.
"""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SRC = ROOT / "projects" / "ores.compass" / "src"
sys.path.insert(0, str(SRC))

import compass  # noqa: E402

TASK = "AAAA0000-0000-0000-0000-000000000001"
OTHER = "BBBB0000-0000-0000-0000-000000000002"

STORY = f"""#+title: Story: T

* Status

| Field        | Value |
|--------------+-------|
| Next         | [[id:{TASK}][Do a thing]], prototyped first. |

* Tasks

| Task | State | Start | End | Description |
|------+-------+-------+-----+-------------|
| [[id:{TASK}][Do a thing]] | BACKLOG | | | Do the thing. |
| [[id:{OTHER}][Other]] | DONE | | | Other. |
"""


def test_removes_the_tasks_row_and_keeps_a_status_reference(tmp_path):
    story = tmp_path / "story.org"
    story.write_text(STORY, encoding="utf-8")

    row = compass._remove_task_row_from_story(story, TASK)

    assert row is not None, "the Tasks row was not found"
    assert "| BACKLOG |" in row, f"a Status fragment was extracted: {row}"
    text = story.read_text(encoding="utf-8")
    assert f"| Next         | [[id:{TASK}][Do a thing]]" in text, (
        "the Status Next cell was removed")
    assert "| BACKLOG |" not in text, "the Tasks row was left behind"
    assert OTHER in text, "the sibling row was removed"


def test_restores_the_placeholder_when_the_last_row_is_removed(tmp_path):
    story = tmp_path / "story.org"
    lone = STORY.replace(
        f"| [[id:{OTHER}][Other]] | DONE | | | Other. |\n", "")
    assert OTHER not in lone, "the sibling row was not dropped from the fixture"
    story.write_text(lone, encoding="utf-8")

    row = compass._remove_task_row_from_story(story, TASK)

    assert row is not None, "the Tasks row was not found"
    text = story.read_text(encoding="utf-8")
    assert f"| Next         | [[id:{TASK}][Do a thing]]" in text, (
        "the Status Next cell was removed")
    assert text.count("[[id:") == 1, "the task row was left behind"
    assert "|   |   |   |   |   |" in text, (
        "no empty placeholder row was restored")


def test_a_lookalike_heading_is_not_the_tasks_table(tmp_path):
    story = tmp_path / "story.org"
    lookalike = STORY.replace("* Tasks\n", "* Tasks: notes so far\n", 1)
    assert lookalike != STORY, "the fixture heading was not replaced"
    story.write_text(lookalike, encoding="utf-8")

    row = compass._remove_task_row_from_story(story, TASK)

    assert row is None, f"a row was extracted outside a Tasks table: {row}"
    assert story.read_text(encoding="utf-8") == lookalike, (
        "the story was modified")
