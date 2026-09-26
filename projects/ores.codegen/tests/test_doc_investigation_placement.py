"""Tests for scaffolding an investigation beside the story that commissioned it.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_doc_investigation_placement.py

An investigation answers a question one story asked, so it is that story's
record: a flat file in the story's folder, at level s1, carrying the story's
ancestor tags. Nothing enforced any of that. The type scaffolded as a bare
``<slug>.org`` at ``level: cross`` with no ancestor tags, which is the shape of
a cross-cutting document rather than a story artifact, and the only reason the
three in the tree looked right is that they were renamed and retagged by hand.

The doc generator had no automated test at all before this one, so these cases
call its real entry point rather than a helper. The type travels as
``--type``; the ``compass add <type>`` spelling is translated upstream.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import doc_generate  # noqa: E402

STORY = """\
:PROPERTIES:
:ID: 11111111-2222-3333-4444-555555555555
:END:
#+title: Story: Probe story
#+type: story
"""


def _story_dir(tmp_path):
    """A minimal version/sprint/story tree, so ancestor tags resolve."""
    story = tmp_path / "v0" / "sprint_99" / "probe_story"
    story.mkdir(parents=True)
    (story / "story.org").write_text(STORY, encoding="utf-8")
    return story


def _scaffold(story, slug):
    """Scaffold one investigation and return the file it wrote.

    The file is discovered rather than predicted: the point of the prefix
    cases is what the generator names it, so the test must not restate the
    naming rule it is checking.
    """
    doc_generate.main([
        "--type", "investigation",
        "--slug", slug,
        "--parent-dir", str(story),
        "--title", "Probe investigation",
        "--description", "A probe of the investigation scaffold.",
    ])
    written = [f for f in story.glob("*.org") if f.name != "story.org"]
    assert len(written) == 1, written
    return written[0]


def _field(text, name):
    for line in text.splitlines():
        if line.startswith(f"#+{name}:"):
            return line.split(":", 1)[1].strip()
    raise AssertionError(f"no #+{name} in\n{text}")


def test_it_lands_in_the_story_folder_with_a_type_prefix(tmp_path):
    out = _scaffold(_story_dir(tmp_path), "probe_one")
    assert out.exists()
    assert out.parent.name == "probe_story"


def test_it_is_scoped_to_the_story_that_commissioned_it(tmp_path):
    text = _scaffold(_story_dir(tmp_path), "probe_two").read_text(encoding="utf-8")
    assert _field(text, "type") == "investigation"
    # s1, the task level: one story's artifact, not a cross-cutting statement.
    assert _field(text, "level") == "s1"


def test_it_carries_the_story_sprint_and_version_tags(tmp_path):
    text = _scaffold(_story_dir(tmp_path), "probe_three").read_text(encoding="utf-8")
    tags = _field(text, "filetags")
    for tag in ("probe_story", "sprint_99", "v0"):
        assert f":{tag}:" in tags, tags


def test_an_already_prefixed_slug_is_not_double_prefixed(tmp_path):
    out = _scaffold(_story_dir(tmp_path), "investigation_probe_four")
    assert out.name == "investigation_probe_four.org"


def test_a_capture_keeps_its_own_shape(tmp_path):
    """The prefix rule is per type: a capture is a cross-sprint idea, not a
    story artifact, so it neither moves nor gains ancestor tags."""
    bucket = tmp_path / "product_backlog" / "inbox"
    bucket.mkdir(parents=True)
    doc_generate.main([
        "--type", "capture",
        "--slug", "probe_capture",
        "--parent-dir", str(bucket),
        "--title", "Probe capture",
        "--description", "A probe of the capture scaffold.",
    ])
    out = bucket / "probe_capture.org"
    assert out.exists()
    text = out.read_text(encoding="utf-8")
    assert _field(text, "level") == "cross"
