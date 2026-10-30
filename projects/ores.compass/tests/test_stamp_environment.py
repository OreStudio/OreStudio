"""
Tests for the environment a scaffold states.

A document that declares =#+environment:= and leaves it empty is one a reader
cannot place, and the generator writes exactly that: the field is in every
template and no value is filled in. =compass add= stamps it from the checkout's
=ORES_ENV_NAME=, and leaves a document that does not declare the field alone.

Run with:  python -m pytest projects/ores.compass/tests/test_stamp_environment.py -v
"""

import sys
from pathlib import Path

import pytest

# Allow importing from the src directory without installing the package.
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass  # noqa: E402


@pytest.fixture
def env(monkeypatch):
    """The environment this checkout claims to be, as .env states it."""
    monkeypatch.setattr(
        compass, "_read_env_map", lambda: {"ORES_ENV_NAME": "probe_env"}
    )
    return "probe_env"


def _doc(tmp_path, frontmatter):
    path = tmp_path / "scenario_probe.org"
    path.write_text(f"{frontmatter}\n\n* Steps\n", encoding="utf-8")
    return path


def test_an_empty_environment_is_stamped(tmp_path, env):
    path = _doc(
        tmp_path,
        "#+title: Test Scenario: A probe\n#+updated: 2026-10-30\n#+environment:",
    )

    compass._stamp_environment(path)

    assert "#+environment: probe_env" in path.read_text(encoding="utf-8")


def test_the_field_stays_where_the_template_put_it(tmp_path, env):
    path = _doc(
        tmp_path,
        "#+title: Test Scenario: A probe\n#+environment:\n#+todo: PENDING | PASSED",
    )

    compass._stamp_environment(path)

    lines = path.read_text(encoding="utf-8").splitlines()
    assert lines[1] == "#+environment: probe_env"
    assert lines[2] == "#+todo: PENDING | PASSED"


def test_a_document_without_the_field_is_left_alone(tmp_path, env):
    path = _doc(tmp_path, "#+title: Capture: A probe\n#+updated: 2026-10-30")
    before = path.read_text(encoding="utf-8")

    compass._stamp_environment(path)

    assert path.read_text(encoding="utf-8") == before


def test_a_stated_environment_is_kept(tmp_path, env):
    path = _doc(tmp_path, "#+title: Task: A probe\n#+environment: another_env")

    compass._stamp_environment(path)

    assert "#+environment: another_env" in path.read_text(encoding="utf-8")


def test_no_environment_in_the_checkout_leaves_the_field_empty(tmp_path, monkeypatch):
    monkeypatch.setattr(compass, "_read_env_map", lambda: {})
    path = _doc(tmp_path, "#+title: Task: A probe\n#+environment:")

    compass._stamp_environment(path)

    assert "#+environment:\n" in path.read_text(encoding="utf-8")


def test_a_path_that_is_not_there_is_not_an_error(tmp_path, env):
    compass._stamp_environment(tmp_path / "absent.org")
    compass._stamp_environment(None)
