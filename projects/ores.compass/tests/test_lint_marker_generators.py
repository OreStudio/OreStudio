"""
Tests for the generated-marker lint.

A marker promises that a named generator rewrites the region. The generators
are either a script in this repository or a codec of the pinned compass-ng
release, and the lint must accept both without reading a file for the second.

Run with:  python -m pytest projects/ores.compass/tests/test_lint_marker_generators.py -v
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import compass


def _lint(text):
    return compass._lint_marker_generators([(Path("doc/example.org"), text)])


def test_a_codec_of_the_pinned_generator_is_accepted():
    assert _lint("# BEGIN generated captures (compass generate: backlog_indexes)\n") == []


def test_a_codec_with_no_name_is_refused():
    found = _lint("# BEGIN generated captures (compass generate: )\n")
    assert found and "no generator codec" in found[0][2]


def test_a_script_that_does_not_exist_is_refused():
    found = _lint("# BEGIN generated x (scripts/not_there.py)\n")
    assert found and "does not exist" in found[0][2]


def test_a_script_that_never_writes_the_file_is_refused():
    found = _lint("# BEGIN generated x (projects/ores.compass/src/env_init.py)\n")
    assert found and "never writes" in found[0][2]
