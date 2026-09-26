"""Tests for the CMake component source-list template.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_cmake_component_files.py

The template renders each component's checked-in source list. These tests
pin what it renders: the source files, the header list, and the absence of
any presentation-toolkit fragment. The drift gate cannot cover this,
because the gate proves regeneration is stable against the checked-in
tree, which stays true while a stale line is regenerated along with
everything else.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import render_template  # noqa: E402

TEMPLATE = (
    REPO_ROOT / "projects/ores.codegen/library/templates/cmake_component_files_src.mustache"
)
LICENCE = "# licence"


def _render(headers):
    return render_template(
        str(TEMPLATE),
        {
            "cmake_license": LICENCE,
            "component": {
                "files": ["alpha.cpp"],
                "headers": headers,
                "has_headers": bool(headers),
            },
        },
    )


def test_renders_no_retired_qt_fragment():
    out = _render(["ores.example/alpha.hpp"])
    assert "Q_OBJECT" not in out
    assert "AUTOMOC" not in out


def test_lists_the_source_files():
    out = _render([])
    assert '"alpha.cpp"' in out
    assert "set(files" in out


def test_headers_are_listed_relative_to_the_include_dir():
    out = _render(["ores.example/alpha.hpp", "ores.example/beta.hpp"])
    assert "set(HEADERS" in out
    assert "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.example/alpha.hpp" in out
    assert "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.example/beta.hpp" in out


def test_no_headers_block_when_the_component_has_no_include_dir():
    out = _render([])
    assert "set(HEADERS" not in out


def test_licence_precedes_the_generated_body():
    out = _render([])
    assert out.index(LICENCE) < out.index("set(files")
