"""Tests for the generated-file marker on C++ output.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_generated_marker.py

Generated SQL names its template in an AUTO-GENERATED block; generated
C++ said nothing, so nothing told a reader that a file was codegen
output. The marker is emitted at the render seam rather than by each
template, so these tests pin the two properties that seam must hold:
every C++ output carries the marker naming its own template, and no
other output carries it.

The drift gate cannot catch a regression here. It proves regeneration
is stable against the checked-in tree, which stays true if the marker
stops being emitted and the tree is regenerated to match.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import (  # noqa: E402
    _emits_cpp,
    cpp_generated_marker,
    render_template,
)

LICENCE = "/* licence */"
MARKER = "AUTO-GENERATED FILE - DO NOT EDIT MANUALLY"


def _render(tmp_path, template_name, body="{{{cpp_license}}}\nbody\n"):
    template = tmp_path / template_name
    template.write_text(body, encoding="utf-8")
    return render_template(str(template), {"cpp_license": LICENCE})


def test_marker_names_the_template(tmp_path):
    out = _render(tmp_path, "cpp_domain_type_entity.hpp.mustache")
    assert MARKER in out
    assert "Template: cpp_domain_type_entity.hpp.mustache" in out


def test_marker_follows_the_licence(tmp_path):
    out = _render(tmp_path, "cpp_domain_type_table.cpp.mustache")
    assert out.index(LICENCE) < out.index(MARKER)
    assert out.index(MARKER) < out.index("body")


def test_implementation_and_header_both_marked(tmp_path):
    for name in ("cpp_service.hpp.mustache", "cpp_service.cpp.mustache"):
        assert MARKER in _render(tmp_path, name)


def test_non_cpp_output_is_not_marked(tmp_path):
    """A template may carry the C++ licence and still not emit C++."""
    out = _render(tmp_path, "cpp_qt_detail_dialog.ui.mustache")
    assert MARKER not in out
    assert LICENCE in out


def test_marker_absent_when_no_cpp_licence(tmp_path):
    template = tmp_path / "sql_schema_table_create.mustache"
    template.write_text("{{{sql_license}}}\n", encoding="utf-8")
    assert MARKER not in render_template(str(template), {"sql_license": "-- l"})


def test_emits_cpp_classifies_by_output_suffix():
    assert _emits_cpp("cpp_enum.hpp.mustache")
    assert _emits_cpp("oresmd_parser.cpp.mustache")
    assert not _emits_cpp("cpp_qt_detail_dialog.ui.mustache")
    assert not _emits_cpp("cmake_component_src.mustache")


def test_marker_block_is_a_closed_c_comment():
    block = cpp_generated_marker("cpp_enum.hpp.mustache")
    assert block.startswith("/**")
    assert block.endswith("*/")
    assert "*/" not in block[:-2]
