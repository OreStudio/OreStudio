"""Tests for the generated-file marker on generated output.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_generated_marker.py

Generated SQL names its template in an AUTO-GENERATED block; generated
C++ and TypeScript said nothing, so nothing told a reader that a file
was codegen output. The marker is emitted at the render seam rather
than by each template, so these tests pin the two properties that seam
must hold: every C++ or TypeScript output carries the marker naming its
own template, and no other output carries it.

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
    _emits_org,
    _emits_ts,
    generated_marker,
    render_template,
)

LICENCE = "/* licence */"
MARKER = "AUTO-GENERATED FILE - DO NOT EDIT MANUALLY"


def _render(tmp_path, template_name, licence_key="cpp_license"):
    template = tmp_path / template_name
    template.write_text("{{{" + licence_key + "}}}\nbody\n", encoding="utf-8")
    return render_template(str(template), {licence_key: LICENCE})


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
    out = _render(tmp_path, "cpp_widget.ui.mustache")
    assert MARKER not in out
    assert LICENCE in out


def test_marker_absent_when_no_cpp_licence(tmp_path):
    template = tmp_path / "sql_schema_table_create.mustache"
    template.write_text("{{{sql_license}}}\n", encoding="utf-8")
    assert MARKER not in render_template(str(template), {"sql_license": "-- l"})


def test_emits_cpp_classifies_by_output_suffix():
    assert _emits_cpp("cpp_enum.hpp.mustache")
    assert _emits_cpp("oresmd_parser.cpp.mustache")
    assert not _emits_cpp("cpp_widget.ui.mustache")
    assert not _emits_cpp("cmake_component_src.mustache")


def test_marker_block_is_a_closed_c_comment():
    block = generated_marker("cpp_enum.hpp.mustache")
    assert block.startswith("/**")
    assert block.endswith("*/")
    assert "*/" not in block[:-2]


def test_typescript_output_is_marked(tmp_path):
    out = _render(tmp_path, "ts_protocol.ts.mustache", "ts_license")
    assert MARKER in out
    assert "Template: ts_protocol.ts.mustache" in out
    assert out.index(LICENCE) < out.index(MARKER) < out.index("body")


def test_cpp_licence_does_not_mark_typescript(tmp_path):
    """The two licences stay paired with the output kind they belong to."""
    out = _render(tmp_path, "ts_protocol.ts.mustache")
    assert MARKER not in out
    assert LICENCE in out


def test_emits_ts_classifies_by_output_suffix():
    assert _emits_ts("ts_protocol.ts.mustache")
    assert not _emits_ts("cpp_enum.hpp.mustache")
    assert not _emits_ts("cpp_widget.ui.mustache")


ORG_DOCUMENT = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-000000000001
:END:
#+title: {{title}}

The blurb.
"""


def _render_org(tmp_path, template_name="shell_recipe.org.mustache"):
    template = tmp_path / template_name
    template.write_text(ORG_DOCUMENT, encoding="utf-8")
    return render_template(str(template), {"title": "A recipe"})


def test_org_output_is_marked(tmp_path):
    out = _render_org(tmp_path)
    assert "AUTO-GENERATED FILE - DO NOT EDIT MANUALLY" in out
    assert "#+generated:" in out
    assert "Template: shell_recipe.org.mustache" in out


def test_org_marker_follows_the_frontmatter_drawer(tmp_path):
    # The doc index reads the drawer from the first line, so a marker above it
    # would be folded into the blurb; and a # comment in that position would
    # too, because the blurb skips only #+ lines and blank lines.
    lines = _render_org(tmp_path).splitlines()
    assert lines[0] == ":PROPERTIES:"
    assert lines[2] == ":END:"
    assert lines[3].startswith("#+generated:")
    assert lines[4] == "#+title: A recipe"


def test_org_marker_keeps_the_blurb(tmp_path):
    out = _render_org(tmp_path)
    assert "The blurb." in out
    assert out.index("#+generated:") < out.index("The blurb.")


def test_org_marker_goes_to_the_top_when_there_is_no_drawer(tmp_path):
    template = tmp_path / "doc_recipe.org.mustache"
    template.write_text("#+title: {{title}}\n\nBody.\n", encoding="utf-8")
    out = render_template(str(template), {"title": "N"})
    assert out.splitlines()[0].startswith("#+generated:")


def test_the_cpp_marker_does_not_reach_an_org_document(tmp_path):
    # The C++ marker is a C comment, which would be inert text in org. The two
    # markers share their wording deliberately, so the comment form is what
    # distinguishes them.
    out = _render_org(tmp_path)
    assert "/**" not in out
    assert generated_marker("shell_recipe.org.mustache") not in out


def test_emits_org_classifies_by_output_suffix():
    assert _emits_org("shell_recipe.org.mustache")
    assert _emits_org("doc_recipe.org.mustache")
    assert not _emits_org("cpp_enum.hpp.mustache")
    assert not _emits_org("ts_protocol.ts.mustache")
    assert not _emits_org("cmake_component_src.mustache")
