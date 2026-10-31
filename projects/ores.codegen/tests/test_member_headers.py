"""Which header a generated unit includes for the types its members hold.

A type recurses into a member's shape, so the unit that encodes it must include
the header that declares that shape. Getting this wrong is not a formatting
problem: the compiler reads the recursion as a type with no shape at all and
refuses it with ``implicit instantiation of undefined template text_form<T>``.

Three defects reached the compiler before this test existed:

- the root was spelled without the product prefix the templates carry, so a
  member's header was looked for at ``refdata.api/...``;
- a member named qualified (``std::vector<ores::refdata::domain::csa>``) was
  taken to the plain header, while the same member named bare resolved to the
  codec header, so only one of the two spellings carried the shape;
- the two callers (a row's columns, a message's fields) were separate copies of
  the same logic, so they could disagree.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_member_headers.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import _member_headers, _model_member_names  # noqa: E402

ENTITY = """#+type: ores.codegen.entity
#+entity_singular: widget
"""

JUNCTION = """#+type: ores.codegen.junction
#+name_singular: widget_role
"""

FIELD_GROUP = """#+type: ores.codegen.field_group
#+entity_singular: widget_details
"""


def a_model_dir(tmp_path):
    """A model directory naming one entity, one junction and one field group."""
    (tmp_path / "ores.x.widget.org").write_text(ENTITY, encoding="utf-8")
    (tmp_path / "ores.x.widget_role.org").write_text(JUNCTION, encoding="utf-8")
    (tmp_path / "ores.x.widget_details.org").write_text(
        FIELD_GROUP, encoding="utf-8")
    return tmp_path / "ores.x.widget.org"


def a_model(component_include="x.api"):
    return {
        "component": "x",
        "subcomponent": "api",
        "component_include": component_include,
    }


def test_a_qualified_member_takes_its_codec_header(tmp_path):
    """The spelling a message uses must resolve to the header with the shape."""
    headers = _member_headers(
        ["std::vector<ores::x::domain::widget_role>"], a_model_dir(tmp_path),
        a_model())
    assert headers == ["ores.x.api/domain/widget_role_codec.hpp"]


def test_a_bare_member_takes_the_same_codec_header(tmp_path):
    """The spelling a column uses must resolve to the same declaration."""
    headers = _member_headers(["std::vector<widget_role>"], a_model_dir(tmp_path),
                              a_model())
    assert headers == ["ores.x.api/domain/widget_role_codec.hpp"]


def test_a_field_group_takes_its_own_header(tmp_path):
    """A group states its shape in its own header, not in a codec header."""
    headers = _member_headers(["widget_details"], a_model_dir(tmp_path), a_model())
    assert headers == ["ores.x.api/domain/widget_details.hpp"]


def test_the_root_carries_the_product_prefix(tmp_path):
    """The templates write 'ores.{{component_include}}/...', so the root must too."""
    headers = _member_headers(["widget_role"], a_model_dir(tmp_path), a_model())
    assert all(h.startswith("ores.") for h in headers)
    assert "refdata.api/domain" not in headers[0]


def test_a_foreign_type_keeps_its_own_component(tmp_path):
    """A member from another component is not looked for in this one."""
    headers = _member_headers(
        ["ores::dq::domain::audit_record"], a_model_dir(tmp_path), a_model())
    assert headers == ["ores.dq.api/domain/audit_record.hpp"]


def test_the_envelopes_own_types_are_left_to_the_envelope_header(tmp_path):
    """ores::utility::domain carries its shapes through the header already included."""
    headers = _member_headers(
        ["ores::utility::domain::change_intent"], a_model_dir(tmp_path),
        a_model())
    assert headers == []


def test_a_header_is_named_once_in_first_seen_order(tmp_path):
    """Two members of one type yield one include, not two."""
    headers = _member_headers(
        ["std::vector<widget_role>", "widget_role"], a_model_dir(tmp_path),
        a_model())
    assert headers == ["ores.x.api/domain/widget_role_codec.hpp"]


def test_a_scalar_member_is_not_a_model_type(tmp_path):
    """A primitive, and a template argument, name no declaration of ours."""
    headers = _member_headers(["std::optional<std::string>", "int"], a_model_dir(
        tmp_path), a_model())
    assert headers == []


def test_the_model_dir_states_which_names_are_groups(tmp_path):
    """The classification is the directory's, not the caller's guess."""
    names, groups = _model_member_names(str(a_model_dir(tmp_path)))
    assert "widget" in names
    assert "widget_role" in names
    assert "widget_details" in groups
