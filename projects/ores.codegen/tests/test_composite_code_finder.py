"""Tests for a finder that reads a parent column and several code columns.

A convention's identity is (party_id, kind_code, name), so its one lookup
reads all three. ``:service_find_by_code_column:`` names the code columns --
one, or several whitespace-separated -- and the generator joins the signature,
the where clause and the log expression once, so a composite finder renders
through the same template as a single-column one.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_composite_code_finder.py
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    parse_org,
    org_document_to_model,
    key_finders,
)
from codegen.core import _prepare_finder_render  # noqa: E402


BODY = """\
* Flags
:PROPERTIES:
:schema: public
:product: ores
:component: refdata
:END:

* Columns

** id
:PROPERTIES:
:type: uuid
:cpp_type: boost::uuids::uuid
:primary_key: true
:END:

** party_id
:PROPERTIES:
:type: uuid
:cpp_type: boost::uuids::uuid
:natural_key: true
:END:

** kind_code
:PROPERTIES:
:type: text
:cpp_type: std::string
:natural_key: true
:END:

** name
:PROPERTIES:
:type: text
:cpp_type: std::string
:natural_key: true
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent: api
:service_find_by_code_column: kind_code name
:service_find_by_code_parent_column: party_id
:END:
"""


def _model() -> dict:
    return org_document_to_model(parse_org(BODY))["domain_entity"]


def test_a_composite_finder_names_every_code_column():
    de = _model()
    assert de["service_find_by_code"] == {
        "column": "kind_code",
        "columns": ["kind_code", "name"],
        "parent_column": "party_id",
    }


def test_key_finders_carries_the_composite_columns_and_its_parent():
    [finder] = key_finders(_model())
    assert finder == {
        "column": "kind_code",
        "columns": ["kind_code", "name"],
        "suffix": "code",
        "parent_column": "party_id",
    }


def test_a_single_column_finder_keeps_the_shape_it_always_had():
    entity = {"service_find_by_code": {"column": "code", "columns": ["code"]}}
    assert key_finders(entity) == [{"column": "code", "suffix": "code"}]


def test_the_render_fragments_read_the_parent_then_each_code_column():
    [finder] = key_finders(_model())
    _prepare_finder_render(finder)
    assert finder["params"] == (
        "const std::string& party_id, const std::string& kind_code, "
        "const std::string& name")
    assert finder["where_and"] == (
        '"party_id"_c == party_id && "kind_code"_c == kind_code && '
        '"name"_c == name')
    assert finder["log_label"] == "party_id/kind_code/name"
    assert finder["log_fields"] == ('party_id << "/" << kind_code << "/" << name')
    assert finder["columns_label"] == "kind_code/name"
    assert finder["key_label"] == "party_id, kind_code and name"
    assert finder["key_tail"] == "kind_code and name"
    assert finder["key_noun"] == "triple"
    assert finder["service_params"] == (
        "const boost::uuids::uuid& party_id, const std::string& kind_code, "
        "const std::string& name")
    assert finder["service_call_args"] == (
        "boost::uuids::to_string(party_id), kind_code, name")


def test_a_single_column_finders_fragments_are_unchanged():
    [finder] = key_finders(
        {"service_find_by_code": {"column": "id_scheme",
                                  "columns": ["id_scheme"],
                                  "parent_column": "party_id"}})
    _prepare_finder_render(finder)
    assert finder["params"] == (
        "const std::string& party_id, const std::string& id_scheme")
    assert finder["where_and"] == (
        '"party_id"_c == party_id && "id_scheme"_c == id_scheme')
    assert finder["key_noun"] == "pair"
    assert finder["service_call_args"] == (
        "boost::uuids::to_string(party_id), id_scheme")
