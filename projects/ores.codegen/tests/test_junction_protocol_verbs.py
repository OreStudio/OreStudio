"""Tests for the generic junction protocol's verb set.

Run::

    pytest projects/ores.codegen/tests/test_junction_protocol_verbs.py

A junction model declares no messages. Its C++ header's ``{{#junction}}``
block and the TypeScript twin's ``{{#junction}}`` branch render one generic
relationship protocol: a paged unscoped read of the junction rows, a paged
by-side read per ``:list_by:= side that returns the ``<junction>_view``
payload, an opt-in whole-set replacement per ``:replace_by:= side, a batch
additive ``save``, a batch ``delete`` by the pair, and a count per side. The
loader derives the same set in
``org_loader.junction_protocol_messages`` so the two twins cannot disagree on
a field or a subject.

The by-side read returns the view; the repository still returns domain rows,
which is why compute's repository tests are untouched by the wire change.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.org_loader import (  # noqa: E402
    junction_protocol_messages,
    load_org_junction_model,
)

CODEGEN = REPO_ROOT / "projects/ores.codegen"

# A junction with both sides declared, an opt-in replacement on the left, a
# payload column and an enriched right side -- the widest set the generic
# protocol emits.
FIXTURE = """\
:PROPERTIES:
:ID: 00000000-0000-0000-0000-0000000000a1
:END:
#+title: ores.widget.widget_owner
#+type: ores.codegen.junction
#+component: widget
#+name: widget_owners
#+name_singular: widget_owner
#+name_title: Widget Owner
#+name_singular_words: widget owner association
#+brief: Links a widget to its owner.
#+product: ores
#+schema: public
#+has_tenant_id: true

* Flags
:PROPERTIES:
:profile: tenant-scoped-junction
:END:

* Left
:PROPERTIES:
:column:        widget_id
:column_short:  widget
:column_title:  Widget
:type:          uuid
:cpp_type:      boost::uuids::uuid
:list_by:       true
:replace_by:    true
:END:

* Right
:PROPERTIES:
:column:        owner_id
:column_short:  owner
:column_title:  Owner
:type:          uuid
:cpp_type:      boost::uuids::uuid
:enrich_code:   true
:references:    ores_widget_owners_tbl
:code_column:   code
:END:

* Columns

** role
:PROPERTIES:
:type:     text
:cpp_type: std::string
:nullable: false
:END:

* SQL

** Flags
:PROPERTIES:
:tablename: ores_widget_widget_owners_tbl
:END:

* C++

** Flags
:PROPERTIES:
:subcomponent: api
:END:
"""


def _render(tmp_path: Path, target_template: str, target_output: str) -> str:
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE, encoding="utf-8")
    generate_from_model(
        str(model), CODEGEN / "library" / "data",
        CODEGEN / "library" / "templates", tmp_path,
        target_template=target_template, target_output=target_output)
    return (tmp_path / target_output).read_text(encoding="utf-8")


def test_the_derived_message_set_is_the_generic_relationship_protocol(tmp_path):
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE, encoding="utf-8")
    junction = load_org_junction_model(model)["junction"]

    messages = junction_protocol_messages(junction)
    assert [(m["name"], m.get("subject")) for m in messages] == [
        ("get_widget_owners_request", "widget.v1.widget_owners.list"),
        ("get_widget_owners_response", None),
        ("get_widget_owners_by_widget_request",
         "widget.v1.widget_owners.list_by_widget_id"),
        ("get_widget_owners_by_widget_response", None),
        ("save_widget_owner_request", "widget.v1.widget_owners.save"),
        ("save_widget_owner_response", None),
        ("delete_widget_owner_request", "widget.v1.widget_owners.delete"),
        ("delete_widget_owner_response", None),
        ("replace_widget_owners_by_widget_request",
         "widget.v1.widget_owners.replace_by_widget_id"),
        ("replace_widget_owners_by_widget_response", None),
        ("count_widget_owners_by_widget_request",
         "widget.v1.widget_owners.count_by_widget_id"),
        ("count_widget_owners_by_widget_response", None),
        ("count_widget_owners_by_owner_request",
         "widget.v1.widget_owners.count_by_owner_id"),
        ("count_widget_owners_by_owner_response", None),
        ("widget_owner_view", None),
    ]


def test_the_derived_messages_carry_the_keys_and_view(tmp_path):
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE, encoding="utf-8")
    junction = load_org_junction_model(model)["junction"]

    by_name = {m["name"]: m for m in junction_protocol_messages(junction)}
    delete = by_name["delete_widget_owner_request"]
    assert [(f["name"], f["ts_type"]) for f in delete["fields"]] == [
        ("widget_ids", "string[]"), ("owner_ids", "string[]")]

    by_side = by_name["get_widget_owners_by_widget_response"]
    assert [(f["name"], f["ts_type"]) for f in by_side["fields"]] == [
        ("widget_owners", "WidgetOwnerView[]"),
        ("total_available_count", "number"),
        ("success", "boolean"),
        ("message", "string")]

    view = by_name["widget_owner_view"]
    assert [(f["name"], f["ts_type"]) for f in view["fields"]] == [
        ("widget_owner", "WidgetOwner"), ("owner_code", "string")]


def test_the_cpp_header_emits_the_read_save_delete_count_and_view(tmp_path):
    header = _render(tmp_path, "cpp_protocol.hpp.mustache",
                     "widget_owner_protocol.hpp")

    assert "struct get_widget_owners_request {" in header
    assert '"widget.v1.widget_owners.list"' in header
    assert "struct get_widget_owners_by_widget_request {" in header
    # The by-side read returns the view, not the bare junction row.
    assert "std::vector<widget_owner_view> widget_owners;" in header
    assert "std::vector<ores::widget::domain::widget_owner> widget_owners;" in header
    assert "struct save_widget_owner_request {" in header
    assert '"widget.v1.widget_owners.save"' in header
    assert "struct delete_widget_owner_request {" in header
    assert '"widget.v1.widget_owners.delete"' in header
    assert "std::vector<std::string> widget_ids;" in header
    assert "std::vector<std::string> owner_ids;" in header
    assert "struct count_widget_owners_by_widget_request {" in header
    assert "struct count_widget_owners_by_owner_request {" in header
    assert "struct widget_owner_view {" in header
    assert "ores::widget::domain::widget_owner widget_owner;" in header
    assert "std::string owner_code;" in header
    # The payload column rides on the saved junction row, not a link request.
    assert "struct link_widget_owner_request {" not in header
    assert "struct unlink_widget_owner_request {" not in header
    # Paging stays on both reads.
    assert header.count("std::uint32_t offset = 0;") == 2
    assert header.count("std::uint32_t limit = 100;") == 2


def test_the_typescript_twin_emits_the_same_set(tmp_path):
    protocol = _render(tmp_path, "ts_protocol.ts.mustache",
                       "widget_owner_protocol.ts")

    assert "import type { WidgetOwner } from '../domain/widget_owner.js';" in protocol
    assert "export interface GetWidgetOwnersRequest {" in protocol
    assert "export interface GetWidgetOwnersResponse {" in protocol
    assert "export interface GetWidgetOwnersByWidgetResponse {" in protocol
    assert "widget_owners: WidgetOwnerView[];" in protocol
    assert "export interface SaveWidgetOwnerRequest {" in protocol
    assert "export interface DeleteWidgetOwnerRequest {" in protocol
    assert "widget_ids: string[];" in protocol
    assert "export interface CountWidgetOwnersByWidgetRequest {" in protocol
    assert "export interface CountWidgetOwnersByOwnerRequest {" in protocol
    assert "export interface WidgetOwnerView {" in protocol
    assert "widget_owner: WidgetOwner;" in protocol
    assert "owner_code: string;" in protocol
    assert "LinkWidgetOwnerRequest" not in protocol
    assert "UnlinkWidgetOwnerRequest" not in protocol
    # Same subjects as the C++ twin, from the one derived list.
    for subject in (
        '"widget.v1.widget_owners.list"',
        '"widget.v1.widget_owners.list_by_widget_id"',
        '"widget.v1.widget_owners.save"',
        '"widget.v1.widget_owners.delete"',
        '"widget.v1.widget_owners.replace_by_widget_id"',
        '"widget.v1.widget_owners.count_by_widget_id"',
        '"widget.v1.widget_owners.count_by_owner_id"',
    ):
        assert subject in protocol


def test_a_junction_without_replace_emits_no_replacement_verb(tmp_path):
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE.replace(":replace_by:    true\n", ""),
                     encoding="utf-8")
    junction = load_org_junction_model(model)["junction"]

    names = [m["name"] for m in junction_protocol_messages(junction)]
    assert not any(name.startswith("replace_") for name in names)
    # The batch write verbs are unconditional.
    assert "save_widget_owner_request" in names
    assert "delete_widget_owner_request" in names
    # Both counts stay: the repository serves both.
    assert "count_widget_owners_by_widget_request" in names
    assert "count_widget_owners_by_owner_request" in names
