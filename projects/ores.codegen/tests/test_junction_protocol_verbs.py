"""Tests for the generic junction protocol's added verbs.

Run::

    pytest projects/ores.codegen/tests/test_junction_protocol_verbs.py

A junction model declares no messages. Its C++ header's ``{{#junction}}``
block and the TypeScript twin's ``{{#junction}}`` branch render one generic
relationship protocol: a paged by-side read per side, an opt-in whole-set
replacement per side, a single-pair link and unlink, a count per side and
the ``<junction>_view`` payload. The loader derives the same set in
``org_loader.junction_protocol_messages`` so the two twins cannot disagree
on a field or a subject.

The by-side read and the replacement keep the shapes they already emitted,
because compute's ``app_version_platform`` and dq's ``dataset_bundle_member``
consumers are outside this work. These cases pin that compatibility too.
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
        ("get_widget_owners_by_widget_request",
         "widget.v1.widget_owners.list_by_widget_id"),
        ("get_widget_owners_by_widget_response", None),
        ("replace_widget_owners_by_widget_request",
         "widget.v1.widget_owners.replace_by_widget_id"),
        ("replace_widget_owners_by_widget_response", None),
        ("link_widget_owner_request",
         "widget.v1.widget_owners.link_widget_owner"),
        ("link_widget_owner_response", None),
        ("unlink_widget_owner_request",
         "widget.v1.widget_owners.unlink_widget_owner"),
        ("unlink_widget_owner_response", None),
        ("count_widget_owners_by_widget_request",
         "widget.v1.widget_owners.count_by_widget_id"),
        ("count_widget_owners_by_widget_response", None),
        ("count_widget_owners_by_owner_request",
         "widget.v1.widget_owners.count_by_owner_id"),
        ("count_widget_owners_by_owner_response", None),
        ("widget_owner_view", None),
    ]


def test_the_derived_messages_carry_the_link_payload_and_view(tmp_path):
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE, encoding="utf-8")
    junction = load_org_junction_model(model)["junction"]

    by_name = {m["name"]: m for m in junction_protocol_messages(junction)}
    link = by_name["link_widget_owner_request"]
    assert [f["name"] for f in link["fields"]] == [
        "widget_id", "owner_id", "role",
        "modified_by", "performed_by", "change_reason_code",
        "change_commentary"]

    view = by_name["widget_owner_view"]
    assert [(f["name"], f["ts_type"]) for f in view["fields"]] == [
        ("widget_owner", "WidgetOwner"), ("owner_code", "string")]


def test_the_cpp_header_emits_the_link_unlink_count_and_view(tmp_path):
    header = _render(tmp_path, "cpp_protocol.hpp.mustache",
                     "widget_owner_protocol.hpp")

    assert "struct link_widget_owner_request {" in header
    assert '"widget.v1.widget_owners.link_widget_owner"' in header
    assert "struct unlink_widget_owner_request {" in header
    assert '"widget.v1.widget_owners.unlink_widget_owner"' in header
    assert "struct count_widget_owners_by_widget_request {" in header
    assert "struct count_widget_owners_by_owner_request {" in header
    assert "struct widget_owner_view {" in header
    assert "ores::widget::domain::widget_owner widget_owner;" in header
    assert "std::string owner_code;" in header
    # The link request carries the junction's own payload columns.
    assert "std::string role;" in header
    # Compatibility: the by-side read still carries the junction rows, not
    # the view, so compute's and dq's consumers keep compiling.
    assert ("std::vector<ores::widget::domain::widget_owner> widget_owners;"
            in header)
    assert "std::vector<widget_owner_view>" not in header
    # Paging stays on the by-side read.
    assert "std::uint32_t offset = 0;" in header
    assert "std::uint32_t limit = 100;" in header


def test_the_typescript_twin_emits_the_same_set(tmp_path):
    protocol = _render(tmp_path, "ts_protocol.ts.mustache",
                       "widget_owner_protocol.ts")

    assert "import type { WidgetOwner } from '../domain/widget_owner.js';" in protocol
    assert "export interface LinkWidgetOwnerRequest {" in protocol
    assert "export interface UnlinkWidgetOwnerRequest {" in protocol
    assert "export interface CountWidgetOwnersByWidgetRequest {" in protocol
    assert "export interface CountWidgetOwnersByOwnerRequest {" in protocol
    assert "export interface WidgetOwnerView {" in protocol
    assert "widget_owner: WidgetOwner;" in protocol
    assert "owner_code: string;" in protocol
    assert "role: string;" in protocol
    # Same subjects as the C++ twin, from the one derived list.
    for subject in (
        '"widget.v1.widget_owners.list_by_widget_id"',
        '"widget.v1.widget_owners.replace_by_widget_id"',
        '"widget.v1.widget_owners.link_widget_owner"',
        '"widget.v1.widget_owners.unlink_widget_owner"',
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
    assert "link_widget_owner_request" in names
