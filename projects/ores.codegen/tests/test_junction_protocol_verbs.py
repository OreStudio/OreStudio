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

import pytest

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


def _render(tmp_path: Path, target_template: str, target_output: str,
            body: str = FIXTURE) -> str:
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(body, encoding="utf-8")
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


def test_a_junction_without_name_singular_is_rejected(tmp_path):
    """The singular names the header, the C++ types and the messages, and no
    rule derives it from the plural safely, so its absence is an error rather
    than a guess."""
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE.replace("#+name_singular: widget_owner\n", ""),
                     encoding="utf-8")

    with pytest.raises(ValueError, match="name_singular"):
        load_org_junction_model(model)


def test_the_batch_delete_refuses_mismatched_key_vectors(tmp_path):
    """The two key vectors address each row by index, so a pair of different
    lengths would silently drop the tail of the longer one and report success
    for a batch it did not fully apply."""
    handler = _render(tmp_path, "cpp_nats_handler.hpp.mustache", "handler.hpp")

    assert "req->widget_ids.size() != req->owner_ids.size()" in handler
    assert "i < req->widget_ids.size() && i < req->owner_ids.size()" not in handler


# A read-only junction's rows are provisioned outside the application, so
# it carries reads and counts and no write verb. The fixture keeps its
# ``:replace_by:`` flag, so the suppression is proven to come from
# ``:read_only:`` rather than from an absent opt-in.
READ_ONLY_FIXTURE = FIXTURE.replace(
    ":subcomponent: api\n",
    ":subcomponent: api\n:read_only:       true\n",
)


def _read_only_junction(tmp_path: Path) -> dict:
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(READ_ONLY_FIXTURE, encoding="utf-8")
    return load_org_junction_model(model)["junction"]


def test_a_read_only_junction_derives_no_write_verb(tmp_path):
    junction = _read_only_junction(tmp_path)

    names = [m["name"] for m in junction_protocol_messages(junction)]
    assert not any(
        name.startswith(("save_", "delete_", "replace_")) for name in names)
    # The reads and the counts stay: the repository serves both, and a
    # read-only row is still readable.
    for expected in (
        "get_widget_owners_request",
        "get_widget_owners_by_widget_request",
        "count_widget_owners_by_widget_request",
        "count_widget_owners_by_owner_request",
        "widget_owner_view",
    ):
        assert expected in names


def test_a_read_only_junction_renders_no_write_verb_on_either_twin(tmp_path):
    for template, name in (
        ("cpp_protocol.hpp.mustache", "widget_owner_protocol.hpp"),
        ("cpp_service.hpp.mustache", "widget_owner_service.hpp"),
        ("cpp_service.cpp.mustache", "widget_owner_service.cpp"),
        ("cpp_nats_handler.hpp.mustache", "widget_owner_handler.hpp"),
        ("cpp_nats_registrar.cpp.mustache", "widget_owner_registrar.cpp"),
        ("ts_protocol.ts.mustache", "widget_owner_protocol.ts"),
    ):
        rendered = _render(tmp_path, template, name, body=READ_ONLY_FIXTURE)
        for verb in ("save_widget_owner", "delete_widget_owner",
                     "remove_widget_owner", "replace_widget_owners"):
            assert verb not in rendered, f"{verb} leaked into {name}"
        # The count read survives on every twin that carries it. The
        # service names it from the repository short name, which this
        # fixture leaves unset, so match the column suffix both spellings
        # share.
        if template != "cpp_service.hpp.mustache":
            assert "count_by_widget" in rendered, name


# A junction whose party_id names the association's target, not the caller's
# own scope. The generic stamp() matches any field of that name and
# overwrites it from the JWT context, which would replace the requested
# association; the codegen emits a bespoke stamp instead.
PARTY_TARGET_FIXTURE = FIXTURE.replace(
    ":subcomponent: api\n",
    ":subcomponent: api\n:party_id_is_target: true\n",
)


def test_a_target_party_id_is_not_stamped_from_the_context(tmp_path):
    rendered = _render(tmp_path, "cpp_service.cpp.mustache",
                       "widget_owner_service.cpp", body=PARTY_TARGET_FIXTURE)
    assert "void stamp_widget_owner(" in rendered
    assert "stamp_widget_owner(t, ctx_);" in rendered
    # The generic helper, which would clobber the target, is not called.
    assert "stamp(t, ctx_);" not in rendered


def test_an_ordinary_junction_keeps_the_generic_stamp(tmp_path):
    rendered = _render(tmp_path, "cpp_service.cpp.mustache",
                       "widget_owner_service.cpp")
    assert "stamp(t, ctx_);" in rendered
    assert "void stamp_widget_owner(" not in rendered


def test_the_service_constructs_its_repository_from_the_stored_context(tmp_path):
    rendered = _render(tmp_path, "cpp_service.cpp.mustache",
                       "widget_owner_service.cpp")
    # ctx_ is declared before repo_ in the header, so it is initialised
    # first and repo_ must read it rather than the moved-from parameter.
    assert "repo_(ctx_)" in rendered
    assert "repo_(ctx)" not in rendered
