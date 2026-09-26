"""Tests for the generic junction protocol's verb set.

Run::

    pytest projects/ores.codegen/tests/test_junction_protocol_verbs.py

A junction model declares no messages. Its C++ header's ``{{#junction}}``
block and the TypeScript twin's ``{{#junction}}`` branch render the same
canonical protocol an entity renders: ``list``, ``get``, ``get_many``, a
scoped ``list_by_<side>`` per ``:list_by:= side, ``put``, ``put_many``,
``delete`` and ``delete_many``, each addressed by a ``<junction>_key`` that
carries both halves of the link. The service, handler and registrar facets
serve that same operation list, so the three cannot drift from the header.
The loader derives the set in ``org_loader.junction_protocol_messages`` so
the twins cannot disagree on a field or a subject.

The scoped read replies with link rows, as any scoped read replies; the
repository still returns domain rows, which is why compute's repository
tests are untouched by the wire change.
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

# A junction with both sides declared, a side read on the left, a payload
# column and an enriched right side -- the widest set the canonical junction
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


def test_the_derived_message_set_is_the_entity_protocol_keyed_by_both_sides(
        tmp_path):
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE, encoding="utf-8")
    junction = load_org_junction_model(model)["junction"]

    messages = junction_protocol_messages(junction)
    assert [(m["name"], m.get("subject")) for m in messages] == [
        ("widget_owner_key", None),
        ("widget_owner_write", None),
        ("widget_owner_change", None),
        ("widget_owner_removal", None),
        ("widget_owner_lookup", None),
        ("widget_owners_filter", None),
        ("list_widget_owners_request", "widget.v1.widget_owners.list"),
        ("list_widget_owners_response", None),
        ("get_widget_owner_request", "widget.v1.widget_owners.get"),
        ("get_widget_owner_response", None),
        ("get_many_widget_owners_request", "widget.v1.widget_owners.get_many"),
        ("get_many_widget_owners_response", None),
        ("put_widget_owner_request", "widget.v1.widget_owners.put"),
        ("put_widget_owner_response", None),
        ("put_many_widget_owners_request", "widget.v1.widget_owners.put_many"),
        ("put_many_widget_owners_response", None),
        ("delete_widget_owner_request", "widget.v1.widget_owners.delete"),
        ("delete_widget_owner_response", None),
        ("delete_many_widget_owners_request",
         "widget.v1.widget_owners.delete_many"),
        ("delete_many_widget_owners_response", None),
        ("list_by_widget_id_widget_owners_request",
         "widget.v1.widget_owners.list_by_widget_id"),
        ("list_by_widget_id_widget_owners_response", None),
    ]


def test_the_derived_messages_carry_the_whole_key_and_a_scoped_read(tmp_path):
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE, encoding="utf-8")
    junction = load_org_junction_model(model)["junction"]

    by_name = {m["name"]: m for m in junction_protocol_messages(junction)}
    key = by_name["widget_owner_key"]
    assert [(f["name"], f["ts_type"]) for f in key["fields"]] == [
        ("widget_id", "string"), ("owner_id", "string")]

    # A junction links rows, so a write has to name both halves of the key.
    write = by_name["widget_owner_write"]
    assert [(f["name"], f["ts_type"]) for f in write["fields"]] == [
        ("widget_id", "string"), ("owner_id", "string"), ("role", "string")]

    # The scoped read replies with link rows, so a caller that wants the
    # owner's code resolves it with get_many rather than a payload type.
    by_side = by_name["list_by_widget_id_widget_owners_response"]
    assert [(f["name"], f["ts_type"]) for f in by_side["fields"]] == [
        ("result", "Result"), ("widget_owners", "WidgetOwner[]"),
        ("total", "number")]
    assert "widget_owner_view" not in by_name


def test_the_cpp_header_emits_the_entity_protocol_keyed_by_both_sides(tmp_path):
    header = _render(tmp_path, "cpp_protocol.hpp.mustache",
                     "widget_owner_protocol.hpp")

    # Both halves of the key, each with its own column's type, and the payload
    # column on the write record rather than on a link request.
    assert "struct widget_owner_key {" in header
    assert "struct widget_owner_write {" in header
    assert "boost::uuids::uuid widget_id;" in header
    assert "boost::uuids::uuid owner_id;" in header
    assert "std::string role;" in header

    assert "struct list_widget_owners_request {" in header
    assert '"widget.v1.widget_owners.list"' in header
    assert "struct list_by_widget_id_widget_owners_request {" in header
    assert '"widget.v1.widget_owners.list_by_widget_id"' in header
    assert "struct put_widget_owner_request {" in header
    assert '"widget.v1.widget_owners.put"' in header
    assert "struct delete_many_widget_owners_request {" in header
    assert '"widget.v1.widget_owners.delete_many"' in header

    assert "struct link_widget_owner_request {" not in header
    assert "struct unlink_widget_owner_request {" not in header
    # Paging stays on both reads, and the scoped read states its reach.
    assert header.count("std::uint32_t offset = 0;") == 2
    assert header.count("std::uint32_t limit = 100;") == 2
    assert "ores::utility::domain::scope scope =" in header
    # The shared records are included, not left to a transitive include.
    assert '#include "ores.utility/domain/protocol.hpp"' in header


def test_the_typescript_twin_emits_the_same_set(tmp_path):
    protocol = _render(tmp_path, "ts_protocol.ts.mustache",
                       "widget_owner_protocol.ts")

    assert "import type { WidgetOwner } from '../domain/widget_owner.js';" in protocol
    assert "export interface ListWidgetOwnersRequest {" in protocol
    assert "export interface GetWidgetOwnerRequest {" in protocol
    assert "export interface WidgetOwnerKey {" in protocol
    assert "widget_id: string;" in protocol
    assert "owner_id: string;" in protocol
    assert "export interface PutWidgetOwnerRequest {" in protocol
    assert "export interface DeleteWidgetOwnerRequest {" in protocol
    assert "export interface ListByWidgetIdWidgetOwnersRequest {" in protocol
    assert "role: string;" in protocol
    # The scoped read replies with link rows and no view type of its own.
    assert "widget_owners: WidgetOwner[];" in protocol
    assert "WidgetOwnerView" not in protocol
    assert "LinkWidgetOwnerRequest" not in protocol
    # Same subjects as the C++ twin, from the one derived list.
    for subject in (
        '"widget.v1.widget_owners.list"',
        '"widget.v1.widget_owners.get"',
        '"widget.v1.widget_owners.list_by_widget_id"',
        '"widget.v1.widget_owners.put"',
        '"widget.v1.widget_owners.put_many"',
        '"widget.v1.widget_owners.delete"',
        '"widget.v1.widget_owners.delete_many"',
    ):
        assert subject in protocol


def test_a_junction_derives_no_announcement(tmp_path):
    """An entity's announcement is published by a generated event registrar.
    No registrar is emitted for a junction, so its announcement would be a
    wire type no code path reaches and no subject carries."""
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE, encoding="utf-8")
    junction = load_org_junction_model(model)["junction"]

    names = [m["name"] for m in junction_protocol_messages(junction)]
    assert "widget_owner_event" not in names
    assert not any(name.endswith("_event") for name in names)
    # The write records an announcement would carry stay: the writes are real.
    assert "widget_owner_write" in names
    assert "widget_owner_key" in names

    for template, name in (
        ("cpp_protocol.hpp.mustache", "widget_owner_protocol.hpp"),
        ("ts_protocol.ts.mustache", "widget_owner_protocol.ts"),
    ):
        rendered = _render(tmp_path, template, name)
        assert "widget_owner_event" not in rendered
        assert "WidgetOwnerEvent" not in rendered


def test_a_side_without_list_by_contributes_no_scoped_read(tmp_path):
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE.replace(":list_by:       true\n", ""),
                     encoding="utf-8")
    junction = load_org_junction_model(model)["junction"]

    names = [m["name"] for m in junction_protocol_messages(junction)]
    assert not any(name.startswith("list_by_") for name in names)
    # The unscoped read and the write verbs are unconditional.
    assert "list_widget_owners_request" in names
    assert "put_widget_owner_request" in names
    assert "delete_widget_owner_request" in names


def test_a_junction_without_name_singular_is_rejected(tmp_path):
    """The singular names the header, the C++ types and the messages, and no
    rule derives it from the plural safely, so its absence is an error rather
    than a guess."""
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(FIXTURE.replace("#+name_singular: widget_owner\n", ""),
                     encoding="utf-8")

    with pytest.raises(ValueError, match="name_singular"):
        load_org_junction_model(model)


def test_the_handler_hands_the_decoded_delete_many_to_one_service_call(tmp_path):
    """The canonical delete-many request carries one removal per link, each
    stating the whole key pair, so there are no per-column key vectors to
    disagree. The handler decodes once and hands the vector to one call."""
    handler = _render(tmp_path, "cpp_nats_handler.hpp.mustache", "handler.hpp")

    assert "decode<delete_many_widget_owners_request>(msg)" in handler
    body = handler.index("decode<delete_many_widget_owners_request>")
    end = handler.index("void ", body)
    delete_many = handler[body:end]

    assert delete_many.count("svc.delete_many_widget_owners(*req)") == 1
    assert "for (" not in delete_many
    # The retired per-column request carried vectors whose lengths had to
    # agree; the canonical request carries none.
    assert "widget_ids.size()" not in handler
    assert "req->widget_ids" not in handler


def test_the_put_many_checks_every_element_before_it_writes(tmp_path):
    """The batch makes the same per-element decision the single write makes,
    through ``prepare_change``, and only reaches the store once every element
    passes. Writing per element would leave a partial batch behind when one
    row is refused."""
    rendered = _render(tmp_path, "cpp_service.cpp.mustache",
                       "widget_owner_service.cpp")
    start = rendered.index("put_many_widget_owners(")
    end = rendered.index("repo_.write(batch, claims);", start)
    batch = rendered[start:end]

    assert batch.count("prepare_change(change, request.intent, value)") == 1
    assert "response.result = result;" in batch
    assert "return response;" in batch
    assert "repo_.write(" not in batch


def test_the_handler_hands_the_decoded_put_many_to_one_service_call(tmp_path):
    """The atomicity comes from the handler passing the whole decoded vector
    to the batch write, which lands it in one statement. A per-row loop in
    the handler would reopen the per-row transaction the batch exists to
    remove."""
    handler = _render(tmp_path, "cpp_nats_handler.hpp.mustache", "handler.hpp")
    body = handler.index("decode<put_many_widget_owners_request>")
    end = handler.index("void ", body)
    put_many = handler[body:end]

    assert put_many.count("svc.put_many_widget_owners(*req)") == 1
    assert "for (" not in put_many


# The canonical surface the fixture's junction derives: one method per
# operation, named after the operation and paired with its request type.
CANONICAL_OPERATIONS = [
    ("list_widget_owners", "list_widget_owners_request",
     "list_widget_owners_response"),
    ("get_widget_owner", "get_widget_owner_request",
     "get_widget_owner_response"),
    ("get_many_widget_owners", "get_many_widget_owners_request",
     "get_many_widget_owners_response"),
    ("put_widget_owner", "put_widget_owner_request",
     "put_widget_owner_response"),
    ("put_many_widget_owners", "put_many_widget_owners_request",
     "put_many_widget_owners_response"),
    ("delete_widget_owner", "delete_widget_owner_request",
     "delete_widget_owner_response"),
    ("delete_many_widget_owners", "delete_many_widget_owners_request",
     "delete_many_widget_owners_response"),
    ("list_by_widget_id_widget_owners",
     "list_by_widget_id_widget_owners_request",
     "list_by_widget_id_widget_owners_response"),
]

# The verbs a junction no longer speaks: a whole-set replacement, a batch
# additive save, a count per side and a per-plural list.
RETIRED_JUNCTION_TOKENS = ("save_", "count_by_", "replace_by_", "get_total_")


def test_the_service_declares_the_canonical_operation_methods(tmp_path):
    service = _render(tmp_path, "cpp_service.hpp.mustache",
                      "widget_owner_service.hpp")

    for method, request, response in CANONICAL_OPERATIONS:
        assert (f"messaging::{response} {method}("
                f"const messaging::{request}& request);") in service, method
    for retired in RETIRED_JUNCTION_TOKENS:
        assert retired not in service, retired
    # A single write and a batch state the same claim, so the check they share
    # is the service's decision and lives behind the operations.
    assert "prepare_change(" in service


def test_the_handler_renders_every_canonical_decode_type(tmp_path):
    handler = _render(tmp_path, "cpp_nats_handler.hpp.mustache", "handler.hpp")

    for method, request, _ in CANONICAL_OPERATIONS:
        assert f"void {method}(ores::nats::message msg)" in handler, method
        assert f"decode<{request}>(msg)" in handler, request
        assert f"svc.{method}(*req)" in handler, method
    # The write verbs prove a permission; the reads need authentication alone.
    starts = [handler.index(f"void {method}(ores::nats::message msg)")
              for method, _, _ in CANONICAL_OPERATIONS]
    starts.append(len(handler))
    for (method, _, _), start, end in zip(CANONICAL_OPERATIONS, starts,
                                          starts[1:]):
        block = handler[start:end]
        wants_permission = method.startswith(("put_", "delete_"))
        assert ("has_permission(" in block) is wants_permission, method
    for retired in RETIRED_JUNCTION_TOKENS:
        assert retired not in handler, retired
    # A transport failure answers with the canonical result envelope rather
    # than the retired success/message pair.
    assert "ores::utility::domain::outcome::failed" in handler
    assert "total_available_count" not in handler


def test_the_registrar_subscribes_every_canonical_subject(tmp_path):
    registrar = _render(tmp_path, "cpp_nats_registrar.cpp.mustache",
                        "widget_owner_registrar.cpp")

    for method, request, _ in CANONICAL_OPERATIONS:
        assert f"{request}::nats_subject" in registrar, request
        assert f"h->{method}(std::move(msg))" in registrar, method
    assert registrar.count("queue_subscribe") == len(CANONICAL_OPERATIONS)
    for retired in ("get_widget_owners_request", "save_widget_owner_request",
                    "count_widget_owners_by_widget_request",
                    "count_widget_owners_by_owner_request",
                    "replace_widget_owners_by_widget_request"):
        assert retired not in registrar, retired


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
        name.startswith(("put_", "put_many_", "delete_", "delete_many_"))
        for name in names)
    # A write record nothing refers to is dead surface, so it goes too.
    assert not any(
        name.endswith(("_write", "_change", "_removal")) for name in names)
    # The reads stay: a read-only row is still readable.
    for expected in (
        "widget_owner_key",
        "widget_owner_lookup",
        "list_widget_owners_request",
        "get_widget_owner_request",
        "get_many_widget_owners_request",
        "list_by_widget_id_widget_owners_request",
    ):
        assert expected in names


def test_a_read_only_junction_renders_no_write_verb_on_either_twin(tmp_path):
    for template, name in (
        ("cpp_protocol.hpp.mustache", "widget_owner_protocol.hpp"),
        ("ts_protocol.ts.mustache", "widget_owner_protocol.ts"),
    ):
        rendered = _render(tmp_path, template, name, body=READ_ONLY_FIXTURE)
        for verb in ("put_widget_owner", "delete_widget_owner",
                     "widget_owner_write", "widget_owner_removal"):
            assert verb not in rendered, f"{verb} leaked into {name}"
        # The reads survive on both twins.
        assert "list_by_widget_id_widget_owners" in rendered, name


def test_a_read_only_junction_renders_no_write_verb_on_the_wire_facets(
        tmp_path):
    """``:read_only:`` suppresses the wire writes on every facet that serves
    them, not only on the protocol twin: the service exposes no write method,
    the handler serves no write subject and the registrar subscribes none."""
    for template, name in (
        ("cpp_service.hpp.mustache", "widget_owner_service.hpp"),
        ("cpp_service.cpp.mustache", "widget_owner_service.cpp"),
        ("cpp_nats_handler.hpp.mustache", "widget_owner_handler.hpp"),
        ("cpp_nats_registrar.cpp.mustache", "widget_owner_registrar.cpp"),
    ):
        rendered = _render(tmp_path, template, name, body=READ_ONLY_FIXTURE)
        for verb in ("put_widget_owner", "put_many_widget_owners",
                     "delete_widget_owner", "delete_many_widget_owners"):
            assert verb not in rendered, f"{verb} leaked into {name}"
        # The reads stay on every facet.
        assert "list_by_widget_id_widget_owners" in rendered, name
    # A service with no write verb has no claim to prepare either.
    service = _render(tmp_path, "cpp_service.hpp.mustache",
                      "widget_owner_service.hpp", body=READ_ONLY_FIXTURE)
    assert "prepare_change(" not in service


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
    assert "stamp_widget_owner(out, ctx_);" in rendered
    # The generic helper, which would clobber the target, is not called.
    assert "stamp(out, ctx_," not in rendered


def test_an_ordinary_junction_keeps_the_generic_stamp(tmp_path):
    rendered = _render(tmp_path, "cpp_service.cpp.mustache",
                       "widget_owner_service.cpp")
    assert "stamp(out, ctx_," in rendered
    assert "void stamp_widget_owner(" not in rendered


def test_the_service_constructs_its_repository_from_the_stored_context(tmp_path):
    rendered = _render(tmp_path, "cpp_service.cpp.mustache",
                       "widget_owner_service.cpp")
    # ctx_ is declared before repo_ in the header, so it is initialised
    # first and repo_ must read it rather than the moved-from parameter.
    assert "repo_(ctx_)" in rendered
    assert "repo_(ctx)" not in rendered


# ``client_read_only`` is the other half of the pair: the rows have a
# server-side producer, so the repository stays writable, but a client
# reaches no write verb. ``read_only`` stops the repository too.
CLIENT_READ_ONLY_FIXTURE = FIXTURE.replace(
    ":subcomponent: api\n",
    ":subcomponent: api\n:client_read_only: true\n",
)


def test_client_read_only_hides_the_wire_writes_and_keeps_the_repository(tmp_path):
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(CLIENT_READ_ONLY_FIXTURE, encoding="utf-8")
    junction = load_org_junction_model(model)["junction"]
    assert junction["client_read_only"] is True
    assert junction["wire_write_enabled"] is False

    names = [m["name"] for m in junction_protocol_messages(junction)]
    assert not any(
        name.startswith(("put_", "put_many_", "delete_", "delete_many_"))
        for name in names)

    # The repository is the difference between the two flags: it keeps its
    # writes here, where a :read_only: junction would have dropped them.
    repository = _render(tmp_path, "cpp_domain_type_repository.hpp.mustache",
                         "widget_owner_repository.hpp",
                         body=CLIENT_READ_ONLY_FIXTURE)
    assert "void write(" in repository
    assert "void remove(" in repository

    # The wire surface is the same as :read_only:'s: no write verb served.
    for template, name in (
        ("cpp_service.hpp.mustache", "widget_owner_service.hpp"),
        ("cpp_nats_handler.hpp.mustache", "widget_owner_handler.hpp"),
        ("cpp_nats_registrar.cpp.mustache", "widget_owner_registrar.cpp"),
    ):
        rendered = _render(tmp_path, template, name,
                           body=CLIENT_READ_ONLY_FIXTURE)
        for verb in ("put_widget_owner", "put_many_widget_owners",
                     "delete_widget_owner", "delete_many_widget_owners"):
            assert verb not in rendered, f"{verb} leaked into {name}"


def test_read_only_drops_the_repository_writes(tmp_path):
    repository = _render(tmp_path, "cpp_domain_type_repository.hpp.mustache",
                         "widget_owner_repository.hpp",
                         body=READ_ONLY_FIXTURE)
    assert "void write(" not in repository
    assert "void remove(" not in repository


# Neither flag is declared without a * C++ drawer, so nothing suppresses the
# write surface. The derived key must still be present: the C++ templates read
# it, and Mustache treats a missing key as false.
NO_CPP_FIXTURE = FIXTURE.split("* C++")[0]


def test_a_junction_without_a_cpp_drawer_keeps_its_wire_writes(tmp_path):
    model = tmp_path / "ores.widget.widget_owner_junction.org"
    model.write_text(NO_CPP_FIXTURE, encoding="utf-8")
    junction = load_org_junction_model(model)["junction"]

    assert junction["wire_write_enabled"] is True
    names = [m["name"] for m in junction_protocol_messages(junction)]
    assert "put_widget_owner_request" in names
    assert "delete_widget_owner_request" in names
