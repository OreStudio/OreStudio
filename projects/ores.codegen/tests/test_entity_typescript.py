"""Tests for the TypeScript projection of an entity's derived CRUD protocol.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_entity_typescript.py

An entity model declares no messages: its C++ protocol header states the
standard CRUD set, and ``org_loader.entity_protocol_messages`` derives the
same list for the TypeScript twin. The two must agree message for message,
because a divergence is a wire shape only one side knows. The drift gate
compares the emitted files byte for byte, which catches a change to the
output but not a wrong derivation, so these cases pin the derivation to the
C++ block's conditionals.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.org_loader import (  # noqa: E402
    _ts_domain_type,
    entity_protocol_messages,
)

CODEGEN = REPO_ROOT / "projects/ores.codegen"
TENANT_TYPE = REPO_ROOT / "projects/ores.iam/modeling/ores.iam.tenant_type.org"
TENANT = REPO_ROOT / "projects/ores.iam/modeling/ores.iam.tenant.org"
ACCOUNT_PARTY = (
    REPO_ROOT / "projects/ores.iam/modeling/ores.iam.account_party_junction.org")

# A member the projection cannot state: it has no registered wire shape, so
# ``_ts_domain_type`` returns None and the domain interface would emit
# ``extra_payload: ;``.
UNMAPPED_COLUMN = """
** extra_payload
:PROPERTIES:
:type:     text
:cpp_type: std::map<std::string, int>
:nullable: false
:END:

A member with no TypeScript projection.
"""


def _write_tenant_type(tmp_path, *, extra_column="", extra_properties=""):
    """The tenant_type model, with an optional member added to * Columns and
    optional properties added to the file-level drawer."""
    text = TENANT_TYPE.read_text(encoding="utf-8")
    if extra_properties:
        text = text.replace(":END:\n#+title:",
                            f"{extra_properties}:END:\n#+title:", 1)
    if extra_column:
        text = text.replace("\n* SQL\n", f"{extra_column}\n* SQL\n", 1)
    path = tmp_path / TENANT_TYPE.name
    path.write_text(text, encoding="utf-8")
    return path


def _render_domain(model_path, tmp_path):
    """Render only the TypeScript domain interface for a model."""
    return generate_from_model(
        str(model_path), CODEGEN / "library" / "data",
        CODEGEN / "library" / "templates", tmp_path,
        target_template="domain_types.ts.mustache", target_output="out.ts")


def _entity(**overrides):
    """The enriched entity dict the derivation reads, at its smallest."""
    entity = {
        "component": "iam",
        "entity_singular": "tenant_type",
        "entity_plural": "tenant_types",
        "entity_plural_short": "types",
        "primary_key": {
            "column": "type",
            "columns": [{"column": "type", "is_uuid": False}],
        },
    }
    entity.update(overrides)
    # The version column, which the enrichment derives from the two shape
    # flags: an ordinary entity keeps one, a current-state table has no
    # history at all, and an audit-less table has a validity window with no
    # version in it.
    entity.setdefault("has_audit_columns",
                      not entity.get("current_state")
                      and not entity.get("no_audit_columns"))
    return entity


def _by_name(messages):
    return {message["name"]: message for message in messages}


def test_the_canonical_set_is_derived_in_the_specification_order():
    """The auxiliary records first, then each operation, request before reply."""
    messages = entity_protocol_messages(_entity())
    assert [m["name"] for m in messages] == [
        "tenant_type_key", "tenant_type_write", "tenant_type_change",
        "tenant_type_removal", "tenant_type_lookup",
        "tenant_type_version_key", "tenant_type_versions_filter",
        "list_tenant_types_request", "list_tenant_types_response",
        "get_tenant_type_request", "get_tenant_type_response",
        "get_many_tenant_types_request", "get_many_tenant_types_response",
        "put_tenant_type_request", "put_tenant_type_response",
        "put_many_tenant_types_request", "put_many_tenant_types_response",
        "delete_tenant_type_request", "delete_tenant_type_response",
        "delete_many_tenant_types_request", "delete_many_tenant_types_response",
        "list_tenant_type_versions_request",
        "list_tenant_type_versions_response",
        "get_tenant_type_version_request", "get_tenant_type_version_response",
    ]
    # A record carries no subject, because only an operation is addressed.
    for message in messages:
        assert ("subject" in message) == message["name"].endswith("_request")


def test_subjects_speak_the_four_segment_grammar():
    messages = _by_name(entity_protocol_messages(_entity()))
    assert {
        name: message["subject"]
        for name, message in messages.items() if "subject" in message
    } == {
        "list_tenant_types_request": "iam.v1.tenant_types.list",
        "get_tenant_type_request": "iam.v1.tenant_types.get",
        "get_many_tenant_types_request": "iam.v1.tenant_types.get_many",
        "put_tenant_type_request": "iam.v1.tenant_types.put",
        "put_many_tenant_types_request": "iam.v1.tenant_types.put_many",
        "delete_tenant_type_request": "iam.v1.tenant_types.delete",
        "delete_many_tenant_types_request": "iam.v1.tenant_types.delete_many",
        "list_tenant_type_versions_request":
            "iam.v1.tenant_types_versions.list",
        "get_tenant_type_version_request": "iam.v1.tenant_types_versions.get",
    }


def test_the_response_collection_uses_the_short_plural():
    messages = _by_name(entity_protocol_messages(_entity()))
    fields = {f["name"]: f
              for f in messages["list_tenant_types_response"]["fields"]}
    assert fields["types"]["ts_type"] == "TenantType[]"
    assert fields["total"]["ts_type"] == "number"


def test_the_key_record_carries_each_key_column_with_its_own_type():
    """A uuid key is not flattened to a string, and a composite key is whole."""
    text = _by_name(entity_protocol_messages(_entity()))
    assert [(f["name"], f["ts_type"])
            for f in text["tenant_type_key"]["fields"]] == [("type", "string")]

    uuid = _by_name(entity_protocol_messages(_entity(
        entity_singular="tenant", entity_plural="tenants",
        entity_plural_short="tenants",
        primary_key={"column": "id",
                     "columns": [{"column": "id", "is_uuid": True}]})))
    assert uuid["tenant_key"]["fields"][0]["cpp_type"] == "boost::uuids::uuid"

    composite = _by_name(entity_protocol_messages(_entity(
        primary_key={"column": "type", "columns": [
            {"column": "type", "is_uuid": False},
            {"column": "name", "is_uuid": False}]})))
    assert [f["name"] for f in composite["tenant_type_key"]["fields"]] == [
        "type", "name"]


def test_the_write_record_drops_every_server_owned_field():
    messages = _by_name(entity_protocol_messages(_entity(columns=[
        {"column": "type", "cpp_type": "std::string"},
        {"column": "name", "cpp_type": "std::string"},
        {"column": "tenant_id", "cpp_type": "boost::uuids::uuid"},
        {"column": "version", "cpp_type": "std::uint32_t"},
        {"column": "change_reason_code", "cpp_type": "std::string"},
    ])))
    assert [f["name"] for f in messages["tenant_type_write"]["fields"]] == [
        "type", "name"]


def test_a_key_column_is_never_stripped_as_server_owned():
    """A junction links parties, so its ``party_id`` is half of its key."""
    messages = _by_name(entity_protocol_messages(_entity(
        columns=[{"column": "party_id", "cpp_type": "boost::uuids::uuid"}],
        primary_key={"column": "party_id",
                     "columns": [{"column": "party_id", "is_uuid": True}]})))
    assert [f["name"] for f in messages["tenant_type_write"]["fields"]] == [
        "party_id"]


def test_the_filter_record_holds_one_optional_member_per_filterable_column():
    """Filtering is a record, so every member is optional and carries its type."""
    messages = _by_name(entity_protocol_messages(_entity(
        list_filter_column="name",
        columns=[{"column": "name", "cpp_type": "std::string"},
                 {"column": "account_id", "cpp_type": "boost::uuids::uuid"}],
        extra_list_requests=[{"filter_column": "account_id",
                              "nats_suffix": "list_by_account_id"}])))
    assert [(f["name"], f["ts_type"])
            for f in messages["tenant_types_filter"]["fields"]] == [
        ("name", "string | null"), ("account_id", "string | null")]
    list_request = {f["name"]: f
                    for f in messages["list_tenant_types_request"]["fields"]}
    assert list_request["filter"]["ts_type"] == "TenantTypesFilter | null"


def test_a_resource_that_filters_on_nothing_has_no_filter_record():
    messages = _by_name(entity_protocol_messages(_entity()))
    assert "tenant_types_filter" not in messages
    assert "filter" not in {f["name"] for f in
                            messages["list_tenant_types_request"]["fields"]}


def test_the_page_and_the_order_are_unconditional():
    fields = {f["name"]: f for f in _by_name(entity_protocol_messages(
        _entity()))["list_tenant_types_request"]["fields"]}
    assert fields["offset"]["default"] == "0"
    assert fields["limit"]["default"] == "100"
    assert fields["order"]["ts_type"] == "Order"


def test_a_scoped_read_is_the_list_with_the_relation_in_its_addressing():
    messages = _by_name(entity_protocol_messages(_entity(
        columns=[{"column": "account_id", "cpp_type": "boost::uuids::uuid"}],
        extra_list_requests=[{"filter_column": "account_id",
                              "nats_suffix": "list_by_account_id"}])))
    request = messages["list_by_account_id_tenant_types_request"]
    assert request["subject"] == "iam.v1.tenant_types.list_by_account_id"
    assert [f["name"] for f in request["fields"]] == [
        "account_id", "scope", "offset", "limit", "order", "filter"]
    scope = {f["name"]: f for f in request["fields"]}["scope"]
    assert scope["ts_type"] == "Scope"
    assert scope["default"] == "ores::utility::domain::scope::direct"
    assert messages["list_by_account_id_tenant_types_response"]["fields"][0][
        "ts_type"] == "Result"


def test_a_parent_relation_is_a_scoped_read_like_any_other():
    """Reading a subtree is the same verb, with ``scope`` saying so."""
    messages = _by_name(entity_protocol_messages(_entity(
        has_parent_id=True,
        presentation={"parent_id_field": "account_id"},
        columns=[{"column": "account_id", "cpp_type": "boost::uuids::uuid"}])))
    assert "list_by_account_id_tenant_types_request" in messages
    assert "get_tenant_type_hierarchy_request" not in messages


def test_the_versions_sub_resource_is_read_only():
    """A version is written by the database, so only the reads exist for it."""
    messages = _by_name(entity_protocol_messages(_entity()))
    assert messages["list_tenant_type_versions_request"]["subject"] == (
        "iam.v1.tenant_types_versions.list")
    assert messages["get_tenant_type_version_request"]["subject"] == (
        "iam.v1.tenant_types_versions.get")
    assert {f["name"]
            for f in messages["tenant_type_versions_filter"]["fields"]} == {
        "version", "from_version", "to_version"}
    assert [f["name"]
            for f in messages["get_tenant_type_version_request"]["fields"]] == [
        "key"]


def test_a_version_is_the_domain_row_itself():
    """A version carries the audit provenance, so it needs no record of its own."""
    messages = _by_name(entity_protocol_messages(_entity()))
    assert "tenant_type_version" not in messages
    assert messages["list_tenant_type_versions_response"]["fields"][1][
        "ts_type"] == "TenantType[]"
    assert messages["get_tenant_type_version_response"]["fields"][1][
        "cpp_type"] == "ores::iam::domain::tenant_type"


def test_a_batch_is_the_same_element_repeated():
    messages = _by_name(entity_protocol_messages(_entity()))
    assert messages["put_many_tenant_types_request"]["fields"][0][
        "cpp_type"] == "std::vector<tenant_type_change>"
    assert messages["delete_many_tenant_types_request"]["fields"][0][
        "cpp_type"] == "std::vector<tenant_type_removal>"
    # The element states its own belief, and the set states one intent.
    assert messages["tenant_type_change"]["fields"][1]["name"] == "precondition"
    assert "precondition" not in {f["name"] for f in
                                  messages["put_many_tenant_types_request"]["fields"]}


def test_the_shared_records_project_to_the_wire_protocol_package():
    messages = _by_name(entity_protocol_messages(_entity()))
    response = {f["name"]: f
                for f in messages["list_tenant_types_response"]["fields"]}
    assert response["result"]["ts_type"] == "Result"
    put = {f["name"]: f for f in messages["put_tenant_type_request"]["fields"]}
    assert put["intent"]["ts_type"] == "ChangeIntent"


def test_domain_member_types_project():
    assert _ts_domain_type("std::string") == "string"
    assert _ts_domain_type("int") == "number"
    assert _ts_domain_type("boost::uuids::uuid") == "string"
    assert _ts_domain_type(
        "std::chrono::system_clock::time_point") == "string"
    assert _ts_domain_type("std::optional<std::string>") == "string | null"
    assert _ts_domain_type(
        "ores::iam::domain::tenant_type") == "TenantType"
    assert _ts_domain_type(
        "std::vector<ores::iam::domain::tenant_type>") == "TenantType[]"
    assert _ts_domain_type("std::map<std::string, int>") is None


def test_the_ip_address_projects_to_a_string():
    """rfl::json writes boost::asio::ip::address through its reflector's
    std::string ReflType, so the domain member is a string, not a gap."""
    assert _ts_domain_type("boost::asio::ip::address") == "string"
    assert _ts_domain_type(
        "std::optional<boost::asio::ip::address>") == "string | null"


def test_an_unmapped_entity_domain_member_refuses_the_model(tmp_path):
    model = _write_tenant_type(tmp_path, extra_column=UNMAPPED_COLUMN)
    with pytest.raises(ValueError) as excinfo:
        _render_domain(model, tmp_path)
    message = str(excinfo.value)
    assert model.name in message
    assert "extra_payload" in message
    assert "std::map<std::string, int>" in message


@pytest.mark.parametrize("properties", [
    ":ores.ts.domain.enabled: nil\n",
    ":ores.ts.protocol.enabled: nil\n",
])
def test_a_model_property_cannot_silence_the_entity_domain_guard(
    tmp_path, properties
):
    model = _write_tenant_type(
        tmp_path, extra_column=UNMAPPED_COLUMN, extra_properties=properties)
    with pytest.raises(ValueError) as excinfo:
        _render_domain(model, tmp_path)
    assert "extra_payload" in str(excinfo.value)


def test_the_tenant_type_twin_matches_the_domain_class(tmp_path):
    for template, name in (
        ("ts_protocol.ts.mustache", "tenant_type_protocol.ts"),
        ("domain_types.ts.mustache", "tenant_type.ts"),
    ):
        generate_from_model(
            str(TENANT_TYPE), CODEGEN / "library" / "data",
            CODEGEN / "library" / "templates", tmp_path,
            target_template=template, target_output=name)

    domain = (tmp_path / "tenant_type.ts").read_text(encoding="utf-8")
    body = domain.split("export interface TenantType {", 1)[1].split("}", 1)[0]
    members = [line.strip().split(":", 1)[0] for line in body.splitlines()
               if line.strip()]
    assert members == [
        "version", "tenant_id", "type", "name", "description",
        "display_order", "modified_by", "performed_by",
        "change_reason_code", "change_commentary", "recorded_at"]

    protocol = (tmp_path / "tenant_type_protocol.ts").read_text(encoding="utf-8")
    assert ("import type { TenantType } from '../domain/tenant_type.js';"
            in protocol)
    assert "tenant_type: TenantType | null;" in protocol
    assert "tenant_type: TenantType;" in protocol
    assert "types: TenantType[];" in protocol
    assert "type: string;" in protocol
    assert 'list_tenant_types_request: "iam.v1.tenant_types.list",' in protocol


@pytest.mark.parametrize("cpp_type, expected", [
    # The fixed-width family and the floating types. Before these were
    # mapped, an unqualified name fell through to the PascalCase fallback and
    # a double rendered as ``Double``, a type that does not exist. Roughly
    # fifty entity members carry one.
    ("double", "number"),
    ("float", "number"),
    ("std::int8_t", "number"),
    ("std::int64_t", "number"),
    ("std::uint8_t", "number"),
    ("std::uint16_t", "number"),
    ("std::chrono::year_month_day", "string"),
    # Nullability and collections compose.
    ("std::optional<double>", "number | null"),
    ("std::optional<std::int64_t>", "number | null"),
    ("std::vector<double>", "number[]"),
    ("std::vector<std::optional<double>>", "number | null[]"),
])
def test_the_numeric_family_and_its_compositions(cpp_type, expected):
    assert _ts_domain_type(cpp_type) == expected


# An entity model may declare messages beside its derived CRUD set. The C++
# header renders them where its paste point sits and the TypeScript twin
# appends them to the derived list, so one section feeds both.
PARTY = REPO_ROOT / "projects/ores.refdata/modeling/ores.refdata.party.org"


def test_an_entity_declares_messages_beside_its_derived_set():
    from codegen.core import load_model

    entity = load_model(PARTY)["domain_entity"]
    declared = entity["declared_messages"]
    assert [m["name"] for m in declared] == [
        "get_party_composite_as_of_request",
        "get_party_composite_as_of_response",
    ]
    assert declared[0]["subject"] == "refdata.v1.parties.composite_as_of"
    assert declared[0]["response_type"] == "get_party_composite_as_of_response"
    assert [(f["name"], f.get("ts_type")) for f in declared[1]["fields"]] == [
        ("success", "boolean"),
        ("message", "string"),
        ("party", "Party"),
        ("identifiers", "PartyIdentifier[]"),
        ("contacts", "PartyContactInformation[]"),
    ]

    names = [m["name"] for m in entity_protocol_messages(entity)]
    assert names[-2:] == [
        "get_party_composite_as_of_request",
        "get_party_composite_as_of_response",
    ]


def test_a_declared_message_renders_on_both_twins(tmp_path):
    for template, name, expected in (
        ("cpp_protocol.hpp.mustache", "party_protocol.hpp", [
            "struct get_party_composite_as_of_request {",
            '    static constexpr std::string_view nats_subject = '
            '"refdata.v1.parties.composite_as_of";',
            "struct get_party_composite_as_of_response {",
            "    ores::refdata::domain::party party;",
            "    std::vector<ores::refdata::domain::party_identifier> identifiers;",
            "    std::vector<ores::refdata::domain::party_contact_information> contacts;",
        ]),
        ("ts_protocol.ts.mustache", "party_protocol.ts", [
            "export interface GetPartyCompositeAsOfRequest {",
            "export interface GetPartyCompositeAsOfResponse {",
            "    party: Party;",
            "    identifiers: PartyIdentifier[];",
            "    contacts: PartyContactInformation[];",
        ]),
    ):
        generate_from_model(
            str(PARTY), CODEGEN / "library" / "data",
            CODEGEN / "library" / "templates", tmp_path,
            target_template=template, target_output=name)
        rendered = (tmp_path / name).read_text(encoding="utf-8")
        assert '"refdata.v1.parties.composite_as_of"' in rendered, name
        for fragment in expected:
            assert fragment in rendered, f"{fragment!r} missing from {name}"


def test_declared_messages_and_the_paste_point_are_mutually_exclusive(tmp_path):
    """The C++ protocol header renders the paste point and then the declared
    messages, while the TypeScript twin has no paste point and renders the
    declared set once. A model that feeds both would ship the same structs
    twice on the C++ side alone, so the loader refuses the combination."""
    from codegen.org_loader import (
        PROTOCOL_MESSAGES_PASTE_KIND,
        org_document_to_model,
        parse_org,
    )

    text = PARTY.read_text(encoding="utf-8") + (
        "\n#+begin_src cpp :name protocol_messages"
        f" :implements {PROTOCOL_MESSAGES_PASTE_KIND}\n"
        "struct get_legacy_party_request {\n};\n#+end_src\n"
    )

    with pytest.raises(ValueError, match="Messages section"):
        org_document_to_model(parse_org(text))


# The cases above read a hand-built entity dict. Such a dict is shaped the way
# the code expects rather than the way the loader produces a model, so four
# defects reached the real models before any of them showed here. These cases
# run the derivation over the committed models, which is what catches them.
def test_a_real_model_derives_named_and_typed_members():
    from codegen.core import load_model

    entity = load_model(TENANT)["domain_entity"]
    messages = _by_name(entity_protocol_messages(entity))

    # An enriched * Columns entry is named ``name``, not ``column``. Reading
    # only one spelling rendered members with no name at all.
    for message in messages.values():
        assert all(f["name"] for f in message["fields"]), message["name"]

    # A key column's type is its own ``cpp_type``, not an ``is_uuid`` flag.
    key = {f["name"]: f for f in messages["tenant_key"]["fields"]}
    assert key["id"]["cpp_type"] == "boost::uuids::uuid"
    assert key["id"]["ts_type"] == "string"


def test_a_real_model_states_its_key_in_the_write_record():
    """The loader partitions the primary key and the natural keys out of
    ``columns``, so a write record built from ``columns`` alone left a create
    unable to say what it creates."""
    from codegen.core import load_model
    from codegen.org_loader import _column_name

    entity = load_model(TENANT)["domain_entity"]
    assert entity["columns"], "the fixture is only meaningful on a real model"
    assert "id" not in [_column_name(c) for c in entity["columns"]]
    assert "code" in [_column_name(c) for c in entity["natural_keys"]]

    write = [f["name"]
             for f in _by_name(entity_protocol_messages(entity))["tenant_write"]
             ["fields"]]
    assert write[:2] == ["id", "code"]
    # Tenancy and version are still derived, not sent.
    assert "tenant_id" not in write
    assert "version" not in write


def test_a_junction_keeps_the_key_column_named_like_provenance():
    """An account_party's party_id is half its key, not the acting party.

    The server-owned set is stated by name, so it stripped the column that
    says which party the link names, and the write could not say what it
    linked.
    """
    from codegen.org_loader import (
        _column_name,
        junction_protocol_messages,
        load_org_junction_model,
    )

    junction = load_org_junction_model(ACCOUNT_PARTY)["junction"]
    assert junction["left"]["column"] == "account_id"
    assert junction["right"]["column"] == "party_id"
    assert _column_name(junction["right"]) == "party_id"

    messages = _by_name(junction_protocol_messages(junction))
    assert [f["name"] for f in messages["account_party_key"]["fields"]] == [
        "account_id", "party_id"]
    assert [f["name"] for f in messages["account_party_write"]["fields"]] == [
        "account_id", "party_id"]
