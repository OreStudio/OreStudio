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
    return entity


def _by_name(messages):
    return {message["name"]: message for message in messages}


def test_the_standard_crud_set_is_derived_in_the_cpp_order():
    messages = entity_protocol_messages(_entity())
    assert [m["name"] for m in messages] == [
        "get_tenant_types_request", "get_tenant_types_response",
        "save_tenant_type_request", "save_tenant_type_response",
        "delete_tenant_type_request", "delete_tenant_type_response",
        "get_tenant_type_history_request", "get_tenant_type_history_response",
    ]


def test_subjects_match_the_cpp_header():
    messages = _by_name(entity_protocol_messages(_entity()))
    assert {
        name: message["subject"]
        for name, message in messages.items() if "subject" in message
    } == {
        "get_tenant_types_request": "iam.v1.tenant_types.list",
        "save_tenant_type_request": "iam.v1.tenant_types.save",
        "delete_tenant_type_request": "iam.v1.tenant_types.delete",
        "get_tenant_type_history_request": "iam.v1.tenant_types.history",
    }


def test_the_response_collection_uses_the_short_plural():
    messages = _by_name(entity_protocol_messages(_entity()))
    fields = {f["name"]: f for f in messages["get_tenant_types_response"]["fields"]}
    assert fields["types"]["ts_type"] == "TenantType[]"
    assert fields["total_available_count"]["ts_type"] == "number"


def test_a_uuid_key_deletes_by_ids_and_a_text_key_by_its_column():
    text = _by_name(entity_protocol_messages(_entity()))
    assert [f["name"] for f in text["delete_tenant_type_request"]["fields"]] == [
        "types"]

    uuid = _by_name(entity_protocol_messages(_entity(
        entity_singular="tenant", entity_plural="tenants",
        entity_plural_short="tenants",
        primary_key={"column": "id",
                     "columns": [{"column": "id", "is_uuid": True}]})))
    assert [f["name"] for f in uuid["delete_tenant_request"]["fields"]] == ["ids"]


def test_a_batch_save_and_a_list_filter_are_honoured():
    messages = _by_name(entity_protocol_messages(_entity(
        has_batch_save=True, list_filter_column="node_id",
        has_as_of_lookup=True)))
    save = messages["save_tenant_type_request"]["fields"]
    assert save[0]["name"] == "types"
    assert save[0]["ts_type"] == "TenantType[]"
    assert [f["name"] for f in messages["get_tenant_types_request"]["fields"]] == [
        "offset", "limit", "node_id", "as_of"]


def test_an_extra_list_request_becomes_its_own_message_pair():
    messages = _by_name(entity_protocol_messages(_entity(
        extra_list_requests=[{
            "name_suffix": "by_account_id",
            "nats_suffix": "list_by_account_id",
            "filter_column": "account_id",
            "default_limit": 100,
        }])))
    extra = messages["get_tenant_types_by_account_id_request"]
    assert extra["subject"] == "iam.v1.tenant_types.list_by_account_id"
    assert [f["name"] for f in extra["fields"]] == [
        "account_id", "offset", "limit"]
    assert messages["get_tenant_types_by_account_id_response"]["fields"][0][
        "ts_type"] == "TenantType[]"


def test_a_read_for_cache_pair_is_derived_only_when_flagged():
    without = _by_name(entity_protocol_messages(_entity()))
    assert "read_tenant_types_for_cache_request" not in without

    with_cache = _by_name(entity_protocol_messages(
        _entity(read_for_cache=True)))
    request = with_cache["read_tenant_types_for_cache_request"]
    assert request["subject"] == "iam.v1.tenant_types.read"
    assert [f["name"] for f in request["fields"]] == ["tenant_id"]


def test_a_registered_utility_type_projects_on_the_hierarchy_response():
    """The hierarchy response carries the shared utility interface, so the
    derived field names it rather than leaving a gap the guard refuses."""
    messages = _by_name(entity_protocol_messages(_entity(has_parent_id=True)))
    hierarchy = messages["get_tenant_type_hierarchy_response"]
    roots = {f["name"]: f for f in hierarchy["fields"]}["roots"]
    assert roots["ts_type"] == "HierarchyNode[]"


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
    assert "data: TenantType;" in protocol
    assert "types: TenantType[];" in protocol
    assert "type: string;" in protocol
    assert 'get_tenant_types_request: "iam.v1.tenant_types.list",' in protocol


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
