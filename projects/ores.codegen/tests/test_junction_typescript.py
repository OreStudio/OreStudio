"""Tests for the TypeScript projection of a junction's domain object.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_junction_typescript.py

A junction model declares no messages: its protocol header comes from a
separate operation model, and ``ores.ts.domain`` emits the interface that
operation protocol imports. The interface must mirror the C++ junction
class member for member, because the protocol payloads carry the object
and a divergence is a wire shape only one side knows. The drift gate
compares the emitted files byte for byte, which catches a change to the
output but not a wrong derivation, so these cases pin the projection to
the C++ block's fields.

The guard that refuses a junction with an unprojectable member is pinned
here too: without it the member would vanish from the interface silently.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import generate_from_model  # noqa: E402
from codegen.org_loader import (  # noqa: E402
    _reject_silent_junction_ts_gap,
    _ts_domain_type,
    junction_ts_fields,
    load_org_junction_model,
    load_org_operation_model,
    ts_domain_imports,
)

CODEGEN = REPO_ROOT / "projects/ores.codegen"
ACCOUNT_PARTY = REPO_ROOT / "projects/ores.iam/modeling/ores.iam.account_party_junction.org"
ACCOUNT_PARTY_MESSAGES = (
    REPO_ROOT / "projects/ores.iam/modeling/ores.iam.account_party_messages.org")


def _junction(**overrides):
    """The enriched junction dict the guard reads, at its smallest."""
    junction = {
        "left": {"column": "account_id",
                 "cpp_type": "boost::uuids::uuid", "ts_type": "string"},
        "right": {"column": "party_id",
                  "cpp_type": "boost::uuids::uuid", "ts_type": "string"},
        "columns": [],
    }
    junction.update(overrides)
    return junction


def test_the_account_party_twin_matches_the_cpp_class(tmp_path):
    for model, template, name in (
        (ACCOUNT_PARTY, "domain_types.ts.mustache", "account_party.ts"),
        (ACCOUNT_PARTY_MESSAGES, "ts_protocol.ts.mustache",
         "account_party_protocol.ts"),
    ):
        generate_from_model(
            str(model), CODEGEN / "library" / "data",
            CODEGEN / "library" / "templates", tmp_path,
            target_template=template, target_output=name)

    domain = (tmp_path / "account_party.ts").read_text(encoding="utf-8")
    body = domain.split("export interface AccountParty {", 1)[1].split("}", 1)[0]
    members = [line.strip().split(":", 1)[0] for line in body.splitlines()
               if line.strip()]
    assert members == [
        "version", "tenant_id", "account_id", "party_id", "modified_by",
        "performed_by", "change_reason_code", "change_commentary",
        "recorded_at"]
    assert "account_id: string;" in domain
    assert "party_id: string;" in domain


def test_the_operation_protocol_imports_the_interface_it_carries(tmp_path):
    generate_from_model(
        str(ACCOUNT_PARTY_MESSAGES), CODEGEN / "library" / "data",
        CODEGEN / "library" / "templates", tmp_path,
        target_template="ts_protocol.ts.mustache",
        target_output="account_party_protocol.ts")

    protocol = (tmp_path / "account_party_protocol.ts").read_text(encoding="utf-8")
    assert ("import type { AccountParty } from '../domain/account_party.js';"
            in protocol)
    assert "account_parties: AccountParty[];" in protocol
    assert "keys: AccountPartyKey[];" in protocol
    assert 'get_account_parties_by_account_request: "iam.v1.account-parties.by-account",' in protocol


def test_junction_member_types_project():
    assert _ts_domain_type("boost::uuids::uuid") == "string"
    assert _ts_domain_type("std::string") == "string"
    assert _ts_domain_type(
        "std::chrono::system_clock::time_point") == "string"
    assert _ts_domain_type("std::optional<int>") == "number | null"
    assert _ts_domain_type("std::map<std::string, int>") is None


def test_junction_fields_are_the_left_right_and_own_columns():
    junction = load_org_junction_model(ACCOUNT_PARTY)["junction"]
    assert [(f["name"], f["cpp_type"]) for f in junction_ts_fields(junction)] == [
        ("account_id", "boost::uuids::uuid"),
        ("party_id", "boost::uuids::uuid"),
    ]


def test_a_junction_member_with_no_projection_is_refused():
    junction = _junction(columns=[
        {"name": "amount", "cpp_type": "std::map<std::string, int>"}])
    with pytest.raises(ValueError, match=r"std::map<std::string, int>"):
        _reject_silent_junction_ts_gap("ores.iam.example_junction.org", junction)


def test_a_fully_projected_junction_passes_the_guard():
    _reject_silent_junction_ts_gap(
        "ores.iam.example_junction.org",
        _junction(columns=[{"name": "display_order",
                            "cpp_type": "int", "ts_type": "number"}]))


def test_an_operation_imports_each_domain_interface_once():
    operation = load_org_operation_model(ACCOUNT_PARTY_MESSAGES)["operation"]
    assert operation["domain_imports"] == [
        {"entity": "account_party", "entity_pascal": "AccountParty"}]

    messages = [
        {"fields": [
            {"cpp_type": "std::vector<ores::iam::domain::account_party>"},
            {"cpp_type": "std::vector<ores::iam::domain::account_party>"},
        ]},
        {"fields": [
            {"cpp_type": "ores::refdata::domain::party"},
            {"cpp_type":
                "std::vector<ores::utility::domain::hierarchy_node>"},
            {"cpp_type": "int"},
        ]},
    ]
    assert ts_domain_imports(messages) == [
        {"entity": "account_party", "entity_pascal": "AccountParty"},
        {"entity": "party", "entity_pascal": "Party"},
    ]
