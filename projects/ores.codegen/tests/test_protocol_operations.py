"""Tests for the operation derivation the service and handler state themselves from.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_protocol_operations.py

The message list is the protocol. The service method, the handler method and
the subject are three names for one operation, so they are read off that one
list rather than kept in step by hand: a rename then moves all three, and an
operation the list does not state has no method to serve it. These cases pin
the projection, the verbs it groups by, and the two facts a service body needs
from the model -- whether a write needs a permission, and what a write record
carries.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.core import _protocol_owned_by_operation  # noqa: E402
from codegen.org_loader import (  # noqa: E402
    entity_protocol_messages,
    operations_by_verb,
    protocol_operations,
    write_record_for,
)

IAM_MODELING = REPO_ROOT / "projects/ores.iam/modeling"


def _entity(**overrides):
    """An enriched entity with a version column, a relation and a key."""
    entity = {
        "component": "iam",
        "entity_singular": "account_contact_information",
        "entity_plural": "account_contact_informations",
        "entity_plural_short": "account_contact_informations",
        "has_audit_columns": True,
        "primary_key": {
            "column": "id",
            "columns": [{"column": "id", "is_uuid": True,
                         "cpp_type": "boost::uuids::uuid"}],
        },
        # The loader partitions a foreign key that identifies the child within
        # its parent into the natural keys, so the relation lives there rather
        # than in the plain columns.
        "natural_keys": [{"column": "account_id", "is_uuid": True,
                          "cpp_type": "boost::uuids::uuid"}],
        "columns": [{"column": "email", "cpp_type": "std::string"}],
        "extra_list_requests": [{"filter_column": "account_id",
                                 "nats_suffix": "list_by_account_id"}],
    }
    entity.update(overrides)
    return entity


def _operations(**overrides):
    return protocol_operations(entity_protocol_messages(_entity(**overrides)))


def _by_method(operations):
    return {operation["method"]: operation for operation in operations}


def test_one_operation_per_subject_with_the_method_named_by_the_request():
    operations = _operations()
    assert [operation["method"] for operation in operations] == [
        "list_account_contact_informations",
        "get_account_contact_information",
        "get_many_account_contact_informations",
        "put_account_contact_information",
        "put_many_account_contact_informations",
        "delete_account_contact_information",
        "delete_many_account_contact_informations",
        "list_by_account_id_account_contact_informations",
        "list_account_contact_information_versions",
        "get_account_contact_information_version",
    ]
    # One name per operation: the request is the method plus its suffix.
    for operation in operations:
        assert operation["request"] == operation["method"] + "_request"
        assert operation["response"]


def test_the_operations_carry_the_subject_the_messages_state():
    by_method = _by_method(_operations())
    assert (by_method["list_account_contact_informations"]["subject"]
            == "iam.v1.account_contact_informations.list")
    assert (by_method["list_by_account_id_account_contact_informations"]["subject"]
            == "iam.v1.account_contact_informations.list_by_account_id")
    assert (by_method["get_account_contact_information_version"]["subject"]
            == "iam.v1.account_contact_informations_versions.get")


def test_each_operation_states_its_verb_and_the_request_does_too():
    by_method = _by_method(_operations())
    assert by_method["list_account_contact_informations"]["verb"] == "list"
    assert (by_method["list_by_account_id_account_contact_informations"]["verb"]
            == "list_scoped")
    assert by_method["get_account_contact_information"]["verb"] == "get"
    assert by_method["get_many_account_contact_informations"]["verb"] == "get_many"
    assert by_method["put_account_contact_information"]["verb"] == "put"
    assert by_method["put_many_account_contact_informations"]["verb"] == "put_many"
    assert by_method["delete_account_contact_information"]["verb"] == "delete"
    assert (by_method["delete_many_account_contact_informations"]["verb"]
            == "delete_many")
    # A versions list is a list and a single version is a get, but each answers
    # a different sub-resource and so takes a body of its own.
    assert (by_method["list_account_contact_information_versions"]["verb"]
            == "list_versions")
    assert (by_method["get_account_contact_information_version"]["verb"]
            == "get_version")


def test_every_verb_the_operations_state_is_a_verb_the_service_renders():
    """A verb no template body renders would be an operation with no method."""
    rendered_verbs = {
        "list", "list_scoped", "list_versions",
        "get", "get_many", "get_version",
        "put", "put_many", "delete", "delete_many",
    }
    assert {operation["verb"] for operation in _operations()} == rendered_verbs


def test_grouping_by_verb_keeps_every_operation_exactly_once():
    operations = _operations()
    grouped = operations_by_verb(operations)
    assert sorted(
        operation["method"] for group in grouped.values() for operation in group
    ) == sorted(operation["method"] for operation in operations)
    assert grouped["list"][0]["method"] == "list_account_contact_informations"
    assert len(grouped["list_scoped"]) == 1
    assert len(grouped["list_versions"]) == 1


def test_a_read_names_no_permission_and_a_write_names_the_one_it_needs():
    by_method = _by_method(_operations())
    assert by_method["list_account_contact_informations"]["is_write"] is False
    assert by_method["list_account_contact_informations"]["permission"] == ""
    assert by_method["get_account_contact_information"]["is_write"] is False
    assert by_method["put_account_contact_information"]["is_write"] is True
    assert by_method["put_account_contact_information"]["permission"] == "write"
    assert by_method["put_many_account_contact_informations"]["permission"] == "write"
    assert by_method["delete_account_contact_information"]["is_write"] is True
    assert by_method["delete_account_contact_information"]["permission"] == "delete"
    assert (by_method["delete_many_account_contact_informations"]["permission"]
            == "delete")


def test_a_scoped_read_names_the_relation_and_its_type():
    scoped = _by_method(_operations())[
        "list_by_account_id_account_contact_informations"]
    assert scoped["leading"] == "account_id"
    assert scoped["has_scope"] is True
    assert scoped["leading_is_uuid"] is True
    # The relation is stated with the column's own type, so a caller does not
    # have to know that the store happens to take it as text.
    assert scoped["fields"][0]["cpp_type"] == "boost::uuids::uuid"


def test_the_relation_is_typed_from_where_the_model_states_it():
    """A relation is often a natural key rather than a plain column, so a
    lookup that searched the columns alone would type it a string while the
    write record typed the same column a uuid -- one column, two types."""
    entity = _entity()
    del entity["columns"]
    messages = {m["name"]: m for m in entity_protocol_messages(entity)}
    write = {field["name"]: field["cpp_type"]
             for field in messages["account_contact_information_write"]["fields"]}
    scoped = {field["name"]: field["cpp_type"]
              for field in messages["list_by_account_id_account_contact_informations_request"]["fields"]}
    assert write["account_id"] == "boost::uuids::uuid"
    assert scoped["account_id"] == write["account_id"]


def test_a_write_record_carries_the_key_and_the_user_owned_fields_alone():
    fields = {field["name"]: field for field in write_record_for(_entity())}
    # A create states its own key, which for this entity is the surrogate id
    # and, as a natural key, the account the row belongs to.
    assert set(fields) == {"id", "account_id", "email"}
    # The domain access path is stated, because a composed entity reaches a
    # field through a group member and the two names are not the same string.
    assert fields["email"]["domain_member"] == "email"


def test_a_write_record_strips_every_field_the_server_derives():
    entity = _entity(columns=[
        {"column": "email", "cpp_type": "std::string"},
        {"column": "tenant_id", "cpp_type": "std::string"},
        {"column": "modified_by", "cpp_type": "std::string"},
        {"column": "performed_by", "cpp_type": "std::string"},
        {"column": "change_reason_code", "cpp_type": "std::string"},
        {"column": "change_commentary", "cpp_type": "std::string"},
        {"column": "version", "cpp_type": "int"},
        {"column": "valid_from", "cpp_type": "std::string"},
        {"column": "valid_to", "cpp_type": "std::string"},
    ])
    assert set(field["name"] for field in write_record_for(entity)) == {
        "id", "account_id", "email"}


def test_a_key_column_is_never_stripped_even_when_its_name_is_server_owned():
    """A junction's party_id is half of its key, not the acting party."""
    entity = _entity(
        primary_key={"column": "party_id",
                     "columns": [{"column": "party_id", "is_uuid": True,
                                  "cpp_type": "boost::uuids::uuid"}]},
        natural_keys=[],
        columns=[{"column": "tenant_id", "cpp_type": "std::string"}])
    assert "party_id" in {field["name"] for field in write_record_for(entity)}


def test_a_derived_protocol_is_not_assumed_where_an_operation_model_owns_it():
    """The service speaks the derived names, so an owned protocol must not be
    assumed: account's list request is spelled differently, and its workflows
    live in the operation model beside it.

    Session used to be in the first group and is not any more -- its operation
    model was renamed, so the entity derives its own protocol. That is the
    conversion the shell pilot exists to make, and this case now states it.
    """
    # Account and session were both owned. Neither is now: each derives its own
    # protocol, and account's writes are declared beside it rather than instead
    # of it, because a row write cannot state what making an account involves.
    for name in ("ores.iam.account.org", "ores.iam.session.org"):
        assert _protocol_owned_by_operation(
            IAM_MODELING / name,
            {"component": "iam", "entity_singular": name.split(".")[-2]}) is False
    assert _protocol_owned_by_operation(
        IAM_MODELING / "ores.iam.tenant_type.org",
        {"component": "iam", "entity_singular": "tenant_type"}) is False
