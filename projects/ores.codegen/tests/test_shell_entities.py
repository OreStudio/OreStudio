"""Tests for the shell's view of an entity's derived operation set.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_shell_entities.py

An entity derives its verbs from its own shape, so the shell renders from that
same derivation rather than a hand-written command per verb. These cases pin
what a command asks for: which shape serves which verb, that a create and a
replace are one verb stating two claims, and that a command is measured against
the inputs it actually reads rather than every input the entity has.
"""
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import entity_shell_commands  # noqa: E402


def _key(name, cpp_type="std::string", **overrides):
    """A primary-key member. The loader spells these `column`, not `name`."""
    column = {"column": name, "cpp_type": cpp_type, "is_user_supplied": True}
    column.update(overrides)
    return column


def _column(name, cpp_type="std::string", **overrides):
    """A non-key column."""
    column = {"name": name, "cpp_type": cpp_type, "is_user_supplied": True}
    column.update(overrides)
    return column


def _write(name, cpp_type="std::string"):
    """A member of the wire write record."""
    return {"name": name, "cpp_type": cpp_type}


def _entity(columns=None, key=None, writes=None, operations=None):
    key = key if key is not None else [_key("type")]
    return {
        "component": "iam",
        "entity_singular": "tenant_type",
        "entity_plural": "tenant_types",
        "columns": columns if columns is not None else [],
        "primary_key": {"column": key[0]["column"], "columns": key},
        "write_fields": writes if writes is not None else [_write("type")],
        "operations": operations if operations is not None else [],
    }


def _operation(verb, request, response, subject, **overrides):
    operation = {
        "verb": verb,
        "request": request,
        "response": response,
        "subject": subject,
        "fields": [],
        "has_order": False,
        "has_filter": False,
        "has_scope": False,
        "leading": "",
    }
    operation.update(overrides)
    return operation


ALL_VERBS = [
    _operation("list", "list_tenant_types_request", "list_tenant_types_response",
               "iam.v1.tenant_types.list", has_order=True),
    _operation("get", "get_tenant_type_request", "get_tenant_type_response",
               "iam.v1.tenant_types.get"),
    _operation("get_many", "get_many_tenant_types_request",
               "get_many_tenant_types_response", "iam.v1.tenant_types.get_many"),
    _operation("put", "put_tenant_type_request", "put_tenant_type_response",
               "iam.v1.tenant_types.put"),
    _operation("put_many", "put_many_tenant_types_request",
               "put_many_tenant_types_response", "iam.v1.tenant_types.put_many"),
    _operation("delete", "delete_tenant_type_request",
               "delete_tenant_type_response", "iam.v1.tenant_types.delete"),
    _operation("delete_many", "delete_many_tenant_types_request",
               "delete_many_tenant_types_response",
               "iam.v1.tenant_types.delete_many"),
    _operation("list_versions", "list_tenant_type_versions_request",
               "list_tenant_type_versions_response",
               "iam.v1.tenant_types_versions.list", has_order=True),
    _operation("get_version", "get_tenant_type_version_request",
               "get_tenant_type_version_response",
               "iam.v1.tenant_types_versions.get"),
]


def _names(commands):
    return [command["command"] for command in commands]


class TestTheCommandSet:
    """One command per verb, and a put makes two."""

    def test_every_derived_verb_becomes_a_command(self):
        assert _names(entity_shell_commands(_entity(operations=ALL_VERBS))) == [
            "list", "get", "get-many", "add", "set", "put-many",
            "delete", "delete-many", "versions", "version",
        ]

    def test_a_create_states_the_row_must_not_exist(self):
        commands = entity_shell_commands(_entity(operations=ALL_VERBS))
        add = next(c for c in commands if c["command"] == "add")
        assert add["verb"] == "put"
        assert add["precondition"] == "must_not_exist"
        # A create names no version to match: it claims the row is absent.
        assert add["allows_version"] is False

    def test_a_replace_states_that_it_may_exist(self):
        commands = entity_shell_commands(_entity(operations=ALL_VERBS))
        set_ = next(c for c in commands if c["command"] == "set")
        assert set_["verb"] == "put"
        assert set_["precondition"] == "any"
        assert set_["allows_version"] is True

    def test_only_the_verbs_the_entity_derives_are_commands(self):
        # account_contact_information derives no delete_many and no versions.
        verbs = [op for op in ALL_VERBS
                 if op["verb"] not in ("delete_many", "list_versions", "get_version")]
        assert _names(entity_shell_commands(_entity(operations=verbs))) == [
            "list", "get", "get-many", "add", "set", "put-many", "delete",
        ]

    def test_a_relation_read_is_a_command_of_its_own(self):
        commands = entity_shell_commands(_entity(operations=[
            _operation("list_by_account_id", "list_by_account_id_x_request",
                       "list_by_account_id_x_response",
                       "iam.v1.x.list_by_account_id", has_order=True),
        ]))
        assert _names(commands) == ["by-account-id"]
        assert commands[0]["kind"] == "list_by"

    def test_an_entity_with_no_operations_has_no_commands(self):
        assert entity_shell_commands(_entity()) == []


class TestWhatACommandAsksFor:
    """The shape decides the inputs, not the entity's whole column list."""

    def test_a_paged_read_asks_for_no_key(self):
        commands = entity_shell_commands(_entity(operations=ALL_VERBS))
        listing = next(c for c in commands if c["command"] == "list")
        assert listing["usage"] == (
            "list [--offset <n>] [--limit <n>] [--order <field>] [--desc]")

    def test_a_versions_read_is_addressed_by_the_key(self):
        commands = entity_shell_commands(_entity(operations=ALL_VERBS))
        versions = next(c for c in commands if c["command"] == "versions")
        assert versions["usage"].startswith("versions <type>")

    def test_a_write_asks_for_its_write_record_and_not_the_key(self):
        commands = entity_shell_commands(_entity(operations=ALL_VERBS))
        add = next(c for c in commands if c["command"] == "add")
        # The key travels inside the write record, so the command must not ask
        # for it twice.
        assert add["usage"] == "add <type> <reason> <commentary>"

    def test_a_delete_is_addressed_by_the_key_and_takes_the_intent(self):
        commands = entity_shell_commands(_entity(operations=ALL_VERBS))
        delete = next(c for c in commands if c["command"] == "delete")
        assert delete["usage"] == "delete <type> <reason> <commentary> [--version <n>]"

    def test_a_version_read_asks_for_the_number(self):
        commands = entity_shell_commands(_entity(operations=ALL_VERBS))
        version = next(c for c in commands if c["command"] == "version")
        assert version["usage"] == "version <type> --version <n>"

    def test_a_batch_write_asks_for_how_many(self):
        commands = entity_shell_commands(_entity(operations=ALL_VERBS))
        batch = next(c for c in commands if c["command"] == "put-many")
        assert batch["usage"].startswith("put-many --count <n>")


class TestTheRefusal:
    """A command is measured against the inputs it reads."""

    def test_a_field_the_key_read_never_touches_does_not_condemn_it(self):
        # login_info carries two address columns a token cannot fill. They are
        # write fields, so the reads must not report them.
        entity = _entity(
            key=[_key("account_id")],
            writes=[_write("last_ip", "boost::asio::ip::address")],
            operations=ALL_VERBS)
        reads = [c for c in entity_shell_commands(entity)
                 if c["command"] in ("list", "get", "get-many", "delete")]
        assert {tuple(c["unsupported"]) for c in reads} == {()}

    def test_a_write_that_reads_an_unfillable_field_says_so(self):
        entity = _entity(
            key=[_key("account_id")],
            writes=[_write("last_ip", "boost::asio::ip::address")],
            operations=ALL_VERBS)
        add = next(c for c in entity_shell_commands(entity) if c["command"] == "add")
        assert add["unsupported"] == ["last_ip"]

    @pytest.mark.parametrize("cpp_type", [
        "std::string", "bool", "int", "std::uint16_t", "std::uint32_t",
        "std::uint64_t", "double", "boost::uuids::uuid",
        "std::vector<std::string>",
    ])
    def test_a_type_a_token_can_fill_is_fillable(self, cpp_type):
        entity = _entity(
            key=[_key("id", cpp_type)],
            writes=[_write("id", cpp_type)],
            operations=ALL_VERBS)
        commands = entity_shell_commands(entity)
        assert {tuple(c["unsupported"]) for c in commands} == {()}


class TestSupply:
    """Who supplies a field decides whether the caller types it."""

    def test_a_user_supplied_key_is_a_positional(self):
        entity = _entity(key=[_key("type")], operations=ALL_VERBS)
        get = next(c for c in entity_shell_commands(entity) if c["command"] == "get")
        assert get["keys"][0]["is_user"] is True

    def test_a_minted_key_is_marked_so_the_command_mints_it(self):
        entity = _entity(
            key=[_key("id", "boost::uuids::uuid", is_minted=True)],
            operations=ALL_VERBS)
        get = next(c for c in entity_shell_commands(entity) if c["command"] == "get")
        assert get["keys"][0]["is_minted"] is True
        assert get["keys"][0]["is_user"] is False

    def test_a_session_party_field_is_marked_so_it_comes_from_the_session(self):
        entity = _entity(
            key=[_key("type")],
            columns=[_column("party_id", "boost::uuids::uuid",
                             is_session_party=True)],
            writes=[_write("type"), _write("party_id", "boost::uuids::uuid")],
            operations=ALL_VERBS)
        add = next(c for c in entity_shell_commands(entity) if c["command"] == "add")
        party = next(f for f in add["writes"] if f["name"] == "party_id")
        assert party["is_session_party"] is True
        assert party["is_user"] is False


class TestTheTokenSetMatchesTheShell:
    """A type the projection calls fillable must be one from_token converts.

    The two sets drifted once: the derived requests carry std::uint32_t for a
    page and a version, and from_token refused every unsigned width, so an
    entity the projection accepted would not compile.
    """

    def test_an_integer_width_the_protocol_uses_is_fillable(self):
        entity = _entity(
            key=[_key("id")],
            writes=[_write("display_order", "std::uint32_t")],
            operations=ALL_VERBS)
        add = next(c for c in entity_shell_commands(entity) if c["command"] == "add")
        assert add["unsupported"] == []

    def test_a_timestamp_is_not_fillable_because_no_token_form_exists(self):
        entity = _entity(
            key=[_key("id")],
            writes=[_write("last_login", "std::chrono::system_clock::time_point")],
            operations=ALL_VERBS)
        add = next(c for c in entity_shell_commands(entity) if c["command"] == "add")
        assert add["unsupported"] == ["last_login"]
