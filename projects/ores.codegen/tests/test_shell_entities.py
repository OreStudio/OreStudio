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

from codegen.org_loader import entity_shell_commands, entity_shell_plan  # noqa: E402

IAM_MODELING = REPO_ROOT / "projects/ores.iam/modeling"


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

    def test_a_scoped_read_is_a_command_of_its_own(self):
        # The derivation states a scoped read's verb as `list_scoped` and names
        # the relation in `leading`, so the command is built from that.
        commands = entity_shell_commands(_entity(operations=[
            _operation("list_scoped", "list_by_account_id_x_request",
                       "list_by_account_id_x_response",
                       "iam.v1.x.list_by_account_id", has_order=True,
                       leading="account_id"),
        ]))
        assert _names(commands) == ["by-account-id"]
        assert commands[0]["kind"] == "list_by"
        assert commands[0]["usage"].startswith("by-account-id <account_id>")

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
            writes=[_write("samples", "std::vector<std::int64_t>")],
            operations=ALL_VERBS)
        add = next(c for c in entity_shell_commands(entity) if c["command"] == "add")
        assert add["unsupported"] == ["samples"]

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

    @pytest.mark.parametrize("cpp_type", [
        "std::chrono::system_clock::time_point", "boost::asio::ip::address",
    ])
    def test_a_type_with_a_text_form_but_no_lexical_cast_is_fillable(self, cpp_type):
        entity = _entity(
            key=[_key("id")], writes=[_write("v", cpp_type)], operations=ALL_VERBS)
        add = next(c for c in entity_shell_commands(entity) if c["command"] == "add")
        assert add["unsupported"] == []

    def test_a_container_the_helpers_cannot_fill_is_reported(self):
        entity = _entity(
            key=[_key("id")], writes=[_write("v", "std::vector<int>")],
            operations=ALL_VERBS)
        add = next(c for c in entity_shell_commands(entity) if c["command"] == "add")
        assert add["unsupported"] == ["v"]


class TestThatNoVerbIsSkipped:
    """A verb with no shape must be reported, not silently dropped.

    The projection builds a command per operation it recognises. An operation
    it does not recognise would simply produce nothing, and the entity would
    answer fewer verbs from the shell than it derives, with no failure anywhere.
    """

    def test_a_known_verb_set_leaves_nothing_uncovered(self):
        plan = entity_shell_plan(_entity(operations=ALL_VERBS))
        assert plan["uncovered_verbs"] == []

    def test_a_verb_with_no_shape_is_named(self):
        plan = entity_shell_plan(_entity(operations=[
            _operation("archive", "archive_x_request", "archive_x_response",
                       "iam.v1.x.archive"),
        ]))
        assert plan["uncovered_verbs"] == ["archive"]
        assert plan["commands"] == []

    def test_a_scoped_read_is_covered_by_its_own_shape(self):
        plan = entity_shell_plan(_entity(operations=[
            _operation("list_scoped", "list_by_account_id_x_request",
                       "list_by_account_id_x_response",
                       "iam.v1.x.list_by_account_id",
                       leading="account_id"),
        ]))
        assert plan["uncovered_verbs"] == []

    def test_every_iam_entity_that_renders_a_unit_covers_every_verb(self):
        # The gate the pilot is for: if a model gains a verb the shell cannot
        # address, this fails rather than the command quietly not existing.
        import codegen.core as core
        from codegen.org_loader import entity_shell_plan

        captured = []
        original = core._RENDERER.render
        core._RENDERER.render = lambda t, d, *a, **k: (captured.append(d), "")[1]
        try:
            for path in sorted(IAM_MODELING.glob("*.org")):
                try:
                    core.generate_from_model(
                        str(path), REPO_ROOT / "projects/ores.codegen/library/data",
                        REPO_ROOT / "projects/ores.codegen/library/templates",
                        REPO_ROOT / ".runtime/render/coverage",
                        is_processing_batch=False,
                        target_template="cpp_shell_command_impl.cpp.mustache",
                        target_output="probe.cpp")
                except Exception:  # noqa: BLE001 - not every model is an entity
                    continue
        finally:
            core._RENDERER.render = original

        checked = 0
        for data in captured:
            entity = data.get("domain_entity")
            if not entity or not entity.get("shell", {}).get("commands"):
                continue
            checked += 1
            plan = entity_shell_plan(entity)
            assert plan["uncovered_verbs"] == [], (
                f"{entity.get('entity_singular')}: {plan['uncovered_verbs']}")
        assert checked >= 7, f"only {checked} entities rendered a shell unit"


def _junction(read_only=False):
    """An account_party-shaped junction, as the loader hands it to the shape."""
    junction = {
        "component": "iam",
        "name": "account_parties",
        "name_singular": "account_party",
        "left": {"column": "account_id", "cpp_type": "boost::uuids::uuid",
                 "type": "uuid", "list_by": True},
        "right": {"column": "party_id", "cpp_type": "boost::uuids::uuid",
                  "type": "uuid"},
        "columns": [],
    }
    if read_only:
        junction["read_only"] = True
    return junction


def _junction_plan(read_only=False):
    """The shape and shell plan core.py assembles for a junction.

    Mirrors that assembly rather than importing it, so a key the assembly
    stops setting shows up here as a failure rather than as agreement.
    """
    from codegen.org_loader import (
        junction_entity_shape,
        junction_protocol_messages,
        protocol_operations,
        write_record_for,
    )

    junction = _junction(read_only=read_only)
    shape = junction_entity_shape(junction)
    shape["messages"] = junction_protocol_messages(junction)
    shape["write_fields"] = write_record_for(shape)
    shape["operations"] = protocol_operations(shape["messages"])
    return shape, entity_shell_plan(shape)


class TestTheJunctionUnit:
    """A junction is an entity, so it renders the same unit an entity does.

    account_party derived eight subjects while its shell offered two by hand,
    so six verbs had no shell surface at all -- including both that remove an
    association. The unit exists because the derivation built an entity shape
    for the protocol and then threw it away; these cases pin the projection
    that reads it, and the archetype gate that let it render nothing at all
    without failing.
    """

    def test_the_plural_is_the_junctions_own_name(self):
        from codegen.org_loader import junction_entity_shape

        shape = junction_entity_shape(_junction())
        assert shape["entity_singular"] == "account_party"
        assert shape["entity_plural"] == "account_parties"

    def test_the_key_carries_the_whole_pair(self):
        from codegen.org_loader import junction_entity_shape

        columns = [c["column"] for c in
                   junction_entity_shape(_junction())["primary_key"]["columns"]]
        assert columns == ["account_id", "party_id"]

    def test_every_derived_verb_becomes_a_command(self):
        _, plan = _junction_plan()
        assert plan["uncovered_verbs"] == []
        assert _names(plan["commands"]) == [
            "list", "get", "get-many", "add", "set",
            "put-many", "delete", "delete-many", "by-account-id",
        ]

    def test_the_shell_sends_to_the_subjects_the_protocol_states(self):
        shape, plan = _junction_plan()
        declared = {m["subject"] for m in shape["messages"] if m.get("subject")}
        assert {c["subject"] for c in plan["commands"]} <= declared

    def test_a_read_only_junction_derives_the_reads_only(self):
        shape, plan = _junction_plan(read_only=True)
        assert "delete" not in _names(plan["commands"])
        assert plan["uncovered_verbs"] == []

    def test_the_junction_derives_no_versions_to_read(self):
        # A junction links rows and carries no valid_from/valid_to axis, so it
        # states no versions sub-resource. Its writes still carry the version
        # precondition a compare-and-swap needs, which is a different fact.
        shape, plan = _junction_plan()
        assert not [m for m in shape["messages"] if "versions" in m["name"]]
        assert {c["kind"] for c in plan["commands"]}.isdisjoint(
            {"versions", "version_read"})
        assert plan["has_version"] is True

    def test_account_parties_renders_a_unit_covering_every_verb(self):
        import codegen.core as core

        captured = []
        original = core._RENDERER.render
        core._RENDERER.render = lambda t, d, *a, **k: (captured.append(d), "")[1]
        try:
            core.generate_from_model(
                str(IAM_MODELING / "ores.iam.account_party_junction.org"),
                REPO_ROOT / "projects/ores.codegen/library/data",
                REPO_ROOT / "projects/ores.codegen/library/templates",
                REPO_ROOT / ".runtime/render/junction",
                is_processing_batch=False,
                target_template="cpp_shell_command_impl.cpp.mustache",
                target_output="probe.cpp")
        finally:
            core._RENDERER.render = original

        assert len(captured) == 1
        plan = captured[0]["domain_entity"]["shell"]
        assert plan["uncovered_verbs"] == []
        assert plan["command_count"] == 9

    def test_the_shell_archetypes_admit_a_junction(self):
        # The gate that hid the gap: the projection was ready and the facet
        # still logged "nothing to generate ... model type 'junction'".
        templates = REPO_ROOT / "projects/ores.codegen/library/templates"
        for name in ("ores.cpp.shell-command.org",
                     "ores.cpp.shell-command.command_header.org",
                     "ores.cpp.shell-command.command_implementation.org",
                     "ores.cpp.shell-command.command_tests.org"):
            text = (templates / name).read_text()
            declared = [line for line in text.splitlines()
                        if line.startswith("#+model_types:")]
            assert declared, name
            assert "junction" in declared[0], name

