"""Tests for the literate shell recipe a model's shell surface renders.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_shell_recipes.py

A recipe is a view of the generated command unit: one document per model, one
section per command, each section exporting its own script into the shell's
library. These cases pin what the document states, the sentinel invocation a
generated script sends, and the gate that keeps a recipe from existing without
the unit it documents.
"""
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    _sentinel_for_field,
    _sentinel_value,
    recipe_org_id,
    shell_menu_name,
    shell_recipe_document,
)

IAM_MODELING = REPO_ROOT / "projects/ores.iam/modeling"


def _entity_command(command, usage, invocation, **overrides):
    """A derived entity command, as `entity_shell_plan` projects one."""
    base = {
        "command": command,
        "usage": usage,
        "invocation": invocation,
        "kind": "paged",
        "verb": "list",
        "subject": "iam.v1.things.list",
        "request": "list_things_request",
        "response_type": "list_things_response",
    }
    base.update(overrides)
    return base


def _document(commands, **overrides):
    args = {
        "component": "iam",
        "menu": "tenant_types",
        "singular": "tenant_type",
        "plural": "tenant_types",
        "commands": commands,
        "is_operation": False,
    }
    args.update(overrides)
    return shell_recipe_document(**args)


class TestTheDocument:
    """One section per command, each naming the script it exports."""

    def test_a_command_becomes_a_section_and_a_script(self):
        doc = _document([_entity_command("list", "list", "list --limit 1")])
        section = doc["commands"][0]
        assert section["heading"] == "list"
        assert section["block"] == "tenant_types-list"
        assert section["script"] == "tenant_types-list.ores"

    def test_the_script_is_named_for_the_menu_and_the_command(self):
        # One menu holds one script per command, so the menu has to be in the
        # name: two entities both answering `list` would otherwise collide in
        # the library.
        doc = _document([
            _entity_command("list", "list", "list"),
            _entity_command("get", "get <id>", "get __none__"),
        ])
        assert [section["script"] for section in doc["commands"]] == [
            "tenant_types-list.ores", "tenant_types-get.ores"]

    def test_the_invocation_states_the_path_a_reader_types(self):
        # The unit's own help is written for a caller already inside the
        # submenu; a script loads at the root and names the whole path.
        doc = _document([_entity_command("get", "get <type>", "get __none__")])
        assert doc["commands"][0]["usage"] == "tenant_types get <type>"
        assert doc["commands"][0]["invocation"] == "tenant_types get __none__"

    def test_every_section_carries_a_stable_id(self):
        doc = _document([_entity_command("list", "list", "list")])
        assert doc["commands"][0]["id"] == recipe_org_id(
            "iam.tenant_types-list")
        again = _document([_entity_command("list", "list", "list")])
        assert again["commands"][0]["id"] == doc["commands"][0]["id"]

    def test_the_document_id_differs_from_its_sections(self):
        doc = _document([_entity_command("list", "list", "list")])
        assert doc["id"] != doc["commands"][0]["id"]

    def test_a_derived_command_states_its_verb_and_precondition(self):
        doc = _document([_entity_command(
            "add", "add <type>", "add __none__",
            kind="put", verb="put", precondition="must_not_exist",
            has_intent=True, allows_version=False)])
        commentary = doc["commands"][0]["commentary"]
        assert "must not exist" in commentary
        assert "reason and a commentary" in commentary

    def test_a_replace_says_what_differs_from_a_create(self):
        doc = _document([_entity_command(
            "set", "set <type>", "set __none__",
            kind="put", verb="put", precondition="any",
            has_intent=True, allows_version=True)])
        commentary = doc["commands"][0]["commentary"]
        assert "replaces" in commentary
        assert "--version" in commentary

    def test_a_declared_operation_states_no_verb(self):
        # An operation states no verb because what it does is the handler's
        # business, so the paragraph states the request and the reply instead.
        command = {
            "command": "assign-role",
            "usage": "assign-role <account_id>",
            "invocation": "assign-role __none__",
            "subject": "iam.v1.roles.assign",
            "request": "assign_role_request",
            "response_type": "assign_role_response",
            "positionals": [{"name": "account_id", "cpp_type": "std::string"}],
            "public": False,
        }
        doc = _document([command], menu="authorization", is_operation=True)
        commentary = doc["commands"][0]["commentary"]
        assert "assign_role_request" in commentary
        assert "=account_id=" in commentary
        assert "must have established a session" in commentary

    def test_a_public_operation_presents_no_token(self):
        command = {
            "command": "login",
            "usage": "login",
            "invocation": "login",
            "subject": "iam.v1.auth.login",
            "request": "login_request",
            "response_type": "login_response",
            "positionals": [],
            "public": True,
        }
        doc = _document([command], menu="login", is_operation=True)
        assert "before it has a session" in doc["commands"][0]["commentary"]

    def test_the_intro_states_where_the_scripts_go(self):
        doc = _document([_entity_command("list", "list", "list")])
        assert "projects/ores.shell/scripts/library/" in doc["intro"]


class TestTheSentinel:
    """A generated script must reach the service, not fail at the client."""

    def test_a_uuid_is_well_formed_and_addresses_nothing(self):
        assert _sentinel_value("boost::uuids::uuid") == (
            "00000000-0000-0000-0000-000000000000")

    def test_every_token_type_has_a_value(self):
        # The shell refuses a token it cannot parse, and a client-side refusal
        # proves nothing about the service. So every type the shell can read
        # needs a sentinel, and the fallback must be a value the strictest
        # parser accepts rather than an empty string.
        for cpp_type in ("std::string", "bool", "int", "std::int32_t",
                         "std::int64_t", "std::uint16_t", "std::uint32_t",
                         "std::uint64_t", "double", "boost::uuids::uuid",
                         "std::chrono::system_clock::time_point",
                         "boost::asio::ip::address"):
            value = _sentinel_value(cpp_type)
            assert value != "", cpp_type
        assert _sentinel_value("std::string") == "__none__"
        assert _sentinel_value("bool") == "false"
        assert _sentinel_value("int") == "0"

    def test_an_unknown_type_still_gets_a_value(self):
        assert _sentinel_value("some::unmapped::type") == "__none__"

    def test_an_intent_field_is_named_not_shaped(self):
        # A reason code is an enum the schema seeds, so a generic string
        # sentinel would be refused before the handler ran.
        assert _sentinel_for_field("reason_code", "std::string") == (
            "system.new_record")
        assert _sentinel_for_field("commentary", "std::string") == (
            "generated_script")
        assert _sentinel_for_field("type", "std::string") == "__none__"


class TestTheDestructiveMarker:
    """A command that destroys its environment says so in the model.

    Nothing about the shape of `reset reset-system` distinguishes it from a
    status read: both are a bare command name. A sweep of this library that
    replayed every generated script ran it and left the system unbootstrapped
    with its tenants hard-deleted, so the fact is declared and carried rather
    than inferred from the name.
    """

    def test_the_reset_model_declares_it(self):
        from codegen.core import load_model

        model = load_model(
            str(IAM_MODELING / "ores.iam.reset_messages.org"))
        marked = {m["name"] for m in model["operation"]["messages"]
                  if m.get("destructive")}
        assert "reset_system_command" in marked
        assert "reset_tenant_command" in marked

    def test_the_projection_carries_it(self):
        from codegen.core import load_model

        model = load_model(
            str(IAM_MODELING / "ores.iam.reset_messages.org"))
        commands = {c["command"]: c
                    for c in model["operation"]["shell_commands"]}
        assert commands["reset-system"]["is_destructive"] is True
        assert commands["reset-tenant"]["is_destructive"] is True

    def test_a_read_is_not_marked(self):
        from codegen.core import load_model

        model = load_model(
            str(IAM_MODELING / "ores.iam.authorization_messages.org"))
        commands = {c["command"]: c
                    for c in model["operation"]["shell_commands"]}
        assert commands["get-account-roles"]["is_destructive"] is False

    def test_the_document_warns_the_reader(self):
        command = {
            "command": "reset-system",
            "usage": "reset-system",
            "invocation": "reset-system",
            "subject": "iam.v1.system.reset",
            "request": "reset_system_command",
            "response_type": "reset_system_result",
            "positionals": [],
            "public": False,
            "is_destructive": True,
        }
        doc = _document([command], menu="reset", is_operation=True)
        assert doc["commands"][0]["is_destructive"] is True
        assert "destroys the system" in doc["commands"][0]["commentary"]

    def test_the_rendered_script_carries_a_warning(self):
        templates = REPO_ROOT / "projects/ores.codegen/library/templates"
        text = (templates / "ores.doc.shell-recipe.recipe.org").read_text()
        assert "DESTRUCTIVE" in text

    def test_a_value_other_than_true_is_refused(self, tmp_path):
        import pytest

        from codegen.org_loader import load_org_operation_model

        model = tmp_path / "probe.org"
        model.write_text(
            ":PROPERTIES:\n:ID: 1\n:END:\n"
            "#+title: probe\n#+type: ores.codegen.operation\n"
            "#+component: iam\n#+entity_singular: probe\n\n"
            "* Messages\n\n"
            "** do_thing_request\n:PROPERTIES:\n"
            ":subject: iam.v1.probe.do\n:response: do_thing_response\n"
            ":destructive: maybe\n:END:\n")
        with pytest.raises(ValueError, match="destructive"):
            load_org_operation_model(model)


class TestTheMenuName:
    """One rule, read by the path resolver and the renderer both."""

    def test_an_entity_answers_on_its_plural(self):
        model = {"domain_entity": {"entity_plural": "tenant_types",
                                   "entity_singular": "tenant_type"}}
        assert shell_menu_name("domain_entity", model) == "tenant_types"

    def test_a_junction_answers_on_its_own_name(self):
        # A junction states its plural as `name`: the table that links accounts
        # to parties is named for the links, not for either side.
        model = {"junction": {"name": "account_parties",
                              "name_singular": "account_party"}}
        assert shell_menu_name("junction", model) == "account_parties"

    def test_an_operation_answers_on_its_singular(self):
        model = {"operation": {"entity_singular": "authorization"}}
        assert shell_menu_name("operation", model) == "authorization"


class TestTheGate:
    """A recipe exists where its unit does, and nowhere else."""

    def test_the_recipe_archetype_requires_the_shell_facet(self):
        templates = REPO_ROOT / "projects/ores.codegen/library/templates"
        text = (templates / "ores.doc.shell-recipe.recipe.org").read_text()
        declared = [line for line in text.splitlines()
                    if line.startswith("#+requires_facet:")]
        assert declared
        assert "ores.cpp.shell-command" in declared[0]

    def test_the_recipe_facet_serves_every_shell_model_type(self):
        templates = REPO_ROOT / "projects/ores.codegen/library/templates"
        for name in ("ores.doc.shell-recipe.org",
                     "ores.doc.shell-recipe.recipe.org"):
            text = (templates / name).read_text()
            line = next(l for l in text.splitlines()
                        if l.startswith("#+model_types:"))
            for model_type in ("domain_entity", "junction", "operation"):
                assert model_type in line, (name, model_type)

    def test_a_model_without_the_shell_facet_renders_no_recipe(self):
        # tenant_type opts in; tenant_status's model is the control here only
        # insofar as it also opts in. The real control is a model that renders
        # a unit but not a recipe, which the render itself would show -- so
        # this pins the rule the gate implements rather than a specific model.
        import codegen.core as core

        captured = []
        original = core._RENDERER.render
        core._RENDERER.render = lambda t, d, *a, **k: (captured.append(d), "")[1]
        try:
            core.generate_from_model(
                str(IAM_MODELING / "ores.iam.tenant_type.org"),
                REPO_ROOT / "projects/ores.codegen/library/data",
                REPO_ROOT / "projects/ores.codegen/library/templates",
                REPO_ROOT / ".runtime/render/recipe",
                is_processing_batch=False,
                target_template="shell_recipe.org.mustache",
                target_output="probe.org")
        finally:
            core._RENDERER.render = original

        assert len(captured) == 1
        assert captured[0]["shell_recipe"]["menu"] == "tenant_types"

    def test_every_iam_model_with_a_shell_unit_renders_a_recipe(self):
        import codegen.core as core
        from codegen.generate import resolve_targets  # noqa: F401

        rendered, units = [], []
        original = core._RENDERER.render
        core._RENDERER.render = lambda t, d, *a, **k: (rendered.append(d), "")[1]
        try:
            for path in sorted(IAM_MODELING.glob("*.org")):
                got = []
                core._RENDERER.render = (
                    lambda t, d, *a, **k: (got.append(d), "")[1])
                try:
                    core.generate_from_model(
                        str(path),
                        REPO_ROOT / "projects/ores.codegen/library/data",
                        REPO_ROOT / "projects/ores.codegen/library/templates",
                        REPO_ROOT / ".runtime/render/recipe_all",
                        is_processing_batch=False,
                        target_template="cpp_shell_command_impl.cpp.mustache",
                        target_output="probe.cpp")
                except Exception:  # noqa: BLE001 - not every model is an entity
                    continue
                if got and got[0].get("domain_entity", {}).get("shell"):
                    units.append(path.name)
                if got:
                    got.clear()
                try:
                    core.generate_from_model(
                        str(path),
                        REPO_ROOT / "projects/ores.codegen/library/data",
                        REPO_ROOT / "projects/ores.codegen/library/templates",
                        REPO_ROOT / ".runtime/render/recipe_all2",
                        is_processing_batch=False,
                        target_template="shell_recipe.org.mustache",
                        target_output="probe.org")
                except Exception:  # noqa: BLE001
                    continue
                if got and "shell_recipe" in got[0]:
                    rendered.append(path.name)
        finally:
            core._RENDERER.render = original

        # Every model that rendered a shell unit also rendered a recipe.
        assert set(units) <= set(rendered), (
            sorted(set(units) - set(rendered)))
        assert len(units) >= 11, f"only {len(units)} models rendered a unit"
