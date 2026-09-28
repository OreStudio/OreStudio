"""Tests for the shell's view of a declared protocol.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_shell_operations.py

An operation model already states its protocol: a subject, a request, a
response, and each field's own type. The shell facet renders from that same
declaration instead of a hand-written command per message, so these cases pin
what a command is made of -- which messages become commands at all, what the
command is called, which fields a caller types and which arrive as flags, and
the one thing the facet refuses rather than drops.
"""
import sys
from pathlib import Path

import pytest
import uuid

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    _SHELL_TOKEN_LITERALS,
    _SHELL_TOKEN_TYPES,
    _reject_silent_shell_gap,
    _shell_field,
    entity_protocol_messages,
    load_org_operation_model,
    parse_declared_messages,
    parse_org,
    shell_command_name,
    shell_command_projection,
)

IAM_MODELING = REPO_ROOT / "projects/ores.iam/modeling"


def _message(name, subject=None, response=None, fields=None):
    message = {"name": name, "fields": fields or []}
    if subject:
        message["subject"] = subject
    if response:
        message["response_type"] = response
    return message


def _field(name, cpp_type, default=None):
    field = {"name": name, "cpp_type": cpp_type}
    if default is not None:
        field["default"] = default
    return field


class TestCommandName:
    """The command a message becomes."""

    def test_the_artefact_suffix_is_dropped(self):
        assert shell_command_name("save_account_request") == "save-account"

    def test_a_command_suffixed_message_loses_that_suffix_too(self):
        assert shell_command_name("reset_system_command") == "reset-system"

    @pytest.mark.parametrize("name", [
        "get_accounts_request",
        "get_accounts_request_typed",
    ])
    def test_a_typed_message_names_the_same_command_as_its_plain_form(self, name):
        # The suffixed spelling declares the same operation, so it must not
        # publish a second command that sends to the same subject.
        assert shell_command_name(name) == "get-accounts"

    def test_a_message_that_is_all_suffix_keeps_the_suffix(self):
        # Dropping every word would emit an empty command name, which the menu
        # cannot register.
        assert shell_command_name("request") == "request"


class TestWhichMessagesBecomeCommands:
    """Only a message with somewhere to go and something to show."""

    def test_a_subject_and_a_response_make_a_command(self):
        commands = shell_command_projection([
            _message("login_request", "iam.v1.auth.login", "login_response"),
        ])
        assert [c["command"] for c in commands] == ["login"]

    def test_a_payload_struct_with_no_subject_is_not_a_command(self):
        commands = shell_command_projection([
            _message("session_view", fields=[_field("username", "std::string")]),
        ])
        assert commands == []

    def test_a_request_with_no_response_is_not_a_command(self):
        # Nothing would be printed, so a command would look like it failed.
        commands = shell_command_projection([
            _message("public_key_request", "iam.v1.auth.public-key"),
        ])
        assert commands == []

    def test_the_declared_order_is_the_registered_order(self):
        commands = shell_command_projection([
            _message("logout_request", "iam.v1.auth.logout", "logout_response"),
            _message("login_request", "iam.v1.auth.login", "login_response"),
        ])
        assert [c["command"] for c in commands] == ["logout", "login"]


class TestHowFieldsArrive:
    """A field without a default is typed; one with a default is a flag."""

    def test_a_field_with_no_default_is_a_positional(self):
        commands = shell_command_projection([
            _message("login_request", "iam.v1.auth.login", "login_response",
                     [_field("principal", "std::string")]),
        ])
        assert [f["name"] for f in commands[0]["positionals"]] == ["principal"]
        assert commands[0]["flags"] == []
        assert commands[0]["usage"] == "login <principal>"

    def test_a_defaulted_field_is_a_flag_so_the_struct_default_stands(self):
        commands = shell_command_projection([
            _message("list_sessions_request", "iam.v1.sessions.list",
                     "list_sessions_response", [
                         _field("account_id", "std::string"),
                         _field("limit", "int", default="50"),
                     ]),
        ])
        assert [f["name"] for f in commands[0]["positionals"]] == ["account_id"]
        assert [f["name"] for f in commands[0]["flags"]] == ["limit"]
        assert commands[0]["usage"] == "list-sessions <account_id> [--limit <v>]"

    def test_a_list_field_is_filled_from_one_token(self):
        commands = shell_command_projection([
            _message("lock_account_request", "iam.v1.accounts.lock",
                     "lock_account_response",
                     [_field("account_ids", "std::vector<std::string>")]),
        ])
        assert commands[0]["positionals"][0]["is_list"]

    def test_the_required_count_is_the_positional_count(self):
        commands = shell_command_projection([
            _message("create_initial_admin_request", "iam.v1.bootstrap.create-admin",
                     "create_initial_admin_response", [
                         _field("principal", "std::string"),
                         _field("password", "std::string"),
                         _field("email", "std::string"),
                     ]),
        ])
        assert commands[0]["positional_count"] == 3

    @pytest.mark.parametrize("cpp_type", [
        "std::string",
        "bool",
        "int",
        "std::uint32_t",
        "std::uint64_t",
        "boost::uuids::uuid", "std::uint16_t", "double",
        "std::vector<std::string>",
    ])
    def test_a_type_a_token_can_fill_is_fillable(self, cpp_type):
        commands = shell_command_projection([
            _message("op_request", "iam.v1.op", "op_response",
                     [_field("v", cpp_type)]),
        ])
        assert commands[0]["unsupported"] == []


class TestTokenLiterals:
    """A generated test's token must be one the command can parse.

    The failure these pin: every positional was handed the same string, so a
    uuid or a number threw inside from_token before the command reached the
    transport, and the test asserted a failure the command never produced.
    """

    @pytest.mark.parametrize("cpp_type,expected", [
        ("std::string", "sample"),
        ("bool", "true"),
        ("int", "1"),
        ("std::int64_t", "1"),
        ("std::uint32_t", "1"),
        ("double", "1.0"),
        ("boost::uuids::uuid", "00000000-0000-0000-0000-000000000001"),
    ])
    def test_the_literal_is_spelled_for_the_type(self, cpp_type, expected):
        assert _shell_field(_field("v", cpp_type))["token_literal"] == expected

    def test_a_list_field_is_filled_from_one_comma_separated_token(self):
        assert _shell_field(_field("v", "std::vector<std::string>"))["token_literal"] == "sample"

    def test_every_type_a_token_can_fill_has_a_literal(self):
        # The structural guarantee: the fallback is never reached for a type
        # the facet claims a token can fill, because reaching it is the defect.
        missing = sorted(_SHELL_TOKEN_TYPES - set(_SHELL_TOKEN_LITERALS))
        assert missing == []

    def test_the_uuid_literal_is_one_a_command_accepts(self):
        literal = _shell_field(_field("v", "boost::uuids::uuid"))["token_literal"]
        assert str(uuid.UUID(literal)) == literal

    def test_a_type_outside_the_table_keeps_the_string_spelling(self):
        assert _shell_field(_field("v", "std::chrono::year_month_day"))["token_literal"] == "sample"


class TestAuthentication:
    """An operation that establishes the session cannot present one."""

    def test_a_message_without_the_property_requires_a_session(self):
        commands = shell_command_projection([
            _message("logout_request", "iam.v1.auth.logout", "logout_response"),
        ])
        assert commands[0]["public"] is False

    def test_the_model_states_it_and_the_command_reads_it(self):
        # The projection reads the message's own fact rather than a second
        # derivation, so the shell and the protocol cannot disagree.
        message = _message("login_request", "iam.v1.auth.login", "login_response")
        message["requires_session"] = "false"
        commands = shell_command_projection([message])
        assert commands[0]["public"] is True

    def test_a_message_without_the_fact_requires_a_session(self):
        message = _message("logout_request", "iam.v1.auth.logout", "logout_response")
        message["requires_session"] = "true"
        assert shell_command_projection([message])[0]["public"] is False


def _declared(org_text):
    """The declared messages of a model body, through the real parser."""
    return parse_declared_messages(parse_org(org_text).root)


def _model_body(auth_line):
    return f"""* Messages

** login_request
:PROPERTIES:
:subject: iam.v1.auth.login
:response: login_response
{auth_line}:END:

*** principal
:PROPERTIES:
:cpp_type: std::string
:END:
"""


class TestTheProtocolsOwnStatement:
    """The fact is the protocol's, and both twins carry it."""

    def test_a_message_without_the_property_requires_a_session(self):
        messages = _declared(_model_body(""))
        assert messages[0]["requires_session"] == "true"

    def test_the_none_value_marks_a_pre_authentication_message(self):
        messages = _declared(_model_body(":auth: none\n"))
        assert messages[0]["requires_session"] == "false"

    def test_the_value_is_read_whatever_its_case(self):
        messages = _declared(_model_body(":auth: NONE\n"))
        assert messages[0]["requires_session"] == "false"

    def test_another_value_is_refused(self):
        # A typo must not quietly produce a message that presents no token.
        with pytest.raises(ValueError, match="only value is 'none'"):
            _declared(_model_body(":auth: optional\n"))

    def test_a_payload_struct_carries_no_fact_because_it_addresses_nothing(self):
        body = """* Messages

** session_view

*** username
:PROPERTIES:
:cpp_type: std::string
:END:
"""
        assert "requires_session" not in _declared(body)[0]

    def test_every_derived_entity_operation_requires_a_session(self):
        # The derivation has no unauthenticated verb, so the default is the
        # only correct answer for the set it produces.
        entity = {
            "component": "iam",
            "entity_singular": "tenant_type",
            "entity_plural": "tenant_types",
            "has_audit_columns": True,
            "primary_key": {
                "column": "type",
                "columns": [{"column": "type", "cpp_type": "std::string"}],
            },
            "columns": [],
        }
        addressed = [m for m in entity_protocol_messages(entity) if m.get("subject")]
        assert addressed, "the derivation produced no operation"
        assert {m["requires_session"] for m in addressed} == {"true"}


class TestTheDeclaredTimeout:
    """A command whose work outlives the transport default states its budget."""

    def _body(self, timeout_line):
        return f"""* Messages

** op_request
:PROPERTIES:
:subject: iam.v1.op
:response: op_response
{timeout_line}:END:
"""

    def test_a_message_without_the_property_declares_no_budget(self):
        assert "request_timeout_seconds" not in _declared(_model_body(""))[0]

    def test_the_model_states_it_and_the_command_reads_it(self):
        messages = _declared(self._body(":request_timeout_seconds: 1800\n"))
        assert messages[0]["request_timeout_seconds"] == 1800
        assert shell_command_projection(messages)[0]["request_timeout_seconds"] == 1800

    def test_a_command_without_a_budget_projects_none(self):
        command = shell_command_projection([
            _message("op_request", "iam.v1.op", "op_response"),
        ])[0]
        assert not command["request_timeout_seconds"]

    @pytest.mark.parametrize("value", ["soon", "0", "-5", "1800.5"])
    def test_a_budget_that_is_not_a_positive_whole_number_is_refused(self, value):
        # A typo must not quietly leave the command on the transport default.
        with pytest.raises(ValueError, match="positive whole number"):
            _declared(self._body(f":request_timeout_seconds: {value}\n"))

    def test_a_budget_on_an_unauthenticated_request_is_refused(self):
        # Only an authenticated request carries a token, and only that call
        # has a timeout parameter to pass.
        with pytest.raises(ValueError, match="timeout the model can raise"):
            _declared(self._body(
                ":request_timeout_seconds: 1800\n:auth: none\n"))


class TestTheRefusal:
    """A field no token can fill fails the model rather than vanishing."""
    def _command_with(self, cpp_type):
        return shell_command_projection([
            _message("op_request", "iam.v1.op", "op_response",
                     [_field("v", cpp_type)]),
        ])

    def test_an_opted_in_model_with_an_unfillable_field_is_refused(self):
        with pytest.raises(ValueError, match="no shell token form"):
            _reject_silent_shell_gap(
                "ores.iam.op_messages.org", self._command_with("std::vector<int>"),
                {"ores.cpp.shell-command.enabled": "true"},
            )

    def test_a_model_that_did_not_opt_in_is_left_alone(self):
        # The projection is computed for every operation model, and most of
        # them render no unit at all.
        _reject_silent_shell_gap(
            "ores.iam.op_messages.org", self._command_with("std::vector<int>"), {},
        )

    def test_a_model_that_opted_out_is_left_alone(self):
        _reject_silent_shell_gap(
            "ores.iam.op_messages.org", self._command_with("std::vector<int>"),
            {"ores.cpp.shell-command.enabled": "nil"},
        )

    def test_a_fillable_model_is_accepted(self):
        _reject_silent_shell_gap(
            "ores.iam.op_messages.org", self._command_with("std::string"),
            {"ores.cpp.shell-command.enabled": "true"},
        )


class TestTheRealModels:
    """The IAM operation models the shell facet is enabled on."""

    def _operation(self, filename):
        return load_org_operation_model(IAM_MODELING / filename)["operation"]

    def test_every_iam_operation_model_projects_without_a_gap(self):
        # The gate the facet relies on: a model may only opt in when every
        # field of every command has a token form.
        for path in sorted(IAM_MODELING.glob("*.org")):
            try:
                operation = load_org_operation_model(path)["operation"]
            except ValueError:
                # Not an operation model, or one the TypeScript gate refuses.
                continue
            unsupported = [
                f"{command['command']}.{name}"
                for command in operation["shell_commands"]
                for name in command["unsupported"]
            ]
            assert unsupported == [], f"{path.name}: {unsupported}"

    def test_a_session_samples_command_is_one_key_and_no_flags(self):
        operation = self._operation("ores.iam.session_samples_messages.org")
        assert [c["command"] for c in operation["shell_commands"]] == \
            ["get-session-samples"]
        command = operation["shell_commands"][0]
        assert command["positional_count"] == 1
        assert command["flags"] == []
        assert command["subject"] == "iam.v1.sessions.samples"
        assert operation["shell_command_count"] == 1

    def test_the_helper_flags_track_the_fields(self):
        # session_samples declares only a string, so neither helper is emitted.
        operation = self._operation("ores.iam.session_samples_messages.org")
        assert operation["shell_has_helpers"] is False
        # account declares a vector and a defaulted int, so both are.
        account = self._operation("ores.iam.account_messages.org")
        assert account["shell_has_list"] is True

    def test_the_public_operations_are_exactly_the_ones_the_sign_in_needs(self):
        # A caller with no session may run these and nothing else. Most of
        # them establish the session, but not all do: bootstrap-status asks
        # whether the environment holds an administrator yet, and the password
        # policy states the rules the first administrator's password must
        # satisfy. The sign-in screen reads both before anyone holds a token,
        # so an operation that only tells the caller what the sign-in demands
        # belongs here, while one that acts on an account does not.
        public = set()
        for path in sorted(IAM_MODELING.glob("*.org")):
            try:
                operation = load_org_operation_model(path)["operation"]
            except ValueError:
                continue
            public.update(
                command["command"] for command in operation["shell_commands"]
                if command["public"]
            )
        assert public == {
            "login", "service-login", "signup",
            "bootstrap-status", "create-initial-admin", "provision-tenant",
            "get-password-policy",
        }

    def test_the_one_command_that_declares_a_budget_is_acme_provisioning(self):
        # Measured, not assumed: provision-acme is the command whose work
        # outlives the transport's default request timeout, and the model now
        # states the budget it needs beside the subject it sends to.
        declared = {}
        for path in sorted(IAM_MODELING.glob("*.org")):
            try:
                operation = load_org_operation_model(path)["operation"]
            except ValueError:
                continue
            for command in operation["shell_commands"]:
                if command["request_timeout_seconds"]:
                    declared[command["command"]] = command["request_timeout_seconds"]
        assert declared == {"provision-acme-tenant": 1800}

    def test_the_budget_is_rendered_into_the_unit_that_uses_it(self):
        # The unit includes <chrono> and passes the budget only because the
        # model declares it; a unit with no budget renders neither.
        operation = self._operation("ores.iam.tenant_provisioning_messages.org")
        assert operation["shell_has_request_timeout"] is True
        assert self._operation("ores.iam.account_messages.org")[
            "shell_has_request_timeout"] is False
