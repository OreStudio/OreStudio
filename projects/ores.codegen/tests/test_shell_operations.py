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

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

from codegen.org_loader import (  # noqa: E402
    _reject_silent_shell_gap,
    load_org_operation_model,
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
        "boost::uuids::uuid",
        "std::chrono::system_clock::time_point",
        "std::vector<std::string>",
    ])
    def test_a_type_a_token_can_fill_is_fillable(self, cpp_type):
        commands = shell_command_projection([
            _message("op_request", "iam.v1.op", "op_response",
                     [_field("v", cpp_type)]),
        ])
        assert commands[0]["unsupported"] == []


class TestAuthentication:
    """An operation that establishes the session cannot present one."""

    def test_a_message_without_the_property_requires_a_session(self):
        commands = shell_command_projection([
            _message("logout_request", "iam.v1.auth.logout", "logout_response"),
        ])
        assert commands[0]["public"] is False

    def test_the_none_value_marks_a_pre_authentication_command(self):
        message = _message("login_request", "iam.v1.auth.login", "login_response")
        message["auth"] = "none"
        commands = shell_command_projection([message])
        assert commands[0]["public"] is True

    def test_the_value_is_read_whatever_its_case(self):
        message = _message("login_request", "iam.v1.auth.login", "login_response")
        message["auth"] = "NONE"
        assert shell_command_projection([message])[0]["public"] is True

    def test_another_value_is_refused_rather_than_read_as_authenticated(self):
        # A typo must not quietly produce a command that presents no token.
        message = _message("login_request", "iam.v1.auth.login", "login_response")
        message["auth"] = "optional"
        with pytest.raises(ValueError, match="unknown :auth:"):
            shell_command_projection([message])


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

    def test_the_pre_authentication_operations_are_exactly_the_session_establishing_ones(self):
        # The shell has one guard for the operations that act on a caller and
        # none for the operations that produce the caller's session, so this
        # set is what decides whether `login` can run at all.
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
        }
