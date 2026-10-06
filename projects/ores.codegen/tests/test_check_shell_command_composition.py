"""Tests that the shell command composition check finds a generated unit nothing
registers, and passes when some call site registers every one.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_check_shell_command_composition.py
"""
import importlib.util
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SCRIPT = REPO_ROOT / "projects/ores.codegen/scripts/check_shell_command_composition.py"

spec = importlib.util.spec_from_file_location("check_shell_commands", SCRIPT)
check = importlib.util.module_from_spec(spec)
spec.loader.exec_module(check)

HEADER = """/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_shell_command_header.hpp.mustache
 */
#ifndef ORES_SHELL_APP_COMMANDS_REFDATA_ALPHA_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_REFDATA_ALPHA_COMMANDS_HPP

namespace ores::shell::app::commands {

class alpha_commands {
public:
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);
};

}

#endif
"""


def shell_with_unit(tmp_path, generated: bool = True) -> Path:
    """A shell tree holding one alpha unit header, generated or hand-written."""
    shell = tmp_path / "ores.shell"
    include = shell / "refdata/include/ores.shell/app/commands/refdata"
    include.mkdir(parents=True)
    text = HEADER if generated else HEADER.replace("AUTO-GENERATED FILE", "hand written")
    (include / "alpha_commands.hpp").write_text(text, encoding="utf-8")
    return shell


def call_sites(tmp_path, calls: str) -> Path:
    """An application source tree holding the given registration calls."""
    root = tmp_path / "application/src"
    root.mkdir(parents=True)
    (root / "composite.cpp").write_text(calls, encoding="utf-8")
    return root


def test_a_unit_nothing_registers_is_reported(tmp_path):
    composed = check.composed_units(call_sites(tmp_path, "int main() { return 0; }\n"))
    failure = check.check_component("refdata", composed, shell_with_unit(tmp_path))
    assert failure is not None
    assert "alpha_commands" in failure and "not registered" in failure


def test_a_registered_unit_passes(tmp_path):
    composed = check.composed_units(
        call_sites(tmp_path, "alpha_commands::register_commands(root, session);\n")
    )
    assert check.check_component("refdata", composed, shell_with_unit(tmp_path)) is None


def test_a_qualified_registration_call_counts(tmp_path):
    composed = check.composed_units(
        call_sites(
            tmp_path,
            "ores::shell::app::commands::alpha_commands::register_commands(*root, session_);\n",
        )
    )
    assert check.check_component("refdata", composed, shell_with_unit(tmp_path)) is None


def test_a_hand_written_unit_is_not_required(tmp_path):
    composed = check.composed_units(call_sites(tmp_path, "int main() { return 0; }\n"))
    shell = shell_with_unit(tmp_path, generated=False)
    assert check.check_component("refdata", composed, shell) is None


def test_a_component_with_no_units_is_not_reported(tmp_path):
    shell = tmp_path / "ores.shell"
    (shell / "empty/include").mkdir(parents=True)
    assert check.check_component("empty", set(), shell) is None


def test_the_tree_registers_every_unit_of_a_checked_component():
    assert check.main() == 0
