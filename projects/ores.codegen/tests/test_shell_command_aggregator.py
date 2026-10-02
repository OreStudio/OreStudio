"""Tests for the shell component registrar the aggregator archetypes emit.

Run::

    python3 -m pytest projects/ores.codegen/tests/test_shell_command_aggregator.py

The registrar is the one shell file the host calls, and it lists a
component's command units. The archetype holds no list of its own: the
generator scans the component's modeling directory for the units that opted
in, merges the model-less units the component's shell surface declares, and
injects both. These cases pin the properties that list must have -- it
lists every opted-in model and every declared local unit, and nothing else,
in unit-name order, with the right call arity for each kind -- plus the
shape of the header the host includes and the component-level opt-in that
decides whether the registrar renders at all.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects/ores.codegen/src"))

import codegen.core as core  # noqa: E402
from codegen.generate import resolve_targets  # noqa: E402

TRADING_MODELING = REPO_ROOT / "projects/ores.trading/modeling"
TEMPLATES = REPO_ROOT / "projects/ores.codegen/library/templates"
DATA = REPO_ROOT / "projects/ores.codegen/library/data"
CODEGEN_BASE = REPO_ROOT / "projects/ores.codegen"

# The shell surface of the trading component, which declares the generated
# registrar and the units no model describes.
TRADING_SHELL_OVERVIEW = (
    REPO_ROOT / "projects/ores.shell/trading/modeling/component_overview.org"
)
# A component whose entities opt into the facet but which has not adopted
# the generated registrar, so its per-entity opt-in must not rewrite it.
DQ_MODELING = REPO_ROOT / "projects/ores.dq/modeling"

AGGREGATOR_TEMPLATES = frozenset({
    "cpp_shell_command_aggregator_header.hpp.mustache",
    "cpp_shell_command_aggregator_impl.cpp.mustache",
})

# A model opts in through its file-level properties drawer; the registry
# states that with one literal line, so this reads the same line rather than
# the generator's resolution of it. That keeps the expected set independent
# of the code the test checks.
_OPT_IN = ":ores.cpp.shell-command.enabled: true"
_LOCAL_UNITS_KEY = "#+shell_command_local_units:"
_AGGREGATOR_KEY = "#+shell_command_aggregator:"

# One call per unit, and every unit reads the root menu and the session:
# the generated list verb spells paging as flags, so no unit asks for the
# shell's pagination state. Reading the whole call lets the pattern
# tolerate a clang-format line wrap, which a line-anchored one would not.
# The captured name is the unit's full class stem, which already ends in
# _commands.
_CALL = re.compile(
    r"\b(\w+_commands)::register_commands\(\s*root_menu,\s*session\s*\);")

# A model that renders a command unit for another component's shell part but
# does not opt in to the trading registrar's list.
_NOT_OPTED_IN = "ascot"

# The two trading units the hand-written application-local pairs became.
_NEWLY_GENERATED = {
    "commodity_instrument_commands",
    "equity_position_option_underlying_commands",
}


def _opted_in_trading_units():
    """Trading entity and junction names whose drawer opts into the facet."""
    names = []
    for path in sorted(TRADING_MODELING.glob("*.org")):
        text = path.read_text(encoding="utf-8")
        head = text.split("#+title:", 1)[0]
        if _OPT_IN not in head:
            continue
        if "#+type: ores.codegen.entity" not in text and \
                "#+type: ores.codegen.junction" not in text:
            continue
        singular = re.search(r"^#\+entity_singular:\s*(\S+)", text,
                             re.MULTILINE)
        if singular:
            names.append(f"{singular.group(1)}_commands")
    return sorted(names)


def _local_trading_units():
    """The local units the trading shell surface declares, read literally."""
    for line in TRADING_SHELL_OVERVIEW.read_text(encoding="utf-8").splitlines():
        if line.startswith(_LOCAL_UNITS_KEY):
            return line[len(_LOCAL_UNITS_KEY):].split()
    return []


def _registration_calls(text):
    """(generated names, local names) in the order the registrar calls them.

    The two kinds are told apart by the component's declaration, not by the
    call's arity: every call has the same shape now, so arity no longer says
    which unit a model produced.
    """
    declared = set(_local_trading_units())
    generated, local = [], []
    for name in _CALL.findall(text):
        (local if name in declared else generated).append(name)
    return generated, local


def _render(tmp_path, model_name, template, output_name):
    core.generate_from_model(
        str(TRADING_MODELING / model_name),
        DATA, TEMPLATES, tmp_path,
        is_processing_batch=False,
        target_template=template,
        target_output=output_name,
    )
    return (tmp_path / output_name).read_text(encoding="utf-8")


class TestTheRegistrar:
    """One call per unit, in unit-name order, and nothing else."""

    def test_the_aggregator_lists_every_opted_in_trading_unit(self, tmp_path):
        out = _render(
            tmp_path, "ores.trading.activity_category.org",
            "cpp_shell_command_aggregator_impl.cpp.mustache",
            "trading_commands.cpp")

        generated, _ = _registration_calls(out)
        expected = _opted_in_trading_units()

        assert expected, "no trading model opts in; the fixture is stale"
        # A dropped opt-in is the regression: the menu would lose its unit.
        assert set(expected) <= set(generated), \
            sorted(set(expected) - set(generated))
        # The scan adds nothing an opt-in did not ask for.
        assert set(generated) <= set(expected), \
            sorted(set(generated) - set(expected))

    def test_the_two_replaced_local_units_are_generated(self, tmp_path):
        out = _render(
            tmp_path, "ores.trading.activity_category.org",
            "cpp_shell_command_aggregator_impl.cpp.mustache",
            "trading_commands.cpp")

        generated, local = _registration_calls(out)
        missing = _NEWLY_GENERATED - set(generated)
        assert not missing, (
            "the units that replaced the hand-written application-local "
            f"pairs must be generated, not dropped: {sorted(missing)}")
        assert not (_NEWLY_GENERATED & set(local))

    def test_the_local_units_are_listed(self, tmp_path):
        out = _render(
            tmp_path, "ores.trading.activity_category.org",
            "cpp_shell_command_aggregator_impl.cpp.mustache",
            "trading_commands.cpp")

        _, local = _registration_calls(out)
        assert local == sorted(_local_trading_units())
        assert "ore_commands" in local

    def test_the_calls_are_in_unit_name_order(self, tmp_path):
        out = _render(
            tmp_path, "ores.trading.activity_category.org",
            "cpp_shell_command_aggregator_impl.cpp.mustache",
            "trading_commands.cpp")
        generated, local = _registration_calls(out)
        ordered = _CALL.findall(out)
        assert ordered == sorted(ordered)
        assert generated == sorted(generated)
        assert local == sorted(local)

    def test_a_model_that_did_not_opt_in_is_absent(self, tmp_path):
        out = _render(
            tmp_path, "ores.trading.activity_category.org",
            "cpp_shell_command_aggregator_impl.cpp.mustache",
            "trading_commands.cpp")
        assert f"{_NOT_OPTED_IN}_commands::register_commands" not in out

    def test_the_aggregator_includes_each_listed_unit(self, tmp_path):
        out = _render(
            tmp_path, "ores.trading.activity_category.org",
            "cpp_shell_command_aggregator_impl.cpp.mustache",
            "trading_commands.cpp")
        for name in _CALL.findall(out):
            assert (f'#include "ores.shell/app/commands/trading/'
                    f'{name}.hpp"') in out, name


class TestTheHeader:
    """The host includes one path, and the host context reaches the class."""

    def test_the_registrar_declares_the_host_entry_point(self, tmp_path):
        out = _render(
            tmp_path, "ores.trading.activity_category.org",
            "cpp_shell_command_aggregator_header.hpp.mustache",
            "trading_commands.hpp")
        assert "#ifndef ORES_SHELL_APP_COMMANDS_TRADING_TRADING_COMMANDS_HPP" in out
        assert "class trading_commands" in out
        assert ("static void register_commands(cli::Menu& root_menu,\n"
                "                                  ores::nats::service::"
                "nats_client& session);") in out

    def test_the_header_carries_no_pagination_context(self, tmp_path):
        out = _render(
            tmp_path, "ores.trading.activity_category.org",
            "cpp_shell_command_aggregator_header.hpp.mustache",
            "trading_commands.hpp")
        assert "pagination_context" not in out


class TestTheComponentOptIn:
    """Only a component that declares the registrar resolves the archetypes."""

    def test_trading_opts_in_and_resolves_the_registrar(self):
        assert _AGGREGATOR_KEY in TRADING_SHELL_OVERVIEW.read_text(
            encoding="utf-8")
        units, _, _ = resolve_targets(
            TRADING_MODELING / "ores.trading.trade_type.org", CODEGEN_BASE,
            address="ores.cpp.shell-command")
        templates = {u["template"] for u in units}
        assert AGGREGATOR_TEMPLATES <= templates

    def test_a_component_that_did_not_opt_in_resolves_no_registrar(self):
        units, _, _ = resolve_targets(
            DQ_MODELING / "ores.dq.badge_definition.org", CODEGEN_BASE,
            address="ores.cpp.shell-command")
        templates = {u["template"] for u in units}
        assert not (AGGREGATOR_TEMPLATES & templates), (
            "a per-entity facet opt-in must not rewrite the hand-authored "
            "dq registrar")
        # The gate must not over-drop: dq still resolves its own units.
        assert "cpp_shell_command_header.hpp.mustache" in templates


class TestTheArchetypes:
    """The pages are facet archetypes that route into the host part."""

    def test_the_pages_declare_the_facet_and_output(self):
        for name in ("ores.cpp.shell-command.command_aggregator_header.org",
                     "ores.cpp.shell-command.command_aggregator_implementation.org"):
            text = (TEMPLATES / name).read_text(encoding="utf-8")
            headers = [line for line in text.splitlines()
                       if line.startswith("#+")]
            joined = "\n".join(headers)
            assert "#+facet: ores.cpp.shell-command" in joined, name
            assert "projects/ores.shell/application/" in joined, name
            assert ":tangle cpp_shell_command_aggregator_" in text, name
