#!/usr/bin/env python3
"""Check that every generated shell command unit of a checked component is registered.

A component's codegen emits one <entity>_commands.hpp per entity, declaring
<entity>_commands::register_commands(). Something has to call each of them.
A unit that nothing calls is dead weight: the entity's verbs never reach the
menu, so every shell recipe for that entity aborts with "Wrong command", and
the generated code looks present while doing nothing.

Registration happens at one of two places, both under
projects/ores.shell/application/src: either a component composite such as
refdata/refdata_commands.cpp, or the REPL itself. This check accepts any call
site there, so it constrains the lesson, not the layout.

Only the components in CHECKED_COMPONENTS are checked. A component joins the
list when its generated units all carry a registration call; the rest are
to-do, not exempt. A census on 2026-10-06 found 21 units with no caller:
ores.analytics 13 (the credit_simulation_*, shift_type, stress_shift_family
and todays_market_* families) and ores.reporting 8 (analytic_type,
configuration, configuration_parameter, configuration_type,
parameter_definition, parameter_value_domain, report_configuration and
report_type_configuration_type). Each of those needs its own story; this check
keeps the component that is complete from regressing.

Run::

    python3 projects/ores.codegen/scripts/check_shell_command_composition.py
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
SHELL = REPO_ROOT / "projects" / "ores.shell"
CALL_SITES = SHELL / "application" / "src"

GENERATED = "AUTO-GENERATED FILE"
REGISTRATION = re.compile(r"\b(\w+)_commands::register_commands\s*\(")

# Components whose every generated command unit is registered. ores.refdata
# joined with the story Migrate the remaining refdata entities to the generated
# shell command units, which wired 51 units the composite had fallen behind.
CHECKED_COMPONENTS = ("refdata",)


def generated_units(component: str, shell: Path = SHELL) -> dict[str, str]:
    """The generated command units of one component, by unit name."""
    include = shell / component / "include" / "ores.shell" / "app" / "commands" / component
    if not include.is_dir():
        return {}
    found: dict[str, str] = {}
    for header in sorted(include.glob("*_commands.hpp")):
        if GENERATED in header.read_text():
            found[header.stem] = header.name
    return found


def composed_units(call_sites: Path = CALL_SITES) -> set[str]:
    """Every command unit some application source file registers."""
    composed: set[str] = set()
    for source in call_sites.rglob("*.cpp"):
        composed.update(f"{name}_commands" for name in REGISTRATION.findall(source.read_text()))
    return composed


def check_component(component: str, composed: set[str], shell: Path = SHELL) -> str | None:
    """Why the component's units are not all registered, or None."""
    units = generated_units(component, shell)
    uncomposed = sorted(set(units) - composed)
    if uncomposed:
        return f"{component}: {len(uncomposed)} unit(s) not registered: {', '.join(uncomposed)}"
    return None


def main() -> int:
    composed = composed_units()
    failures = [f for name in CHECKED_COMPONENTS if (f := check_component(name, composed))]

    if failures:
        print("Generated shell command units with no registration call:")
        for failure in failures:
            print(f"  {failure}")
        print()
        print("Call each unit's register_commands() from a composite or from the REPL.")
        return 1

    total = sum(len(generated_units(name)) for name in CHECKED_COMPONENTS)
    print(f"Every generated shell command unit is registered ({total} units in "
          f"{', '.join(CHECKED_COMPONENTS)}).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
