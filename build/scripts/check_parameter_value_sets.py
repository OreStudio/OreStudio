#!/usr/bin/env python3
"""Fail when a parameter is drawn without a value set, or a per-pair one returns.

The value set tables are the one source of a parameter's control and rule. This
check reads the curation the seed is written from and fails when:

- a parameter a screen draws has no value set;
- a link names a set the curation does not state;
- a set states a rule its own kind cannot hold, such as a floor above its
  ceiling, an enumerated set with no values, a catalogue that is not registered,
  or a regular expression with no pattern;
- a rule comes back to the tables that hold the value: a per-pair kind or
  options, or a bound or list written as a column check on an engine settings
  part.

A set of source kind text makes a parameter free text, which is data and passes.
A parameter with no set is a gap and fails.

The script the seed is written to is checked exactly by
=generate_value_sets.py --check=, which renders it and compares, so this check
does not repeat that weaker test.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

UTILITY = REPO_ROOT / "projects" / "ores.sql" / "utility"

# The columns a per-pair row carried before the value set stated the same facts.
# A table that declares one is a rule stated twice, which is what this check is
# for.
OVERRIDE_COLUMNS = ("kind", "options", "minimum", "maximum")

# The tables and sources that must not state an override, with the words that
# would state one.
GUARDED = (
    (
        REPO_ROOT / "projects/ores.sql/create/analytics/analytics_pricing_pair_settings_create.sql",
        ("kind", "options"),
    ),
    (
        REPO_ROOT / "projects/ores.analytics/modeling/ores.analytics.pricing_pair_setting.org",
        ("kind", "options"),
    ),
    (
        REPO_ROOT
        / "projects/ores.sql/create/reporting/reporting_engine_settings_group_settings_create.sql",
        OVERRIDE_COLUMNS,
    ),
    (
        REPO_ROOT
        / "projects/ores.reporting/modeling/ores.reporting.engine_settings_group_setting.org",
        OVERRIDE_COLUMNS,
    ),
)

# The server and the screen serve a setting from its set. A read of the retired
# per-pair column here is the fallback this check exists to keep out, stated as
# the shape the read takes in that file, so the guard can actually fire.
NO_OVERRIDE_READS = (
    (
        REPO_ROOT / "projects/ores.web/packages/bff/src/pricing-engines.ts",
        r"row\['(kind|options)'\]|kindSchema",
    ),
    (
        REPO_ROOT / "projects/ores.web/packages/web/src/api/pricingEngines.ts",
        r"z\.enum\(\s*\[\s*'flag'",
    ),
)

# A bound or a list written as a column check states the rule a second time,
# beside the set the guard reads. The engine settings parts carry a guard
# instead, so a check on a value column here is a regression.
NO_VALUE_CHECKS = tuple(
    REPO_ROOT / path
    for path in (
        "projects/ores.sql/create/reporting/reporting_engine_settings_fixings_create.sql",
        "projects/ores.sql/create/reporting/reporting_engine_settings_building_create.sql",
        "projects/ores.sql/create/reporting/reporting_engine_settings_execution_create.sql",
        "projects/ores.reporting/modeling/ores.reporting.engine_settings_fixings.org",
        "projects/ores.reporting/modeling/ores.reporting.engine_settings_building.org",
        "projects/ores.reporting/modeling/ores.reporting.engine_settings_execution.org",
    )
)

VALUE_CHECK = re.compile(r"check\s*\([^)]*(>=|<=|\bin\s*\(|\bbetween\b)", re.IGNORECASE)

SOURCE_KINDS = ("enumerated", "catalogue", "range", "regex", "boolean", "integer", "number", "text")


def curation():
    """The value sets the seed is written from, resolved as the generator does."""
    sys.path.insert(0, str(UTILITY))
    import generate_value_sets
    import setup_value_sets

    data = generate_value_sets.load()
    problems = generate_value_sets.resolve(data)
    problems += generate_value_sets.add_conventions(data)
    problems += generate_value_sets.add_curves(data)
    problems += generate_value_sets.add_setup(data)
    return generate_value_sets, setup_value_sets, data, problems


def drawn_parameters(generate_value_sets, setup_value_sets):
    """Every (scope, name) a configuration screen draws."""
    parameters = set(generate_value_sets.contract_pairs())
    parameters.update(("Setup", setting["ore"]) for setting in setup_value_sets.settings())
    return parameters


def set_problems(data):
    """What a set states that its own kind cannot hold."""
    problems = []
    catalogues = data["catalogues"]
    if isinstance(catalogues, list):
        catalogues = {entry["code"]: entry for entry in catalogues}
    for code, entry in sorted(data["sets"].items()):
        kind = entry.get("source_kind", "")
        if kind not in SOURCE_KINDS:
            problems.append(f"{code}: unknown source kind {kind!r}")
            continue
        minimum = entry.get("minimum")
        maximum = entry.get("maximum")
        if minimum is not None and not isinstance(minimum, (int, float)):
            problems.append(f"{code}: a floor that is not a number")
        if maximum is not None and not isinstance(maximum, (int, float)):
            problems.append(f"{code}: a ceiling that is not a number")
        if isinstance(minimum, (int, float)) and isinstance(maximum, (int, float)):
            if minimum > maximum:
                problems.append(f"{code}: a floor above its ceiling")
        if kind == "enumerated":
            values = [value.get("value", "") for value in entry.get("values", [])]
            if not values:
                problems.append(f"{code}: enumerated with no values")
            if len(set(values)) != len(values):
                problems.append(f"{code}: enumerated with a value stated twice")
            if "" in values:
                problems.append(f"{code}: enumerated with an empty value")
        elif kind == "catalogue":
            if entry.get("catalogue_code") not in catalogues:
                problems.append(f"{code}: names a catalogue that is not registered")
            if entry.get("values"):
                problems.append(f"{code}: a catalogue set that also lists values")
        elif kind == "regex" and not entry.get("pattern"):
            problems.append(f"{code}: a regular expression with no pattern")
        elif kind in ("integer", "number", "range") and entry.get("values"):
            problems.append(f"{code}: a numeric set that also lists values")
    return problems


def override_problems():
    """Whether a per-pair override came back to a table, a model or a reader."""
    problems = []
    for path, columns in GUARDED:
        if not path.exists():
            problems.append(f"{path.relative_to(REPO_ROOT)}: does not exist")
            continue
        text = path.read_text(encoding="utf-8")
        for column in columns:
            if f'"{column}"' in text or f"** {column}" in text:
                problems.append(
                    f"{path.relative_to(REPO_ROOT)}: states the override column {column}"
                )
    for path in NO_VALUE_CHECKS:
        if not path.exists():
            problems.append(f"{path.relative_to(REPO_ROOT)}: does not exist")
            continue
        text = path.read_text(encoding="utf-8")
        for number, line in enumerate(text.splitlines(), start=1):
            if VALUE_CHECK.search(line):
                problems.append(
                    f"{path.relative_to(REPO_ROOT)}:{number}: states a value as a column check;"
                    " the set states it"
                )
    for path, pattern in NO_OVERRIDE_READS:
        if not path.exists():
            problems.append(f"{path.relative_to(REPO_ROOT)}: does not exist")
            continue
        text = path.read_text(encoding="utf-8")
        for found in sorted(set(re.findall(pattern, text))):
            column = found if isinstance(found, str) else found[0]
            problems.append(
                f"{path.relative_to(REPO_ROOT)}: reads the retired pair setting {column}"
            )
    return problems


def main() -> int:
    generate_value_sets, setup_value_sets, data, problems = curation()
    parameters = drawn_parameters(generate_value_sets, setup_value_sets)
    linked = {}
    for key, set_code in data["links"].items():
        scope, _, name = key.partition("|")
        linked[(scope, name)] = set_code

    for scope, name in sorted(parameters):
        if (scope, name) not in linked:
            problems.append(f"{scope}.{name}: no value set, and the set does not say text")
    for key, set_code in sorted(data["links"].items()):
        if set_code not in data["sets"]:
            problems.append(f"{key}: names the set {set_code}, which no set states")
    for code in sorted(data["sets"]):
        if code not in set(linked.values()):
            problems.append(f"{code}: no parameter names it")
    problems += set_problems(data)

    problems += override_problems()

    if problems:
        print("\n".join(problems), file=sys.stderr)
        return 1
    print(
        f"{len(parameters)} drawn parameters, {len(data['sets'])} sets and "
        f"{len(data['links'])} links; none states its rule twice."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
