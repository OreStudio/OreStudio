#!/usr/bin/env python3
"""Seed the recipes of the scripted twins, from the trade type catalogue.

A product ORE carries in both forms is booked twice: its own element, and a
generic ScriptedTrade over the library script the catalogue's Scripted forms
table names. The catalogue's Scripted twin recipes table states where each
input of the script takes its value from in the native trade's rows. This
script joins the two tables with the inputs the script library seed mines from
ORE's library, refuses a recipe that misses an input of its script or names one
the script does not read, and writes the recipes twice: as the seed of the
script twin inputs table, and as the table the trade generator compiles in.

Run with no argument to write both files, and with --check to fail when either
committed file is not what the catalogue gives.
"""
import argparse
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
CATALOGUE = ROOT / "projects/ores.trading/modeling/ores.trading.trade_type_catalogue.org"
LIBRARY_SEED = ROOT / "projects/ores.sql/populate/trading/trading_script_library_populate.sql"
SEED = ROOT / "projects/ores.sql/populate/trading/trading_script_twin_inputs_populate.sql"
HEADER = ROOT / "projects/ores.synthetic/core/src/service/script_twin_recipes.hpp"
ENTITY = ROOT / "projects/ores.trading/modeling/ores.trading.script_twin_input.org"

LICENCE = """/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
"""


def org_table(text, heading):
    """The rows of the first table under the heading, as lists of cells."""
    start = text.index(f"\n* {heading}\n")
    end = text.find("\n* ", start + 1)
    section = text[start:end if end != -1 else len(text)]
    rows = []
    for line in section.splitlines():
        if not line.startswith("|") or line.startswith("|-"):
            continue
        rows.append([cell.strip() for cell in line.strip().strip("|").split("|")])
    return rows[1:]


def library_scripts():
    """Every script the library seed states, in its order."""
    text = LIBRARY_SEED.read_text()
    block = text[text.index("insert into ores_trading_scripts_tbl"):]
    block = block[:block.index(";\n")]
    names = re.findall(r"^    \('([^']+)', ores_utility_system_tenant_id_fn\(\)", block, re.M)
    if not names:
        sys.exit(f"found no script in {LIBRARY_SEED.relative_to(ROOT)}; its layout changed")
    return names


HELD = {"both forms", "element only", "script only", "not yet"}


def script_inputs():
    """Each script's inputs, in the library's order, as (name, type, is_array)."""
    text = LIBRARY_SEED.read_text()
    block = text[text.index("insert into ores_trading_script_inputs_tbl"):]
    pattern = re.compile(r"^    \('([^']+)', ores_utility_system_tenant_id_fn\(\), 0, (\d+), "
                         r"'([^']+)', '([^']+)', (true|false),", re.M)
    inputs = {}
    for script, _, name, typ, array in pattern.findall(block):
        inputs.setdefault(script, []).append((name, typ, array == "true"))
    if not inputs:
        sys.exit(f"found no script input in {LIBRARY_SEED.relative_to(ROOT)}; its layout changed")
    return inputs


def split(source, separator):
    """The parts of a source between each separator, a quoted literal being read whole."""
    parts, quoted, start, i = [], False, 0, 0
    while i < len(source):
        if source[i] == "'":
            quoted = not quoted
        elif not quoted and source.startswith(separator, i):
            parts.append(source[start:i])
            start = i + len(separator)
            i = start
            continue
        i += 1
    parts.append(source[start:])
    return parts


def source_parts(source):
    """Each list part of an `a and b` source, as the alternatives of its `x or y`."""
    return [split(listed, " or ") for listed in split(source, " and ")]


SCHEDULE = re.compile(r"instrument_schedules\[\w+\](?: based on (\w+))?")
DERIVED = re.compile(r"(\w+) shifted \w+ on \w+ \w+")


def transforms(text):
    """The transforms' codes, as (transform, value, code) rows, checked against the entity."""
    rows = [tuple(row) for row in org_table(text, "Scripted twin transforms")]
    names = sorted({name for name, _, _ in rows})
    accepted = re.search(r'"transform" in \(([^)]*)\)', ENTITY.read_text())
    stated = sorted(re.findall(r"'([^']+)'", accepted.group(1))) if accepted else []
    if stated != names:
        sys.exit(f"{ENTITY.relative_to(ROOT)} accepts the transforms {stated}, "
                 f"but the catalogue states {names}")
    return rows


def recipes():
    """Every recipe row: a native type's twin, then the scripts that type carries."""
    text = CATALOGUE.read_text()
    known = {name for name, _, _ in transforms(text)}
    forms = {code: script for code, script in org_table(text, "Scripted forms")}
    library = script_inputs()
    # Each form is a native type and a script: the type's twin, or a script with
    # no element of its own that the type's rows carry.
    stated = {}
    errors, rows = [], []

    def state(code, script, name, source, transform):
        if transform and transform not in known:
            errors.append(f"{code}: {name} names the unknown transform {transform}")
        for part in (p for listed in source_parts(source) for p in listed):
            if part.startswith("'") != part.endswith("'"):
                errors.append(f"{code}: {name} states an unclosed literal {part}")
        stated.setdefault((code, script), {})[name] = (source, transform or None)

    # A type of the Scripted forms table with no recipe is not twinned yet: the
    # recipes land a few products at a time.
    for code, name, source, transform in org_table(text, "Scripted twin recipes"):
        if code not in forms:
            errors.append(f"{code}: a recipe for a type the Scripted forms table does not name")
            continue
        state(code, forms[code], name, source, transform)
    for code, script, name, source, transform in org_table(text, "Script-only recipes"):
        if script in forms.values():
            errors.append(f"{code}: {script} is a type's scripted form, so it is not script only")
        state(code, script, name, source, transform)

    carried = {script for code, script in stated if forms.get(code) != script}
    twinned = set(forms.values())
    held = {}
    for script, how, _ in org_table(text, "Library scripts"):
        if script in held:
            errors.append(f"Library scripts: {script} is listed twice")
        if how not in HELD:
            errors.append(f"Library scripts: {script} is held as {how}, not one of {sorted(HELD)}")
        held[script] = how
    for script in library_scripts():
        how = held.pop(script, None)
        if how is None:
            errors.append(f"Library scripts: {script} is in the library but not listed")
        elif (how == "both forms") != (script in twinned):
            errors.append(f"Library scripts: {script} is held as {how}, but the Scripted forms "
                          f"table {'names' if script in twinned else 'does not name'} it")
        elif (how == "script only") != (script in carried):
            errors.append(f"Library scripts: {script} is held as {how}, but "
                          f"{'a' if script in carried else 'no'} script-only recipe states it")
    for script in held:
        errors.append(f"Library scripts: {script} is not in the library")

    sequences = {}
    for (code, script) in sorted(stated, key=lambda k: (k[0], forms.get(k[0]) != k[1], k[1])):
        twin = forms.get(code) == script
        inputs = library.get(script, [])
        names = [name for name, _, _ in inputs]
        for name in stated[(code, script)]:
            if name not in names:
                errors.append(f"{code}: {script} reads no input {name}")
        for name, typ, array in inputs:
            if name not in stated[(code, script)]:
                errors.append(f"{code}: the recipe states no source for {script}'s {name}")
                continue
            sequence = sequences[code] = sequences.get(code, 0) + 1
            source, transform = stated[(code, script)][name]
            listed = source_parts(source)
            parts = [part for alternatives in listed for part in alternatives]
            events = [part for part in parts if SCHEDULE.fullmatch(part) or DERIVED.fullmatch(part)]
            for part in parts:
                if part.startswith("'") or part in events or "." in part:
                    continue
                errors.append(f"{code}: {name} names no column in {part}")
            if events and typ != "Event":
                errors.append(f"{code}: {name} states an event but is not an Event")
            # A list of several parts is an event's dates, each part a schedule or values.
            if events and len(listed) > 1 and not array:
                errors.append(f"{code}: {name} lists an event's dates but is not a list")
            for alternatives in listed:
                stated_events = [part for part in alternatives if part in events]
                if stated_events and len(stated_events) != len(alternatives):
                    errors.append(f"{code}: {name} mixes an event with values")
            for part in events:
                base = DERIVED.fullmatch(part) or SCHEDULE.fullmatch(part)
                if base and base.group(1) and (
                        base.group(1) == name or
                        (base.group(1), "Event") not in {(n, t) for n, t, _ in inputs}):
                    errors.append(f"{code}: {name} derives from {base.group(1)}, "
                                  f"which is no other Event of {script}")
            rows.append((code, sequence, script, twin, name, typ, array, source, transform))
    if errors:
        sys.exit("\n".join(errors))
    return rows


def literal(value):
    if value is None:
        return "null"
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, int):
        return str(value)
    return "'" + value.replace("'", "''") + "'"


def seed(rows):
    out = ["/* Generated by projects/ores.sql/utility/generate_script_twins.py from the trade\n"
           " * type catalogue. Do not edit: run the script, which --check verifies. */\n\n"
           "\\echo '--- Script twin inputs ---'\n\n"]
    if rows:
        body = ",\n".join(
            "    (" + literal(code) + ", ores_utility_system_tenant_id_fn(), 0, "
            + ", ".join(literal(v) for v in (sequence, script, name, typ, array, source,
                                             transform))
            + ", current_user, 'system.initial_load', 'Seed the script twin recipes')"
            for code, sequence, script, _, name, typ, array, source, transform in rows)
        out.append("insert into ores_trading_script_twin_inputs_tbl (\n"
                   "    trade_type_code, tenant_id, version, sequence_number, script_name, "
                   "input_name, data_type, is_array, source, transform,\n"
                   "    modified_by, change_reason_code, change_commentary\n"
                   f") values\n{body}\n"
                   "on conflict (tenant_id, trade_type_code, sequence_number)\n"
                   "where valid_to = ores_utility_infinity_timestamp_fn()\ndo nothing;\n")
    return "".join(out)


def cxx(value):
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def header(rows, codes):
    lines = [LICENCE,
             "// Generated by projects/ores.sql/utility/generate_script_twins.py from the trade\n"
             "// type catalogue. Do not edit: run the script, which --check verifies.\n",
             "#ifndef ORES_SYNTHETIC_CORE_SERVICE_SCRIPT_TWIN_RECIPES_HPP",
             "#define ORES_SYNTHETIC_CORE_SERVICE_SCRIPT_TWIN_RECIPES_HPP\n",
             "#include <string_view>\n",
             "namespace ores::synthetic::service {\n",
             "/**",
             " * One input of a scripted form of a native trade and where its value comes",
             " * from in the native's rows. The form is the native's twin, or a script with",
             " * no element of its own that the native's rows carry.",
             " */",
             "struct script_twin_recipe final {",
             "    std::string_view trade_type_code;",
             "    std::string_view script_name;",
             "    bool twin;",
             "    std::string_view input_name;",
             "    std::string_view data_type;",
             "    bool is_array;",
             "    std::string_view source;",
             "    std::string_view transform;",
             "};\n",
             "/// One code a transform turns a native value into.",
             "struct script_twin_transform final {",
             "    std::string_view transform;",
             "    std::string_view value;",
             "    std::string_view code;",
             "};\n",
             "/// The transforms' codes, as the catalogue states them.",
             "inline constexpr script_twin_transform script_twin_transforms[] = {"]
    for name, value, code in codes:
        lines.append(f"    {{{cxx(name)}, {cxx(value)}, {cxx(code)}}},")
    lines += ["};\n",
             "/// The recipes, by trade type, the twin first, each in its script's input order.",
             "inline constexpr script_twin_recipe script_twin_recipes[] = {"]
    for code, _, script, twin, name, typ, array, source, transform in rows:
        lines.append(f"    {{{cxx(code)}, {cxx(script)}, {'true' if twin else 'false'}, "
                     f"{cxx(name)}, {cxx(typ)}, "
                     f"{'true' if array else 'false'}, {cxx(source)}, {cxx(transform or '')}}},")
    lines += ["};\n", "}\n", "#endif"]
    text = "\n".join(lines) + "\n"
    # The header is formatted as every other source is, so the format check passes it.
    try:
        return subprocess.run(["clang-format", f"--assume-filename={HEADER}"], input=text,
                              capture_output=True, text=True, check=True).stdout
    except (OSError, subprocess.CalledProcessError) as error:
        sys.exit(f"clang-format, which formats the generated header, failed: {error}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    rows = recipes()
    codes = transforms(CATALOGUE.read_text())
    outputs = {SEED: seed(rows), HEADER: header(rows, codes)}
    if args.check:
        stale = [str(p.relative_to(ROOT)) for p, text in outputs.items()
                 if not p.exists() or p.read_text() != text]
        if stale:
            sys.exit("not what the catalogue gives; run the script: " + ", ".join(stale))
        return
    for path, text in outputs.items():
        path.write_text(text)


if __name__ == "__main__":
    main()
