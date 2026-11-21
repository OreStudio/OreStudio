#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""Copy ORE's script parser into ores.trading.scripting.

The files are read from an ORE checkout at the commit COMMIT names, through git,
so the checkout's own state does not matter. Each file receives the same
mechanical transforms: ORE Studio's namespace, include paths and include guard,
the standard library in place of the few QuantLib names ore_compat.hpp states,
and the PATCHES below, which are the only changes of substance. The result is
clang-formatted as every other source is.

--check produces the files in memory and fails when the committed copy differs,
so the copy is always what this script makes of ORE's files at COMMIT. To take a
new ORE release, change COMMIT, run the script, and review the diff.
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
COMPONENT = ROOT / "projects/ores.trading/scripting"
INCLUDE = COMPONENT / "include/ores.trading.scripting/ore"
SOURCE = COMPONENT / "src/ore"
ORE_DIR = "OREData/ored/scripting"

# The ORE commit the copy is taken from.
COMMIT = "c3ffbae78"

# ORE's file stem, and the stem it takes here.
FILES = {
    "ast": "ast",
    "grammar": "grammar",
    "scriptparser": "script_parser",
    "astprinter": "ast_printer",
    "asttoscriptconverter": "ast_to_script_converter",
    "randomastgenerator": "random_ast_generator",
}

# The grammar's header pulls Spirit and Phoenix in, so it stays private.
PRIVATE_HEADERS = {"grammar"}

# Changes of substance, each with its reason: (ORE file, text, replacement).
PATCHES = [
    ("ast.hpp",
     "    // cache for optimised variable reference retrieval\n"
     "    bool isCached = false, isScalar = false;\n"
     "    ValueType* cachedScalar = nullptr;\n"
     "    std::vector<ValueType>* cachedVector = nullptr;\n",
     # The engine caches a variable's value on its node. ORE Studio never
     # evaluates a script, and the value types bring in the engine.
     ""),
]


def ore_text(ore_dir, path):
    result = subprocess.run(["git", "-C", str(ore_dir), "show", f"{COMMIT}:{path}"],
                            capture_output=True, text=True)
    if result.returncode != 0:
        sys.exit(f"cannot read {path} at {COMMIT} in {ore_dir}: {result.stderr.strip()}")
    return result.stdout


def include_of(stem, own_header):
    """The include line an ORE scripting header becomes."""
    if stem in PRIVATE_HEADERS:
        return f'#include "{FILES[stem]}.hpp"'
    return f'#include "ores.trading.scripting/ore/{FILES[stem]}.hpp"'


def transform(name, text):
    stem, ext = name.rsplit(".", 1)
    for file, old, new in PATCHES:
        if file == name:
            if text.count(old) != 1:
                sys.exit(f"the patch of {name} finds its text {text.count(old)} times; review it")
            text = text.replace(old, new)

    compat = '#include "ores.trading.scripting/ore/ore_compat.hpp"'
    lines, compat_added = [], False
    for line in text.split("\n"):
        m = re.match(r"#include <ored/scripting/(\w+)\.hpp>", line)
        if m:
            if m.group(1) == "value":
                continue
            line = include_of(m.group(1), name)
        elif re.match(r"#include <(ql|qle|ored)/", line):
            if compat_added:
                continue
            line, compat_added = compat, True
        elif line.strip() == "using namespace QuantLib;":
            continue
        lines.append(line)
    text = "\n".join(lines)
    if not compat_added:
        text = re.sub(r"(#include [<\"][^\n]+\n)", r"\1" + compat + "\n", text, count=1)

    text = text.replace("namespace ore {\nnamespace data {", "namespace ores::trading::scripting {")
    text = re.sub(r"\} // namespace data\n\} // namespace \w+", "}", text)
    if re.search(r"namespace (ore|data)\b", text):
        sys.exit(f"{name} keeps one of ORE's namespaces after the transforms; review them")
    text = text.replace("QuantLib::ext::", "std::")
    text = text.replace("ore::data::to_string", "to_string")
    text = re.sub(r"\bQL_REQUIRE\b", "ORES_SCRIPTING_REQUIRE", text)
    text = re.sub(r"\bQL_FAIL\b", "ORES_SCRIPTING_FAIL", text)

    notice = (f"/*\n Copied into ORE Studio from ORE's {ORE_DIR}/{name} at commit {COMMIT} by\n"
              f" projects/ores.trading/scripting/utility/import_ore_scripting.py. Do not edit:\n"
              f" change the script, which states every change it makes.\n*/\n")
    licence_end = text.index("*/") + 2
    text = text[:licence_end] + "\n\n" + notice + text[licence_end:]

    text = re.sub(r"\n{3,}", "\n\n", text)

    if ext == "hpp":
        guard = "ORES_TRADING_SCRIPTING_ORE_" + FILES[stem].upper() + "_HPP"
        if FILES[stem] in {FILES[s] for s in PRIVATE_HEADERS}:
            guard = "ORES_TRADING_SCRIPTING_SRC_ORE_" + FILES[stem].upper() + "_HPP"
        text = text.replace("#pragma once", f"#ifndef {guard}\n#define {guard}", 1)
        text = text.rstrip() + "\n\n#endif\n"
    return text


def formatted(text, path):
    try:
        return subprocess.run(["clang-format", f"--assume-filename={path}"], input=text,
                              capture_output=True, text=True, check=True).stdout
    except (OSError, subprocess.CalledProcessError) as error:
        sys.exit(f"clang-format failed on {path}: {error}")


def outputs(ore_dir):
    out = {}
    for ore_stem, stem in FILES.items():
        for ext in ("hpp", "cpp"):
            name = f"{ore_stem}.{ext}"
            listed = subprocess.run(["git", "-C", str(ore_dir), "cat-file", "-e",
                                     f"{COMMIT}:{ORE_DIR}/{name}"], capture_output=True)
            if listed.returncode != 0:
                continue
            if ext == "hpp" and ore_stem not in PRIVATE_HEADERS:
                path = INCLUDE / f"{stem}.hpp"
            else:
                path = SOURCE / f"{stem}.{ext}"
            out[path] = formatted(transform(name, ore_text(ore_dir, f"{ORE_DIR}/{name}")), path)
    return out


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--ore-dir", required=True, help="an ORE checkout that holds COMMIT")
    parser.add_argument("--check", action="store_true",
                        help="fail when the committed copy differs from what the script makes")
    args = parser.parse_args()
    files = outputs(Path(args.ore_dir).expanduser())
    if args.check:
        stale = [str(p.relative_to(ROOT)) for p, text in files.items()
                 if not p.exists() or p.read_text() != text]
        if stale:
            sys.exit("the copy differs from ORE at " + COMMIT + "; run the script: " +
                     ", ".join(stale))
        print(f"the copy is ORE's {ORE_DIR} at {COMMIT}, as the script makes it")
        return
    for path, text in files.items():
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        print(path.relative_to(ROOT))


if __name__ == "__main__":
    main()
