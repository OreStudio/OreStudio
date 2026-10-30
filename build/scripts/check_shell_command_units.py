#!/usr/bin/env python3
"""The shell's command units are composed once, and each name has one owner.

Two units registering the same menu name, or one aggregator registering the same
unit twice, stops ``ores.shell`` at start-up with "Two shell command units
register the name ...". The guard in ``shell_root_menu`` raises at run time, and
nothing before that point builds the full menu: a merge that adds a registration
in two branches passes every test and fails the first person who opens a shell,
which is also the first person who tries to provision a tenant.

The two rules below state the guard's invariant over the source: a menu name is
constructed by one unit, and an aggregator names each unit once.

Usage: check_shell_command_units.py [<dir> ...]
"""

import re
import sys
from collections import Counter
from pathlib import Path

MENU = re.compile(r'make_unique<cli::Menu>\(\s*"([^"]+)"\s*\)')
REGISTER = re.compile(r"^\s*(\w+)::register_commands\(")


def menu_names(text):
    return MENU.findall(text)


def registered_units(text):
    return [m.group(1) for line in text.splitlines() if (m := REGISTER.match(line))]


def main(dirs):
    files = sorted(f for d in dirs for f in Path(d).rglob("*.cpp")
                   if "tests" not in f.parts)
    problems = []
    owner = {}
    for path in files:
        text = path.read_text(errors="replace")
        names = menu_names(text)
        for name, count in Counter(names).items():
            if count > 1:
                problems.append(
                    f"{path}: builds the menu '{name}' {count} times;"
                    " one unit has one menu")
        for name in names:
            if name in owner and owner[name] != path:
                problems.append(
                    f"{path}: builds the menu '{name}', which {owner[name]} already builds;"
                    " one name has one owner")
            owner.setdefault(name, path)
        for unit, count in Counter(registered_units(text)).items():
            if count > 1:
                problems.append(
                    f"{path}: registers {unit} {count} times;"
                    " the second registration finds the name taken and the shell stops")
    for problem in problems:
        print(problem)
    print(f"{len(files)} file(s) checked, {len(problems)} problem(s)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:] or ["projects/ores.shell"]))
