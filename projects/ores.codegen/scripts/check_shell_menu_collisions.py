#!/usr/bin/env python3
"""Check that no two shell units own one name at the shell's root.

The shell had no owner for a menu name. cli::Menu keeps its children in a vector
and answers a name by first match, so two units that registered the same name
silently shadowed each other: the first unit answered every verb the two shared,
and the menu's help described only that unit, so the other unit's verbs answered
without appearing in the help.

shell_root_menu now refuses a second claim at startup, which is the enforcement.
This check is the earlier gate, and it exists for two reasons the runtime check
cannot cover on its own.

First, the runtime check reads a menu's name through Menu::Prompt() because
cli::Command keeps the name protected. Prompt() and the name are the same string
only while every menu is constructed from its name alone, so this check rejects
a construction that passes a separate description or prompt. The runtime check
rests on that invariant; this is where it is held.

Second, a duplicate that only two hand-written units produce needs a process
start to surface, and a duplicate in a unit the fleet does not start would never
surface at all.

Usage:
    check_shell_menu_collisions.py                 # check the repository
    check_shell_menu_collisions.py --root DIR      # check another tree
    check_shell_menu_collisions.py --self-test     # prove this check fails
                                                   # on a planted defect

Exits 0 when every root name has one owner, 2 when a defect is found, and 1 on a
usage or input error.
"""

from __future__ import annotations

import argparse
import re
import sys
import tempfile
from collections import defaultdict
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]

# A menu is built from its name and nothing else, so that Menu::Prompt() is the
# name the CLI resolves. A second argument is a description or a prompt, and
# either one makes the two differ.
MENU_CONSTRUCTION = re.compile(r'cli::Menu\s*>?\s*\(\s*"([^"]*)"\s*([,)])')

# A verb inserted into the root rather than into a submenu owns its name the
# same way a menu does.
ROOT_COMMAND = re.compile(r'root_menu\.Insert\(\s*\n\s*"([^"]+)"')


def scan(root: Path) -> tuple[list[str], dict[str, list[str]], int]:
    """Return the prompt defects, the owners of each root name, and a file count."""
    prompt_defects: list[str] = []
    owners: dict[str, list[str]] = defaultdict(list)
    files = 0
    for path in sorted(root.rglob("*.cpp")):
        source = path.read_text(errors="replace")
        files += 1
        rel = str(path.relative_to(root))
        for match in MENU_CONSTRUCTION.finditer(source):
            name, terminator = match.group(1), match.group(2)
            owners[name].append(rel)
            if terminator == ",":
                prompt_defects.append(
                    f"{rel}: cli::Menu(\"{name}\", ...) passes a second argument. Every "
                    "menu must be constructed from its name alone, because the shell "
                    "reads the name back through Menu::Prompt()."
                )
        for match in ROOT_COMMAND.finditer(source):
            owners[match.group(1)].append(f"{rel} (root command)")
    return prompt_defects, owners, files


def report(prompt_defects: list[str], owners: dict[str, list[str]], files: int) -> int:
    collisions = {name: sites for name, sites in owners.items() if len(sites) > 1}
    for defect in prompt_defects:
        print(f"PROMPT   {defect}")
    for name, sites in sorted(collisions.items()):
        print(f"COLLIDES {name} is owned by {len(sites)} units:")
        for site in sites:
            print(f"             {site}")
    print(
        f"\n{files} file(s) scanned, {len(owners)} root name(s), "
        f"{len(collisions)} collision(s), {len(prompt_defects)} prompt defect(s)."
    )
    return 2 if collisions or prompt_defects else 0


def self_test() -> int:
    """Plant one defect of each kind and require this check to find both."""
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp) / "projects" / "ores.shell"
        root.mkdir(parents=True)
        (root / "collides.cpp").write_text(
            'auto a = std::make_unique<cli::Menu>("accounts");\n'
            'auto b = std::make_unique<cli::Menu>("accounts");\n'
        )
        (root / "prompt.cpp").write_text(
            'auto c = std::make_unique<cli::Menu>("roles", "Roles");\n'
        )
        prompt_defects, owners, files = scan(root)
        failures = []
        if len(owners.get("accounts", [])) != 2:
            failures.append("the planted duplicate was not found")
        if len(prompt_defects) != 1:
            failures.append("the planted prompt was not found")
        verdict = report(prompt_defects, owners, files)
        if verdict != 2:
            failures.append(f"the planted defects did not fail the check (exit {verdict})")
        if failures:
            for failure in failures:
                print(f"SELF-TEST FAILED: {failure}")
            return 1
        print("Self-test passed: the planted duplicate and the planted prompt both fail it.")
        return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--root", type=Path, default=REPO / "projects" / "ores.shell",
                        help="the tree to scan (default: the repository's ores.shell)")
    parser.add_argument("--self-test", action="store_true",
                        help="plant a duplicate and a prompt defect and require both")
    args = parser.parse_args()

    if args.self_test:
        return self_test()
    if not args.root.is_dir():
        print(f"No such tree: {args.root}", file=sys.stderr)
        return 1

    prompt_defects, owners, files = scan(args.root)
    return report(prompt_defects, owners, files)


if __name__ == "__main__":
    raise SystemExit(main())
