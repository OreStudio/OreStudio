#!/usr/bin/env python3
"""Inventory ores.compute for the Component Clean Standard, items B02 to B04.

    python3 doc/agile/versions/v0/sprint_26/clean-compute/inventory.py

Prints four tables: models by metatype, C++ files by generated or
hand-written, messaging headers, and the NATS subjects the checked-in
protocol headers declare beside the raw subject literals left in
hand-written code. A reviewer reruns this instead of trusting a table
typed by hand.

Exits non-zero when a table comes back empty, so a silent break shows up
as a failure rather than as a plausible-looking blank.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[6]
COMPONENT_DIR = REPO / "projects" / "ores.compute"
SHELL_DIR = REPO / "projects" / "ores.shell" / "compute"
GENERATED_MARKER = "AUTO-GENERATED FILE"
CXX_SUFFIXES = (".hpp", ".cpp")
SUBJECT_LITERAL = re.compile(r'"(compute\.v1\.[^"]*)"')
NATS_CONSTANT = re.compile(r'nats_subject\s*=\s*"([^"]+)"')

PROJECTS = REPO / "projects"
CONSUMER_SUFFIXES = (".hpp", ".cpp", ".ts", ".mustache")
SKIP_DIRS = frozenset(("venv", "node_modules", "__pycache__", "build", "dist"))
STRUCT = re.compile(r"^struct (\w+) \{(.*?)^\};", re.S | re.M)
RESPONSE_TYPE = re.compile(r"using response_type = struct (\w+);")
FIELD_TYPE = re.compile(r"([\w:]+(?:<[\w:<>, ]+>)?)\s+\w+\s*[;=]")
CARRIER = re.compile(r"^(std::vector|std::optional)<(.+)>$")


def models_by_metatype() -> list[tuple[str, str, str]]:
    rows = []
    for path in sorted((COMPONENT_DIR / "modeling").glob("*.org")):
        text = path.read_text(encoding="utf-8")
        metatype = re.search(r"^#\+type:\s*(\S+)", text, re.M)
        profile = re.search(r"^:profile:\s*(\S+)", text, re.M)
        rows.append(
            (
                path.name,
                metatype.group(1).replace("ores.codegen.", "") if metatype else "?",
                profile.group(1) if profile else "-",
            )
        )
    return rows


def cxx_files() -> tuple[list[Path], list[Path]]:
    generated, handwritten = [], []
    for path in sorted(COMPONENT_DIR.rglob("*")):
        if path.suffix not in CXX_SUFFIXES or "/tests/" in path.as_posix():
            continue
        if GENERATED_MARKER in path.read_text(encoding="utf-8", errors="replace"):
            generated.append(path)
        else:
            handwritten.append(path)
    return generated, handwritten


def messaging_headers() -> tuple[list[Path], list[Path]]:
    generated, handwritten = [], []
    for path in sorted(COMPONENT_DIR.rglob("*.hpp")):
        if "messaging" not in path.parts:
            continue
        target = (
            generated
            if GENERATED_MARKER in path.read_text(encoding="utf-8", errors="replace")
            else handwritten
        )
        target.append(path)
    return generated, handwritten


def subjects() -> tuple[dict[str, list[str]], list[tuple[str, int, str]]]:
    declared: dict[str, list[str]] = {}
    for path in sorted(COMPONENT_DIR.rglob("*_protocol.hpp")):
        found = NATS_CONSTANT.findall(path.read_text(encoding="utf-8", errors="replace"))
        if found:
            declared[path.name] = found

    raw: list[tuple[str, int, str]] = []
    for root in (COMPONENT_DIR, SHELL_DIR):
        if not root.exists():
            continue
        for path in sorted(root.rglob("*")):
            if path.suffix not in CXX_SUFFIXES or "/tests/" in path.as_posix():
                continue
            content = path.read_text(encoding="utf-8", errors="replace")
            if GENERATED_MARKER in content:
                continue
            for number, line in enumerate(content.splitlines(), 1):
                match = SUBJECT_LITERAL.search(line)
                if match:
                    raw.append((path.relative_to(REPO).as_posix(), number, match.group(1)))
    return declared, raw


def unserved_records() -> list[tuple[str, str]]:
    """P03: the declared records no code path reaches.

    A record is served when a caller can reach it. The roots are the requests
    that state a subject, and every record any file outside the record's own
    protocol header names -- the eventing registrars, the handlers, the shell
    and the TypeScript callers. A record a served record carries is served too,
    transitively. What is left is a wire type nothing reaches.
    """
    structs: dict[str, tuple[str, list[str], bool]] = {}
    for path in sorted(COMPONENT_DIR.rglob("*_protocol.hpp")):
        text = path.read_text(encoding="utf-8", errors="replace")
        for name, body in STRUCT.findall(text):
            fields: list[str] = []
            for declaration in body.splitlines():
                declaration = declaration.strip()
                answered = RESPONSE_TYPE.match(declaration)
                if answered:
                    fields.append(answered.group(1))
                    continue
                if (not declaration or declaration.startswith(("/", "*", "#", "}"))
                        or declaration.startswith("static")):
                    continue
                carried = FIELD_TYPE.match(declaration)
                if carried:
                    fields.append(carried.group(1))
            structs[name] = (path.name, fields, "nats_subject" in body)

    roots = {name for name, (_, _, on_wire) in structs.items() if on_wire}
    reference = re.compile(
        r"\b(" + "|".join(sorted((re.escape(n) for n in structs), key=len, reverse=True)) + r")\b")
    for path in PROJECTS.rglob("*"):
        if path.suffix not in CONSUMER_SUFFIXES or not path.is_file():
            continue
        if any(part in SKIP_DIRS for part in path.parts):
            continue
        text = path.read_text(encoding="utf-8", errors="replace")
        for name in set(reference.findall(text)):
            if path.name != structs[name][0]:
                roots.add(name)

    seen: set[str] = set()
    pending = list(roots)
    while pending:
        name = pending.pop()
        if name in seen:
            continue
        seen.add(name)
        for carried in structs.get(name, (None, [], False))[1]:
            base = CARRIER.sub(r"\2", carried)
            if base in structs and base not in seen:
                pending.append(base)
    return [(structs[name][0], name) for name in sorted(set(structs) - seen)]


def main() -> int:
    models = models_by_metatype()
    generated, handwritten = cxx_files()
    msg_generated, msg_handwritten = messaging_headers()
    declared, raw = subjects()
    unserved = unserved_records()

    print(f"# ores.compute clean-standard inventory\n")

    print(f"## B02 models by metatype ({len(models)})")
    for name, metatype, profile in models:
        print(f"{metatype:12} {profile:26} {name}")

    print(f"\n## B03 C++ files outside tests ({len(generated) + len(handwritten)})")
    print(f"generated    {len(generated)}")
    print(f"hand-written {len(handwritten)}")
    by_dir: dict[str, int] = {}
    for path in handwritten:
        by_dir[path.parent.relative_to(COMPONENT_DIR).as_posix()] = (
            by_dir.get(path.parent.relative_to(COMPONENT_DIR).as_posix(), 0) + 1
        )
    for directory, count in sorted(by_dir.items()):
        print(f"  {count:3} {directory}")

    print(f"\n## B03 messaging headers ({len(msg_generated) + len(msg_handwritten)})")
    print(f"generated    {len(msg_generated)}")
    print(f"hand-written {len(msg_handwritten)}")
    for path in msg_handwritten:
        print(f"  {path.relative_to(REPO).as_posix()}")

    total = sum(len(v) for v in declared.values())
    print(f"\n## B04 declared subjects ({total} across {len(declared)} protocol headers)")
    for name in sorted(declared):
        print(f"{name}: {', '.join(sorted(declared[name]))}")

    print(f"\n## B04 raw subject literals in hand-written code ({len(raw)})")
    for path, number, literal in raw:
        print(f"{path}:{number} {literal}")

    print(f"\n## P03 declared records no code path reaches ({len(unserved)})")
    for header, name in unserved:
        print(f"{header} {name}")

    empty = [name for name, value in
             (("B02", models), ("B03 cpp", generated + handwritten),
              ("B03 messaging", msg_generated + msg_handwritten), ("B04", declared))
             if not value]
    if empty:
        print(f"\nEMPTY: {', '.join(empty)}", file=sys.stderr)
        return 1
    if unserved:
        print(f"\nUNSERVED: {len(unserved)} record(s) nothing reaches", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
