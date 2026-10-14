#!/usr/bin/env python3
"""Measure the translation unit that MSVC fails on.

The unit is projects/ores.refdata/core/src/messaging/configuration_document_handler.cpp.
It reports two things:

  1. The include closure: how many repository headers the unit pulls in, and
     which third-party libraries, because MSVC's C1202 is a limit on the
     accumulated template instantiation graph.
  2. Every reflected literal the encode path reaches: the element type of each
     std::vector in the two document structs, and its member count. reflect-cpp
     builds one rfl::Literal per plain struct, holding that struct's own field
     names, so the member count is the literal width.

Run from any directory: python3 measure_handler_tu.py
"""

import pathlib
import re
import sys
from collections import Counter

UNIT = "projects/ores.refdata/core/src/messaging/configuration_document_handler.cpp"
PROTOCOL = "projects/ores.refdata/api/include/ores.refdata.api/messaging/configuration_document_protocol.hpp"


def find_root() -> pathlib.Path:
    for parent in pathlib.Path(__file__).resolve().parents:
        if (parent / "vcpkg.json").exists():
            return parent
    sys.exit("run from inside the repository: no vcpkg.json above this script")


ROOT = find_root()


def include_dirs() -> list[pathlib.Path]:
    dirs = []
    for base in sorted((ROOT / "projects").glob("*")):
        for parent in [base, *sorted(base.glob("*"))]:
            for kind in ("include", "src"):
                directory = parent / kind
                if directory.is_dir():
                    dirs.append(directory)
    return dirs


INCLUDE_DIRS = include_dirs()


def includes_of(path: pathlib.Path) -> list[tuple[str, bool]]:
    found: list[tuple[str, bool]] = []
    try:
        text = path.read_text(errors="replace")
    except OSError:
        return found
    for match in re.finditer(r'^\s*#\s*include\s*([<"])([^">]+)[">]', text, re.M):
        found.append((match.group(2), match.group(1) == "<"))
    return found


def resolve(name: str, current_dir: pathlib.Path) -> pathlib.Path | None:
    candidate = current_dir / name
    if candidate.exists():
        return candidate.resolve()
    for directory in INCLUDE_DIRS:
        candidate = directory / name
        if candidate.exists():
            return candidate.resolve()
    return None


def closure(entry: pathlib.Path) -> tuple[set[pathlib.Path], Counter[str]]:
    repo: set[pathlib.Path] = set()
    third_party: Counter[str] = Counter()
    seen: set[pathlib.Path] = set()
    stack = [entry.resolve()]
    while stack:
        path = stack.pop()
        if path in seen:
            continue
        seen.add(path)
        for name, angled in includes_of(path):
            resolved = resolve(name, path.parent)
            if resolved is not None and ROOT in resolved.parents:
                repo.add(resolved)
                stack.append(resolved)
            elif angled or resolved is None:
                third_party[name.split("/")[0]] += 1
    return repo, third_party


def strip_comments(text: str) -> str:
    text = re.sub(r"/\*.*?\*/", "", text, flags=re.S)
    return re.sub(r"//[^\n]*", "", text)


def body_of(text: str, start: int) -> str:
    depth = 0
    for index in range(start, len(text)):
        if text[index] == "{":
            depth += 1
        elif text[index] == "}":
            depth -= 1
            if depth == 0:
                return text[start + 1 : index]
    return ""


def struct_body(text: str, name: str) -> str | None:
    match = re.search(rf"\bstruct\s+{name}\b[^;{{]*\{{", strip_comments(text))
    if not match:
        return None
    return body_of(strip_comments(text), match.end() - 1)


def member_count(body: str) -> int:
    parts, depth, current = [], 0, ""
    for char in body:
        if char in "{(<":
            depth += 1
        elif char in "})>":
            depth -= 1
        if char == ";" and depth <= 0:
            parts.append(current)
            current = ""
        else:
            current += char
    count = 0
    for part in parts:
        declaration = part.strip()
        if not declaration:
            continue
        if re.search(r"\b(friend|using|typedef|static|struct|class|enum|union|template)\b", declaration):
            continue
        if "(" in declaration and "=" not in declaration:
            continue
        count += 1
    return count


def vector_elements(body: str) -> list[str]:
    return re.findall(r"std::vector<\s*([\w:]+)\s*>", body)


def header_for(type_name: str) -> pathlib.Path | None:
    short = type_name.split("::")[-1]
    for directory in INCLUDE_DIRS:
        candidate = directory / "ores.refdata.api" / "domain" / f"{short}.hpp"
        if candidate.exists():
            return candidate.resolve()
    return None


def main() -> int:
    entry = ROOT / UNIT
    protocol = ROOT / PROTOCOL
    repo, third_party = closure(entry)
    protocol_text = strip_comments(protocol.read_text())

    print(f"root      {ROOT}")
    print(f"unit      {UNIT}")
    print()
    print("include closure")
    print(f"  repository headers reached   {len(repo)}")
    print(f"  third-party include sites    {sum(third_party.values())}")
    print(f"  third-party roots            {', '.join(sorted(third_party))}")
    print()

    # The control: the same struct reflected in a unit of its own. If this unit
    # builds while the handler unit does not, the failing literal is not too
    # wide by itself and the difference is the accumulated context.
    control = ROOT / "projects/ores.refdata/api/src/domain/tenor_basis_swap_convention_json_io.cpp"
    control_repo, control_third = closure(control)
    print("control unit (same struct, unit of its own)")
    print(f"  unit                         {control.relative_to(ROOT)}")
    print(f"  repository headers reached   {len(control_repo)}")
    print(f"  third-party include sites    {sum(control_third.values())}")
    print()

    for document in ("conventions_document", "curve_configuration_document"):
        body = struct_body(protocol_text, document)
        if body is None:
            print(f"{document}: not found")
            continue
        elements = vector_elements(body)
        widths = []
        for element in elements:
            header = header_for(element)
            short = element.split("::")[-1]
            if header is None:
                inner = struct_body(protocol_text, short)
                widths.append((short, member_count(inner) if inner else -1))
                continue
            text = strip_comments(header.read_text(errors="replace"))
            inner = struct_body(text, short)
            widths.append((short, member_count(inner) if inner else -1))
        widths.sort(key=lambda item: -item[1])
        print(f"{document}: {len(elements)} reflected element types")
        for name, width in widths:
            print(f"  {width:>4}  {name}")
        print()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
