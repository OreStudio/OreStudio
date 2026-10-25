#!/usr/bin/env python3
"""Hold every hand-written list request in the BFF to the C++ protocol.

The services decode a list request strictly. A request that leaves out a member
the struct declares -- a filter member, or `as_of` even though it is optional --
is refused as bad_request, and a screen built on it shows an error instead of
rows. A request that reads the rows from a field the reply does not carry fails
the other way: the reply parses to an empty list and the screen shows blank
cells while reporting success.

Both faults are invisible in a unit test whose fixture repeats the mistake. This
reads the C++ request and response structs, which are the truth, and checks the
payloads the BFF writes by hand against them.

It reads only what it can see: a request literal, a filter literal, a rows key.
A payload assembled by a function is left alone, because a static reader cannot
know what it builds.
"""

from __future__ import annotations

import re
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
PROJECTS = ROOT / "projects"
BFF = PROJECTS / "ores.web" / "packages" / "bff" / "src"

REQUEST_KEYS = frozenset({"filter", "as_of", "offset", "limit", "order"})
STRUCT = re.compile(r"struct\s+(\w+)\s*\{(.*?)\n\};", re.S)
MEMBER = re.compile(r"^\s{4}(?:std::[^;]*?\s+)?(\w+)\s*(?:=\s*[^;]+)?;\s*$", re.M)


@dataclass
class Request:
    """What the C++ says a list request is: its filter and its reply's rows."""

    filter_type: str | None = None
    as_of: bool = False
    rows: tuple[str, ...] = field(default_factory=tuple)


def cpp_headers() -> list[dict[str, str]]:
    """The structs of each protocol header, kept apart.

    A struct name can appear in two components, so a filter is resolved inside
    the header that declares the request that names it.
    """
    headers = []
    for header in sorted(PROJECTS.glob("*/api/include/*/messaging/*_protocol.hpp")):
        headers.append({m.group(1): m.group(2) for m in STRUCT.finditer(header.read_text())})
    return headers


def members_of(body: str) -> tuple[str, ...]:
    names = []
    for line in body.splitlines():
        line = line.strip()
        if not line.endswith(";") or "(" in line:
            continue
        declared = re.match(r"(?:std::[^=;]*?\s+)?(\w+)\s*(?:=[^;]*)?;$", line)
        if declared and not line.startswith("using ") and not line.startswith("static "):
            names.append(declared.group(1))
    return tuple(names)


def requests(headers: list[dict[str, str]]) -> dict[str, Request]:
    """Each list request: the filter struct it names and whether it carries as_of."""
    found: dict[str, Request] = {}
    for structs in headers:
      for name, body in structs.items():
        if not re.fullmatch(r"list_\w+_request", name):
            continue
        filter_type = None
        named = re.search(r"std::optional<(\w+)>\s+filter\s*;", body)
        if named:
            filter_type = named.group(1)
        reply = name.replace("_request", "_response")
        rows = tuple(
            member
            for member in members_of(structs.get(reply, ""))
            if "vector<" in structs.get(reply, "").split(member, 1)[0][-60:]
        )
        found[name] = Request(
            filter_type=filter_type,
            as_of=bool(re.search(r"std::optional<[^>]+>\s+as_of\s*;", body)),
            rows=rows,
        )
    return found


def literal_at(text: str, start: int) -> tuple[str, int] | None:
    """The object literal beginning at the first `{` after `start`, and its end."""
    opening = text.find("{", start)
    if opening == -1:
        return None
    depth = 0
    for index in range(opening, len(text)):
        if text[index] == "{":
            depth += 1
        elif text[index] == "}":
            depth -= 1
            if depth == 0:
                return text[opening : index + 1], index + 1
    return None


def top_level_keys(literal: str) -> tuple[str, ...]:
    """The keys a literal states, ignoring the ones nested in a value."""
    inner = literal.strip()
    if inner.startswith("{") and inner.endswith("}"):
        inner = inner[1:-1]
    keys: list[str] = []
    depth = 0
    index = 0
    expect_key = True
    while index < len(inner):
        character = inner[index]
        if character in "([{":
            depth += 1
            expect_key = False
        elif character in ")]}":
            depth -= 1
        elif depth == 0 and character == ",":
            expect_key = True
        elif depth == 0 and not character.isspace() and expect_key:
            named = re.match(r"([A-Za-z_]\w*)\s*:", inner[index:])
            if named:
                keys.append(named.group(1))
                index += named.end()
                expect_key = False
                continue
            # A shorthand property states a member without a colon: `filter,`.
            shorthand = re.match(r"([A-Za-z_]\w*)\s*[,}]", inner[index:] + "}")
            if shorthand:
                keys.append(shorthand.group(1))
                index += shorthand.end() - 1
                expect_key = False
                continue
            expect_key = False
        index += 1
    return tuple(keys)


def read_all_sites(text: str) -> list[tuple[int, str, str]]:
    """Each readAll call: the subject expression, the rows key, and the offset."""
    sites = []
    for match in re.finditer(r"readAll\(\s*session\s*,", text):
        rest = text[match.end() :]
        parts = split_arguments(rest)
        if len(parts) < 2:
            continue
        subject = parts[1].strip().rsplit(".", 1)[-1]
        rows = parts[2].strip().strip("'\"") if len(parts) > 2 else ""
        if subject and rows:
            sites.append((match.start(), subject, rows))
    return sites


def split_arguments(text: str) -> list[str]:
    """The first few arguments of a call, split at top-level commas."""
    parts, depth, current = [], 0, []
    for index, character in enumerate(text):
        if character in "([{":
            depth += 1
        elif character in ")]}":
            if depth == 0:
                parts.append("".join(current))
                return parts
            depth -= 1
        if character == "," and depth == 0:
            parts.append("".join(current))
            current = []
            continue
        current.append(character)
    parts.append("".join(current))
    return parts


def is_call_argument(text: str, start: int, end: int) -> bool:
    """Whether this mention is a value passed to a call.

    The same name appears as a key in the tables that map a resource to its
    subjects, where the object that follows is the table entry and not a
    request. A payload is a mention preceded by an opening parenthesis or a
    comma, and not followed by a colon.
    """
    index = start
    while index > 0 and (text[index - 1].isalnum() or text[index - 1] in "_."):
        index -= 1
    before = text[:index].rstrip()
    if not before or before[-1] not in "(,":
        return False
    after = text[end:].lstrip()
    return not after.startswith(":")


def read_all_spans(text: str) -> list[tuple[int, int]]:
    """The character span of every readAll call, which completes its payload."""
    spans = []
    for match in re.finditer(r"\breadAll\(", text):
        depth = 0
        for index in range(match.end() - 1, len(text)):
            if text[index] == "(":
                depth += 1
            elif text[index] == ")":
                depth -= 1
                if depth == 0:
                    spans.append((match.start(), index))
                    break
    return spans


def check_file(path: Path, requests_by_name: dict[str, Request], structs: dict[str, str]) -> list[str]:
    text = path.read_text()
    problems: list[str] = []
    relative = path.relative_to(ROOT)

    for match in re.finditer(r"\b(list_\w+_request)\b", text):
        name = match.group(1)
        request = requests_by_name.get(name)
        if request is None:
            continue
        if not is_call_argument(text, match.start(), match.end()):
            continue
        line = text[: match.start()].count("\n") + 1
        literal = literal_at(text, match.end())
        if literal is None:
            continue
        body, _ = literal
        keys = top_level_keys(body)
        # A payload carries the request's own members. A table that maps a
        # resource to its subjects names the same constant with no literal after
        # it, so this keeps the two apart without guessing at layout.
        if not keys or not REQUEST_KEYS.intersection(keys):
            continue
        # readAll completes the payload from the generated table, so a call that
        # goes through it need not state every member at the call site.
        if any(start <= match.start() <= end for start, end in read_all_spans(text)):
            continue
        if request.as_of and "as_of" not in keys:
            problems.append(
                f"{relative}:{line} {name} leaves out as_of, which the request declares"
            )
        if request.filter_type is None:
            continue
        if "filter" not in keys:
            problems.append(f"{relative}:{line} {name} states no filter at all")
            continue
        stated = re.search(r"\bfilter\s*:", body)
        if stated is None:
            # A filter held in a variable: a static reader cannot know what it
            # carries, so it is left to the tests.
            continue
        if not body[stated.end() :].lstrip().startswith("{"):
            # Built by a function, whose own argument names are not wire members.
            continue
        inner = literal_at(body, stated.end())
        if inner is None:
            continue
        filter_body, _ = inner
        if filter_body.strip() == "{}":
            continue
        expected = set(members_of(structs.get(request.filter_type, "")))
        given = set(top_level_keys(filter_body))
        for missing in sorted(expected - given):
            problems.append(
                f"{relative}:{line} {name} filter leaves out {missing}, which the filter declares"
            )
        for unknown in sorted(given - expected):
            problems.append(
                f"{relative}:{line} {name} filter states {unknown}, which the filter does not declare"
            )

    for offset, subject, rows in read_all_sites(text):
        request = requests_by_name.get(subject)
        if request is None or not request.rows:
            continue
        if rows not in request.rows:
            line = text[:offset].count("\n") + 1
            problems.append(
                f"{relative}:{line} reads {subject} rows from '{rows}', but the reply carries "
                + ", ".join(f"'{row}'" for row in request.rows)
            )
    return problems


def table_is_current() -> bool:
    """Whether the generated descriptor table matches the headers it is made from."""
    generator = Path(__file__).with_name("regenerate_list_request_descriptors.py")
    result = subprocess.run(
        [sys.executable, str(generator), "--check"],
        capture_output=True,
        text=True,
    )
    return result.returncode == 0


def main() -> int:
    headers = cpp_headers()
    structs = {name: body for header in headers for name, body in header.items()}
    requests_by_name = requests(headers)
    problems: list[str] = []
    for source in sorted(BFF.glob("*.ts")):
        if source.name.endswith(".test.ts"):
            continue
        problems.extend(check_file(source, requests_by_name, structs))
    if not table_is_current():
        problems.append(
            "the generated list request table is stale: run "
            "projects/ores.codegen/scripts/regenerate_list_request_descriptors.py"
        )
    if problems:
        for problem in problems:
            print(problem)
        print(f"\n{len(problems)} hand-written list request(s) disagree with the protocol.")
        return 1
    print(f"every hand-written list request agrees with the protocol ({len(requests_by_name)} requests read)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
