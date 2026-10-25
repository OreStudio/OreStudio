#!/usr/bin/env python3
"""Give every rule of the UX standards a stable identifier, and check them.

A rule is a top-level bullet in a keyed section of a keyed page, or a paragraph
that states a rule the page keeps as prose or as a table's lead. Its identifier
stands at the start of the bullet or the paragraph in bold, such as
=*RS-LIST-03*=: the page's code, the section's code and a number that is never reused. The identifier is
permanent: a rule that moves keeps it, so the section code says where the rule
was born and not where it lives.

Three rules:

1. Every bullet in a keyed section carries one identifier, and no identifier
   appears twice across the keyed pages.
2. A section code is one the page declares below; a bullet under a heading with
   no code is a mistake in the page or in this table.
3. Every line of the screen review checklist cites at least one identifier that
   exists, or cites a page this script does not key yet. A token shaped like an
   identifier with the wrong number of digits is a problem.

A bold identifier anywhere but the start of a rule is a reference to another
rule and is not checked. A rule the page keeps as prose is seen only once it
carries an identifier or a placeholder; a new paragraph rule with no marker is
invisible to this check.

Usage:
  python3 build/scripts/check_rule_identifiers.py            check
  python3 build/scripts/check_rule_identifiers.py --assign   give the bullets
      that lack one, and a paragraph marked =*RS-LIST-??*=, the next free
      identifier of their section, and check
Exits non-zero and names each problem.
"""
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
UI = ROOT / "doc" / "knowledge" / "ui"
CHECKLIST = UI / "screen_review_checklist.org"

# The keyed pages: their code, and a code for each level-two section whose
# bullets are rules. A section not named here holds no rules: a table, a status
# list, a reading order or links.
PAGES = {
    "RS": {
        "path": UI / "record_screen_standard.org",
        "sections": {
            "Where this standard applies": "SCOPE",
            "What a record kind declares": "DECL",
            "The layers": "LAYER",
            "Addresses": "ADDR",
            "The list": "LIST",
            "The record page": "PAGE",
            "Writes": "WRITE",
            "History": "HIST",
            "Related records": "RELATED",
            "Flags and images": "FLAG",
            "Icons and cells": "ICON",
            "Feedback, access and safety": "FEEDBACK",
            "Naming": "NAME",
            "Exceptions": "EXCEPT",
            "Building a record screen": "BUILD",
        },
    },
    "ES": {
        "path": UI / "entity_screen_specification.org",
        "sections": {
            "User journeys and screens": "JOURNEY",
            "The shape of an entity screen": "SHAPE",
            "The list": "LIST",
            "The page": "PAGE",
            "Changing a record": "EDIT",
            "References are links": "REF",
            "Workflows on the entity screen": "WORKFLOW",
            "People are links": "PEOPLE",
            "Photos, flags and images": "PHOTO",
            "Paging: the pager is the request": "PAGING",
            "History and the timeline": "HIST",
            "Several kinds of record in one entity": "ASPECT",
            "A trade, a structure and a package": "TRADE",
            "The entity protocol": "PROTOCOL",
            "Relationship views": "RELATION",
            "Who sees what": "SEES",
            "Proposals and four-eyes": "PROPOSE",
            "The shell around a screen": "SHELL",
            "Feedback, states and words": "WORDS",
            "Updates and refresh": "REFRESH",
            "What a screen or a form needs, and what the log keeps": "NEEDS",
            "Verifying a rule": "VERIFY",
        },
    },
}

# Pages the checklist cites that carry no identifiers yet. A line that cites
# only one of these passes rule 3.
UNKEYED = ("UXL", "UDP", "CFG", "FSS", "OLS")

START = re.compile(r"^(?:- )?\*((?:RS|ES)-[A-Z]+-\d{2})\* ")
PLACEHOLDER = re.compile(r"^(?:- )?\*((?:RS|ES)-[A-Z]+)-\?\?\* ")
BULLET = re.compile(r"^(?:- (?!\[ \])|\*(?:RS|ES)-[A-Z]+-(?:\d{2}|\?\?)\* )(.*)$")
HEADING = re.compile(r"^(\*+) (.+?)\s*$", re.M)
TOKEN = re.compile(r"\b((?:RS|ES)-[A-Z]+-\d+)\b")


def sections_of(text: str):
    """Yield (level-two heading, start, end) over the page."""
    heads = [(m.start(), len(m.group(1)), m.group(2)) for m in HEADING.finditer(text)]
    tops = [(s, lv, t) for s, lv, t in heads if lv <= 2]
    for i, (start, level, title) in enumerate(tops):
        if level != 2:
            continue
        end = tops[i + 1][0] if i + 1 < len(tops) else len(text)
        yield title, start, end


def rules(page_code: str, text: str):
    """Yield (section code or None, line index, line) for every bullet in a
    level-two section, skipping the level-one Summary and See also."""
    lines = text.split("\n")
    offsets = []
    pos = 0
    for line in lines:
        offsets.append(pos)
        pos += len(line) + 1
    sections = PAGES[page_code]["sections"]
    for title, start, end in sections_of(text):
        code = sections.get(title)
        in_block = False
        for i, line in enumerate(lines):
            if offsets[i] < start or offsets[i] >= end:
                continue
            if line.startswith("#+begin_"):
                in_block = True
            elif line.startswith("#+end_"):
                in_block = False
            if in_block:
                continue
            if BULLET.match(line):
                yield title, code, i, line


def assign(page_code: str) -> int:
    path = PAGES[page_code]["path"]
    text = path.read_text(encoding="utf-8")
    lines = text.split("\n")
    highest: dict[str, int] = {}
    for m in START.finditer(text):
        _, section, number = m.group(1).split("-")
        highest[section] = max(highest.get(section, 0), int(number))
    added = 0
    for title, code, i, line in list(rules(page_code, text)):
        if code is None or START.match(line):
            continue
        highest[code] = highest.get(code, 0) + 1
        ident = f"{page_code}-{code}-{highest[code]:02d}"
        if PLACEHOLDER.match(line):
            lines[i] = PLACEHOLDER.sub(f"*{ident}*", line, count=1)
        else:
            lines[i] = f"- *{ident}* {line[2:]}"
        added += 1
        print(f"  {ident}  {lines[i][:72]}")
    path.write_text("\n".join(lines), encoding="utf-8")
    return added


def check() -> list[str]:
    problems: list[str] = []
    seen: dict[str, str] = {}
    for page_code, page in PAGES.items():
        text = page["path"].read_text(encoding="utf-8")
        rel = page["path"].relative_to(ROOT).as_posix()
        for title, code, i, line in rules(page_code, text):
            start = START.match(line)
            if code is None:
                if start:
                    problems.append(f"{rel}:{i + 1}: a rule under '{title}', a section this script does not key")
                continue
            if not start:
                problems.append(f"{rel}:{i + 1}: a rule without an identifier at its start under '{title}'")
                continue
            ident = start.group(1)
            if not ident.startswith(f"{page_code}-"):
                problems.append(f"{rel}:{i + 1}: {ident} carries another page's code")
            if ident in seen:
                problems.append(f"{rel}:{i + 1}: {ident} is also at {seen[ident]}")
            seen[ident] = f"{rel}:{i + 1}"
    checklist = CHECKLIST.read_text(encoding="utf-8")
    rel = CHECKLIST.relative_to(ROOT).as_posix()
    for i, line in enumerate(checklist.split("\n")):
        if not line.startswith("- [ ]"):
            continue
        cited = TOKEN.findall(line)
        for token in cited:
            if not re.fullmatch(r"(?:RS|ES)-[A-Z]+-\d{2}", token):
                problems.append(f"{rel}:{i + 1}: {token} is not an identifier; two digits are required")
        unkeyed = any(f"({p} " in line or f"; {p} " in line or f", {p} " in line for p in UNKEYED)
        if not cited and not unkeyed:
            problems.append(f"{rel}:{i + 1}: a checklist line that cites no identifier")
        for ident in cited:
            if ident not in seen:
                problems.append(f"{rel}:{i + 1}: cites {ident}, which no page carries")
    return problems


def main() -> int:
    if "--assign" in sys.argv[1:]:
        for page_code in PAGES:
            n = assign(page_code)
            print(f"{page_code}: {n} identifier(s) assigned")
    problems = check()
    for problem in problems:
        print(problem)
    total = sum(1 for page_code, page in PAGES.items()
                for line in page["path"].read_text(encoding="utf-8").split("\n")
                if START.match(line))
    print(f"{total} identifiers, {len(problems)} problem(s)")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
