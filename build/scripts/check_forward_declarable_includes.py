#!/usr/bin/env python3
"""A header that only names a type includes its declaration, not its definition.

Why this exists
---------------

A header that passes a type around -- a parameter, a return, a member held by
pointer -- does not need the type's definition. Including the definition anyway
is what made a change to =context.hpp= rebuild most of the tree: 4497 headers
included it to say the name, and every object whose depfile listed it rebuilt
when a private member changed. The compiler accepts both spellings, so nothing
in the build reports the one that costs more.

The analysis in
doc/agile/versions/v0/sprint_30/break_the_large_compilation_dependencies/
records a decision for each type it found, and the types it decided to
forward-declare live in =forward_declarable_types.list= beside this script. The
check reads that list, so a new type joins by adding a line.

How it reads a header
---------------------

Three questions, in order, for each type in the list:

  * Does the header include the header that defines the type? An include of a
    header that reaches the definition indirectly is not the offender: the fix
    belongs in whichever header pulls it in, and that header is checked too.
  * Does it name the type? A name counts when it is spelled in full, when a
    =using= alias in the same header binds a shorter name to it, or when the
    header opens the type's namespace. A comment does not count, and neither
    does an include path.
  * Does it need the definition? It does when the name is used as a value: a
    parameter or member with no =*=/=&=, a base class, =T::= member access,
    =sizeof=, a construction, or an argument of a template that stores the type
    -- =std::vector=, =std::optional=, =std::variant= and the like. It does not
    when the name is a reference, a pointer, a =class= declaration, a =using=
    alias, or a =shared_ptr=/=weak_ptr= argument, whose deleter is type-erased.

A header that includes the definition, names the type and needs no definition
in it is the offender, and the fix is the type's declaration: the
forward-declaration header the list names, or a declaration written in place.

A generated header is reported once for its template, not once for each file,
because the fix is one line in the archetype and the next regeneration
overwrites anything else. The banner names the template.

What it cannot see
------------------

The reading is textual, so it errs towards silence: a use it does not recognise
is read as needing the definition, and a short name in a header that neither
aliases it nor opens its namespace is not read as a use at all. A header that
reaches the definition through another header is not an offender, and the
header that pulls it in may be in a component that has no business including it.
=std::unique_ptr= of the type is read as needing the definition, because the
destructor does unless it is declared out of line.

Offenders that existed when the rule was written sit in
=forward_declarable_includes.baseline=, one per line, so the debt stays visible
and the check fails only on one that is not there. Regenerate the baseline with
=--write-baseline= after deliberately fixing some.

Usage
-----

    python3 build/scripts/check_forward_declarable_includes.py
        [--root .] [--write-baseline] [--verbose]

Exit codes:
  0 -- no offender outside its baseline
  1 -- an offender is not in its baseline
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

LIST_PATH = Path(__file__).with_name("forward_declarable_types.list")
BASELINE_PATH = Path(__file__).with_name(
    "forward_declarable_includes.baseline")

GENERATED_MARKER = "AUTO-GENERATED FILE - DO NOT EDIT MANUALLY"
TEMPLATE_RE = re.compile(r"^\s*\*\s*Template:\s*(\S+)\s*$", re.MULTILINE)
INCLUDE_RE = re.compile(r'^[ \t]*#[ \t]*include[ \t]*[<"]([^>"]+)[>"]',
                        re.MULTILINE)
HEADER_SUFFIXES = (".hpp", ".h", ".ipp")

# Directories that hold no header of the tree.
SKIPPED_DIRECTORIES = {"venv", ".venv", "node_modules", "build", "_deps"}

# Templates whose argument may be incomplete in a declaration: the deleter is
# type-erased, so the pointer outlives the name. std::unique_ptr is deliberately
# absent -- its destructor needs the definition unless it is declared out of
# line, which a header cannot promise.
INCOMPLETE_FRIENDLY_TEMPLATES = frozenset({"shared_ptr", "weak_ptr"})

BASELINE_HEADER = (
    "# Headers that include the definition of a type they only name.\n"
    "# One per line as <type>:<key>, where <key> is a path under the root or\n"
    "# template:<archetype>. Regenerate with --write-baseline.\n"
)


@dataclass(frozen=True)
class Declarable:
    """One type a header may name without its definition."""

    name: str
    keyword: str
    defining_header: str
    forwarding_header: str | None

    @property
    def short(self) -> str:
        return self.name.rsplit("::", 1)[1]

    @property
    def namespace(self) -> str:
        return self.name.rsplit("::", 1)[0]

    def qualifiers(self) -> list[str]:
        """The qualifications a header may write before the name: =pq::=,
        =database::pq::, =ores::database::pq::=. The bare name is not one of
        them: it needs the header to open the namespace or alias the name."""
        parts = self.namespace.split("::")
        return ["::".join(parts[index:]) + "::" for index in range(len(parts))]


def read_declarables(path: Path) -> list[Declarable]:
    declarables = []
    for number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        fields = stripped.split()
        if len(fields) != 4:
            raise SystemExit(f"{path.name}:{number}: expected four fields, "
                             f"found {len(fields)}")
        name, keyword, defining, forwarding = fields
        if keyword not in ("class", "struct"):
            raise SystemExit(f"{path.name}:{number}: class key must be class "
                             f"or struct, found {keyword!r}")
        declarables.append(Declarable(
            name=name,
            keyword=keyword,
            defining_header=defining,
            forwarding_header=None if forwarding == "-" else forwarding,
        ))
    if not declarables:
        raise SystemExit(f"{path.name}: no type listed, so the guard is off")
    return declarables


def strip_comments(text: str) -> str:
    """Drops comments and keeps every newline, so line numbers still hold."""
    out: list[str] = []
    index, length = 0, len(text)
    while index < length:
        char = text[index]
        if char == "/" and text.startswith("//", index):
            while index < length and text[index] != "\n":
                index += 1
        elif char == "/" and text.startswith("/*", index):
            index += 2
            while index + 1 < length and not text.startswith("*/", index):
                if text[index] == "\n":
                    out.append("\n")
                index += 1
            index += 2
        elif char in "\"'":
            quote = char
            out.append(char)
            index += 1
            while index < length:
                if text[index] == "\\" and index + 1 < length:
                    out.append(text[index:index + 2])
                    index += 2
                    continue
                out.append(text[index])
                index += 1
                if out[-1] == quote:
                    break
        else:
            out.append(char)
            index += 1
    return "".join(out)


def body_of(text: str) -> str:
    """The text a type scan reads: comments gone, include lines blank."""
    return INCLUDE_RE.sub("", strip_comments(text))


def opens_namespace(decl: Declarable, text: str) -> bool:
    """True when a short name in this header resolves to the type unqualified."""
    if re.search(r"\busing\s+namespace\s+" + re.escape(decl.namespace) + r"\s*;",
                 text):
        return True
    if re.search(r"\bnamespace\s+" + re.escape(decl.namespace) + r"\b[^;]*\{",
                 text):
        return True
    # The namespace opened one component at a time: outer, then the last part.
    parts = decl.namespace.split("::")
    if len(parts) > 1 and re.search(
            r"\bnamespace\s+" + re.escape("::".join(parts[:-1])) + r"\b[^;]*\{",
            text) and re.search(r"\bnamespace\s+" + re.escape(parts[-1]) + r"\b",
                                text):
        return True
    return False


def binds_short_name(decl: Declarable, text: str) -> bool:
    """True when a using alias or typedef in this header binds the short name."""
    return bool(re.search(
        r"\busing\s+" + re.escape(decl.short) + r"\s*=\s*"
        + r"(?:\w+::)*" + re.escape(decl.short) + r"\s*;", text))


def enclosing_template(text: str, position: int) -> str | None:
    """The name of the template whose argument list holds this position."""
    depth = 0
    index = position - 1
    while index >= 0:
        char = text[index]
        if char == ">":
            depth += 1
        elif char == "<":
            if depth == 0:
                match = re.search(r"([A-Za-z_]\w*)\s*$", text[:index])
                return match.group(1) if match else None
            depth -= 1
        elif char in ";{}" and depth == 0:
            return None
        index -= 1
    return None


def classify(decl: Declarable, text: str, start: int, end: int) -> str:
    """=names= when the position only names the type, =needs= otherwise."""
    before = text[:start]
    after = text[end:]
    following = after.lstrip()
    if re.search(r"(?:^|[^\w:])(?:class|struct|enum\s+class)\s*$", before):
        return "names"
    if following[:1] in ("*", "&"):
        return "names"
    if re.search(r"\b(?:using|typedef)\b[^;{}]*$", before):
        return "names"
    if following[:1] in (">", ",", ")"):
        template = enclosing_template(text, start)
        if template in INCOMPLETE_FRIENDLY_TEMPLATES:
            return "names"
        return "needs"
    return "needs"


def bound_names(decl: Declarable, text: str) -> set[str]:
    """The names in this header that refer to an object of the type.

    A header that only takes =const context&= may still call through it in an
    inline function, and that needs the definition. The names come from the
    reference and pointer parameters that spell the type, and from the smart
    pointer members that hold it.
    """
    spellings = [re.escape(qualifier) + re.escape(decl.short)
                 for qualifier in decl.qualifiers() + [""]]
    names = set()
    for spelling in spellings:
        parameter = re.compile(
            r"(?<![\w:])(?:const\s+)?" + spelling
            + r"\s*(?:const\s*)?[&*]+\s*(?:const\s+)?([A-Za-z_]\w*)")
        names.update(parameter.findall(text))
        member = re.compile(
            r"\b(?:shared_ptr|weak_ptr|unique_ptr)\s*<\s*" + spelling
            + r"\s*>\s*([A-Za-z_]\w*)")
        names.update(member.findall(text))
    return names


def dereferences(text: str, names: set[str]) -> bool:
    for name in names:
        if re.search(r"\b" + re.escape(name) + r"\s*(?:\.|->)", text):
            return True
    return False


def scan(decl: Declarable, text: str) -> str | None:
    """=names=, =needs= or None for one header's use of one type."""
    verdicts = set()
    for qualifier in decl.qualifiers():
        pattern = re.compile(r"(?<![\w:])" + re.escape(qualifier)
                             + re.escape(decl.short) + r"(?![\w])")
        for match in pattern.finditer(text):
            verdicts.add(classify(decl, text, match.start(), match.end()))
    if opens_namespace(decl, text) or binds_short_name(decl, text):
        pattern = re.compile(r"(?<![\w:])" + re.escape(decl.short) + r"(?![\w])")
        for match in pattern.finditer(text):
            # A bare name that a longer qualification already reached is the
            # same use, and it is classified the same way either way.
            verdicts.add(classify(decl, text, match.start(), match.end()))
    if "needs" in verdicts:
        return "needs"
    if dereferences(text, bound_names(decl, text)):
        return "needs"
    return "names" if verdicts else None


def headers(root: Path) -> list[Path]:
    found = []
    for path in (root / "projects").rglob("*"):
        if path.suffix not in HEADER_SUFFIXES or not path.is_file():
            continue
        if SKIPPED_DIRECTORIES.intersection(path.parts):
            continue
        found.append(path)
    return sorted(found)


@dataclass
class Offender:
    decl: Declarable
    path: Path
    line: int
    template: str | None


def find_offenders(root: Path,
                   declarables: list[Declarable]) -> list[Offender]:
    by_include: dict[str, list[Declarable]] = {}
    for decl in declarables:
        by_include.setdefault(decl.defining_header, []).append(decl)

    offenders = []
    for path in headers(root):
        text = path.read_text(errors="ignore")
        included = INCLUDE_RE.findall(text)
        own = relative_include(path)
        candidates = [decl for include in included
                      for decl in by_include.get(include, ())
                      if decl.defining_header != own]
        if not candidates:
            continue
        template = None
        if GENERATED_MARKER in text:
            match = TEMPLATE_RE.search(text[:2000])
            template = match.group(1) if match else "unknown"
        body = body_of(text)
        for decl in candidates:
            if scan(decl, body) == "names":
                offenders.append(Offender(
                    decl=decl, path=path,
                    line=line_number(text, decl.defining_header),
                    template=template))
    return offenders


def relative_include(path: Path) -> str:
    """The include path a header is written under: everything after include/."""
    parts = path.parts
    if "include" in parts:
        return "/".join(parts[len(parts) - parts[::-1].index("include"):])
    return path.name


def line_number(text: str, include: str) -> int:
    for number, line in enumerate(text.splitlines(), 1):
        if include in line and "#include" in line:
            return number
    return 0


def key(offender: Offender, root: Path) -> str:
    if offender.template:
        return f"{offender.decl.name}:template:{offender.template}"
    return f"{offender.decl.name}:{offender.path.relative_to(root).as_posix()}"


def read_baseline(path: Path) -> tuple[set[str], dict[str, str]]:
    """The recorded offenders, and the reason beside each one that has one."""
    if not path.exists():
        raise FileNotFoundError(
            f"the baseline {path.as_posix()} is missing; a type with no debt "
            f"still needs its entry elsewhere, or the guard is silently off")
    keys: set[str] = set()
    reasons: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        key, _, reason = line.partition("#")
        keys.add(key.strip())
        if reason.strip():
            reasons[key.strip()] = reason.strip()
    return keys, reasons


def describe(offenders: list[Offender], root: Path, verbose: bool) -> None:
    grouped: dict[str, list[Offender]] = {}
    for offender in offenders:
        grouped.setdefault(key(offender, root), []).append(offender)
    for entry in sorted(grouped):
        group = grouped[entry]
        first = group[0]
        fix = (f"include {first.decl.forwarding_header} instead"
               if first.decl.forwarding_header
               else f"declare {first.decl.keyword} {first.decl.name};")
        location = (f"{first.path.relative_to(root).as_posix()}:{first.line}"
                    if not first.template else
                    f"template {first.template} ({len(group)} file(s))")
        print(f"{entry}")
        print(f"    {location} -> {fix}")
        if verbose and not first.template:
            for offender in group[1:]:
                print(f"    {offender.path.relative_to(root).as_posix()}"
                      f":{offender.line}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=".", help="repository root")
    parser.add_argument("--write-baseline", action="store_true",
                        help="record every offender found, then pass")
    parser.add_argument("--verbose", action="store_true",
                        help="print every file of a generated offender")
    args = parser.parse_args()

    root = Path(args.root).resolve()
    if not (root / "projects").is_dir():
        print(f"error: {root}/projects is missing", file=sys.stderr)
        return 1

    declarables = read_declarables(LIST_PATH)
    offenders = find_offenders(root, declarables)
    keys = {key(offender, root) for offender in offenders}

    if args.write_baseline:
        # A reason written beside an entry survives a regeneration, so the one
        # offender that cannot be fixed stays explained.
        reasons = read_baseline(BASELINE_PATH)[1] if BASELINE_PATH.exists() else {}
        lines = [f"{key}  # {reasons[key]}" if key in reasons else key
                 for key in sorted(keys)]
        BASELINE_PATH.write_text(BASELINE_HEADER + "\n".join(lines) + "\n",
                                 encoding="utf-8")
        print(f"recorded {len(keys)} offender(s) in {BASELINE_PATH.name}")
        return 0

    known, _ = read_baseline(BASELINE_PATH)
    new = sorted(keys - known)
    fixed = sorted(known - keys)
    stale = sorted(k for k in fixed if not any(
        k.startswith(decl.name + ":") for decl in declarables))

    if keys:
        print(f"Headers that include a definition they only name: {len(keys)} "
              f"distinct offender(s) over {len(offenders)} file(s).")
        describe(offenders, root, args.verbose)
    else:
        print("Headers that include a definition they only name: none.")

    if fixed:
        print(f"\nFixed since the baseline was written: {len(fixed)}")
        for entry in fixed:
            print(f"  {entry}  (remove it from {BASELINE_PATH.name})")
    for entry in stale:
        print(f"  {entry}  (its type is no longer listed; remove the line)")

    if new:
        print(f"\nNEW offenders, which this check exists to prevent: "
              f"{len(new)}")
        for entry in new:
            print(f"  {entry}")
        print(f"\nEither take the type's declaration, or add the line to "
              f"{BASELINE_PATH.name} if the include is genuinely needed.")
        return 1

    print(f"Every offender is in {BASELINE_PATH.name}.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
