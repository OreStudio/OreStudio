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
"""The curve configuration terms, read from the ORE source.

The source is the structure. `CurveConfigurations::fromXML` names every section
and the element its entries use, in one `getNode(node, "<Section>", "<Entry>")`
call per family. Each entry is parsed by a class whose `fromXML` names its
element with `checkNode` and reads each leaf in one of three ways:

- `getChildValue(node, "Leaf", ...)`, which names the leaf and says whether it
  is mandatory and, where the code gives one, its default;
- `getChildNode(node, "Leaf")` into a variable whose value is then read with
  `getNodeValue`, which names a leaf the same way with a looser type;
- `getChildNode(node, "Leaf")` into a variable whose own children are read,
  which makes `Leaf` a block that holds leaves.

A block is resolved two ways. A member parses it (`bootstrapConfig_.fromXML(n)`)
and the member's type names the class; or the parser dispatches on the node's
name (`Segments`, whose children are `Direct`, `Simple` and the rest) and each
alternative is a block of its own. A derived config may inherit `fromXML` from
a base class, which parses the same node, so the base's leaves belong to the
derived entry as well. A block that none of these resolve is recorded with the
source line that reads it, so the gap is counted rather than dropped.

Every term records where each fact came from: the source file and the line of
the read.

    extract_curve_terms.py [--ore /path/to/ORE/Engine]

The ORE checkout comes from --ore, else from the environment variable
ORES_ORE_ENGINE, else from the default the guide extractor also uses.
"""

import argparse
import json
import os
import re
import subprocess
import sys

DEFAULT_ORE = os.environ.get("ORES_ORE_ENGINE", "/home/marco/Vault/Development/ORE/Engine")
CONFIG = "OREData/ored/configuration"
DOCUMENT = os.path.join(CONFIG, "curveconfigurations.cpp")
GUIDE = "Docs/UserGuide/parameterisation/curveconfig.tex"
OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "curve_terms.json")

SCHEMA_NAME = "curveconfig.xsd"

GET_NODE = re.compile(r'getNode\(\s*\w+\s*,\s*"(\w+)"\s*,\s*"(\w+)"\s*\)')

FUNCTION = re.compile(
    r"^(?:void|XMLNode\*)\s+([\w:]+)::(fromXML|build)\(\s*XMLNode\*\s*(\w+)\s*\)", re.M)
INLINE = re.compile(r"^\s*(?:virtual\s+)?void\s+fromXML\(\s*XMLNode\*\s*(\w+)\s*\)", re.M)
CHECK = re.compile(r'XMLUtils::checkNode\(\s*(\w+)\s*,\s*"(\w+)"\s*\)')
READ = re.compile(
    r"XMLUtils::(getChildValue(?:AsBool|AsInt|AsDouble)?"
    r"|getChildrenValues(?:AsDoubles)?)\(\s*(\w+)\s*,\s*\"([\w.]+)\"\s*"
    r"(?:,\s*(true|false))?(?:\s*,\s*([^;]*?))?\s*\)")
CHILD = re.compile(
    r"(?:auto|XMLNode\*)\s+(\w+)\s*=\s*XMLUtils::getChildNode\(\s*(\w+)\s*,\s*\"([\w.]+)\"\s*\)")
NODEVALUE = re.compile(r"XMLUtils::getNodeValue\(\s*(\w+)\s*\)")
CALL = re.compile(r"(\w+)\.fromXML\(\s*(\w+)\s*\)")
DISPATCH = re.compile(r'childName\s*==\s*"(\w+)"')
CLASS = re.compile(r"^(?:class|struct)\s+(\w+)", re.M)
BASE = re.compile(r"^(?:class|struct)\s+(\w+)\s*:\s*public\s+([\w:]+)", re.M)
MEMBER = re.compile(r"^\s+([A-Za-z_][\w:<>]*)\s+(\w+)_\s*;", re.M)

TYPED = {
    "getChildValueAsBool": "boolean",
    "getChildValueAsInt": "integer",
    "getChildValueAsDouble": "double",
    "getChildrenValues": "text",
    "getChildrenValuesAsDoubles": "double",
    "getChildValue": "text",
}

ARGS = None


def read(path):
    with open(path, encoding="utf-8", errors="replace") as f:
        return f.read()


def body(text, start):
    """The text of a function body, from its opening brace to its matching one."""
    i = text.find("{", start)
    if i < 0:
        return ""
    depth = 0
    for j in range(i, len(text)):
        if text[j] == "{":
            depth += 1
        elif text[j] == "}":
            depth -= 1
            if depth == 0:
                return text[i:j + 1]
    return text[i:]


def line_of(text, offset):
    return text.count("\n", 0, offset) + 1


def ref(path, text, offset):
    return f"{os.path.relpath(path, ARGS.ore)}:{line_of(text, offset)}"


def literal(text):
    """The default the code states, or nothing where it computes one.

    A default argument is often an expression over the other members, such as
    `interpolationVariable_ == "Zero" ? "Linear" : "LogLinear`. That is a rule,
    not a value, so it is not recorded as the leaf's default.
    """
    value = (text or "").strip()
    if len(value) >= 2 and value[0] == '"' and value[-1] == '"':
        return value[1:-1]
    if re.fullmatch(r"-?\d+(\.\d+)?", value) or value in ("true", "false"):
        return value
    return None


def parse_function(path, text, chunk, start, node, element):
    """The leaves and blocks one `fromXML` body reads from its own node."""
    reads, nested = [], []

    def leaf(name, kind, mandatory, default, offset):
        return {"name": name, "type": kind, "mandatory": mandatory,
                "default": default, "source_ref": ref(path, text, start + offset)}

    for m in READ.finditer(chunk):
        call, read_node, name, mandatory, rest = m.groups()
        if read_node != node:
            continue
        reads.append(leaf(name, TYPED.get(call, "text"), mandatory == "true",
                          literal(rest), m.start()))

    valued = {m.group(1) for m in NODEVALUE.finditer(chunk)}
    calls = {m.group(2): m.group(1) for m in CALL.finditer(chunk)}
    children = list(CHILD.finditer(chunk))
    used = set()

    for m in children:
        var, parent, name = m.groups()
        if parent != node:
            continue
        if var in valued:
            reads.append(leaf(name, "text", False, None, m.start()))
            used.add(m.start())
        elif var in calls:
            nested.append({"element": name, "member": calls[var], "reads": []})
            used.add(m.start())

    # A child node whose own children are read is a block of leaves. Where the
    # parser dispatches on those children's names, the block holds alternatives.
    dispatched = {m.group(1) for m in re.finditer(r"getChildNode\(\s*(\w+)\s*\)", chunk)}
    for m in children:
        var, parent, name = m.groups()
        if parent != node or m.start() in used:
            continue
        if var in dispatched and DISPATCH.search(chunk):
            nested.append({"element": name, "member": None, "reads": [],
                           "dispatch": True})
            continue
        inner = [x for x in READ.finditer(chunk) if x.group(2) == var]
        block_reads = [{"name": x.group(3),
                        "type": TYPED.get(x.group(1), "text"),
                        "mandatory": x.group(4) == "true",
                        "default": literal(x.group(5)),
                        "source_ref": ref(path, text, start + x.start())}
                       for x in inner]
        nested.append({"element": name, "member": None, "reads": block_reads})

    return reads, nested


def classes(paths):
    """Every class that parses a document node, keyed by its name."""
    out = {}
    for path in paths:
        text = read(path)
        bases = {m.group(1): m.group(2) for m in BASE.finditer(text)}
        for match in FUNCTION.finditer(text):
            name = match.group(1)
            chunk = body(text, match.start())
            check = CHECK.search(chunk)
            reads, nested = parse_function(path, text, chunk, match.start(),
                                           match.group(3), check.group(2) if check else None)
            out.setdefault(name, {
                "element": check.group(2) if check else None,
                "base": bases.get(name),
                "reads": reads,
                "nested": nested,
                "dispatch": [m.group(1) for m in DISPATCH.finditer(chunk)],
                "source": os.path.relpath(path, ARGS.ore),
                "line": line_of(text, match.start()),
            })
    return out


def inline_classes(paths, found):
    """The parsers a header defines inside its class body.

    A small config declares `fromXML` inline, with no `Class::` qualifier and no
    source of its own, so the scan above cannot see it.
    """
    for path in paths:
        if not path.endswith(".hpp"):
            continue
        text = read(path)
        bases = {m.group(1): m.group(2) for m in BASE.finditer(text)}
        marks = list(CLASS.finditer(text))
        for index, mark in enumerate(marks):
            name = mark.group(1)
            if name in found:
                continue
            end = marks[index + 1].start() if index + 1 < len(marks) else len(text)
            chunk = text[mark.start():end]
            match = INLINE.search(chunk)
            if not match:
                # A config that only inherits its parser still has to be known,
                # so the walk can follow its base and name the element it parses.
                found[name] = {
                    "element": None,
                    "base": bases.get(name),
                    "reads": [],
                    "nested": [],
                    "dispatch": [],
                    "source": os.path.relpath(path, ARGS.ore),
                    "line": line_of(text, mark.start()),
                }
                continue
            fn = body(chunk, match.start())
            check = CHECK.search(fn)
            reads, nested = parse_function(path, text, fn,
                                           mark.start() + chunk.find("{", match.start()),
                                           match.group(1), check.group(2) if check else None)
            found[name] = {
                "element": check.group(2) if check else None,
                "base": bases.get(name),
                "reads": reads,
                "nested": nested,
                "dispatch": [],
                "source": os.path.relpath(path, ARGS.ore),
                "line": line_of(text, mark.start()),
            }
    return found


def members(paths):
    """The member names and types of every class, read from the headers."""
    out = {}
    for path in paths:
        text = read(path)
        current = None
        for line in text.splitlines():
            match = CLASS.match(line)
            if match and not line.rstrip().endswith(";"):
                current = match.group(1)
                out.setdefault(current, {})
                continue
            if current:
                match = MEMBER.match(line + "\n")
                if match:
                    out[current][match.group(2)] = match.group(1)
    return out


def resolve(member, node_class, found, members_of):
    """The class a member parses with: its declared type, else its own name."""
    kind = members_of.get(node_class, {}).get(member)
    if kind in found:
        return kind
    guess = member.rstrip("_").lower()
    for name in found:
        if name.lower() == guess:
            return name
    return None


def walk(node_class, found, members_of, by_element, path, depth, seen):
    """Collect the leaves under one class, following the blocks it parses."""
    entry = found.get(node_class)
    if not entry or node_class in seen or depth > 4:
        return []
    seen = seen | {node_class}
    terms = []
    base = entry.get("base")
    if base and base in found and base not in seen:
        terms.extend(walk(base, found, members_of, by_element, path, depth,
                          seen | {base}))
    for r in entry["reads"]:
        terms.append(dict(r, path=path + [r["name"]], element=entry["element"]))
    for name in entry["dispatch"]:
        child = by_element.get(name)
        if child:
            terms.extend(walk(child, found, members_of, by_element,
                              path + [name], depth + 1, seen))
        else:
            terms.append({"path": path + [name], "element": name, "name": None,
                          "unresolved": "dispatch with no class"})
    for block in entry["nested"]:
        element = block["element"]
        if block.get("dispatch"):
            for name in entry["dispatch"]:
                child = by_element.get(name)
                if child:
                    terms.extend(walk(child, found, members_of, by_element,
                                      path + [element, name], depth + 1, seen))
                else:
                    terms.append({"path": path + [element, name], "element": name,
                                  "name": None, "unresolved": "dispatch with no class"})
            continue
        kind = resolve(block["member"], node_class, found, members_of) if block["member"] else None
        child = found.get(kind) if kind else None
        if child and child["element"]:
            terms.extend(walk(kind, found, members_of, by_element,
                              path + [element], depth + 1, seen))
        elif block["reads"]:
            for r in block["reads"]:
                terms.append(dict(r, path=path + [element, r["name"]],
                                  element=element))
        else:
            terms.append({"path": path + [element], "element": element,
                          "name": None, "unresolved": "child node with no parser"})
    return terms


def dedupe(terms):
    """One term per leaf, where the parser reads the same leaf in two branches.

    A curve type reads `DayCounter` once for its piecewise form and once for its
    direct form. Both are the same leaf of the document, so they are one term,
    and the leaf is conditional because no branch always reads it.
    """
    out, seen = [], {}
    for term in terms:
        key = tuple(term["path"])
        first = seen.get(key)
        if first:
            first["mandatory"] = first["mandatory"] and term["mandatory"]
            first["conditional"] = True
            if first["default"] is None:
                first["default"] = term["default"]
            continue
        term.setdefault("conditional", False)
        seen[key] = term
        out.append(term)
    return out


GUIDE_DIRS = ("Docs/UserGuide/parameterisation", "Docs/UserGuide/curve_configurations")

# The guide names an element three ways: a plain `\item Name:`, a
# `\item \lstinline!Name! [Optional]:`, and a bare `\lstinline!Name!:`. Each
# entry runs to the next entry, so a name quoted inside the prose does not end it.
ITEM = re.compile(
    r"^[ \t]*(?:\\item\s+)?(?:\\lstinline!([\w.]+)!|([A-Za-z][\w]*))\s*"
    r"(?:\[([^\]]*)\])?\s*:\s*(.*?)"
    r"(?=^[ \t]*(?:\\item\s+|\\lstinline!)"
    r"|\\end\{itemize\}|\Z)",
    re.S | re.M)
EMPH = re.compile(r"\\emph\{([^}]*)\}")
LISTING = re.compile(r"\\lstinline!([^!]*)!")
REF = re.compile(r"\\ref\{[^}]*\}")


def clean(text):
    """The guide's prose as a sentence, with its LaTeX taken off."""
    text = EMPH.sub(r"\1", text)
    text = LISTING.sub(r"\1", text)
    text = re.sub(r"\$([^$]*)\$", r"\1", text)
    text = REF.sub("", text)
    text = text.replace("~", " ").replace("\\\\", " ")
    text = re.sub(r"\\[a-zA-Z]+\*?(\[[^\]]*\])?\{?", "", text)
    text = text.replace("{", "").replace("}", "")
    # A dropped reference leaves "in Table ." behind; name the guide instead.
    text = re.sub(r"\b(?:Table|Listing)\s*\.", "the guide.", text)
    text = re.sub(r"\b(?:Table|Listing)\s*$", "the guide", text)
    return re.sub(r"\s+", " ", text).strip()


FILE_SECTION = {
    "yieldcurves.tex": "YieldCurves",
    "default_curves_from_cds.tex": "DefaultCurves",
    "default_curves_other.tex": "DefaultCurves",
    "capfloorvolatility.tex": "CapFloorVolatilities",
    "commodity_curves.tex": "CommodityCurves",
    "commodity_volatilities.tex": "CommodityVolatilities",
    "bond_future_volatilities.tex": "BondFutureVolatilities",
    "intradaypowercurves.tex": "IntradayPowerCurves",
}

SUBSECTION = re.compile(r"\\subsubsection\{([^}]*)\}")


def stem(word):
    """A word with its plural taken off, so prose and code names agree."""
    word = word.lower()
    if word.endswith("ies") and len(word) > 4:
        return word[:-3] + "y"
    if word.endswith("s") and len(word) > 3:
        return word[:-1]
    return word


def words(text):
    """The significant words of a name, whether it is prose or camel case."""
    parts = re.findall(r"[A-Z]+(?![a-z])|[A-Z]?[a-z]+|[0-9]+", text)
    return {stem(w) for w in parts} - {"and", "the", "of"}


def alike(a, b):
    """Whether two words name the same idea: equal, or sharing a long stem.

    The guide writes "Volatility" where the document writes "Volatilities", so a
    shared stem counts as a match and exact equality is not required.
    """
    if a == b:
        return True
    shortest = min(len(a), len(b))
    return shortest >= 4 and a[:shortest] == b[:shortest]


def shared(heading, name):
    return sum(1 for a in heading if any(alike(a, b) for b in name))


def resolve_section(heading, sections):
    """The section a guide sub-heading describes, by the words they share.

    The guide names a family in prose ("Swaption Volatility Structures") and the
    document names it in code (=SwaptionVolatilities=). An exact match wins
    outright; otherwise the heading takes the family it shares most words with,
    and a tie is left unattributed rather than guessed.
    """
    normal = words(heading)
    for name in sections:
        if normal == words(name):
            return name
    best, score = None, 0
    for name in sections:
        count = shared(normal, words(name))
        if count > score:
            best, score = name, count
        elif count == score and count:
            best = None
    return best if score else None


def guide_entries(text, path, ore, section):
    """The `\\item Name [Optional]: text` entries of one guide file."""
    for match in ITEM.finditer(text):
        quoted, plain, optional, body = match.groups()
        leaf = quoted or plain
        prose = clean(body)
        if prose:
            yield (section, leaf), {
                "text": prose,
                "optional": (optional or "").strip().lower() == "optional",
                "source_ref": f"{os.path.relpath(path, ore)}:{line_of(text, match.start())}",
            }


def guide_terms(ore, sections):
    """The prose the guide gives each leaf, keyed by the section and the name.

    Where a name appears in more than one section the section decides which text
    it takes, so a generic name such as CurveId reads as its own family's words
    and not the first family the guide happens to describe.
    """
    found = {}
    for directory in GUIDE_DIRS:
        root = os.path.join(ore, directory)
        if not os.path.isdir(root):
            continue
        for name in sorted(os.listdir(root)):
            if not name.endswith(".tex"):
                continue
            path = os.path.join(root, name)
            text = read(path)
            section = FILE_SECTION.get(name)
            if section:
                for key, value in guide_entries(text, path, ore, section):
                    found.setdefault(key, value)
                continue
            # The chapter names each family under a sub-heading, so read it in
            # slices and attribute each slice to the family its heading names.
            marks = list(SUBSECTION.finditer(text))
            for index, mark in enumerate(marks):
                end = marks[index + 1].start() if index + 1 < len(marks) else len(text)
                part = text[mark.end():end]
                part_section = resolve_section(mark.group(1), sections)
                for key, value in guide_entries(part, path, ore, part_section):
                    found.setdefault(key, value)
    return found


def commit(ore):
    try:
        return subprocess.run(["git", "-C", ore, "rev-parse", "--short", "HEAD"],
                              capture_output=True, text=True, check=True).stdout.strip()
    except Exception:
        return "unknown"


def main():
    global ARGS
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--ore", default=DEFAULT_ORE,
                        help="the ORE Engine checkout to read")
    ARGS = parser.parse_args()
    root = os.path.join(ARGS.ore, CONFIG)
    if not os.path.isdir(root):
        print(f"No {CONFIG} under {ARGS.ore}; set ORES_ORE_ENGINE to an ORE checkout.",
              file=sys.stderr)
        return 1

    document = read(os.path.join(ARGS.ore, DOCUMENT))
    sections = [{"section": m.group(1), "entry_element": m.group(2),
                 "source_ref": f"{DOCUMENT}:{line_of(document, m.start())}"}
                for m in GET_NODE.finditer(document)]
    if not sections:
        print("The document names no section; the source changed shape.", file=sys.stderr)
        return 1

    paths = sorted(os.path.join(root, f) for f in os.listdir(root)
                   if f.endswith((".cpp", ".hpp")))
    found = inline_classes(paths, classes(paths))
    members_of = members(p for p in paths if p.endswith(".hpp"))
    by_element = {c["element"]: name for name, c in found.items() if c["element"]}
    guide = guide_terms(ARGS.ore, [x["section"] for x in sections])
    covered = 0

    out = {"schema": SCHEMA_NAME, "guide": GUIDE,
           "ore": {"path": ARGS.ore, "commit": commit(ARGS.ore)}, "sections": {}}
    total, unresolved = 0, 0
    for section in sections:
        entry = section["entry_element"]
        # A config with no `checkNode` of its own inherits its parser from a
        # base class, and is named for the element it parses.
        entry_class = by_element.get(entry)
        if not entry_class:
            for candidate in (f"{entry}CurveConfig", f"{entry}Config"):
                if candidate in found:
                    entry_class = candidate
                    break
        if not entry_class:
            out["sections"][section["section"]] = dict(
                section, entry_class=None, terms=[],
                unresolved=[{"path": [section["section"], entry],
                             "unresolved": "no parser for the entry element"}])
            unresolved += 1
            continue
        terms = walk(entry_class, found, members_of, by_element,
                     [section["section"], entry], 0, frozenset())
        leaves = dedupe([t for t in terms if t.get("name")])
        for term in leaves:
            prose = (guide.get((section["section"], term["name"]))
                     or guide.get((None, term["name"])))
            term["guide_text"] = prose["text"] if prose else ""
            term["guide_ref"] = prose["source_ref"] if prose else ""
            term["guide_optional"] = prose["optional"] if prose else None
            covered += 1 if prose else 0
        gaps = [t for t in terms if t.get("unresolved")]
        total += len(leaves)
        unresolved += len(gaps)
        out["sections"][section["section"]] = dict(
            section, entry_class=entry_class, terms=leaves, unresolved=gaps)

    with open(OUT, "w") as f:
        json.dump(out, f, indent=1, sort_keys=True)
        f.write("\n")
    print(f"Wrote {OUT}")
    print(f"{len(sections)} sections, {total} leaves ({covered} with guide text), {unresolved} unresolved blocks.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
