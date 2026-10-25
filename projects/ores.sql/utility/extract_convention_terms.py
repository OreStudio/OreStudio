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
"""Extract the terms of ORE's conventions document from the ORE source and guide.

The source is the structure. Each convention class's fromXML names its element with
checkNode and reads each term with getChildValue or one of its typed forms, saying
whether the term is mandatory; build turns a term's text into a value with a parse
function, which says the term's type. A term read inside a branch is mandatory only
on that branch, so it is recorded as conditional, not required.

The guide is the prose. Each convention section lists its elements as
"Name [Optional]: text", and that text is the term's description.

Every term records where each fact came from: the source file and line of the read,
and the guide line of the text. The result, convention_terms.json beside this file,
is derived: run this script again when ORE changes, and read the counts it prints.

    extract_convention_terms.py [--ore /path/to/ORE/Engine]

The ORE checkout comes from --ore, else from the environment variable ORES_ORE_ENGINE,
else from the default the guide extractor also uses.
"""

import argparse
import json
import os
import re
import subprocess

DEFAULT_ORE = os.environ.get("ORES_ORE_ENGINE", "/home/marco/Vault/Development/ORE/Engine")
SOURCE = "OREData/ored/configuration/conventions.cpp"
PARSERS = "OREData/ored/utilities/parsers.cpp"
GUIDE = "Docs/UserGuide/parameterisation/conventions.tex"
OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "convention_terms.json")

FUNCTION = re.compile(r"^(?:void|XMLNode\*) ([\w:]+)::(fromXML|build)\((?:XMLNode\* \w+)?\)\s*(?:const\s*)?\{",
                      re.M)
CHECK = re.compile(r'XMLUtils::checkNode\((\w+), "(\w+)"\)')
CHILD_NODE = re.compile(r'(\w+)\s*=\s*XMLUtils::getChildNode\((\w+), "(\w+)"\)')
READ = re.compile(
    r'(?:(\w+)\s*=\s*)?(?:parse\w+\()?XMLUtils::(getChildValue(?:AsBool|AsInt|AsDouble)?|getChildrenValues'
    r'(?:AsDoubles)?)\((\w+), "(\w+)"(?:, "(\w+)")?(?:, (true|false))?')
PARSE = re.compile(r"\b(parse\w+)\((\w+)\)")

# The type a parse function, or a typed read, gives a term.
TYPE_OF = {
    "getChildValueAsBool": "boolean",
    "getChildValueAsInt": "integer",
    "getChildValueAsDouble": "double",
    "getChildrenValuesAsDoubles": "double",
    "parseBool": "boolean",
    "parseInteger": "integer",
    "parseReal": "double",
    "parseCalendar": "calendar",
    "parseDayCounter": "day_counter",
    "parseBusinessDayConvention": "business_day_convention",
    "parseFrequency": "frequency",
    "parsePeriod": "period",
    "parseCurrency": "currency",
    "parseIborIndex": "index",
    "parseZeroInflationIndex": "index",
    "parseDateGenerationRule": "date_generation_rule",
    "parseMonth": "month",
    "parseWeekday": "weekday",
    "parseCompounding": "compounding",
    "parseSubPeriodsCouponType": "enumerated",
    "parseRateCutoff": "integer",
    "parseAndValidateFrequency": "frequency",
}


PARSER_DEF = re.compile(r"^[\w:<>]+ (parse\w+)\(const (?:std::)?string& \w+\)")
MAP_KEY = re.compile(r'\{\s*"([^"]+)"\s*,\s*([\w:]+)')
COMPARED = re.compile(r'\(\s*\w+\s*==\s*"([^"]+)"\s*\)\s*\{?\s*return\s+([\w:]+)')


def parser_values(text, relative):
    """The strings each parser accepts, in its own order, with its line.

    A parser states them in one of two ways: a static map at its head, or a chain of
    comparisons, each returning an enumerator. The whole body is read, to its closing
    brace, so neither form depends on how long it is. Each string is recorded with the
    value it means, the last part of that enumerator, so "A" means Annual and a person
    reads the word, not the code. A parser that states neither, such as one that reads
    a number, has no entry.
    """
    out = {}
    lines = text.split("\n")
    for index, line in enumerate(lines):
        match = PARSER_DEF.match(line)
        if not match or line.rstrip().endswith("}"):
            continue
        end = index + 1
        while end < len(lines) and not lines[end].startswith("}"):
            end += 1
        body = "\n".join(lines[index + 1:end])
        found = [{"value": value, "means": target.rsplit("::", 1)[-1]}
                 for value, target in MAP_KEY.findall(body) + COMPARED.findall(body)]
        if found:
            out[match.group(1)] = {"values": found, "source_ref": f"{relative}:{index + 1}"}
    return out


def git_head(root):
    try:
        return subprocess.run(["git", "-C", root, "rev-parse", "--short", "HEAD"],
                              capture_output=True, text=True, check=True).stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return "unknown"


def functions(text):
    """Each fromXML and build body, with the line it starts on."""
    out = []
    for match in FUNCTION.finditer(text):
        depth, i = 1, match.end()
        while depth and i < len(text):
            depth += {"{": 1, "}": -1}.get(text[i], 0)
            i += 1
        out.append({"cls": match.group(1), "kind": match.group(2),
                    "body": text[match.end():i - 1],
                    "line": text.count("\n", 0, match.start()) + 1,
                    "offset": match.end()})
    return out


def depth_at(body, pos):
    return body.count("{", 0, pos) - body.count("}", 0, pos)


def classes(text):
    """Every convention class's element, its terms in document order, and the nested elements."""
    found = {}
    for fn in functions(text):
        if fn["kind"] != "fromXML":
            continue
        body = fn["body"]
        check = CHECK.search(body)
        if not check:
            continue
        # A node variable is reused, so it is resolved at the place it is read: the last
        # assignment before that place says which element it holds.
        assigned = [(check.start(), check.group(1), [check.group(2)])]

        def path_of(var, pos):
            for start, name, path in reversed(assigned):
                if name == var and start <= pos:
                    return path
            return None

        terms, nested = [], {}
        for child in CHILD_NODE.finditer(body):
            parent = path_of(child.group(2), child.start())
            if parent is None:
                continue
            assigned.append((child.start(), child.group(1), parent + [child.group(3)]))
            assigned.sort(key=lambda a: a[0])
            # A node whose value is read directly is a term, not a container.
            after = body[child.end():child.end() + 240]
            value = re.search(r"(parse\w+)?\(?XMLUtils::getNodeValue\(" + child.group(1) + r"\)", after)
            if value and "getChildNode" not in after[:value.start()]:
                line = text.count("\n", 0, fn["offset"] + child.start()) + 1
                terms.append({
                    "path": parent[1:], "name": child.group(3), "member": None,
                    "read": "getNodeValue", "parser": value.group(1), "list_item": None,
                    "mandatory": False, "conditional": depth_at(body, child.start()) > 0,
                    "source_ref": f"{SOURCE}:{line}", "_pos": child.start(),
                })
        for read in READ.finditer(body):
            var, how, node, name, single, mandatory = read.groups()
            path = path_of(node, read.start())
            if path is None:
                continue
            line = text.count("\n", 0, fn["offset"] + read.start()) + 1
            terms.append({
                "path": path[1:],
                "name": name,
                "member": var,
                "read": how,
                "list_item": single,
                "mandatory": mandatory == "true",
                "conditional": depth_at(body, read.start()) > 0,
                "source_ref": f"{SOURCE}:{line}",
                "_pos": read.start(),
            })
        terms.sort(key=lambda t: t.pop("_pos"))
        seen, unique = set(), []
        for term in terms:
            key = tuple(term["path"] + [term["name"]])
            if key not in seen:
                seen.add(key)
                unique.append(term)
        terms = unique
        for call in re.finditer(r"(\w+)(?:\.|->)fromXML\((\w+)\)", body):
            path = path_of(call.group(2), call.start())
            if path is not None and len(path) > 1:
                nested[call.group(1)] = {"path": path[1:],
                                         "conditional": depth_at(body, call.start()) > 0}
        found[fn["cls"]] = {"element": check.group(2), "terms": terms, "nested": nested,
                            "source_ref": f"{SOURCE}:{fn['line']}"}
    for fn in functions(text):
        if fn["kind"] != "build" or fn["cls"] not in found:
            continue
        parsers = {member: parser for parser, member in PARSE.findall(fn["body"])}
        for term in found[fn["cls"]]["terms"]:
            if term.get("parser") is None and term["member"] in parsers:
                term["parser"] = parsers[term["member"]]
    for term in (t for c in found.values() for t in c["terms"]):
        term.setdefault("parser", None)
        term["type"] = TYPE_OF.get(term["parser"] or term["read"], "text")
    return found


NAMED = re.compile(r"node with name\s+(?:\\emph\{(\w+)\}|\\lstinline!(\w+)!)")
ITEM = re.compile(r"\s*\\item\s+(?:\\lstinline!(\w+)!|\\emph\{(\w+)\}|\{\\tt (\w+)\}|(\w+))"
                  r"(\s*\[[Oo]ptional\])?\s*(?::|--+)\s*(.*)")


def guide_texts(lines):
    """The guide's "Name [Optional]: text" items, by the element their section names.

    A section names its element in the sentence "A node with name X is used to store";
    the root of its first listing stands in where the sentence is missing. An item whose
    name is not a term of the element, such as a value listed in prose, is never matched
    to a term, so it does no harm here.
    """
    out, element, named, in_list = {}, None, False, False
    for number, line in enumerate(lines, 1):
        if line.startswith("\\subsubsection"):
            element, named, in_list = None, False, False
        name = NAMED.search(line)
        if name and not named:
            element, named = name.group(1) or name.group(2), True
        root = re.match(r"\s*<(\w+)>\s*$", line)
        if root and element is None:
            element = root.group(1)
        if "\\begin{itemize}" in line:
            in_list = True
        item = ITEM.match(line)
        if in_list and item and element:
            term = next(g for g in item.groups()[:4] if g)
            text = item.group(6).strip()
            k = number
            while k < len(lines) and not re.match(r"\s*\\(item|end)", lines[k]):
                text += " " + lines[k].strip()
                k += 1
            out.setdefault(element, {}).setdefault(term, {
                "optional": bool(item.group(5)),
                "text": clean(text),
                "guide_ref": f"{GUIDE}:{number}",
            })
    return out


def clean(text):
    text = re.sub(r"\\lstinline!([^!]*)!", r"\1", text)
    text = re.sub(r"``|''", '"', text)
    text = re.sub(r"\\(?:emph|textit|textbf|texttt|tt)\{([^}]*)\}", r"\1", text)
    text = re.sub(r"\{\\(?:tt|em|it|bf) ([^}]*)\}?", r"\1", text)
    text = re.sub(r"\$([^$]*)\$", r"\1", text)
    text = re.sub(r"`([^`']*)'", r"\1", text)
    text = re.sub(r"\\(?:ref|label|cite)\{[^}]*\}", "the guide", text)
    text = text.replace("\\_", "_").replace("\\%", "%").replace("~", " ")
    return re.sub(r"\s+", " ", text).strip()


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--ore", default=DEFAULT_ORE, help="an ORE checkout")
    args = parser.parse_args()
    for needed in (SOURCE, GUIDE, PARSERS):
        if not os.path.exists(os.path.join(args.ore, needed)):
            parser.error(f"{os.path.join(args.ore, needed)} does not exist; pass --ore or set "
                         "ORES_ORE_ENGINE to an ORE checkout")
    with open(os.path.join(args.ore, SOURCE)) as f:
        found = classes(f.read())
    with open(os.path.join(args.ore, GUIDE)) as f:
        texts = guide_texts(f.read().split("\n"))
    by_element = {c["element"]: c for c in found.values()}
    conventions = {}
    for cls, c in sorted(found.items(), key=lambda kv: kv[1]["element"]):
        if "::" in cls and cls.rsplit("::", 1)[0] in found:
            continue
        if c["element"] == "Conventions":
            continue
        guide = texts.get(c["element"], {})
        terms = []
        for term in c["terms"]:
            terms.append(dict(term, path=[c["element"]] + term["path"]))
        for child_cls, child in found.items():
            if child_cls.rsplit("::", 1)[0] != cls:
                continue
            for reach in c["nested"].values():
                if reach["path"][-1] == child["element"]:
                    # A term of an element the parent reads only on a branch is
                    # needed only on that branch.
                    for term in child["terms"]:
                        terms.append(dict(term, path=[c["element"]] + reach["path"] + term["path"],
                                          conditional=term["conditional"] or reach["conditional"]))
        for term in terms:
            words = guide.get(term["name"]) or {}
            term["guide_text"] = words.get("text", "")
            term["guide_optional"] = words.get("optional")
            term["guide_ref"] = words.get("guide_ref", "")
        conventions[c["element"]] = {"class": cls, "source_ref": c["source_ref"], "terms": terms}
    maps = {}
    for relative in (SOURCE, PARSERS):
        with open(os.path.join(args.ore, relative)) as f:
            maps.update(parser_values(f.read(), relative))
    used = {t["parser"] for c in conventions.values() for t in c["terms"] if t["parser"]}
    result = {"ore_commit": git_head(args.ore), "source": SOURCE, "guide": GUIDE,
              "parsers": {name: maps[name] for name in sorted(used) if name in maps},
              "conventions": conventions}
    with open(OUT, "w") as f:
        json.dump(result, f, indent=2, sort_keys=True)
        f.write("\n")
    all_terms = [t for c in conventions.values() for t in c["terms"]]
    print(f"Wrote {OUT} at ORE {result['ore_commit']}: {len(conventions)} conventions, "
          f"{len(all_terms)} terms, {sum(1 for t in all_terms if t['guide_text'])} with guide text, "
          f"{sum(1 for t in all_terms if t['type'] != 'text')} typed, "
          f"{sum(1 for t in all_terms if t['conditional'])} conditional.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
