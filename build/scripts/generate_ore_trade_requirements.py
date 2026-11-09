#!/usr/bin/env python3
"""Generate the table of what ORE requires of each trade the generator books.

Reads three sources and writes one org table per concern:

- ORE's XSDs (external/ore/xsd), for the terms a trade must state.
- ORE's own samples (external/ore/examples), for the data element of a type.
- ORE's Engine source (--engine), for each type's class and pricing engine.

A required term that a recorded generated trade does not state is flagged,
because that is a term the trade builder must supply.

Run::

    python3 build/scripts/generate_ore_trade_requirements.py \\
        --engine /path/to/ORE/Engine --write
"""
from __future__ import annotations

import argparse
import re
import subprocess
import sys
import xml.etree.ElementTree as ET
from collections import Counter
from dataclasses import dataclass, field
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
XSD_DIR = REPO / "external/ore/xsd"
SAMPLES = REPO / "external/ore/examples"
RECORDED = REPO / "projects/ores.synthetic/core/tests/data/generated_trades"
DOCUMENT = REPO / "doc/knowledge/domain/ore_trade_requirements.org"
BEGIN = "# BEGIN GENERATED"
END = "# END GENERATED"
XS = "{http://www.w3.org/2001/XMLSchema}"
REGISTRATION = re.compile(r'ORE_REGISTER_TRADE_BUILDER\("(\w+)",\s*([\w:]+)')
ENGINE_REQUEST = re.compile(r'builder\(\s*(?:"(\w+)"|(tradeType_|tradeType\(\)))\s*\)')
BASE_CLASS = re.compile(r"class\s+(?:ORE\w+\s+)?(\w+)\s*(?:final\s*)?:\s*(?:virtual\s+)?public\s+([\w:]+)")
BUILD_START = re.compile(r"\b(\w+)::\w*[Bb]uild\w*\s*\([^{;]*\)\s*(?:const\s*)?\{")
GENERATOR_METHOD = re.compile(r"void\s+build(\w+)\s*\(")


def local_name(qualified: str) -> str:
    """An XSD reference without its namespace prefix."""
    return qualified.split(":")[-1]


@dataclass(frozen=True)
class Term:
    name: str
    xsd_type: str = ""


@dataclass
class Schema:
    """The named complex types and the elements a trade's data can be."""

    types: dict[str, ET.Element] = field(default_factory=dict)
    groups: dict[str, ET.Element] = field(default_factory=dict)

    @classmethod
    def load(cls, directory: Path) -> "Schema":
        schema = cls()
        for path in sorted(directory.glob("*.xsd")):
            root = ET.parse(path).getroot()
            for node in root.findall(f"{XS}complexType"):
                schema.types[node.get("name", "")] = node
            for node in root.findall(f"{XS}group"):
                schema.groups[node.get("name", "")] = node
        return schema

    def required(self, type_name: str) -> list[Term]:
        """The terms a type must state; a required choice is one term."""
        node = self.types.get(local_name(type_name))
        return [] if node is None else self._walk(node)

    def _walk(self, node: ET.Element) -> list[Term]:
        terms: list[Term] = []
        for child in node:
            tag = child.tag.removeprefix(XS)
            optional = child.get("minOccurs") == "0"
            if tag == "element" and not optional:
                terms.append(Term(local_name(child.get("name") or child.get("ref", "")),
                                  local_name(child.get("type", ""))))
            elif tag == "choice" and not optional:
                options = [o.name for o in self._alternatives(child)]
                if options:
                    terms.append(Term(" or ".join(options)))
            elif tag == "group" and not optional:
                group = self.groups.get(local_name(child.get("ref", "")))
                if group is not None:
                    terms += self._walk(group)
            elif tag == "extension":
                terms += self.required(child.get("base", "")) + self._walk(child)
            elif tag in ("sequence", "all", "complexContent") and not optional:
                terms += self._walk(child)
        return terms

    def _alternatives(self, choice: ET.Element) -> list[Term]:
        terms: list[Term] = []
        for child in choice:
            tag = child.tag.removeprefix(XS)
            if tag == "element":
                terms.append(Term(child.get("name", "")))
            elif tag == "sequence":
                terms += self._walk(child)[:1]
        return terms


def data_elements() -> dict[str, Counter[str]]:
    """Each trade type's data element, as ORE's own samples spell it."""
    found: dict[str, Counter[str]] = {}
    for path in SAMPLES.rglob("*.xml"):
        try:
            root = ET.parse(path).getroot()
        except ET.ParseError:
            continue
        for trade in root.iter("Trade"):
            kind = trade.findtext("TradeType")
            if not kind:
                continue
            for child in trade:
                if child.tag.endswith("Data"):
                    found.setdefault(kind.strip(), Counter())[child.tag] += 1
    return found


def element_types() -> dict[str, str]:
    """Trade data element name to XSD type, from every local declaration."""
    out: dict[str, str] = {}
    for path in sorted(XSD_DIR.glob("*.xsd")):
        for node in ET.parse(path).getroot().iter(f"{XS}element"):
            if node.get("name", "").endswith("Data") and node.get("type"):
                out.setdefault(node.get("name"), local_name(node.get("type")))
    return out


def scan_engine(engine: Path) -> tuple[dict[str, str], dict[str, list[str]], set[str], set[str], str]:
    roots = [engine / "OREData/ored", engine / "OREAnalytics/orea"]
    text = {}
    for root in roots:
        for path in list(root.rglob("*.cpp")) + list(root.rglob("*.hpp")):
            text[path] = re.sub(r"//[^\n]*|/\*.*?\*/", "", path.read_text(errors="replace"), flags=re.S)
    registered: dict[str, str] = {}
    bases: dict[str, str] = {}
    build_bodies: dict[str, list[str]] = {}
    for source in text.values():
        for name, cls in REGISTRATION.findall(source):
            registered[name] = cls.split("::")[-1]
        for cls, base in BASE_CLASS.findall(source):
            bases.setdefault(cls, base.split("::")[-1])
        for match in BUILD_START.finditer(source):
            build_bodies.setdefault(match.group(1), []).append(_body(source, match.end() - 1))
    engines: dict[str, list[str]] = {}
    scripted: set[str] = set()
    for name, cls in registered.items():
        requested: list[str] = []
        for link in _chain(cls, bases):
            if link == "ScriptedTrade":
                scripted.add(name)
            for body in build_bodies.get(link, []):
                for literal, own_type in ENGINE_REQUEST.findall(body):
                    engine_name = name if own_type else literal
                    if engine_name not in requested:
                        requested.append(engine_name)
        engines[name] = requested
    generator = engine / "OREData/ored/portfolio/tradegenerator.hpp"
    covered = set(GENERATOR_METHOD.findall(generator.read_text()))
    commit = subprocess.run(["git", "-C", str(engine), "rev-parse", "--short", "HEAD"],
                            capture_output=True, text=True, check=True).stdout.strip()
    return registered, engines, scripted, covered, commit


def _chain(cls: str, bases: dict[str, str]) -> list[str]:
    chain: list[str] = []
    while cls and cls not in chain and cls != "Trade":
        chain.append(cls)
        cls = bases.get(cls, "")
    return chain


def _body(text: str, start: int) -> str:
    depth = 0
    for index in range(start, len(text)):
        depth += {"{": 1, "}": -1}.get(text[index], 0)
        if depth == 0:
            return text[start:index]
    return text[start:]


def recorded_status(trade_type: str) -> tuple[str, set[str]]:
    written = RECORDED / f"{trade_type}.xml"
    if written.exists():
        return "written", {node.tag for node in ET.parse(written).getroot().iter()}
    if (RECORDED / f"{trade_type}.refused.txt").exists():
        return "refused", set()
    return "absent", set()


def generated_types(directory: Path) -> list[str]:
    """The trade types recorded in a directory, written or refused."""
    names = {p.name.split(".")[0] for p in directory.glob("*.xml")}
    return sorted(names | {p.name.split(".")[0] for p in directory.glob("*.refused.txt")})


def cell(values: list[str]) -> str:
    return ", ".join(f"={v}=" for v in values) if values else "none"


def render(engine: Path) -> tuple[str, dict[str, int]]:
    schema = Schema.load(XSD_DIR)
    samples = data_elements()
    declared = element_types()
    registered, engines, scripted, covered, commit = scan_engine(engine)
    generated = generated_types(RECORDED)
    rows, shared = [], Counter()
    counts = Counter()
    for kind in generated:
        element = samples.get(kind, Counter()).most_common(1)
        element = element[0][0] if element else f"{kind}Data"
        xsd = declared.get(element, "")
        terms = schema.required(xsd) if xsd else []
        status, tags = recorded_status(kind)
        needed = [t.name for t in terms]
        missing = [n for n in needed if status == "written" and not any(p in tags for p in n.split(" or "))]
        for term in terms:
            if term.xsd_type in schema.types:
                shared[term.xsd_type] += 1
        pricing = "scripted" if kind in scripted else (cell(engines.get(kind, [])) if engines.get(kind) else "not named")
        counts["types"] += 1
        counts["no_class"] += kind not in registered
        counts["no_schema"] += not xsd
        counts["no_engine"] += pricing == "not named"
        counts["missing_terms"] += len(missing)
        counts["refused"] += status == "refused"
        counts["generator"] += kind in covered
        rows.append(f"| ={kind}= | {'=' + registered[kind] + '=' if kind in registered else 'not registered'} "
                    f"| {'=' + element + '=' if xsd else 'not in the XSD'} | {cell(needed)} | {pricing} "
                    f"| {'yes' if kind in covered else 'no'} | {status} | {cell(missing)} |")
    out = [BEGIN, "", f"Read at ORE commit ={commit}=. Regenerate with =build/scripts/generate_ore_trade_requirements.py=.", "",
           "** Trade types", "",
           "| Type | ORE class | Data element | Required terms | Pricing engine | TradeGenerator | Recorded | Required, not written |",
           "|-", *rows, "", "** Shared pieces", "",
           "| XSD type | Used by | Required terms |", "|-"]
    for name, uses in shared.most_common():
        out.append(f"| ={name}= | {uses} | {cell([t.name for t in schema.required(name)])} |")
    out += ["", END]
    return "\n".join(out) + "\n", dict(counts)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--engine", type=Path, required=True)
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args()
    table, counts = render(args.engine)
    print(counts)
    if args.write:
        text = DOCUMENT.read_text()
        if text.count(BEGIN) != 1 or text.count(END) != 1:
            print(f"{DOCUMENT} needs one '{BEGIN}' and one '{END}' line", file=sys.stderr)
            return 1
        head, tail = text.split(BEGIN)[0], text.split(END)[1]
        DOCUMENT.write_text(head + table.rstrip("\n") + tail)
    return 0


if __name__ == "__main__":
    sys.exit(main())
