#!/usr/bin/env python3
"""Join what ORE does with each trade type into one table.

Reads the output of trade_catalogue (every type ORE's trade factory registers,
the class it builds and whether that class derives from ScriptedTrade), ORE's
source, its schema, its Products script library, the curated twin list and ORE
Studio's trade type catalogue, and writes one row per trade type that either
ORE registers or the catalogue declares. A data element follows ORE's convention
unless trade_type_elements.txt names the one the class reads instead.

    mine_trade_types.py <ORE source dir> <trade_catalogue output> <repo root>

The columns are written as a header line. Every value is a string; a dash
means the fact does not apply.
"""

import difflib
import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

HERE = Path(__file__).resolve().parent
NEAR = 0.7
COLUMNS = [
    "trade_type",
    "in_catalogue",
    "registered",
    "ore_class",
    "form",
    "element",
    "element_in_schema",
    "element_read",
    "library_scripts",
    "script_match",
]


def norm(text):
    return re.sub(r"\s+", " ", text).strip()


def catalogue_codes(repo):
    path = repo / "projects/ores.trading/modeling/ores.trading.trade_type_catalogue.org"
    codes, inside = [], False
    for line in path.read_text().splitlines():
        if line.startswith("* "):
            inside = line.strip() == "* Trade types"
            continue
        if inside and line.startswith("| ") and not line.startswith("| code"):
            codes.append(line.split("|")[1].strip())
    return codes


def harness_rows(path):
    rows = {}
    for line in Path(path).read_text().splitlines():
        trade_type, ore_class, form = (line.split("\t") + ["", ""])[:3]
        rows[trade_type] = (ore_class.removeprefix("ore::data::"), form)
    return rows


def schema_elements(repo):
    text = (repo / "external/ore/xsd/instruments.xsd").read_text()
    return set(re.findall(r'<xs:element\s[^>]*name="(\w+Data)"', text))


def library_scripts(repo):
    root = ET.parse(repo / "external/ore/examples/Products/Input/scriptlibrary.xml").getroot()
    scripts = {}
    for entry in root.findall("Script"):
        name = entry.findtext("Name")
        code = entry.find("Script/Code")
        if name and code is not None:
            scripts[name] = norm(code.text or "")
    return scripts


def pairs_of(path):
    mapping = {}
    for line in Path(path).read_text().splitlines():
        if line.strip() and not line.startswith("#"):
            key, value = line.split("\t")
            mapping[key] = value
    return mapping


def twins(path):
    mapping = {}
    for line in Path(path).read_text().splitlines():
        if not line.strip() or line.startswith("#"):
            continue
        trade_type, scripts = line.split("\t")
        mapping[trade_type] = scripts.split()
    return mapping


class Source:
    """ORE's portfolio sources, indexed by class."""

    def __init__(self, ore_source):
        self.dir = Path(ore_source) / "OREData/ored/portfolio"
        self.bases = {}
        self.files = {}
        for header in self.dir.glob("*.hpp"):
            for name, base in re.findall(r"class\s+(\w+)\s*(?:final\s*)?:\s*public\s+(\w+)", header.read_text()):
                self.bases.setdefault(name, base)
                self.files.setdefault(name, header.with_suffix(".cpp"))

    def chain(self, name):
        seen = []
        while name and name not in seen:
            seen.append(name)
            name = self.bases.get(name)
        return seen

    def text(self, name):
        path = self.files.get(name)
        return path.read_text() if path and path.exists() else ""

    def reads(self, name, element):
        for cls in self.chain(name):
            body = self.text(cls)
            if f'"{element}"' in body or re.search(r"tradeType(?:\(\)|_)\s*\+\s*\"Data\"", body):
                return True
        return False

    def scripts(self, name):
        found = []
        for cls in self.chain(name):
            body = re.sub(r"//[^\n]*", "", self.text(cls))
            for run in re.finditer(r'(?:"(?:[^"\\]|\\.)*"\s*){2,}', body):
                parts = re.findall(r'"((?:[^"\\]|\\.)*)"', run.group(0))
                code = "".join(parts)
                if len(code) > 100:
                    found.append(norm(code.encode().decode("unicode_escape")))
        return found


def match(source, ore_class, scripts, library):
    if not scripts:
        return "-"
    embedded = source.scripts(ore_class)
    best = 0.0
    for name in scripts:
        code = library.get(name)
        if code is None:
            sys.exit(f"twin script {name} is not in the Products script library")
        for candidate in embedded:
            if candidate == code:
                return "identical"
            best = max(best, difflib.SequenceMatcher(None, candidate, code, autojunk=False).ratio())
    return "near" if best >= NEAR else "named"


def main():
    if len(sys.argv) != 4:
        sys.exit(__doc__)
    ore_source, harness, repo = sys.argv[1], sys.argv[2], Path(sys.argv[3])
    codes = catalogue_codes(repo)
    ore = harness_rows(harness)
    elements = schema_elements(repo)
    library = library_scripts(repo)
    pairs = twins(HERE / "trade_type_twins.txt")
    exceptions = pairs_of(HERE / "trade_type_elements.txt")
    source = Source(ore_source)

    print("\t".join(COLUMNS))
    for trade_type in sorted(set(codes) | set(ore)):
        ore_class, form = ore.get(trade_type, ("-", "-"))
        element = exceptions.get(trade_type, trade_type.replace("_", "") + "Data")
        scripts = pairs.get(trade_type, [])
        read = "-" if ore_class == "-" else ("yes" if source.reads(ore_class, element) else "no")
        print("\t".join([
            trade_type,
            "yes" if trade_type in codes else "no",
            "yes" if trade_type in ore else "no",
            ore_class,
            form,
            element,
            "yes" if element in elements else "no",
            read,
            " ".join(scripts) or "-",
            match(source, ore_class, scripts, library) if ore_class != "-" else ("named" if scripts else "-"),
        ]))


if __name__ == "__main__":
    main()
