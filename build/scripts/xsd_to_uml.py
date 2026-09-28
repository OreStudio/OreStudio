#!/usr/bin/env python3
"""Render UML class diagrams from the ORE XML Schemas.

Reads XSD files and writes PlantUML: one class per complexType, one composition
edge per element whose type is a complexType, one attribute per element or
attribute of a simple type, and a generalization edge per xs:extension.
Anonymous inline complexTypes become classes named after their element and the
type that declares them, so every element in the schema is represented.

The schema is the authoritative description of the ORE document vocabulary, so
the diagrams are a mechanical projection of it: rerun this script after a
schema change rather than editing a diagram.

Usage:
  xsd_to_uml.py --out-dir doc/knowledge/architecture
  xsd_to_uml.py --xsd pricingengines curveconfig --format json
"""

from __future__ import annotations

import argparse
import json
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from pathlib import Path

XS = "{http://www.w3.org/2001/XMLSchema}"

# The ORE documents that configure a report run. Market data (todaysmarket),
# trades (instruments, portfolio, scriptlibrary) and observed data
# (collateralbalance, intradaypowerloaddata) are not configuration and are not
# listed here.
CONFIG_DOCUMENTS = [
    "ore",
    "pricingengines",
    "curveconfig",
    "conventions",
    "calendaradjustment",
    "currencyconfig",
    "referencedata",
    "nettingsetdefinitions",
    "counterparty",
    "simulation",
    "creditsimulation",
    "simmcalibration",
    "sensitivity",
    "stress",
    "historicalreturnconfig",
    "iborfallbackconfig",
    "baselTrafficLightconfig",
    "ore_types",
]


def tag_name(tag: str) -> str:
    return tag[len(XS):] if tag.startswith(XS) else tag


def local(type_name: str | None) -> str | None:
    """Strips a namespace prefix: 'xs:string' -> 'string'."""
    if type_name is None:
        return None
    return type_name.split(":")[-1]


@dataclass
class Attribute:
    name: str
    type_name: str
    required: bool


@dataclass
class Association:
    name: str
    target: str
    low: str
    high: str


@dataclass
class Class:
    name: str
    document: str
    kind: str = "complexType"
    base: str | None = None
    attributes: list[Attribute] = field(default_factory=list)
    associations: list[Association] = field(default_factory=list)
    anonymous: bool = False
    doc: str = ""


@dataclass
class Enum:
    name: str
    document: str
    values: list[str] = field(default_factory=list)


class Schema:
    """One XSD file, or the union of several when they include each other."""

    def __init__(self, path: Path):
        self.path = path
        self.document = path.stem
        self.classes: dict[str, Class] = {}
        self.enums: dict[str, Enum] = {}
        self.complex_types: set[str] = set()
        self.simple_types: set[str] = set()
        self.roots: list[tuple[str, str]] = []
        self.groups: dict[str, ET.Element] = {}
        self._parse()

    def _parse(self) -> None:
        root = ET.parse(self.path).getroot()
        # Named types first, so a reference can be resolved whichever order the
        # schema declares them in.
        for child in root:
            name = child.get("name")
            if name is None:
                continue
            if tag_name(child.tag) == "complexType":
                self.complex_types.add(name)
            elif tag_name(child.tag) == "simpleType":
                self.simple_types.add(name)
            elif tag_name(child.tag) == "element":
                self.roots.append((name, local(child.get("type")) or ""))
            elif tag_name(child.tag) == "group":
                self.groups[name] = child
        for child in root:
            self._read(child, enclosing=None)

    def _read(self, node: ET.Element, enclosing: str | None) -> None:
        kind = tag_name(node.tag)
        name = node.get("name")

        if kind == "complexType" and name:
            self._read_complex(node, name, anonymous=False)
        elif kind == "simpleType" and name:
            self._read_simple(node, name)
        elif kind == "element":
            if name and self._inline_complex(node) is not None:
                self._read_complex(self._inline_complex(node), name, anonymous=True)
        elif kind in ("sequence", "all", "choice", "complexContent", "simpleContent",
                      "extension", "restriction", "attributeGroup"):
            for sub in node:
                self._read(sub, enclosing)

    @staticmethod
    def _inline_complex(node: ET.Element) -> ET.Element | None:
        for sub in node:
            if tag_name(sub.tag) == "complexType":
                return sub
        return None

    def _read_simple(self, node: ET.Element, name: str) -> None:
        enum = Enum(name=name, document=self.document)
        for sub in node.iter(f"{XS}enumeration"):
            value = sub.get("value")
            if value is not None:
                enum.values.append(value)
        if enum.values:
            self.enums[name] = enum

    def _read_complex(self, node: ET.Element, name: str, anonymous: bool) -> None:
        cls = Class(name=name, document=self.document, anonymous=anonymous)
        doc = node.find(f"{XS}annotation/{XS}documentation")
        if doc is not None and doc.text:
            cls.doc = " ".join(doc.text.split())
        for sub in node:
            kind = tag_name(sub.tag)
            if kind in ("sequence", "all", "choice"):
                self._read_particles(sub, cls)
            elif kind == "complexContent":
                for inner in sub:
                    if tag_name(inner.tag) == "extension":
                        cls.base = local(inner.get("base"))
                        for part in inner:
                            if tag_name(part.tag) in ("sequence", "all", "choice"):
                                self._read_particles(part, cls)
                            elif tag_name(part.tag) == "attribute":
                                self._read_attribute(part, cls)
            elif kind == "simpleContent":
                for inner in sub:
                    if tag_name(inner.tag) == "extension":
                        cls.base = local(inner.get("base"))
                        for part in inner:
                            if tag_name(part.tag) == "attribute":
                                self._read_attribute(part, cls)
            elif kind == "attribute":
                self._read_attribute(sub, cls)
            elif kind == "attributeGroup":
                ref = sub.get("ref")
                if ref:
                    cls.attributes.append(Attribute(name=f"<<{local(ref)}>>",
                                                    type_name="attributeGroup",
                                                    required=False))
        self.classes[name] = cls

    def _read_particles(self, node: ET.Element, cls: Class) -> None:
        for sub in node:
            kind = tag_name(sub.tag)
            if kind in ("sequence", "all", "choice"):
                self._read_particles(sub, cls)
            elif kind == "group":
                ref = local(sub.get("ref") or "")
                group = self.groups.get(ref)
                if group is not None:
                    self._read_particles(group, cls)
            elif kind == "element":
                self._read_element(sub, cls)

    def _read_element(self, node: ET.Element, cls: Class) -> None:
        name = node.get("name")
        ref = node.get("ref")
        if name is None and ref:
            name = local(ref)
        if name is None:
            return
        low = node.get("minOccurs", "1")
        high = node.get("maxOccurs", "1")
        inline = self._inline_complex(node)
        type_name = local(node.get("type"))
        if inline is not None:
            # An inline complexType has no name of its own, so it takes the
            # element's, qualified by the type that declares it: two documents
            # both have a "Parameter" and they are not the same class.
            target = f"{cls.name}_{name}"
            self._read_complex(inline, target, anonymous=True)
            self.complex_types.add(target)
            cls.associations.append(Association(name=name, target=target, low=low, high=high))
        elif type_name and type_name in self.complex_types:
            cls.associations.append(
                Association(name=name, target=type_name, low=low, high=high))
        elif type_name and type_name in self.simple_types:
            cls.attributes.append(
                Attribute(name=name, type_name=type_name, required=low != "0"))
        else:
            cls.attributes.append(
                Attribute(name=name, type_name=type_name or "string", required=low != "0"))

    def _read_attribute(self, node: ET.Element, cls: Class) -> None:
        name = node.get("name")
        if name is None:
            return
        cls.attributes.append(
            Attribute(name=name,
                      type_name=local(node.get("type")) or "string",
                      required=node.get("use") == "required"))


def as_json(schemas: list[Schema]) -> dict:
    out: dict = {"documents": []}
    for schema in schemas:
        doc = {
            "document": schema.document,
            "path": str(schema.path),
            "roots": [{"element": e, "type": t} for e, t in schema.roots],
            "classes": [],
            "enums": [],
        }
        for cls in schema.classes.values():
            doc["classes"].append({
                "name": cls.name,
                "anonymous": cls.anonymous,
                "out_of_scope": is_out_of_scope(schema.document, cls.name),
                "base": cls.base,
                "doc": cls.doc,
                "attributes": [
                    {"name": a.name, "type": a.type_name, "required": a.required}
                    for a in cls.attributes
                ],
                "associations": [
                    {"name": a.name, "target": a.target, "low": a.low, "high": a.high}
                    for a in cls.associations
                ],
            })
        for enum in schema.enums.values():
            doc["enums"].append({"name": enum.name, "values": enum.values})
        out["documents"].append(doc)
    return out


def is_out_of_scope(document: str, name: str) -> bool:
    """True when a class is market data or trades rather than configuration.

    Two in-scope documents carry an out-of-scope subtree. `simulation` declares
    the market-data overrides the simulation market is built from, under `market`
    and `market_*`, and the curve algebra that reshapes them; both are market data
    and are excluded from the reporting taxonomy, as the brief requires.
    """
    if document == "simulation":
        return (name == "market" or name.startswith("market_")
                or name.startswith("curveAlgebra"))
    return False


def entity_set(schemas: list[Schema]) -> dict[str, set[str]]:
    """The classes that are entities rather than embedded value types.

    An entity is a document root, a member of a collection (a child element
    with maxOccurs greater than one), or a block reached once but holding
    children of its own. Everything else is a value type: a leaf that exists
    only inside its parent. Classes that belong to market data or trades are in
    neither set.
    """
    resolved: dict[str, dict[str, str]] = {}
    for schema in schemas:
        table: dict[str, str] = {}
        for other in schemas:
            for name in other.classes:
                table.setdefault(name, other.document)
        resolved[schema.document] = table

    keep: dict[str, set[str]] = {s.document: set() for s in schemas}
    for schema in schemas:
        for element, type_name in schema.roots:
            for name, cls in schema.classes.items():
                if name == type_name or name.startswith(f"{type_name}_"):
                    keep[schema.document].add(name)
        for cls in schema.classes.values():
            for assoc in cls.associations:
                if assoc.high == "unbounded":
                    keep[schema.document].add(assoc.target)
    # A 1:1 child that itself has children is a block, not a value type.
    changed = True
    while changed:
        changed = False
        for schema in schemas:
            for cls in schema.classes.values():
                if cls.name not in keep[schema.document]:
                    continue
                for assoc in cls.associations:
                    if assoc.target in keep[schema.document]:
                        continue
                    target = schema.classes.get(assoc.target)
                    if target is not None and target.associations:
                        keep[schema.document].add(assoc.target)
                        changed = True
    for schema in schemas:
        keep[schema.document] = {n for n in keep[schema.document]
                                 if not is_out_of_scope(schema.document, n)}
    return keep


# Where each document's entities belong in ORE Studio, and how the report
# definition reaches them. "owned" means the reporting domain owns the entity and
# the definition has it as a child; "referenced" means another component owns it
# and the definition or its run setup names it. This is the design decision the
# taxonomy exists to state, so it lives beside the extraction rather than in a
# table someone has to keep in step by hand.
DOCUMENT_OWNERSHIP: dict[str, tuple[str, str, str]] = {
    # document: (owning component, reporting entity or entities, relation)
    "ore": ("reporting", "report_run_setup, report_market_binding, report_analytic", "owned"),
    "pricingengines": ("ores.analytics", "pricing_engine_type, pricing_model_config", "referenced"),
    "curveconfig": ("ores.refdata / ores.marketdata", "curve_recipe", "referenced"),
    "conventions": ("ores.refdata", "convention", "referenced"),
    "calendaradjustment": ("ores.refdata", "calendar, calendar_adjustment", "referenced"),
    "currencyconfig": ("ores.refdata", "currency, currency_pair", "referenced"),
    "referencedata": ("ores.refdata", "reference datum per flavour", "referenced"),
    "nettingsetdefinitions": ("ores.trading", "netting_set (missing today)", "referenced"),
    "counterparty": ("ores.refdata", "counterparty", "referenced"),
    "simulation": ("reporting", "simulation_config", "owned"),
    "creditsimulation": ("reporting", "credit_simulation_config", "owned"),
    "simmcalibration": ("reporting", "simm_calibration", "owned"),
    "sensitivity": ("reporting", "sensitivity_config", "owned"),
    "stress": ("reporting", "stress_config", "owned"),
    "historicalreturnconfig": ("reporting", "historical_return_config", "owned"),
    "iborfallbackconfig": ("ores.refdata", "ibor_fallback_rule", "referenced"),
    "baselTrafficLightconfig": ("reporting", "basel_traffic_light_config", "owned"),
    "ore_types": ("shared", "value types and enums, no entity", "shared"),
}


def emit_mapping(schemas: list[Schema]) -> str:
    """One row per entity: document, entity, owner, reporting entity, relation."""
    keep = entity_set(schemas)
    lines = [
        "<!-- Generated by build/scripts/xsd_to_uml.py --format mapping. -->",
        "",
        "Every class the extraction rule keeps, which is every class that becomes "
        "something in the model: a true entity, or a block that is embedded in one. "
        "The reviewed entity/value-type split is tabulated in TAXONOMY.org. "
        "Classes that belong to market data or trades are absent.",
        "",
        "| Document | Entity | Owning component | Reporting entity | Relation |",
             "|----------+--------+------------------+------------------+----------|"]
    for schema in schemas:
        owner, reporting, relation = DOCUMENT_OWNERSHIP.get(
            schema.document, ("unknown", "unmapped", "unknown"))
        names = sorted(n for n in schema.classes if n in keep[schema.document])
        if not names:
            lines.append(f"| ={schema.document}= | (no entities) | {owner} | {reporting} | "
                         f"{relation} |")
            continue
        for name in names:
            lines.append(f"| ={schema.document}= | ={name}= | {owner} | {reporting} | "
                         f"{relation} |")
    return "\n".join(lines) + "\n"


def emit_puml(schemas: list[Schema], title: str, entities_only: bool = False) -> str:
    # Elements reference types that are often declared in another document, so
    # associations resolve against the whole set, preferring the declaring
    # document when a name occurs in more than one.
    index: dict[str, list[tuple[str, str]]] = {}
    for schema in schemas:
        for name in schema.classes:
            index.setdefault(name, []).append((schema.document, name))

    def resolve(target: str, document: str) -> str | None:
        hits = index.get(target)
        if not hits:
            return None
        for hit_doc, hit_name in hits:
            if hit_doc == document:
                return f"{hit_doc}__{hit_name}"
        return f"{hits[0][0]}__{hits[0][1]}"

    lines = ["@startuml", f"title {title}", "hide empty members",
             "skinparam classAttributeIconSize 0", "skinparam nodesep 12",
             "skinparam ranksep 24", ""]
    keep = entity_set(schemas) if entities_only else {s.document: set(s.classes)
                                                      for s in schemas}

    def kept(document: str, name: str) -> bool:
        return name in keep[document]

    edges: list[str] = []
    for schema in schemas:
        lines.append(f'package "{schema.document}.xml" {{')
        for cls in sorted(schema.classes.values(), key=lambda c: c.name):
            if not kept(schema.document, cls.name):
                continue
            stereotype = " <<value>>" if cls.anonymous else ""
            lines.append(f'  class "{cls.name}" as {schema.document}__{cls.name}{stereotype} {{')
            for attr in cls.attributes:
                marker = "" if attr.required else "?"
                lines.append(f"    {attr.name}{marker} : {attr.type_name}")
            lines.append("  }")
        for enum in sorted(schema.enums.values(), key=lambda e: e.name):
            lines.append(f'  enum "{enum.name}" as {schema.document}__{enum.name} {{')
            for value in enum.values:
                lines.append(f"    {value}")
            lines.append("  }")
        lines.append("}")
        for cls in sorted(schema.classes.values(), key=lambda c: c.name):
            if not kept(schema.document, cls.name):
                continue
            if cls.base:
                base = resolve(cls.base, schema.document)
                if base:
                    edges.append(f"{base} <|-- {schema.document}__{cls.name}")
            for assoc in cls.associations:
                target = resolve(assoc.target, schema.document)
                if target is None:
                    continue
                high = "*" if assoc.high == "unbounded" else assoc.high
                multi = f'"{assoc.low}..{high}"' if (high, assoc.low) != ("1", "1") else '"1"'
                edges.append(f"{schema.document}__{cls.name} *-- {multi} "
                             f"{target} : {assoc.name}")
        lines.append("")
    lines.extend(sorted(set(edges)))
    lines.append("@enduml")
    return "\n".join(lines) + "\n"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--xsd-dir", default="external/ore/xsd")
    ap.add_argument("--xsd", nargs="*", default=None,
                    help="schema basenames to read (default: the configuration set)")
    ap.add_argument("--out-dir", default=None)
    ap.add_argument("--format", choices=("puml", "json", "mapping", "both"), default="puml")
    ap.add_argument("--out-name", default=None, help="output .puml file name")
    ap.add_argument("--title", default="ORE report configuration entities")
    ap.add_argument("--entities-only", action="store_true",
                    help="keep entities and blocks; drop pure value types")
    args = ap.parse_args()

    xsd_dir = Path(args.xsd_dir)
    names = args.xsd if args.xsd else CONFIG_DOCUMENTS
    paths = [xsd_dir / f"{n}.xsd" for n in names]
    missing = [p for p in paths if not p.is_file()]
    if missing:
        for p in missing:
            print(f"missing schema: {p}", file=sys.stderr)
        return 1

    schemas = [Schema(p) for p in paths]

    if args.format in ("json", "both"):
        payload = json.dumps(as_json(schemas), indent=2)
        if args.out_dir:
            out = Path(args.out_dir) / "ore_report_configuration_entities.json"
            out.write_text(payload + "\n")
            print(f"wrote {out}")
        else:
            print(payload)
    if args.format in ("mapping", "both"):
        text = emit_mapping(schemas)
        if args.out_dir:
            out = Path(args.out_dir) / "ore_report_configuration_mapping.md"
            out.write_text(text)
            print(f"wrote {out}")
        else:
            print(text)
    if args.format in ("puml", "both"):
        text = emit_puml(schemas, args.title, entities_only=args.entities_only)
        if args.out_dir:
            out = Path(args.out_dir) / (args.out_name or "ore_report_configuration_entities.puml")
            out.write_text(text)
            print(f"wrote {out}")
        else:
            print(text)
    total = sum(len(s.classes) for s in schemas)
    print(f"{len(schemas)} schema(s), {total} classes", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
