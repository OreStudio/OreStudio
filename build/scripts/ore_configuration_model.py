#!/usr/bin/env python3
"""Design the ORE configuration object model as one UML class diagram.

Two halves, per the design decision:

- `ores.reporting` owns the named configurations, the configuration types, the
  binding from a report definition, and the parameter vocabulary that describes
  an ORE run document.
- `ores.analytics` owns the configuration types themselves — what a stress
  test, a SIMM calibration, a sensitivity analysis, a simulation, a credit
  simulation, a historical return configuration or a Basel traffic light
  configuration is made of.

Data-oriented design, so the diagram carries structs and their members and
nothing else: no behaviour, no methods, one struct per entity, members named in
this model's own snake_case rather than ORE's CamelCase.

The analytics half is generated from the schema extraction, so it cannot drift
from the vocabulary it models. The reporting half is written here, because it is
a design and no schema states it.

Usage:
  python3 build/scripts/ore_configuration_model.py --out-dir <dir>
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

# The configuration types whose structure analytics owns, and the document each
# is read from.
ANALYTICS_DOCUMENTS = [
    "stress",
    "simmcalibration",
    "sensitivity",
    "simulation",
    "creditsimulation",
    "historicalreturnconfig",
    "baselTrafficLightconfig",
]

# The reporting half of the model: the named configurations and the parameter
# vocabulary for an ORE run document. Written out because it is the design, not
# a projection of a schema.
REPORTING = """
package "ores.reporting" #E8F4FF {
  class report_type {
    id : uuid
    code : text
    name : text
    description : text
    display_order : integer
  }

  class report_definition {
    id : uuid
    name : text
    report_type_id : uuid
    party_id : uuid
    description : text
    schedule_expression : text
    concurrency_policy : text
    fsm_state_id : uuid
    scheduler_job_id : uuid
    workspace_id : uuid
    audit : audit_envelope
  }

  class configuration_type {
    id : uuid
    code : text
    name : text
    owning_component : text
    parameterised : boolean
  }

  class configuration {
    id : uuid
    name : text
    configuration_type_id : uuid
    party_id : uuid
    description : text
    workspace_id : uuid
    audit : audit_envelope
  }

  class report_configuration {
    report_definition_id : uuid
    configuration_type_id : uuid
    configuration_id : uuid
  }

  class value_domain {
    id : uuid
    code : text
    storage_type : text
    referenced_entity : text
  }

  class parameter_definition {
    id : uuid
    configuration_type_id : uuid
    subtype : text
    name : text
    value_domain_id : uuid
    required : boolean
    display_order : integer
  }

  class configuration_parameter {
    configuration_id : uuid
    parameter_definition_id : uuid
    position : integer
    value : text
  }
}
"""


def snake(name: str) -> str:
    s = re.sub(r"(?<=[a-z0-9])(?=[A-Z])", "_", name)
    s = re.sub(r"(?<=[A-Z])(?=[A-Z][a-z])", "_", s)
    return s.replace("-", "_").lower()


def storage_type(ore_type: str) -> str:
    t = (ore_type or "").lower()
    if t in ("bool", "boolean"):
        return "boolean"
    if t in ("integer", "int", "nonnegativeinteger", "positiveinteger", "long"):
        return "integer"
    if t in ("decimal", "double", "float", "non-negative-decimal", "nonnegativedecimal"):
        return "numeric"
    if t == "date":
        return "date"
    if t in ("datetime", "timestamp"):
        return "timestamptz"
    if t == "period":
        return "text"
    if t.endswith("type") or t.endswith("code") or t in (
            "currencycode", "currencypair", "indexnametype", "calendar",
            "daycounter", "businessdayconvention", "extendedcurrencycode"):
        return "text /* coded */"
    return "text"


def emit(inventory: Path, title: str) -> str:
    data = json.loads(inventory.read_text())
    by_doc = {d["document"]: d for d in data["documents"]}

    # Import the extraction rule so the analytics half is the same set of
    # entities the taxonomy reviewed, not a second opinion.
    import xsd_to_uml as extractor

    schemas = [extractor.Schema(Path("external/ore/xsd") / f"{d}.xsd")
               for d in ANALYTICS_DOCUMENTS]
    keep = extractor.entity_set(schemas)
    classes = {s.document: s.classes for s in schemas}
    # Cross-document association targets resolve globally.
    global_docs: dict[str, str] = {}
    for doc, cs in classes.items():
        for name in cs:
            global_docs.setdefault(name, doc)

    # Structs that differ only in name are one shape: draw the first and list
    # the rest in a note. Eleven stress shift families, twenty-six sensitivity
    # families and six SIMM risk classes are the same struct three times over,
    # and drawing each one hides the design.
    def shape(cls) -> tuple:
        members = tuple(sorted((snake(a.name), storage_type(a.type_name))
                               for a in cls.attributes if not a.name.startswith("<<")))
        kids = tuple(sorted((snake(a.name), a.low, a.high) for a in cls.associations))
        return members, kids

    groups: dict[str, dict[tuple, list[str]]] = {}
    for doc in ANALYTICS_DOCUMENTS:
        for name in sorted(keep[doc]):
            cls = classes[doc].get(name)
            if cls is None or not cls.attributes:
                continue
            groups.setdefault(doc, {}).setdefault(shape(cls), []).append(name)

    draw: dict[str, set[str]] = {d: set() for d in ANALYTICS_DOCUMENTS}
    variants: list[str] = []
    for doc, by_shape in groups.items():
        for sig, names in by_shape.items():
            draw[doc].add(names[0])
            if len(names) > 1:
                variants.append(f'{doc}__{names[0]} : "… same shape as: {", ".join(names[1:])}"')

    out = ["@startuml", f"title {title}", "hide empty members",
           "skinparam classAttributeIconSize 0", "skinparam nodesep 10",
           "skinparam ranksep 22", "left to right direction", ""]
    out.append(REPORTING)

    edges: list[str] = []
    out.append('package "ores.analytics" #EAF7EA {')
    for doc in ANALYTICS_DOCUMENTS:
        out.append(f'  package "{doc}" {{')
        for name in sorted(draw[doc]):
            cls = classes[doc].get(name)
            if cls is None:
                continue
            alias = f"{doc}__{name}"
            out.append(f'    class "{name}" as {alias} {{')
            for attr in cls.attributes:
                if attr.name.startswith("<<"):
                    continue
                out.append(f"      {snake(attr.name)} : {storage_type(attr.type_name)}")
            out.append("    }")
        out.append("  }")
    out.append("}")
    for doc in ANALYTICS_DOCUMENTS:
        for name in sorted(draw[doc]):
            cls = classes[doc].get(name)
            if cls is None:
                continue
            src = f"{doc}__{name}"
            for assoc in cls.associations:
                target_doc = doc if assoc.target in classes[doc] else global_docs.get(assoc.target)
                if target_doc is None or target_doc not in keep:
                    continue
                if assoc.target not in keep[target_doc]:
                    continue
                high = "*" if assoc.high == "unbounded" else assoc.high
                multi = f'"{assoc.low}..{high}"' if (high, assoc.low) != ("1", "1") else '"1"'
                edges.append(f'{src} *-- {multi} {target_doc}__{assoc.target} : {snake(assoc.name)}')

    # Reporting-side relationships: the design, stated once.
    edges += [
        'report_definition *-- "1" report_type : typed as',
        'report_definition *-- "0..*" report_configuration : binds',
        'report_configuration *-- "1" configuration_type : by type',
        'report_configuration *-- "1" configuration : to',
        'configuration *-- "1" configuration_type : is one of',
        'configuration *-- "0..*" configuration_parameter : holds',
        'configuration_parameter *-- "1" parameter_definition : named by',
        'parameter_definition *-- "1" value_domain : valued as',
        'parameter_definition *-- "1" configuration_type : belongs to',
        'configuration "1" -- "0..*" stress__stresstesting : detail of a stress configuration',
        'configuration "1" -- "0..*" simmcalibration__SIMMCalibrationData : detail',
        'configuration "1" -- "0..*" sensitivity__sensitivityanalysis : detail',
        'configuration "1" -- "0..*" simulation__simulation : detail',
        'configuration "1" -- "0..*" creditsimulation__creditsimulation : detail',
        'configuration "1" -- "0..*" historicalreturnconfig__ReturnConfiguration : detail',
        'configuration "1" -- "0..*" baselTrafficLightconfig__BaselTrafficLightConfig : detail',
    ]
    out.append("")
    out.extend(sorted(set(edges)))
    out.extend(variants)
    out.append("@enduml")
    return "\n".join(out) + "\n"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--inventory", default=".audit/ore-taxonomy/ore_report_configuration_entities.json")
    ap.add_argument("--out-dir", default=".")
    ap.add_argument("--out-name", default="ore_configuration_model.puml")
    ap.add_argument("--title", default="ORE configuration object model")
    ap.add_argument("--reporting-only", action="store_true",
                    help="emit only the reporting half, which is the design core")
    ap.add_argument("--analytics-only", action="store_true",
                    help="emit only the analytics half, without the reporting package")
    args = ap.parse_args()

    if args.reporting_only:
        body = ["@startuml", f"title {args.title}", "hide empty members",
                "skinparam classAttributeIconSize 0", "left to right direction", "",
                REPORTING, "",
                'report_definition *-- "1" report_type : typed as',
                'report_definition *-- "0..*" report_configuration : binds',
                'report_configuration *-- "1" configuration_type : by type',
                'report_configuration *-- "1" configuration : to',
                'configuration *-- "1" configuration_type : is one of',
                'configuration *-- "0..*" configuration_parameter : holds',
                'configuration_parameter *-- "1" parameter_definition : named by',
                'parameter_definition *-- "1" value_domain : valued as',
                'parameter_definition *-- "1" configuration_type : belongs to',
                'note bottom of configuration\n  one row per named configuration:\n  "Stress Testing / eur_6m_up_library"\nend note',
                'note bottom of parameter_definition\n  the vocabulary: which names a\n  (type, subtype) accepts, and what\n  domain each value has\nend note',
                "@enduml"]
        text = "\n".join(body) + "\n"
    elif args.analytics_only:
        text = emit(Path(args.inventory), args.title)
        # Drop the reporting packages: this diagram is the analytics half only.
        keep_lines, skip = [], False
        for line in text.splitlines():
            if line.startswith('package "ores.reporting'):
                skip = True
                continue
            if skip:
                if line.startswith("package "):
                    skip = False
                else:
                    continue
            keep_lines.append(line)
        text = "\n".join(l for l in keep_lines
                         if not l.startswith(('report_definition', 'report_configuration',
                                              'configuration ', 'configuration_parameter',
                                              'parameter_definition', 'configuration_type',
                                              'value_domain', 'report_type'))) + "\n"
    else:
        text = emit(Path(args.inventory), args.title)
    out = Path(args.out_dir) / args.out_name
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(text)
    print(f"wrote {out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
