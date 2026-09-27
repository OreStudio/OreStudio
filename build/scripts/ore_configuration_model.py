#!/usr/bin/env python3
"""Emit the normalised ORE configuration object model as UML.

Normalised, not per-class: if two relations have the same shape they are one
relation with a discriminator. The shift families are the case that matters —
sensitivity's twenty-six, stress's eleven — and they become one entity each,
keyed by a family row that names the XML element the family serialises to.

Lossless, so that the XML can be rebuilt from the rows:

- every value sits in a typed member, and every member's XML spelling is held
  in `configuration_xml_element`, which is generated from the schemas;
- every repeated child is reached by an edge whose label is the XML element
  name, and the map carries its ordinal, so order and nesting come back;
- every discriminator (a family, a kind, a risk class) is a row whose own XML
  element is recorded, so the element carrying a value is never guessed.

Usage:
  python3 build/scripts/ore_configuration_model.py --half analytics --out-dir <dir>
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import xsd_to_uml as extractor  # noqa: E402  the extraction rule, reused

ANALYTICS_DOCUMENTS = [
    "stress",
    "simmcalibration",
    "sensitivity",
    "simulation",
    "creditsimulation",
    "historicalreturnconfig",
    "baselTrafficLightconfig",
]

# Families that are one relation with a discriminator. Each group becomes
# `<document>_shift`, keyed by a `<document>_shift_family` row that carries the
# XML element the family serialises to and the domain of its key.
SHIFT_FAMILIES = {
    "sensitivity": [
        "discountcurve", "indexcurve", "yieldcurve", "fxspot", "fxvolatility",
        "swaptionvolatility", "yieldvolatility", "capfloorvolatility",
        "cdsvolatility", "creditcurve", "equityspot", "equityvolatility",
        "zeroinflationindexcurve", "yyinflationindexcurve",
        "cpicapfloorvolatility", "yycapfloorvolatility", "basecorrelation",
        "securityspread", "dividendyield", "commodityCurve",
        "intradaypowercurve", "commodityvolatility", "correlationcurve",
        "sensiBondFutureVolatility", "survivalprobability", "recoveryrate",
    ],
    "stress": [
        "stressdiscountcurve", "stressindexcurve", "stressyieldcurve",
        "stresssurvivalprobability", "stressfxvolatility",
        "stressswaptionvolatility", "stresscapfloorvolatility",
        "stresscommoditycurve", "stressintradaypowercurve",
        "stresscommodityvolatility",
    ],
}
# The shift block every family shares, once the family's key has been lifted
# into (family_id, object_code).
SHIFT_BLOCK = [
    ("shift_type_id", "uuid"),
    ("shift_scheme_id", "uuid"),
    ("shift_size", "numeric"),
    ("par_conversion", "text"),
    ("is_relative", "boolean"),
]
# The grids ORE writes as comma-separated strings are rows here, so that a tenor
# is a reference to the tenor entity rather than a string in a list.
SHIFT_ROWS = {
    "shift_tenor": [("shift_id", "uuid"), ("sequence", "integer"), ("tenor_id", "uuid")],
    "shift_strike": [("shift_id", "uuid"), ("sequence", "integer"), ("strike", "numeric")],
    "shift_point": [("shift_id", "uuid"), ("expiry_tenor_id", "uuid"),
                    ("term_tenor_id", "uuid"), ("strike", "numeric"),
                    ("value", "numeric")],
    "shift_key_entry": [("shift_id", "uuid"), ("risk_factor_key", "text"),
                        ("shift_type_id", "uuid"), ("shift_size", "numeric"),
                        ("shift_scheme_id", "uuid")],
}

# Tidy names: every analytics struct carries the configuration type it belongs to
# and reads as configuration rather than as a schema class. One namespace, so the
# prefix is what keeps them apart.
TYPE_PREFIX = {
    "stress": "stress",
    "simmcalibration": "simm",
    "sensitivity": "sensitivity",
    "simulation": "simulation",
    "creditsimulation": "credit_simulation",
    "historicalreturnconfig": "historical_return",
    "baselTrafficLightconfig": "basel_traffic_light",
}
ROOT_NAME = {
    "stress": "stress_testing_config",
    "simmcalibration": "simm_calibration_config",
    "sensitivity": "sensitivity_config",
    "simulation": "simulation_config",
    "creditsimulation": "credit_simulation_config",
    "historicalreturnconfig": "historical_return_config",
    "baselTrafficLightconfig": "basel_traffic_light_config",
}
ROOTS = {
    "stress": "stresstesting",
    "simmcalibration": "SIMMCalibrationData",
    "sensitivity": "sensitivityanalysis",
    "simulation": "simulation",
    "creditsimulation": "creditsimulation",
    "historicalreturnconfig": "ReturnConfiguration",
    "baselTrafficLightconfig": "BaselTrafficLightConfig",
}


# ORE spells several types as one lowercase word; a name is tidied by splitting
# the tokens it is made of, not only the camel-case ones.
COMPOUNDS = {
    "transitionmatrix": "transition_matrix",
    "transitionmatrices": "transition_matrices",
    "parconversion": "par_conversion",
    "stresstesting": "stress_testing",
    "sensitivityanalysis": "sensitivity_analysis",
    "crossassetmodel": "cross_asset_model",
    "averageois": "average_ois",
    "tenorbasis": "tenor_basis",
    "crosscurrency": "cross_currency",
    "zerospread": "zero_spread",
    "discountratio": "discount_ratio",
    "iborfallback": "ibor_fallback",
    "capfloor": "cap_floor",
    "bondfuture": "bond_future",
    "fxspot": "fx_spot",
    "equityspot": "equity_spot",
    "yieldcurve": "yield_curve",
    "discountcurve": "discount_curve",
    "indexcurve": "index_curve",
    "creditcurve": "credit_curve",
    "survivalprobability": "survival_probability",
    "recoveryrate": "recovery_rate",
    "basecorrelation": "base_correlation",
    "securityspread": "security_spread",
    "dividendyield": "dividend_yield",
    "commoditycurve": "commodity_curve",
    "intradaypowercurve": "intraday_power_curve",
    "commodityvolatility": "commodity_volatility",
    "correlationcurve": "correlation_curve",
    "swaptionvolatility": "swaption_volatility",
    "yieldvolatility": "yield_volatility",
    "cdsvolatility": "cds_volatility",
    "riskweights": "risk_weights",
    "currencylists": "currency_lists",
    "concentrationthresholds": "concentration_thresholds",
    "historicalvolatilityratio": "historical_volatility_ratio",
    "mporcalendar": "mpor_calendar",
    "mporDays": "mpor_days",
    "yoy": "year_on_year",
}


def split_compounds(sn: str) -> str:
    sn = sn.replace("yo_y", "yoy")
    return "_".join(COMPOUNDS.get(tok, tok) for tok in sn.split("_"))


def tidy(doc: str, name: str) -> str:
    """`{type}_{what it models}_config`, in one namespace."""
    if name == ROOTS.get(doc):
        return ROOT_NAME[doc]
    sn = snake(name)
    for drop in ("simmcalibration_", "stress_", "sensitivity_", "simulation_",
                 "creditsimulation_", "historicalreturnconfig_",
                 "baseltrafficlightconfig_"):
        if sn.startswith(drop):
            sn = sn[len(drop):]
            break
    sn = split_compounds(sn)
    sn = re.sub(r"_+", "_", sn).strip("_")
    if not sn:
        return ROOT_NAME[doc]
    # A name that repeats a token, or repeats the type prefix it has just been
    # given, is saying the same thing twice.
    toks = f"{TYPE_PREFIX[doc]}_{sn}_config".split("_")
    toks = [x for i, x in enumerate(toks) if i == 0 or x != toks[i - 1]]
    pfx = TYPE_PREFIX[doc].split("_")
    if toks[:len(pfx)] == pfx and toks[len(pfx):2 * len(pfx)] == pfx:
        toks = toks[:len(pfx)] + toks[2 * len(pfx):]
    return "_".join(x for x in toks if x)


def tidy_all(structs, edges, notes):
    rename = {d: {n: tidy(d, n) for n in names} for d, names in structs.items()}
    for d, names in structs.items():
        structs[d] = {rename[d][n]: m for n, m in names.items()}
    edges = [(d, rename[d][s], rename[d][t], lbl, lo, hi) for d, s, t, lbl, lo, hi in edges]
    notes = [re.sub(r"^([a-zA-Z_]+)", lambda m: rename.get(
        next((d for d in rename if m.group(1) in rename[d]), ""), {}).get(m.group(1), m.group(1)),
        n) for n in notes]
    return structs, edges, notes


REPORTING = '''package "ores.reporting" #E8F4FF {
  class report_definition {
    id : uuid
    name : text
    report_type_id : uuid
    party_id : uuid
    schedule_expression : text
    concurrency_policy : text
    fsm_state_id : uuid
    scheduler_job_id : uuid
    workspace_id : uuid
  }
  class report_type {
    id : uuid
    code : text
    name : text
  }
  class report_configuration {
    report_definition_id : uuid
    configuration_type_id : uuid
    configuration_id : uuid
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
    workspace_id : uuid
  }
  class configuration_parameter {
    configuration_id : uuid
    parameter_definition_id : uuid
    position : integer
    value : text
  }
  class parameter_definition {
    id : uuid
    configuration_type_id : uuid
    scope : text
    subtype : text
    name : text
    value_domain_id : uuid
    required : boolean
    position : integer
  }
  class value_domain {
    id : uuid
    code : text
    storage_type : text
    referenced_entity : text
  }
}'''


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
    if t in ("decimal", "double", "float", "non-negative-decimal"):
        return "numeric"
    if t == "date":
        return "date"
    if t == "period":
        return "text"
    if t.endswith("type") or t.endswith("code") or t in (
            "currencycode", "currencypair", "indexnametype", "calendar",
            "daycounter", "businessdayconvention", "extendedcurrencycode"):
        return "text"
    return "text"


REPORTING_RELS = [
    'report_definition *-- "1" report_type : typed as',
    'report_definition *-- "0..*" report_configuration : binds',
    'report_configuration *-- "1" configuration_type : by type',
    'report_configuration *-- "1" configuration : to',
    'configuration *-- "1" configuration_type : is one of',
    'configuration *-- "0..*" configuration_parameter : holds',
    'configuration_parameter *-- "1" parameter_definition : named by',
    'parameter_definition *-- "1" value_domain : valued as',
    'parameter_definition *-- "1" configuration_type : belongs to',
]

def load(inventory: Path) -> dict:
    data = json.loads(inventory.read_text())
    return {d["document"]: d for d in data["documents"]}


def keep_sets() -> dict[str, set[str]]:
    """The entities and blocks the extraction rule keeps, per document."""
    schemas = [extractor.Schema(Path("external/ore/xsd") / f"{d}.xsd")
               for d in ANALYTICS_DOCUMENTS]
    return extractor.entity_set(schemas)


def build(docs: dict, keep: dict[str, set[str]] | None = None):
    """Returns (structs per document, edges, notes)."""
    keep = keep or keep_sets()
    structs: dict[str, dict[str, list[tuple[str, str]]]] = {}
    edges: list[tuple[str, str, str, str, str]] = []
    notes: list[str] = []

    for doc in ANALYTICS_DOCUMENTS:
        classes = {c["name"]: c for c in docs[doc]["classes"]
                   if not c.get("out_of_scope") and c["name"] in keep[doc]}
        members = {n: [(snake(a["name"]), storage_type(a["type"])) for a in c["attributes"]]
                   for n, c in classes.items()}
        children = {n: [(a["name"], a["target"], a["low"], a["high"])
                        for a in c["associations"]] for n, c in classes.items()}
        # An empty repetition carrier is not a relation: its edge passes through.
        # A class with no members is not a relation: it is a grouping or a
        # repetition carrier, and its edges pass through it.
        carriers = {n for n in classes if not members[n]}

        def resolve(n: str, depth: int = 0) -> str:
            # A grouping with one child passes straight through; one with many
            # cannot, so it stays and the edge keeps the grouping element.
            seen = set()
            while n in carriers and len(children[n]) == 1 and n not in seen:
                seen.add(n)
                n = children[n][0][1]
            return n

        merged: dict[str, str] = {}

        # Shift families: one relation, discriminated by a seeded family row.
        rows = []
        for family in SHIFT_FAMILIES.get(doc, []):
            if family not in classes:
                continue
            element = next((snake(lbl) for c in classes.values()
                            for lbl, tgt, _lo, _hi in children[c["name"]] if tgt == family),
                           snake(family))
            rows.append((family, element))
            merged[family] = f"{doc}_shift"
        if rows:
            structs.setdefault(doc, {})[f"{doc}_shift"] = (
                [("configuration_id", "uuid"), ("family", "text"),
                 ("object_code", "text")] + SHIFT_BLOCK)
            for suffix, cols in SHIFT_ROWS.items():
                structs[doc][f"{doc}_{suffix}"] = cols
                edges.append((doc, f"{doc}_shift", f"{doc}_{suffix}", suffix, "0", "unbounded"))
            notes.append(f'{doc}_shift_config : "one relation for {len(rows)} families. '
                         f'family holds the code: ' + ", ".join(f for f, _e in rows) + '"')
            notes.append(f'{doc}_shift_tenor_config : "tenor_id references the tenor '
                         f'entity; ORE writes these as a comma-separated string"')
            
        # Rows a parent reaches by one element name are one relation, provided
        # their shapes agree.
        by_element: dict[str, list[str]] = {}
        for parent in classes:
            for lbl, target, _lo, _hi in children[parent]:
                tgt = resolve(target)
                if tgt in classes and tgt not in merged:
                    by_element.setdefault(snake(lbl), []).append(tgt)
        for element, targets in sorted(by_element.items()):
            unique = sorted(set(targets))
            if len(unique) < 2:
                continue
            if len({tuple(members[t]) for t in unique}) != 1:
                continue
            name = f"{doc}_{element}"
            structs.setdefault(doc, {})[name] = (
                [("configuration_id", "uuid"), ("parent_id", "uuid"),
                 ("bucket", "text"), ("label1", "text"), ("label2", "text")]
                + list(members[unique[0]]))
            for t in unique:
                merged[t] = name
            notes.append(f'{name} : "one relation for {len(unique)} <{element}> rows"')

        for name in sorted(classes):
            tgt = resolve(name)
            if tgt != name or name in merged or name in carriers:
                continue
            if not members[name]:
                continue
            structs.setdefault(doc, {})[tgt] = list(members[name])

        for parent in sorted(classes):
            src = merged.get(resolve(parent), resolve(parent))
            if src not in structs.get(doc, {}):
                continue
            for lbl, target, low, high in children[parent]:
                tgt = merged.get(resolve(target), resolve(target))
                if tgt not in structs.get(doc, {}):
                    continue
                edges.append((doc, src, tgt, snake(lbl), low, high))

    return structs, edges, notes


def emit(docs: dict, title: str, include_reporting: bool) -> str:
    structs, edges, notes = build(docs, keep_sets())
    structs, edges, notes = tidy_all(structs, edges, notes)
    out = ["@startuml", f"title {title}", "hide empty members",
           "skinparam classAttributeIconSize 0", "left to right direction", ""]
    if include_reporting:
        out += [REPORTING, ""]
    out.append('package "ores.analytics" #EAF7EA {')
    for doc in ANALYTICS_DOCUMENTS:
        for name in sorted(structs.get(doc, {})):
            out.append(f'  class "{name}" as {doc}__{name} {{')
            for member, typ in structs[doc][name]:
                out.append(f"      {member} : {typ}")
            out.append("  }")
    out.append("}")
    rels = list(REPORTING_RELS) if include_reporting else []
    for doc, src, tgt, label, low, high in edges:
        h = "*" if high == "unbounded" else high
        multi = f'"{low}..{h}"' if (h, low) != ("1", "1") else '"1"'
        rels.append(f"{doc}__{src} *-- {multi} {doc}__{tgt} : {label}")
    out.append("")
    out.extend(sorted(set(rels)))
    out.extend(notes)
    out.append("@enduml")
    return "\n".join(out) + "\n"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--inventory",
                    default=".audit/ore-taxonomy/ore_report_configuration_entities.json")
    ap.add_argument("--out-dir", default=".")
    ap.add_argument("--half", choices=("reporting", "analytics"), default="analytics")
    ap.add_argument("--title", default="ORE configuration object model")
    ap.add_argument("--out-name", default=None)
    args = ap.parse_args()

    docs = load(Path(args.inventory))
    text = emit(docs, args.title, include_reporting=(args.half == "reporting"))
    if args.half == "reporting":
        body = text.split('package "ores.analytics"')[0].rstrip()
        # The relationships live after the analytics package, so the split drops
        # them: put the reporting ones back.
        text = "\n".join(body.splitlines()[:-1] + [""] + REPORTING_RELS
                         + ["", "@enduml"]) + "\n"
        name = args.out_name or "configuration_model_reporting.puml"
    else:
        name = args.out_name or "configuration_model_analytics.puml"
    out = Path(args.out_dir) / name
    out.write_text(text)
    structs, _edges, _notes = build(docs, keep_sets())
    counts = {d: len(v) for d, v in structs.items()}
    print(f"wrote {out}  structs total {sum(counts.values())}  {counts}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
