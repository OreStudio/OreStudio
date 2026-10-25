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
"""The pricing engine parameters, one row for each parameter of the pricing document.

The words of a product's model and engine parameters come from the ORE user guide,
chapter "Pricing Engine Configuration", through the vocabulary the seeder extracted
from it. Each row is one (product type, scope, name) the guide describes, so the same
name may carry a different text under another product type.

The text of a setting whose guide text is too short, the name fixes, the product types the
guide does not describe and the global parameters are curated data, in the curation files
of the seeder's ore_pricing_engines dataset. The release each parameter entered is measured
from the ORE release tags and kept in pricing_parameter_releases.json beside this file.
"""

import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(
    os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "..")),
    "projects", "ores.seeder", "datasets", "ore_pricing_engines"))
import curation  # noqa: E402
from ore_parameter_seed import TEXT_KEYS  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
CURATED = curation.CURATED
VOCABULARY = os.path.join(ROOT, "projects", "ores.seeder", "datasets",
                          "ore_pricing_engines", "pricing_vocabulary.json")
RELEASES = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                        "pricing_parameter_releases.json")

SCOPE_OF = {"model": "ModelParameters", "engine": "EngineParameters"}

DOMAIN_OF = {"number": "double", "select": "string", "text": "string",
             "tenor": "string", "reference": "string"}

FORMAT_OF = {
    "number": "A number.",
    "text": "Text.",
    "tenor": "A tenor, such as 3M or 1Y.",
    "reference": "The code of a sensitivity template.",
}


def humanise(name):
    curated = CURATED.label(name)
    if curated:
        return curated
    parts = re.split(r"[._]", name)
    words = []
    for part in parts:
        words.extend(re.findall(r"[A-Z]+(?![a-z])|[A-Z]?[a-z]+|[0-9]+", part) or [part])
    words = [word if (len(word) > 1 and word.isupper()) else word.lower() for word in words]
    text = " ".join(words).strip()
    text = re.sub(r"\bFx\b", "FX", text)
    text = re.sub(r"\bUsd\b", "USD", text)
    if text == "":
        return name
    return text[0].upper() + text[1:]


def first_sentence(text):
    match = re.match(r"(.+?[.!?])(\s|$)", text)
    return match.group(1) if match else text


def format_of(kind, options):
    if kind == "select":
        if options:
            return "One of: " + ", ".join(options) + "."
        return "A choice from a fixed list."
    return FORMAT_OF.get(kind, "Text.")


def releases():
    if not os.path.exists(RELEASES):
        return {}
    with open(RELEASES) as f:
        return json.load(f)


def setting_rows():
    with open(VOCABULARY) as f:
        vocabulary = json.load(f)
    releases_by_name = releases()
    chosen = {}
    for type_code, entry in sorted(vocabulary.items()):
        for pair in entry.get("pairs", []):
            usual = bool(pair.get("usual"))
            for setting in pair.get("settings", []):
                key = (type_code, setting["scope"], setting["name"])
                if key in chosen and (not usual or chosen[key]["usual"]):
                    continue
                chosen[key] = {"setting": setting, "usual": usual}
    out = []
    for (type_code, scope, name), picked in sorted(chosen.items()):
        setting = picked["setting"]
        text = (setting.get("text") or "").strip()
        out.append({
            "scope": SCOPE_OF.get(scope, scope),
            "subtype": type_code,
            "name": name,
            "domain": DOMAIN_OF.get(setting.get("kind"), "string"),
            "required": bool(setting.get("required")),
            "label": humanise(name),
            "help": first_sentence(text),
            "description": text,
            "default_text": setting.get("usual") or "Not set in the examples.",
            "format": format_of(setting.get("kind"), setting.get("options") or []),
            "introduced_in": releases_by_name.get(name) or "1.8.0.7 or earlier",
            "removed_in": None,
            "xpath": (f"/PricingEngines/Product[@type='{type_code}']/"
                      f"{SCOPE_OF.get(scope, scope)}/Parameter[@name='{name}']"),
        })
    return out


def global_rows():
    releases_by_name = releases()
    out = []
    for name, entry in CURATED.globals.items():
        row = {
            "scope": "GlobalParameters",
            "subtype": "",
            "name": name,
            "domain": entry["domain"],
            "required": False,
            "label": entry["label"],
            "help": entry["help"],
            "description": entry["description"],
            "default_text": entry["default_text"] or "Not set.",
            "format": {
                "boolean": "A flag, written true or false.",
                "double": "A number.",
                "string": "Text.",
            }[entry["domain"]],
            "introduced_in": releases_by_name.get(name) or "1.8.0.7 or earlier",
            "removed_in": None,
            "xpath": f"/PricingEngines/GlobalParameters/Parameter[@name='{name}']",
        }
        out.append(row)
    return out


def release_at_least(*names):
    """The latest release that holds every name, or the floor of this checkout."""
    versions = [releases().get(name, "") for name in names]
    known = [v for v in versions if v not in ("", "1.8.0.7 or earlier")]
    if not known:
        return "1.8.0.7 or earlier"
    def key(text):
        return [int(part) for part in text.split(".") if part.isdigit()]
    return max(known, key=key)


def model_rows():
    rows = []
    for scope, table, kind, path in (
        ("Models", CURATED.models, "model", "Model"),
        ("Engines", CURATED.engines, "engine", "Engine"),
    ):
        for code, entry in sorted(table.items()):
            description = entry["description"]
            text = entry["detail"] if entry["detail"] else description
            rows.append({
                "scope": scope,
                "subtype": "",
                "name": code,
                "domain": "string",
                "required": False,
                "label": code,
                "help": first_sentence(description),
                "description": text,
                "default_text": "The name of a pricing " + kind + ".",
                "format": "The name of a pricing " + kind + ".",
                "introduced_in": release_at_least(code),
                "removed_in": None,
                "xpath": f"/PricingEngines/Product/{path}[@name='{code}']",
            })
    return rows


def pair_rows():
    with open(VOCABULARY) as f:
        vocabulary = json.load(f)
    seen = {}
    for type_code, entry in sorted(vocabulary.items()):
        for pair in entry.get("pairs", []):
            key = (type_code, pair["model"], pair["engine"])
            if key in seen:
                continue
            seen[key] = pair
    rows = []
    for (type_code, model, engine), pair in sorted(seen.items()):
        note = (pair.get("note") or "").strip()
        if note == "":
            note = f"{model} with {engine} prices {type_code}."
        rows.append({
            "scope": "Pairs",
            "subtype": type_code,
            "name": f"{model}|{engine}",
            "domain": "string",
            "required": False,
            "label": f"{model} / {engine}",
            "help": first_sentence(note),
            "description": note,
            "default_text": "The usual pair." if pair.get("usual") else "Not the usual pair.",
            "format": f"The model and engine that price {type_code} together.",
            "introduced_in": release_at_least(model, engine),
            "removed_in": None,
            "xpath": (f"/PricingEngines/Product[@type='{type_code}']/"
                      f"Model[@name='{model}']/Engine[@name='{engine}']"),
        })
    return rows


def type_row(code, text, help_text):
    return {
        "scope": "Types",
        "subtype": "",
        "name": code,
        "domain": "string",
        "required": False,
        "label": humanise(code),
        "help": help_text,
        "description": text,
        "default_text": "The product type code.",
        "format": "The product type code of an ORE pricing engine.",
        "introduced_in": "1.8.0.7 or earlier",
        "removed_in": None,
        "xpath": f"/PricingEngines/Product[@type='{code}']",
    }


def documented_type_row(code, documentation):
    return type_row(code, documentation, first_sentence(documentation))


def type_rows():
    with open(VOCABULARY) as f:
        vocabulary = json.load(f)
    rows = []
    for code in sorted(set(CURATED.types) - set(vocabulary)):
        rows.append(documented_type_row(code, CURATED.documentation(code)))
    for code, entry in sorted(vocabulary.items()):
        documentation = CURATED.documentation(code)
        if documentation:
            rows.append(documented_type_row(code, documentation))
            continue
        pairs = entry.get("pairs", [])
        usual = next((p for p in pairs if p.get("usual")), pairs[0] if pairs else None)
        used = (entry.get("used_by") or "").strip()
        text = f"The {humanise(code)} product type."
        if used:
            text += f" ORE uses it for the trade type {used.rstrip('.')}."
        if usual is not None:
            text += (f" {len(pairs)} model and engine pair"
                     f"{'s' if len(pairs) != 1 else ''} price it; the usual pair is "
                     f"{usual['model']} with {usual['engine']}.")
        rows.append(type_row(
            code, text,
            f"Used by {used.rstrip('.')}." if used else f"The {humanise(code)} product type."))
    return rows


def rows():
    out = []
    for position, row in enumerate(
        setting_rows() + global_rows() + model_rows() + pair_rows() + type_rows(), start=1
    ):
        out.append({**row, "position": position})
    return out


def missing(all_rows):
    bad = []
    for row in all_rows:
        if any(not str(row.get(key) or "").strip() for key in TEXT_KEYS):
            bad.append(row.get("name", ""))
    return bad
