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

The global parameters come from the guide's global parameter section and from the ORE
source that reads them. The release each parameter entered is measured from the ORE
release tags and kept in pricing_parameter_releases.json beside this file.
"""

import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(
    os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "..")),
    "projects", "ores.seeder", "datasets", "ore_pricing_engines"))
import descriptions  # noqa: E402
from ore_parameter_seed import TEXT_KEYS  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
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

# A few names the guide writes as one word, and the capital letters it uses.
NAME_WORDS = {"PseudoCurrency": "Pseudo-currency"}


def humanise(name):
    if name in NAME_WORDS:
        return NAME_WORDS[name]
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


GLOBAL_PARAMETERS = [
    {
        "name": "ContinueOnCalibrationError",
        "domain": "boolean",
        "default_text": "false",
        "label": "Continue on a calibration error",
        "help": "Whether a calibration error stops the trade build.",
        "description": (
            "When it is true, the build does not fail when a model misses its calibration "
            "tolerance. It logs a warning and prices the trade anyway. Leave it false when a "
            "calibration miss must be visible, as in a validation run."
        ),
    },
    {
        "name": "AllowModelFallbacks",
        "domain": "boolean",
        "default_text": "false",
        "label": "Allow model fallbacks",
        "help": "Whether a missing market object uses a fallback.",
        "description": (
            "When it is true, a model build that misses a market object does not fail. The "
            "build uses a fallback instead, such as a zero volatility surface when the surface "
            "is missing. The price then rests on the fallback, so read it with care."
        ),
    },
    {
        "name": "Calibrate",
        "domain": "boolean",
        "default_text": "true",
        "label": "Calibrate",
        "help": "Whether the model is calibrated.",
        "description": (
            "When it is false, model calibration is off. A person does not normally set it: "
            "ORE uses it for its own workflows that do not need a calibration."
        ),
    },
    {
        "name": "GenerateAdditionalResults",
        "domain": "boolean",
        "default_text": "false",
        "label": "Generate additional results",
        "help": "Whether the engines produce results beyond the NPV.",
        "description": (
            "When it is false, the engines that support it do not produce their additional "
            "results. ORE sets it false for a run that needs only the NPV, as in a sensitivity "
            "or a stress, to save time."
        ),
    },
    {
        "name": "RunType",
        "domain": "string",
        "default_text": "",
        "label": "Run type",
        "help": "The kind of run the portfolio is built for.",
        "description": (
            "One of NPV, SensitivityDelta, SensitivityDeltaGamma, Stress, Exposure, Capital, "
            "TradeDetails, PortfolioAnalyser, HistoricalPnL, BondSpreadImply or "
            "AbsMaturityUpdate. ORE sets it from the context. Some engines change what they "
            "compute to suit it, such as a first order pnl expansion for a sensitivity run."
        ),
    },
    {
        "name": "McType",
        "domain": "string",
        "default_text": "",
        "label": "Monte Carlo type",
        "help": "The kind of Monte Carlo the run uses.",
        "description": (
            "One of Classic or American. ORE sets it from the context in which the pricing "
            "engine file is used. Some trade constructions change what they attach to suit it."
        ),
    },
    {
        "name": "StrikeSpread",
        "domain": "double",
        "default_text": "0.1",
        "label": "Strike spread",
        "help": "The strike spread that replication engines use.",
        "description": (
            "It overrides the default value a replication engine uses to compute the strike "
            "spread of a replication portfolio."
        ),
    },
    {
        "name": "BondFutureAmericanAsEuropean",
        "domain": "string",
        "default_text": "false",
        "label": "American bond future as European",
        "help": "Whether an American bond future option is priced as European.",
        "description": (
            "It takes true, false or a regular expression. When it is true, every American "
            "bond future option is priced as though its exercise were European. When it is a "
            "regular expression, only the options whose underlying contract name matches it "
            "are. When it is false, the default, it has no effect."
        ),
    },
    {
        "name": "PseudoCurrency.TreatAsFX",
        "domain": "boolean",
        "default_text": "true",
        "label": "Pseudo-currency: treat as FX",
        "help": "Whether the pseudo-currency market is treated as an FX market.",
        "description": (
            "It passes through to the pure FX handling of the pseudo-currency. With no global "
            "parameters, ORE treats the pseudo-currency as FX."
        ),
    },
    {
        "name": "PseudoCurrency.BaseCurrency",
        "domain": "string",
        "default_text": "USD",
        "label": "Pseudo-currency base currency",
        "help": "The currency the pseudo-currency quotes against.",
        "description": "It names the base currency of the pseudo-currency market, usually USD.",
    },
    {
        "name": "PseudoCurrency.FXIndexTag",
        "domain": "string",
        "default_text": "GENERIC",
        "label": "Pseudo-currency FX index tag",
        "help": "The tag ORE uses when it asks for an FX correlation.",
        "description": (
            "It is the tag of the FX index, such as GENERIC, so ORE requests a correlation "
            "named FX-GENERIC-USD-EUR rather than one named for a real currency pair."
        ),
    },
    {
        "name": "PseudoCurrency.Curve.XAU",
        "domain": "string",
        "default_text": "PM:XAUUSD",
        "label": "Pseudo-currency curve for gold",
        "help": "The curve that prices the pseudo-currency XAU.",
        "description": "It maps the three-letter code XAU to the curve that prices it, such as PM:XAUUSD.",
    },
    {
        "name": "PseudoCurrency.Curve.XAG",
        "domain": "string",
        "default_text": "PM:XAGUSD",
        "label": "Pseudo-currency curve for silver",
        "help": "The curve that prices the pseudo-currency XAG.",
        "description": "It maps the three-letter code XAG to the curve that prices it, such as PM:XAGUSD.",
    },
    {
        "name": "PseudoCurrency.Curve.XPT",
        "domain": "string",
        "default_text": "PM:XPTUSD",
        "label": "Pseudo-currency curve for platinum",
        "help": "The curve that prices the pseudo-currency XPT.",
        "description": "It maps the three-letter code XPT to the curve that prices it, such as PM:XPTUSD.",
    },
    {
        "name": "PseudoCurrency.Curve.XPD",
        "domain": "string",
        "default_text": "PM:XPDUSD",
        "label": "Pseudo-currency curve for palladium",
        "help": "The curve that prices the pseudo-currency XPD.",
        "description": "It maps the three-letter code XPD to the curve that prices it, such as PM:XPDUSD.",
    },
    {
        "name": "PseudoCurrency.DefaultCorrelation",
        "domain": "double",
        "default_text": "",
        "label": "Pseudo-currency default correlation",
        "help": "The correlation to use when the market has none.",
        "description": (
            "It is optional. When it is set, ORE uses it when the market holds no correlation "
            "for the pseudo-currency."
        ),
    },
    {
        "name": "ImplyAbsMaturityForRegex",
        "domain": "string",
        "default_text": "^RED:[0-9A-Z]{9}$",
        "label": "Imply absolute maturity for a trade id",
        "help": "A regular expression on trade ids whose maturity ORE implies.",
        "description": (
            "The ORE examples set it to a regular expression on trade ids. The ORE source in "
            "this checkout does not read it, so a configuration that carries it round-trips "
            "unchanged and the run ignores it. Treat it as a parameter ORE does not document."
        ),
    },
]


def global_rows():
    releases_by_name = releases()
    out = []
    for entry in GLOBAL_PARAMETERS:
        row = {
            "scope": "GlobalParameters",
            "subtype": "",
            "name": entry["name"],
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
            "introduced_in": releases_by_name.get(entry["name"]) or "1.8.0.7 or earlier",
            "removed_in": None,
            "xpath": (f"/PricingEngines/GlobalParameters/"
                      f"Parameter[@name='{entry['name']}']"),
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
        ("Models", descriptions.MODELS, "model", "Model"),
        ("Engines", descriptions.ENGINES, "engine", "Engine"),
    ):
        for code, (description, detail, _source) in sorted(table.items()):
            text = detail if detail else description
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


def rows():
    out = []
    for position, row in enumerate(
        setting_rows() + global_rows() + model_rows() + pair_rows(), start=1
    ):
        out.append({**row, "position": position})
    return out


def missing(all_rows):
    bad = []
    for row in all_rows:
        if any(not str(row.get(key) or "").strip() for key in TEXT_KEYS):
            bad.append(row.get("name", ""))
    return bad
