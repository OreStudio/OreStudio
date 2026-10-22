"""
Generates the report configuration source data for the ACME Corporation dataset:
the pricing engines document of each legal entity, read by generate_sql.py in this
same directory.

The documents are Acme's own, crafted for the reports each entity runs. They are
inspired by the ORE examples under external/ore/examples and are not ORE import
data. Which engine prices a product is a choice of the group, and the choice here
follows the desks an entity runs (rule D15 of the sprint 27 investigation "Which
reports each ACME party runs").

Each entity holds several documents, one for each job its reports do, and a report
binds exactly one of them. Each document is a header, its products and their parameters, staged in three
files that match the DQ artefact tables of the pricing engines document:
pricing_model_configs.json, pricing_model_products.json and
pricing_model_product_parameters.json. The staged ids are fixed by hashing, so a
regeneration reproduces them.

Usage:
    python3 generate_configuration_data.py
"""
import json
import os
import uuid

import generate_party_data as parties

NAMESPACE = uuid.UUID("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# The party the staged rows name. The publish stamps the party it is given, so
# the staged value is only a placeholder.
NO_PARTY = "00000000-0000-0000-0000-000000000000"

LABEL = {
    "acme_group": "Group",
    "acme_uk": "UK",
    "acme_us": "US",
    "acme_hk": "HK",
}

# Every product an Acme entity prices, with the model and engine that price it.
# (product type, model, model parameters, engine, engine parameters). The choices
# follow the ORE examples: discounted cash flows for linear products, a
# Garman-Kohlhagen model for FX options and Black-Bachelier for swaptions.
PRODUCTS = {
    "Swap": ("DiscountedCashflows", {}, "DiscountingSwapEngine", {}),
    "CrossCurrencySwap": ("DiscountedCashflows", {}, "DiscountingCrossCurrencySwapEngine", {}),
    "FxForward": ("DiscountedCashflows", {}, "DiscountingFxForwardEngine", {}),
    "CreditDefaultSwap": ("DiscountedCashflows", {}, "MidPointCdsEngine", {}),
    "Bond": ("DiscountedCashflows", {}, "DiscountingRiskyBondEngine", {"TimestepPeriod": "6M"}),
    "FxOption": ("GarmanKohlhagen", {}, "AnalyticEuropeanEngine", {}),
    "CapFloor": ("IborCapModel", {}, "IborCapEngine", {}),
    "EuropeanSwaption": ("BlackBachelier", {}, "BlackBachelierSwaptionEngine", {}),
}

# The pairs and settings ORE documents for every product type, extracted from the ORE user
# guide by ../ore_pricing_engines/generate_vocabulary_sql.py.
with open(os.path.join(SCRIPT_DIR, "..", "ore_pricing_engines", "pricing_vocabulary.json")) as handle:
    VOCABULARY = json.load(handle)


def usual_pair(product_type):
    """The model and engine ORE lists first for a product type, or None when it lists none."""
    pairs = VOCABULARY.get(product_type, {}).get("pairs", [])
    return next((p for p in pairs if p["usual"]), None)


def required_settings(product_type, model, engine):
    """The settings ORE needs for the pair, with the value its guide shows, as (model, engine)."""
    pair = next((p for p in VOCABULARY.get(product_type, {}).get("pairs", [])
                 if p["model"] == model and p["engine"] == engine), None)
    found = {"model": {}, "engine": {}}
    for setting in (pair or {"settings": []})["settings"]:
        if setting["required"] and setting["usual"] is not None and setting["scope"] in found:
            found[setting["scope"]][setting["name"]] = setting["usual"]
    return found["model"], found["engine"]


# What each entity prices. The holding company holds intercompany loans and the
# hedges of its subsidiaries' currencies, so it prices linear products only. A
# trading entity prices the products its desks trade. London alone holds option
# books (design rule D8), so it alone prices options.
ENTITY_PRODUCTS = {
    "acme_group": ["Swap", "FxForward", "Bond"],
    "acme_uk": ["Swap", "CrossCurrencySwap", "FxForward", "CreditDefaultSwap", "Bond",
                "FxOption", "CapFloor", "EuropeanSwaption"],
    "acme_us": ["Swap", "CrossCurrencySwap", "FxForward", "CreditDefaultSwap", "Bond"],
    "acme_hk": ["Swap", "CrossCurrencySwap", "FxForward", "CreditDefaultSwap", "Bond"],
}

# The engine settings of the American Monte Carlo engine, as the ORE example
# AmericanMonteCarlo/Input/pricingengine_amc.xml sets them for a swap and for a
# cross currency swap: a training run on a fixed seed, then pricing on the
# Sobol sequence.
def amc(samples, order):
    return {
        "Training.Sequence": "MersenneTwisterAntithetic",
        "Training.Seed": "42",
        "Training.Samples": str(samples),
        "Pricing.Sequence": "SobolBrownianBridge",
        "Pricing.Seed": "17",
        "Pricing.Samples": "0",
        "Training.BasisFunction": "Monomial",
        "Training.BasisFunctionOrder": str(order),
    }


def on_amc(samples, order):
    return ("CrossAssetModel", {}, "AMC", amc(samples, order))


# The sensitivity method ORE offers a swap, and its key.
SENSITIVITIES = {"SensitivityTemplate": "IR_Analytical"}

# The pricing engine configurations a party holds. A report binds exactly one, so
# each is named for the job its reports do and not for the products it prices.
# The cycle (end of day or intraday), the risk question and the rule a regulator
# sets all change the model, the engine or its settings.
#
# - skip: product types the configuration does not price.
# - bond_step: the step of the default probability grid of a bond. A finer step
#   costs more and is for the numbers that are booked or filed.
# - calibration_stops: whether a failed calibration stops the run. An official
#   number must not be built on a model that did not fit, and an intraday or
#   stress run must go on with the last fit it has.
# - replace: the (model, engine, engine settings) used in place of the usual
#   ones for a product type.
CONFIGURATIONS = {
    "EOD Official": {
        "description": "The numbers that are booked. Every product type ORE prices, each on its "
                       "usual pair, a fine grid for bonds and a stop on any calibration that "
                       "fails.",
        "all_products": True,
        "bond_step": "6M",
        "calibration_stops": True,
    },
    "Intraday Indicative": {
        "description": "A fast view of the book during the day. The usual pair for each "
                       "product with a coarse bond grid. A failed calibration does not stop "
                       "the run. The numbers are not official.",
        "bond_step": "1Y",
        "calibration_stops": False,
    },
    "Pre-Trade Quick Price": {
        "description": "The price a trader sees while a trade is built. It answers at once, so "
                       "it uses a coarse bond grid and leaves out credit default swaps.",
        "skip": ["CreditDefaultSwap"],
        "bond_step": "1Y",
        "calibration_stops": False,
    },
    "Risk Sensitivities": {
        "description": "The prices that the delta, gamma and DV01 reports bump. A swap offers its "
                       "analytic sensitivities, and a failed calibration stops the run so a "
                       "bump never mixes two fits.",
        "bond_step": "6M",
        "calibration_stops": True,
        "settings": {"Swap": SENSITIVITIES},
    },
    "XVA Overnight": {
        "description": "The overnight run that simulates exposure. Swaps and cross currency swaps "
                       "use the American Monte Carlo engine on a fixed seed, and the rest keep "
                       "the usual pair.",
        "bond_step": "3M",
        "calibration_stops": False,
        "replace": {"Swap": on_amc(50000, 6), "CrossCurrencySwap": on_amc(10000, 2)},
    },
    "Regulatory FRTB": {
        "description": "The prices behind the capital charge of the fundamental review. The "
                       "sensitivities are analytic, the bond grid is fine and a failed "
                       "calibration stops the run.",
        "bond_step": "3M",
        "calibration_stops": True,
        "settings": {"Swap": SENSITIVITIES},
    },
    "Regulatory SIMM": {
        "description": "The prices behind the initial margin of uncleared derivatives. Bonds are "
                       "not margined, so they are left out.",
        "skip": ["Bond"],
        "calibration_stops": True,
        "settings": {"Swap": SENSITIVITIES},
    },
    "Stress Test Shocked": {
        "description": "The prices under large shocks. The usual pair for each product, a fine "
                       "bond grid, and a failed calibration does not stop the run because a "
                       "shock can break a fit.",
        "bond_step": "3M",
        "calibration_stops": False,
    },
    "Quant Sandbox": {
        "description": "Where the quants try a model before it goes live. Swaps use a small "
                       "American Monte Carlo run. Nothing booked or filed uses it.",
        "bond_step": "6M",
        "calibration_stops": False,
        "replace": {"Swap": on_amc(5000, 2)},
    },
}

# Which configurations each entity holds. The holding company has no desk, so it
# holds the official numbers and the overnight exposure run. A trading entity
# holds what its desks and its regulator need. London alone holds the
# sensitivities, the margin prices and the sandbox: it has the option books and
# the quants (design rule D8), and the other entities take their sensitivities
# from the official numbers.
ENTITY_CONFIGURATIONS = {
    "acme_group": ["EOD Official", "XVA Overnight"],
    "acme_uk": list(CONFIGURATIONS),
    "acme_us": ["EOD Official", "Intraday Indicative", "Pre-Trade Quick Price",
                "XVA Overnight", "Regulatory FRTB", "Stress Test Shocked"],
    "acme_hk": ["EOD Official", "Intraday Indicative", "Pre-Trade Quick Price",
                "XVA Overnight", "Regulatory FRTB", "Stress Test Shocked"],
}

# The configuration a report prefers. A report not named here runs on the official
# numbers. An entity that does not hold the preferred configuration falls back to
# the official one.
DEFAULT_PRICING = "EOD Official"
REPORT_PRICING = {
    "Intraday Risk Monitor": "Intraday Indicative",
    "Headline Position": "Intraday Indicative",
    "Exposure": "XVA Overnight",
    "CVA/DVA/FVA": "XVA Overnight",
    "Model Calibration": "XVA Overnight",
    "Group Consolidated Exposure": "XVA Overnight",
    "Cross-Entity Counterparty Concentration": "XVA Overnight",
    "Intercompany Exposure Matrix": "XVA Overnight",
    "Delta and Gamma": "Risk Sensitivities",
    "Bucketed DV01": "Risk Sensitivities",
    "Vega": "Risk Sensitivities",
    "Stressed VaR": "Stress Test Shocked",
    "FRTB SA Capital": "Regulatory FRTB",
}


def pricing_for_report(company, report_name):
    """The name of the one pricing engine configuration the report binds."""
    wanted = REPORT_PRICING.get(report_name, DEFAULT_PRICING)
    return wanted if wanted in ENTITY_CONFIGURATIONS[company] else DEFAULT_PRICING


# The global parameter that says whether a failed calibration stops the run.
CALIBRATION_PARAMETER = "ContinueOnCalibrationError"


# The configuration types a risk report binds a document of, and the name of an
# entity's document of each type. A binding names its document, so the name is fixed
# here and used by the report data too. The conventions have no document of their own:
# the binding means the conventions the entity sees, and the first binding creates the
# configuration that says so. The run settings are named per report, by
# generate_risk_config_data.py.
DOCUMENT_NAMES = {
    "conventions": "{label} conventions",
    "todays_market": "{label} today's market",
    "curve_configuration": "{label} curve configuration",
}

# The currencies an entity builds an overnight discount curve for, as (overnight
# index, OIS convention). The conventions are the ids of the instrument convention
# set, and each was checked against it. A currency with no OIS convention in that
# set, such as the Korean won and the Taiwan dollar, has no curve here, and neither
# has a currency whose FX books are all that the entity holds, such as the Chilean
# peso.
OVERNIGHT = {
    "USD": ("USD-SOFR", "USD-SOFR-OIS"),
    "EUR": ("EUR-ESTER", "EUR-ESTER-OIS"),
    "GBP": ("GBP-SONIA", "GBP-OIS-CONVENTIONS"),
    "JPY": ("JPY-TONAR", "JPY-OIS-CONVENTIONS"),
    "CHF": ("CHF-SARON", "CHF-OIS"),
    "CAD": ("CAD-CORRA", "CAD-OIS-CONVENTIONS"),
    "AUD": ("AUD-AONIA", "AUD-OIS"),
    "NZD": ("NZD-OCR", "NZD-OIS"),
    "SEK": ("SEK-SIOR", "SEK-OIS-CONVENTIONS"),
    "NOK": ("NOK-NOWA", "NOK-OIS"),
    "DKK": ("DKK-DESTR", "DKK-DESTR-OIS"),
    "HKD": ("HKD-HONIA", "HKD-OIS"),
    "SGD": ("SGD-SORA", "SGD-OIS"),
    "ZAR": ("ZAR-ZARONIA", "ZAR-ZARONIA-OIS"),
    "PLN": ("PLN-POLONIA", "PLN-OIS"),
    "CZK": ("CZK-CZEONIA", "CZK-OIS"),
    "HUF": ("HUF-BUBORON", "HUF-OIS"),
    "ILS": ("ILS-TELBORON", "ILS-OIS"),
    "TRY": ("TRY-TLREF", "TRY-OIS"),
    "BRL": ("BRL-CDI", "BRL-OIS"),
    "MXN": ("MXN-FTIIE", "MXN-OIS"),
    "INR": ("INR-MIBOROIS", "INR-OIS"),
}

# The holding company holds Group Treasury, whose intercompany loans and hedges
# are in the currencies of its subsidiaries. A trading entity builds a curve for
# each of its currencies that has an overnight convention.
ENTITY_CURRENCIES = {
    "acme_group": ["GBP", "USD", "HKD", "EUR"],
    "acme_uk": [c for c in parties.CURRENCIES["acme_uk"] if c in OVERNIGHT],
    "acme_us": [c for c in parties.CURRENCIES["acme_us"] if c in OVERNIGHT],
    "acme_hk": [c for c in parties.CURRENCIES["acme_hk"] if c in OVERNIGHT],
}

# The overnight swap tenors the curve is built from, and the days between the trade
# and the start of the swap. The synthetic feeds publish exactly this pillar ladder
# and start every pillar on the trade date: see the template entries of
# synthetic_feeds_bloomberg_populate.sql, which is the ladder a live deployment fills
# and so the ladder a curve has to name. One year is 12M and not 1Y, because the
# RATES_SPOT_FORWARD convention the feed resolves its pillars under names the year that
# way and would refuse a pillar code it has no resolution for. A deposit segment would
# name a money market quote no feed publishes, which is why an overnight curve here is
# swaps throughout.
OIS_TENORS = ["1M", "3M", "6M", "12M", "2Y", "3Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y"]
SPOT_LAG = "0D"

# The currencies that quote against the dollar with the dollar as the base, and
# the ones that quote with the dollar as the quote. A pair follows market
# convention: euro, sterling, the Australian and New Zealand dollars quote
# against the dollar, and the rest the other way round.
DOLLAR_QUOTED = ["EUR", "GBP", "AUD", "NZD"]


def document_name(company, type_code):
    return DOCUMENT_NAMES[type_code].format(label=LABEL[company])


def new_uuid(key):
    return str(uuid.uuid5(NAMESPACE, key))


def write(name, records):
    path = os.path.join(SCRIPT_DIR, f"{name}.json")
    with open(path, "w") as f:
        json.dump(records, f, indent=2, ensure_ascii=False)
        f.write("\n")
    print(f"Wrote {path} ({len(records)} records)")


def pricing_engines():
    headers, products, parameters = [], [], []
    for company, names in ENTITY_CONFIGURATIONS.items():
        for name in names:
            spec = CONFIGURATIONS[name]
            key = f"pricing_engines:{company}:{name}"
            header_id = new_uuid(key)
            headers.append({
                "company_code": company,
                "id": header_id,
                "version": 0,
                "party_id": NO_PARTY,
                "name": name,
                "description": spec["description"],
                "config_variant": "",
                "configuration_id": new_uuid(f"configuration:{key}"),
            })
            wanted = [t for t in ENTITY_PRODUCTS[company] if t not in spec.get("skip", [])]
            if spec.get("all_products"):
                wanted += sorted(t for t in VOCABULARY if t not in wanted and usual_pair(t))
            for position, product_type in enumerate(wanted):
                if product_type in spec.get("replace", {}) or product_type in PRODUCTS:
                    model, model_parameters, engine, engine_parameters = spec.get(
                        "replace", {}).get(product_type, PRODUCTS.get(product_type))
                else:
                    pair = usual_pair(product_type)
                    model, model_parameters, engine, engine_parameters = (
                        pair["model"], {}, pair["engine"], {})
                needed_model, needed_engine = required_settings(product_type, model, engine)
                model_parameters = {**needed_model, **model_parameters}
                engine_parameters = {**needed_engine, **engine_parameters}
                if "TimestepPeriod" in engine_parameters:
                    engine_parameters["TimestepPeriod"] = spec["bond_step"]
                engine_parameters.update(spec.get("settings", {}).get(product_type, {}))
                product_id = new_uuid(f"{key}:{product_type}")
                products.append({
                    "company_code": company,
                    "id": product_id,
                    "version": 0,
                    "party_id": NO_PARTY,
                    "pricing_model_config_id": header_id,
                    "pricing_engine_type_code": product_type,
                    "model": model,
                    "engine": engine,
                    "position": position,
                })
                for scope, values in (("model", model_parameters), ("engine", engine_parameters)):
                    for index, (setting, value) in enumerate(values.items()):
                        parameters.append({
                            "company_code": company,
                            "id": new_uuid(f"{key}:{product_type}:{scope}:{setting}"),
                            "version": 0,
                            "party_id": NO_PARTY,
                            "pricing_model_config_id": header_id,
                            "pricing_model_product_id": product_id,
                            "parameter_scope": scope,
                            "parameter_name": setting,
                            "parameter_value": value,
                            "position": index,
                        })
            parameters.append({
                "company_code": company,
                "id": new_uuid(f"{key}:global:{CALIBRATION_PARAMETER}"),
                "version": 0,
                "party_id": NO_PARTY,
                "pricing_model_config_id": header_id,
                "pricing_model_product_id": None,
                "parameter_scope": "global",
                "parameter_name": CALIBRATION_PARAMETER,
                "parameter_value": "false" if spec["calibration_stops"] else "true",
                "position": 0,
            })
    return headers, products, parameters


def row(company, **values):
    return {"company_code": company, "version": 0, "party_id": NO_PARTY, **values}


def fx_pairs(company):
    """The pairs against the dollar of the entity's currencies, in market convention."""
    pairs = []
    for currency in ENTITY_CURRENCIES[company]:
        if currency == "USD":
            continue
        pairs.append((currency, "USD") if currency in DOLLAR_QUOTED else ("USD", currency))
    return pairs


def todays_market():
    """One configuration, default, that discounts every trade on its own overnight curve."""
    headers, collections, entries, configurations, bindings = [], [], [], [], []
    for company in ENTITY_CURRENCIES:
        header_id = new_uuid(f"todays_market:{company}")
        headers.append(row(
            company, id=header_id, name=document_name(company, "todays_market"),
            description=f"How ACME Corporation {LABEL[company]} marks its trades: each currency "
                        "on its own overnight curve.",
            config_variant="",
            configuration_id=new_uuid(f"configuration:todays_market:{company}")))

        def collection(position, kind, collection_id, rows):
            cid = new_uuid(f"todays_market:{company}:{kind}:{collection_id}")
            collections.append(row(company, id=cid, todays_market_config_id=header_id,
                                   collection=kind, collection_id=collection_id,
                                   position=position))
            for index, (key, target) in enumerate(rows):
                entries.append(row(
                    company, id=new_uuid(f"todays_market:{company}:{kind}:{key}"),
                    todays_market_config_id=header_id, todays_market_collection_id=cid,
                    key_value=key, key_value_2=None, target=target, discounting=None,
                    position=index))

        currencies = ENTITY_CURRENCIES[company]
        collection(0, "DiscountingCurves", "ois",
                   [(c, f"Yield/{c}/{c}1D") for c in currencies])
        collection(1, "IndexForwardingCurves", "default",
                   [(OVERNIGHT[c][0], f"Yield/{c}/{c}1D") for c in currencies])
        collection(2, "FxSpots", "default",
                   [(f"{b}{q}", f"FX/{b}/{q}") for b, q in fx_pairs(company)])

        configuration_id = new_uuid(f"todays_market:{company}:configuration:default")
        configurations.append(row(
            company, id=configuration_id, todays_market_config_id=header_id,
            configuration_id="default", position=0))
        for index, (kind, reference) in enumerate(
                [("DiscountingCurves", "ois"), ("IndexForwardingCurves", "default"),
                 ("FxSpots", "default")]):
            bindings.append(row(
                company, id=new_uuid(f"todays_market:{company}:binding:{kind}"),
                todays_market_configuration_id=configuration_id, collection=kind,
                reference=reference, position=index))
    return headers, collections, entries, configurations, bindings


def curve_configuration():
    """An overnight discount curve for each currency of the entity, built from the
    overnight swap ladder the synthetic feeds publish."""
    headers, definitions, yields, segments, quotes = [], [], [], [], []
    for company, currencies in ENTITY_CURRENCIES.items():
        header_id = new_uuid(f"curve_configuration:{company}")
        headers.append(row(
            company, id=header_id, name=document_name(company, "curve_configuration"),
            description=f"The curves ACME Corporation {LABEL[company]} marks its trades on.",
            configuration_id=new_uuid(f"configuration:curve_configuration:{company}")))
        for position, currency in enumerate(currencies):
            index, ois_convention = OVERNIGHT[currency]
            curve_id = f"{currency}1D"
            definition_id = new_uuid(f"curve_configuration:{company}:{curve_id}")
            definitions.append(row(
                company, id=definition_id, curve_configuration_id=header_id,
                section_code="YieldCurves", curve_id=curve_id,
                description=f"{currency} discount curve bootstrapped from {index} swap rates",
                position=position))
            yields.append(row(
                company, id=new_uuid(f"curve_configuration:{company}:{curve_id}:yield"),
                curve_definition_id=definition_id, currency=currency, discount_curve=curve_id,
                interpolation_variable="Discount", interpolation_method="LogLinear",
                day_counter="A365", tolerance=1e-12, has_report=False))

            def segment(segment_position, kind, convention, quote_keys):
                segment_id = new_uuid(f"curve_configuration:{company}:{curve_id}:{kind}")
                segments.append(row(
                    company, id=segment_id, curve_definition_id=definition_id,
                    segment_type=kind, position=segment_position, conventions=convention))
                for number, key in enumerate(quote_keys):
                    quotes.append(row(
                        company,
                        id=new_uuid(f"curve_configuration:{company}:{curve_id}:{kind}:{key}"),
                        curve_definition_id=definition_id, curve_segment_id=segment_id,
                        quote_text=key, position=number))

            segment(0, "OIS", ois_convention,
                    [f"IR_SWAP/RATE/{currency}/{SPOT_LAG}/1D/{tenor}" for tenor in OIS_TENORS])
    return headers, definitions, yields, segments, quotes


def main():
    headers, products, parameters = pricing_engines()
    write("pricing_model_configs", headers)
    write("pricing_model_products", products)
    write("pricing_model_product_parameters", parameters)

    names = ("todays_market_configs", "todays_market_collections", "todays_market_entries",
             "todays_market_configurations", "todays_market_configuration_bindings")
    for name, records in zip(names, todays_market()):
        write(name, records)

    names = ("curve_configurations", "curve_definitions", "yield_curve_configs", "curve_segments",
             "curve_quotes")
    for name, records in zip(names, curve_configuration()):
        write(name, records)


if __name__ == "__main__":
    main()
