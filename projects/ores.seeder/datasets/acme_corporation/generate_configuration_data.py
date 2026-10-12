"""
Generates the report configuration source data for the ACME Corporation dataset:
the pricing engines document of each legal entity, read by generate_sql.py in this
same directory.

The documents are Acme's own, crafted for the reports each entity runs. They are
inspired by the ORE examples under external/ore/examples and are not ORE import
data. Which engine prices a product is a choice of the group, and the choice here
follows the desks an entity runs (rule D15 of the sprint 27 investigation "Which
reports each ACME party runs").

Each document is a header, its products and their parameters, staged in three
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

# The global parameters of every document, as the ORE example sets them.
GLOBAL_PARAMETERS = {"ContinueOnCalibrationError": "true"}


# The configuration types a risk report binds a document of, and the name of an
# entity's document of each type. A binding names its document, so the name is fixed
# here and used by the report data too. The conventions have no document of their own:
# the binding means the conventions the entity sees, and the first binding creates the
# configuration that says so. The run settings are named per report, by
# generate_risk_config_data.py.
DOCUMENT_NAMES = {
    "conventions": "{label} conventions",
    "pricing_engines": "{label} pricing engines",
    "todays_market": "{label} today's market",
    "curve_configuration": "{label} curve configuration",
}

# The currencies an entity builds an overnight discount curve for, as
# (overnight index, OIS convention, overnight deposit convention or None). The
# conventions are the ids of the instrument convention set, and each was checked
# against it. A currency with no OIS convention in that set, such as the Korean won
# and the Taiwan dollar, has no curve here, and neither has a currency whose FX
# books are all that the entity holds, such as the Chilean peso.
OVERNIGHT = {
    "USD": ("USD-SOFR", "USD-SOFR-OIS", "USD-ON-SOFR-DEPOSIT"),
    "EUR": ("EUR-ESTER", "EUR-ESTER-OIS", "EUR-ON-DEPOSIT-ESTER"),
    "GBP": ("GBP-SONIA", "GBP-OIS-CONVENTIONS", None),
    "JPY": ("JPY-TONAR", "JPY-OIS-CONVENTIONS", None),
    "CHF": ("CHF-SARON", "CHF-OIS", "CHF-ON-DEPOSIT"),
    "CAD": ("CAD-CORRA", "CAD-OIS-CONVENTIONS", "CAD-ON-DEPOSIT"),
    "AUD": ("AUD-AONIA", "AUD-OIS", "AUD-ON-DEPOSIT"),
    "NZD": ("NZD-OCR", "NZD-OIS", "NZD-ON-DEPOSIT"),
    "SEK": ("SEK-SIOR", "SEK-OIS-CONVENTIONS", "SEK-ON-DEPOSIT"),
    "NOK": ("NOK-NOWA", "NOK-OIS", "NOK-ON-DEPOSIT"),
    "DKK": ("DKK-DESTR", "DKK-DESTR-OIS", "DKK-ON-DEPOSIT-DESTR"),
    "HKD": ("HKD-HONIA", "HKD-OIS", "HKD-ON-HONIA-DEPOSIT"),
    "SGD": ("SGD-SORA", "SGD-OIS", "SGD-ON-DEPOSIT"),
    "ZAR": ("ZAR-ZARONIA", "ZAR-ZARONIA-OIS", "ZAR-ON-DEPOSIT-ZARONIA"),
    "PLN": ("PLN-POLONIA", "PLN-OIS", "PLN-ON-POLONIA-DEPOSIT"),
    "CZK": ("CZK-CZEONIA", "CZK-OIS", "CZK-ON-DEPOSIT"),
    "HUF": ("HUF-BUBORON", "HUF-OIS", "HUF-ON-DEPOSIT"),
    "ILS": ("ILS-TELBORON", "ILS-OIS", "ILS-ON-DEPOSIT"),
    "TRY": ("TRY-TLREF", "TRY-OIS", "TRY-ON-DEPOSIT"),
    "BRL": ("BRL-CDI", "BRL-OIS", "BRL-ON-DEPOSIT"),
    "MXN": ("MXN-FTIIE", "MXN-OIS", "MXN-ON-DEPOSIT"),
    "INR": ("INR-MIBOROIS", "INR-OIS", "INR-ON-DEPOSIT"),
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
# and the start of the swap. Sterling settles on the trade date and the other
# currencies two days after it.
OIS_TENORS = ["1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y"]
SPOT_LAG = {"GBP": "0D"}

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
    for company, wanted in ENTITY_PRODUCTS.items():
        header_id = new_uuid(f"pricing_engines:{company}")
        headers.append({
            "company_code": company,
            "id": header_id,
            "version": 0,
            "party_id": NO_PARTY,
            "name": document_name(company, "pricing_engines"),
            "description": f"The model and engine that price each product ACME Corporation "
                           f"{LABEL[company]} trades.",
            "config_variant": "",
            "configuration_id": new_uuid(f"configuration:pricing_engines:{company}"),
        })
        for position, product_type in enumerate(wanted):
            model, model_parameters, engine, engine_parameters = PRODUCTS[product_type]
            product_id = new_uuid(f"pricing_engines:{company}:{product_type}")
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
                for index, (name, value) in enumerate(values.items()):
                    parameters.append({
                        "company_code": company,
                        "id": new_uuid(f"pricing_engines:{company}:{product_type}:{scope}:{name}"),
                        "version": 0,
                        "party_id": NO_PARTY,
                        "pricing_model_config_id": header_id,
                        "pricing_model_product_id": product_id,
                        "parameter_scope": scope,
                        "parameter_name": name,
                        "parameter_value": value,
                        "position": index,
                    })
        for index, (name, value) in enumerate(GLOBAL_PARAMETERS.items()):
            parameters.append({
                "company_code": company,
                "id": new_uuid(f"pricing_engines:{company}:global:{name}"),
                "version": 0,
                "party_id": NO_PARTY,
                "pricing_model_config_id": header_id,
                "pricing_model_product_id": None,
                "parameter_scope": "global",
                "parameter_name": name,
                "parameter_value": value,
                "position": index,
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
    """An overnight discount curve for each currency of the entity, built from a
    deposit and overnight swaps, on the quotes ORE's own examples use."""
    headers, definitions, yields, segments, quotes = [], [], [], [], []
    for company, currencies in ENTITY_CURRENCIES.items():
        header_id = new_uuid(f"curve_configuration:{company}")
        headers.append(row(
            company, id=header_id, name=document_name(company, "curve_configuration"),
            description=f"The curves ACME Corporation {LABEL[company]} marks its trades on.",
            configuration_id=new_uuid(f"configuration:curve_configuration:{company}")))
        for position, currency in enumerate(currencies):
            index, ois_convention, deposit_convention = OVERNIGHT[currency]
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

            next_position = 0
            if deposit_convention:
                segment(next_position, "Deposit", deposit_convention,
                        [f"MM/RATE/{currency}/0D/1D"])
                next_position += 1
            lag = SPOT_LAG.get(currency, "2D")
            segment(next_position, "OIS", ois_convention,
                    [f"IR_SWAP/RATE/{currency}/{lag}/1D/{tenor}" for tenor in OIS_TENORS])
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
