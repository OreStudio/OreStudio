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


# The configuration types a risk report binds, and the name of an entity's document
# of each type. A binding names its document, so the name is fixed here and used by
# the report data too.
DOCUMENT_NAMES = {
    "pricing_engines": "{label} pricing engines",
}


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


def main():
    headers, products, parameters = pricing_engines()
    write("pricing_model_configs", headers)
    write("pricing_model_products", products)
    write("pricing_model_product_parameters", parameters)


if __name__ == "__main__":
    main()
