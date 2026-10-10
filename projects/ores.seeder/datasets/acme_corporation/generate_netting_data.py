"""
Generates the counterparty netting source data for the ACME Corporation
dataset: netting_agreements.json, netting_sets.json and csas.json, read by
generate_sql.py in this same directory.

The panel, the documents and the collateral terms are the ones a group of this
shape would hold. The ORE sample netting data was the starting point (one
agreement per bank, a netting set per agreement, a CSA per collateralised set)
and each of its weaknesses is corrected here:

- Every entity faces the banks that fit its jurisdiction, under the master
  agreement and governing law that jurisdiction uses, not the eight banks the
  ORE samples happen to name.
- A CSA is in the currency its entity margins in, on that currency's current
  overnight index. None uses a retired index.
- The three collateral regimes a bank relationship takes are all present:
  variation margin only, variation and initial margin, and uncollateralised.
- Thresholds, minimum transfer amounts and margin periods differ by regime and
  currency, and are not the same number on both sides by accident.
- The group entity and its subsidiaries also hold agreements with each other.
  The two sides of one intragroup agreement share its reference.

The banks are real legal entities, named by the LEI the GLEIF counterparty set
holds for them. An affiliate with a similar name is a different counterparty.

Usage:
    python3 generate_netting_data.py
"""
import json
import os

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# The legal entity of each bank the panels use, by the LEI GLEIF holds.
BANKS = {
    "JPM": ("7H6GLXDRUGQFU57RNE97", "JPMorgan Chase Bank, National Association"),
    "BOFA": ("B4TYDEB6GKMZO031MB27", "Bank of America, National Association"),
    "CITI": ("E57ODZWZ7FF32TWEFA76", "Citibank, National Association"),
    "BARC": ("G5GSEF7VJP5I7OUK5573", "Barclays Bank PLC"),
    "HSBC": ("MP6I5ZYZBEU3UXPYFY54", "HSBC Bank plc"),
    "DB": ("7LTWFZYICNSX8D621K86", "Deutsche Bank AG"),
    "BNPP": ("R0MUWSFPU8MPRO8K5P83", "BNP Paribas"),
    "UBS": ("BFM8T61CT2L1QCEMIK50", "UBS AG"),
    "HBAP": ("2HI3YI5320L3RW6NJ957",
             "The Hongkong and Shanghai Banking Corporation Limited"),
    "SCB": ("RILFO74KP1CM8P6PCT96", "Standard Chartered Bank"),
}

# The four legal entities: dataset company, short code, LEI, name.
ENTITIES = {
    "acme_group": ("ACGR", "9695ACMEGROUP0000030", "Acme Corporation Plc"),
    "acme_uk": ("ACUK", "9695ACMEUK0000000047", "ACME Corporation UK plc"),
    "acme_us": ("ACUS", "9695ACMEUS0000000043", "ACME Corporation US Inc"),
    "acme_hk": ("ACHK", "9695ACMEHK0000000018", "ACME Corporation HK Ltd"),
}

# The overnight index and the minimum transfer amount of each margin currency.
CURRENCY = {
    "USD": {"index": "USD-SOFR", "mta": 250000.0, "mta_im": 500000.0},
    "GBP": {"index": "GBP-SONIA", "mta": 200000.0, "mta_im": 400000.0},
    "EUR": {"index": "EUR-ESTER", "mta": 250000.0, "mta_im": 500000.0},
}

# The initial margin regulations by the jurisdiction of the entity.
IM_REGULATIONS = {
    "acme_us": "CFTC,PRUDENTIAL",
    "acme_uk": "ESA",
    "acme_group": "ESA",
    "acme_hk": "HKMA",
}

LAW_DOCUMENTS = {
    "English": {
        "vm": "the 2016 Credit Support Annex for Variation Margin (English law, title transfer)",
        "vmim": "the 2016 Credit Support Annex for Variation Margin (English law, title transfer) "
                "and the 2016 Credit Support Deed for Initial Margin (English law, security interest)",
    },
    "New York": {
        "vm": "the 2016 Credit Support Annex for Variation Margin (New York law)",
        "vmim": "the 2016 Credit Support Annex for Variation Margin (New York law) "
                "and the 2018 Credit Support Annex for Initial Margin (New York law)",
    },
}

# (entity, bank, year signed, governing law, [(regime, margin currency, suffix)])
# A regime is vm, vmim or unc. A second set under one agreement holds trades
# the credit support annex does not cover.
BANK_AGREEMENTS = [
    ("acme_us", "JPM", 2015, "New York",
     [("vmim", "USD", ""), ("unc", None, "CMDTY")]),
    ("acme_us", "BOFA", 2017, "New York", [("vm", "USD", "")]),
    ("acme_us", "CITI", 2016, "New York", [("vm", "USD", "")]),
    ("acme_uk", "BARC", 2014, "English",
     [("vmim", "GBP", ""), ("unc", None, "CMDTY")]),
    ("acme_uk", "HSBC", 2016, "English", [("vm", "GBP", "")]),
    ("acme_uk", "DB", 2015, "English", [("vm", "EUR", "")]),
    ("acme_uk", "BNPP", 2018, "English", [("vm", "EUR", "")]),
    ("acme_uk", "UBS", 2012, "English", [("unc", None, "")]),
    ("acme_hk", "HBAP", 2017, "English", [("vm", "USD", "")]),
    ("acme_hk", "SCB", 2018, "English", [("vm", "USD", "")]),
    ("acme_hk", "CITI", 2019, "English", [("vm", "USD", "")]),
    ("acme_group", "HSBC", 2016, "English", [("vm", "GBP", "")]),
    ("acme_group", "JPM", 2015, "New York", [("vm", "USD", "")]),
]

# (first entity, second entity, year signed, governing law, regime, currency)
# Both entities hold the agreement, under the same reference.
INTRAGROUP_AGREEMENTS = [
    ("acme_group", "acme_uk", 2018, "English", "unc", None),
    ("acme_group", "acme_us", 2018, "New York", "unc", None),
    ("acme_group", "acme_hk", 2018, "English", "unc", None),
    ("acme_uk", "acme_us", 2019, "New York", "vm", "USD"),
]

REGIME_TEXT = {
    "vm": "variation margin only",
    "vmim": "variation and initial margin",
    "unc": "uncollateralised",
}


def agreement_description(entity_name, counterparty_name, law, year):
    return (f"ISDA 2002 Master Agreement between {entity_name} and "
            f"{counterparty_name}, {law} law, signed {year}")


def set_description(counterparty_name, regime, law, suffix):
    if suffix == "CMDTY":
        return (f"Commodity derivatives with {counterparty_name} outside the "
                "credit support annex, uncollateralised")
    if regime == "unc":
        return f"OTC derivatives with {counterparty_name}, uncollateralised"
    return (f"OTC derivatives with {counterparty_name} under "
            f"{LAW_DOCUMENTS[law][regime]}, {REGIME_TEXT[regime]}, margined daily")


def csa_row(company, set_code, regime, currency, intragroup):
    ccy = CURRENCY[currency]
    if intragroup:
        mta = 0.0
        threshold = 0.0
    else:
        mta = ccy["mta_im"] if regime == "vmim" else ccy["mta"]
        threshold = 0.0
    eligible = [currency] + [c for c in ("USD", "EUR", "GBP") if c != currency]
    row = {
        "company_code": company,
        "netting_set_code": set_code,
        "is_active": True,
        "bilateral": "Bilateral",
        "csa_currency": currency,
        "index_name": ccy["index"],
        "threshold_pay": threshold,
        "threshold_receive": threshold,
        "minimum_transfer_amount_pay": mta,
        "minimum_transfer_amount_receive": mta,
        "independent_amount_held": 0.0,
        "independent_amount_type": "FIXED",
        "call_frequency": "1D",
        "post_frequency": "1D",
        "margin_period_of_risk": "10D",
        "collateral_compounding_spread_receive": 0.0,
        "collateral_compounding_spread_pay": 0.0,
        "apply_initial_margin": regime == "vmim",
        "initial_margin_type": "Bilateral" if regime == "vmim" else None,
        "calculate_im_amount": regime == "vmim",
        "calculate_vm_amount": True,
        "non_exempt_im_regulations": IM_REGULATIONS[company] if regime == "vmim" else None,
        "eligible_currencies": ",".join(eligible),
    }
    return row


def build():
    agreements, sets, csas = [], [], []

    def add(company, agreement_number, counterparty_code, counterparty_lei, counterparty_name,
            law, year, regimes, intragroup, entity_name):
        agreements.append({
            "company_code": company,
            "agreement_number": agreement_number,
            "counterparty_lei": counterparty_lei,
            "agreement_type": "ISDA",
            "governing_law": law,
            "description": agreement_description(entity_name, counterparty_name, law, year),
        })
        short = ENTITIES[company][0]
        for regime, currency, suffix in regimes:
            parts = [short, counterparty_code]
            if intragroup:
                parts.append("IC")
            if suffix:
                parts.append(suffix)
            parts.append(regime.upper())
            code = "-".join(parts)
            sets.append({
                "company_code": company,
                "code": code,
                "agreement_number": agreement_number,
                "description": (
                    f"Intragroup derivatives with {counterparty_name}, "
                    f"{REGIME_TEXT[regime]}" if intragroup
                    else set_description(counterparty_name, regime, law, suffix)),
            })
            if regime != "unc":
                csas.append(csa_row(company, code, regime, currency, intragroup))

    for company, bank, year, law, regimes in BANK_AGREEMENTS:
        lei, name = BANKS[bank]
        number = f"{ENTITIES[company][0]}-{bank}-ISDA-{year}"
        add(company, number, bank, lei, name, law, year, regimes, False,
            ENTITIES[company][2])

    for first, second, year, law, regime, currency in INTRAGROUP_AGREEMENTS:
        number = f"{ENTITIES[first][0]}-{ENTITIES[second][0]}-ISDA-{year}"
        for company, other in ((first, second), (second, first)):
            add(company, number, ENTITIES[other][0], ENTITIES[other][1], ENTITIES[other][2],
                law, year, [(regime, currency, "")], True, ENTITIES[company][2])

    return agreements, sets, csas


def main():
    agreements, sets, csas = build()
    for name, rows in (("netting_agreements", agreements),
                       ("netting_sets", sets),
                       ("csas", csas)):
        path = os.path.join(SCRIPT_DIR, f"{name}.json")
        with open(path, "w", encoding="utf-8") as f:
            json.dump(rows, f, indent=2)
            f.write("\n")
        print(f"{name}: {len(rows)} rows")


if __name__ == "__main__":
    main()
