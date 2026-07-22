#!/usr/bin/env python3
"""
Generates the Acme Bank source-data JSON dataset.

Produces flat-array JSON files -- one per DQ artefact family (LEI
entities, LEI relationships, business units, portfolios, books,
accounts) -- matching the shape of the corresponding
ores_dq_*_artefact_tbl columns, following the same convention as
projects/ores.seeder/datasets/slovaris's country_currency.json: a flat
array of records, field names chosen to map directly onto SQL columns,
consumed later by ores.codegen mustache templates (a separate task).

The org-chart shape (which desks, how many people) is fixed per
office in OFFICES below; the filler data (names, emails, addresses)
is generated -- Faker for locales it supports (GB, US), a curated
name pool for Hong Kong (Faker has no English-script HK locale).

Usage:
    python3 generate_data.py
"""

import json
import os
import uuid

from faker import Faker

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# Fixed namespace for deterministic UUIDs (uuid5), so regenerating the
# dataset without changing its shape produces identical output -- an
# additive diff when e.g. a new office is added, not a full rewrite.
NAMESPACE = uuid.UUID("6f2a6c1e-6b8c-4e6a-9b8e-2b6b6b6b6b6b")

# GLEIF's reserved pre-LOU test prefix -- never assigned to a real entity.
LEI_TEST_PREFIX = "9695"

# Real scrypt hash of "Secure-Password-123", computed via the actual
# ores::security::crypto::password_hasher (same convention as
# barclays_system_provision.ores's tenant_admin/super_admin password).
# Every generated account shares this single hash/password.
FIXED_PASSWORD_HASH = (
    "$scrypt$ln=14,r=8,p=1$0h1vacFtQdXZd9nqj+0G/Q=="
    "$4zoqrV+ojPxe/IwWAMLDXGHZQYJQGYgU6270fCl4kcXYep+uLyganiEM9ARBGQa83MbfvyurLPH304ZTdy8msw=="
)

HK_GIVEN_NAMES = [
    "Wai Ming", "Siu Fung", "Ka Yee", "Chi Wing", "Man Yee", "Tsz Ching",
    "Ho Yin", "Ying Ying", "Kwok Wing", "Suet Ying", "Hoi Yan", "Chun Kit",
]
HK_FAMILY_NAMES = [
    "Chan", "Wong", "Lee", "Cheung", "Ng", "Lau", "Tang", "Lam", "Yip", "Ho",
]


def lei_check_digits(base18: str) -> str:
    """ISO 17442 (mod-97) check digits for an 18-character LEI prefix."""
    digits = ""
    for ch in base18:
        digits += ch if ch.isdigit() else str(ord(ch.upper()) - ord("A") + 10)
    remainder = int(digits + "00") % 97
    return f"{98 - remainder:02d}"


def make_lei(entity_code: str) -> str:
    """Builds a checksum-valid LEI on the 9695 test prefix from a short code."""
    base = (LEI_TEST_PREFIX + entity_code.upper().replace("_", "")).ljust(18, "0")[:18]
    return base + lei_check_digits(base)


def new_uuid(key: str) -> str:
    """Deterministic uuid5 off a stable key, so regenerating the dataset
    without changing its shape reproduces identical IDs."""
    return str(uuid.uuid5(NAMESPACE, key))


# =============================================================================
# Office configuration -- the fixed shape. Add an office here (e.g. Singapore)
# to extend the dataset; no other code changes needed.
# =============================================================================

OFFICES = [
    {
        "code": "acme_uk",
        "name": "Acme Bank UK plc",
        "country": "GB",
        "business_centre_code": "GBLO",
        "locale": "en_GB",
        "city": "London",
    },
    {
        "code": "acme_us",
        "name": "Acme Bank US Inc",
        "country": "US",
        "business_centre_code": "USNY",
        "locale": "en_US",
        "city": "New York",
    },
    {
        "code": "acme_hk",
        "name": "Acme Bank HK Ltd",
        "country": "HK",
        "business_centre_code": "HKHK",
        "locale": None,  # curated name pool, see HK_GIVEN_NAMES/HK_FAMILY_NAMES
        "city": "Hong Kong",
    },
]

DESKS = [
    {"code": "ir_swaps", "name": "IR Swaps", "head_count": 1, "trader_count": 2},
    {"code": "credit_trading", "name": "Credit Trading", "head_count": 1, "trader_count": 2},
    {"code": "fx_rates", "name": "FX Rates", "head_count": 1, "trader_count": 2},
]

SUPPORT_FUNCTIONS = [
    {"code": "middle_office", "name": "Middle Office", "manager_count": 1, "analyst_count": 2},
    {"code": "market_risk", "name": "Market Risk", "manager_count": 1, "analyst_count": 2},
]

HOLDING = {
    "code": "acme_group",
    "name": "Acme Bank Group plc",
    "country": "GB",
    "city": "London",
}


def build_holding_lei_entity():
    lei = make_lei(HOLDING["code"])
    return lei, {
        "lei": lei,
        "entity_legal_name": HOLDING["name"],
        "entity_entity_category": "GENERAL",
        "entity_entity_status": "ACTIVE",
        "entity_legal_jurisdiction": HOLDING["country"],
        "entity_legal_address_city": HOLDING["city"],
        "entity_legal_address_country": HOLDING["country"],
        "entity_headquarters_address_city": HOLDING["city"],
        "entity_headquarters_address_country": HOLDING["country"],
    }


def build_office_lei_entity(office, faker):
    lei = make_lei(office["code"])
    city = office["city"]
    address = faker.street_address() if faker else f"{HK_GIVEN_NAMES[0]} Building, Central"
    return lei, {
        "lei": lei,
        "entity_legal_name": office["name"],
        "entity_entity_category": "GENERAL",
        "entity_entity_status": "ACTIVE",
        "entity_legal_jurisdiction": office["country"],
        "entity_legal_address_first_address_line": address,
        "entity_legal_address_city": city,
        "entity_legal_address_country": office["country"],
        "entity_headquarters_address_first_address_line": address,
        "entity_headquarters_address_city": city,
        "entity_headquarters_address_country": office["country"],
    }


def build_relationship(parent_lei, child_lei):
    return {
        "relationship_start_node_node_id": child_lei,
        "relationship_start_node_node_id_type": "LEI",
        "relationship_end_node_node_id": parent_lei,
        "relationship_end_node_node_id_type": "LEI",
        "relationship_relationship_type": "IS_DIRECTLY_CONSOLIDATED_BY",
        "relationship_relationship_status": "ACTIVE",
    }


def gen_hk_name(index):
    given = HK_GIVEN_NAMES[index % len(HK_GIVEN_NAMES)]
    family = HK_FAMILY_NAMES[index % len(HK_FAMILY_NAMES)]
    return given, family


def gen_person(office, faker, index):
    if faker:
        first = faker.first_name()
        last = faker.last_name()
    else:
        first, last = gen_hk_name(index)
    slug_first = first.lower().replace(" ", ".")
    slug_last = last.lower().replace(" ", ".")
    # index is folded into both username and email: two people can
    # legitimately draw the same Faker name within an office, and email
    # is unique per (tenant_id, email) -- see accounts_email_uniq_idx in
    # iam_accounts_create.sql.
    username = f"{slug_first}.{slug_last}.{office['code']}.{index}"
    email = f"{slug_first}.{slug_last}.{index}@{office['code']}.acmebank.example"
    full_name = f"{first} {last}"
    return username, email, full_name


def build_office(office, faker):
    company_code = office["code"]
    lei, lei_entity = build_office_lei_entity(office, faker)

    business_units = []
    portfolios = []
    books = []
    accounts = []

    person_index = 0

    def add_account(unit_code, role):
        nonlocal person_index
        username, email, full_name = gen_person(office, faker, person_index)
        person_index += 1
        accounts.append({
            "company_code": company_code,
            "id": new_uuid(f"account:{username}"),
            "username": username,
            "email": email,
            "full_name": full_name,
            "password_hash": FIXED_PASSWORD_HASH,
            "account_type": "user",
            "role": role,
            "business_unit_code": unit_code,
        })

    global_markets_code = f"{company_code}.global_markets"
    global_markets_id = new_uuid(f"business_unit:{global_markets_code}")
    business_units.append({
        "company_code": company_code,
        "id": global_markets_id,
        "unit_name": "Global Markets",
        "unit_code": global_markets_code,
        "parent_business_unit_code": None,
        "business_centre_code": office["business_centre_code"],
        "unit_type_code": "DIVISION",
    })

    global_virtual_portfolio_id = new_uuid(f"portfolio:{global_markets_code}")
    portfolios.append({
        "company_code": company_code,
        "id": global_virtual_portfolio_id,
        "name": f"{office['name']} Global Markets",
        "parent_portfolio_code": None,
        "portfolio_code": f"{company_code}.global_markets",
        "owner_unit_code": f"{company_code}.global_markets",
        "is_virtual": True,
    })

    for desk in DESKS:
        desk_code = f"{company_code}.{desk['code']}"
        business_units.append({
            "company_code": company_code,
            "id": new_uuid(f"business_unit:{desk_code}"),
            "unit_name": f"{desk['name']} {office['city']}",
            "unit_code": desk_code,
            "parent_business_unit_code": f"{company_code}.global_markets",
            "business_centre_code": office["business_centre_code"],
            "unit_type_code": "DESK",
        })

        physical_portfolio_code = f"{desk_code}.portfolio"
        portfolios.append({
            "company_code": company_code,
            "id": new_uuid(f"portfolio:{physical_portfolio_code}"),
            "name": f"{desk['name']} {office['city']} Portfolio",
            "parent_portfolio_code": f"{company_code}.global_markets",
            "portfolio_code": physical_portfolio_code,
            "owner_unit_code": desk_code,
            "is_virtual": False,
        })

        books.append({
            "company_code": company_code,
            "id": new_uuid(f"book:{physical_portfolio_code}"),
            "name": f"{desk['name']} {office['city']} Book",
            "portfolio_code": physical_portfolio_code,
            "owner_unit_code": desk_code,
            "regulatory_book_type": "Trading",
        })

        for _ in range(desk["head_count"]):
            add_account(desk_code, "Desk Head")
        for _ in range(desk["trader_count"]):
            add_account(desk_code, "Trader")

    for func in SUPPORT_FUNCTIONS:
        func_code = f"{company_code}.{func['code']}"
        business_units.append({
            "company_code": company_code,
            "id": new_uuid(f"business_unit:{func_code}"),
            "unit_name": f"{func['name']} {office['city']}",
            "unit_code": func_code,
            "parent_business_unit_code": None,
            "business_centre_code": office["business_centre_code"],
            "unit_type_code": "COST_CENTRE",
        })

        for _ in range(func["manager_count"]):
            add_account(func_code, "Manager")
        for _ in range(func["analyst_count"]):
            add_account(func_code, "Analyst")

    return lei, lei_entity, business_units, portfolios, books, accounts


def main():
    holding_lei, holding_entity = build_holding_lei_entity()

    lei_entities = [holding_entity]
    lei_relationships = []
    all_business_units = []
    all_portfolios = []
    all_books = []
    all_accounts = []

    companies = [{
        "code": HOLDING["code"],
        "name": HOLDING["name"],
        "lei": holding_lei,
        "parent_code": None,
        "is_holding": True,
    }]

    for office in OFFICES:
        if office["locale"]:
            faker = Faker(office["locale"])
            # Seeded per office (not globally) so adding a new office
            # doesn't change the names already generated for existing
            # ones -- regeneration stays additive.
            faker.seed_instance(f"acme_bank:{office['code']}")
        else:
            faker = None
        lei, entity, business_units, portfolios, books, accounts = build_office(office, faker)

        lei_entities.append(entity)
        lei_relationships.append(build_relationship(holding_lei, lei))
        all_business_units.extend(business_units)
        all_portfolios.extend(portfolios)
        all_books.extend(books)
        all_accounts.extend(accounts)

        companies.append({
            "code": office["code"],
            "name": office["name"],
            "lei": lei,
            "parent_code": HOLDING["code"],
            "is_holding": False,
            "business_centre_code": office["business_centre_code"],
        })

    def write(name, data):
        path = os.path.join(SCRIPT_DIR, name)
        with open(path, "w") as f:
            json.dump(data, f, indent=2)
            f.write("\n")
        print(f"Wrote {path} ({len(data)} records)")

    write("companies.json", companies)
    write("lei_entities.json", lei_entities)
    write("lei_relationships.json", lei_relationships)
    write("business_units.json", all_business_units)
    write("portfolios.json", all_portfolios)
    write("books.json", all_books)
    write("accounts.json", all_accounts)


if __name__ == "__main__":
    main()
