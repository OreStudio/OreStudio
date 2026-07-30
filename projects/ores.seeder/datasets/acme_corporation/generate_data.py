#!/usr/bin/env python3
"""
Generates the ACME Corporation source-data JSON dataset.

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
        "name": "ACME Corporation UK plc",
        "country": "GB",
        "business_centre_code": "GBLO",
        "locale": "en_GB",
        "city": "London",
        "email_domain": "acmecorp.co.uk",
        "street": "1 Poultry",
        "state": None,
        "postal_code": "EC2R 8EJ",
        "phone": "+44 20 7946 0958",
    },
    {
        "code": "acme_us",
        "name": "ACME Corporation US Inc",
        "country": "US",
        "business_centre_code": "USNY",
        "locale": "en_US",
        "city": "New York",
        "email_domain": "acmecorp.com",
        "street": "200 Park Avenue",
        "state": "NY",
        "postal_code": "10166",
        "phone": "+1 212 555 0148",
    },
    {
        "code": "acme_hk",
        "name": "ACME Corporation HK Ltd",
        "country": "HK",
        "business_centre_code": "HKHK",
        "locale": None,  # curated name pool, see HK_GIVEN_NAMES/HK_FAMILY_NAMES
        "city": "Hong Kong",
        "email_domain": "acmecorp.hk",
        "street": "1 Queen's Road Central",
        "state": None,
        "postal_code": "999077",
        "phone": "+852 2543 1188",
    },
]

# RBAC roles (ores_iam_roles_tbl has no desk-granularity roles like "Desk
# Head"/"Trader" -- Trading is the permission set that covers every desk
# job, Operations covers Middle Office/Market Risk management, Viewer
# gives their analysts read-only access). ores_iam_publish_accounts_from_dq_fn
# matches this value verbatim against ores_iam_roles_tbl.name -- these must
# stay real role names, kept separate from the free-text job_title that
# actually describes what the person does (see add_account below).
DESK_STAFF_ROLE = "Trading"
SUPPORT_MANAGER_ROLE = "Operations"
SUPPORT_ANALYST_ROLE = "Viewer"


def seniority_title(role_label, index, count):
    """Builds a job title like "Senior Trader"/"Junior Trader" for a cohort
    of `count` people doing the same job -- first is Senior, last is
    Junior, any in between are Mid-Level. A cohort of one gets no tier
    prefix at all (nothing to rank them against)."""
    if count <= 1:
        return role_label
    tier = "Senior" if index == 0 else "Junior" if index == count - 1 else "Mid-Level"
    return f"{tier} {role_label}"

DESKS = [
    {"code": "ir_swaps", "name": "IR Swaps", "head_count": 1, "trader_count": 2},
    {"code": "credit_trading", "name": "Credit Trading", "head_count": 1, "trader_count": 2},
    {"code": "fx_rates", "name": "FX Rates", "head_count": 1, "trader_count": 2},
]

# Currency/product leaf books per desk, per office -- deliberately tiered
# by office role rather than uniform: London is the head office, so it
# gets the full multi-currency tree (matching the depth of the generic
# "risk_management" fixture other tenants get); New York is a full
# regional desk with a couple of books each; Hong Kong is deliberately
# the shallowest (single book per desk) -- not every office needs to be
# equally deep for the dataset to be realistic, a smaller regional
# office with a leaner book structure is itself realistic. Each entry
# is (code_suffix, label); the leaf portfolio/book name is built from
# the label, parented under the desk's own virtual portfolio.
DESK_BOOKS_BY_OFFICE = {
    "acme_uk": {
        "ir_swaps": [
            ("gbp_rates", "GBP Rates"),
            ("eur_rates", "EUR Rates"),
            ("usd_rates", "USD Rates"),
        ],
        "credit_trading": [
            ("ig_credit", "IG Credit EMEA"),
            ("hy_credit", "HY Credit EMEA"),
        ],
        "fx_rates": [
            ("g10_fx", "G10 FX"),
            ("em_fx", "EM FX"),
        ],
    },
    "acme_us": {
        "ir_swaps": [
            ("usd_rates", "USD Rates"),
            ("cad_rates", "CAD Rates"),
        ],
        "credit_trading": [
            ("ig_credit_americas", "IG Credit Americas"),
        ],
        "fx_rates": [
            ("g10_fx_americas", "G10 FX Americas"),
        ],
    },
    "acme_hk": {
        "ir_swaps": [
            ("jpy_rates", "JPY Rates"),
        ],
        "credit_trading": [
            ("ig_credit_apac", "IG Credit APAC"),
        ],
        "fx_rates": [
            ("g10_fx_apac", "G10 FX APAC"),
        ],
    },
}

# reports_to_role selects which of build_office's three independent
# leadership lines (see its own segregation-of-duties comment) this
# function's own Head reports to -- "coo" for Operations, "risk" for
# Risk, keeping Middle Office and Market Risk out of the Trading line
# they oversee.
SUPPORT_FUNCTIONS = [
    {"code": "middle_office", "name": "Middle Office", "manager_count": 1, "analyst_count": 2,
     "reports_to_role": "coo"},
    {"code": "market_risk", "name": "Market Risk", "manager_count": 1, "analyst_count": 2,
     "reports_to_role": "risk"},
]

# London is deliberately built as an exact replica of the shared
# "risk_management"/testdata Barclays fixture (see
# projects/ores.sql/populate/testdata/testdata_{business_units,
# portfolios,books}_artefact_populate.sql) -- same business units, same
# portfolios, same books, verbatim -- rather than the flatter
# office-generic tree US/HK use. Only the 3 EMEA desks (the ones that
# already had staff before this replica) are staffed; the Americas/APAC
# business units this pulls in (mirroring Barclays' own tree, which
# spans regions within one party) are structural only, since Acme
# already has separate US/HK legal entities with their own staff --
# duplicating headcount under London for desks it doesn't actually run
# would be unrealistic, not more complete.
#
# Each tuple: (code, name, parent_code, unit_type, business_centre_code).
# parent_code is always an explicit code here -- "global_markets" (added
# generically by build_office below) or another row's own code -- except
# risk_management, which (like global_markets itself) is a genuine
# top-level DIVISION with no parent at all, exactly mirroring the
# fixture's own two-root-DIVISION shape.
BARCLAYS_UK_BUSINESS_UNITS = [
    ("emea_trading", "EMEA Trading", "global_markets", "BUSINESS_AREA", "GBLO"),
    ("rates_trading_emea", "Rates Trading EMEA", "emea_trading", "DESK", "GBLO"),
    ("credit_trading_emea", "Credit Trading EMEA", "emea_trading", "DESK", "GBLO"),
    ("fx_trading_emea", "FX Trading EMEA", "emea_trading", "DESK", "GBLO"),
    ("americas_trading", "Americas Trading", "global_markets", "BUSINESS_AREA", "USNY"),
    ("rates_trading_americas", "Rates Trading Americas", "americas_trading", "DESK", "USNY"),
    ("credit_trading_americas", "Credit Trading Americas", "americas_trading", "DESK", "USNY"),
    ("apac_trading", "APAC Trading", "global_markets", "BUSINESS_AREA", "JPTO"),
    ("rates_trading_apac", "Rates Trading APAC", "apac_trading", "DESK", "JPTO"),
    ("fx_trading_apac", "FX Trading APAC", "apac_trading", "DESK", "HKHK"),
    ("risk_management", "Risk Management", None, "DIVISION", "GBLO"),
    ("counterparty_risk", "Counterparty Risk", "risk_management", "BUSINESS_AREA", "GBLO"),
]

# Each tuple: (code, name, parent_code_or_None, owner_unit_code, currency, is_virtual).
# parent_code is always explicit -- "global_markets" (the root portfolio
# added generically by build_office below, reusing that business unit's
# code as its own) or another row's own code -- except Regulatory
# Capital, which (like Global Portfolio itself) is a genuine standalone
# root portfolio with no parent, matching the fixture exactly.
BARCLAYS_UK_PORTFOLIOS = [
    ("emea_portfolio", "EMEA Portfolio", "global_markets", "emea_trading", "EUR", True),
    ("rates_emea", "Rates EMEA", "emea_portfolio", "rates_trading_emea", "EUR", True),
    ("gbp_rates", "GBP Rates", "rates_emea", "rates_trading_emea", "GBP", False),
    ("eur_rates", "EUR Rates", "rates_emea", "rates_trading_emea", "EUR", False),
    ("credit_emea", "Credit EMEA", "emea_portfolio", "credit_trading_emea", "EUR", True),
    ("ig_credit_emea", "IG Credit EMEA", "credit_emea", "credit_trading_emea", "EUR", False),
    ("fx_emea", "FX EMEA", "emea_portfolio", "fx_trading_emea", "EUR", True),
    ("g10_fx_emea", "G10 FX EMEA", "fx_emea", "fx_trading_emea", "EUR", False),
    ("americas_portfolio", "Americas Portfolio", "global_markets", "americas_trading", "USD", True),
    ("rates_americas", "Rates Americas", "americas_portfolio", "rates_trading_americas", "USD", True),
    ("usd_rates", "USD Rates", "rates_americas", "rates_trading_americas", "USD", False),
    ("cad_rates", "CAD Rates", "rates_americas", "rates_trading_americas", "CAD", False),
    ("credit_americas", "Credit Americas", "americas_portfolio", "credit_trading_americas", "USD", True),
    ("ig_credit_americas", "IG Credit Americas", "credit_americas", "credit_trading_americas", "USD", False),
    ("apac_portfolio", "APAC Portfolio", "global_markets", "apac_trading", "JPY", True),
    ("rates_apac", "Rates APAC", "apac_portfolio", "rates_trading_apac", "JPY", True),
    ("jpy_rates", "JPY Rates", "rates_apac", "rates_trading_apac", "JPY", False),
    ("fx_apac", "FX APAC", "apac_portfolio", "fx_trading_apac", "JPY", False),
    ("regulatory_capital", "Regulatory Capital", None, "risk_management", "USD", False),
]

# Each tuple: (name, portfolio_code, currency, gl_account_ref, cost_center,
# book_status, regulatory_book_type, is_sweepable, rates_centre_code).
BARCLAYS_UK_BOOKS = [
    ("GBP Nostro Sweep", "gbp_rates", "GBP", "GL-TREAS-001", "CC-EMEA-TREASURY", "Active", "Trading", True, "GBLO"),
    ("GBP Vanilla Swaps", "gbp_rates", "GBP", "GL-RATES-001", "CC-EMEA-RATES", "Active", "Trading", False, "GBLO"),
    ("GBP Rates Options", "gbp_rates", "GBP", "GL-RATES-002", "CC-EMEA-RATES", "Active", "Trading", False, "GBLO"),
    ("EUR Vanilla Swaps", "eur_rates", "EUR", "GL-RATES-003", "CC-EMEA-RATES", "Active", "Trading", False, "FRPA"),
    ("EUR Exotic Rates", "eur_rates", "EUR", "GL-RATES-004", "CC-EMEA-RATES", "Active", "Trading", False, "FRPA"),
    ("IG CDS EMEA", "ig_credit_emea", "EUR", "GL-CREDIT-001", "CC-EMEA-CREDIT", "Active", "Trading", False, "GBLO"),
    ("IG Bonds EMEA", "ig_credit_emea", "EUR", "GL-CREDIT-002", "CC-EMEA-CREDIT", "Active", "Banking", False, "GBLO"),
    ("G10 FX Spot Forward", "g10_fx_emea", "EUR", "GL-FX-001", "CC-EMEA-FX", "Active", "Trading", False, "GBLO"),
    ("G10 FX Options", "g10_fx_emea", "EUR", "GL-FX-002", "CC-EMEA-FX", "Active", "Trading", False, "GBLO"),
    ("USD Cash Management Sweep", "usd_rates", "USD", "GL-TREAS-002", "CC-AMER-TREASURY", "Active", "Trading", True, "USNY"),
    ("USD Vanilla Swaps", "usd_rates", "USD", "GL-RATES-005", "CC-AMER-RATES", "Active", "Trading", False, "USNY"),
    ("USD Rates Options", "usd_rates", "USD", "GL-RATES-006", "CC-AMER-RATES", "Active", "Trading", False, "USNY"),
    ("CAD Swaps", "cad_rates", "CAD", "GL-RATES-007", "CC-AMER-RATES", "Active", "Trading", False, "CATO"),
    ("IG CDS Americas", "ig_credit_americas", "USD", "GL-CREDIT-003", "CC-AMER-CREDIT", "Active", "Trading", False, "USNY"),
    ("IG Bonds Americas", "ig_credit_americas", "USD", "GL-CREDIT-004", "CC-AMER-CREDIT", "Active", "Banking", False, "USNY"),
    ("JPY Vanilla Swaps", "jpy_rates", "JPY", "GL-RATES-008", "CC-APAC-RATES", "Active", "Trading", False, "JPTO"),
    ("JPY Rates Options", "jpy_rates", "JPY", "GL-RATES-009", "CC-APAC-RATES", "Active", "Trading", False, "JPTO"),
    ("APAC FX Spot Forward", "fx_apac", "JPY", "GL-FX-003", "CC-APAC-FX", "Active", "Trading", False, "SGSI"),
    ("APAC FX NDF", "fx_apac", "JPY", "GL-FX-004", "CC-APAC-FX", "Active", "Trading", False, "SGSI"),
    ("Regulatory Banking Book", "regulatory_capital", "USD", "GL-REG-001", "CC-RISK", "Active", "Banking", False, None),
    ("Regulatory Trading Book", "regulatory_capital", "USD", "GL-REG-002", "CC-RISK", "Active", "Trading", False, None),
    ("Regulatory CVA Book", "regulatory_capital", "USD", "GL-REG-003", "CC-RISK", "Closed", "Trading", False, None),
]

# Maps the 3 staffed Barclays-replica desks back onto the original head/
# trader counts every Acme desk uses, so London keeps the same headcount
# it had before -- just attached to the Barclays-named desk codes.
BARCLAYS_UK_STAFFED_DESKS = {
    "rates_trading_emea": next(d for d in DESKS if d["code"] == "ir_swaps"),
    "credit_trading_emea": next(d for d in DESKS if d["code"] == "credit_trading"),
    "fx_trading_emea": next(d for d in DESKS if d["code"] == "fx_rates"),
}

HOLDING = {
    "code": "acme_group",
    "name": "Acme Corporation Plc",
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


def gen_person(office, faker, index, used_slugs):
    # Alternate gender deterministically by index -- used both to pick a
    # gendered first name (Faker) and, in assign_photo_keys, a
    # gender-matched photo.
    gender = "female" if index % 2 == 0 else "male"
    if faker:
        first = faker.first_name_female() if gender == "female" else faker.first_name_male()
        last = faker.last_name()
    else:
        first, last = gen_hk_name(index)
    slug_first = first.lower().replace(" ", ".")
    slug_last = last.lower().replace(" ", ".")
    # Two people can legitimately draw the same Faker name within an
    # office; a plain "first.last" username/email is only unique per
    # (tenant_id, username/email) -- see accounts_email_uniq_idx in
    # iam_accounts_create.sql -- so a numeric suffix is added only on an
    # actual collision, keeping the common case realistic
    # ("kevin.barrett" rather than "kevin.barrett.acme_us.6").
    base_slug = f"{slug_first}.{slug_last}"
    slug = base_slug
    suffix = 1
    while slug in used_slugs:
        suffix += 1
        slug = f"{base_slug}{suffix}"
    used_slugs.add(slug)

    username = slug
    email = f"{slug}@{office['email_domain']}"
    full_name = f"{first} {last}"
    return username, email, full_name, gender


def build_office(office, faker):
    company_code = office["code"]
    lei, lei_entity = build_office_lei_entity(office, faker)

    business_units = []
    portfolios = []
    books = []
    accounts = []

    person_index = 0
    used_slugs = set()

    def add_account(unit_code, job_title, rbac_role, reports_to_username=None):
        nonlocal person_index
        username, email, full_name, gender = gen_person(office, faker, person_index, used_slugs)
        person_index += 1
        accounts.append({
            "company_code": company_code,
            "id": new_uuid(f"account:{username}"),
            "username": username,
            "email": email,
            "full_name": full_name,
            "gender": gender,
            "password_hash": FIXED_PASSWORD_HASH,
            "account_type": "user",
            "role": rbac_role,
            "job_title": job_title,
            "reports_to_username": reports_to_username,
            "business_unit_code": unit_code,
            # Every worker shares their office's own official address --
            # a real company puts its registered office address on staff
            # contact records, not a personal home address.
            "street": office["street"],
            "city": office["city"],
            "state": office["state"],
            "country_code": office["country"],
            "postal_code": office["postal_code"],
            "phone": office["phone"],
        })
        return username

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
        "name": "Global Portfolio" if company_code == "acme_uk" else f"{office['name']} Global Markets",
        "parent_portfolio_code": None,
        "portfolio_code": f"{company_code}.global_markets",
        "owner_unit_code": f"{company_code}.global_markets",
        "aggregation_ccy": "USD" if company_code == "acme_uk" else None,
        "is_virtual": True,
    })

    # Segregation of duties, mirrored the way a real legal entity actually
    # structures it: the CEO/Country Head sits at the top with three
    # independent lines reporting directly to them -- Trading (the
    # business), Operations (via the COO), and Risk (via the Head of
    # Risk) -- rather than Risk/Operations reporting into the business
    # line they oversee. Only the CEO is common to all three; Trading,
    # Operations, and Risk staff never report across lines.
    ceo_username = add_account(global_markets_code, "Country Head", SUPPORT_MANAGER_ROLE)
    coo_username = add_account(global_markets_code, "COO", SUPPORT_MANAGER_ROLE,
                               reports_to_username=ceo_username)
    head_of_risk_username = add_account(global_markets_code, "Head of Risk", SUPPORT_MANAGER_ROLE,
                                        reports_to_username=ceo_username)

    # Head of Trading sits above every desk in the office -- the business-
    # line head Desk Heads report to.
    head_of_trading_username = add_account(
        global_markets_code, "Head of Trading", DESK_STAFF_ROLE,
        reports_to_username=ceo_username)

    if company_code == "acme_uk":
        build_barclays_replica_org(office, business_units, portfolios, books, global_markets_code)
        for desk_code, desk in BARCLAYS_UK_STAFFED_DESKS.items():
            full_desk_code = f"{company_code}.{desk_code}"
            head_username = None
            for _ in range(desk["head_count"]):
                head_username = add_account(full_desk_code, "Head of Desk", DESK_STAFF_ROLE,
                                           reports_to_username=head_of_trading_username)
            for i in range(desk["trader_count"]):
                title = seniority_title("Trader", i, desk["trader_count"])
                add_account(full_desk_code, title, DESK_STAFF_ROLE,
                           reports_to_username=head_username)
    else:
        desk_books = DESK_BOOKS_BY_OFFICE[company_code]
        build_generic_desks_org(office, business_units, portfolios, books, add_account, desk_books,
                                head_of_trading_username)

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

        function_head_reports_to = (
            coo_username if func["reports_to_role"] == "coo" else head_of_risk_username)
        manager_username = None
        for _ in range(func["manager_count"]):
            manager_username = add_account(
                func_code, f"Head of {func['name']}", SUPPORT_MANAGER_ROLE,
                reports_to_username=function_head_reports_to)
        for i in range(func["analyst_count"]):
            title = seniority_title("Analyst", i, func["analyst_count"])
            add_account(func_code, title, SUPPORT_ANALYST_ROLE,
                       reports_to_username=manager_username)

    return lei, lei_entity, business_units, portfolios, books, accounts


def build_generic_desks_org(office, business_units, portfolios, books, add_account, desk_books,
                            head_of_trading_username):
    company_code = office["code"]
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

        # Desk-level virtual portfolio -- the aggregation node its
        # currency/product leaf books roll up into, mirroring the
        # Rates EMEA/Credit EMEA/FX EMEA layer in the generic
        # risk_management fixture rather than one flat book per desk.
        desk_portfolio_code = f"{desk_code}.portfolio"
        portfolios.append({
            "company_code": company_code,
            "id": new_uuid(f"portfolio:{desk_portfolio_code}"),
            "name": f"{desk['name']} {office['city']} Portfolio",
            "parent_portfolio_code": f"{company_code}.global_markets",
            "portfolio_code": desk_portfolio_code,
            "owner_unit_code": desk_code,
            "is_virtual": True,
        })

        for book_suffix, book_label in desk_books[desk["code"]]:
            leaf_portfolio_code = f"{desk_portfolio_code}.{book_suffix}"
            portfolios.append({
                "company_code": company_code,
                "id": new_uuid(f"portfolio:{leaf_portfolio_code}"),
                "name": f"{book_label} Portfolio",
                "parent_portfolio_code": desk_portfolio_code,
                "portfolio_code": leaf_portfolio_code,
                "owner_unit_code": desk_code,
                "is_virtual": False,
            })

            books.append({
                "company_code": company_code,
                "id": new_uuid(f"book:{leaf_portfolio_code}"),
                "name": f"{book_label} Book",
                "portfolio_code": leaf_portfolio_code,
                "owner_unit_code": desk_code,
                "regulatory_book_type": "Trading",
            })

        head_username = None
        for _ in range(desk["head_count"]):
            head_username = add_account(desk_code, "Head of Desk", DESK_STAFF_ROLE,
                                       reports_to_username=head_of_trading_username)
        for i in range(desk["trader_count"]):
            title = seniority_title("Trader", i, desk["trader_count"])
            add_account(desk_code, title, DESK_STAFF_ROLE, reports_to_username=head_username)


def build_barclays_replica_org(office, business_units, portfolios, books, global_markets_code):
    """Appends London's exact Barclays-fixture replica (business units,
    portfolios, books) onto the caller's lists. See BARCLAYS_UK_* above
    for the source of truth this mirrors."""
    company_code = office["code"]

    for code, name, parent_code, unit_type, business_centre_code in BARCLAYS_UK_BUSINESS_UNITS:
        # None means a genuine top-level unit (only risk_management, a
        # second root DIVISION alongside global_markets) -- every other
        # row names its parent explicitly, including "global_markets".
        parent = None if parent_code is None else f"{company_code}.{parent_code}"
        business_units.append({
            "company_code": company_code,
            "id": new_uuid(f"business_unit:{company_code}.{code}"),
            "unit_name": name,
            "unit_code": f"{company_code}.{code}",
            "parent_business_unit_code": parent,
            "business_centre_code": business_centre_code,
            "unit_type_code": unit_type,
        })

    for code, name, parent_code, owner_unit_code, ccy, is_virtual in BARCLAYS_UK_PORTFOLIOS:
        # None means a genuine standalone root (only regulatory_capital,
        # alongside Global Portfolio itself) -- every other row names its
        # parent explicitly, including "global_markets".
        parent = None if parent_code is None else f"{company_code}.{parent_code}"
        portfolios.append({
            "company_code": company_code,
            "id": new_uuid(f"portfolio:{company_code}.{code}"),
            "name": name,
            "parent_portfolio_code": parent,
            "portfolio_code": f"{company_code}.{code}",
            "owner_unit_code": f"{company_code}.{owner_unit_code}",
            "aggregation_ccy": ccy,
            "is_virtual": is_virtual,
        })

    for (name, portfolio_code, ccy, gl_ref, cost_center, status, reg_type, sweepable,
         rates_centre) in BARCLAYS_UK_BOOKS:
        full_portfolio_code = f"{company_code}.{portfolio_code}"
        books.append({
            "company_code": company_code,
            "id": new_uuid(f"book:{company_code}.{portfolio_code}.{name}"),
            "name": name,
            "portfolio_code": full_portfolio_code,
            "functional_currency": ccy,
            "gl_account_ref": gl_ref,
            "cost_center": cost_center,
            "book_status": status,
            "regulatory_book_type": reg_type,
            "is_sweepable": sweepable,
            "rates_centre_code": rates_centre,
        })


FACES_DIR = os.path.join(SCRIPT_DIR, "..", "..", "..", "..", "external", "facestudio", "faces")

# Each office's photo pool leans toward the ethnicities plausible for its
# generated names -- Hong Kong's curated Cantonese name pool gets
# east_asian faces, London/New York (Faker en_GB/en_US, Western-sounding
# names) get a broader/Western-leaning mix. Not a precision-matching
# claim, just avoiding an obvious mismatch (e.g. an east_asian face next
# to "Clifford Shaw").
OFFICE_FACE_ETHNICITIES = {
    "acme_uk": ["european", "south_asian", "african", "west_asian"],
    "acme_us": ["european", "african", "latin_american", "east_asian", "south_asian"],
    "acme_hk": ["east_asian", "southeast_asian"],
}


def load_face_files():
    """Returns {(ethnicity, gender): [filenames]}, each list sorted for
    determinism."""
    by_bucket = {}
    for f in sorted(os.listdir(FACES_DIR)):
        if not f.endswith(".jpeg"):
            continue
        gender, _age, ethnicity = f.split("_", 2)
        ethnicity = ethnicity.rsplit("_", 1)[0]  # strip the trailing _NN index
        by_bucket.setdefault((ethnicity, gender), []).append(f)
    return by_bucket


def assign_photo_keys(office, accounts, faces_by_bucket, bucket_indices):
    """Assigns each account a deterministic photo_key: gender-matched, and
    ethnicity-matched to the office's pool (cycling through that pool for
    variety). bucket_indices tracks the next unused index per (ethnicity,
    gender) bucket across the whole run, so offices sharing an ethnicity
    (e.g. UK and US both drawing "european") don't hand out duplicate
    photos before they have to."""
    ethnicities = OFFICE_FACE_ETHNICITIES[office["code"]]
    for i, a in enumerate(accounts):
        ethnicity = ethnicities[i % len(ethnicities)]
        bucket = (ethnicity, a["gender"])
        pool = faces_by_bucket[bucket]
        idx = bucket_indices.get(bucket, 0)
        face = pool[idx % len(pool)]
        bucket_indices[bucket] = idx + 1
        a["photo_key"] = f"acme_staff_photo:{face.rsplit('.', 1)[0]}"


def main():
    holding_lei, holding_entity = build_holding_lei_entity()
    faces_by_bucket = load_face_files()
    bucket_indices = {}

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

        assign_photo_keys(office, accounts, faces_by_bucket, bucket_indices)

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
