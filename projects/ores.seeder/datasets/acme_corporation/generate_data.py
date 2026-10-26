#!/usr/bin/env python3
"""
Generates the ACME Corporation source-data JSON dataset.

Produces flat-array JSON files -- one per DQ artefact family (LEI
entities, LEI relationships, business units, portfolios, books,
accounts) -- matching the shape of the corresponding
ores_dq_*_artefact_tbl columns, following the same convention as
projects/ores.sql/populate/iso/country_currency.json: a flat
array of records, field names chosen to map directly onto SQL columns,
consumed later by ores.codegen mustache templates (a separate task).

The org-chart shape (which desks, how many people) is fixed per
office in OFFICES below; the filler data (names, emails, addresses)
is generated -- Faker for locales it supports (GB, US), a curated
name pool for Hong Kong (Faker has no English-script HK locale).

Regeneration methodology (photo_assignments.json)
---------------------------------------------------
Every account's photo_key is pinned in photo_assignments.json (username
-> photo_key), not recomputed from scratch on every run. Face images
in external/facestudio/faces/ are added to over time in unrelated
commits; assigning photos by position in the sorted file listing (the
original approach) meant a *new* face file landing earlier in sort
order than an existing one silently reshuffled every account's photo
downstream of it -- output was reproducible for one snapshot of the
faces directory, but not stable as that directory grew.

The fix: photo_assignments.json is the durable source of truth for
"who has which photo".
- An account whose username is already a key in that file keeps
  exactly that photo_key, forever, regardless of what's added to
  faces/ later.
- Only a genuinely new username (one never seen before) gets a fresh
  assignment -- deterministically, the lowest-index file in its
  (ethnicity, gender) bucket not already claimed by any pinned or
  newly-assigned account this run.
- main() writes the merged (pinned + newly-assigned) map back to
  photo_assignments.json before exiting, so the next run's "already
  pinned" set includes this run's new accounts too.

To regenerate: run `python3 generate_data.py`, then `python3
generate_sql.py`, and commit accounts.json, photo_assignments.json,
and the regenerated projects/ores.sql/populate/acme/*.sql together.
Never hand-edit accounts.json or photo_assignments.json directly --
add a new account by extending OFFICES/build_office (or, for
group-level staff, build_group_accounts), then regenerate.

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

# The account artefact carries no password material. An account's password is
# set after it is published, by the provisioning step that follows load_staff,
# which calls the server's own password API so the hasher, its parameters and
# its salt stay the server's business. The column below is required by the
# artefact table but is deliberately empty: the publish function stores no
# password hash for an account that stages none.
NO_STAGED_PASSWORD = ""

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
# The two functions every office kept no staff for. Both role names are the
# seed's own, match verbatim against ores_iam_roles_tbl.name, and each has a
# persona the demonstration exercises: the salesperson reads what the desk
# may show a client, and the support engineer reads the tenant's
# administrative state without changing it.
SALES_ROLE = "Sales"
CLIENT_SUPPORT_ROLE = "Support"


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

# The portfolio tree of each trading entity. Every entity has one global root
# with two top branches. The trading branch holds the desks, and the banking
# branch is owned by Treasury. The trading book and the banking book are
# separated by a regulatory line, so the two owners differ (see the books and
# portfolios page, "The trading branch and the banking branch").
#
# A leaf is (code_suffix, label, currency, rates_centre, [book names]). The
# FX leaves follow market convention: Cable and Sterling, the G10 majors, the
# Scandies and the emerging markets of the region. A book's currency is its
# leaf's. A book name that contains "Sweep" is the sweep target.
#
# London alone holds option books and the XVA desk (design rules D8 and D13).
OFFICE_TREE = {
    "acme_uk": {
        "currency": "GBP",
        "desks": {
            "ir_swaps": [
                ("gbp_rates", "GBP Rates", "GBP", "GBLO", ["GBP Vanilla Swaps", "GBP Rates Options"]),
                ("eur_rates", "EUR Rates", "EUR", "FRPA", ["EUR Vanilla Swaps", "EUR Exotic Rates"]),
                ("usd_rates", "USD Rates", "USD", "USNY", ["USD Vanilla Swaps", "USD Rates Options"]),
                ("chf_rates", "CHF Rates", "CHF", "CHZU", ["CHF Vanilla Swaps"]),
            ],
            "credit_trading": [
                ("ig_credit_emea", "IG Credit EMEA", "EUR", "GBLO", ["IG CDS EMEA"]),
                ("hy_credit_emea", "HY Credit EMEA", "EUR", "GBLO", ["HY CDS EMEA"]),
            ],
            "fx_rates": [
                ("cable", "Cable and Sterling", "GBP", "GBLO", ["Cable Spot Forward", "Cable Options"]),
                ("g10_majors", "G10 Majors", "USD", "GBLO", ["G10 Majors Spot Forward", "G10 Majors Options"]),
                ("scandies", "Scandies", "EUR", "GBLO", ["Scandies Spot Forward"]),
                ("ceemea_fx", "CEEMEA FX", "USD", "GBLO", ["CEEMEA Spot Forward", "CEEMEA NDF"]),
            ],
        },
        "xva": [("cva_hedges", "CVA Hedges", "GBP", "GBLO", ["CVA Hedge Book"])],
        "banking": [
            ("liquidity_buffer", "Liquidity Buffer", "GBP", "GBLO", ["Liquidity Buffer Bonds"]),
            ("cash_management_gbp", "GBP Cash Management", "GBP", "GBLO", ["GBP Nostro Sweep"]),
            ("cash_management_usd", "USD Cash Management", "USD", "USNY",
             ["USD Cash Management Sweep"]),
        ],
    },
    "acme_us": {
        "currency": "USD",
        "desks": {
            "ir_swaps": [
                ("usd_rates", "USD Rates", "USD", "USNY", ["USD Vanilla Swaps"]),
                ("cad_rates", "CAD Rates", "CAD", "CATO", ["CAD Swaps"]),
            ],
            "credit_trading": [
                ("ig_credit_americas", "IG Credit Americas", "USD", "USNY", ["IG CDS Americas"]),
            ],
            "fx_rates": [
                ("g10_americas", "G10 FX Americas", "USD", "USNY", ["G10 Americas Spot Forward"]),
                ("latam_fx", "LatAm FX", "USD", "USNY", ["LatAm Spot Forward", "LatAm NDF"]),
            ],
        },
        "xva": [],
        "banking": [
            ("liquidity_buffer", "Liquidity Buffer", "USD", "USNY", ["Liquidity Buffer Bonds"]),
            ("cash_management_usd", "USD Cash Management", "USD", "USNY",
             ["USD Cash Management Sweep"]),
        ],
    },
    "acme_hk": {
        "currency": "HKD",
        "desks": {
            "ir_swaps": [
                ("jpy_rates", "JPY Rates", "JPY", "JPTO", ["JPY Vanilla Swaps"]),
                ("hkd_rates", "HKD Rates", "HKD", "HKHK", ["HKD Vanilla Swaps"]),
            ],
            "credit_trading": [
                ("ig_credit_apac", "IG Credit APAC", "USD", "HKHK", ["IG CDS APAC"]),
            ],
            "fx_rates": [
                ("g10_apac", "G10 FX APAC", "USD", "HKHK", ["G10 APAC Spot Forward"]),
                ("asia_em_fx", "Asia EM FX", "USD", "HKHK", ["Asia EM Spot Forward", "Asia EM NDF"]),
            ],
        },
        "xva": [],
        "banking": [
            ("liquidity_buffer", "Liquidity Buffer", "HKD", "HKHK", ["Liquidity Buffer Bonds"]),
            ("cash_management_hkd", "HKD Cash Management", "HKD", "HKHK",
             ["HKD Cash Management Sweep"]),
        ],
    },
}

# The books that are sweep targets. A sweep is cash management, so it sits in the
# banking branch and is the target the sweeps of other books flow into.
SWEEP_BOOKS = {"GBP Nostro Sweep", "USD Cash Management Sweep", "HKD Cash Management Sweep"}

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

HOLDING = {
    "code": "acme_group",
    "name": "Acme Corporation Plc",
    "country": "GB",
    "city": "London",
    "street": "25 Old Broad Street",
    "postal_code": "EC2N 1HQ",
    "phone": "+44 20 7946 0000",
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


def build_group_accounts():
    """The holding company's own staff: the Group CEO and the Group Treasurer, in
    that order. No desks or business units of its own beyond Group Treasury (see
    build_group_treasury() -- trading desks still live per-office), and the
    Treasurer sits in that unit. Every office's Country Head reports to the CEO,
    so the org chart is one real tree rather than three disconnected per-office
    roots, and the Treasurer reports to the CEO.

    The Treasurer carries the Trading role because the group carries trades of
    its own -- intercompany loans and hedges in its treasury books -- and the
    person who enters them must be somebody of the group who may book: the
    population the demonstration generates states its author, and the
    authorisation requests it raises belong to that author. The CEO keeps the
    Operations role and books nothing.

    Fixed (not Faker-generated): two people don't need per-office locale/name
    variety, and staying deterministic keeps it stable across regenerations.
    The CEO must stay first, because main() names it from the first entry."""
    username = "adrian.vance"
    treasurer_username = "nadia.fischer"
    return [{
        "company_code": HOLDING["code"],
        "id": new_uuid(f"account:{username}"),
        "username": username,
        "email": f"{username}@acmecorp.com",
        "full_name": "Adrian Vance",
        "gender": "male",
        "password_hash": NO_STAGED_PASSWORD,
        "account_type": "user",
        "role": "Operations",
        "job_title": "Group Chief Executive Officer",
        "reports_to_username": None,
        "business_unit_code": None,
        "street": HOLDING["street"],
        "city": HOLDING["city"],
        "state": None,
        "country_code": HOLDING["country"],
        "postal_code": HOLDING["postal_code"],
        "phone": HOLDING["phone"],
    }, {
        "company_code": HOLDING["code"],
        "id": new_uuid(f"account:{treasurer_username}"),
        "username": treasurer_username,
        "email": f"{treasurer_username}@acmecorp.com",
        "full_name": "Nadia Fischer",
        "gender": "female",
        "password_hash": NO_STAGED_PASSWORD,
        "account_type": "user",
        "role": "Trading",
        "job_title": "Group Treasurer",
        "reports_to_username": username,
        "business_unit_code": f"{HOLDING['code']}.treasury",
        "street": HOLDING["street"],
        "city": HOLDING["city"],
        "state": None,
        "country_code": HOLDING["country"],
        "postal_code": HOLDING["postal_code"],
        "phone": HOLDING["phone"],
    }]


def build_group_treasury():
    """The holding company's own treasury book/portfolio/business unit --
    intercompany loans and FX hedges the parent itself carries, distinct
    from the per-office trading desks. One DIVISION-type unit (not a
    deeper DESK/BUSINESS_AREA hierarchy -- the holding company doesn't
    need more than one treasury desk yet), one virtual top-level
    portfolio, two Banking-book books (financing and hedges of
    non-trading exposure, not held-for-trading positions). All GBP,
    matching the group's own London HQ/GBLO business centre. See
    doc/agile/versions/v0/sprint_25/holding-company-treasury-functionality/
    task_define-holding-company-treasury-data-model.org for the full
    design rationale."""
    unit_code = f"{HOLDING['code']}.treasury"
    unit_id = new_uuid(f"business_unit:{unit_code}")
    business_units = [{
        "company_code": HOLDING["code"],
        "id": unit_id,
        "unit_name": "Group Treasury",
        "unit_code": unit_code,
        "parent_business_unit_code": None,
        "business_centre_code": "GBLO",
        "unit_type_code": "DIVISION",
    }]

    portfolio_code = f"{HOLDING['code']}.treasury"
    portfolio_id = new_uuid(f"portfolio:{portfolio_code}")
    portfolios = [{
        "company_code": HOLDING["code"],
        "id": portfolio_id,
        "name": "Group Treasury Portfolio",
        "parent_portfolio_code": None,
        "portfolio_code": portfolio_code,
        "owner_unit_code": unit_code,
        "aggregation_ccy": "GBP",
        "is_virtual": True,
    }]

    def book(name, gl_suffix):
        return {
            "company_code": HOLDING["code"],
            "id": new_uuid(f"book:{HOLDING['code']}.{gl_suffix}"),
            "name": name,
            "portfolio_code": portfolio_code,
            "functional_currency": "GBP",
            "gl_account_ref": f"GL-TREAS-{gl_suffix}",
            "cost_center": "CC-GROUP-TREASURY",
            "book_status": "Active",
            "regulatory_book_type": "Banking",
            "is_sweepable": False,
            "rates_centre_code": "GBLO",
        }

    books = [
        book("Group Intercompany Loans", "001"),
        book("Group FX Hedges", "002"),
    ]

    return business_units, portfolios, books


def build_office(office, faker, group_ceo_username=None):
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
            "password_hash": NO_STAGED_PASSWORD,
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

    tree = OFFICE_TREE[company_code]
    functional_ccy = tree["currency"]

    # One global root, named the same in every entity so a sandbox anchors under
    # it. Its two branches are the trading book and the banking book.
    portfolios.append({
        "company_code": company_code,
        "id": new_uuid(f"portfolio:{global_markets_code}"),
        "name": "Global Portfolio",
        "parent_portfolio_code": None,
        "portfolio_code": global_markets_code,
        "owner_unit_code": global_markets_code,
        "aggregation_ccy": functional_ccy,
        "is_virtual": True,
    })

    # Segregation of duties, mirrored the way a real legal entity actually
    # structures it: the CEO/Country Head sits at the top with three
    # independent lines reporting directly to them -- Trading (the
    # business), Operations (via the COO), and Risk (via the Head of
    # Risk) -- rather than Risk/Operations reporting into the business
    # line they oversee. Only the CEO is common to all three; Trading,
    # Operations, and Risk staff never report across lines.
    ceo_username = add_account(global_markets_code, "Country Head", SUPPORT_MANAGER_ROLE,
                               reports_to_username=group_ceo_username)
    coo_username = add_account(global_markets_code, "COO", SUPPORT_MANAGER_ROLE,
                               reports_to_username=ceo_username)
    head_of_risk_username = add_account(global_markets_code, "Head of Risk", SUPPORT_MANAGER_ROLE,
                                        reports_to_username=ceo_username)

    # Head of Trading sits above every desk in the office -- the business-
    # line head Desk Heads report to.
    head_of_trading_username = add_account(
        global_markets_code, "Head of Trading", DESK_STAFF_ROLE,
        reports_to_username=ceo_username)

    build_tree_org(office, business_units, portfolios, books, add_account,
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

    # The two seeded roles the offices carried nobody for: =Sales= and
    # =Support=. A demonstration of either persona had no account to sign in
    # as, so the screens those roles reach were exercised by nobody. Both are
    # global-markets functions and report to the Country Head, like the other
    # function heads. They are added last so the people above keep the names,
    # the usernames and the photos the earlier demonstrations know.
    add_account(global_markets_code, "Head of Sales", SALES_ROLE,
                reports_to_username=ceo_username)
    add_account(global_markets_code, "Head of Client Support", CLIENT_SUPPORT_ROLE,
                reports_to_username=ceo_username)

    return lei, lei_entity, business_units, portfolios, books, accounts


def build_tree_org(office, business_units, portfolios, books, add_account,
                   head_of_trading_username):
    """The business units, portfolios and books of one trading entity: the
    staffed desks with their leaf portfolios under the trading branch, an XVA
    desk where the entity has one, and Treasury with the banking branch."""
    company_code = office["code"]
    tree = OFFICE_TREE[company_code]
    functional_ccy = tree["currency"]
    root_code = f"{company_code}.global_markets"
    gl_counters = {}

    def book(name, leaf_code, ccy, rates_centre, area, regulatory_type, purpose="Trading"):
        gl_counters[area] = gl_counters.get(area, 0) + 1
        record = {
            "company_code": company_code,
            "id": new_uuid(f"book:{leaf_code}.{name}"),
            "name": name,
            "portfolio_code": leaf_code,
            "functional_currency": ccy,
            "gl_account_ref": f"GL-{area}-{gl_counters[area]:03d}",
            "cost_center": f"CC-{company_code.removeprefix('acme_').upper()}-{area}",
            "book_status": "Active",
            "regulatory_book_type": regulatory_type,
            "is_sweepable": name in SWEEP_BOOKS,
            "rates_centre_code": rates_centre,
            "book_purpose_type": purpose,
        }
        books.append(record)

    def portfolio(code, name, parent_code, owner_code, ccy, is_virtual):
        portfolios.append({
            "company_code": company_code,
            "id": new_uuid(f"portfolio:{code}"),
            "name": name,
            "parent_portfolio_code": parent_code,
            "portfolio_code": code,
            "owner_unit_code": owner_code,
            "aggregation_ccy": ccy,
            "is_virtual": is_virtual,
        })

    def unit(code, name, parent_code, unit_type):
        business_units.append({
            "company_code": company_code,
            "id": new_uuid(f"business_unit:{code}"),
            "unit_name": name,
            "unit_code": code,
            "parent_business_unit_code": parent_code,
            "business_centre_code": office["business_centre_code"],
            "unit_type_code": unit_type,
        })

    trading_code = f"{root_code}.trading_book"
    portfolio(trading_code, "Trading Book", root_code, root_code, functional_ccy, True)

    for desk in DESKS:
        desk_code = f"{company_code}.{desk['code']}"
        unit(desk_code, f"{desk['name']} {office['city']}", root_code, "DESK")

        desk_portfolio_code = f"{desk_code}.portfolio"
        portfolio(desk_portfolio_code, f"{desk['name']} {office['city']} Portfolio",
                  trading_code, desk_code, functional_ccy, True)

        area = {"ir_swaps": "RATES", "credit_trading": "CREDIT", "fx_rates": "FX"}[desk["code"]]
        for suffix, label, ccy, rates_centre, names in tree["desks"][desk["code"]]:
            leaf_code = f"{desk_portfolio_code}.{suffix}"
            portfolio(leaf_code, f"{label} Portfolio", desk_portfolio_code, desk_code, ccy, False)
            for name in names:
                book(name, leaf_code, ccy, rates_centre, area, "Trading")

        head_username = None
        for _ in range(desk["head_count"]):
            head_username = add_account(desk_code, "Head of Desk", DESK_STAFF_ROLE,
                                       reports_to_username=head_of_trading_username)
        for i in range(desk["trader_count"]):
            title = seniority_title("Trader", i, desk["trader_count"])
            add_account(desk_code, title, DESK_STAFF_ROLE, reports_to_username=head_username)

    # The XVA desk hedges the CVA risk of the entity. It has no staff here: its
    # structure is what the trading branch needs.
    if tree["xva"]:
        xva_code = f"{company_code}.xva"
        unit(xva_code, f"XVA {office['city']}", root_code, "DESK")
        xva_portfolio_code = f"{xva_code}.portfolio"
        portfolio(xva_portfolio_code, f"XVA {office['city']} Portfolio", trading_code, xva_code,
                  functional_ccy, True)
        for suffix, label, ccy, rates_centre, names in tree["xva"]:
            leaf_code = f"{xva_portfolio_code}.{suffix}"
            portfolio(leaf_code, f"{label} Portfolio", xva_portfolio_code, xva_code, ccy, False)
            for name in names:
                book(name, leaf_code, ccy, rates_centre, "XVA", "Trading")

    # Treasury owns the banking book. Like the XVA desk it has no staff here.
    treasury_code = f"{company_code}.treasury"
    unit(treasury_code, f"Treasury {office['city']}", None, "DIVISION")
    banking_code = f"{root_code}.banking_book"
    portfolio(banking_code, "Banking Book", root_code, treasury_code, functional_ccy, True)
    for suffix, label, ccy, rates_centre, names in tree["banking"]:
        leaf_code = f"{banking_code}.{suffix}"
        portfolio(leaf_code, f"{label} Portfolio", banking_code, treasury_code, ccy, False)
        for name in names:
            purpose = "SweepTarget" if name in SWEEP_BOOKS else "Funding"
            book(name, leaf_code, ccy, rates_centre, "TREAS", "Banking", purpose)


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
    "acme_group": ["european"],
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


def load_photo_assignments():
    """Loads the pinned username -> photo_key map (see module docstring
    for why assignments are pinned rather than recomputed). Missing file
    (first-ever run) is treated as an empty pin set, not an error."""
    path = os.path.join(SCRIPT_DIR, "photo_assignments.json")
    if not os.path.exists(path):
        return {}
    with open(path) as f:
        return json.load(f)


def save_photo_assignments(assignments):
    path = os.path.join(SCRIPT_DIR, "photo_assignments.json")
    with open(path, "w") as f:
        json.dump(assignments, f, indent=2, sort_keys=True)
        f.write("\n")
    print(f"Wrote {path} ({len(assignments)} pinned)")


def assign_photo_keys(office, accounts, faces_by_bucket, photo_assignments, used_photo_keys):
    """Assigns each account a photo_key: an already-pinned username (see
    load_photo_assignments) always keeps its existing photo_key
    unchanged, however faces/ or this dataset has grown since it was
    pinned. Only a genuinely new username gets a fresh assignment --
    gender-matched, ethnicity-matched to the office's pool, deterministically
    the lowest-index file in that (ethnicity, gender) bucket not already
    claimed by used_photo_keys (which starts pre-seeded with every pinned
    photo_key, so new assignments never collide with existing staff)."""
    ethnicities = OFFICE_FACE_ETHNICITIES[office["code"]]
    for i, a in enumerate(accounts):
        pinned = photo_assignments.get(a["username"])
        if pinned:
            a["photo_key"] = pinned
            used_photo_keys.add(pinned)
            continue

        ethnicity = ethnicities[i % len(ethnicities)]
        bucket = (ethnicity, a["gender"])
        pool = faces_by_bucket[bucket]
        photo_key = next(
            (f"acme_staff_photo:{face.rsplit('.', 1)[0]}" for face in pool
             if f"acme_staff_photo:{face.rsplit('.', 1)[0]}" not in used_photo_keys),
            None)
        if photo_key is None:
            raise RuntimeError(
                f"No unclaimed photo left in bucket {bucket} for new account "
                f"{a['username']!r} -- add more face images to faces/.")

        a["photo_key"] = photo_key
        used_photo_keys.add(photo_key)
        photo_assignments[a["username"]] = photo_key


def main():
    holding_lei, holding_entity = build_holding_lei_entity()
    faces_by_bucket = load_face_files()
    photo_assignments = load_photo_assignments()
    used_photo_keys = set(photo_assignments.values())

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

    group_accounts = build_group_accounts()
    assign_photo_keys({"code": HOLDING["code"]}, group_accounts, faces_by_bucket,
                      photo_assignments, used_photo_keys)
    all_accounts.extend(group_accounts)
    group_ceo_username = group_accounts[0]["username"]

    treasury_units, treasury_portfolios, treasury_books = build_group_treasury()
    all_business_units.extend(treasury_units)
    all_portfolios.extend(treasury_portfolios)
    all_books.extend(treasury_books)

    for office in OFFICES:
        if office["locale"]:
            faker = Faker(office["locale"])
            # Seeded per office (not globally) so adding a new office
            # doesn't change the names already generated for existing
            # ones -- regeneration stays additive.
            faker.seed_instance(f"acme_bank:{office['code']}")
        else:
            faker = None
        lei, entity, business_units, portfolios, books, accounts = build_office(
            office, faker, group_ceo_username=group_ceo_username)

        lei_entities.append(entity)
        lei_relationships.append(build_relationship(holding_lei, lei))
        all_business_units.extend(business_units)
        all_portfolios.extend(portfolios)
        all_books.extend(books)
        all_accounts.extend(accounts)

        assign_photo_keys(office, accounts, faces_by_bucket, photo_assignments, used_photo_keys)

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
    save_photo_assignments(photo_assignments)


if __name__ == "__main__":
    main()
