#!/usr/bin/env python3
"""
Generates populate SQL for the Smokehouse dataset.

Smokehouse is the smallest tenant that exercises the whole publishing lifecycle:
one legal entity, one business unit, one portfolio and three books, each book
approved for exactly one product. The profile that provisions it subscribes to
one market data provider, bootstraps the curves the system blueprints state,
builds the forward sets, calibrates the surfaces, and books one trade per book,
so a gate check can watch every kind of derived market object publish, supersede
and announce without paying for a demonstration tenant.

The legal entity is fictional, so its LEI is synthetic: the 9695 test prefix
ACME uses, with ISO 17442 mod-97 check digits computed here. The party's
counterparties are LEIs of the GLEIF set every deployment publishes, read from
the Acme dataset so the two cannot drift apart.

The books state the product each one may carry. Publishing derives the approval
matrix from those rows, up the chain to the party, which is what the trade
planner reads before it books anything.

Usage:
    python3 generate_sql.py
"""

import json
import os
import re
import uuid

NAMESPACE = uuid.UUID("6ba7b810-9dad-11d1-80b4-00c04fd430c8")

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, "..", "..", "..", ".."))
OUTPUT_DIR = os.path.join(REPO_ROOT, "projects", "ores.sql", "populate", "smokehouse")
ACME_JSON_DIR = os.path.join(REPO_ROOT, "projects", "ores.seeder", "datasets",
                             "acme_corporation")

CATALOG_NAME = "Smokehouse"
DATASET_SOURCE_SYSTEM = "SMOKEHOUSE"
DATASET_BUSINESS_CONTEXT = "Smokehouse generated data"
LEGAL_NAME = "Smokehouse Trading Ltd"
LEI_ENTITY_CODE = "SMOKEHOUSE"

# The market the tenant prices on: one pair and the two currencies that name it.
# The pair is the one the tenant's curves leave it, because a desk books only a
# pair whose two currencies it can price.
# The report documents are cut to these, because a document that names every
# currency the installation knows makes the engine read a market the tenant never
# publishes, and the run fails on the first missing quote.
MARKET_CURRENCIES = ("EUR", "GBP", "USD")

# The currencies whose discount curve the market configuration names. The United
# States dollar is in the market and not among these: the tenant's pair is a
# cross, and ORE triangulates it through the dollar, so the market needs the two
# dollar legs and no dollar curve of its own.
CURVE_CURRENCIES = ("EUR", "GBP", "USD")
MARKET_PAIRS = ("EURGBP",)

# The office whose configuration documents are the template. Each office's
# documents carry the same content, so one is as good as another.
TEMPLATE_COMPANY = "acme_uk"

# What the documents call the template company, rewritten to this one. Applied to
# every string of every template row, so a document and the report binding that
# names it move together.
RENAMES = [
    (re.compile(r"ACME Corporation UK plc"), LEGAL_NAME),
    (re.compile(r"ACME Corporation UK"), LEGAL_NAME),
    (re.compile(r"ACME Corporation"), CATALOG_NAME),
    (re.compile(r"ACME"), CATALOG_NAME),
    (re.compile(r"\bUK\b"), CATALOG_NAME),
    (re.compile(r"\bAcme\b"), CATALOG_NAME),
]

# The configuration documents a risk report reads, in the shape ACME's own
# generator stages them: a family, its title, and the staging tables.
CONFIGURATION_DOCUMENTS = [
    ("pricing_engines", "Pricing Engines",
     ["pricing_model_configs", "pricing_model_products", "pricing_model_product_parameters"]),
    ("todays_market", "Today's Market",
     ["todays_market_configs", "todays_market_collections", "todays_market_entries",
      "todays_market_configurations", "todays_market_configuration_bindings"]),
    ("curve_configuration", "Curve Configuration",
     ["curve_configurations", "curve_definitions", "yield_curve_configs", "curve_segments",
      "curve_segment_conventions", "curve_quotes", "fx_volatility_configs",
      "fx_volatility_smiles"]),
    ("risk_report_configs", "Risk Report Configurations",
     ["risk_report_configs", "configuration_analytics", "configuration_analytic_parameters",
      "configuration_market_bindings"]),
    ("engine_settings", "Engine Settings",
     ["engine_settings", "engine_settings_fixings", "engine_settings_building",
      "engine_settings_execution"]),
]

# The number of counterparties the party may trade with. One trade per book
# draws from this set; a handful is enough to exercise the counterparty read and
# small enough to keep the dataset readable.
COUNTERPARTY_LINK_COUNT = 4

OFFICE = {
    "street": "1 Smokehouse Yard",
    "city": "London",
    "postal": "EC2V 5AA",
}

LICENSE_HEADER = """/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
"""

GENERATED_NOTE = """
-- =============================================================================
-- AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
-- Generated by projects/ores.seeder/datasets/smokehouse/generate_sql.py.
-- To change a row, update the generator or the file it reads and regenerate.
-- =============================================================================
"""


def lei_check_digits(base18):
    """ISO 17442 (mod-97) check digits for an 18-character LEI prefix."""
    digits = "".join(ch if ch.isdigit() else str(ord(ch.upper()) - ord("A") + 10)
                     for ch in base18)
    return f"{98 - int(digits + '00') % 97:02d}"


def make_lei(entity_code):
    """A checksum-valid synthetic LEI on the 9695 test prefix."""
    base = ("9695" + entity_code.upper().replace("_", "")).ljust(18, "0")[:18]
    return base + lei_check_digits(base)


LEI = make_lei(LEI_ENTITY_CODE)


def uuid5(key):
    return str(uuid.uuid5(NAMESPACE, "smokehouse:" + key))


def load_acme(name):
    with open(os.path.join(ACME_JSON_DIR, f"{name}.json")) as f:
        return json.load(f)


def sql_str(v):
    if v is None:
        return "null"
    return "'" + str(v).replace("'", "''") + "'"


def sql_bool(v):
    return "true" if v else "false"


def counterparty_leis():
    """The GLEIF counterparties the party may trade with, by LEI."""
    rows = load_acme("party_counterparties")
    gleif = [r["counterparty_lei"] for r in rows
             if r["company_code"] == "acme_uk"
             and not r["counterparty_lei"].startswith("9695")]
    return sorted(set(gleif))[:COUNTERPARTY_LINK_COUNT]


def load_acme(name):
    with open(os.path.join(ACME_JSON_DIR, f"{name}.json")) as f:
        return json.load(f)


def sql_num(v):
    return "null" if v is None else repr(float(v))


def sql_value(value):
    if value is None:
        return "null"
    if isinstance(value, bool):
        return sql_bool(value)
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        return sql_num(value)
    if isinstance(value, list):
        return "array[" + ", ".join(sql_num(v) for v in value) + "]::numeric(5,4)[]"
    return sql_str(value)


def rename_text(value):
    """Rewrites the one company a document names, and nothing else."""
    if not isinstance(value, str):
        return value
    for pattern, replacement in RENAMES:
        value = pattern.sub(replacement, value)
    return value


def rename_record(record):
    out = {}
    for key, value in record.items():
        if key == "company_code":
            continue
        out[key] = rename_text(value)
    return out


def template_rows(name):
    """The template office's rows of one JSON file, renamed to this company."""
    return [rename_record(r) for r in load_acme(name) if r["company_code"] == TEMPLATE_COMPANY]


def market_currency(target: str) -> bool:
    """Whether a market data target names only the currencies this tenant prices."""
    currencies = set(MARKET_CURRENCIES)
    parts = target.split("/")
    if parts[0] == "Yield":
        return len(parts) > 1 and parts[1] in currencies
    # FX/GBP/USD and FXVolatility/GBP/USD/GBPUSD both name the pair in the two
    # parts after the family.
    return len(parts) > 2 and parts[1] in currencies and parts[2] in currencies


def priced_by_the_report(rows, keep):
    """The rows of one staging table that the kept rows still read.

    The documents are one graph of ids, so cutting one table leaves the others
    naming rows nobody reads, and the publish would stage a document larger than
    the market it describes. Each family states its own rule below; this is the
    common shape.
    """
    kept = {r["id"] for r in keep}
    return [r for r in rows if r.get("id") in kept]


def write(filename, content):
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    path = os.path.join(OUTPUT_DIR, filename)
    with open(path, "w") as f:
        f.write(content)
    print(f"Wrote {path}")


# =============================================================================
# The company's own shape
# =============================================================================

BUSINESS_UNITS = [
    dict(code="smokehouse.trading", name="Trading", parent=None, centre="GBLO",
         type_="DESK"),
]

PORTFOLIOS = [
    dict(code="smokehouse.global", name="Global Portfolio", parent=None,
         owner="smokehouse.trading", ccy="GBP", virtual=False),
]

# One book per product the smokehouse books, because the approval is the book's:
# a book that permits the forward refuses the option, so the three trades need
# three books. The product each book carries is stated in the books artefact
# below and published into the approval matrix.
BOOKS = [
    dict(name="GBP FX Book", portfolio="smokehouse.global", ccy="GBP",
         gl="GL-SMK-FWD", cost="CC-SMK-FX", regulatory="Trading", centre="GBLO",
         product="FxForward"),
    dict(name="GBP FX Options Book", portfolio="smokehouse.global", ccy="GBP",
         gl="GL-SMK-OPT", cost="CC-SMK-OPT", regulatory="Trading", centre="GBLO",
         product="FxOption"),
    dict(name="GBP Rates Book", portfolio="smokehouse.global", ccy="GBP",
         gl="GL-SMK-RAT", cost="CC-SMK-RAT", regulatory="Trading", centre="GBLO",
         product="Swap"),
]


# =============================================================================
# Script writers
# =============================================================================

def dataset_upsert(code, subject_area, name, description, artefact_type):
    def q(v):
        return v.replace("'", "''")
    return f"""    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        '{q(code)}',
        '{q(CATALOG_NAME)}',
        '{q(subject_area)}',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        '{q(name)}',
        '{q(description)}',
        '{DATASET_SOURCE_SYSTEM}',
        '{q(DATASET_BUSINESS_CONTEXT)}',
        current_date,
        'Internal Use Only',
        '{q(artefact_type)}'
    );
"""


def generate_catalog_populate():
    content = LICENSE_HEADER + GENERATED_NOTE + f"""
/**
 * Smokehouse Catalog Population Script
 *
 * Creates the catalog the smokehouse datasets are grouped under. Smokehouse is
 * the smallest tenant that exercises the publishing lifecycle: one legal
 * entity, one business unit, one portfolio, three books and one trade per book.
 * This script is idempotent.
 */

DO $$
BEGIN
    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        '{CATALOG_NAME}',
        'The smallest tenant that exercises the publishing lifecycle: one legal entity, three books, one trade per book and one feed.',
        'Development Team'
    );
END $$;
"""
    write("smokehouse_catalog_populate.sql", content)


def generate_dataset_populate():
    body = [
        dataset_upsert(
            "smokehouse.lei_entities", "Parties", "Smokehouse LEI Entities",
            "The company's own legal entity, on a checksum-valid synthetic LEI "
            "with the 9695 test prefix.", "lei_entities"),
        dataset_upsert(
            "smokehouse.lei_relationships", "Parties",
            "Smokehouse LEI Relationships",
            "The company has no subsidiaries, so the registration exists and "
            "carries no rows. The hierarchy publisher reads it by naming "
            "convention and requires the dataset to exist.",
            "lei_relationships"),
        dataset_upsert(
            "smokehouse.lei_parties", "Parties", "Smokehouse LEI Parties",
            "Publish trigger for the company's party. Publishing this dataset "
            "reads smokehouse.lei_entities/smokehouse.lei_relationships and "
            "creates the party the root LEI names. Carries no artefact rows of "
            "its own.", "lei_parties"),
        dataset_upsert(
            "smokehouse.business_units", "Organisation", "Smokehouse Business Units",
            "The company's single desk.", "business_units"),
        dataset_upsert(
            "smokehouse.portfolios", "Trading", "Smokehouse Portfolios",
            "The company's single portfolio.", "portfolios"),
        dataset_upsert(
            "smokehouse.books", "Trading", "Smokehouse Books",
            "Three books, each approved for the one product its trade carries: a "
            "forward, an option and a swap.", "books"),
        dataset_upsert(
            "smokehouse.party_counterparties", "Organisation",
            "Smokehouse Party Counterparties",
            "The GLEIF counterparties the party may trade with, by LEI, because "
            "a counterparty's id differs from tenant to tenant.",
            "party_counterparties"),
    ]
    for family, title, _tables in CONFIGURATION_DOCUMENTS:
        body.append(dataset_upsert(
            f"smokehouse.{family}", "Trading", f"Smokehouse {title}",
            f"The company's {title.lower()} document, cut to the currencies and "
            "the pair it prices.", family))
    body.append(dataset_upsert(
        "smokehouse.report_definitions", "Trading", "Smokehouse Report Definitions",
        "The one report the tenant runs, with the binding of each report to the "
        "configuration documents it reads.", "report_definitions"))
    content = LICENSE_HEADER + GENERATED_NOTE + f"""
/**
 * Smokehouse Dataset Population Script
 *
 * Registers the datasets the artefact scripts in this same directory publish.
 * This script is idempotent.
 */

DO $$
BEGIN
{''.join(body)}END $$;
"""
    write("smokehouse_dataset_populate.sql", content)


def generate_bundle_populate():
    content = LICENSE_HEADER + GENERATED_NOTE + f"""
/**
 * Smokehouse Provisioning Bundles
 *
 * Two bundles. =smokehouse_lei_import= publishes the company's legal entity as
 * a party, tenant-wide, before the party exists. =smokehouse= carries the
 * party's own organisation data and is published once the party exists, with a
 * {{"party_id": "..."}} param. This script is idempotent.
 */

DO $$
BEGIN
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'smokehouse_lei_import',
        'Smokehouse LEI Import',
        'tenant',
        'The Smokehouse legal entity. Published tenant-wide with the {{"root_lei": "{LEI}"}} param, before the party exists.'
    );
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse_lei_import', 'smokehouse.lei_entities', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse_lei_import', 'smokehouse.lei_relationships', 20);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse_lei_import', 'smokehouse.lei_parties', 30);

    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse', 'smokehouse.business_units', 10);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse', 'smokehouse.portfolios', 20);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse', 'smokehouse.books', 30);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse', 'smokehouse.party_counterparties', 40);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse', 'smokehouse.pricing_engines', 50);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse', 'smokehouse.todays_market', 60);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse', 'smokehouse.curve_configuration', 70);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse', 'smokehouse.engine_settings', 80);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse', 'smokehouse.risk_report_configs', 90);
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(), 'smokehouse', 'smokehouse.report_definitions', 100);

    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'smokehouse',
        'Smokehouse',
        'tenant',
        'Smokehouse Trading Ltd''s business unit, portfolio, books, tradeable counterparties and the report documents its valuation reads. Publish with a {{"party_id": "..."}} param scoping every member to the party.'
    );
END $$;
"""
    write("smokehouse_bundle_populate.sql", content)


def generate_dataset_dependency_populate():
    dependencies = [
        ("smokehouse.portfolios", "smokehouse.business_units", "owner_unit_source"),
        ("smokehouse.books", "smokehouse.portfolios", "parent_portfolio_source"),
        ("smokehouse.party_counterparties", "gleif.lei_counterparties.tiny",
         "counterparty_reference"),
        ("smokehouse.risk_report_configs", "smokehouse.portfolios", "scope_source"),
    ] + [
        (f"smokehouse.{family}", "refdata.conventions", "convention_source")
        for family, _title, _tables in CONFIGURATION_DOCUMENTS
    ] + [
        ("smokehouse.report_definitions", f"smokehouse.{family}", "configuration_source")
        for family, _title, _tables in CONFIGURATION_DOCUMENTS
    ]
    body = "".join(
        f"    PERFORM ores_dq_dataset_dependencies_upsert_fn("
        f"ores_utility_system_tenant_id_fn(),\n"
        f"        '{child}', '{parent}', '{reason}');\n"
        for child, parent, reason in dependencies)
    content = LICENSE_HEADER + GENERATED_NOTE + """
/**
 * Smokehouse Dataset Dependencies
 *
 * Portfolios resolve their owning business unit and books their parent
 * portfolio, so those publish first. The counterparty links name GLEIF LEIs, so
 * the counterparties publish first. This script is idempotent.
 */

DO $$
BEGIN
""" + body + """END $$;
"""
    write("smokehouse_dataset_dependency_populate.sql", content)


def artefact_script(code, filename, title, note, table, columns, rows):
    values = ",\n".join(rows)
    content = LICENSE_HEADER + GENERATED_NOTE + f"""
/**
 * Smokehouse {title} Artefact Population Script
 *
 * {note} This script is idempotent.
 */

DO $$
declare
    v_dataset_id uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = '{code}'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: {code}. Run smokehouse_dataset_populate.sql first.';
    end if;

    delete from {table}
    where dataset_id = v_dataset_id;

    insert into {table} (
        {", ".join(["dataset_id", "tenant_id"] + columns)}
    )
    values
{values};
end $$;
"""
    write(filename, content)


def generate_lei_entities_populate():
    office = OFFICE
    rows = [
        "        (v_dataset_id, ores_utility_system_tenant_id_fn(), "
        f"{sql_str(LEI)}, 0, {sql_str(LEGAL_NAME)}, 'GENERAL', null, 'ACTIVE', "
        f"null, null, 'GB', {sql_str(office['street'])}, {sql_str(office['city'])}, "
        f"null, 'GB', {sql_str(office['postal'])}, "
        f"{sql_str(office['street'])}, {sql_str(office['city'])}, null, 'GB', "
        f"{sql_str(office['postal'])})"
    ]
    artefact_script(
        "smokehouse.lei_entities", "smokehouse_lei_entities_artefact_populate.sql",
        "LEI Entities",
        "The company's own legal entity, addressed at its London office.",
        "ores_dq_lei_entities_artefact_tbl",
        ["lei", "version", "entity_legal_name", "entity_entity_category",
         "entity_entity_sub_category", "entity_entity_status",
         "entity_legal_form_entity_legal_form_code", "entity_legal_form_other_legal_form",
         "entity_legal_jurisdiction", "entity_legal_address_first_address_line",
         "entity_legal_address_city", "entity_legal_address_region",
         "entity_legal_address_country", "entity_legal_address_postal_code",
         "entity_headquarters_address_first_address_line",
         "entity_headquarters_address_city", "entity_headquarters_address_region",
         "entity_headquarters_address_country",
         "entity_headquarters_address_postal_code"], rows)


def generate_lei_relationships_populate():
    content = LICENSE_HEADER + GENERATED_NOTE + """
/**
 * Smokehouse LEI Relationships Artefact Population Script
 *
 * The company has no subsidiaries, so this registration carries no rows. The
 * hierarchy publisher reads the dataset by naming convention and creates the
 * root party alone. This script is idempotent.
 */

DO $$
declare
    v_dataset_id uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'smokehouse.lei_relationships'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: smokehouse.lei_relationships. Run smokehouse_dataset_populate.sql first.';
    end if;

    delete from ores_dq_lei_relationships_artefact_tbl
    where dataset_id = v_dataset_id;
end $$;
"""
    write("smokehouse_lei_relationships_artefact_populate.sql", content)


# =============================================================================
# The report documents
# =============================================================================

def missing_spot_entries(entries):
    """The tenant's own spot entries, for the pairs the template does not name.

    The template's market names a spot for every pair a bank quotes. This tenant
    trades one pair, and the template may not name it: EUR/GBP carries a
    volatility smile in the template and no spot, because the template's bank
    crosses it through the dollar. The party's own market holds the spot, so the
    entry is its own.
    """
    spot_collection = None
    for collection in template_rows("todays_market_collections"):
        if collection["collection"] == "FxSpots":
            spot_collection = collection
            break
    if spot_collection is None:
        raise ValueError("the template states no FxSpots collection")

    template = [e for e in template_rows("todays_market_entries")
                if e["target"].startswith("FX/")]
    if not template:
        raise ValueError("the template states no FX spot entry to copy")

    out = []
    for pair in MARKET_PAIRS:
        target = f"FX/{pair[:3]}/{pair[3:]}"
        if any(e["target"] == target for e in entries):
            continue
        out.append({
            **template[0],
            "id": uuid5("todays_market_entry:" + target),
            "todays_market_collection_id": spot_collection["id"],
            "key_value": pair,
            "key_value_2": None,
            "target": target,
            "position": max(e["position"] for e in entries) + 1 + len(out),
        })
    return out


def todays_market_entries():
    """The market entries this tenant reads, the pair's own spot among them."""
    entries = [r for r in template_rows("todays_market_entries")
               if market_currency(r["target"])]
    return entries + missing_spot_entries(entries)


def todays_market_rows(table):
    """Today's market, cut to the currencies and the pair the tenant prices."""
    rows = template_rows(table)
    entries = todays_market_entries()
    used = {e["todays_market_collection_id"] for e in entries}

    if table == "todays_market_entries":
        return entries
    if table == "todays_market_collections":
        return [r for r in rows if r["id"] in used]
    if table == "todays_market_configuration_bindings":
        names = {c["collection"] for c in template_rows("todays_market_collections")
                 if c["id"] in used}
        return [r for r in rows if r["collection"] in names]
    return rows


def curve_configuration_rows(table):
    """The curve configuration, cut to the curves of the tenant's market.

    Two kinds of curve are kept: the discount curve of each currency the tenant
    prices on, and the volatility surface of the one pair it trades. A trade
    names both -- its discount curves and, if it is an option, its surface -- and
    the run refuses a trade whose curve the configuration does not name.
    """
    rows = template_rows(table)
    currencies = set(CURVE_CURRENCIES)
    pairs = set(MARKET_PAIRS)

    yields = [y for y in template_rows("yield_curve_configs") if y["currency"] in currencies]
    yield_definitions = {y["curve_definition_id"] for y in yields}
    vol_definitions = {d["id"] for d in template_rows("curve_definitions")
                       if d["section_code"] == "FXVolatilities" and d["curve_id"] in pairs}
    definitions = yield_definitions | vol_definitions

    if table == "curve_configurations":
        return rows
    if table == "curve_definitions":
        return [r for r in rows if r["id"] in definitions]
    if table == "yield_curve_configs":
        return yields
    if table == "curve_segments":
        return [r for r in rows if r["curve_definition_id"] in definitions]
    if table == "curve_segment_conventions":
        segments = {s["id"] for s in template_rows("curve_segments")
                    if s["curve_definition_id"] in definitions}
        return [r for r in rows if r["curve_segment_id"] in segments]
    if table == "curve_quotes":
        return [r for r in rows if r["curve_definition_id"] in definitions]
    if table == "fx_volatility_configs":
        return [r for r in rows if r["curve_definition_id"] in vol_definitions]
    if table == "fx_volatility_smiles":
        configs = {c["id"] for c in template_rows("fx_volatility_configs")
                   if c["curve_definition_id"] in vol_definitions}
        return [r for r in rows if r["fx_volatility_config_id"] in configs]
    return []


# The one run settings document the valuation report binds, and the one report
# definition this tenant runs. A tenant that books three trades needs one of
# each, and every row naming anything else would be a document the report never
# reads.
REPORT_CONFIGURATION_NAME = "Valuation - Global Portfolio"
REPORT_DEFINITION_NAME = "NPV"


def risk_report_configs_rows(table):
    """The one run settings document the valuation report binds."""
    rows = template_rows(table)
    configs = [r for r in template_rows("risk_report_configs")
               if r["name"] == REPORT_CONFIGURATION_NAME]
    # A risk report configuration is registered as a reporting configuration of
    # its own, and the analytics and the market bindings hang off that id rather
    # than off the config's.
    ids = {c["configuration_id"] for c in configs}

    if table == "risk_report_configs":
        return configs
    if table == "configuration_analytics":
        return [r for r in rows if r["configuration_id"] in ids]
    if table == "configuration_analytic_parameters":
        analytics = {a["id"] for a in template_rows("configuration_analytics")
                     if a["configuration_id"] in ids}
        return [r for r in rows if r["configuration_analytic_id"] in analytics]
    if table == "configuration_market_bindings":
        return [r for r in rows if r["configuration_id"] in ids]
    return []


def generate_document_populate(family, title, tables, rows_of, note):
    code = f"smokehouse.{family}"
    body = []
    for table in tables:
        rows = rows_of(table)
        if not rows:
            raise ValueError(f"no {table} rows for {TEMPLATE_COMPANY}")
        columns = list(rows[0].keys())
        values = ",\n".join(
            "        (v_dataset_id, ores_utility_system_tenant_id_fn(), "
            + ", ".join(sql_value(r[c]) for c in columns) + ")"
            for r in rows)
        artefact = f"ores_dq_{table}_artefact_tbl"
        body.append(f"""
    delete from {artefact}
    where dataset_id = v_dataset_id;

    insert into {artefact} (
        dataset_id, tenant_id, {", ".join(columns)}
    )
    values
{values};
""")
    content = LICENSE_HEADER + GENERATED_NOTE + f"""
/**
 * Smokehouse {title} Artefact Population Script
 *
 * {note} This script is idempotent.
 */

DO $$
declare
    v_dataset_id uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = '{code}'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: {code}. Run smokehouse_dataset_populate.sql first.';
    end if;
{"".join(body)}end $$;
"""
    write(f"smokehouse_{family}_artefact_populate.sql", content)


def generate_report_definitions_populate():
    reports = [r for r in template_rows("report_definitions")
               if r["name"] == REPORT_DEFINITION_NAME]
    bindings = [r for r in template_rows("report_configurations")
                if r["report_name"] == REPORT_DEFINITION_NAME]
    if not reports or not bindings:
        raise ValueError("the template holds no NPV definition with its bindings")

    definition_rows = ",\n".join(
        "        (v_dataset_id, ores_utility_system_tenant_id_fn(), gen_random_uuid(), 1, "
        f"{sql_str(r['name'])}, {sql_str(r['description'])}, {sql_str(r['report_type'])}, "
        f"{sql_str(r['schedule_expression'])}, {sql_str(r['concurrency_policy'])}, "
        f"{r['display_order']}, {sql_str(r['tier'])}, "
        f"{sql_str(r['applicable_jurisdiction'])}, {sql_str(r['regulatory_book_type'])}, "
        f"{sql_str(r.get('cut_kind_code'))})"
        for r in reports)
    binding_rows = ",\n".join(
        "        (v_dataset_id, ores_utility_system_tenant_id_fn(), 0, "
        f"{sql_str(b['report_name'])}, {sql_str(b['configuration_type_code'])}, "
        f"{sql_str(b['configuration_name'])})" for b in bindings)
    content = LICENSE_HEADER + GENERATED_NOTE + f"""
/**
 * Smokehouse Report Definitions Artefact Population Script
 *
 * The one report the tenant runs, and the configuration documents it binds.
 * The bindings name a document, so the documents publish first. This script is
 * idempotent.
 */

DO $$
declare
    v_dataset_id uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'smokehouse.report_definitions'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: smokehouse.report_definitions. Run smokehouse_dataset_populate.sql first.';
    end if;

    delete from ores_dq_report_definitions_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_report_definitions_artefact_tbl (
        dataset_id, tenant_id, id, version, name, description, report_type,
        schedule_expression, concurrency_policy, display_order, tier,
        applicable_jurisdiction, regulatory_book_type, cut_kind_code
    )
    values
{definition_rows};

    delete from ores_dq_report_configurations_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_report_configurations_artefact_tbl (
        dataset_id, tenant_id, version, report_name, configuration_type_code,
        configuration_name
    )
    values
{binding_rows};
end $$;
"""
    write("smokehouse_report_definitions_artefact_populate.sql", content)


def generate_business_units_populate():
    id_by_code = {u["code"]: uuid5("bu:" + u["code"]) for u in BUSINESS_UNITS}
    rows = [
        "        (v_dataset_id, ores_utility_system_tenant_id_fn(), "
        f"{sql_str(id_by_code[u['code']])}, 0, {sql_str(u['name'])}, "
        f"{sql_str(id_by_code[u['parent']] if u['parent'] else None)}, "
        f"{sql_str(u['code'])}, {sql_str(u['centre'])}, {sql_str(u['type_'])})"
        for u in BUSINESS_UNITS]
    artefact_script(
        "smokehouse.business_units", "smokehouse_business_units_artefact_populate.sql",
        "Business Units",
        "parent_business_unit_id is the artefact-space id of the parent unit's own "
        "row in this dataset, which the publish remaps.",
        "ores_dq_business_units_artefact_tbl",
        ["id", "version", "unit_name", "parent_business_unit_id", "unit_code",
         "business_centre_code", "unit_type_code"], rows)


def generate_portfolios_populate():
    unit_id_by_code = {u["code"]: uuid5("bu:" + u["code"]) for u in BUSINESS_UNITS}
    portfolio_id_by_code = {p["code"]: uuid5("portfolio:" + p["code"]) for p in PORTFOLIOS}
    rows = [
        "        (v_dataset_id, ores_utility_system_tenant_id_fn(), "
        f"{sql_str(portfolio_id_by_code[p['code']])}, 0, {sql_str(p['name'])}, "
        f"{sql_str(portfolio_id_by_code[p['parent']] if p['parent'] else None)}, "
        f"{sql_str(unit_id_by_code[p['owner']])}, 'Risk', {sql_str(p['ccy'])}, "
        f"{sql_bool(p['virtual'])})"
        for p in PORTFOLIOS]
    artefact_script(
        "smokehouse.portfolios", "smokehouse_portfolios_artefact_populate.sql",
        "Portfolios",
        "parent_portfolio_id and owner_unit_id are the artefact-space ids of the "
        "rows in this dataset and the business units dataset, which the publish "
        "resolves.",
        "ores_dq_portfolios_artefact_tbl",
        ["id", "version", "name", "parent_portfolio_id", "owner_unit_id",
         "purpose_type", "aggregation_ccy", "is_virtual"], rows)


def generate_books_populate():
    portfolio_id_by_code = {p["code"]: uuid5("portfolio:" + p["code"]) for p in PORTFOLIOS}
    book_rows = [
        "        (v_dataset_id, ores_utility_system_tenant_id_fn(), "
        f"{sql_str(uuid5('book:' + b['name']))}, 0, {sql_str(b['name'])}, "
        f"{sql_str(portfolio_id_by_code[b['portfolio']])}, {sql_str(b['ccy'])}, "
        f"{sql_str(b['gl'])}, {sql_str(b['cost'])}, 'Active', "
        f"{sql_str(b['regulatory'])}, false, {sql_str(b['centre'])}, "
        "'Trading', 'Automatic')"
        for b in BOOKS]
    product_type_rows = [
        "        (v_dataset_id, ores_utility_system_tenant_id_fn(), "
        f"{sql_str(uuid5('book:' + b['name']))}, {sql_str(b['product'])}, 0)"
        for b in BOOKS]
    content = LICENSE_HEADER + GENERATED_NOTE + """
/**
 * Smokehouse Books Artefact Population Script
 *
 * Three books, and the one product type each may carry. The publishing derives
 * the approval matrix from these rows: the book restricts to the type, its desk
 * and the party inherit it, and the trade planner refuses any product the book's
 * matrix does not permit. parent_portfolio_id is the artefact-space id of the
 * owning row in the portfolios dataset, which the publish resolves by name. This
 * script is idempotent.
 */

DO $$
declare
    v_dataset_id uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'smokehouse.books'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: smokehouse.books. Run smokehouse_dataset_populate.sql first.';
    end if;

    delete from ores_dq_books_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_books_artefact_tbl (
        dataset_id, tenant_id, id, version, name, parent_portfolio_id,
        functional_currency, gl_account_ref, cost_center, book_status,
        regulatory_book_type, is_sweepable, rates_centre_code, book_purpose_type,
        ledger_feed_type
    )
    values
""" + ",\n".join(book_rows) + """;

    delete from ores_dq_book_product_types_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_book_product_types_artefact_tbl (
        dataset_id, tenant_id, book_id, product_type_code, version
    )
    values
""" + ",\n".join(product_type_rows) + """;
end $$;
"""
    write("smokehouse_books_artefact_populate.sql", content)


def generate_party_counterparties_populate():
    rows = [
        "        (v_dataset_id, ores_utility_system_tenant_id_fn(), "
        f"{sql_str(lei)}, 0)"
        for lei in counterparty_leis()]
    artefact_script(
        "smokehouse.party_counterparties",
        "smokehouse_party_counterparties_artefact_populate.sql",
        "Party Counterparties",
        "The counterparties the party may trade with, by LEI, because a "
        "counterparty's id differs from tenant to tenant.",
        "ores_dq_party_counterparties_artefact_tbl",
        ["counterparty_lei", "version"], rows)


def main():
    generate_catalog_populate()
    generate_dataset_populate()
    generate_bundle_populate()
    generate_dataset_dependency_populate()
    generate_lei_entities_populate()
    generate_lei_relationships_populate()
    generate_business_units_populate()
    generate_portfolios_populate()
    generate_books_populate()
    generate_party_counterparties_populate()
    generate_document_populate(
        "pricing_engines", "Pricing Engines", CONFIGURATION_DOCUMENTS[0][2],
        lambda table: template_rows(table),
        "The pricing engines the run resolves each product to. The template's own.")
    generate_document_populate(
        "todays_market", "Today's Market", CONFIGURATION_DOCUMENTS[1][2],
        todays_market_rows,
        "How the tenant marks its trades: the currencies it prices on and the one "
        "pair it trades. The template's market, cut to those.")
    generate_document_populate(
        "curve_configuration", "Curve Configuration", CONFIGURATION_DOCUMENTS[2][2],
        curve_configuration_rows,
        "The yield curves the run builds, one per currency the tenant prices on. "
        "The template's, cut to those.")
    generate_document_populate(
        "risk_report_configs", "Risk Report Configurations", CONFIGURATION_DOCUMENTS[3][2],
        risk_report_configs_rows,
        "The run settings the valuation report binds, and the portfolio it scopes "
        "them to.")
    generate_document_populate(
        "engine_settings", "Engine Settings", CONFIGURATION_DOCUMENTS[4][2],
        lambda table: template_rows(table),
        "The engine settings the run resolves for a valuation. The template's own.")
    generate_report_definitions_populate()


if __name__ == "__main__":
    main()
