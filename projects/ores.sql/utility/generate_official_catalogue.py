#!/usr/bin/env python3
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
#
"""Writes the catalogue of the official rates providers.

Each provider below is a publisher whose terms were read and allow its rates to
be stored; the page that records what each says is
doc/knowledge/domain/synthetic_calibration_sources.org. Their currencies are
read from Frankfurter's provider list, so the seed names exactly what each
publisher quotes against its pivot. Each currency is a vendor instrument of the
provider, in the publisher's notation (iso4217, the currency, the rate), mapped
to the ORE FX fixing FX-<KEY>-<PIVOT>-<CURRENCY>. The system party subscribes to
each provider and routes to its Frankfurter source.

Run from the repository root; it rewrites the populate script:

    python3 projects/ores.sql/utility/generate_official_catalogue.py
    python3 projects/ores.sql/utility/generate_official_catalogue.py --providers saved.json
"""

import argparse
import json
import urllib.request
import uuid

OUTPUT = "projects/ores.sql/populate/marketdata/marketdata_official_catalogue_populate.sql"
FRANKFURTER = "https://api.frankfurter.dev/v2/providers"
NAMESPACE = uuid.UUID("3b4f2d0e-7a51-4c8e-9a2b-6f1d0c5e8a47")

# Read 2026-10-28. The credit is what each publisher's terms ask a copy to carry.
PROVIDERS = [
    {
        "code": "ecb", "key": "ECB", "name": "European Central Bank",
        "terms_url": "https://www.ecb.europa.eu/services/using-our-site/disclaimer/html/index.en.html",
        "credit": "Source: ECB statistics",
        "description": "The euro foreign exchange reference rates, published each business day at about 16:00 CET.",
    },
    {
        "code": "imf", "key": "IMF", "name": "International Monetary Fund",
        "terms_url": "https://www.imf.org/en/about/copyright-and-terms",
        "credit": "Source: International Monetary Fund",
        "description": "The representative exchange rates against the US dollar. Commercial reuse needs the IMF's permission.",
    },
    {
        "code": "boc", "key": "BOC", "name": "Bank of Canada",
        "terms_url": "https://www.bankofcanada.ca/terms/",
        "credit": "Source: Bank of Canada",
        "description": "The indicative exchange rates against the Canadian dollar, published each business day.",
    },
    {
        "code": "tcmb", "key": "TCMB", "name": "Central Bank of the Republic of Türkiye",
        "terms_url": "https://www.tcmb.gov.tr/wps/wcm/connect/EN/TCMB+EN/Bottom+Menu/Other/Disclaimer",
        "credit": "Source: Central Bank of the Republic of Türkiye",
        "description": "The indicative exchange rates against the Turkish lira. Commercial use needs the bank's permission.",
    },
]

LICENCE = """/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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


def q(text):
    return "'" + text.replace("'", "''") + "'"


def source_name(p):
    return f"{p['code']}-frankfurter"


def source_id(p):
    return str(uuid.uuid5(NAMESPACE, "source:" + source_name(p)))


def fixing_uri(pivot, key, currency):
    return f"oresmd://fx/{pivot}?type=fixing&index=fx&source={key}&ccy={currency}"


def load(path):
    if path:
        with open(path) as f:
            return json.load(f)
    with urllib.request.urlopen(FRANKFURTER, timeout=30) as r:
        return json.load(r)


def render(listed):
    by_key = {p["key"]: p for p in listed}
    rows = []
    for p in PROVIDERS:
        entry = by_key[p["key"]]
        pivot = entry["pivot_currency"]
        for c in sorted(entry["currencies"]):
            if c != pivot:
                rows.append((p["code"], c, fixing_uri(pivot, p["key"], c)))

    out = [LICENCE, """/**
 * The Official Rates Catalogue
 *
 * The providers whose daily rates the official rates adapter reads through
 * Frankfurter, their sources, their currencies as vendor instruments mapped to
 * FX fixings, and the system party's subscription and route to each. A provider
 * carries its terms; nothing is fetched for it until the system administrator
 * accepts them.
 *
 * GENERATED by projects/ores.sql/utility/generate_official_catalogue.py from
 * Frankfurter's provider list. Do not edit by hand; rerun the script.
 */
"""]
    out.append("\\echo '--- Official Rates Providers ---'\n")
    out.append("""insert into ores_marketdata_providers_tbl (
    tenant_id, code, version, name, kind, description, display_order,
    terms_url, terms_credit, allows_storage, credential_ref,
    modified_by, performed_by, change_reason_code, change_commentary
)
values""")
    values = []
    for i, p in enumerate(PROVIDERS):
        values.append(
            f"    (ores_utility_system_tenant_id_fn(), {q(p['code'])}, 0, {q(p['name'])}, 'reference',\n"
            f"     {q(p['description'])},\n"
            f"     {20 + i}, {q(p['terms_url'])}, {q(p['credit'])}, true, '',\n"
            f"     current_user, current_user, 'system.initial_load', 'Initial population of the official rates providers')")
    out.append(",\n".join(values))
    out.append("""on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
""")
    out.append("\\echo '--- Official Rates Sources ---'\n")
    out.append("""insert into ores_marketdata_market_data_sources_tbl (
    id, tenant_id, version, name, provider_code, kind, is_default, description,
    modified_by, performed_by, change_reason_code, change_commentary
)
values""")
    values = []
    for p in PROVIDERS:
        values.append(
            f"    ({q(source_id(p))}, ores_utility_system_tenant_id_fn(), 0,\n"
            f"     {q(source_name(p))}, {q(p['code'])}, 'live', true,\n"
            f"     {q('The rates of ' + p['name'] + ' read through Frankfurter by the official rates adapter.')},\n"
            f"     current_user, current_user, 'system.initial_load', 'Initial population of the official rates sources')")
    out.append(",\n".join(values))
    out.append("""on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
""")
    out.append("\\echo '--- Official Rates Vendor Instruments and Mappings ---'\n")
    out.append("begin;\n\ncreate temporary table t_official_inventory (\n    provider_code text, external_id text, oresmd_uri text\n) on commit drop;\n")
    out.append("insert into t_official_inventory (provider_code, external_id, oresmd_uri) values")
    out.append(",\n".join(f"    ({q(code)}, {q(c)}, {q(uri)})" for code, c, uri in rows) + ";\n")
    out.append("""insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted, deviation_note,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5(t.provider_code || '|iso4217|' || t.external_id || '|rate')::uuid,
       ores_utility_system_tenant_id_fn(), 0, t.provider_code, 'iso4217', t.external_id, 'rate',
       false, '', current_user, current_user, 'system.initial_load',
       'Initial population of the official rates vendor instruments'
from t_official_inventory t
where not exists (
    select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
    where v.tenant_id = ores_utility_system_tenant_id_fn()
      and v.provider_code = t.provider_code
      and v.id_scheme = 'iso4217'
      and v.external_id = t.external_id
      and v.field = 'rate'
      and v.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_data_mappings_tbl (
    id, tenant_id, version, vendor_instrument_id, oresmd_uri, effective_from,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v.id, t.oresmd_uri,
       timestamptz '2026-01-01 00:00:00+00', current_user, current_user, 'system.initial_load',
       'Initial population of the official rates mappings'
from t_official_inventory t
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.tenant_id = ores_utility_system_tenant_id_fn()
 and v.provider_code = t.provider_code
 and v.id_scheme = 'iso4217'
 and v.external_id = t.external_id
 and v.field = 'rate'
 and v.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_market_data_mappings_tbl m
    where m.tenant_id = ores_utility_system_tenant_id_fn()
      and m.vendor_instrument_id = v.id
      and m.valid_to = ores_utility_infinity_timestamp_fn()
);

commit;
""")
    out.append("\\echo '--- Official Rates Subscriptions and Routes of the System Party ---'\n")
    out.append("do $$\ndeclare\n    v_party uuid := ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn());\nbegin\n    if v_party is null then\n        raise notice 'No system party yet: the official rates subscriptions wait for it';\n        return;\n    end if;\n")
    for p in PROVIDERS:
        sub = str(uuid.uuid5(NAMESPACE, "subscription:" + p["code"]))
        route = str(uuid.uuid5(NAMESPACE, "route:" + p["code"]))
        out.append(f"""    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        {q(sub)}, ores_utility_system_tenant_id_fn(), 0, v_party, {q(p['code'])},
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes the official rates of {p['key']} for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, provider_code, source_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        {q(route)}, ores_utility_system_tenant_id_fn(), 0, v_party, {q(p['code'])}, {q(source_id(p))},
        current_user, current_user, 'system.initial_load',
        'The system party reads the official rates of {p['key']} through Frankfurter'
    ) on conflict do nothing;
""")
    out.append("end $$;\n")
    return "\n".join(out)


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--providers", help="a saved /v2/providers response, instead of reading it")
    args = ap.parse_args()
    with open(OUTPUT, "w") as f:
        f.write(render(load(args.providers)))
    print(f"wrote {OUTPUT}")


if __name__ == "__main__":
    main()
