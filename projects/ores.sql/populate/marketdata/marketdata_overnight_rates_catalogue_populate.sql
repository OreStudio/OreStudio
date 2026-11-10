/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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

/**
 * The Overnight Rates Catalogue
 *
 * Each publisher's overnight rate series is a vendor instrument of that
 * publisher, in its own notation, mapped to the fixing item of its index. The
 * item is the oresmd address that the index's built-in convention states, so a
 * public fixing and a synthetic feed of one index name the same item. The terms
 * of each publisher are in doc/knowledge/domain/synthetic_calibration_sources.org.
 * The ECB and the Bank of Canada are seeded by the official rates catalogue.
 */

\echo '--- Overnight Rates Providers ---'
insert into ores_marketdata_providers_tbl (
    tenant_id, code, version, name, kind, description, display_order,
    terms_url, terms_credit, allows_storage, credential_ref,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'nyfed', 0, 'Federal Reserve Bank of New York', 'reference',
     'The reference rates of the US dollar, among them SOFR, published each business day at about 08:00 New York time.',
     30, 'https://www.newyorkfed.org/privacy/termsofuse',
     'Source: Federal Reserve Bank of New York', true, '',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers'),
    (ores_utility_system_tenant_id_fn(), 'boe', 0, 'Bank of England', 'reference',
     'The statistical database of the Bank of England, among them SONIA, published each London business day.',
     31, 'https://www.bankofengland.co.uk/legal',
     'SONIA and/or SONIA Compounded Index data licensed under the Open Government Licence v3.0 and copyright the Governor and Company of the Bank of England', true, '',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

\echo '--- Overnight Rates Vendor Instruments and Mappings ---'
create temporary table t_overnight_inventory (
    provider_code text, id_scheme text, external_id text, field text, oresmd_uri text
) on commit drop;

insert into t_overnight_inventory (provider_code, id_scheme, external_id, field, oresmd_uri) values
    ('nyfed', 'nyfed_rate_type', 'SOFR', 'percentRate', 'oresmd://ir/USD?type=fixing&index=ibor&name=SOFR'),
    ('ecb', 'ecb_sdmx', 'EST.B.EU000A2X2A25.WT', 'OBS_VALUE', 'oresmd://ir/EUR?type=fixing&index=ibor&name=ESTER'),
    ('boe', 'boe_iadb', 'IUDSOIA', 'value', 'oresmd://ir/GBP?type=fixing&index=ibor&name=SONIA'),
    ('boc', 'boc_valet', 'AVG.INTWO', 'v', 'oresmd://ir/CAD?type=fixing&index=ibor&name=CORRA');

insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted, deviation_note,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5(t.provider_code || '|' || t.id_scheme || '|' || t.external_id || '|' || t.field)::uuid,
       ores_utility_system_tenant_id_fn(), 0, t.provider_code, t.id_scheme, t.external_id, t.field,
       false, '', current_user, current_user, 'system.initial_load',
       'Initial population of the overnight rates vendor instruments'
from t_overnight_inventory t
where not exists (
    select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
    where v.tenant_id = ores_utility_system_tenant_id_fn()
      and v.provider_code = t.provider_code
      and v.id_scheme = t.id_scheme
      and v.external_id = t.external_id
      and v.field = t.field
      and v.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_data_mappings_tbl (
    id, tenant_id, version, vendor_instrument_id, oresmd_uri, effective_from,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v.id, t.oresmd_uri,
       timestamptz '2026-01-01 00:00:00+00', current_user, current_user, 'system.initial_load',
       'Initial population of the overnight rates mappings'
from t_overnight_inventory t
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.tenant_id = ores_utility_system_tenant_id_fn()
 and v.provider_code = t.provider_code
 and v.id_scheme = t.id_scheme
 and v.external_id = t.external_id
 and v.field = t.field
 and v.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_market_data_mappings_tbl m
    where m.tenant_id = ores_utility_system_tenant_id_fn()
      and m.vendor_instrument_id = v.id
      and m.valid_to = ores_utility_infinity_timestamp_fn()
);
