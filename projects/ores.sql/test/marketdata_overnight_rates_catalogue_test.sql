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
 * pgTAP tests for the catalogue of the overnight rates.
 *
 * - Each publisher's overnight series maps to the fixing item that the index's
 *   built-in convention states, so a public fixing and the index name the same
 *   item.
 * - Each publisher of an overnight series states its terms and its credit, so
 *   the terms can be accepted at setup.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/marketdata_overnight_rates_catalogue_test.sql
 */

begin;

select plan(2);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select results_eq(
    $$select v.provider_code || ' ' || v.external_id || ' ' || coalesce(c.id, '<no convention>')
      from ores_marketdata_market_data_vendor_instruments_tbl v
      join ores_marketdata_market_data_mappings_tbl m
        on m.vendor_instrument_id = v.id
       and m.tenant_id = v.tenant_id
       and m.valid_to = ores_utility_infinity_timestamp_fn()
      left join ores_refdata_overnight_index_conventions_tbl c
        on c.oresmd_uri = m.oresmd_uri
       and c.tenant_id = v.tenant_id
       and c.valid_to = ores_utility_infinity_timestamp_fn()
      where v.tenant_id = ores_utility_system_tenant_id_fn()
        and v.id_scheme in ('nyfed_rate_type', 'ecb_sdmx', 'boe_iadb', 'boc_valet')
        and v.valid_to = ores_utility_infinity_timestamp_fn()
      order by 1$$,
    $$values ('boc AVG.INTWO CAD-CORRA'), ('boe IUDSOIA GBP-SONIA'),
             ('ecb EST.B.EU000A2X2A25.WT EUR-ESTER'), ('nyfed SOFR USD-SOFR')$$,
    'each overnight series maps to the item of its index convention'
);

select is_empty(
    $$select code from ores_marketdata_providers_tbl
      where tenant_id = ores_utility_system_tenant_id_fn()
        and code in ('nyfed', 'ecb', 'boe', 'boc')
        and valid_to = ores_utility_infinity_timestamp_fn()
        and (terms_url = '' or terms_credit = '' or not allows_storage)$$,
    'each publisher of an overnight series states its terms and its credit'
);

select * from finish();
rollback;
