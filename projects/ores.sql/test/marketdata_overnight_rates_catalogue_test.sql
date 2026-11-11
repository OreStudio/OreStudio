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
 *   item. Where a curve names the index by an alias convention, such as
 *   EUR-ESTR for EUR-ESTER, the alias states the same item.
 * - Each publisher of an overnight series states its terms and its credit, so
 *   the terms can be accepted at setup.
 * - The feed of each overnight adapter is scoped to its one item, so the demand
 *   asks it for nothing else.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/marketdata_overnight_rates_catalogue_test.sql
 */

begin;

select plan(5);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select results_eq(
    $$select v.provider_code || ' ' || v.external_id || ' ' || coalesce(c.id, '<no convention>')
      from ores_marketdata_market_data_vendor_instruments_tbl v
      join ores_marketdata_market_data_mappings_tbl m
        on m.vendor_instrument_id = v.id
       and m.tenant_id = v.tenant_id
       and m.valid_to = ores_utility_infinity_timestamp_fn()
      left join (
          select a.id, a.oresmd_uri from ores_dq_overnight_index_conventions_artefact_tbl a
          join ores_dq_datasets_tbl d
            on d.id = a.dataset_id
           and d.code = 'refdata.conventions'
           and d.valid_to = ores_utility_infinity_timestamp_fn()
      ) c on c.oresmd_uri = m.oresmd_uri
      where v.tenant_id = ores_utility_system_tenant_id_fn()
        and v.id_scheme in ('nyfed_rate_type', 'ecb_sdmx', 'boe_iadb', 'boc_valet', 'boj_stat', 'nb_sdmx', 'riksbank_swestr', 'rba_table', 'bcb_sgs', 'banxico_sie')
        and v.valid_to = ores_utility_infinity_timestamp_fn()
      order by 1$$,
    $$values ('banxico SF331451 MXN-FTIIE'),
             ('bcb 4389 BRL-CDI'),
             ('boc AVG.INTWO CAD-CORRA'),
             ('boe IUDSOIA GBP-SONIA'),
             ('boj FM01.STRDCLUCON JPY-TONA'),
             ('boj FM01.STRDCLUCON JPY-TONAR'),
             ('ecb EST.B.EU000A2X2A25.WT EUR-ESTER'),
             ('ecb EST.B.EU000A2X2A25.WT EUR-ESTR'),
             ('norgesbank SHORT_RATES.B.NOWA.ON.R NOK-NOWA'),
             ('nyfed SOFR USD-SOFR'),
             ('rba f1.FIRMMCRID AUD-AONIA'),
             ('riksbank SWESTR SEK-SWESTR')$$,
    'each overnight series maps to the item of an overnight index convention of the conventions dataset'
);

select is_empty(
    $$select code from ores_marketdata_providers_tbl
      where tenant_id = ores_utility_system_tenant_id_fn()
        and code in ('nyfed', 'ecb', 'boe', 'boc', 'boj', 'norgesbank', 'riksbank', 'rba', 'bcb', 'banxico')
        and valid_to = ores_utility_infinity_timestamp_fn()
        and (terms_url = '' or terms_credit = '' or not allows_storage)$$,
    'each publisher of an overnight series states its terms and its credit'
);

select is_empty(
    $$select code from ores_marketdata_providers_tbl
      where tenant_id = ores_utility_system_tenant_id_fn()
        and valid_to = ores_utility_infinity_timestamp_fn()
        and credential_ref <> ''
        and credential_ref !~ '^env:ORES_MARKETDATA_[A-Z0-9_]+$'$$,
    'a provider names its key by an env ref, never by the key itself'
);

select results_eq(
    $$select code || ' ' || credential_ref from ores_marketdata_providers_tbl
      where tenant_id = ores_utility_system_tenant_id_fn()
        and code in ('banxico', 'riksbank')
        and valid_to = ores_utility_infinity_timestamp_fn()
      order by 1$$,
    $$values ('banxico env:ORES_MARKETDATA_BANXICO_API_KEY'),
             ('riksbank env:ORES_MARKETDATA_RIKSBANK_API_KEY')$$,
    'Banco de Mexico and the Riksbank name the variable of their key'
);

select results_eq(
    $$select f.name || ' ' || s.asset_class || ' ' || s.item
      from ores_marketdata_feed_scopes_tbl s
      join ores_marketdata_feeds_tbl f
        on f.id = s.feed_id
       and f.tenant_id = s.tenant_id
       and f.valid_to = ores_utility_infinity_timestamp_fn()
      where s.tenant_id = ores_utility_system_tenant_id_fn()
        and f.name in ('nyfed-sofr', 'ecb-estr', 'boe-sonia', 'boc-corra', 'boj-tona', 'norgesbank-nowa', 'riksbank-swestr', 'rba-aonia', 'bcb-cdi', 'banxico-tiie-fondeo')
        and s.valid_to = ores_utility_infinity_timestamp_fn()
      order by 1$$,
    $$values ('banxico-tiie-fondeo interest_rates oresmd://ir/MXN?type=fixing&index=ibor&name=FTIIE'),
             ('bcb-cdi interest_rates oresmd://ir/BRL?type=fixing&index=ibor&name=CDI'),
             ('boc-corra interest_rates oresmd://ir/CAD?type=fixing&index=ibor&name=CORRA'),
             ('boe-sonia interest_rates oresmd://ir/GBP?type=fixing&index=ibor&name=SONIA'),
             ('boj-tona interest_rates oresmd://ir/JPY?type=fixing&index=ibor&name=TONAR'),
             ('ecb-estr interest_rates oresmd://ir/EUR?type=fixing&index=ibor&name=ESTER'),
             ('norgesbank-nowa interest_rates oresmd://ir/NOK?type=fixing&index=ibor&name=NOWA'),
             ('nyfed-sofr interest_rates oresmd://ir/USD?type=fixing&index=ibor&name=SOFR'),
             ('rba-aonia interest_rates oresmd://ir/AUD?type=fixing&index=ibor&name=AONIA'),
             ('riksbank-swestr interest_rates oresmd://ir/SEK?type=fixing&index=ibor&name=SWESTR')$$,
    'each overnight feed is scoped to the item of its series'
);

select * from finish();
rollback;
