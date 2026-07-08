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
 * Currency Pairs Seed Population Script
 *
 * Registers the refdata.currency_pairs dataset and seeds the artefact table
 * with a starter set of standard FX currency pairs (majors, G10 crosses, and
 * common USD/EM crosses), so a party can get a usable currency_pairs set in
 * one bundle Apply instead of hand-authoring pairs. base/quote follow the
 * standard FX market quoting convention (not derived from
 * currency.base_precedence — some liquid crosses, e.g. GBP/JPY, don't follow
 * the precedence ordering exactly).
 *
 * Execution order: this file registers its own catalog/dataset, no
 * dependency on other populate scripts other than the ISO currencies
 * artefact already existing (soft FK only, not enforced here).
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Catalog Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        'FX Market Conventions',
        'Curated FX market reference data: standard currency pairs and their conventions.',
        'OreStudio Development Team'
    );
END $$;

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_pairs',
        'FX Market Conventions',
        'Currency Pairs',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Standard Currency Pairs',
        'Curated starter set of standard FX currency pairs (majors, G10 crosses, common USD/EM crosses) for party provisioning.',
        'ORESTUDIO',
        'Seed data for the currency pairs Librarian bundle',
        current_date,
        'Internal Use Only',
        'currency_pairs'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

DO $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.currency_pairs'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.currency_pairs';
    end if;

    -- Clear existing pairs for this dataset (idempotency)
    delete from ores_dq_currency_pairs_artefact_tbl
    where dataset_id = v_dataset_id;

    raise debug 'Populating currency pairs for dataset: refdata.currency_pairs';

    insert into ores_dq_currency_pairs_artefact_tbl (
        dataset_id, tenant_id, pair_code, version,
        base_currency, quote_currency, deliverable, settlement_currency,
        classification, fixing_source
    )
    select
        v_dataset_id,
        v_tenant_id,
        c.base_currency || '/' || c.quote_currency,
        0,
        c.base_currency,
        c.quote_currency,
        c.deliverable,
        c.settlement_currency,
        c.classification,
        c.fixing_source
    from (values
        -- Majors
        ('EUR', 'USD', true, null::text, 'major', 'WM/Reuters 4pm London'),
        ('GBP', 'USD', true, null, 'major', 'WM/Reuters 4pm London'),
        ('USD', 'JPY', true, null, 'major', 'WM/Reuters 4pm London'),
        ('USD', 'CHF', true, null, 'major', 'WM/Reuters 4pm London'),
        ('USD', 'CAD', true, null, 'major', 'WM/Reuters 4pm London'),
        ('AUD', 'USD', true, null, 'major', 'WM/Reuters 4pm London'),
        ('NZD', 'USD', true, null, 'major', 'WM/Reuters 4pm London'),
        -- EUR crosses
        ('EUR', 'GBP', true, null, 'minor', null),
        ('EUR', 'JPY', true, null, 'minor', null),
        ('EUR', 'CHF', true, null, 'minor', null),
        ('EUR', 'CAD', true, null, 'minor', null),
        ('EUR', 'AUD', true, null, 'minor', null),
        ('EUR', 'NZD', true, null, 'minor', null),
        ('EUR', 'NOK', true, null, 'minor', null),
        ('EUR', 'SEK', true, null, 'minor', null),
        ('EUR', 'DKK', true, null, 'minor', null),
        ('EUR', 'PLN', true, null, 'minor', null),
        ('EUR', 'CZK', true, null, 'minor', null),
        ('EUR', 'HUF', true, null, 'minor', null),
        ('EUR', 'TRY', true, null, 'minor', null),
        -- GBP crosses
        ('GBP', 'JPY', true, null, 'minor', null),
        ('GBP', 'CHF', true, null, 'minor', null),
        ('GBP', 'CAD', true, null, 'minor', null),
        ('GBP', 'AUD', true, null, 'minor', null),
        ('GBP', 'NZD', true, null, 'minor', null),
        -- Other G10 crosses
        ('AUD', 'JPY', true, null, 'minor', null),
        ('AUD', 'CAD', true, null, 'minor', null),
        ('AUD', 'CHF', true, null, 'minor', null),
        ('AUD', 'NZD', true, null, 'minor', null),
        ('NZD', 'JPY', true, null, 'minor', null),
        ('CAD', 'JPY', true, null, 'minor', null),
        ('CHF', 'JPY', true, null, 'minor', null),
        ('NOK', 'SEK', true, null, 'minor', null),
        -- USD / EM crosses (deliverable=false + settlement_currency=USD for
        -- currencies flagged non-deliverable in the ISO currencies dataset)
        ('USD', 'CNY', false, 'USD', 'exotic', null),
        ('USD', 'HKD', true, null, 'exotic', null),
        ('USD', 'SGD', true, null, 'exotic', null),
        ('USD', 'INR', false, 'USD', 'exotic', null),
        ('USD', 'KRW', false, 'USD', 'exotic', null),
        ('USD', 'TWD', false, 'USD', 'exotic', null),
        ('USD', 'THB', true, null, 'exotic', null),
        ('USD', 'MYR', false, 'USD', 'exotic', null),
        ('USD', 'IDR', false, 'USD', 'exotic', null),
        ('USD', 'PHP', false, 'USD', 'exotic', null),
        ('USD', 'ZAR', true, null, 'exotic', null),
        ('USD', 'MXN', true, null, 'exotic', null),
        ('USD', 'BRL', true, null, 'exotic', null),
        ('USD', 'TRY', true, null, 'exotic', null),
        ('USD', 'RUB', false, 'USD', 'exotic', null),
        ('USD', 'ILS', true, null, 'exotic', null),
        ('USD', 'SAR', true, null, 'exotic', null),
        ('USD', 'AED', true, null, 'exotic', null),
        ('USD', 'PLN', true, null, 'exotic', null),
        ('USD', 'CZK', true, null, 'exotic', null),
        ('USD', 'HUF', true, null, 'exotic', null),
        ('USD', 'RON', true, null, 'exotic', null)
    ) as c(base_currency, quote_currency, deliverable, settlement_currency, classification, fixing_source);

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % hand-curated currency pairs for dataset: refdata.currency_pairs', v_count;

    -- Mechanical coverage pass: pair every remaining ISO 4217 currency
    -- against USD so every published currency has at least one pair,
    -- rather than leaving the long tail of EM/frontier currencies
    -- entirely unpaired. Convention: fiat currencies quote against USD
    -- (USD/<ccy>); commodity currencies (precious metals) are quoted as
    -- <metal>/USD, matching real market convention. deliverable and
    -- settlement_currency are derived from the currency's own curated
    -- flag (see iso_currencies_artefact_populate.sql), not re-decided
    -- here. Supranational (XDR) is skipped: not a spot-traded pair.
    -- Already-covered currencies (either leg of a hand-curated pair
    -- above) are skipped via the anti-join.
    insert into ores_dq_currency_pairs_artefact_tbl (
        dataset_id, tenant_id, pair_code, version,
        base_currency, quote_currency, deliverable, settlement_currency,
        classification, fixing_source
    )
    select
        v_dataset_id,
        v_tenant_id,
        case when iso.monetary_nature = 'commodity'
             then iso.iso_code || '/USD'
             else 'USD/' || iso.iso_code
        end,
        0,
        case when iso.monetary_nature = 'commodity' then iso.iso_code else 'USD' end,
        case when iso.monetary_nature = 'commodity' then 'USD' else iso.iso_code end,
        iso.deliverable,
        case when iso.deliverable then null else 'USD' end,
        case when iso.monetary_nature = 'commodity' then 'commodity' else 'exotic' end,
        null
    from ores_dq_currencies_artefact_tbl iso
    join ores_dq_datasets_tbl iso_ds
        on iso_ds.id = iso.dataset_id
        and iso_ds.name = 'ISO 4217 Currency Codes'
    where iso.tenant_id = v_tenant_id
      and iso.iso_code <> 'USD'
      and iso.monetary_nature in ('fiat', 'commodity')
      and not exists (
          select 1 from ores_dq_currency_pairs_artefact_tbl existing
          where existing.dataset_id = v_dataset_id
            and (existing.base_currency = iso.iso_code or existing.quote_currency = iso.iso_code)
      );

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % mechanically-derived currency pairs for dataset: refdata.currency_pairs', v_count;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Currency Pairs Summary ---'

select 'Total DQ Currency Pairs' as metric, count(*) as count
from ores_dq_currency_pairs_artefact_tbl
union all
select 'Major Pairs', count(*)
from ores_dq_currency_pairs_artefact_tbl
where classification = 'major'
union all
select 'Minor Pairs', count(*)
from ores_dq_currency_pairs_artefact_tbl
where classification = 'minor'
union all
select 'Exotic Pairs', count(*)
from ores_dq_currency_pairs_artefact_tbl
where classification = 'exotic'
union all
select 'Commodity Pairs', count(*)
from ores_dq_currency_pairs_artefact_tbl
where classification = 'commodity'
union all
select 'Non-Deliverable Pairs', count(*)
from ores_dq_currency_pairs_artefact_tbl
where deliverable = false;
