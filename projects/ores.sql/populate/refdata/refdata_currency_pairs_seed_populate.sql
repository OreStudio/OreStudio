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
        base_currency, quote_currency, classification
    )
    select
        v_dataset_id,
        v_tenant_id,
        c.base_currency || '/' || c.quote_currency,
        0,
        c.base_currency,
        c.quote_currency,
        c.classification
    from (values
        -- Majors
        ('EUR', 'USD', 'major'),
        ('GBP', 'USD', 'major'),
        ('USD', 'JPY', 'major'),
        ('USD', 'CHF', 'major'),
        ('USD', 'CAD', 'major'),
        ('AUD', 'USD', 'major'),
        ('NZD', 'USD', 'major'),
        -- EUR crosses
        ('EUR', 'GBP', 'minor'),
        ('EUR', 'JPY', 'minor'),
        ('EUR', 'CHF', 'minor'),
        ('EUR', 'CAD', 'minor'),
        ('EUR', 'AUD', 'minor'),
        ('EUR', 'NZD', 'minor'),
        ('EUR', 'NOK', 'minor'),
        ('EUR', 'SEK', 'minor'),
        ('EUR', 'DKK', 'minor'),
        ('EUR', 'PLN', 'minor'),
        ('EUR', 'CZK', 'minor'),
        ('EUR', 'HUF', 'minor'),
        ('EUR', 'TRY', 'minor'),
        -- GBP crosses
        ('GBP', 'JPY', 'minor'),
        ('GBP', 'CHF', 'minor'),
        ('GBP', 'CAD', 'minor'),
        ('GBP', 'AUD', 'minor'),
        ('GBP', 'NZD', 'minor'),
        -- Other G10 crosses
        ('AUD', 'JPY', 'minor'),
        ('AUD', 'CAD', 'minor'),
        ('AUD', 'CHF', 'minor'),
        ('AUD', 'NZD', 'minor'),
        ('NZD', 'JPY', 'minor'),
        ('CAD', 'JPY', 'minor'),
        ('CHF', 'JPY', 'minor'),
        ('NOK', 'SEK', 'minor'),
        -- USD / EM crosses
        ('USD', 'CNY', 'exotic'),
        ('USD', 'HKD', 'exotic'),
        ('USD', 'SGD', 'exotic'),
        ('USD', 'INR', 'exotic'),
        ('USD', 'KRW', 'exotic'),
        ('USD', 'TWD', 'exotic'),
        ('USD', 'THB', 'exotic'),
        ('USD', 'MYR', 'exotic'),
        ('USD', 'IDR', 'exotic'),
        ('USD', 'PHP', 'exotic'),
        ('USD', 'ZAR', 'exotic'),
        ('USD', 'MXN', 'exotic'),
        ('USD', 'BRL', 'exotic'),
        ('USD', 'TRY', 'exotic'),
        ('USD', 'RUB', 'exotic'),
        ('USD', 'ILS', 'exotic'),
        ('USD', 'SAR', 'exotic'),
        ('USD', 'AED', 'exotic'),
        ('USD', 'PLN', 'exotic'),
        ('USD', 'CZK', 'exotic'),
        ('USD', 'HUF', 'exotic'),
        ('USD', 'RON', 'exotic')
    ) as c(base_currency, quote_currency, classification);

    get diagnostics v_count = row_count;

    raise debug 'Successfully populated % hand-curated currency pairs for dataset: refdata.currency_pairs', v_count;

    -- Mechanical coverage pass: pair every remaining ISO 4217 currency
    -- against USD so every published currency has at least one pair,
    -- rather than leaving the long tail of EM/frontier currencies
    -- entirely unpaired. Convention: fiat currencies quote against USD
    -- (USD/<ccy>); commodity currencies (precious metals) are quoted as
    -- <metal>/USD, matching real market convention. Supranational (XDR)
    -- is skipped: not a spot-traded pair. Already-covered currencies
    -- (either leg of a hand-curated pair above) are skipped via the
    -- anti-join.
    insert into ores_dq_currency_pairs_artefact_tbl (
        dataset_id, tenant_id, pair_code, version,
        base_currency, quote_currency, classification
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
        case when iso.monetary_nature = 'commodity' then 'commodity' else 'exotic' end
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
where classification = 'commodity';
