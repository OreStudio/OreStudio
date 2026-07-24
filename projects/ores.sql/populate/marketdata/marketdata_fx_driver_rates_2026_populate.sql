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
 * FX Driver Rates Seed Population Script — 2026-05-05 Vintage
 *
 * Registers the marketdata.fx_driver_rates_2026 dataset (member of the
 * marketdata.reference_vintage_2026_05_05 bundle) and seeds the artefact
 * table with real Federal Reserve H.10 FX spot rates for the same
 * USD-hub spanning tree as the 2016 vintage (see
 * marketdata_fx_driver_rates_populate.sql): EUR/USD, GBP/USD, USD/CHF,
 * USD/JPY, USD/SEK, AUD/USD, USD/CAD, NZD/USD, USD/ZAR, USD/MXN,
 * USD/INR, USD/NOK, USD/DKK. Every value is an individual, direct Fed
 * H.10 quote - none are derived/computed here.
 *
 * Date: 2026-05-05, a single current-regime snapshot (unlike the 2016
 * dataset's 25 vintages across the year) -- this dataset exists
 * specifically to ground synthetic.fx_spot_configs.realistic_2026 in a
 * real current FX vintage instead of reusing 2016-02-05 data, closing
 * the gap noted in that dataset's own description.
 *
 * Values fetched via tools/fetch_fed_h10_rates.sh (a thin bash wrapper
 * around tools/fetch_fed_h10_rates.py, committed - reused for any future
 * date/currency addition):
 *   ./tools/fetch_fed_h10_rates.sh --currency eu,uk,sz,ja,sd,al,ca,nz,sf,mx,in,no,dn \
 *       --dates 5-MAY-26 --format sql
 *
 * Execution order: this file registers its own catalog/dataset/bundle, no
 * dependency on other populate scripts.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Methodology Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_methodologies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'Federal Reserve H.10 Historical Rates',
        'Real, individually-quoted foreign exchange rates from the U.S. Federal Reserve Board''s H.10 Foreign Exchange Rates release (https://www.federalreserve.gov/releases/h10/). Each rate is that release''s own published value for its target date - never derived, computed, or triangulated by OreStudio. Applies to any dataset sourced from H.10, not just this one; per-value citation (release page URL, retrieval timestamp) lives on the artefact row itself (source_url/retrieved_at), not here.',
        'Federal Reserve Board H.10 Foreign Exchange Rates, historical release pages (https://www.federalreserve.gov/releases/h10/hist/)',
        'One release page per currency, each published daily; the value used is that page''s own row for the dataset''s target date.'
    );
END $$;

-- =============================================================================
-- Catalog Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        'FX Driver Rates',
        'Curated, real FX spot driver rates for use as Cross-Rates Matrix input and synthetic market data seeding.',
        'OreStudio Development Team'
    );
END $$;

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.fx_driver_rates_2026',
        'FX Driver Rates',
        'FX Spot',
        'Market Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'Federal Reserve H.10 Historical Rates',
        'FX Driver Rates 2026-05-05',
        'One real Federal Reserve H.10 FX spot vintage at 2026-05-05, the same minimal USD-hub spanning tree over EUR/USD/GBP/CHF/JPY/SEK/AUD/CAD/NZD/ZAR/MXN/INR/NOK/DKK as the 2016 dataset.',
        'ORESTUDIO',
        'Seed data for the 2026 Realistic synthetic FX theme and the FX driver rates Librarian bundle',
        '2026-05-05',
        'Internal Use Only',
        'market_data_observations'
    );
END $$;

-- =============================================================================
-- Bundle Registration
--
-- Groups every market-data dataset for this vintage date under one
-- provisionable unit, mirroring marketdata.reference_vintage_2016_02_05.
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.reference_vintage_2026_05_05',
        'Market Data Reference Vintage: 2026-05-05',
        'Every curated, real market-data dataset for the 2026-05-05 reference vintage - FX spot today, more asset classes over time.'
    );

    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.reference_vintage_2026_05_05',
        'marketdata.fx_driver_rates_2026',
        1,
        false
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

DO $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_retrieved_at timestamptz := '2026-07-23 00:00:00+00';
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'marketdata.fx_driver_rates_2026'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: marketdata.fx_driver_rates_2026';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_market_data_observations_artefact_tbl
    where dataset_id = v_dataset_id;

    raise debug 'Populating FX driver rates for dataset: marketdata.fx_driver_rates_2026';

    insert into ores_dq_market_data_observations_artefact_tbl (
        dataset_id, tenant_id, version,
        series_type, metric, qualifier, point_id, observation_date, value, source,
        source_url, retrieved_at
    )
    select
        v_dataset_id,
        v_tenant_id,
        0,
        'FX',
        'RATE',
        r.qualifier,
        'SPOT',
        r.observation_date,
        r.value,
        'fed.h10.' || r.observation_date::text,
        r.source_url,
        v_retrieved_at
    from (values
        -- 2026-05-05: current-regime reference vintage for the 2026 Realistic synthetic theme.
        ('AUD/USD', date '2026-05-05', 0.7196, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2026-05-05', 1.1707, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2026-05-05', 1.3576, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2026-05-05', 0.5903, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2026-05-05', 1.3609, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2026-05-05', 0.7822, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/DKK', date '2026-05-05', 6.3827, 'https://www.federalreserve.gov/releases/h10/hist/dat00_dn.htm'),
        ('USD/INR', date '2026-05-05', 95.3200, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2026-05-05', 157.6800, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2026-05-05', 17.3741, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/NOK', date '2026-05-05', 9.2402, 'https://www.federalreserve.gov/releases/h10/hist/dat00_no.htm'),
        ('USD/SEK', date '2026-05-05', 9.2460, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2026-05-05', 16.6371, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm')
    ) as r(qualifier, observation_date, value, source_url);

    get diagnostics v_count = row_count;
    raise debug 'Populated % FX driver rate row(s)', v_count;
end $$;
