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
 * FX Driver Rates Seed Population Script
 *
 * Registers the marketdata.fx_driver_rates dataset (member of the
 * marketdata.reference_vintage_2016_02_05 bundle) and seeds the artefact
 * table with 8 real Federal Reserve H.10 FX spot rates for 2016-02-05 - a
 * minimal USD-hub spanning tree over EUR, USD, GBP, CHF, JPY, SEK, AUD,
 * CAD, NZD, fit for purpose as the future Cross-Rates Matrix's driver
 * input. Every value is an individual, direct Fed H.10 quote - none are
 * derived/computed here; each row carries its own source_url/retrieved_at
 * citation rather than a shared text blob, so future additions (more
 * dates, more asset classes: rates curves, vol surfaces, ...) don't grow
 * an ever-larger methodology description.
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
-- Subject Area Registration
--
-- First dataset under the Market Data domain (as opposed to Reference
-- Data > Market Data, which is for market-data *reference types* like
-- asset classes - this is actual observation/rate values).
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_subject_areas_upsert_fn(ores_utility_system_tenant_id_fn(),
        'Market Data',
        'FX Spot',
        'FX spot observation/rate values.'
    );
END $$;

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.fx_driver_rates',
        'FX Driver Rates',
        'FX Spot',
        'Market Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'Federal Reserve H.10 Historical Rates',
        'FX Driver Rates 2016-02-05',
        '8 real Federal Reserve H.10 FX spot rates for 2016-02-05, a minimal USD-hub spanning tree over EUR/USD/GBP/CHF/JPY/SEK/AUD/CAD/NZD.',
        'ORESTUDIO',
        'Seed data for the FX driver rates Librarian bundle',
        '2016-02-05',
        'Internal Use Only',
        'market_data_observations'
    );
END $$;

-- =============================================================================
-- Bundle Registration
--
-- Groups every market-data dataset for this vintage date under one
-- provisionable unit. This is the first member; future additions for the
-- same date (rates curves, vol surfaces, ...) become further members of
-- this same bundle, not new bundles.
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_dataset_bundles_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.reference_vintage_2016_02_05',
        'Market Data Reference Vintage: 2016-02-05',
        'Every curated, real market-data dataset for the 2016-02-05 reference vintage - FX spot today, more asset classes over time.'
    );

    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.reference_vintage_2016_02_05',
        'marketdata.fx_driver_rates',
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
    v_retrieved_at timestamptz := '2026-07-10 00:00:00+00';
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'marketdata.fx_driver_rates'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: marketdata.fx_driver_rates';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_market_data_observations_artefact_tbl
    where dataset_id = v_dataset_id;

    raise debug 'Populating FX driver rates for dataset: marketdata.fx_driver_rates';

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
        date '2016-02-05',
        r.value,
        'fed.h10.2016-02-05',
        r.source_url,
        v_retrieved_at
    from (values
        ('EUR/USD', 1.1131,   'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', 1.4468,   'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('USD/CHF', 0.9938,   'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/JPY', 121.2300, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/SEK', 8.4802,   'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('AUD/USD', 0.7084,   'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('USD/CAD', 1.3875,   'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('NZD/USD', 0.6619,   'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm')
    ) as r(qualifier, value, source_url);

    get diagnostics v_count = row_count;
    raise debug 'Populated % FX driver rate row(s)', v_count;
end $$;
