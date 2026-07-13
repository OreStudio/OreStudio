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
 * table with real Federal Reserve H.10 FX spot rates for an 11-pair
 * USD-hub spanning tree: the original 8 majors (EUR/USD, GBP/USD,
 * USD/CHF, USD/JPY, USD/SEK, AUD/USD, USD/CAD, NZD/USD) plus 3 EM/exotic
 * currencies (USD/ZAR, USD/MXN, USD/INR) added for the Cross-Rates
 * Matrix's exotics tier -- covers EUR, USD, GBP, CHF, JPY, SEK, AUD,
 * CAD, NZD, ZAR, MXN, INR. Every value is an individual, direct Fed
 * H.10 quote - none are derived/computed here; each row carries its own
 * source_url/retrieved_at citation rather than a shared text blob, so
 * future additions (more dates, more asset classes: rates curves, vol
 * surfaces, ...) don't grow an ever-larger methodology description.
 *
 * Dates: 25 vintages across 2016 - the first and last trading day of each
 * calendar month (24 dates, spanning the year so the dataset isn't
 * hardcoded to a single point in time), plus 2016-02-05 specifically,
 * which is ORE's own bundled reference vintage date (see
 * external/ore/examples/Legacy/Example_56/Input/market.txt,
 * https://github.com/OpenSourceRisk/Engine/tree/master/Examples/Legacy/Example_56)
 * - chosen so this dataset can stand in for that vintage directly.
 *
 * Values regenerated via tools/fetch_fed_h10_rates.py (committed - reused
 * for any future date/currency addition). Raw source HTML for every
 * currency page is archived at external/fed_h10/ (see its README.md) for
 * full reproducibility independent of the Fed's site staying up:
 *   ./tools/fetch_fed_h10_rates.py --currency eu,uk,sz,ja,sd,al,ca,nz,sf,mx,in \
 *       --dates <25 target dates> --format sql
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
        'FX Driver Rates 2016',
        '25 real Federal Reserve H.10 FX spot vintages across 2016 (start/end of each month, plus 2016-02-05 - ORE''s own bundled reference vintage date), a minimal USD-hub spanning tree over EUR/USD/GBP/CHF/JPY/SEK/AUD/CAD/NZD/ZAR/MXN/INR.',
        'ORESTUDIO',
        'Seed data for the FX driver rates Librarian bundle',
        '2016-12-31',
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
    v_retrieved_at timestamptz := '2026-07-13 00:00:00+00';
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
        r.observation_date,
        r.value,
        'fed.h10.' || r.observation_date::text,
        r.source_url,
        v_retrieved_at
    from (values
        ('AUD/USD', date '2016-01-04', 0.7166, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-01-04', 1.0803, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-01-04', 1.4686, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-01-04', 0.6740, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-01-04', 1.3970, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-01-04', 1.0042, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-01-04', 66.5600, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-01-04', 119.3000, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-01-04', 17.3600, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-01-04', 8.5057, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-01-04', 15.6150, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-01-04 / 2016-01-29: first/last trading day of January.
        ('AUD/USD', date '2016-01-29', 0.7071, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-01-29', 1.0832, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-01-29', 1.4184, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-01-29', 0.6469, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-01-29', 1.4074, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-01-29', 1.0226, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-01-29', 67.8700, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-01-29', 121.0500, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-01-29', 18.2110, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-01-29', 8.5709, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-01-29', 15.9535, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-02-01 / 2016-02-29: first/last trading day of February.
        ('AUD/USD', date '2016-02-01', 0.7084, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-02-01', 1.0888, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-02-01', 1.4367, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-02-01', 0.6512, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-02-01', 1.4005, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-02-01', 1.0202, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-02-01', 67.9200, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-02-01', 121.0600, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-02-01', 18.2930, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-02-01', 8.5385, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-02-01', 15.9480, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-02-05: ORE's own bundled reference vintage date (Legacy/Example_56).
        ('AUD/USD', date '2016-02-05', 0.7084, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-02-05', 1.1131, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-02-05', 1.4468, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-02-05', 0.6619, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-02-05', 1.3875, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-02-05', 0.9938, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-02-05', 67.7800, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-02-05', 116.9900, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-02-05', 18.3720, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-02-05', 8.4802, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-02-05', 15.9785, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-02-29', 0.7152, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-02-29', 1.0868, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-02-29', 1.3926, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-02-29', 0.6601, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-02-29', 1.3522, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-02-29', 0.9960, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-02-29', 68.2100, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-02-29', 112.9000, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-02-29', 18.0675, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-02-29', 8.5709, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-02-29', 15.7465, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-03-01 / 2016-03-31: first/last trading day of March.
        ('AUD/USD', date '2016-03-01', 0.7172, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-03-01', 1.0847, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-03-01', 1.3948, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-03-01', 0.6619, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-03-01', 1.3404, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-03-01', 0.9994, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-03-01', 67.7500, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-03-01', 113.9400, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-03-01', 17.9000, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-03-01', 8.6312, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-03-01', 15.5465, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-03-31', 0.7677, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-03-31', 1.1390, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-03-31', 1.4381, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-03-31', 0.6926, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-03-31', 1.2969, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-03-31', 0.9583, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-03-31', 66.2500, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-03-31', 112.4200, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-03-31', 17.2140, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-03-31', 8.0962, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-03-31', 14.7100, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-04-01 / 2016-04-29: first/last trading day of April.
        ('AUD/USD', date '2016-04-01', 0.7672, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-04-01', 1.1385, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-04-01', 1.4204, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-04-01', 0.6899, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-04-01', 1.3047, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-04-01', 0.9588, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-04-01', 66.3400, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-04-01', 112.0600, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-04-01', 17.3205, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-04-01', 8.1164, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-04-01', 14.7350, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-04-29', 0.7612, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-04-29', 1.1441, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-04-29', 1.4625, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-04-29', 0.6984, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-04-29', 1.2549, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-04-29', 0.9598, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-04-29', 66.3900, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-04-29', 106.9000, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-04-29', 17.1900, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-04-29', 8.0267, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-04-29', 14.2355, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-05-02 / 2016-05-31: first/last trading day of May.
        ('AUD/USD', date '2016-05-02', 0.7641, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-05-02', 1.1516, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-05-02', 1.4666, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-05-02', 0.7009, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-05-02', 1.2544, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-05-02', 0.9556, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-05-02', 66.3600, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-05-02', 106.4800, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-05-02', 17.2425, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-05-02', 7.9761, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-05-02', 14.3255, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-05-31', 0.7242, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-05-31', 1.1135, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-05-31', 1.4530, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-05-31', 0.6769, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-05-31', 1.3097, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-05-31', 0.9934, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-05-31', 67.1200, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-05-31', 110.7500, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-05-31', 18.4130, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-05-31', 8.3360, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-05-31', 15.7150, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-06-01 / 2016-06-30: first/last trading day of June.
        ('AUD/USD', date '2016-06-01', 0.7242, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-06-01', 1.1165, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-06-01', 1.4395, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-06-01', 0.6807, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-06-01', 1.3089, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-06-01', 0.9892, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-06-01', 67.4400, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-06-01', 109.5500, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-06-01', 18.5475, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-06-01', 8.3030, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-06-01', 15.5975, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-06-30', 0.7432, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-06-30', 1.1032, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-06-30', 1.3242, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-06-30', 0.7120, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-06-30', 1.3010, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-06-30', 0.9792, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-06-30', 67.5100, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-06-30', 102.7700, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-06-30', 18.4935, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-06-30', 8.5028, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-06-30', 14.7755, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-07-01 / 2016-07-29: first/last trading day of July.
        ('AUD/USD', date '2016-07-01', 0.7485, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-07-01', 1.1145, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-07-01', 1.3281, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-07-01', 0.7167, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-07-01', 1.2897, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-07-01', 0.9730, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-07-01', 67.2400, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-07-01', 102.5500, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-07-01', 18.3930, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-07-01', 8.4301, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-07-01', 14.5755, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-07-29', 0.7599, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-07-29', 1.1168, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-07-29', 1.3270, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-07-29', 0.7216, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-07-29', 1.3040, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-07-29', 0.9690, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-07-29', 66.7700, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-07-29', 102.3200, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-07-29', 18.7610, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-07-29', 8.5503, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-07-29', 13.8875, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-08-01 / 2016-08-31: first/last trading day of August.
        ('AUD/USD', date '2016-08-01', 0.7567, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-08-01', 1.1176, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-08-01', 1.3209, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-08-01', 0.7196, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-08-01', 1.3096, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-08-01', 0.9671, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-08-01', 66.6300, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-08-01', 102.2600, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-08-01', 18.8325, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-08-01', 8.5701, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-08-01', 13.9005, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-08-31', 0.7519, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-08-31', 1.1146, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-08-31', 1.3129, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-08-31', 0.7257, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-08-31', 1.3122, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-08-31', 0.9830, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-08-31', 66.9400, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-08-31', 103.3800, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-08-31', 18.8490, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-08-31', 8.5689, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-08-31', 14.6985, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-09-01 / 2016-09-30: first/last trading day of September.
        ('AUD/USD', date '2016-09-01', 0.7542, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-09-01', 1.1194, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-09-01', 1.3273, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-09-01', 0.7282, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-09-01', 1.3107, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-09-01', 0.9792, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-09-01', 66.8500, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-09-01', 103.2200, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-09-01', 18.8260, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-09-01', 8.5504, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-09-01', 14.6845, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-09-30', 0.7667, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-09-30', 1.1238, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-09-30', 1.3015, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-09-30', 0.7290, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-09-30', 1.3115, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-09-30', 0.9694, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-09-30', 66.5800, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-09-30', 101.2100, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-09-30', 19.3355, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-09-30', 8.5726, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-09-30', 13.7200, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-10-03 / 2016-10-31: first/last trading day of October.
        ('AUD/USD', date '2016-10-03', 0.7664, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-10-03', 1.1210, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-10-03', 1.2840, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-10-03', 0.7263, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-10-03', 1.3138, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-10-03', 0.9740, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-10-03', 66.5200, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-10-03', 101.5400, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-10-03', 19.3350, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-10-03', 8.5641, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-10-03', 13.6450, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-10-31', 0.7611, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-10-31', 1.0962, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-10-31', 1.2212, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-10-31', 0.7156, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-10-31', 1.3403, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-10-31', 0.9890, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-10-31', 66.7200, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-10-31', 105.0700, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-10-31', 18.7900, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-10-31', 9.0207, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-10-31', 13.4850, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-11-01 / 2016-11-30: first/last trading day of November.
        ('AUD/USD', date '2016-11-01', 0.7659, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-11-01', 1.1042, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-11-01', 1.2218, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-11-01', 0.7176, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-11-01', 1.3377, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-11-01', 0.9790, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-11-01', 66.7000, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-11-01', 104.5900, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-11-01', 19.1000, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-11-01', 8.9479, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-11-01', 13.5750, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-11-30', 0.7387, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-11-30', 1.0578, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-11-30', 1.2481, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-11-30', 0.7081, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-11-30', 1.3425, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-11-30', 1.0187, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-11-30', 68.5600, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-11-30', 114.3400, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-11-30', 20.4565, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-11-30', 9.2548, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-11-30', 14.0775, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        -- 2016-12-01 / 2016-12-30: first/last trading day of December.
        ('AUD/USD', date '2016-12-01', 0.7411, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-12-01', 1.0634, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-12-01', 1.2596, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-12-01', 0.7072, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-12-01', 1.3330, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-12-01', 1.0126, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-12-01', 68.2900, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-12-01', 114.3400, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-12-01', 20.7375, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-12-01', 9.2479, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-12-01', 14.0750, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm'),
        ('AUD/USD', date '2016-12-30', 0.7230, 'https://www.federalreserve.gov/releases/h10/hist/dat00_al.htm'),
        ('EUR/USD', date '2016-12-30', 1.0552, 'https://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm'),
        ('GBP/USD', date '2016-12-30', 1.2337, 'https://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm'),
        ('NZD/USD', date '2016-12-30', 0.6958, 'https://www.federalreserve.gov/releases/h10/hist/dat00_nz.htm'),
        ('USD/CAD', date '2016-12-30', 1.3426, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ca.htm'),
        ('USD/CHF', date '2016-12-30', 1.0160, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sz.htm'),
        ('USD/INR', date '2016-12-30', 67.9200, 'https://www.federalreserve.gov/releases/h10/hist/dat00_in.htm'),
        ('USD/JPY', date '2016-12-30', 116.7800, 'https://www.federalreserve.gov/releases/h10/hist/dat00_ja.htm'),
        ('USD/MXN', date '2016-12-30', 20.6170, 'https://www.federalreserve.gov/releases/h10/hist/dat00_mx.htm'),
        ('USD/SEK', date '2016-12-30', 9.0803, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sd.htm'),
        ('USD/ZAR', date '2016-12-30', 13.7000, 'https://www.federalreserve.gov/releases/h10/hist/dat00_sf.htm')
    ) as r(qualifier, observation_date, value, source_url);

    get diagnostics v_count = row_count;
    raise debug 'Populated % FX driver rate row(s)', v_count;
end $$;
