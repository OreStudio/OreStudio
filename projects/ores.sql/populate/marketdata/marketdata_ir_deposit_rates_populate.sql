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
 * IR Deposit Rates Seed Population Script
 *
 * Registers the marketdata.ir_deposit_rates dataset (member of the
 * marketdata.reference_vintage_2016_02_05 bundle, alongside
 * marketdata.fx_driver_rates) and seeds the artefact table with a single
 * real IR deposit-tenor rate: 3-month USD LIBOR on 2016-02-05, taken
 * directly from ORE's own bundled Legacy/Example_56 sample market data
 * (external/ore/examples/Legacy/Example_56/Input/market.txt, row
 * "MM/RATE/USD/2D/3M 0.007961"). This is the exact DEPOSIT-tenor point
 * ir_curve_generation_config.price_source='vintage' resolves against for
 * a USD-LIBOR-3M config (see resolve_vintage_initial_rate in
 * ir_curve_feed.cpp) -- synthetic.themes.ore_samples_2016's
 * USD-LIBOR-3M config is seeded to use it.
 *
 * Scoped to one currency/tenor for now (unlike the FX driver rates'
 * full 14-currency, 25-date spread) -- extend with more
 * currency/tenor/date rows here as more IR curve configs move to
 * price_source='vintage'.
 *
 * Execution order: this file registers its own dataset and joins the
 * existing marketdata.reference_vintage_2016_02_05 bundle registered by
 * marketdata_fx_driver_rates_populate.sql -- run after that script.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Methodology Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_methodologies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ORE Bundled Sample Market Data',
        'Real market rates bundled with the Open Source Risk Engine (ORE) itself as reference example input, taken from its Legacy/Example_56 sample (https://github.com/OpenSourceRisk/Engine/tree/master/Examples/Legacy/Example_56). Each rate is that file''s own published value for its target date - never derived, computed, or triangulated by OreStudio. Per-value citation (source file path, retrieval timestamp) lives on the artefact row itself (source_url/retrieved_at), not here.',
        'OpenSourceRisk/Engine, Examples/Legacy/Example_56/Input/market.txt',
        'One bundled market data file, containing rates for the 2016-02-05 reference date; the value used is that file''s own row for the given series.'
    );
END $$;

-- =============================================================================
-- Catalog Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        'IR Deposit Rates',
        'Curated, real IR deposit-tenor rates for use as synthetic IR curve vintage-mode seeding.',
        'OreStudio Development Team'
    );
END $$;

-- =============================================================================
-- Subject Area Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_subject_areas_upsert_fn(ores_utility_system_tenant_id_fn(),
        'Market Data',
        'IR Rates',
        'Interest rate curve observation/rate values (deposit, FRA, swap).'
    );
END $$;

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.ir_deposit_rates',
        'IR Deposit Rates',
        'IR Rates',
        'Market Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'ORE Bundled Sample Market Data',
        'IR Deposit Rates 2016',
        'Real IR deposit-tenor rate(s) from ORE''s own bundled Legacy/Example_56 sample, for the 2016-02-05 reference vintage -- currently just USD LIBOR-3M, the DEPOSIT anchor for synthetic.themes.ore_samples_2016''s USD-LIBOR-3M config.',
        'ORESTUDIO',
        'Seed data for the IR deposit rates Librarian bundle',
        '2016-02-05',
        'Internal Use Only',
        'market_data_observations'
    );
END $$;

-- =============================================================================
-- Bundle Membership
--
-- Joins the existing 2016-02-05 reference vintage bundle registered by
-- marketdata_fx_driver_rates_populate.sql, rather than creating a new one.
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_dataset_bundle_members_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.reference_vintage_2016_02_05',
        'marketdata.ir_deposit_rates',
        2,
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
    v_retrieved_at timestamptz := '2026-07-25 00:00:00+00';
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'marketdata.ir_deposit_rates'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: marketdata.ir_deposit_rates';
    end if;

    -- Clear existing rows for this dataset (idempotency)
    delete from ores_dq_market_data_observations_artefact_tbl
    where dataset_id = v_dataset_id;

    raise debug 'Populating IR deposit rates for dataset: marketdata.ir_deposit_rates';

    insert into ores_dq_market_data_observations_artefact_tbl (
        dataset_id, tenant_id, version,
        series_type, metric, qualifier, point_id, observation_date, value, source,
        source_url, retrieved_at
    )
    select
        v_dataset_id,
        v_tenant_id,
        0,
        'RATES',
        'YIELD',
        r.qualifier,
        r.point_id,
        r.observation_date,
        r.value,
        'ore.samples.' || r.observation_date::text,
        r.source_url,
        v_retrieved_at
    from (values
        -- 2016-02-05: ORE's own bundled reference vintage date (Legacy/Example_56), matching the
        -- FX driver rates dataset's own reference date. qualifier = currency/index-without-prefix
        -- (see strip_currency_prefix in ir_curve_feed.cpp); point_id = the DEPOSIT entry's
        -- end_tenor_code (see select_vintage_anchor_entry).
        ('USD/LIBOR-3M', '3M', date '2016-02-05', 0.007961,
         'external/ore/examples/Legacy/Example_56/Input/market.txt (MM/RATE/USD/2D/3M)')
    ) as r(qualifier, point_id, observation_date, value, source_url);

    get diagnostics v_count = row_count;
    raise debug 'Populated % IR deposit rate row(s)', v_count;
end $$;
