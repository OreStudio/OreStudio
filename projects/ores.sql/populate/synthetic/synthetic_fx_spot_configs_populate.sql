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
 * Synthetic FX Spot Config Seed Population Script
 *
 * Registers the synthetic.fx_spot_configs dataset and seeds the artefact
 * table with a starter set of synthetic FX spot generation configs, so a
 * party can get a usable FX synthetic data setup in one bundle Apply
 * instead of hand-authoring configs. Starting spots are plausible,
 * mutually-consistent reference values (not sourced from a specific
 * as-of date) - a full internally-consistent 30-pair library is tracked
 * separately.
 *
 * Execution order: this file registers its own catalog/dataset, no
 * dependency on other populate scripts.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Catalog Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        'Synthetic Market Data',
        'Synthetic market data generation configs for parties: GMM-parameterised tick generators seeded with plausible starting values.',
        'OreStudio Development Team'
    );
END $$;

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.fx_spot_configs',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic FX Spot Configs',
        'Starter set of synthetic FX spot generation configs (currency pair, GMM initial price, tick cadence) for party provisioning.',
        'ORESTUDIO',
        'Seed data for the synthetic FX spot Librarian bundle',
        current_date,
        'Internal Use Only',
        'synthetic_fx_spot_configs'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

do $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'synthetic.fx_spot_configs'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.fx_spot_configs';
    end if;

    -- Skip if already populated
    if exists (
        select 1 from ores_dq_synthetic_fx_spot_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic FX spot configs artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic FX spot configs for dataset: synthetic.fx_spot_configs';

    insert into ores_dq_synthetic_fx_spot_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled,
        base_currency_code, quote_currency_code,
        gmm_initial_price, ticks_per_hour, process_type
    )
    values
    (v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
     'Synthetic FX Spot',
     'Default synthetic FX spot generator for the party''s market data configuration.',
     true, 'EUR', 'USD', 1.0850, 60, 'geometric');
end $$;
