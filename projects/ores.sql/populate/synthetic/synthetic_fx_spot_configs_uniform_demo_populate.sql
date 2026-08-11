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
 * Synthetic FX Spot Config Seed Population Script — Uniform Volatility Demo
 *
 * Registers the synthetic.fx_spot_configs.uniform_demo dataset: all 8 major +
 * 3 EM/exotic FX driver pairs from the marketdata.fx_driver_rates
 * dataset (see marketdata_fx_driver_rates_populate.sql), seeded from
 * the 2016-02-05 Fed H.10 vintage (an arbitrary starting price for this
 * archetype, not a vintage grounding -- see the note below), each on a
 * single-component geometric (multiplicative) process with a deliberately
 * exaggerated per-tick volatility — the same for every pair, not
 * individually calibrated — so movement is easy to eyeball on a chart and
 * every UI feature (ticking, charting, GMM editing) gets exercised without
 * needing to squint.
 *
 * This is not a vintage theme -- it doesn't pin to any point in market
 * history -- it's a deliberately simple demo/exercise archetype. Contrast
 * with synthetic_fx_spot_configs_ore_samples_2016_populate.sql, whose
 * per-pair volatility is calibrated to plausible real-world FX behaviour
 * for the 2016 ORE Samples vintage theme.
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

-- Registers the theme dataset itself (synthetic.themes.uniform_demo) -- shared
-- with this theme's IR curve configs (see
-- synthetic_ir_curve_configs_uniform_demo_populate.sql), which looks this
-- dataset up by code rather than registering its own. One theme, one
-- dataset, published atomically by ores_synthetic_publish_theme_from_dq_fn --
-- see doc/plans/2026-07-27-synthetic-theme-atomic-dataset-design.org.
DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.themes.uniform_demo',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic Theme: Uniform Volatility Demo',
        '8 major + 3 EM/exotic FX driver pairs (single-component geometric process) and one Vasicek short-rate curve per top-20-by-turnover currency, all deliberately exaggerated/uniform for easy eyeballing — exercises every UI feature.',
        'ORESTUDIO',
        'Uniform Volatility Demo theme for the Synthetic data collections bundle',
        current_date,
        'Internal Use Only',
        'synthetic_theme'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

do $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    -- Exaggerated, uniform per-tick stdev (log-return) at 3600 ticks/hour
    -- (1/sec) — deliberately far above any pair's real volatility so
    -- motion is obvious within a short demo session.
    v_stdev constant double precision := 0.0008;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'synthetic.themes.uniform_demo'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.themes.uniform_demo';
    end if;

    if exists (
        select 1 from ores_dq_synthetic_fx_spot_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic FX spot configs (uniform demo) artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic FX spot configs (uniform demo) for dataset: synthetic.themes.uniform_demo';

    insert into ores_dq_synthetic_fx_spot_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled,
        -- auto_start=true on every row: all starter FX configs are
        -- auto-start eligible (enabled under an enabled container),
        -- matching the boot behaviour they had before the column existed.
        auto_start,
        base_currency_code, quote_currency_code,
        gmm_initial_price, ticks_per_hour, process_type,
        price_source, vintage_source, vintage_date
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        -- Name prefix mirrors price_source, same convention as the synthetic IR curve config
        -- populate scripts -- every row here is 'vintage' today.
        'Vintage Synthetic FX Spot (Uniform Demo): ' || p.base || '/' || p.quote,
        'Uniform Volatility Demo synthetic FX spot generator: single-component geometric process, exaggerated volatility.',
        true, true, p.base, p.quote,
        0, 3600, 'geometric',
        'vintage', 'fed.h10.2016-02-05', '2016-02-05'
    from (values
        ('EUR', 'USD'), ('GBP', 'USD'), ('USD', 'CHF'), ('USD', 'JPY'),
        ('USD', 'SEK'), ('AUD', 'USD'), ('USD', 'CAD'), ('NZD', 'USD'),
        ('USD', 'ZAR'), ('USD', 'MXN'), ('USD', 'INR')
    ) as p(base, quote);

    insert into ores_dq_synthetic_gmm_components_artefact_tbl (
        dataset_id, tenant_id, base_currency_code, quote_currency_code,
        component_index, description, mean, stdev, weight
    )
    select
        v_dataset_id, v_tenant_id, p.base, p.quote,
        0, 'Uniform Volatility Demo single-component GMM: exaggerated uniform volatility for visual demo purposes.',
        0.0, v_stdev, 1.0
    from (values
        ('EUR', 'USD'), ('GBP', 'USD'), ('USD', 'CHF'), ('USD', 'JPY'),
        ('USD', 'SEK'), ('AUD', 'USD'), ('USD', 'CAD'), ('NZD', 'USD'),
        ('USD', 'ZAR'), ('USD', 'MXN'), ('USD', 'INR')
    ) as p(base, quote);
end $$;
