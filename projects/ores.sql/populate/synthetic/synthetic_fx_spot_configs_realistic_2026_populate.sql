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
 * Synthetic FX Spot Config Seed Population Script — 2026 Realistic
 *
 * Registers the synthetic.fx_spot_configs.realistic_2026 dataset: the
 * same 8 G10 + 3 EM/exotic + 2 Nordic driver pairs as the 2016 ORE
 * Samples theme, but seeded from the real 2026-05-05 Fed H.10 vintage
 * (see marketdata_fx_driver_rates_2026_populate.sql) instead of reusing
 * 2016 data -- this closes the gap noted in
 * synthetic_fx_spot_configs_ore_samples_2016_populate.sql's own
 * description and the 2026 Realistic bundle's previous FX-reuse
 * compromise.
 *
 * Process choice — geometric (GBM), not Ornstein-Uhlenbeck: every pair
 * here remains freely-floating in 2026 (or, for INR, RBI-managed but
 * not pegged/banded), same reasoning as the 2016 ORE Samples theme.
 *
 * Per-pair volatility — each pair gets a 2-component Gaussian mixture: a
 * primary component (weight 0.95) calibrated to that pair's approximate
 * current (2026) annualised realised volatility, and a low-weight tail
 * component (weight 0.05, 4x the primary's stdev) for fat tails. Both
 * components have zero mean. ticks_per_hour = 1800 (once every 2
 * seconds), matching the 2016 ORE Samples theme's own cadence choice.
 * Divisor used to convert each pair's annualised vol into a per-tick
 * stdev:
 *   ticks/year = 1800 * 24 * 365 = 15,768,000; sqrt(ticks/year) ~= 3970.9
 *   per-tick stdev = annualised_vol / 3970.9
 * Annualised vols are approximate, plausible 2026 realised-vol ranges
 * (not fitted to any single data source):
 *   EUR/USD 8.5%, GBP/USD 9.5%, USD/CHF 8.5%, USD/JPY 11.5% (elevated
 *   on persistent BoJ policy-normalisation/intervention risk),
 *   USD/SEK 9.5%, AUD/USD 11.0%, USD/CAD 8.0%, NZD/USD 11.5% (G10
 *   majors); USD/ZAR 17.5%, USD/MXN 14.0%, USD/INR 6.5% (RBI-managed
 *   float, structurally calmer than the other two EM currencies here)
 *   (EM); USD/NOK 10.0% (oil-linked), USD/DKK 8.5% (DKK's tight ERM II
 *   peg to EUR means its USD vol tracks EUR/USD's own) (Scandies).
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
        'synthetic.fx_spot_configs.realistic_2026',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic FX Spot Configs: 2026 Realistic',
        '8 major + 3 EM/exotic + 2 Nordic FX driver pairs, 2-component geometric (GBM) Gaussian mixture per pair, calibrated to plausible 2026 realised FX volatility, seeded from the real 2026-05-05 Fed H.10 vintage.',
        'ORESTUDIO',
        '2026 Realistic theme for the Synthetic data collections bundle',
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
      and code = 'synthetic.fx_spot_configs.realistic_2026'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.fx_spot_configs.realistic_2026';
    end if;

    if exists (
        select 1 from ores_dq_synthetic_fx_spot_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic FX spot configs (2026 realistic) artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic FX spot configs (2026 realistic) for dataset: synthetic.fx_spot_configs.realistic_2026';

    insert into ores_dq_synthetic_fx_spot_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled,
        base_currency_code, quote_currency_code,
        gmm_initial_price, ticks_per_hour, process_type,
        price_source, vintage_source, vintage_date
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Synthetic FX Spot (2026 Realistic): ' || p.base || '/' || p.quote,
        '2026 Realistic synthetic FX spot generator: 2-component geometric Gaussian mixture calibrated to plausible current FX volatility.',
        true, p.base, p.quote,
        0, 1800, 'geometric',
        'vintage', 'fed.h10.2026-05-05', '2026-05-05'
    from (values
        ('EUR', 'USD'), ('GBP', 'USD'), ('USD', 'CHF'), ('USD', 'JPY'),
        ('USD', 'SEK'), ('AUD', 'USD'), ('USD', 'CAD'), ('NZD', 'USD'),
        ('USD', 'ZAR'), ('USD', 'MXN'), ('USD', 'INR'),
        ('USD', 'NOK'), ('USD', 'DKK')
    ) as p(base, quote);

    -- Primary component: weight 0.95, calibrated per-pair stdev
    -- (annualised vol / 3970.9, per the header comment).
    insert into ores_dq_synthetic_gmm_components_artefact_tbl (
        dataset_id, tenant_id, base_currency_code, quote_currency_code,
        component_index, description, mean, stdev, weight
    )
    select v_dataset_id, v_tenant_id, p.base, p.quote, 0,
        '2026 Realistic primary component: calibrated to ~' || p.annualised_vol_pct || '% annualised realised vol.',
        0.0, p.stdev, 0.95
    from (values
        ('EUR', 'USD', 8.5,  0.0000214),
        ('GBP', 'USD', 9.5,  0.0000239),
        ('USD', 'CHF', 8.5,  0.0000214),
        ('USD', 'JPY', 11.5, 0.0000290),
        ('USD', 'SEK', 9.5,  0.0000239),
        ('AUD', 'USD', 11.0, 0.0000277),
        ('USD', 'CAD', 8.0,  0.0000201),
        ('NZD', 'USD', 11.5, 0.0000290),
        ('USD', 'ZAR', 17.5, 0.0000441),
        ('USD', 'MXN', 14.0, 0.0000353),
        ('USD', 'INR', 6.5,  0.0000164),
        ('USD', 'NOK', 10.0, 0.0000252),
        ('USD', 'DKK', 8.5,  0.0000214)
    ) as p(base, quote, annualised_vol_pct, stdev);

    -- Tail component: weight 0.05, 4x the primary's stdev, adding the fat
    -- tails a single Gaussian understates.
    insert into ores_dq_synthetic_gmm_components_artefact_tbl (
        dataset_id, tenant_id, base_currency_code, quote_currency_code,
        component_index, description, mean, stdev, weight
    )
    select v_dataset_id, v_tenant_id, p.base, p.quote, 1,
        '2026 Realistic tail component: 4x primary stdev, low weight, for fat-tailed jumps.',
        0.0, p.stdev, 0.05
    from (values
        ('EUR', 'USD', 0.0000856),
        ('GBP', 'USD', 0.0000956),
        ('USD', 'CHF', 0.0000856),
        ('USD', 'JPY', 0.0001160),
        ('USD', 'SEK', 0.0000956),
        ('AUD', 'USD', 0.0001108),
        ('USD', 'CAD', 0.0000804),
        ('NZD', 'USD', 0.0001160),
        ('USD', 'ZAR', 0.0001764),
        ('USD', 'MXN', 0.0001412),
        ('USD', 'INR', 0.0000656),
        ('USD', 'NOK', 0.0001008),
        ('USD', 'DKK', 0.0000856)
    ) as p(base, quote, stdev);
end $$;
