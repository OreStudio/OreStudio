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
 * Synthetic FX Spot Config Seed Population Script — Realistic
 *
 * Registers the synthetic.fx_spot_configs.realistic dataset: all 8 G10
 * FX driver pairs, 3 EM/exotic driver pairs (USD/ZAR, USD/MXN,
 * USD/INR -- added for the Cross-Rates Matrix "exotics" tier), and 2
 * more Nordic driver pairs (USD/NOK, USD/DKK -- added for the
 * Cross-Rates Matrix "scandies" tier, alongside the majors' own
 * EUR/USD and USD/SEK), seeded from the same 2016-02-05 Fed H.10
 * vintage as Basic, but calibrated to look like real FX behaviour
 * instead of an exaggerated demo.
 *
 * Process choice — geometric (GBM), not Ornstein-Uhlenbeck: every pair
 * here is freely-floating (or, for INR, RBI-managed but not pegged/
 * banded) with no active peg or central-bank defended band in 2016
 * (CHF's franc floor was abandoned in Jan 2015; none of the others were
 * ever pegged). Floating pairs behave close to a random walk at the
 * tick/intraday horizons this generator operates at — real mean
 * reversion in FX shows up only at multi-year horizons (PPP reversion),
 * far beyond what a tick generator models. OU would misrepresent these
 * pairs; it belongs to a pegged/managed-band pair, none of which are in
 * this dataset.
 *
 * Per-pair volatility — each pair gets a 2-component Gaussian mixture: a
 * primary component (weight 0.95) calibrated to that pair's typical 2016
 * annualised realised volatility, and a low-weight tail component
 * (weight 0.05, 4x the primary's stdev) to add the fat tails real FX
 * returns exhibit that a single Gaussian understates. Both components
 * have zero mean (a random walk has no systematic drift over the
 * generator's horizon). ticks_per_hour = 1800 (once every 2 seconds) —
 * the feed loop always sleeps a full tick period before its *first*
 * publish (no immediate tick on start), so this is also a UX choice, not
 * just a calibration one: once/minute (the original choice) meant a full
 * 60s wait with nothing visible after starting a feed; once every 2s is
 * a plausible streaming-feed cadence while staying responsive. Divisor
 * used to convert each pair's annualised vol into a per-tick stdev:
 *   ticks/year = 1800 * 24 * 365 = 15,768,000; sqrt(ticks/year) ~= 3970.9
 *   per-tick stdev = annualised_vol / 3970.9
 * Annualised vols are approximate, well-known 2016 realised-vol ranges
 * (not fitted to any single data source):
 *   EUR/USD 9.5%, GBP/USD 11.5% (elevated ahead of the Brexit
 *   referendum), USD/CHF 9.0%, USD/JPY 10.0%, USD/SEK 10.5%,
 *   AUD/USD 12.0%, USD/CAD 9.5%, NZD/USD 12.5% (G10 majors);
 *   USD/ZAR 20% (post-"Nenegate" 2015/16 volatility), USD/MXN 17%
 *   (2016 US-election risk premium), USD/INR 7.5% (RBI-managed float,
 *   structurally calmer than the other two EM currencies here) (EM);
 *   USD/NOK 11.0% (oil-linked, somewhat more volatile than the other
 *   Nordics), USD/DKK 9.5% (DKK's tight ERM II peg to EUR means its USD
 *   vol tracks EUR/USD's own) (Scandies).
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
        'synthetic.fx_spot_configs.realistic',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic FX Spot Configs: Realistic',
        '8 major + 3 EM/exotic FX driver pairs, 2-component geometric (GBM) Gaussian mixture per pair, calibrated to plausible 2016 realised FX volatility.',
        'ORESTUDIO',
        'Realistic archetype for the Synthetic data collections bundle',
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
      and code = 'synthetic.fx_spot_configs.realistic'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.fx_spot_configs.realistic';
    end if;

    if exists (
        select 1 from ores_dq_synthetic_fx_spot_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic FX spot configs (realistic) artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic FX spot configs (realistic) for dataset: synthetic.fx_spot_configs.realistic';

    insert into ores_dq_synthetic_fx_spot_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled,
        base_currency_code, quote_currency_code,
        gmm_initial_price, ticks_per_hour, process_type,
        price_source, vintage_source, vintage_date
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Synthetic FX Spot (Realistic): ' || p.base || '/' || p.quote,
        'Realistic-archetype synthetic FX spot generator: 2-component geometric Gaussian mixture calibrated to plausible real FX volatility.',
        true, p.base, p.quote,
        0, 1800, 'geometric',
        'vintage', 'fed.h10.2016-02-05', '2016-02-05'
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
        'Realistic primary component: calibrated to ~' || p.annualised_vol_pct || '% annualised realised vol.',
        0.0, p.stdev, 0.95
    from (values
        ('EUR', 'USD', 9.5,  0.0000239),
        ('GBP', 'USD', 11.5, 0.0000290),
        ('USD', 'CHF', 9.0,  0.0000227),
        ('USD', 'JPY', 10.0, 0.0000252),
        ('USD', 'SEK', 10.5, 0.0000264),
        ('AUD', 'USD', 12.0, 0.0000302),
        ('USD', 'CAD', 9.5,  0.0000239),
        ('NZD', 'USD', 12.5, 0.0000315),
        ('USD', 'ZAR', 20.0, 0.0000504),
        ('USD', 'MXN', 17.0, 0.0000428),
        ('USD', 'INR', 7.5,  0.0000189),
        ('USD', 'NOK', 11.0, 0.0000277),
        ('USD', 'DKK', 9.5,  0.0000239)
    ) as p(base, quote, annualised_vol_pct, stdev);

    -- Tail component: weight 0.05, 4x the primary's stdev, adding the fat
    -- tails a single Gaussian understates.
    insert into ores_dq_synthetic_gmm_components_artefact_tbl (
        dataset_id, tenant_id, base_currency_code, quote_currency_code,
        component_index, description, mean, stdev, weight
    )
    select v_dataset_id, v_tenant_id, p.base, p.quote, 1,
        'Realistic tail component: 4x primary stdev, low weight, for fat-tailed jumps.',
        0.0, p.stdev, 0.05
    from (values
        ('EUR', 'USD', 0.0000956),
        ('GBP', 'USD', 0.0001160),
        ('USD', 'CHF', 0.0000908),
        ('USD', 'JPY', 0.0001008),
        ('USD', 'SEK', 0.0001056),
        ('AUD', 'USD', 0.0001208),
        ('USD', 'CAD', 0.0000956),
        ('NZD', 'USD', 0.0001260),
        ('USD', 'ZAR', 0.0002015),
        ('USD', 'MXN', 0.0001712),
        ('USD', 'INR', 0.0000755),
        ('USD', 'NOK', 0.0001108),
        ('USD', 'DKK', 0.0000956)
    ) as p(base, quote, stdev);
end $$;
