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
 * Synthetic IR Curve Config Seed Population Script — Uniform Volatility Demo
 *
 * Registers the synthetic.ir_curve_configs.uniform_demo dataset: one curve
 * per top-20-by-turnover currency (same currency/index set as
 * synthetic.ir_curve_configs.realistic_2026), each a Vasicek short-rate
 * process with plain, real annualised parameters -- the day-per-tick
 * scaling (ir_curve_template_resolver's "1 tick = 1 day" convention)
 * is handled by process_factory::make_yield_curve_process()'s own dt
 * parameter, not here -- and a simple three-entry Curve Template
 * (Deposit/FRA/Swap) per curve, so every curve_role pricing
 * derivation gets exercised.
 *
 * This is not a vintage theme -- it doesn't pin to any point in market
 * history -- it's a deliberately simple demo/exercise archetype: one
 * uniform kappa/sigma across all 20 curves (only theta/initial_rate vary,
 * by currency) using Vasicek, so every curve's dynamics are easy to
 * reason about while exercising the tick-batch/pricing pipeline. Contrast
 * with the vintage themes (2016 ORE Samples, 2026 Realistic), each of
 * which per-curve-calibrates and (for 2016 ORE Samples) is grounded in a
 * real historical curve.
 *
 * auto_start = false: this dataset shares (currency_code, index_name)
 * pairs with synthetic.ir_curve_configs.realistic_2026 (e.g. USD/USD-SOFR),
 * so starting both at once for the same currency would trip
 * curve_feed_controller's qualifier-collision check. Still enabled = true
 * (available, manually startable) for direct comparison against a vintage
 * theme's own calibration; select this collection explicitly (never from
 * Root, which prompts for a single theme) to start it.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_catalogs_upsert_fn(ores_utility_system_tenant_id_fn(),
        'Synthetic Market Data',
        'Synthetic market data generation configs for parties: GMM-parameterised tick generators seeded with plausible starting values.',
        'OreStudio Development Team'
    );
END $$;

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.ir_curve_configs.uniform_demo',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic IR Curve Configs: Uniform Volatility Demo',
        'One Vasicek short-rate curve per top-20-by-turnover currency, uniform annualised kappa/sigma, three-entry (Deposit/FRA/Swap) Curve Template each.',
        'ORESTUDIO',
        'Uniform Volatility Demo theme for the Synthetic data collections bundle',
        current_date,
        'Internal Use Only',
        'synthetic_ir_curve_configs'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================

do $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    -- Plain, real annualised Vasicek parameters -- day-per-tick scaling
    -- (ir_curve_template_resolver's "1 tick = 1 day" convention) is handled
    -- by process_factory::make_yield_curve_process()'s own dt parameter, not
    -- here (see the "Fix day-scaled kappa/sigma calibration" task: doing
    -- this arithmetic in SQL was untested and, worse, was masking a second,
    -- more serious bug in discount_factor()'s own tick-to-time accounting).
    -- kappa = 0.5 gives a realistic multi-year reversion half-life (~1.4
    -- years); sigma = 0.01 is a 1%-annualised-vol short rate.
    v_kappa constant double precision := 0.5;
    v_sigma constant double precision := 0.01;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'synthetic.ir_curve_configs.uniform_demo'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.ir_curve_configs.uniform_demo';
    end if;

    if exists (
        select 1 from ores_dq_synthetic_ir_curve_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic IR curve configs (uniform demo) artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic IR curve configs (uniform demo) for dataset: synthetic.ir_curve_configs.uniform_demo';

    insert into ores_dq_synthetic_ir_curve_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled, auto_start,
        currency_code, index_name, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Synthetic IR Curve (Uniform Demo): ' || c.currency_code || '/' || c.index_name,
        'Uniform Volatility Demo archetype: a single, uniform Vasicek short-rate process (same '
        || 'kappa/sigma across every currency, only theta/initial_rate vary) applied to '
        || c.currency_code || '''s current risk-free rate, ' || c.index_name || '. '
        || 'Intended for exercising the tick-batch/pricing pipeline with predictable, '
        || 'easy-to-reason-about dynamics -- not for realistic curve shape or '
        || 'per-currency calibration (see synthetic.ir_curve_configs.realistic_2026 for that). '
        || 'Not auto-started by default -- both this and realistic_2026 seed the same '
        || '(currency, index) pairs, so only one may run at a time per currency; enable '
        || 'and start this one explicitly to compare against a vintage theme''s calibration.',
        true, false, c.currency_code, c.index_name, 'VASICEK',
        v_kappa, c.theta, v_sigma, c.theta,
        60, 'Quarterly'
    from (values
        -- currency, index code, theta (mean/initial level) -- same 20 currencies and levels
        -- as synthetic.ir_curve_configs.realistic_2026, but uniform kappa/sigma (v_kappa/v_sigma
        -- above) rather than per-curve calibration.
        ('USD', 'USD-SOFR',      0.0400),
        ('EUR', 'EUR-ESTR',      0.0300),
        ('JPY', 'JPY-TONAR',     0.0025),
        ('GBP', 'GBP-SONIA',     0.0450),
        ('CHF', 'CHF-SARON',     0.0100),
        ('AUD', 'AUD-AONIA',     0.0430),
        ('CAD', 'CAD-CORRA',     0.0350),
        ('CNY', 'CNY-SHIBOR-ON', 0.0180),
        ('HKD', 'HKD-HONIA',     0.0450),
        ('SGD', 'SGD-SORA',      0.0300),
        ('SEK', 'SEK-SWESTR',    0.0250),
        ('NOK', 'NOK-NOWA',      0.0400),
        ('NZD', 'NZD-NZIONA',    0.0400),
        ('KRW', 'KRW-KOFR',      0.0280),
        ('INR', 'INR-MIBOR',     0.0650),
        ('MXN', 'MXN-TIIE-ON',   0.1000),
        ('ZAR', 'ZAR-ZARONIA',   0.0750),
        ('DKK', 'DKK-DESTR',     0.0280),
        ('PLN', 'PLN-POLONIA',   0.0550),
        ('TWD', 'TWD-TAIBOR-ON', 0.0200)
    ) as c(currency_code, index_name, theta);

    insert into ores_dq_synthetic_ir_curve_template_entries_artefact_tbl (
        dataset_id, tenant_id, currency_code, index_name,
        sequence_index, start_tenor_code, end_tenor_code, instrument_code
    )
    select
        v_dataset_id, v_tenant_id, c.currency_code, c.index_name,
        e.sequence_index, e.start_tenor_code, e.end_tenor_code, e.instrument_code
    from (values
        ('USD', 'USD-SOFR'), ('EUR', 'EUR-ESTR'), ('JPY', 'JPY-TONAR'), ('GBP', 'GBP-SONIA'),
        ('CHF', 'CHF-SARON'), ('AUD', 'AUD-AONIA'), ('CAD', 'CAD-CORRA'),
        ('CNY', 'CNY-SHIBOR-ON'), ('HKD', 'HKD-HONIA'), ('SGD', 'SGD-SORA'),
        ('SEK', 'SEK-SWESTR'), ('NOK', 'NOK-NOWA'), ('NZD', 'NZD-NZIONA'),
        ('KRW', 'KRW-KOFR'), ('INR', 'INR-MIBOR'), ('MXN', 'MXN-TIIE-ON'),
        ('ZAR', 'ZAR-ZARONIA'), ('DKK', 'DKK-DESTR'), ('PLN', 'PLN-POLONIA'),
        ('TWD', 'TWD-TAIBOR-ON')
    ) as c(currency_code, index_name)
    cross join (values
        (0, 'SPOT', '3M', 'DEPO'),
        (1, '3M', '6M', 'FRA'),
        (2, 'SPOT', '2Y', 'IRS')
    ) as e(sequence_index, start_tenor_code, end_tenor_code, instrument_code);
end $$;
