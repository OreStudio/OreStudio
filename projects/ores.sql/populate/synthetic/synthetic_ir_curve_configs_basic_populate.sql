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
 * Synthetic IR Curve Config Seed Population Script — Basic
 *
 * Registers the synthetic.ir_curve_configs.basic dataset: one curve
 * per top-20-by-turnover currency (same currency/index set as
 * synthetic.ir_curve_configs.realistic), each a Vasicek short-rate
 * process with day-scaled parameters (kappa/sigma calibrated per
 * calendar day, matching ir_curve_template_resolver's "1 tick = 1
 * day" convention -- annual-scale numbers here would produce
 * nonsensical published rates and frozen far-dated points, as
 * happened during this dataset's own manual verification) and a
 * simple three-entry Curve Template (Deposit/FRA/Swap) per curve, so
 * every curve_role pricing derivation gets exercised.
 *
 * Basic vs realistic, by design: basic keeps one uniform kappa/sigma
 * across all 20 curves (only theta/initial_rate vary, by currency)
 * and uses Vasicek -- realistic per-curve-calibrates kappa/sigma and
 * uses CIR (volatility scaling with the level, non-negative by
 * construction). Vasicek's simpler, uniform-vol shape is deliberately
 * the "basic" archetype's point, mirroring the FX basic/realistic
 * split's own single-component-vs-calibrated-mixture distinction.
 *
 * No vintage grounding yet -- initial_rate/theta are plausible fixed
 * values, not sourced from a real historical curve. Contrast with FX's
 * basic/realistic split, both of which seed from the same real vintage
 * and differ only in volatility calibration; IR's own vintage sourcing
 * and basic/realistic split is tracked separately (see the
 * seed-ir-curve-sample-data follow-on task).
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
        'synthetic.ir_curve_configs.basic',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic IR Curve Configs: Basic',
        'One Vasicek short-rate curve per top-20-by-turnover currency, uniform day-scaled kappa/sigma, three-entry (Deposit/FRA/Swap) Curve Template each.',
        'ORESTUDIO',
        'Basic archetype for the Synthetic data collections bundle',
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
    -- Day-scaled Vasicek parameters: kappa/sigma calibrated per calendar day
    -- (ir_curve_template_resolver's "1 tick = 1 day" convention), not per
    -- year. kappa = 0.5/365 gives a realistic multi-year reversion half-life
    -- (~1.4 years); sigma = 0.01/sqrt(365) is an annualised-1%-vol short rate
    -- discretised to daily steps.
    v_kappa constant double precision := 0.5 / 365.0;
    v_sigma constant double precision := 0.01 / sqrt(365.0);
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'synthetic.ir_curve_configs.basic'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.ir_curve_configs.basic';
    end if;

    if exists (
        select 1 from ores_dq_synthetic_ir_curve_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic IR curve configs (basic) artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic IR curve configs (basic) for dataset: synthetic.ir_curve_configs.basic';

    insert into ores_dq_synthetic_ir_curve_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled,
        currency_code, index_name, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Synthetic IR Curve (Basic): ' || c.currency_code || '/' || c.index_name,
        'Basic-archetype synthetic IR curve generator: Vasicek short-rate process, day-scaled parameters.',
        true, c.currency_code, c.index_name, 'VASICEK',
        v_kappa, c.theta, v_sigma, c.theta,
        60, 'Quarterly'
    from (values
        -- currency, index code, theta (mean/initial level) -- same 20 currencies and levels
        -- as synthetic.ir_curve_configs.realistic, but uniform kappa/sigma (v_kappa/v_sigma
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
