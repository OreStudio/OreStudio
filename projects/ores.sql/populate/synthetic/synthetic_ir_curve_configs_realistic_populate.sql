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
 * Synthetic IR Curve Config Seed Population Script — Realistic
 *
 * Registers the synthetic.ir_curve_configs.realistic dataset: the same
 * three currency/index curves as the basic dataset (USD/SOFR,
 * EUR/ESTR, GBP/SONIA), but with per-currency-calibrated Vasicek
 * parameters (distinct reversion speed/vol/level per curve, reflecting
 * each market's own typical short-rate behaviour) rather than one
 * uniform day-scaled set, and a longer seven-entry Curve Template per
 * curve (short-end deposits through a 10Y swap) instead of basic's
 * three, so a bootstrapped curve has real shape to show.
 *
 * All parameters remain day-scaled (kappa/sigma calibrated per
 * calendar day, matching ir_curve_template_resolver's "1 tick = 1
 * day" convention) -- "realistic" here means per-curve calibration,
 * not vintage grounding; no real historical curve is sourced yet (see
 * the seed-ir-curve-sample-data follow-on task for that).
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.ir_curve_configs.realistic',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic IR Curve Configs: Realistic',
        'USD/SOFR, EUR/ESTR, GBP/SONIA Vasicek short-rate curves, per-curve-calibrated day-scaled parameters, seven-entry (short deposits through 10Y swap) Curve Template each.',
        'ORESTUDIO',
        'Realistic archetype for the Synthetic data collections bundle',
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
    v_days constant double precision := 365.0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'synthetic.ir_curve_configs.realistic'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.ir_curve_configs.realistic';
    end if;

    if exists (
        select 1 from ores_dq_synthetic_ir_curve_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic IR curve configs (realistic) artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic IR curve configs (realistic) for dataset: synthetic.ir_curve_configs.realistic';

    -- Per-currency calibration: annual kappa/sigma divided by v_days to
    -- day-scale them, distinct per curve rather than one uniform set --
    -- USD/SOFR reverts fastest with the lowest vol (deepest, most liquid
    -- market); GBP/SONIA the slowest-reverting and most volatile of the
    -- three (smaller, less liquid market), EUR/ESTR in between.
    insert into ores_dq_synthetic_ir_curve_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled,
        currency_code, index_name, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Synthetic IR Curve (Realistic): ' || c.currency_code || '/' || c.index_name,
        'Realistic-archetype synthetic IR curve generator: Vasicek short-rate process, per-curve-calibrated day-scaled parameters.',
        true, c.currency_code, c.index_name, 'VASICEK',
        c.annual_kappa / v_days, c.theta, c.annual_sigma / sqrt(v_days), c.theta,
        60, 'Quarterly'
    from (values
        ('USD', 'USD-SOFR', 0.55, 0.008, 0.04),
        ('EUR', 'EUR-ESTR', 0.35, 0.010, 0.03),
        ('GBP', 'GBP-SONIA', 0.25, 0.013, 0.045)
    ) as c(currency_code, index_name, annual_kappa, annual_sigma, theta);

    insert into ores_dq_synthetic_ir_curve_template_entries_artefact_tbl (
        dataset_id, tenant_id, currency_code, index_name,
        sequence_index, start_tenor_code, end_tenor_code, instrument_code
    )
    select
        v_dataset_id, v_tenant_id, c.currency_code, c.index_name,
        e.sequence_index, e.start_tenor_code, e.end_tenor_code, e.instrument_code
    from (values ('USD', 'USD-SOFR'), ('EUR', 'EUR-ESTR'), ('GBP', 'GBP-SONIA')) as c(currency_code, index_name)
    cross join (values
        (0, 'SPOT', '1M', 'DEPO'),
        (1, 'SPOT', '3M', 'DEPO'),
        (2, '3M', '6M', 'FRA'),
        (3, '6M', '12M', 'FRA'),
        (4, 'SPOT', '2Y', 'IRS'),
        (5, 'SPOT', '5Y', 'IRS'),
        (6, 'SPOT', '10Y', 'IRS')
    ) as e(sequence_index, start_tenor_code, end_tenor_code, instrument_code);
end $$;
