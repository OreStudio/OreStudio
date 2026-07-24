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
 * Synthetic IR Curve Config Seed Population Script — 2016 ORE Samples
 *
 * Registers the synthetic.ir_curve_configs.ore_samples_2016 dataset: four
 * legacy IBOR-era curves (USD-LIBOR-3M, EUR-EURIBOR-3M, GBP-LIBOR-6M,
 * JPY-LIBOR-6M), a Vasicek short-rate process each, pinned to the
 * pre-benchmark-reform world ORE's own sample data comes from -- so
 * ORE Samples-derived examples reproduce faithfully against this
 * theme. Vasicek's constant-vol Gaussian dynamics were the
 * conventional choice for this era's near-zero-rate environment,
 * before CIR/RFR-style modelling (see the sibling
 * synthetic_ir_curve_configs_realistic_2026_populate.sql for the
 * modern, RFR/CIR equivalent).
 *
 * This is a wholly separate dataset/collection from the 2026 Realistic
 * theme, not an auto_start=false sibling sharing its dataset --
 * selecting this theme at the collection level (see
 * MarketSimulatorWindow's Start-at-Root theme prompt) and starting it
 * starts every curve here, since a legacy vintage and a current one are
 * mutually exclusive, never layered.
 *
 * kappa/sigma are representative of pre-cessation IBOR short-rate
 * dynamics -- lower vol than the RFR curves' own calibration,
 * reflecting the quieter, near-zero-rate environment most of these
 * indices spent their final years in. Curve templates below mirror how
 * a real 3M/6M LIBOR/EURIBOR curve was actually constructed (deposit +
 * FRA strip bridging to the 2Y swap point + swap strip out to 30Y),
 * not an arbitrary abbreviation. Note: like every synthetic curve in
 * this codebase today, each is a single self-discounting curve -- the
 * pre-2008 convention. A real LIBOR/EURIBOR curve is properly a
 * projection-only curve paired with a separate OIS discount curve;
 * that split is the still-BACKLOG dual-curve task, not modelled here
 * yet. See doc/knowledge/domain/interest_rate_benchmark_types.org for
 * the IBOR/RFR distinction.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.ir_curve_configs.ore_samples_2016',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic IR Curve Configs: 2016 ORE Samples',
        'Four legacy IBOR-era Vasicek short-rate curves (USD-LIBOR-3M, EUR-EURIBOR-3M, GBP-LIBOR-6M, JPY-LIBOR-6M), pinned to the pre-benchmark-reform world ORE''s own sample data comes from.',
        'ORESTUDIO',
        '2016 ORE Samples theme for the Synthetic data collections bundle',
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
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'synthetic.ir_curve_configs.ore_samples_2016'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.ir_curve_configs.ore_samples_2016';
    end if;

    if exists (
        select 1 from ores_dq_synthetic_ir_curve_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic IR curve configs (2016 ORE Samples) artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic IR curve configs (2016 ORE Samples) for dataset: synthetic.ir_curve_configs.ore_samples_2016';

    insert into ores_dq_synthetic_ir_curve_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled, auto_start,
        currency_code, index_name, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Synthetic IR Curve (2016 ORE Samples): ' || c.currency_code || '/' || c.index_name,
        '2016 ORE Samples archetype: a Vasicek short-rate process for ' || c.currency_code
        || '''s discontinued IBOR-era benchmark, ' || c.index_name || ' -- '
        || c.retirement_note
        || ' This is the dataset this theme''s own provisioning publishes and'
        || ' auto-starts by default when the 2016 ORE Samples collection is'
        || ' selected.',
        true, true, c.currency_code, c.index_name, 'VASICEK',
        0.4, c.theta, 0.006, c.theta,
        60, 'Quarterly'
    from (values
        -- currency, legacy index code, theta (representative pre-cessation level), retirement note
        ('USD', 'USD-LIBOR-3M',   0.0025, 'USD LIBOR ceased 30 June 2023 (most tenors); superseded by SOFR.'),
        ('EUR', 'EUR-EURIBOR-3M', 0.0000, 'EURIBOR was never fully retired, unlike LIBOR, but €STR (since Oct 2019) is now EUR''s primary risk-free reference.'),
        ('GBP', 'GBP-LIBOR-6M',   0.0050, 'GBP LIBOR ceased 31 December 2021; superseded by SONIA.'),
        ('JPY', 'JPY-LIBOR-6M',   0.0010, 'JPY LIBOR ceased end 2021; superseded by TONA/TONAR.')
    ) as c(currency_code, index_name, theta, retirement_note);

    -- Legacy curve templates: fuller LIBOR-style construction -- the spot
    -- fixing, an FRA strip bridging to the 2Y swap point (3x6/6x9/9x12 for
    -- 3M-tenor indices, 6x12/12x18 for 6M-tenor indices), then the
    -- standard swap-curve ladder out to 30Y. This is how a real 3M/6M
    -- LIBOR/EURIBOR curve was actually constructed (deposit + FRA strip +
    -- swap strip), not an arbitrary abbreviation.
    insert into ores_dq_synthetic_ir_curve_template_entries_artefact_tbl (
        dataset_id, tenant_id, currency_code, index_name,
        sequence_index, start_tenor_code, end_tenor_code, instrument_code
    )
    select
        v_dataset_id, v_tenant_id, e.currency_code, e.index_name,
        e.sequence_index, e.start_tenor_code, e.end_tenor_code, e.instrument_code
    from (
        select 'USD' currency_code, 'USD-LIBOR-3M' index_name, s.sequence_index,
               s.start_tenor_code, s.end_tenor_code, s.instrument_code
        from (values
            (0, 'SPOT', '3M',  'DEPO'),
            (1, '3M',   '6M',  'FRA'),
            (2, '6M',   '9M',  'FRA'),
            (3, '9M',   '12M', 'FRA'),
            (4, 'SPOT', '2Y',  'IRS'),
            (5, 'SPOT', '3Y',  'IRS'),
            (6, 'SPOT', '5Y',  'IRS'),
            (7, 'SPOT', '7Y',  'IRS'),
            (8, 'SPOT', '10Y', 'IRS'),
            (9, 'SPOT', '15Y', 'IRS'),
            (10, 'SPOT', '20Y', 'IRS'),
            (11, 'SPOT', '30Y', 'IRS')
        ) as s(sequence_index, start_tenor_code, end_tenor_code, instrument_code)
        union all
        select 'EUR', 'EUR-EURIBOR-3M', s.sequence_index,
               s.start_tenor_code, s.end_tenor_code, s.instrument_code
        from (values
            (0, 'SPOT', '3M',  'DEPO'),
            (1, '3M',   '6M',  'FRA'),
            (2, '6M',   '9M',  'FRA'),
            (3, '9M',   '12M', 'FRA'),
            (4, 'SPOT', '2Y',  'IRS'),
            (5, 'SPOT', '3Y',  'IRS'),
            (6, 'SPOT', '5Y',  'IRS'),
            (7, 'SPOT', '7Y',  'IRS'),
            (8, 'SPOT', '10Y', 'IRS'),
            (9, 'SPOT', '15Y', 'IRS'),
            (10, 'SPOT', '20Y', 'IRS'),
            (11, 'SPOT', '30Y', 'IRS')
        ) as s(sequence_index, start_tenor_code, end_tenor_code, instrument_code)
        union all
        select 'GBP', 'GBP-LIBOR-6M', s.sequence_index,
               s.start_tenor_code, s.end_tenor_code, s.instrument_code
        from (values
            (0, 'SPOT', '6M',  'DEPO'),
            (1, '6M',   '12M', 'FRA'),
            (2, '12M',  '18M', 'FRA'),
            (3, 'SPOT', '2Y',  'IRS'),
            (4, 'SPOT', '3Y',  'IRS'),
            (5, 'SPOT', '5Y',  'IRS'),
            (6, 'SPOT', '7Y',  'IRS'),
            (7, 'SPOT', '10Y', 'IRS'),
            (8, 'SPOT', '15Y', 'IRS'),
            (9, 'SPOT', '20Y', 'IRS'),
            (10, 'SPOT', '30Y', 'IRS')
        ) as s(sequence_index, start_tenor_code, end_tenor_code, instrument_code)
        union all
        select 'JPY', 'JPY-LIBOR-6M', s.sequence_index,
               s.start_tenor_code, s.end_tenor_code, s.instrument_code
        from (values
            (0, 'SPOT', '6M',  'DEPO'),
            (1, '6M',   '12M', 'FRA'),
            (2, '12M',  '18M', 'FRA'),
            (3, 'SPOT', '2Y',  'IRS'),
            (4, 'SPOT', '3Y',  'IRS'),
            (5, 'SPOT', '5Y',  'IRS'),
            (6, 'SPOT', '7Y',  'IRS'),
            (7, 'SPOT', '10Y', 'IRS'),
            (8, 'SPOT', '15Y', 'IRS'),
            (9, 'SPOT', '20Y', 'IRS'),
            (10, 'SPOT', '30Y', 'IRS')
        ) as s(sequence_index, start_tenor_code, end_tenor_code, instrument_code)
    ) as e(currency_code, index_name, sequence_index, start_tenor_code, end_tenor_code, instrument_code);
end $$;
