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
 * Registers the synthetic.ir_curve_configs.realistic dataset: one
 * overnight-RFR-based curve per each of the top 20 currencies by FX
 * turnover (USD/SOFR, EUR/ESTR, JPY/TONAR, GBP/SONIA, CHF/SARON, AUD/
 * AONIA, CAD/CORRA, CNY/SHIBOR-ON, HKD/HONIA, SGD/SORA, SEK/SWESTR,
 * NOK/NOWA, NZD/NZIONA, KRW/KOFR, INR/MIBOR, MXN/TIIE-ON, ZAR/ZARONIA,
 * DKK/DESTR, PLN/POLONIA, TWD/TAIBOR-ON), with per-currency-calibrated
 * CIR parameters (distinct reversion speed/vol/level per curve,
 * reflecting each market's own typical short-rate level and
 * volatility -- e.g. JPY near-zero and low-vol, EM currencies
 * (MXN/ZAR/INR/PLN) higher level and higher vol than G10) rather than
 * one uniform set, and a seven-entry Curve Template per
 * curve (short-end deposits through a 10Y swap) so a bootstrapped
 * curve has real shape to show. CIR chosen over Vasicek here because
 * volatility scaling with the level and non-negativity by
 * construction are a better fit for short rates, without needing a
 * real reference curve to calibrate against (which Hull-White's own
 * advantage over Vasicek would require, and which isn't available
 * for a purely generative dataset).
 *
 * All indices are overnight RFRs, not IBOR-style term rates -- see
 * this story's dual-curve-discounting-projection-model follow-on task
 * for why that distinction matters for single-curve modeling (an OIS
 * swap referencing an overnight index is genuinely single-curve/
 * self-discounting in real markets; an IBOR-referencing swap is not).
 *
 * Parameters are plain, real annualised CIR values -- day-per-tick
 * scaling (ir_curve_template_resolver's "1 tick = 1 day" convention)
 * is handled by process_factory::make_yield_curve_process()'s own dt
 * parameter, not here (see the "Fix day-scaled kappa/sigma
 * calibration" task: doing this arithmetic in SQL was untested and
 * was masking a second, more serious bug in discount_factor()'s own
 * tick-to-time accounting). "Realistic" here means per-curve
 * calibration, not vintage grounding; no real historical curve is
 * sourced yet (see the seed-ir-curve-sample-data follow-on task for
 * that).
 *
 * Also carries four legacy IBOR-era curves (USD-LIBOR-3M, EUR-EURIBOR-3M,
 * GBP-LIBOR-6M, JPY-LIBOR-6M) in the SAME dataset/folder/collection as
 * the RFR curves above -- there is no separate "legacy" collection.
 * Nothing in the schema distinguishes a legacy IBOR curve from a normal
 * one structurally (there's no retired/deprecated flag on
 * floating_index_type); it's purely a documentation convention carried
 * in each config's own description. currency_code+index_name differs
 * from every RFR row here (e.g. USD/USD-LIBOR-3M vs USD/USD-SOFR), so
 * these configs never collide on curve_feed_controller's published-
 * qualifier check -- they coexist, they don't compete. auto_start=false
 * throughout: legacy curves are for explicit, deliberate use (testing
 * pre-cessation scenarios), never a service's own default running set.
 * See doc/knowledge/domain/interest_rate_benchmark_types.org for the
 * IBOR/RFR distinction.
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
        'One overnight-RFR CIR short-rate curve per top-20-by-turnover currency, per-curve-calibrated annualised parameters, seven-entry (short deposits through 10Y swap) Curve Template each -- plus four legacy IBOR-era Vasicek curves (USD-LIBOR-3M, EUR-EURIBOR-3M, GBP-LIBOR-6M, JPY-LIBOR-6M) for pre-cessation testing, never auto-started.',
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

    -- Per-currency calibration: plain annualised kappa/sigma, distinct per curve rather than one
    -- uniform set. G10 currencies keep the original tighter reversion/lower vol; EM currencies
    -- (INR/MXN/ZAR/PLN) get slower reversion and higher vol, matching their typically less
    -- liquid, more volatile short-rate markets; JPY keeps its long-standing near-zero level and
    -- low vol; HKD (USD-pegged) and DKK (EUR-pegged) mirror their anchor currency's level.
    insert into ores_dq_synthetic_ir_curve_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled, auto_start,
        currency_code, index_name, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Synthetic IR Curve (Realistic): ' || c.currency_code || '/' || c.index_name,
        'Realistic archetype: a per-currency-calibrated CIR short-rate process for '
        || c.currency_code || '''s current overnight risk-free rate, ' || c.index_name || '. '
        || 'CIR''s volatility-scales-with-level, non-negative-by-construction dynamics are a '
        || 'better fit for short rates than Vasicek''s constant-vol Gaussian, without needing '
        || 'a real reference curve to calibrate Hull-White against. This is the dataset '
        || 'Barclays'' own provisioning flow publishes and auto-starts by default -- the '
        || 'primary, currently-live curve for this currency; see the sibling basic dataset '
        || 'for a simpler, uniform-parameter comparison, kept enabled but not auto-started '
        || 'to avoid both claiming the same market data key at once.',
        true, true, c.currency_code, c.index_name, 'CIR',
        c.annual_kappa, c.theta, c.annual_sigma, c.theta,
        60, 'Quarterly'
    from (values
        -- currency, index code, annual kappa, annual sigma, theta (mean/initial level)
        ('USD', 'USD-SOFR',      0.55, 0.008, 0.0400),
        ('EUR', 'EUR-ESTR',      0.35, 0.010, 0.0300),
        ('JPY', 'JPY-TONAR',     0.20, 0.006, 0.0025),
        ('GBP', 'GBP-SONIA',     0.25, 0.013, 0.0450),
        ('CHF', 'CHF-SARON',     0.30, 0.009, 0.0100),
        ('AUD', 'AUD-AONIA',     0.40, 0.012, 0.0430),
        ('CAD', 'CAD-CORRA',     0.45, 0.011, 0.0350),
        ('CNY', 'CNY-SHIBOR-ON', 0.20, 0.006, 0.0180),
        ('HKD', 'HKD-HONIA',     0.50, 0.009, 0.0450),
        ('SGD', 'SGD-SORA',      0.35, 0.010, 0.0300),
        ('SEK', 'SEK-SWESTR',    0.30, 0.012, 0.0250),
        ('NOK', 'NOK-NOWA',      0.30, 0.013, 0.0400),
        ('NZD', 'NZD-NZIONA',    0.35, 0.014, 0.0400),
        ('KRW', 'KRW-KOFR',      0.30, 0.011, 0.0280),
        ('INR', 'INR-MIBOR',     0.30, 0.018, 0.0650),
        ('MXN', 'MXN-TIIE-ON',   0.25, 0.022, 0.1000),
        ('ZAR', 'ZAR-ZARONIA',   0.25, 0.020, 0.0750),
        ('DKK', 'DKK-DESTR',     0.35, 0.010, 0.0280),
        ('PLN', 'PLN-POLONIA',   0.28, 0.016, 0.0550),
        ('TWD', 'TWD-TAIBOR-ON', 0.30, 0.008, 0.0200)
    ) as c(currency_code, index_name, annual_kappa, annual_sigma, theta);

    -- Legacy IBOR-era curves -- same dataset/folder as the RFR curves
    -- above, distinguished from them only by currency_code+index_name
    -- (never colliding on curve_feed_controller's qualifier check) and
    -- by auto_start=false. kappa/sigma representative of pre-cessation
    -- IBOR short-rate dynamics -- lower vol than the RFR curves' own
    -- calibration, reflecting the quieter, near-zero-rate environment
    -- most of these indices spent their final years in.
    insert into ores_dq_synthetic_ir_curve_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled, auto_start,
        currency_code, index_name, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Synthetic IR Curve (Legacy): ' || c.currency_code || '/' || c.index_name,
        'Legacy archetype: a Vasicek short-rate process for ' || c.currency_code
        || '''s discontinued IBOR-era benchmark, ' || c.index_name || ' -- '
        || c.retirement_note
        || ' Offered for testing pre-cessation scenarios (e.g. legacy trade '
        || 'valuation, curve-migration tooling) alongside, not instead of, '
        || c.currency_code || '''s current RFR-based curve above. Enabled '
        || 'but never auto-started: start it explicitly when you specifically '
        || 'need the legacy regime. Note: like every synthetic curve in this '
        || 'codebase today, this is a single self-discounting curve -- the '
        || 'pre-2008 convention. A real LIBOR/EURIBOR curve is properly a '
        || 'projection-only curve paired with a separate OIS discount curve; '
        || 'that split is the still-BACKLOG dual-curve task, not modelled '
        || 'here yet.',
        true, false, c.currency_code, c.index_name, 'VASICEK',
        0.4, c.theta, 0.006, c.theta,
        60, 'Quarterly'
    from (values
        -- currency, legacy index code, theta (representative pre-cessation level), retirement note
        ('USD', 'USD-LIBOR-3M',   0.0025, 'USD LIBOR ceased 30 June 2023 (most tenors); superseded by SOFR.'),
        ('EUR', 'EUR-EURIBOR-3M', 0.0000, 'EURIBOR was never fully retired, unlike LIBOR, but €STR (since Oct 2019) is now EUR''s primary risk-free reference.'),
        ('GBP', 'GBP-LIBOR-6M',   0.0050, 'GBP LIBOR ceased 31 December 2021; superseded by SONIA.'),
        ('JPY', 'JPY-LIBOR-6M',   0.0010, 'JPY LIBOR ceased end 2021; superseded by TONA/TONAR.')
    ) as c(currency_code, index_name, theta, retirement_note);

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
        (0, 'SPOT', '1M', 'DEPO'),
        (1, 'SPOT', '3M', 'DEPO'),
        (2, '3M', '6M', 'FRA'),
        (3, '6M', '12M', 'FRA'),
        (4, 'SPOT', '2Y', 'IRS'),
        (5, 'SPOT', '5Y', 'IRS'),
        (6, 'SPOT', '10Y', 'IRS')
    ) as e(sequence_index, start_tenor_code, end_tenor_code, instrument_code);

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
