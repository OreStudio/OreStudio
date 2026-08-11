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
 * Synthetic IR Curve Config Seed Population Script — 2026 Realistic
 *
 * Registers the synthetic.ir_curve_configs.realistic_2026 dataset: one
 * overnight-RFR-based curve per each of the top 20 currencies by FX
 * turnover (USD/SOFR, EUR/ESTR, JPY/TONA, GBP/SONIA, CHF/SARON, AUD/
 * AONIA, CAD/CORRA, CNY/SHIBOR, HKD/HONIA, SGD/SORA, SEK/SWESTR,
 * NOK/NOWA, NZD/NZONIA, KRW/KOFR, INR/MIBOR, MXN/TIIE, ZAR/ZARONIA,
 * DKK/DESTR, PLN/POLONIA, TWD/TAIBOR), with per-currency-calibrated
 * Cox-Ingersoll-Ross parameters (distinct reversion speed/vol/level per curve,
 * reflecting each market's own typical short-rate level and
 * volatility -- e.g. JPY near-zero and low-vol, EM currencies
 * (MXN/ZAR/INR/PLN) higher level and higher vol than G10) rather than
 * one uniform set, and a seven-entry Curve Template per
 * curve (short-end deposits through a 10Y swap) so a bootstrapped
 * curve has real shape to show. Cox-Ingersoll-Ross chosen over Vasicek here because
 * volatility scaling with the level and non-negativity by
 * construction are a better fit for short rates, without needing a
 * real reference curve to calibrate against (which Hull-White's own
 * advantage over Vasicek would require, and which isn't available
 * for a purely generative dataset).
 *
 * All indices are overnight RFRs, not IBOR-style term rates -- for the
 * pre-2019, IBOR-era equivalent of this theme, see
 * synthetic_ir_curve_configs_ore_samples_2016_populate.sql, a wholly
 * separate dataset/collection now, not a same-dataset auto_start=false
 * sibling -- the two eras are mutually-exclusive vintages a user picks
 * between at the collection level (see MarketSimulatorWindow's Start-at-
 * Root theme prompt), not layers meant to run side by side.
 *
 * Parameters are plain, real annualised Cox-Ingersoll-Ross values -- day-per-tick
 * scaling (ir_curve_template_resolver's "1 tick = 1 day" convention)
 * is handled by process_factory::make_yield_curve_process()'s own dt
 * parameter, not here (see the "Fix day-scaled kappa/sigma
 * calibration" task: doing this arithmetic in SQL was untested and
 * was masking a second, more serious bug in discount_factor()'s own
 * tick-to-time accounting). "2026 Realistic" here means per-curve
 * calibration approximating today's market, not vintage grounding from
 * a real historical curve; no real historical curve is sourced yet
 * (see the source-vintage-historical-ir-rate-dataset follow-on task --
 * this dataset's own FX companion in the synthetic_realistic_2026
 * bundle, synthetic.fx_spot_configs.realistic_2026, is already grounded
 * in the real 2026-05-05 Fed H.10 vintage; only the IR side remains
 * ungrounded).
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Artefact Seed Data
-- =============================================================================
--
-- The theme dataset itself (synthetic.themes.realistic_2026) is registered by
-- synthetic_fx_spot_configs_realistic_2026_populate.sql, which this script's
-- \ir ordering in synthetic_populate.sql runs after -- this script looks it
-- up by code rather than registering its own dataset, so FX and IR rows
-- share one dataset id, published atomically by
-- ores_synthetic_publish_theme_from_dq_fn. See
-- doc/plans/2026-07-27-synthetic-theme-atomic-dataset-design.org.

do $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'synthetic.themes.realistic_2026'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.themes.realistic_2026';
    end if;

    if exists (
        select 1 from ores_dq_synthetic_ir_curve_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic IR curve configs (2026 realistic) artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic IR curve configs (2026 realistic) for dataset: synthetic.themes.realistic_2026';

    -- Per-currency calibration: plain annualised kappa/sigma, distinct per curve rather than one
    -- uniform set. G10 currencies keep the original tighter reversion/lower vol; EM currencies
    -- (INR/MXN/ZAR/PLN) get slower reversion and higher vol, matching their typically less
    -- liquid, more volatile short-rate markets; JPY keeps its long-standing near-zero level and
    -- low vol; HKD (USD-pegged) and DKK (EUR-pegged) mirror their anchor currency's level.
    insert into ores_dq_synthetic_ir_curve_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled, auto_start,
        currency_code, index_family, tenor, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code,
        price_source, vintage_source, vintage_date
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        -- Name prefix mirrors price_source (see the 2016 ORE Samples populate script's own
        -- comment on this convention) -- every row here is 'fixed' today.
        'Fixed Synthetic IR Curve (2026 Realistic): ' || c.currency_code || '/' || c.currency_code || '-' || upper(c.index_family),
        '2026 Realistic archetype: a per-currency-calibrated Cox-Ingersoll-Ross short-rate process for '
        || c.currency_code || '''s current overnight risk-free rate, ' || c.currency_code || '-' || upper(c.index_family) || '. '
        || 'Cox-Ingersoll-Ross''s volatility-scales-with-level, non-negative-by-construction dynamics are a '
        || 'better fit for short rates than Vasicek''s constant-vol Gaussian, without needing '
        || 'a real reference curve to calibrate Hull-White against. This is the dataset '
        || 'Barclays'' own provisioning flow publishes and auto-starts by default -- the '
        || 'primary, currently-live curve for this currency.',
        true, true, c.currency_code, c.index_family, '', 'COX_INGERSOLL_ROSS',
        c.annual_kappa, c.theta, c.annual_sigma, c.theta,
        60, 'Quarterly',
        'fixed', '', ''
    from (values
        -- currency, index_family, annual kappa, annual sigma, theta (mean/initial level).
        -- Real-benchmark conventions (fixing calendar/day-count/settlement) for each family
        -- are seeded in refdata_overnight_index_conventions_populate.sql /
        -- refdata_ibor_index_conventions_populate.sql, sourced from ORE's own QuantExt/
        -- QuantLib index definitions (see task C80D94BB's Notes).
        ('USD', 'sofr',    0.55, 0.008, 0.0400),
        ('EUR', 'estr',    0.35, 0.010, 0.0300),
        ('JPY', 'tona',    0.20, 0.006, 0.0025),
        ('GBP', 'sonia',   0.25, 0.013, 0.0450),
        ('CHF', 'saron',   0.30, 0.009, 0.0100),
        ('AUD', 'aonia',   0.40, 0.012, 0.0430),
        ('CAD', 'corra',   0.45, 0.011, 0.0350),
        ('CNY', 'shibor',  0.20, 0.006, 0.0180),
        ('HKD', 'honia',   0.50, 0.009, 0.0450),
        ('SGD', 'sora',    0.35, 0.010, 0.0300),
        ('SEK', 'swestr',  0.30, 0.012, 0.0250),
        ('NOK', 'nowa',    0.30, 0.013, 0.0400),
        ('NZD', 'nzonia',  0.35, 0.014, 0.0400),
        ('KRW', 'kofr',    0.30, 0.011, 0.0280),
        ('INR', 'mibor',   0.30, 0.018, 0.0650),
        ('MXN', 'tiie',    0.25, 0.022, 0.1000),
        ('ZAR', 'zaronia', 0.25, 0.020, 0.0750),
        ('DKK', 'destr',   0.35, 0.010, 0.0280),
        ('PLN', 'polonia', 0.28, 0.016, 0.0550),
        ('TWD', 'taibor',  0.30, 0.008, 0.0200)
    ) as c(currency_code, index_family, annual_kappa, annual_sigma, theta);

    insert into ores_dq_synthetic_ir_curve_template_entries_artefact_tbl (
        dataset_id, tenant_id, currency_code, index_family, tenor,
        sequence_index, start_tenor_code, end_tenor_code, instrument_code
    )
    select
        v_dataset_id, v_tenant_id, c.currency_code, c.index_family, '',
        e.sequence_index, e.start_tenor_code, e.end_tenor_code, e.instrument_code
    from (values
        ('USD', 'sofr'), ('EUR', 'estr'), ('JPY', 'tona'), ('GBP', 'sonia'),
        ('CHF', 'saron'), ('AUD', 'aonia'), ('CAD', 'corra'),
        ('CNY', 'shibor'), ('HKD', 'honia'), ('SGD', 'sora'),
        ('SEK', 'swestr'), ('NOK', 'nowa'), ('NZD', 'nzonia'),
        ('KRW', 'kofr'), ('INR', 'mibor'), ('MXN', 'tiie'),
        ('ZAR', 'zaronia'), ('DKK', 'destr'), ('PLN', 'polonia'),
        ('TWD', 'taibor')
    ) as c(currency_code, index_family)
    cross join (values
        (0, 'SPOT', '1M', 'DEPO'),
        (1, 'SPOT', '3M', 'DEPO'),
        (2, '3M', '6M', 'FRA'),
        (3, '6M', '12M', 'FRA'),
        (4, 'SPOT', '2Y', 'IRS'),
        (5, 'SPOT', '5Y', 'IRS'),
        (6, 'SPOT', '10Y', 'IRS')
    ) as e(sequence_index, start_tenor_code, end_tenor_code, instrument_code);

    -- The FOMC-dated short end, USD only: one extra config for the same
    -- (currency, index_family) keyed by tenor 'FOMC', so it publishes its
    -- own distinct series identity, 'USD/SOFR-FOMC' (see ir_curve_qualifier
    -- in ir_curve_template_resolver.cpp) -- the raw grid the bootstrap
    -- config consumes. Only USD has FOMC-dated pillars; the 20-currency
    -- cross-join above must not be extended. The process calibration is the
    -- same per-currency CIR set as the main USD curve row, so the two
    -- segments move consistently.
    insert into ores_dq_synthetic_ir_curve_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        name, description, enabled, auto_start,
        currency_code, index_family, tenor, process_type,
        kappa, theta, sigma, initial_rate,
        ticks_per_hour, fixed_leg_payment_frequency_code,
        price_source, vintage_source, vintage_date
    )
    values (
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        'Fixed Synthetic IR Curve (2026 Realistic): USD/USD-SOFR-FOMC',
        '2026 Realistic archetype: the FOMC-dated short end of USD-SOFR. A Cox-Ingersoll-Ross '
        || 'short-rate process (the same calibration as the main USD curve) quotes the '
        || 'meeting-dated point ids 1F..8F and the 1Y split tenor, publishing into the '
        || 'RATES/YIELD USD/SOFR-FOMC raw grid the bootstrap config consumes; the curve itself '
        || 'is built by the republish chain, not by this feed.',
        true, true, 'USD', 'sofr', 'FOMC', 'COX_INGERSOLL_ROSS',
        0.55, 0.04, 0.008, 0.04,
        60, 'Quarterly',
        'fixed', '', ''
    );

    insert into ores_dq_synthetic_ir_curve_template_entries_artefact_tbl (
        dataset_id, tenant_id, currency_code, index_family, tenor,
        sequence_index, start_tenor_code, end_tenor_code, instrument_code
    )
    select
        v_dataset_id, v_tenant_id, 'USD', 'sofr', 'FOMC',
        e.sequence_index, e.start_tenor_code, e.end_tenor_code, e.instrument_code
    from (values
        (0, 'SPOT', '1F', 'DEPO'),
        (1, '1F', '2F', 'FRA'),
        (2, '2F', '3F', 'FRA'),
        (3, '3F', '4F', 'FRA'),
        (4, '4F', '5F', 'FRA'),
        (5, '5F', '6F', 'FRA'),
        (6, '6F', '7F', 'FRA'),
        (7, '7F', '8F', 'FRA'),
        (8, '8F', '1Y', 'IRS')
    ) as e(sequence_index, start_tenor_code, end_tenor_code, instrument_code);
end $$;
