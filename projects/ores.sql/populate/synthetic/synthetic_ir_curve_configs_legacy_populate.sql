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
 * Synthetic IR Curve Config Seed Population Script — Legacy (IBOR-era)
 *
 * Registers the synthetic.ir_curve_configs.legacy dataset: one Vasicek
 * short-rate curve per major currency, referencing the *IBOR-era* index
 * that currency's basic/realistic datasets have since moved on from --
 * USD-LIBOR-3M, EUR-EURIBOR-3M, GBP-LIBOR-6M, JPY-LIBOR-6M (the four
 * legacy floating_index_type rows already in the catalog; GBP/JPY have
 * no 3M LIBOR entry seeded, only 6M, so 6M is used for both).
 *
 * This is the concrete answer to the pre-2019/post-2019 curve-regime
 * question this story's design settled: rather than force RFR index
 * names (SOFR/€STR/SONIA) onto a period before they existed (SOFR: Apr
 * 2018; €STR: Oct 2019; reformed SONIA: Apr 2018), a *separate*,
 * period-correct dataset carries the indices that were actually quoted
 * pre-cessation, coexisting with (not replacing) basic/realistic's
 * RFR-based curves -- see doc/knowledge/domain/interest_rate_benchmark_types.org
 * for the IBOR/RFR distinction and doc/agile/versions/v0/sprint_24/
 * ir-rates-followups/task_source-vintage-ir-dataset.org for the full
 * design trace.
 *
 * currency_code + index_name differs from basic/realistic for every
 * currency here (e.g. USD/USD-LIBOR-3M vs USD/USD-SOFR), so this
 * dataset's configs never collide on curve_feed_controller's published-
 * qualifier check with either -- they coexist, they don't compete.
 * auto_start = false throughout: legacy curves are for explicit,
 * deliberate use (testing pre-cessation scenarios), never a service's
 * own default running set.
 *
 * As with basic/realistic, initial_rate/theta are plausible fixed
 * values representative of the LIBOR/EURIBOR era's typical levels, not
 * yet sourced from a real dated historical curve with citation --
 * real vintage-observation sourcing (a distinct concept from these
 * config-level model parameters) remains tracked separately.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'synthetic.ir_curve_configs.legacy',
        'Synthetic Market Data',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Synthetic IR Curve Configs: Legacy (IBOR-era)',
        'One Vasicek short-rate curve per major currency, referencing that currency''s legacy IBOR index (USD-LIBOR-3M, EUR-EURIBOR-3M, GBP-LIBOR-6M, JPY-LIBOR-6M) instead of its current RFR -- for testing pre-cessation scenarios; never auto-started.',
        'ORESTUDIO',
        'Legacy archetype for the Synthetic data collections bundle',
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
    -- Plain, real annualised Vasicek parameters, same day-per-tick dt
    -- convention as basic/realistic (handled by process_factory, not
    -- here). kappa/sigma representative of pre-cessation IBOR short-rate
    -- dynamics -- lower vol than the RFR datasets' own calibration,
    -- reflecting the quieter, near-zero-rate environment most of these
    -- indices spent their final years in.
    v_kappa constant double precision := 0.4;
    v_sigma constant double precision := 0.006;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'synthetic.ir_curve_configs.legacy'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: synthetic.ir_curve_configs.legacy';
    end if;

    if exists (
        select 1 from ores_dq_synthetic_ir_curve_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Synthetic IR curve configs (legacy) artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating synthetic IR curve configs (legacy) for dataset: synthetic.ir_curve_configs.legacy';

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
        || c.currency_code || '''s current RFR-based curve in the realistic '
        || 'dataset. Enabled but never auto-started: start it explicitly when '
        || 'you specifically need the legacy regime. Note: like every synthetic '
        || 'curve in this codebase today, this is a single self-discounting '
        || 'curve -- the pre-2008 convention. A real 3M/6M LIBOR curve is '
        || 'properly a projection-only curve paired with a separate OIS '
        || 'discount curve; that split is the still-BACKLOG dual-curve task, '
        || 'not modelled here yet.',
        true, false, c.currency_code, c.index_name, 'VASICEK',
        v_kappa, c.theta, v_sigma, c.theta,
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
        ('USD', 'USD-LIBOR-3M'), ('EUR', 'EUR-EURIBOR-3M'),
        ('GBP', 'GBP-LIBOR-6M'), ('JPY', 'JPY-LIBOR-6M')
    ) as c(currency_code, index_name)
    cross join (values
        (0, 'SPOT', '3M', 'DEPO'),
        (1, '3M', '6M', 'FRA'),
        (2, 'SPOT', '2Y', 'IRS')
    ) as e(sequence_index, start_tenor_code, end_tenor_code, instrument_code);
end $$;
