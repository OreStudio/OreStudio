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
 * Risk Report Config Seed Population Script
 *
 * Registers the ore.risk_report_configs dataset and stages the run settings
 * each seeded report definition resolves.
 *
 * A report definition is a name and a schedule. The run resolves a risk report
 * config by definition id before it reads anything else, and until this dataset
 * is published no seeded definition is runnable.
 *
 * 27 of the 31 seeded definitions have every ORE document their run reads. The
 * other four carry a reading that reaches a configuration type nothing can
 * store yet -- historical_return for Board Risk Dashboard, Stressed VaR and
 * Intraday Risk Monitor, basel_traffic_light for P&L Attribution -- so their
 * rows are absent here. Add them when those two types gain a saver.
 *
 * The four documents every risk run needs -- pricing engines, today's market,
 * the curve configuration and the conventions -- come from `ore import-run`,
 * which is not a DQ dataset. The publish reads the bindings the import wrote
 * and skips a definition that does not carry all four, so a missing import
 * leaves a definition without a config rather than a config that fails later.
 *
 * Execution order within reporting_populate.sql: report_definitions (this
 * file's dependency), then this file.
 *
 * This script is idempotent.
 */

-- =============================================================================
-- Dataset Registration
-- =============================================================================

-- --- ORE Analytics: Risk Report Configs Dataset ---

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.risk_report_configs',
        'ORE Analytics',
        'Trading',
        'Reference Data',
        'NONE',
        'Primary',
        'Synthetic',
        'Raw',
        'OreStudio Code Generation Methodology',
        'ORE Analytics Risk Report Configs',
        'The run settings the seeded ORE report definitions resolve: analytics flags, base currency, observation model, threading and market data type. Published per party, scoped to the party root portfolio, and gated on the definition carrying all four ORE configuration documents the run reads.',
        'ORESTUDIO',
        'Seed data for party provisioning report setup',
        current_date,
        'Internal Use Only',
        'risk_report_configs'
    );
END $$;

-- =============================================================================
-- Artefact Seed Data — one row per runnable report
-- =============================================================================

do $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'ore.risk_report_configs'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: ore.risk_report_configs';
    end if;

    if exists (
        select 1 from ores_dq_risk_report_configs_artefact_tbl
        where dataset_id = v_dataset_id
    ) then
        raise debug 'Risk report config artefact already populated for dataset %', v_dataset_id;
        return;
    end if;

    raise debug 'Populating risk report configs for dataset: ore.risk_report_configs';

    -- The analytics come from ores_reporting_analytic_flags_fn, the map's only
    -- author, so the flags cannot drift between the two.
    insert into ores_dq_risk_report_configs_artefact_tbl (
        dataset_id, tenant_id, id, version,
        report_name, base_currency, observation_model, n_threads, market_data_type,
        npv_enabled, cashflow_enabled, curves_enabled, sensitivity_enabled,
        simulation_enabled, xva_enabled, stress_enabled, parametric_var_enabled,
        initial_margin_enabled, pfe_enabled, xva_cva_enabled, xva_dva_enabled,
        xva_fva_enabled, display_order
    )
    select
        v_dataset_id, v_tenant_id, gen_random_uuid(), 1,
        r.report_name, 'GBP', 'disable', 1, 'eod',
        coalesce((f.flags->>'npv')::integer, 0),
        coalesce((f.flags->>'cashflow')::integer, 0),
        coalesce((f.flags->>'curves')::integer, 0),
        coalesce((f.flags->>'sensitivity')::integer, 0),
        coalesce((f.flags->>'simulation')::integer, 0),
        coalesce((f.flags->>'xva')::integer, 0),
        coalesce((f.flags->>'stress')::integer, 0),
        coalesce((f.flags->>'pvar')::integer, 0),
        coalesce((f.flags->>'initial_margin')::integer, 0),
        coalesce((f.flags->>'pfe')::integer, 0),
        coalesce((f.flags->>'cva')::integer, 0),
        coalesce((f.flags->>'dva')::integer, 0),
        coalesce((f.flags->>'fva')::integer, 0),
        r.display_order
    from (values
        -- Common
        ('Data Quality Completeness', 5),
        ('System Health', 10),
        ('Audit Trail Summary', 15),
        -- Regulatory
        ('SA-CCR Exposure', 20),
        ('Leverage Ratio', 25),
        ('FRTB SA Capital', 30),
        ('Liquidity Coverage', 35),
        ('Large Exposures', 40),
        ('MIFID II Transaction Reporting', 45),
        ('CFTC Swap Data Reporting', 50),
        ('HKMA Trade Repository', 55),
        -- Strategic
        ('Group Consolidated Exposure', 60),
        ('Cross-Entity Counterparty Concentration', 70),
        ('Intercompany Exposure Matrix', 75),
        -- Trading
        ('Model Calibration', 80),
        ('Yield Curves', 85),
        ('FX Spot Rates', 90),
        ('Volatility Surfaces', 95),
        ('Credit Curves', 100),
        ('NPV', 105),
        ('Cashflows', 110),
        ('Delta and Gamma', 115),
        ('Vega', 120),
        ('Bucketed DV01', 125),
        ('Exposure', 130),
        ('CVA/DVA/FVA', 135),
        ('Headline Position', 155)
    ) as r(report_name, display_order)
    cross join lateral (
        select ores_reporting_analytic_flags_fn(r.report_name) as flags
    ) f;

    raise debug 'Inserted risk report config artefacts for dataset: ore.risk_report_configs';
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Risk Report Config Summary ---'

select 'DQ: Risk Report Config Artefacts' as entity, count(*) as count
from ores_dq_risk_report_configs_artefact_tbl
where dataset_id = (
    select id from ores_dq_datasets_tbl
    where code = 'ore.risk_report_configs'
      and valid_to = ores_utility_infinity_timestamp_fn()
);
