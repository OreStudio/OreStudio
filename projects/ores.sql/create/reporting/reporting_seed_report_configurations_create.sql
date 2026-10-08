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

-- =============================================================================
-- Seed the report configurations a party's definitions need
-- =============================================================================
--
-- A report definition is a name and a schedule. The run cannot start until it
-- resolves a risk report config by definition id, and no seed writes one, so
-- every seeded definition is unrunnable.
--
-- Two things a risk run needs are split across two paths, and this file owns
-- only the second:
--
--   1. The run document and the four configuration documents a risk run
--      reads, which are the pricing engines, today's market, the curve
--      configuration and the conventions. `ore import-run` copies them from
--      an ORE input directory and binds each to the definition. That is
--      already built.
--   2. The risk report config the run resolves by definition id, the
--      analytics it turns on, and the scope that says which books it reads.
--      Nothing wrote these. This file does.
--
-- The seeder therefore runs after the import, and it creates a config only for
-- a definition that already carries all four bindings. A config whose
-- definition has no documents would fail later and hide why.
--
-- The scope is the party's root portfolio, the one whose parent is null. The
-- run reaches the books through the config's portfolio scope, and
-- ores_reporting_resolve_book_ids_for_config_fn walks that subtree through
-- ores_trading_get_book_ids_by_portfolio_fn, so no new resolver is needed.
-- -----------------------------------------------------------------------------

-- The analytics a report turns on, keyed by the definition's name. The names
-- are the ones seeded for ore.report_definitions; a name the map does not know
-- turns nothing on, which is the right default for a report that inspects data
-- rather than prices it.
create or replace function ores_reporting_analytic_flags_fn(
    p_report_name text
)
returns jsonb as $$
    select to_jsonb(f) - 'name'
    from (values
        ('Data Quality Completeness',              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('System Health',                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Audit Trail Summary',                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('SA-CCR Exposure',                        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('Leverage Ratio',                         0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('FRTB SA Capital',                        0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Liquidity Coverage',                     0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Large Exposures',                        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('MIFID II Transaction Reporting',         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('CFTC Swap Data Reporting',               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('HKMA Trade Repository',                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Group Consolidated Exposure',            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('Board Risk Dashboard',                   1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0),
        ('Cross-Entity Counterparty Concentration',0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('Intercompany Exposure Matrix',           0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
        ('Model Calibration',                      0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Yield Curves',                           0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('FX Spot Rates',                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Volatility Surfaces',                    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Credit Curves',                          0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('NPV',                                    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Cashflows',                              0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Delta and Gamma',                        0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Vega',                                   0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Bucketed DV01',                          0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Exposure',                               0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0),
        ('CVA/DVA/FVA',                            0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1),
        ('Stressed VaR',                           0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0),
        ('P&L Attribution',                        1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        ('Intraday Risk Monitor',                  1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0),
        ('Headline Position',                      1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    ) as f(name, npv, cashflow, curves, sensitivity, simulation,
           xva, stress, pvar, initial_margin, pfe, cva, dva, fva)
    where f.name = p_report_name;
$$ language sql immutable;

comment on function ores_reporting_analytic_flags_fn(text) is
'Returns the ORE analytics a seeded report turns on, as a JSON object of the
 risk report config enable flags. A name the map does not know returns null.';

-- Creates, for every active report definition of a party that already carries
-- all four configuration bindings, the risk report config the run resolves and
-- its root portfolio scope. Idempotent: a definition that already has a risk
-- report config is left alone.
create or replace function ores_reporting_seed_report_configurations_fn(
    p_tenant_id uuid,
    p_party_id  uuid
)
returns table (
    action       text,
    record_count bigint
) as $$
declare
    v_required       text[] := array['pricing_engines', 'todays_market',
                                     'curve_configuration', 'conventions'];
    v_root_portfolio uuid;
    v_base_currency  text;
    v_flags          jsonb;
    v_config         uuid;
    v_seeded         bigint := 0;
    v_skipped        bigint := 0;
    d                record;
begin
    perform ores_utility_allow_version_replace_fn();

    -- The root portfolio is the one with no parent. Its currency is the
    -- report's base currency; the run needs something to aggregate to.
    select p.id, coalesce(p.aggregation_ccy, 'GBP')
    into v_root_portfolio, v_base_currency
    from ores_refdata_portfolios_tbl p
    where p.tenant_id = p_tenant_id
      and p.party_id = p_party_id
      and p.parent_portfolio_id is null
      and p.valid_to = ores_utility_infinity_timestamp_fn()
    order by p.name
    limit 1;

    if v_root_portfolio is null then
        return query select 'skipped_no_root_portfolio'::text, 0::bigint;
        return;
    end if;

    for d in
        select id, name
        from ores_reporting_report_definitions_tbl
        where tenant_id = p_tenant_id
          and party_id = p_party_id
          and report_type = 'risk'
          and valid_to = ores_utility_infinity_timestamp_fn()
        order by name
    loop
        -- The import has not run for this definition, so it has nothing to
        -- read yet.
        if (
            select count(distinct b.configuration_type_code)
            from ores_reporting_report_configurations_tbl b
            where b.tenant_id = p_tenant_id
              and b.report_definition_id = d.id
              and b.configuration_type_code = any(v_required)
              and b.valid_to = ores_utility_infinity_timestamp_fn()
        ) < array_length(v_required, 1) then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if exists (
            select 1 from ores_reporting_risk_report_configs_tbl
            where tenant_id = p_tenant_id
              and report_definition_id = d.id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            continue;
        end if;

        v_flags := coalesce(ores_reporting_analytic_flags_fn(d.name),
                            '{}'::jsonb);
        v_config := gen_random_uuid();

        insert into ores_reporting_risk_report_configs_tbl (
            tenant_id, id, version, report_definition_id, base_currency,
            observation_model, n_threads, market_data_type,
            npv_enabled, cashflow_enabled, curves_enabled, sensitivity_enabled,
            simulation_enabled, xva_enabled, stress_enabled,
            parametric_var_enabled, initial_margin_enabled, pfe_enabled,
            xva_cva_enabled, xva_dva_enabled, xva_fva_enabled,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_tenant_id, v_config, 0, d.id, v_base_currency,
            'disable', 1, 'eod',
            coalesce((v_flags->>'npv')::integer, 0),
            coalesce((v_flags->>'cashflow')::integer, 0),
            coalesce((v_flags->>'curves')::integer, 0),
            coalesce((v_flags->>'sensitivity')::integer, 0),
            coalesce((v_flags->>'simulation')::integer, 0),
            coalesce((v_flags->>'xva')::integer, 0),
            coalesce((v_flags->>'stress')::integer, 0),
            coalesce((v_flags->>'pvar')::integer, 0),
            coalesce((v_flags->>'initial_margin')::integer, 0),
            coalesce((v_flags->>'pfe')::integer, 0),
            coalesce((v_flags->>'cva')::integer, 0),
            coalesce((v_flags->>'dva')::integer, 0),
            coalesce((v_flags->>'fva')::integer, 0),
            coalesce(ores_iam_current_service_fn(), current_user),
            current_user,
            'system.initial_load',
            'Seeded for the definitions the party carries'
        );

        insert into ores_reporting_risk_report_config_portfolios_tbl (
            tenant_id, risk_report_config_id, portfolio_id, valid_from, valid_to
        ) values (
            p_tenant_id, v_config, v_root_portfolio,
            clock_timestamp(), ores_utility_infinity_timestamp_fn()
        );

        v_seeded := v_seeded + 1;
    end loop;

    return query select 'seeded'::text, v_seeded;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_reporting_seed_report_configurations_fn(uuid, uuid) is
'Creates the risk report config and its root portfolio scope for every active
 report definition of a party that already carries all four required
 configuration bindings. Idempotent per definition.';
