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
 * Reporting Publish-from-DQ Functions
 *
 * SECURITY DEFINER functions called by the reporting service NATS handler for the
 * reporting.v1.<entity>.publish-from-dq subjects. Each function reads DQ artefact
 * tables (system tenant) and writes only to ores_reporting_* tables.
 *
 * All functions use SECURITY DEFINER set search_path = public, pg_temp so they
 * execute with the definer's privileges without needing cross-service DML grants.
 */

-- =============================================================================
-- Report Definitions: reporting.v1.ops.publish_report_definitions_from_dq
-- =============================================================================

create or replace function ores_reporting_publish_report_definitions_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_root_party_id uuid;
    v_inserted bigint := 0;
begin
    perform ores_utility_allow_version_replace_fn();
    -- Resolve root party: explicit param or query tenant's operational root.
    v_root_party_id := coalesce(
        (p_params ->> 'party_id')::uuid,
        (select id from ores_refdata_parties_tbl
         where tenant_id = p_target_tenant_id
           and parent_party_id is null
           and party_category <> 'System'
           and valid_to = ores_utility_infinity_timestamp_fn()
         limit 1)
    );

    if v_root_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    -- Idempotency: skip if this party already has report definitions for this dataset
    if exists (
        select 1 from ores_reporting_report_definitions_tbl
        where tenant_id = p_target_tenant_id
          and party_id = v_root_party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    -- Bulk insert report definitions filtered by tier and jurisdiction.
    -- p_params may contain:
    --   tiers:       comma-separated tier list (default: all four)
    --   jurisdiction: ISO 3166 alpha-2 for jurisdiction-specific reports
    --   party_id:    optional explicit root party override
    -- A definition published from a DQ dataset executes every phase: the
    -- dataset describes what to report, not how much of the pipeline to run.
    insert into ores_reporting_report_definitions_tbl (
        tenant_id, id, version, party_id, name,
        description, report_type, schedule_expression, concurrency_policy,
        fsm_state_id, scheduler_job_id,
        pre_processing, prepared_input_key, post_processing,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        gen_random_uuid(), 0, v_root_party_id, a.name,
        coalesce(a.description, ''), a.report_type, a.schedule_expression, a.concurrency_policy,
        null, null,
        'execute', null, 'execute',
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.external_data_import', 'Published from DQ dataset'
    from ores_dq_report_definitions_artefact_tbl a
    where a.dataset_id = p_dataset_id
      and a.tier = any(string_to_array(
          coalesce(nullif(p_params->>'tiers', ''), 'common,regulatory,strategic,trading'), ','))
      and (p_params->>'jurisdiction' is null
           or a.applicable_jurisdiction is null
           or a.applicable_jurisdiction = p_params->>'jurisdiction')
    order by a.display_order, a.name;

    get diagnostics v_inserted = row_count;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Risk Report Configs: reporting.v1.ops.publish_risk_report_configs_from_dq
-- =============================================================================

-- Reads the risk report config artefacts and writes, for each row whose report
-- definition the party already carries, the risk report config the run
-- resolves by definition id plus the root portfolio scope that says which
-- books it reads.
--
-- The four ORE configuration documents a risk run reads -- pricing engines,
-- today's market, the curve configuration and the conventions -- come from
-- `ore import-run`, which is not a DQ dataset. This publish therefore reads
-- the bindings the import wrote and skips a definition that does not carry
-- all four, rather than binding a document the run would fail on.
create or replace function ores_reporting_publish_risk_report_configs_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_required       text[] := array['pricing_engines', 'todays_market',
                                     'curve_configuration', 'conventions'];
    v_root_party_id  uuid;
    v_root_portfolio uuid;
    v_portfolio_ccy  text;
    v_definition_id  uuid;
    v_config         uuid;
    v_inserted       bigint := 0;
    v_skipped        bigint := 0;
    a                record;
begin
    perform ores_utility_allow_version_replace_fn();

    v_root_party_id := coalesce(
        (p_params ->> 'party_id')::uuid,
        (select id from ores_refdata_parties_tbl
         where tenant_id = p_target_tenant_id
           and parent_party_id is null
           and party_category <> 'System'
           and valid_to = ores_utility_infinity_timestamp_fn()
         limit 1)
    );

    if v_root_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    -- The root portfolio is the one with no parent. The run reaches its books
    -- through the config's portfolio scope, and it aggregates to the
    -- portfolio's currency unless the portfolio names none.
    select p.id, p.aggregation_ccy
    into v_root_portfolio, v_portfolio_ccy
    from ores_refdata_portfolios_tbl p
    where p.tenant_id = p_target_tenant_id
      and p.party_id = v_root_party_id
      and p.parent_portfolio_id is null
      and p.valid_to = ores_utility_infinity_timestamp_fn()
    order by p.name
    limit 1;

    if v_root_portfolio is null then
        return query select 'skipped_no_root_portfolio'::text, 0::bigint;
        return;
    end if;

    for a in
        select *
        from ores_dq_risk_report_configs_artefact_tbl
        where dataset_id = p_dataset_id
        order by display_order, report_name
    loop
        -- The config is keyed by report definition id, so the definition must
        -- be published for this party before its configuration can be.
        select d.id into v_definition_id
        from ores_reporting_report_definitions_tbl d
        where d.tenant_id = p_target_tenant_id
          and d.party_id = v_root_party_id
          and d.name = a.report_name
          and d.report_type = 'risk'
          and d.valid_to = ores_utility_infinity_timestamp_fn();

        if v_definition_id is null then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if (
            select count(distinct b.configuration_type_code)
            from ores_reporting_report_configurations_tbl b
            where b.tenant_id = p_target_tenant_id
              and b.report_definition_id = v_definition_id
              and b.configuration_type_code = any(v_required)
              and b.valid_to = ores_utility_infinity_timestamp_fn()
        ) < array_length(v_required, 1) then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if exists (
            select 1 from ores_reporting_risk_report_configs_tbl
            where tenant_id = p_target_tenant_id
              and report_definition_id = v_definition_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            v_skipped := v_skipped + 1;
            continue;
        end if;

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
            p_target_tenant_id, v_config, 0, v_definition_id,
            coalesce(v_portfolio_ccy, a.base_currency),
            a.observation_model, a.n_threads, a.market_data_type,
            a.npv_enabled, a.cashflow_enabled, a.curves_enabled,
            a.sensitivity_enabled, a.simulation_enabled, a.xva_enabled,
            a.stress_enabled, a.parametric_var_enabled,
            a.initial_margin_enabled, a.pfe_enabled,
            a.xva_cva_enabled, a.xva_dva_enabled, a.xva_fva_enabled,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Published from DQ dataset'
        );

        insert into ores_reporting_risk_report_config_portfolios_tbl (
            tenant_id, risk_report_config_id, portfolio_id, valid_from, valid_to
        ) values (
            p_target_tenant_id, v_config, v_root_portfolio,
            clock_timestamp(), ores_utility_infinity_timestamp_fn()
        );

        v_inserted := v_inserted + 1;
    end loop;

    return query select 'inserted'::text, v_inserted where v_inserted > 0;
    return query select 'skipped'::text, v_skipped where v_skipped > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_reporting_publish_risk_report_configs_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the risk report config and its root portfolio scope for every
 artefact row whose report definition the target party already carries with all
 four required ORE configuration bindings. Idempotent per report definition.';
