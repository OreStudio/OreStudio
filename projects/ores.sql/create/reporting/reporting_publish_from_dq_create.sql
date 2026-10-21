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
    v_bound bigint := 0;
    v_staged_bindings bigint := 0;
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
        pre_processing, prepared_input_key, post_processing, regulatory_book_type,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        gen_random_uuid(), 0, v_root_party_id, a.name,
        coalesce(a.description, ''), a.report_type, a.schedule_expression, a.concurrency_policy,
        null, null,
        'execute', null, 'execute', a.regulatory_book_type,
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

    -- The configuration documents each definition binds. A dataset that stages
    -- bindings names the definition and the configuration, and both resolve for
    -- this party, so the documents are published before the definitions.
    -- Conventions have no document of their own: a binding to them means the
    -- conventions the party sees. Their configuration is a header that says so, and
    -- the first binding to one creates it.
    insert into ores_reporting_configurations_tbl (
        tenant_id, id, version, party_id, name, configuration_type_code, owning_component,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select distinct
        p_target_tenant_id, ores_dq_party_uuid_fn(v_root_party_id,
            md5('conventions:' || b.configuration_name)::uuid),
        0, v_root_party_id, b.configuration_name, b.configuration_type_code, 'ores.refdata',
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.external_data_import', 'Published from DQ dataset'
    from ores_dq_report_configurations_artefact_tbl b
    where b.dataset_id = p_dataset_id
      and b.configuration_type_code = 'conventions'
      and not exists (
        select 1 from ores_reporting_configurations_tbl c
        where c.tenant_id = p_target_tenant_id and c.party_id = v_root_party_id
          and c.configuration_type_code = b.configuration_type_code
          and c.name = b.configuration_name
          and c.valid_to = ores_utility_infinity_timestamp_fn());

    insert into ores_reporting_report_configurations_tbl (
        tenant_id, id, version, party_id, report_definition_id, configuration_type_code,
        configuration_id, modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id, gen_random_uuid(), 0, v_root_party_id, d.id,
        b.configuration_type_code, c.id,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.external_data_import', 'Published from DQ dataset'
    from ores_dq_report_configurations_artefact_tbl b
    join ores_reporting_report_definitions_tbl d
      on d.tenant_id = p_target_tenant_id and d.party_id = v_root_party_id
     and d.name = b.report_name and d.valid_to = ores_utility_infinity_timestamp_fn()
    join ores_reporting_configurations_tbl c
      on c.tenant_id = p_target_tenant_id and c.party_id = v_root_party_id
     and c.configuration_type_code = b.configuration_type_code
     and c.name = b.configuration_name and c.valid_to = ores_utility_infinity_timestamp_fn()
    where b.dataset_id = p_dataset_id;

    get diagnostics v_bound = row_count;

    select count(*) into v_staged_bindings
    from ores_dq_report_configurations_artefact_tbl
    where dataset_id = p_dataset_id;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'bound'::text, v_bound
    where v_bound > 0
    union all select 'skipped'::text, v_staged_bindings - v_bound
    where v_staged_bindings - v_bound > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Risk Report Configs: reporting.v1.ops.publish_risk_report_configs_from_dq
-- =============================================================================

-- Publishes the risk report configurations a dataset stages for the target
-- party, each with its analytics and their parameters. The copy, the re-keying and
-- the reporting configuration each one is registered as are the shared
-- ores_dq_publish_configuration_document_fn. A staged parameter names its parameter,
-- and the analytic type comes from the staged analytic it points at, so the
-- publish resolves the pair to the definition of the target database. A
-- definition binds a configuration by name, as it binds its pricing engines, so
-- this publish needs no definition and many definitions can share one row.
--
-- A staged row may name a scope portfolio. The portfolio is resolved by name for
-- the target party and written as the configuration's portfolio scope. A scope
-- that names a portfolio the party does not hold is refused: the configuration
-- would read no books, and every run of it would fail.
create or replace function ores_reporting_publish_risk_report_configs_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (action text, record_count bigint) as $$
declare
    v_party_id uuid;
    v_missing  text;
    v_problems text;
    v_scoped   bigint;
    r          record;
begin
    v_party_id := ores_refdata_publish_target_party_fn(p_target_tenant_id, p_params);
    if v_party_id is null then
        return query select 'skipped_no_party'::text, 0::bigint;
        return;
    end if;

    select string_agg(distinct a.scope_portfolio, ', ') into v_missing
    from ores_dq_risk_report_configs_artefact_tbl a
    where a.dataset_id = p_dataset_id
      and a.scope_portfolio is not null
      and not exists (
          select 1 from ores_refdata_portfolios_tbl pf
          where pf.tenant_id = p_target_tenant_id
            and pf.party_id = v_party_id
            and pf.name = a.scope_portfolio
            and pf.valid_to = ores_utility_infinity_timestamp_fn());
    if v_missing is not null then
        raise exception 'Dataset % scopes risk report configurations to portfolios the party does not hold: %.',
            p_dataset_id, v_missing;
    end if;

    for r in
        select * from ores_dq_publish_configuration_document_fn(
            p_dataset_id, p_target_tenant_id, v_party_id,
            'risk_report_config', 'ores.reporting', 'ores_reporting_',
            array['risk_report_configs', 'configuration_analytics',
                  'configuration_analytic_parameters', 'configuration_market_bindings'],
            array[]::text[],
            jsonb_build_object('configuration_analytic_parameters', jsonb_build_object(
                'parameter_definition_id',
                'select d.id from ores_reporting_parameter_definitions_tbl d '
                || 'join ores_dq_configuration_analytics_artefact_tbl p '
                || 'on p.dataset_id = a.dataset_id and p.id = a.configuration_analytic_id '
                || 'where d.tenant_id = ores_utility_system_tenant_id_fn() '
                || 'and d.scope = ''analytic'' and d.subtype = p.analytic_type_code '
                || 'and d.name = a.parameter_name '
                || 'and d.valid_to = ores_utility_infinity_timestamp_fn()')))
    loop
        action := r.action;
        record_count := r.record_count;
        return next;
    end loop;

    select string_agg(format('%s: %s', x.name, x.problem), '; ' order by x.name)
    into v_problems
    from (
        select c.name,
               ores_reporting_configuration_analytics_problem_fn(
                   p_target_tenant_id, c.configuration_id) as problem
        from ores_reporting_risk_report_configs_tbl c
        where c.tenant_id = p_target_tenant_id
          and c.party_id = v_party_id
          and c.valid_to = ores_utility_infinity_timestamp_fn()
          and c.id in (select ores_dq_party_uuid_fn(v_party_id, a.id)
                       from ores_dq_risk_report_configs_artefact_tbl a
                       where a.dataset_id = p_dataset_id)
    ) x
    where x.problem is not null;
    if v_problems is not null then
        raise exception 'Dataset % stages risk report configurations whose analytics make no run: %.',
            p_dataset_id, v_problems;
    end if;

    insert into ores_reporting_risk_report_config_portfolios_tbl (
        tenant_id, risk_report_config_id, portfolio_id, valid_from, valid_to
    )
    select distinct on (live_id, pf.id)
           p_target_tenant_id, live_id, pf.id,
           clock_timestamp(), ores_utility_infinity_timestamp_fn()
    from (
        select a.scope_portfolio,
               ores_dq_party_uuid_fn(v_party_id, a.id) as live_id
        from ores_dq_risk_report_configs_artefact_tbl a
        where a.dataset_id = p_dataset_id
          and a.scope_portfolio is not null
    ) s
    join ores_refdata_portfolios_tbl pf
      on pf.tenant_id = p_target_tenant_id
     and pf.party_id = v_party_id
     and pf.name = s.scope_portfolio
     and pf.valid_to = ores_utility_infinity_timestamp_fn()
    where exists (
        select 1 from ores_reporting_risk_report_configs_tbl c
        where c.tenant_id = p_target_tenant_id
          and c.id = s.live_id
          and c.valid_to = ores_utility_infinity_timestamp_fn())
      and not exists (
        select 1 from ores_reporting_risk_report_config_portfolios_tbl o
        where o.tenant_id = p_target_tenant_id
          and o.risk_report_config_id = s.live_id
          and o.portfolio_id = pf.id
          and o.valid_to = ores_utility_infinity_timestamp_fn());

    get diagnostics v_scoped = row_count;
    if v_scoped > 0 then
        action := 'scoped';
        record_count := v_scoped;
        return next;
    end if;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_reporting_publish_risk_report_configs_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the risk report configurations a dataset stages for the target party,
 each registered as a reporting configuration, with the portfolio scope a staged
 row names. A publish only inserts, so the mode has no effect.';
