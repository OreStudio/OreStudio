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
-- Scheduler publish functions
--
-- A job definition and a retention policy are authored by the platform, so they
-- arrive as datasets published into the system tenant. Each carries two kinds of
-- field, and the publish treats them differently. The contract is what the
-- platform declares: the job's command and payload, the policy's target and
-- action. It is reconciled, so a release that changes what the sweep runs
-- changes it in every installation. The rest is the installation's own decision:
-- when a job fires, whether it is on, and a policy's period, cadence and state.
-- Those are written only when the row does not exist, so a republish leaves the
-- choice a person made alone.
-- =============================================================================

-- =============================================================================
-- Job Definitions
-- =============================================================================

-- compass:except(entity_writes/sql_writer) expires=2027-05-18 approver=marco reason=publishes a DQ dataset into the system tenant, like every other publish_from_dq function
create or replace function ores_scheduler_publish_job_definitions_from_dq_fn(
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
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    v_job_id uuid;
    v_schedule text;
    v_is_active boolean;
    v_exists boolean;
    v_unchanged boolean;
    v_new_version integer;
    r record;
begin
    perform ores_utility_allow_version_replace_fn();

    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- replace_all closes the jobs this dataset stages, and no others: the
    -- reporting and inbox services write job definitions of their own beside
    -- these, and a publish must not remove a job it never declared.
    if p_mode = 'replace_all' then
        update ores_scheduler_job_definitions_tbl
        set valid_to = clock_timestamp()
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and job_name in (
              select dq.job_name from ores_dq_job_definitions_artefact_tbl dq
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn());

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select dq.job_name, dq.description, dq.command, dq.schedule_expression,
               dq.action_type, dq.action_payload, dq.is_active
        from ores_dq_job_definitions_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_scheduler_job_definitions_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.job_name = r.job_name
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        -- The job's identity and the installation's own values come from the
        -- latest row the tenant held, closed or not: a publish that closes the
        -- row first, as replace_all does, must still leave the schedule, the
        -- state and the id a person's screens point at.
        select existing.id, existing.schedule_expression, existing.is_active
        into v_job_id, v_schedule, v_is_active
        from ores_scheduler_job_definitions_tbl existing
        where existing.tenant_id = p_target_tenant_id
          and existing.job_name = r.job_name
        order by existing.valid_from desc
        limit 1;

        -- A row the tenant holds with the same contract is left alone, so a
        -- repeat publish adds no version.
        select exists (
            select 1 from ores_scheduler_job_definitions_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.job_name = r.job_name
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
              and (existing.description, existing.command, existing.action_type,
                   existing.action_payload)
                  is not distinct from
                  (r.description, r.command, r.action_type, r.action_payload)
        ) into v_unchanged;

        if v_unchanged then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_scheduler_job_definitions_tbl (
            id, tenant_id, version, job_name, party_id, description, command,
            schedule_expression, action_type, action_payload, is_active,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            coalesce(v_job_id, gen_random_uuid()), p_target_tenant_id, 0, r.job_name, null,
            r.description, r.command,
            coalesce(v_schedule, r.schedule_expression),
            r.action_type, r.action_payload,
            coalesce(v_is_active, r.is_active),
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_scheduler_publish_job_definitions_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the job definitions a dataset stages into the target tenant. The
 command, the payload and the description are the contract and are reconciled;
 the schedule, the state and the id are the installation''s and are carried from
 the latest row it held, even where the publish closed it first, as replace_all
 does. A job with the same contract is skipped, so a repeat publish adds no
 version.';

-- =============================================================================
-- Retention Policies
-- =============================================================================

-- compass:except(entity_writes/sql_writer) expires=2027-05-18 approver=marco reason=publishes a DQ dataset into the system tenant, like every other publish_from_dq function
create or replace function ores_scheduler_publish_retention_policies_from_dq_fn(
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
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    v_period integer;
    v_cadence_hours integer;
    v_enabled boolean;
    v_exists boolean;
    v_unchanged boolean;
    v_new_version integer;
    r record;
begin
    perform ores_utility_allow_version_replace_fn();

    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_scheduler_retention_policies_tbl
        set valid_to = clock_timestamp()
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and code in (
              select dq.code from ores_dq_retention_policies_artefact_tbl dq
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn());

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select dq.code, dq.target_kind, dq.target, dq.action, dq.retention_days,
               dq.cadence_hours, dq.enabled, dq.description
        from ores_dq_retention_policies_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_scheduler_retention_policies_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        -- The installation's own values come from the latest row it held,
        -- closed or not: a publish that closes a row first, as replace_all
        -- does, must still leave the period and the state a person chose.
        select existing.retention_days, existing.cadence_hours, existing.enabled
        into v_period, v_cadence_hours, v_enabled
        from ores_scheduler_retention_policies_tbl existing
        where existing.tenant_id = p_target_tenant_id
          and existing.code = r.code
        order by existing.valid_from desc
        limit 1;

        select exists (
            select 1 from ores_scheduler_retention_policies_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
              and (existing.target_kind, existing.target, existing.action, existing.description)
                  is not distinct from (r.target_kind, r.target, r.action, r.description)
        ) into v_unchanged;

        if v_unchanged then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_scheduler_retention_policies_tbl (
            code, tenant_id, version, target_kind, target, action, retention_days,
            cadence_hours, enabled, description,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            r.code, p_target_tenant_id, 0, r.target_kind, r.target, r.action,
            coalesce(v_period, r.retention_days),
            coalesce(v_cadence_hours, r.cadence_hours),
            coalesce(v_enabled, r.enabled),
            r.description,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_scheduler_publish_retention_policies_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the retention policies a dataset stages into the target tenant. The
 target, the action and the description are the contract and are reconciled; the
 period, the cadence and the enabled state are the installation''s and are carried
 from the latest row it held, even where the publish closed it first, as
 replace_all does. A policy with the same contract is skipped, so a repeat publish
 adds no version.';

-- =============================================================================
-- Retained Stores
-- =============================================================================

-- compass:except(entity_writes/sql_writer) expires=2027-05-26 approver=marco reason=publishes a DQ dataset into the system tenant, like every other publish_from_dq function
create or replace function ores_scheduler_publish_retained_stores_from_dq_fn(
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
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    v_exists boolean;
    v_unchanged boolean;
    v_new_version integer;
    r record;
begin
    perform ores_utility_allow_version_replace_fn();

    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    if p_mode = 'replace_all' then
        update ores_scheduler_retained_stores_tbl
        set valid_to = clock_timestamp()
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and code in (
              select dq.code from ores_dq_retained_stores_artefact_tbl dq
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn());

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select dq.code, dq.display_name, dq.description, dq.owner_component,
               dq.scope, dq.target_kind, dq.target, dq.time_column, dq.measure_kind,
               dq.bound_kind, dq.default_action, dq.default_retention_days,
               dq.default_cadence_hours, dq.default_reason
        from ores_dq_retained_stores_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_scheduler_retained_stores_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        select exists (
            select 1 from ores_scheduler_retained_stores_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
              and (existing.display_name, existing.description, existing.owner_component,
                   existing.scope, existing.target_kind, existing.target,
                   existing.time_column, existing.measure_kind, existing.bound_kind,
                   existing.default_action, existing.default_retention_days,
                   existing.default_cadence_hours, existing.default_reason)
                  is not distinct from
                  (r.display_name, r.description, r.owner_component,
                   r.scope, r.target_kind, r.target,
                   r.time_column, r.measure_kind, r.bound_kind,
                   r.default_action, r.default_retention_days,
                   r.default_cadence_hours, r.default_reason)
        ) into v_unchanged;

        if v_unchanged then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_scheduler_retained_stores_tbl (
            code, tenant_id, version, display_name, description, owner_component,
            scope, target_kind, target, time_column, measure_kind, bound_kind,
            default_action, default_retention_days, default_cadence_hours,
            default_reason,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            r.code, p_target_tenant_id, 0, r.display_name, r.description,
            r.owner_component, r.scope, r.target_kind, r.target, r.time_column,
            r.measure_kind, r.bound_kind, r.default_action, r.default_retention_days,
            r.default_cadence_hours, r.default_reason,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_scheduler_publish_retained_stores_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the catalogue of stores a dataset stages into the target tenant. The
 whole row is the platform''s statement of what the store is and the bound it ships
 with, so every field is reconciled and a repeat publish adds no version. What an
 installation decides about a bound it records in the policy, not here, which is
 why a changed default leaves an existing policy alone.';
