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
 * ORE Publish-from-DQ Functions
 *
 * SECURITY DEFINER functions called by the ore service NATS handler for the
 * ore.v1.ops.publish_*_from_dq subjects, and by the bring-up publish. Each function
 * reads DQ artefact tables (system tenant) and writes only to ores_ore_* tables.
 *
 * All functions use SECURITY DEFINER set search_path = public, pg_temp so they
 * execute with the definer's privileges without needing cross-service DML grants.
 */

-- =============================================================================
-- Ore Parameter Value Domains: ore.v1.ops.publish_ore_parameter_value_domains_from_dq
-- =============================================================================

-- compass:except(entity_writes/sql_writer) expires=2027-05-18 approver=marco reason=publishes the value domains the dq catalogue defines into the tenant, the publish-from-dq pattern beside it
create or replace function ores_ore_publish_ore_parameter_value_domains_from_dq_fn(
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
    r record;
    v_exists boolean;
    v_unchanged boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
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

    -- replace_all closes every current row of the target tenant, not only the
    -- rows this dataset wrote, because the dataset is the table's only writer.
    if p_mode = 'replace_all' then
        update ores_ore_parameter_value_domains_tbl
        set valid_to = clock_timestamp()
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code,
            dq.name,
            dq.storage_kind,
            dq.referenced_entity
        from ores_dq_ore_parameter_value_domains_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_ore_parameter_value_domains_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        select exists (
            select 1 from ores_ore_parameter_value_domains_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code = r.code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
              and (existing.name, existing.storage_kind, existing.referenced_entity)
                  is not distinct from (r.name, r.storage_kind, r.referenced_entity)
        ) into v_unchanged;

        -- A row the tenant holds with the same values is left alone, so a
        -- repeat publish adds no version.
        if v_unchanged then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_ore_parameter_value_domains_tbl (
            tenant_id,
            code, version,
            name, storage_kind, referenced_entity,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code, 0,
            r.name, r.storage_kind, r.referenced_entity,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
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

comment on function ores_ore_publish_ore_parameter_value_domains_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the ORE parameter value domains a dataset stages into the target tenant,
 each replacing the row the tenant holds under its code. A row with the same values
 is skipped, so a repeat publish adds no version. An upsert never closes a domain a
 later dataset version drops. A replace_all closes every current row of the target
 tenant, because the dataset is the only writer of the table.';

-- =============================================================================
-- Ore Parameters: ore.v1.ops.publish_ore_parameters_from_dq
-- =============================================================================

-- compass:except(entity_writes/sql_writer) expires=2027-05-18 approver=marco reason=publishes the ore parameter documentation the dq catalogue defines into the tenant, the publish-from-dq pattern beside it
create or replace function ores_ore_publish_ore_parameters_from_dq_fn(
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
    r record;
    v_exists boolean;
    v_unchanged boolean;
    v_new_version integer;
begin
    -- A publish is an upsert by design: it replaces the row it finds.
    -- Stating version 0 asserts the row does not exist, so the publish
    -- asks for the version replace the store grants a bulk writer.
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

    -- replace_all closes every current row of the target tenant, not only the
    -- rows this dataset wrote, because the dataset is the table's only writer.
    if p_mode = 'replace_all' then
        update ores_ore_parameters_tbl
        set valid_to = clock_timestamp()
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- A live row that holds the same parameter under another id is superseded
    -- first. The id is derived from the schema and the XPath, and that
    -- derivation has changed before now; an id-keyed check cannot see the
    -- generation it no longer reproduces, so the row would stay live beside the
    -- new one and the dataset would hold the same parameter twice.
    if p_mode = 'upsert' then
        update ores_ore_parameters_tbl live
        set valid_to = clock_timestamp()
        where live.tenant_id = p_target_tenant_id
          and live.valid_to = ores_utility_infinity_timestamp_fn()
          and exists (
              select 1 from ores_dq_ore_parameters_artefact_tbl dq
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn()
                and dq.id <> live.id
                and (dq.scope, dq.subtype, dq.name, dq.schema_name, dq.xpath)
                    = (live.scope, live.subtype, live.name, live.schema_name, live.xpath));
    end if;

    for r in
        select
            dq.id,
            dq.scope,
            dq.subtype,
            dq.name,
            dq.schema_name,
            dq.xpath,
            dq.position,
            dq.ore_parameter_value_domain_code,
            dq.is_required,
            dq.label,
            dq.help,
            dq.description,
            dq.default_text,
            dq.format,
            dq.introduced_in,
            dq.removed_in
        from ores_dq_ore_parameters_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_ore_parameters_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.id = r.id
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        select exists (
            select 1 from ores_ore_parameters_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.id = r.id
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
              and (existing.scope, existing.subtype, existing.name, existing.schema_name, existing.xpath, existing.position, existing.ore_parameter_value_domain_code, existing.is_required, existing.label, existing.help, existing.description, existing.default_text, existing.format, existing.introduced_in, existing.removed_in)
                  is not distinct from (r.scope, r.subtype, r.name, r.schema_name, r.xpath, r.position, r.ore_parameter_value_domain_code, r.is_required, r.label, r.help, r.description, r.default_text, r.format, r.introduced_in, r.removed_in)
        ) into v_unchanged;

        -- A row the tenant holds with the same values is left alone, so a
        -- repeat publish adds no version.
        if v_unchanged then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_ore_parameters_tbl (
            tenant_id,
            id, version,
            scope, subtype, name, schema_name,
            xpath, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text,
            format, introduced_in, removed_in,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.id, 0,
            r.scope, r.subtype, r.name, r.schema_name,
            r.xpath, r.position, r.ore_parameter_value_domain_code, r.is_required,
            r.label, r.help, r.description, r.default_text,
            r.format, r.introduced_in, r.removed_in,
            coalesce(ores_iam_current_service_fn(), current_user), current_user, 'system.external_data_import',
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

comment on function ores_ore_publish_ore_parameters_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the ORE parameter documentation a dataset stages into the target tenant,
 each replacing the row the tenant holds under its id. A row with the same values
 is skipped, so a repeat publish adds no version. The value domains are published
 first, because the insert checks the domain of each parameter. An upsert never
 closes a parameter that a later dataset version drops. A replace_all closes every
 current row of the target tenant, because the dataset is the only writer of the
 table. The id comes from the XPath of the leaf, so a renamed leaf is a new row.';
