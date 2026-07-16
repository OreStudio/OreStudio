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
 * Badge Mappings Population Functions
 *
 * Functions to publish badge mappings from the DQ artefact table into a
 * tenant's own copy of ores_dq_badge_mappings_tbl. Self-referential (like
 * coding_schemes/badge_severities/badge_definitions/code_domains): DQ
 * publishes into its own table, not another service's.
 *
 * Unlike the other three badge datasets, badge_mappings' natural key is the
 * (code_domain_code, entity_code) pair, not a single code column.
 *
 * Usage:
 *   -- Preview what will be published
 *   SELECT * FROM ores_dq_badge_mapping_preview_fn(dataset_id);
 *
 *   -- Publish to a tenant
 *   SELECT * FROM ores_dq_badge_mappings_publish_fn(dataset_id, target_tenant_id, 'upsert');
 */

-- =============================================================================
-- Preview Function
-- =============================================================================

create or replace function ores_dq_badge_mapping_preview_fn(p_dataset_id uuid)
returns table (
    action text,
    code_domain_code text,
    entity_code text,
    badge_code text,
    reason text
) as $$
begin
    return query
    select
        case
            when existing.code_domain_code is not null then 'update'
            else 'insert'
        end as action,
        dq.code_domain_code,
        dq.entity_code,
        dq.badge_code,
        case
            when existing.code_domain_code is not null then 'Record already exists'
            else 'New record'
        end as reason
    from ores_dq_badge_mappings_artefact_tbl dq
    left join ores_dq_badge_mappings_tbl existing
        on existing.code_domain_code = dq.code_domain_code
        and existing.entity_code = dq.entity_code
        and existing.valid_to = ores_utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.code_domain_code, dq.entity_code;
end;
$$ language plpgsql;

-- =============================================================================
-- Population Function
-- =============================================================================

/**
 * Populate ores_dq_badge_mappings_tbl from a DQ badge mappings dataset.
 *
 * Uses SECURITY DEFINER to bypass RLS: it reads artefacts staged from the
 * system tenant and writes into the tenant specified by p_target_tenant_id.
 *
 * @param p_dataset_id       The DQ dataset to populate from
 * @param p_target_tenant_id The tenant to publish data to
 * @param p_mode             Population mode: 'upsert', 'insert_only', or 'replace_all'
 */
create or replace function ores_dq_badge_mappings_publish_fn(
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
    v_new_version integer;
begin
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
        update ores_dq_badge_mappings_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and change_commentary = 'Imported from DQ dataset: ' || v_dataset_name
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select
            dq.code_domain_code,
            dq.entity_code,
            dq.badge_code
        from ores_dq_badge_mappings_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_dq_badge_mappings_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.code_domain_code = r.code_domain_code
              and existing.entity_code = r.entity_code
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_dq_badge_mappings_tbl (
            tenant_id,
            code_domain_code, entity_code, version, badge_code,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            r.code_domain_code, r.entity_code, 0, r.badge_code,
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
$$ language plpgsql security definer;
