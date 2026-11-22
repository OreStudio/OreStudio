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
-- Knowledge Pages: orgmode.v1.ops.publish_knowledge_pages_from_dq
-- =============================================================================

-- compass:except(entity_writes/sql_writer) expires=2027-05-18 approver=marco reason=publishes the knowledge pages the dq dataset stages into the system tenant, the publish-from-dq pattern
create or replace function ores_orgmode_publish_knowledge_pages_from_dq_fn(
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
    -- A publish replaces the row it finds; stating version 0 asks for the
    -- version replace the store grants a bulk writer.
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

    -- replace_all closes the pages the target tenant holds and the dataset no
    -- longer does: a page that stopped declaring #+audience: user leaves the
    -- knowledge base. The dataset is the table's only writer, so this is every
    -- current row of the tenant not in the dataset. A page the dataset still
    -- holds is replaced only when it changed, below, so a repeat publish in
    -- any mode adds no version.
    if p_mode = 'replace_all' then
        update ores_orgmode_knowledge_pages_tbl existing
        set valid_to = clock_timestamp()
        where existing.tenant_id = p_target_tenant_id
          and existing.valid_to = ores_utility_infinity_timestamp_fn()
          and not exists (
              select 1 from ores_dq_knowledge_pages_artefact_tbl dq
              where dq.dataset_id = p_dataset_id
                and dq.tenant_id = ores_utility_system_tenant_id_fn()
                and dq.id = existing.id);

        get diagnostics v_deleted = row_count;
    end if;

    for r in
        select dq.id, dq.path, dq.org
        from ores_dq_knowledge_pages_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        select exists (
            select 1 from ores_orgmode_knowledge_pages_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.id = r.id
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_exists;

        select exists (
            select 1 from ores_orgmode_knowledge_pages_tbl existing
            where existing.tenant_id = p_target_tenant_id
              and existing.id = r.id
              and existing.valid_to = ores_utility_infinity_timestamp_fn()
              and (existing.path, existing.org) is not distinct from (r.path, r.org)
        ) into v_unchanged;

        if v_unchanged then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_orgmode_knowledge_pages_tbl (
            tenant_id, id, version, path, org,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id, r.id, 0, r.path, r.org,
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

comment on function ores_orgmode_publish_knowledge_pages_from_dq_fn(uuid, uuid, text, jsonb) is
'Publishes the knowledge pages a dataset stages into the target tenant, the
 system tenant, each replacing the row the tenant holds under its org id when its
 path or text changed. An unchanged page is skipped, so a repeat publish adds no
 version. A replace_all closes the pages the tenant holds that the dataset no
 longer does; the dataset is the only writer of the table.';
