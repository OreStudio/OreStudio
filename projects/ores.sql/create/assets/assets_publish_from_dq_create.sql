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
 * Assets Publish-from-DQ Functions
 *
 * SECURITY DEFINER functions called by the assets service NATS handler for the
 * assets.v1.<entity>.publish-from-dq subjects. Each function reads DQ artefact
 * tables (system tenant) and writes only to ores_assets_* tables.
 *
 * All functions use SECURITY DEFINER set search_path = public, pg_temp so they
 * execute with the definer's privileges without needing cross-service DML grants.
 */

-- =============================================================================
-- Images: assets.v1.images.publish-from-dq
-- =============================================================================

create or replace function ores_assets_publish_images_from_dq_fn(
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
    v_existing_image_id uuid;
    v_new_version integer;
begin
    -- Validate dataset exists
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode: soft-delete all existing images for this tenant
    if p_mode = 'replace_all' then
        update ores_assets_images_tbl
        set valid_to = current_timestamp
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each image from DQ dataset (read from system tenant)
    for r in
        select
            dq.image_id,
            dq.key,
            dq.description,
            dq.svg_data
        from ores_dq_images_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and dq.tenant_id = ores_utility_system_tenant_id_fn()
    loop
        -- Check if an image with this key already exists in the target tenant
        select image_id into v_existing_image_id
        from ores_assets_images_tbl existing
        where existing.key = r.key
          and existing.tenant_id = p_target_tenant_id
          and existing.valid_to = ores_utility_infinity_timestamp_fn();

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_existing_image_id is not null then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert image - trigger handles versioning automatically.
        -- Use existing image_id when updating to preserve referential integrity.
        -- DQ artefact staging is SVG-only text; base64-encode into the
        -- generalised (mime_type, data) shape of the live table.
        insert into ores_assets_images_tbl (
            tenant_id,
            image_id, version, key, description, mime_type, data,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            p_target_tenant_id,
            coalesce(v_existing_image_id, r.image_id), 0, r.key, r.description,
            'image/svg+xml', encode(convert_to(r.svg_data, 'UTF8'), 'base64'),
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import',
            'Published from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    -- Return summary
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
