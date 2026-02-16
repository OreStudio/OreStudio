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
 * Business Units Publication Function
 *
 * Copies business unit artefact data into the refdata table for a target
 * tenant. The root party is resolved from p_params->>'party_id' or by
 * querying the tenant's operational root party.
 *
 * Business units form a self-referencing hierarchy, so they are inserted
 * depth-first (roots first, then children) to satisfy the insert trigger's
 * parent FK validation.
 *
 * @param p_dataset_id       The DQ dataset containing business unit artefacts
 * @param p_target_tenant_id The tenant to publish data to
 * @param p_mode             Population mode (only 'upsert' supported)
 * @param p_params           Optional: {"party_id": "<uuid>"} to override root party
 */

-- =============================================================================
-- Business Units Publication
-- =============================================================================

create or replace function ores_dq_business_units_publish_fn(
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
    v_current_depth int;
    v_max_depth int;
    v_level_count bigint;
begin
    -- Idempotency: skip if tenant already has business units
    if exists (
        select 1 from ores_refdata_business_units_tbl
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    -- Resolve root party: explicit param or query tenant's operational root
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

    -- Build mapping table: artefact ID -> new UUID, with depth for ordering
    create temp table bu_publish_map (
        artefact_id uuid primary key,
        new_id uuid not null default gen_random_uuid(),
        parent_artefact_id uuid,
        depth int not null default 0,
        unit_name text not null,
        unit_code text,
        business_centre_code text
    ) on commit drop;

    -- Seed with root nodes (no parent)
    insert into bu_publish_map (
        artefact_id, parent_artefact_id, depth,
        unit_name, unit_code, business_centre_code
    )
    select id, parent_business_unit_id, 0,
           unit_name, unit_code, business_centre_code
    from ores_dq_business_units_artefact_tbl
    where dataset_id = p_dataset_id
      and parent_business_unit_id is null;

    -- Iteratively add children at increasing depth
    v_current_depth := 0;
    loop
        insert into bu_publish_map (
            artefact_id, parent_artefact_id, depth,
            unit_name, unit_code, business_centre_code
        )
        select a.id, a.parent_business_unit_id, v_current_depth + 1,
               a.unit_name, a.unit_code, a.business_centre_code
        from ores_dq_business_units_artefact_tbl a
        join bu_publish_map m on m.artefact_id = a.parent_business_unit_id
        where a.dataset_id = p_dataset_id
          and m.depth = v_current_depth
          and not exists (select 1 from bu_publish_map x where x.artefact_id = a.id);

        if not found then exit; end if;
        v_current_depth := v_current_depth + 1;
    end loop;

    select max(depth) into v_max_depth from bu_publish_map;

    -- Insert level by level so the trigger can validate parent references
    for v_current_depth in 0..coalesce(v_max_depth, 0) loop
        insert into ores_refdata_business_units_tbl (
            tenant_id, id, version, party_id, unit_name,
            parent_business_unit_id, unit_code, business_centre_code,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            m.new_id, 0, v_root_party_id, m.unit_name,
            parent_m.new_id, m.unit_code, m.business_centre_code,
            current_user, current_user, 'system.external_data_import',
            'Published from organisation dataset'
        from bu_publish_map m
        left join bu_publish_map parent_m
            on parent_m.artefact_id = m.parent_artefact_id
        where m.depth = v_current_depth;

        get diagnostics v_level_count = row_count;
        v_inserted := v_inserted + v_level_count;
    end loop;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0;
end;
$$ language plpgsql security definer;
