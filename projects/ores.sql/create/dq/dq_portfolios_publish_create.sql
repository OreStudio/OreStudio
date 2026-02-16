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
 * Portfolios Publication Function
 *
 * Copies portfolio artefact data into the refdata table for a target tenant.
 * Portfolios have a self-referencing hierarchy (parent_portfolio_id) and an
 * optional FK to business units (owner_unit_id). Both references are resolved
 * by matching names against already-published refdata.
 *
 * Business units must be published before portfolios so that owner_unit_id
 * references can be resolved.
 *
 * @param p_dataset_id       The DQ dataset containing portfolio artefacts
 * @param p_target_tenant_id The tenant to publish data to
 * @param p_mode             Population mode (only 'upsert' supported)
 * @param p_params           Reserved for future use
 */

-- =============================================================================
-- Portfolios Publication
-- =============================================================================

create or replace function ores_dq_portfolios_publish_fn(
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
    v_bu_dataset_id uuid;
    v_inserted bigint := 0;
    v_current_depth int;
    v_max_depth int;
    v_level_count bigint;
begin
    -- Idempotency: skip if tenant already has portfolios
    if exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = p_target_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    -- Find the business units artefact dataset for owner_unit_id mapping
    select id into v_bu_dataset_id
    from ores_dq_datasets_tbl
    where code = 'testdata.business_units'
      and valid_to = ores_utility_infinity_timestamp_fn();

    -- Build owner_unit_id reference map: artefact BU ID -> published BU ID
    -- Joined on unit_name since IDs differ between artefact and published data
    create temp table bu_ref_map (
        artefact_id uuid primary key,
        published_id uuid not null
    ) on commit drop;

    if v_bu_dataset_id is not null then
        insert into bu_ref_map (artefact_id, published_id)
        select a.id, r.id
        from ores_dq_business_units_artefact_tbl a
        join ores_refdata_business_units_tbl r
            on r.unit_name = a.unit_name
            and r.tenant_id = p_target_tenant_id
            and r.valid_to = ores_utility_infinity_timestamp_fn()
        where a.dataset_id = v_bu_dataset_id;
    end if;

    -- Build portfolio mapping table with depth
    create temp table portfolio_publish_map (
        artefact_id uuid primary key,
        new_id uuid not null default gen_random_uuid(),
        parent_artefact_id uuid,
        owner_unit_artefact_id uuid,
        depth int not null default 0,
        name text not null,
        purpose_type text,
        aggregation_ccy text,
        is_virtual integer
    ) on commit drop;

    -- Seed with root nodes (no parent)
    insert into portfolio_publish_map (
        artefact_id, parent_artefact_id, owner_unit_artefact_id, depth,
        name, purpose_type, aggregation_ccy, is_virtual
    )
    select id, parent_portfolio_id, owner_unit_id, 0,
           name, purpose_type, aggregation_ccy, is_virtual
    from ores_dq_portfolios_artefact_tbl
    where dataset_id = p_dataset_id
      and parent_portfolio_id is null;

    -- Iteratively add children at increasing depth
    v_current_depth := 0;
    loop
        insert into portfolio_publish_map (
            artefact_id, parent_artefact_id, owner_unit_artefact_id, depth,
            name, purpose_type, aggregation_ccy, is_virtual
        )
        select a.id, a.parent_portfolio_id, a.owner_unit_id, v_current_depth + 1,
               a.name, a.purpose_type, a.aggregation_ccy, a.is_virtual
        from ores_dq_portfolios_artefact_tbl a
        join portfolio_publish_map m on m.artefact_id = a.parent_portfolio_id
        where a.dataset_id = p_dataset_id
          and m.depth = v_current_depth
          and not exists (select 1 from portfolio_publish_map x where x.artefact_id = a.id);

        if not found then exit; end if;
        v_current_depth := v_current_depth + 1;
    end loop;

    select max(depth) into v_max_depth from portfolio_publish_map;

    -- Insert level by level so the trigger can validate parent references
    for v_current_depth in 0..coalesce(v_max_depth, 0) loop
        insert into ores_refdata_portfolios_tbl (
            tenant_id, id, version, name, parent_portfolio_id,
            owner_unit_id, purpose_type, aggregation_ccy, is_virtual,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            m.new_id, 0, m.name,
            parent_m.new_id,
            bu_map.published_id,
            m.purpose_type, m.aggregation_ccy, m.is_virtual,
            current_user, current_user, 'system.external_data_import',
            'Published from organisation dataset'
        from portfolio_publish_map m
        left join portfolio_publish_map parent_m
            on parent_m.artefact_id = m.parent_artefact_id
        left join bu_ref_map bu_map
            on bu_map.artefact_id = m.owner_unit_artefact_id
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
