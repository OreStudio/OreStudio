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
 * LEI Counterparties Publication Function
 *
 * Populates counterparties and counterparty identifiers from GLEIF LEI
 * entity and relationship staging data. All LEI entities are published as
 * counterparties with parent-child hierarchy derived from
 * IS_DIRECTLY_CONSOLIDATED_BY relationships.
 *
 * This function reads ALL records from the LEI staging tables (not filtered
 * by p_dataset_id) since these are shared staging tables.
 */

-- =============================================================================
-- LEI Counterparties Publication
-- =============================================================================

/**
 * Populate counterparties from GLEIF LEI entity data.
 *
 * This function uses SECURITY DEFINER to bypass RLS. It reads from the
 * LEI staging tables and writes counterparties to the target tenant.
 *
 * Algorithm:
 *   1. Build UUID map for all LEI entities
 *   2. Build parent map from IS_DIRECTLY_CONSOLIDATED_BY relationships
 *   3. Insert counterparties (parents first, then children)
 *   4. Insert counterparty identifiers with LEI scheme
 *
 * @param p_dataset_id       The DQ dataset (used for audit trail only)
 * @param p_target_tenant_id The tenant to publish data to
 * @param p_mode             Population mode (only 'upsert' supported)
 * @param p_params           Ignored for counterparties
 */
create or replace function ores_dq_lei_counterparties_publish_fn(
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
    v_inserted_counterparties bigint := 0;
    v_inserted_identifiers bigint := 0;
    v_dataset_name text;
    v_dataset_size text;
    v_entity_dataset_id uuid;
    v_rel_dataset_id uuid;
begin
    -- Validate dataset exists
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Determine which LEI dataset to use (default: large)
    v_dataset_size := coalesce(p_params ->> 'lei_dataset_size', 'large');

    select id into v_entity_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_entities.' || v_dataset_size
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_rel_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_relationships.' || v_dataset_size
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_entity_dataset_id is null or v_rel_dataset_id is null then
        raise exception 'LEI dataset not found for size: %', v_dataset_size;
    end if;

    -- Check if target tenant already has LEI-sourced counterparties (idempotency)
    if exists (
        select 1
        from ores_refdata_counterparty_identifiers_tbl ci
        where ci.tenant_id = p_target_tenant_id
          and ci.id_scheme = 'LEI'
          and ci.valid_to = ores_utility_infinity_timestamp_fn()
        limit 1
    ) then
        raise notice 'Target tenant already has LEI counterparties, skipping.';
        return query select 'skipped'::text, 0::bigint;
        return;
    end if;

    -- Create temp table mapping LEI codes to generated UUIDs
    create temp table lei_counterparty_uuid_map (
        lei text primary key,
        counterparty_uuid uuid not null default gen_random_uuid(),
        parent_lei text null,
        entity_legal_name text not null,
        entity_legal_address_country text not null,
        entity_entity_status text not null
    ) on commit drop;

    -- Populate LEI entities from the selected dataset
    insert into lei_counterparty_uuid_map (lei, entity_legal_name, entity_legal_address_country, entity_entity_status)
    select distinct on (e.lei)
        e.lei,
        e.entity_legal_name,
        e.entity_legal_address_country,
        e.entity_entity_status
    from ores_dq_lei_entities_artefact_tbl e
    where e.dataset_id = v_entity_dataset_id
    order by e.lei;

    -- Set parent LEI from active IS_DIRECTLY_CONSOLIDATED_BY relationships
    -- relationship_start_node_node_id = child, relationship_end_node_node_id = parent
    update lei_counterparty_uuid_map m
    set parent_lei = r.relationship_end_node_node_id
    from ores_dq_lei_relationships_artefact_tbl r
    where r.relationship_start_node_node_id = m.lei
      and r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
      and r.relationship_relationship_status = 'ACTIVE'
      and r.dataset_id = v_rel_dataset_id;

    -- Insert counterparties: parents first (parent_lei IS NULL), then children
    -- Using a single INSERT with the parent UUID resolved via self-join
    insert into ores_refdata_counterparties_tbl (
        tenant_id,
        id, version, full_name, short_code, party_type,
        parent_counterparty_id, status,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        m.counterparty_uuid, 0, m.entity_legal_name, m.lei, 'Corporate',
        parent_map.counterparty_uuid, 'Active',
        current_user, current_user, 'system.external_data_import',
        'Imported from GLEIF LEI dataset: ' || v_dataset_name
    from lei_counterparty_uuid_map m
    left join lei_counterparty_uuid_map parent_map on parent_map.lei = m.parent_lei;

    get diagnostics v_inserted_counterparties = row_count;

    -- Insert counterparty identifiers (LEI scheme)
    insert into ores_refdata_counterparty_identifiers_tbl (
        tenant_id,
        id, version, counterparty_id, id_scheme, id_value,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        gen_random_uuid(), 0, m.counterparty_uuid, 'LEI', m.lei,
        current_user, current_user, 'system.external_data_import',
        'Imported from GLEIF LEI dataset: ' || v_dataset_name
    from lei_counterparty_uuid_map m;

    get diagnostics v_inserted_identifiers = row_count;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted_counterparties
    where v_inserted_counterparties > 0
    union all
    select 'inserted_identifiers'::text, v_inserted_identifiers
    where v_inserted_identifiers > 0;
end;
$$ language plpgsql security definer;
