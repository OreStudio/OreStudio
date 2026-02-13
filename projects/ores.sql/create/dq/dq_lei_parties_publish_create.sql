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
 * LEI Parties Publication Function
 *
 * Populates parties and party identifiers from GLEIF LEI entity and
 * relationship staging data. Only the subtree rooted at a user-specified
 * root LEI is published. The root becomes the tenant's root party.
 *
 * The root LEI is extracted from p_params->>'root_lei'.
 *
 * This function reads ALL records from the LEI staging tables (not filtered
 * by p_dataset_id) since these are shared staging tables.
 */

-- =============================================================================
-- LEI Parties Publication
-- =============================================================================

/**
 * Populate parties from GLEIF LEI entity data for a specific subtree.
 *
 * This function uses SECURITY DEFINER to bypass RLS. It reads from the
 * LEI staging tables and writes parties to the target tenant.
 *
 * Algorithm:
 *   1. Extract root_lei from p_params
 *   2. Validate root LEI exists and tenant has no existing root party
 *   3. Recursive CTE to resolve subtree from root
 *   4. Build UUID map for subtree members
 *   5. Insert parties with parent-child hierarchy
 *   6. Insert party identifiers with LEI scheme
 *
 * @param p_dataset_id       The DQ dataset (used for audit trail only)
 * @param p_target_tenant_id The tenant to publish data to
 * @param p_mode             Population mode (only 'upsert' supported)
 * @param p_params           Must contain {"root_lei": "<LEI code>"}
 */
create or replace function ores_dq_lei_parties_publish_fn(
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
    v_root_lei text;
    v_inserted_parties bigint := 0;
    v_inserted_identifiers bigint := 0;
    v_inserted_bic_identifiers bigint := 0;
    v_dataset_name text;
    v_dataset_code text;
    v_dataset_size text;
    v_entity_dataset_id uuid;
    v_rel_dataset_id uuid;
    v_bic_dataset_id uuid;
begin
    -- Validate dataset exists and get code for size derivation
    select name, code into v_dataset_name, v_dataset_code
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Derive size from dataset code (e.g. 'gleif.lei_parties.small' -> 'small')
    -- Falls back to params or 'small' as default.
    v_dataset_size := coalesce(
        substring(v_dataset_code from '[^.]+$'),
        p_params ->> 'lei_dataset_size',
        'small');

    select id into v_entity_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_entities.' || v_dataset_size
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_rel_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_relationships.' || v_dataset_size
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_bic_dataset_id
    from ores_dq_datasets_tbl
    where code = 'gleif.lei_bic'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_entity_dataset_id is null or v_rel_dataset_id is null then
        raise exception 'LEI dataset not found for size: %', v_dataset_size;
    end if;

    -- Extract root_lei from params
    v_root_lei := p_params ->> 'root_lei';
    if v_root_lei is null or v_root_lei = '' then
        raise exception 'root_lei parameter is required for LEI parties publication';
    end if;

    -- Validate root LEI exists in the selected dataset
    if not exists (
        select 1
        from ores_dq_lei_entities_artefact_tbl
        where lei = v_root_lei
          and dataset_id = v_entity_dataset_id
    ) then
        raise exception 'Root LEI not found in staging data: %', v_root_lei;
    end if;

    -- Validate target tenant has no existing operational root party.
    -- The system party (party_category='system') is excluded — it is
    -- a platform-level entity, not the operational party hierarchy root.
    if exists (
        select 1
        from ores_refdata_parties_tbl p
        where p.tenant_id = p_target_tenant_id
          and p.parent_party_id is null
          and p.party_category <> 'system'
          and p.valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Target tenant already has a root party. Cannot publish LEI parties.';
    end if;

    -- Resolve subtree using recursive CTE with depth tracking
    create temp table lei_party_subtree (
        lei text primary key,
        party_uuid uuid not null default gen_random_uuid(),
        parent_lei text null,
        depth int not null default 0,
        entity_legal_name text not null,
        entity_legal_address_country text not null,
        entity_entity_status text not null
    ) on commit drop;

    -- Find all LEIs in the subtree with depth (filtered to selected dataset)
    with recursive subtree as (
        -- Root node (depth 0)
        select e.lei, 0 as depth
        from ores_dq_lei_entities_artefact_tbl e
        where e.lei = v_root_lei
          and e.dataset_id = v_entity_dataset_id

        union

        -- Children: entities whose parent (end_node) is already in the subtree
        select distinct r.relationship_start_node_node_id, s.depth + 1
        from ores_dq_lei_relationships_artefact_tbl r
        join subtree s on s.lei = r.relationship_end_node_node_id
        where r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
          and r.relationship_relationship_status = 'ACTIVE'
          and r.dataset_id = v_rel_dataset_id
    )
    insert into lei_party_subtree (lei, depth, entity_legal_name, entity_legal_address_country, entity_entity_status)
    select distinct on (s.lei)
        s.lei,
        s.depth,
        e.entity_legal_name,
        e.entity_legal_address_country,
        e.entity_entity_status
    from subtree s
    join ores_dq_lei_entities_artefact_tbl e on e.lei = s.lei
        and e.dataset_id = v_entity_dataset_id
    order by s.lei;

    -- Set parent LEI from active IS_DIRECTLY_CONSOLIDATED_BY relationships
    -- Only for entities within the subtree
    update lei_party_subtree m
    set parent_lei = r.relationship_end_node_node_id
    from ores_dq_lei_relationships_artefact_tbl r
    where r.relationship_start_node_node_id = m.lei
      and r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
      and r.relationship_relationship_status = 'ACTIVE'
      and r.dataset_id = v_rel_dataset_id
      and m.lei <> v_root_lei;  -- Root has no parent

    -- Insert parties level by level (root first, then children) so that
    -- the insert trigger can validate parent_party_id references.
    -- Names are normalised from ALL CAPS to Proper Case, with common
    -- corporate suffixes restored to uppercase.
    declare
        v_current_depth int := 0;
        v_max_depth int;
        v_level_count bigint;
    begin
        select max(depth) into v_max_depth from lei_party_subtree;

        for v_current_depth in 0..coalesce(v_max_depth, 0) loop
            insert into ores_refdata_parties_tbl (
                tenant_id,
                id, version, full_name, short_code,
                party_category, party_type,
                parent_party_id, business_center_code, status,
                modified_by, performed_by, change_reason_code, change_commentary
            )
            select
                p_target_tenant_id,
                m.party_uuid, 0,
                m.entity_legal_name,
                m.lei, 'operational', 'Corporate',
                parent_map.party_uuid,
                -- Default business centre from country
                coalesce(bc_map.business_center_code, 'WRLD'),
                'Active',
                current_user, current_user, 'system.external_data_import',
                'Imported from GLEIF LEI dataset: ' || v_dataset_name
            from lei_party_subtree m
            left join lei_party_subtree parent_map on parent_map.lei = m.parent_lei
            left join (values
                ('AE', 'AEDU'), ('AT', 'ATVI'), ('AU', 'AUSY'), ('BE', 'BEBR'),
                ('BR', 'BRSP'), ('CA', 'CATO'), ('CH', 'CHZU'), ('CL', 'CLSA'),
                ('CN', 'CNBE'), ('CO', 'COBO'), ('CZ', 'CZPR'), ('DE', 'DEFR'),
                ('DK', 'DKCO'), ('ES', 'ESMA'), ('FI', 'FIHE'), ('FR', 'FRPA'),
                ('GB', 'GBLO'), ('GR', 'GRAT'), ('HK', 'HKHK'), ('HU', 'HUBU'),
                ('ID', 'IDJA'), ('IE', 'IEDU'), ('IL', 'ILTA'), ('IN', 'INMU'),
                ('IT', 'ITMI'), ('JP', 'JPTO'), ('KR', 'KRSE'), ('KY', 'KYGE'),
                ('LU', 'LULU'), ('MX', 'MXMC'), ('MY', 'MYKL'), ('NL', 'NLAM'),
                ('NO', 'NOOS'), ('NZ', 'NZAU'), ('PH', 'PHMA'), ('PL', 'PLWA'),
                ('PT', 'PTLI'), ('RO', 'ROBU'), ('RU', 'RUMO'), ('SA', 'SARI'),
                ('SE', 'SEST'), ('SG', 'SGSI'), ('TH', 'THBA'), ('TR', 'TRIS'),
                ('TW', 'TWTA'), ('US', 'USNY'), ('ZA', 'ZAJO')
            ) as bc_map(country_code, business_center_code)
                on bc_map.country_code = m.entity_legal_address_country
            where m.depth = v_current_depth;

            get diagnostics v_level_count = row_count;
            v_inserted_parties := v_inserted_parties + v_level_count;
        end loop;
    end;

    -- Insert party identifiers (LEI scheme)
    insert into ores_refdata_party_identifiers_tbl (
        tenant_id,
        id, version, party_id, id_scheme, id_value,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        p_target_tenant_id,
        gen_random_uuid(), 0, m.party_uuid, 'LEI', m.lei,
        current_user, current_user, 'system.external_data_import',
        'Imported from GLEIF LEI dataset: ' || v_dataset_name
    from lei_party_subtree m;

    get diagnostics v_inserted_identifiers = row_count;

    -- Insert party identifiers (BIC scheme)
    if v_bic_dataset_id is not null then
        insert into ores_refdata_party_identifiers_tbl (
            tenant_id,
            id, version, party_id, id_scheme, id_value,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            gen_random_uuid(), 0, m.party_uuid, 'BIC', b.bic,
            current_user, current_user, 'system.external_data_import',
            'Imported from GLEIF LEI-BIC dataset'
        from lei_party_subtree m
        join ores_dq_lei_bic_artefact_tbl b
            on b.lei = m.lei
            and b.dataset_id = v_bic_dataset_id;

        get diagnostics v_inserted_bic_identifiers = row_count;
    end if;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted_parties
    where v_inserted_parties > 0
    union all
    select 'inserted_identifiers'::text, v_inserted_identifiers
    where v_inserted_identifiers > 0
    union all
    select 'inserted_bic_identifiers'::text, v_inserted_bic_identifiers
    where v_inserted_bic_identifiers > 0;
end;
$$ language plpgsql security definer;
