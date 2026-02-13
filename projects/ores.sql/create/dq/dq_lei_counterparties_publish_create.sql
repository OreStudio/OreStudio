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

    -- Derive size from dataset code (e.g. 'gleif.lei_counterparties.small' -> 'small')
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

    -- Create temp table mapping LEI codes to generated UUIDs with depth
    create temp table lei_counterparty_uuid_map (
        lei text primary key,
        counterparty_uuid uuid not null default gen_random_uuid(),
        parent_lei text null,
        depth int not null default 0,
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
    -- Only set parent when the parent entity is also in our dataset.
    update lei_counterparty_uuid_map m
    set parent_lei = r.relationship_end_node_node_id
    from ores_dq_lei_relationships_artefact_tbl r
    where r.relationship_start_node_node_id = m.lei
      and r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
      and r.relationship_relationship_status = 'ACTIVE'
      and r.dataset_id = v_rel_dataset_id
      and exists (select 1 from lei_counterparty_uuid_map p
                  where p.lei = r.relationship_end_node_node_id);

    -- Compute depth: roots (no parent_lei) are 0, children are parent+1.
    -- Iterative approach to handle arbitrary nesting.
    declare
        v_changed boolean := true;
    begin
        while v_changed loop
            update lei_counterparty_uuid_map child
            set depth = parent.depth + 1
            from lei_counterparty_uuid_map parent
            where child.parent_lei = parent.lei
              and child.depth <= parent.depth;
            v_changed := found;
        end loop;
    end;

    -- Insert counterparties level by level (roots first, then children)
    -- so that the insert trigger can validate parent_counterparty_id.
    declare
        v_current_depth int := 0;
        v_max_depth int;
        v_level_count bigint;
    begin
        select max(depth) into v_max_depth from lei_counterparty_uuid_map;

        for v_current_depth in 0..coalesce(v_max_depth, 0) loop
            insert into ores_refdata_counterparties_tbl (
                tenant_id,
                id, version, full_name, short_code, party_type,
                parent_counterparty_id, business_center_code, status,
                modified_by, performed_by, change_reason_code, change_commentary
            )
            select
                p_target_tenant_id,
                m.counterparty_uuid, 0, m.entity_legal_name, m.lei, 'Corporate',
                parent_map.counterparty_uuid,
                -- Default business centre from country
                coalesce(bc_map.business_center_code, 'WRLD'),
                'Active',
                current_user, current_user, 'system.external_data_import',
                'Imported from GLEIF LEI dataset: ' || v_dataset_name
            from lei_counterparty_uuid_map m
            left join lei_counterparty_uuid_map parent_map on parent_map.lei = m.parent_lei
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
            v_inserted_counterparties := v_inserted_counterparties + v_level_count;
        end loop;
    end;

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

    -- Insert counterparty identifiers (BIC scheme)
    if v_bic_dataset_id is not null then
        insert into ores_refdata_counterparty_identifiers_tbl (
            tenant_id,
            id, version, counterparty_id, id_scheme, id_value,
            modified_by, performed_by, change_reason_code, change_commentary
        )
        select
            p_target_tenant_id,
            gen_random_uuid(), 0, m.counterparty_uuid, 'BIC', b.bic,
            current_user, current_user, 'system.external_data_import',
            'Imported from GLEIF LEI-BIC dataset'
        from lei_counterparty_uuid_map m
        join ores_dq_lei_bic_artefact_tbl b
            on b.lei = m.lei
            and b.dataset_id = v_bic_dataset_id;

        get diagnostics v_inserted_bic_identifiers = row_count;
    end if;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted_counterparties
    where v_inserted_counterparties > 0
    union all
    select 'inserted_identifiers'::text, v_inserted_identifiers
    where v_inserted_identifiers > 0
    union all
    select 'inserted_bic_identifiers'::text, v_inserted_bic_identifiers
    where v_inserted_bic_identifiers > 0;
end;
$$ language plpgsql security definer;
