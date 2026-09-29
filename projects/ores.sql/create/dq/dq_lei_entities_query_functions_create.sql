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
 * LEI Entity Query Functions
 *
 * Functions for querying the LEI entity artefact table.
 * Used by the DQ service to power the GLEIF counterparty picker UI.
 *
 * Both functions return only root entities: those that are NOT the child
 * (start_node) of an active IS_DIRECTLY_CONSOLIDATED_BY relationship.
 * This excludes subsidiaries and branches, showing only top-level legal
 * entities to the user.
 *
 * DISTINCT ON (lei) is used throughout to deduplicate: the artefact table
 * accumulates one row per (dataset_id, lei) across publications, so the
 * same LEI can appear multiple times.
 */

-- =============================================================================
-- Distinct Countries Function
-- =============================================================================

/**
 * Returns distinct countries present in the LEI entities artefact for the
 * current tenant or the system tenant, considering root entities only.
 * Used to populate the country filter dropdown in the UI.
 *
 * LEI data is global reference data loaded under the system tenant, so
 * both the current tenant and the system tenant are included.
 */
create or replace function ores_dq_lei_entities_distinct_countries_fn()
returns table (
    country text
) as $$
begin
    return query
    select distinct deduped.entity_legal_address_country
    from (
        select distinct on (e.lei)
            e.entity_legal_address_country
        from ores_dq_lei_entities_artefact_tbl e
        where e.tenant_id in (
                  ores_iam_current_tenant_id_fn(),
                  ores_utility_system_tenant_id_fn()
              )
          and e.entity_entity_status = 'ACTIVE'
          and not exists (
              select 1
              from ores_dq_lei_relationships_artefact_tbl r
              where r.tenant_id in (
                        ores_iam_current_tenant_id_fn(),
                        ores_utility_system_tenant_id_fn()
                    )
                and r.relationship_start_node_node_id = e.lei
                and r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
                and r.relationship_relationship_status = 'ACTIVE'
          )
        order by e.lei
    ) deduped
    order by deduped.entity_legal_address_country;
end;
$$ language plpgsql stable;

-- =============================================================================
-- Summary by Country Function
-- =============================================================================

/**
 * Returns LEI entity summaries for a given country with pagination support.
 * Returns only root entities (no subsidiaries) deduplicated by LEI code.
 *
 * @param p_country  ISO 3166-1 alpha-2 country code to filter by
 * @param p_limit    Maximum number of records to return
 * @param p_offset   Number of records to skip
 */
create or replace function ores_dq_lei_entities_summary_by_country_fn(
    p_country text,
    p_limit   integer default 1000,
    p_offset  integer default 0
)
returns table (
    lei                  text,
    entity_legal_name    text,
    entity_category      text,
    country              text
) as $$
begin
    return query
    select
        deduped.lei,
        deduped.entity_legal_name,
        deduped.entity_entity_category,
        deduped.entity_legal_address_country
    from (
        select distinct on (e.lei)
            e.lei,
            e.entity_legal_name,
            e.entity_entity_category,
            e.entity_legal_address_country
        from ores_dq_lei_entities_artefact_tbl e
        where e.tenant_id in (
                  ores_iam_current_tenant_id_fn(),
                  ores_utility_system_tenant_id_fn()
              )
          and e.entity_legal_address_country = p_country
          and e.entity_entity_status = 'ACTIVE'
          and not exists (
              select 1
              from ores_dq_lei_relationships_artefact_tbl r
              where r.tenant_id in (
                        ores_iam_current_tenant_id_fn(),
                        ores_utility_system_tenant_id_fn()
                    )
                and r.relationship_start_node_node_id = e.lei
                and r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
                and r.relationship_relationship_status = 'ACTIVE'
          )
        order by e.lei
    ) deduped
    order by deduped.entity_legal_name
    limit p_limit
    offset p_offset;
end;
$$ language plpgsql stable;

-- =============================================================================
-- Search Function
-- =============================================================================

/**
 * Returns the root legal entities whose name or LEI matches a search, each with
 * the number of parties importing its hierarchy would create.
 *
 * This is the read a search uses. Both filters are optional and an empty one
 * means every value, because a person looking for the entity a tenant is built
 * around knows its name, not the page it happens to fall on: the deployment
 * holds tens of thousands of entities, so a page over them answers the wrong
 * question. The LEI is matched from its start, which is how somebody who knows
 * it types it, and the name anywhere in it.
 *
 * The count walks the same IS_DIRECTLY_CONSOLIDATED_BY relationships the
 * publication walks, so a starting point can state how much work choosing an
 * entity is before the choice is made. It counts the entities the deployment
 * holds, and an entity a relationship names but the deployment does not hold is
 * not a party the import would create.
 */
create or replace function ores_dq_lei_entities_search_fn(
    p_search  text default '',
    p_country text default '',
    p_limit   integer default 20,
    p_offset  integer default 0
)
returns table (
    lei               text,
    entity_legal_name text,
    entity_category   text,
    country           text,
    party_count       bigint
) as $$
begin
    return query
    with recursive matched as (
        select distinct on (e.lei)
            e.lei,
            e.entity_legal_name,
            e.entity_entity_category,
            e.entity_legal_address_country
        from ores_dq_lei_entities_artefact_tbl e
        where e.tenant_id in (
                  ores_iam_current_tenant_id_fn(),
                  ores_utility_system_tenant_id_fn()
              )
          and e.entity_entity_status = 'ACTIVE'
          and (p_country = '' or e.entity_legal_address_country = p_country)
          and (
              p_search = ''
              or e.entity_legal_name ilike '%' || p_search || '%'
              or e.lei ilike p_search || '%'
          )
          and not exists (
              select 1
              from ores_dq_lei_relationships_artefact_tbl r
              where r.tenant_id in (
                        ores_iam_current_tenant_id_fn(),
                        ores_utility_system_tenant_id_fn()
                    )
                and r.relationship_start_node_node_id = e.lei
                and r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
                and r.relationship_relationship_status = 'ACTIVE'
          )
        order by e.lei
    ),
    page as (
        select m.lei,
               m.entity_legal_name,
               m.entity_entity_category,
               m.entity_legal_address_country
        from matched m
        order by m.entity_legal_name, m.lei
        limit p_limit
        offset p_offset
    ),
    subtree as (
        select p.lei as node, p.lei as root
        from page p

        union

        select distinct r.relationship_start_node_node_id, s.root
        from ores_dq_lei_relationships_artefact_tbl r
        join subtree s on s.node = r.relationship_end_node_node_id
        where r.tenant_id in (
                  ores_iam_current_tenant_id_fn(),
                  ores_utility_system_tenant_id_fn()
              )
          and r.relationship_relationship_type = 'IS_DIRECTLY_CONSOLIDATED_BY'
          and r.relationship_relationship_status = 'ACTIVE'
    ),
    sizes as (
        select s.root, count(*) as party_count
        from subtree s
        where exists (
            select 1
            from ores_dq_lei_entities_artefact_tbl e
            where e.lei = s.node
              and e.tenant_id in (
                        ores_iam_current_tenant_id_fn(),
                        ores_utility_system_tenant_id_fn()
                    )
              and e.entity_entity_status = 'ACTIVE'
        )
        group by s.root
    )
    select p.lei,
           p.entity_legal_name,
           p.entity_entity_category,
           p.entity_legal_address_country,
           coalesce(sizes.party_count, 0)
    from page p
    left join sizes on sizes.root = p.lei
    order by p.entity_legal_name, p.lei;
end;
$$ language plpgsql stable;
