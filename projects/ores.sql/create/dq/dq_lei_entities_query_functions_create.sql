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
 */

-- =============================================================================
-- Distinct Countries Function
-- =============================================================================

/**
 * Returns distinct countries present in the LEI entities artefact for the
 * current tenant. Used to populate the country filter dropdown in the UI.
 */
create or replace function ores_dq_lei_entities_distinct_countries_fn()
returns table (
    country text
) as $$
begin
    return query
    select distinct entity_legal_address_country
    from ores_dq_lei_entities_artefact_tbl
    where tenant_id = ores_iam_current_tenant_id_fn()
    order by entity_legal_address_country;
end;
$$ language plpgsql stable;

-- =============================================================================
-- Summary by Country Function
-- =============================================================================

/**
 * Returns LEI entity summaries for a given country with pagination support.
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
        e.lei,
        e.entity_legal_name,
        e.entity_entity_category,
        e.entity_legal_address_country
    from ores_dq_lei_entities_artefact_tbl e
    where e.tenant_id = ores_iam_current_tenant_id_fn()
      and e.entity_legal_address_country = p_country
    order by e.entity_legal_name
    limit p_limit
    offset p_offset;
end;
$$ language plpgsql stable;
