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
-- Tenant Search Function
-- =============================================================================
-- The tenant roster's read: one page of the tenants a deployment holds, the
-- ones that match a search and two filters, and how many match in all.
--
-- The system tenant is the deployment's own bookkeeping, not a tenant somebody
-- set up, so it is never in the answer. A row's identity is its id; the
-- tenant_id column of this system-scoped table names the registry's owner.
--
-- Parameters:
--   p_search: text found, without regard to case, anywhere in the code, the
--             name or the hostname; empty matches every tenant. It is plain
--             text: % and _ are characters, not wildcards.
--   p_type:   a tenant type code to keep, or empty for every type
--   p_status: a tenant status code to keep, or empty for every status
--   p_exclude_type: a tenant type code to leave out, or empty to leave none out
--   p_limit:  the most rows to return
--   p_offset: how many matching rows to skip, in code order
--
-- Returns the page's rows, each carrying the total number of matches. A page
-- past the last match returns one row whose id is null, so the total still
-- arrives: a reader on a page that emptied under them learns how many tenants
-- there are, not that there are none.

-- The function gained a parameter; the old signature would make a call ambiguous.
drop function if exists ores_iam_tenants_search_fn(text, text, text, integer, integer);

create or replace function ores_iam_tenants_search_fn(
    p_search text default '',
    p_type   text default '',
    p_status text default '',
    p_limit  integer default 100,
    p_offset integer default 0,
    p_exclude_type text default ''
)
returns table (
    id                      uuid,
    version                 integer,
    code                    text,
    name                    text,
    type                    text,
    description             text,
    hostname                text,
    status                  text,
    is_registration_default boolean,
    modified_by             text,
    performed_by            text,
    change_reason_code      text,
    change_commentary       text,
    valid_from              timestamp with time zone,
    total                   bigint
) as $$
begin
    return query
    with matched as (
        select t.*
        from ores_iam_tenants_tbl t
        where t.valid_to = ores_utility_infinity_timestamp_fn()
          and t.id <> ores_utility_system_tenant_id_fn()
          and (p_type = '' or t.type = p_type)
          and (p_status = '' or t.status = p_status)
          and (p_exclude_type = '' or t.type <> p_exclude_type)
          and (
              p_search = ''
              or strpos(lower(t.code), lower(p_search)) > 0
              or strpos(lower(t.name), lower(p_search)) > 0
              or strpos(lower(t.hostname), lower(p_search)) > 0
          )
    ),
    counted as (
        select count(*) as total from matched
    )
    select
        m.id,
        m.version,
        m.code,
        m.name,
        m.type,
        m.description,
        m.hostname,
        m.status,
        m.is_registration_default,
        m.modified_by,
        m.performed_by,
        m.change_reason_code,
        m.change_commentary,
        m.valid_from,
        c.total
    from counted c
    left join lateral (
        select *
        from matched
        order by matched.code
        limit p_limit
        offset p_offset
    ) m on true
    order by m.code;
end;
$$ language plpgsql stable;
