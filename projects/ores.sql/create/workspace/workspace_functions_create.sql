/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software: redistribute under GPLv3 or later.
 *
 */

-- =============================================================================
-- Workspace functions the entity generator cannot express.
-- =============================================================================

-- =============================================================================
-- Resolution order function. Returns the UUID ancestor chain starting from
-- p_workspace_id up to the Live root workspace.
-- =============================================================================

create or replace function ores_workspace_resolution_order_fn(
    p_workspace_id uuid,
    p_tenant_id    uuid
) returns uuid[] language sql stable as $$
    with recursive chain(id, depth) as (
        select id, 0
        from ores_workspaces_tbl
        where id = p_workspace_id
          and tenant_id = p_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select w.parent_workspace_id, c.depth + 1
        from ores_workspaces_tbl w
        join chain c on c.id = w.id
        where w.tenant_id = p_tenant_id
          and w.parent_workspace_id is not null
          and w.valid_to = ores_utility_infinity_timestamp_fn()
    )
    select array_agg(id order by depth) from chain
$$;

-- =============================================================================
-- Workspace FK validation function. Called from BEFORE INSERT triggers on any
-- table that carries a workspace_id column. Defined SECURITY DEFINER because
-- service users hold no SELECT on ores_workspaces_tbl (service table isolation).
-- The Live sentinel UUID is accepted unconditionally: one row exists per tenant
-- but workspace-aware triggers call this without tenant context.
-- =============================================================================

create or replace function ores_workspace_validate_fn(p_workspace_id uuid)
returns uuid
language plpgsql
stable
security definer
set search_path = public, pg_temp
as $$
begin
    -- Live sentinel is always valid; each tenant has one Live row but triggers
    -- call this without tenant context, so we short-circuit unconditionally.
    if p_workspace_id = ores_utility_live_workspace_id_fn() then
        return p_workspace_id;
    end if;

    if not exists (
        select 1 from ores_workspaces_tbl
        where id = p_workspace_id
          and status_code = 'active'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'workspace_id % does not reference an active workspace',
            p_workspace_id
            using errcode = '23503';
    end if;

    return p_workspace_id;
end;
$$;
