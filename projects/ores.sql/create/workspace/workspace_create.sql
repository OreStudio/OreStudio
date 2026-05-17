/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software: redistribute under GPLv3 or later.
 *
 */

-- =============================================================================
-- Workspaces table.
-- Each workspace is a named, isolated data context. Workspace 0 is Live (always
-- exists, cannot be deleted or archived). parent_workspace_id enables Docker-
-- layer-style inheritance: data not present in a workspace is resolved from the
-- parent, then grandparent, up to workspace 0.
--
-- scope_portfolio_id is a soft FK to ores_refdata_portfolios_tbl(id) — no hard
-- DB constraint because the workspace table is created before refdata to allow
-- refdata tables to hold a hard FK back to this table.
-- =============================================================================

create table if not exists ores_workspaces_tbl (
    id                  integer      not null generated always as identity primary key,
    name                text         not null,
    description         text         not null default '',
    source_path         text         not null default '',
    parent_workspace_id integer      null references ores_workspaces_tbl(id),
    scope_portfolio_id  uuid         null,
    created_at          timestamptz  not null default now(),
    created_by          text         not null,
    status              text         not null default 'active',
    constraint ores_workspaces_name_uq unique (name),
    constraint ores_workspaces_no_self_parent check (id != parent_workspace_id),
    constraint ores_workspaces_status_chk check (status in ('active', 'archived'))
);

create index if not exists workspaces_status_idx
on ores_workspaces_tbl (status);

create index if not exists workspaces_parent_idx
on ores_workspaces_tbl (parent_workspace_id)
where parent_workspace_id is not null;

-- Seed the live workspace. Override the identity sequence to force id = 0.
insert into ores_workspaces_tbl (id, name, description, created_by)
overriding system value
values (0, 'Live', 'Live production data space', 'system');

-- =============================================================================
-- Trade scope whitelist. Rows present → only listed trade IDs are visible in
-- this workspace (must also be in scope via scope_portfolio_id). No rows →
-- all trades reachable via the portfolio scope are visible.
-- =============================================================================

create table if not exists ores_workspace_trade_scope_tbl (
    workspace_id  integer  not null references ores_workspaces_tbl(id),
    trade_id      uuid     not null,
    primary key (workspace_id, trade_id)
);

-- =============================================================================
-- Cycle-prevention trigger. Prevents a parent_workspace_id update that would
-- create a cycle in the workspace inheritance chain.
-- =============================================================================

create or replace function ores_workspaces_prevent_cycle_fn()
returns trigger as $$
declare
    v_ancestor_id integer;
begin
    if NEW.parent_workspace_id is null then
        return NEW;
    end if;
    if NEW.parent_workspace_id = NEW.id then
        raise exception 'Workspace cannot be its own parent'
            using errcode = '23514';
    end if;
    -- Walk the parent chain and check for cycles.
    v_ancestor_id := NEW.parent_workspace_id;
    while v_ancestor_id is not null loop
        if v_ancestor_id = NEW.id then
            raise exception 'Cycle detected in workspace parent chain'
                using errcode = '23514';
        end if;
        select parent_workspace_id into v_ancestor_id
        from ores_workspaces_tbl
        where id = v_ancestor_id;
    end loop;
    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_workspaces_cycle_prevention_trg
before insert or update of parent_workspace_id
on ores_workspaces_tbl
for each row execute function ores_workspaces_prevent_cycle_fn();

-- =============================================================================
-- Resolution order function. Returns the ancestor chain as an integer array
-- starting from p_workspace_id up to the root (workspace 0).
-- =============================================================================

create or replace function ores_workspace_resolution_order_fn(p_workspace_id integer)
returns integer[] language sql stable as $$
    with recursive chain(id, depth) as (
        select id, 0
        from ores_workspaces_tbl
        where id = p_workspace_id
        union all
        select w.parent_workspace_id, c.depth + 1
        from ores_workspaces_tbl w
        join chain c on c.id = w.id
        where w.parent_workspace_id is not null
    )
    select array_agg(id order by depth) from chain
$$;
