/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software: redistribute under GPLv3 or later.
 *
 */

-- =============================================================================
-- Workspaces table (bitemporal).
-- Each workspace is a named, isolated data context. The Live workspace uses
-- the sentinel UUID ores_utility_live_workspace_id_fn() and cannot be
-- archived. parent_workspace_id enables Docker-layer-style inheritance.
--
-- scope_portfolio_id is a soft FK to ores_refdata_portfolios_tbl(id) — no
-- hard DB constraint; workspace is created before refdata so refdata can hold
-- a hard FK back to this table.
-- =============================================================================

create table if not exists ores_workspaces_tbl (
    id                  uuid         not null,
    version             integer      not null,
    name                text         not null,
    description         text         not null default '',
    source_path         text         not null default '',
    parent_workspace_id uuid         null,
    scope_portfolio_id  uuid         null,
    owner_id            uuid         not null,
    status_code         text         not null default 'active',
    modified_by         text         not null,
    performed_by        text         not null,
    change_reason_code  text         not null,
    change_commentary   text         not null,
    valid_from          timestamptz  not null,
    valid_to            timestamptz  not null,
    primary key (id, valid_from, valid_to),
    exclude using gist (
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check (valid_from < valid_to),
    check (id <> ores_utility_nil_uuid_fn()),
    check (name <> ''),
    check (status_code in ('active', 'archived')),
    check (id <> parent_workspace_id)
);

create unique index if not exists workspaces_id_uniq_idx
on ores_workspaces_tbl (id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists workspaces_version_uniq_idx
on ores_workspaces_tbl (id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists workspaces_name_uniq_idx
on ores_workspaces_tbl (name)
where valid_to = ores_utility_infinity_timestamp_fn()
  and status_code = 'active';

create index if not exists workspaces_status_idx
on ores_workspaces_tbl (status_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists workspaces_owner_idx
on ores_workspaces_tbl (owner_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Insert trigger: manages version, valid_from/valid_to, and audit fields.
-- =============================================================================

create or replace function ores_workspaces_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(null, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from ores_workspaces_tbl
    where id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update ores_workspaces_tbl
        set valid_to = current_timestamp
        where id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_workspaces_insert_trg
before insert on ores_workspaces_tbl
for each row execute function ores_workspaces_insert_fn();

-- Soft-delete: archive (set valid_to = now) rather than physically delete
create or replace rule ores_workspaces_delete_rule as
on delete to ores_workspaces_tbl do instead
    update ores_workspaces_tbl
    set valid_to = current_timestamp
    where id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Cycle-prevention trigger. Prevents a parent_workspace_id update that would
-- create a cycle in the workspace inheritance chain.
-- =============================================================================

create or replace function ores_workspaces_prevent_cycle_fn()
returns trigger as $$
declare
    v_ancestor_id uuid;
begin
    if NEW.parent_workspace_id is null then
        return NEW;
    end if;
    if NEW.parent_workspace_id = NEW.id then
        raise exception 'Workspace cannot be its own parent'
            using errcode = '23514';
    end if;
    v_ancestor_id := NEW.parent_workspace_id;
    while v_ancestor_id is not null loop
        if v_ancestor_id = NEW.id then
            raise exception 'Cycle detected in workspace parent chain'
                using errcode = '23514';
        end if;
        select parent_workspace_id into v_ancestor_id
        from ores_workspaces_tbl
        where id = v_ancestor_id
          and valid_to = ores_utility_infinity_timestamp_fn();
    end loop;
    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_workspaces_cycle_prevention_trg
before insert or update of parent_workspace_id
on ores_workspaces_tbl
for each row execute function ores_workspaces_prevent_cycle_fn();

-- =============================================================================
-- Resolution order function. Returns the UUID ancestor chain starting from
-- p_workspace_id up to the Live root workspace.
-- =============================================================================

create or replace function ores_workspace_resolution_order_fn(p_workspace_id uuid)
returns uuid[] language sql stable as $$
    with recursive chain(id, depth) as (
        select id, 0
        from ores_workspaces_tbl
        where id = p_workspace_id
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select w.parent_workspace_id, c.depth + 1
        from ores_workspaces_tbl w
        join chain c on c.id = w.id
        where w.parent_workspace_id is not null
          and w.valid_to = ores_utility_infinity_timestamp_fn()
    )
    select array_agg(id order by depth) from chain
$$;

-- =============================================================================
-- Trade scope whitelist (references workspace UUID, not integer).
-- =============================================================================

create table if not exists ores_workspace_trade_scope_tbl (
    workspace_id  uuid  not null,
    trade_id      uuid  not null,
    primary key (workspace_id, trade_id)
);
