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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * Portfolio Table
 *
 * Represents organizational, risk, or reporting groupings.
 * Never holds trades directly. Supports hierarchical structure
 * via self-referencing parent_portfolio_id.
 */

create table if not exists "ores_refdata_portfolios_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "name" text not null,
    "description" text null,
    "parent_portfolio_id" uuid null,
    "owner_unit_id" uuid null,
    "purpose_type" text not null,
    "aggregation_ccy" text null,
    "is_virtual" integer not null,
    "status" text not null,
    "workspace_id" uuid not null default ores_utility_live_workspace_id_fn(), -- soft FK to ores_workspaces_tbl(id)
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Composite natural key: unique combination for active records
create unique index if not exists portfolios_party_id_name_uniq_idx
on "ores_refdata_portfolios_tbl" (tenant_id, party_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists portfolios_version_uniq_idx
on "ores_refdata_portfolios_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists portfolios_id_uniq_idx
on "ores_refdata_portfolios_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists portfolios_tenant_idx
on "ores_refdata_portfolios_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists portfolios_workspace_idx
on "ores_refdata_portfolios_tbl" (workspace_id)
where valid_to = ores_utility_infinity_timestamp_fn();


-- =============================================================================
-- Touch-version function for portfolio
-- Bumps this entity's version without changing any of its own columns,
-- called by a child entity's insert trigger / delete rule when that
-- child is flagged :bump_parent_version: true against this entity. See
-- the "Temporal composite entity versioning" architecture doc.
--
-- Deliberately does NOT duplicate the version/valid_from/valid_to
-- management here: it fetches the current row, resets version to the
-- 0 sentinel, and re-inserts — the table's own insert trigger (below)
-- then performs the exact same close-current/bump-version dance a
-- normal application save does. This also sidesteps a real footgun:
-- current_timestamp is frozen for the whole transaction, so a
-- same-transaction bulk import (parent + child created together, as
-- GLEIF provisioning does) would otherwise make the just-inserted
-- parent row's own valid_from collide with this function's close
-- timestamp.
--
-- Defined before the insert trigger/delete rule below (not after):
-- for a self-referencing entity (this entity is its own composite
-- parent, e.g. a portfolio tree), the delete rule calling this
-- function lives in the *same* file. PostgreSQL parses CREATE RULE
-- eagerly, so the function must already exist by then -- unlike a
-- PL/pgSQL function body (e.g. the insert trigger function), which is
-- opaque at CREATE time and only resolves calls at execution.
--
-- p_reason_code is passed through as-is (already a validated code from
-- the child's own row); p_child_entity distinguishes which child
-- triggered the bump in the free-text commentary.
-- =============================================================================
create or replace function ores_refdata_portfolios_touch_version_fn(
    p_tenant_id uuid,
    p_id uuid,
    p_reason_code text,
    p_commentary text,
    p_modified_by text,
    p_performed_by text,
    p_child_entity text
) returns void as $$
declare
    rec ores_refdata_portfolios_tbl%rowtype;
begin
    -- for update: takes the same row lock the parent's own insert
    -- trigger takes, so the snapshot in rec can't be based on a
    -- business-column value a concurrent direct edit is about to
    -- change — without this, that concurrent edit could be silently
    -- reverted once this function's later insert proceeds. See the
    -- "Temporal composite entity versioning" architecture doc,
    -- Concurrency section.
    select * into rec
    from "ores_refdata_portfolios_tbl"
    where tenant_id = p_tenant_id
      and id = p_id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if not found then
        return;
    end if;

    rec.version := 0;
    rec.modified_by := p_modified_by;
    rec.performed_by := p_performed_by;
    rec.change_reason_code := p_reason_code;
    rec.change_commentary := format('Bumped by child %s: %s', p_child_entity, coalesce(p_commentary, ''));

    insert into "ores_refdata_portfolios_tbl"
    select (rec).*;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace function ores_refdata_portfolios_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate workspace_id
    NEW.workspace_id := ores_workspace_validate_fn(NEW.workspace_id);

    -- Validate parent_portfolio_id (optional soft FK to ores_refdata_portfolios_tbl)
    if NEW.parent_portfolio_id is not null then
        if not exists (
            select 1 from ores_refdata_portfolios_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.parent_portfolio_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid parent_portfolio_id: %. No active portfolio found with this id.', NEW.parent_portfolio_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Bump ores_refdata_portfolios_tbl's version alongside this write (composite entity
    -- versioning — see the "Temporal composite entity versioning"
    -- architecture doc). The touch function re-validates modified_by
    -- itself; change_reason_code is passed through as-is since it was
    -- already validated above.
    perform ores_refdata_portfolios_touch_version_fn(
        NEW.tenant_id,
        NEW.parent_portfolio_id,
        NEW.change_reason_code,
        NEW.change_commentary,
        NEW.modified_by,
        NEW.performed_by,
        'portfolio'
    );

    -- Version management
    select version into current_version
    from "ores_refdata_portfolios_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_portfolios_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_refdata_portfolios_insert_trg
before insert on "ores_refdata_portfolios_tbl"
for each row execute function ores_refdata_portfolios_insert_fn();

create or replace rule ores_refdata_portfolios_delete_rule as
on delete to "ores_refdata_portfolios_tbl" do instead (
    update "ores_refdata_portfolios_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
    -- Bump ores_refdata_portfolios_tbl's version on delete too (composite entity
    -- versioning), symmetric with the insert-side call above.
    select ores_refdata_portfolios_touch_version_fn(
        OLD.tenant_id,
        OLD.parent_portfolio_id,
        OLD.change_reason_code,
        OLD.change_commentary,
        OLD.modified_by,
        OLD.performed_by,
        'portfolio'
    );
);
