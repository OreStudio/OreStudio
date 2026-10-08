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
 * Structure Table
 *
 * The immutable identity of a deal assembled from several trades. It holds
 * what never changes for the deal's life: the party, the counterparty, the
 * kind of binding, the template it was shaped by, and its parent when it is
 * itself a leg of a larger deal.
 *
 * It is the anchor of the composition ladder. The legs are not owned
 * children: a trade points at the structure that holds it, so the same trade
 * can be unlinked from one deal and linked to another without being
 * rewritten. The economics live on the legs, and the structure's internal
 * version is read from them rather than stored beside them.
 *
 * A structure nests one level at most, because a deal inside a deal inside a
 * deal has no confirmation to hang on. The insert trigger enforces it.
 */

create table if not exists "ores_trading_structures_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "counterparty_id" uuid not null,
    "kind" text not null,
    "template_code" text null,
    "parent_structure_id" uuid null,
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

-- Version uniqueness for optimistic concurrency
create unique index if not exists structures_version_uniq_idx
on "ores_trading_structures_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists structures_id_uniq_idx
on "ores_trading_structures_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists structures_tenant_idx
on "ores_trading_structures_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_structures_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate kind (soft FK to ores_trading_structure_kinds_tbl)
    if not exists (
        select 1 from ores_trading_structure_kinds_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.kind
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid kind: %. No active structure kind found with this code.', NEW.kind
            using errcode = '23503';
    end if;

    -- Validate template_code (optional soft FK to ores_trading_structure_templates_tbl)
    if NEW.template_code is not null then
        if not exists (
            select 1 from ores_trading_structure_templates_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and code = NEW.template_code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid template_code: %. No active structure template found with this code.', NEW.template_code
                using errcode = '23503';
        end if;
    end if;

    -- Validate parent_structure_id (optional soft FK to ores_trading_structures_tbl)
    if NEW.parent_structure_id is not null then
        if not exists (
            select 1 from ores_trading_structures_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.parent_structure_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid parent_structure_id: %. No active structure found with this id.', NEW.parent_structure_id
                using errcode = '23503';
        end if;
    end if;

    -- Structures nest one level at most. A structure that has a parent is a
    -- leg of a larger deal, and a leg carries no legs of its own: the
    -- confirmation hangs on the deal the customer sees, and there is no
    -- second deal above it to confirm.
    if NEW.parent_structure_id is not null and exists (
        select 1 from ores_trading_structures_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.parent_structure_id
          and parent_structure_id is not null
    ) then
        raise exception 'Invalid parent_structure_id: %. The parent is itself a leg, and structures nest one level at most.',
            NEW.parent_structure_id
            using errcode = '23514';
    end if;

    -- A structure is not its own parent.
    if NEW.parent_structure_id = NEW.id then
        raise exception 'Invalid parent_structure_id: %. A structure cannot be its own parent.',
            NEW.parent_structure_id
            using errcode = '23514';
    end if;
    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_trading_structures_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        -- The write states what it believes about the row, and the store is
        -- what decides. Version zero means one thing: no current row exists.
        -- So a create that collides with a live row is refused here, for every
        -- client, rather than by a check each client has to remember.
        if NEW.version = 0 then
            if not ores_utility_version_replace_allowed_fn() then
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'structure',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'structure',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_structures_tbl"
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
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_structures_insert_trg
before insert on "ores_trading_structures_tbl"
for each row execute function ores_trading_structures_insert_fn();

create or replace rule ores_trading_structures_delete_rule as
on delete to "ores_trading_structures_tbl" do instead (
    update "ores_trading_structures_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Structure
-- =============================================================================
alter table ores_trading_structures_tbl enable row level security;

drop policy if exists structures_tbl_tenant_isolation_policy
    on ores_trading_structures_tbl;

create policy structures_tbl_tenant_isolation_policy
on ores_trading_structures_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
