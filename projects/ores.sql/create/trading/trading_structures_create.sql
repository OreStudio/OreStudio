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
    "party_id" uuid not null,
    "counterparty_id" uuid not null,
    "kind" text not null,
    "template_code" text null,
    "parent_structure_id" uuid null,
    primary key (tenant_id, id),
    check ("id" <> ores_utility_nil_uuid_fn()),
    constraint ores_trading_structures_parent_structure_id_fk foreign key ("tenant_id", "parent_structure_id") references "ores_trading_structures_tbl" ("tenant_id", "id")
);



create or replace function ores_trading_structures_insert_fn()
returns trigger as $$
declare
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


    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_structures_insert_trg
before insert on "ores_trading_structures_tbl"
for each row execute function ores_trading_structures_insert_fn();

create or replace function ores_trading_structures_immutable_fn()
returns trigger as $$
begin
    -- A tenant purge is the one sanctioned delete. It turns the switch on
    -- for its own transaction and off again after its delete.
    if TG_OP = 'DELETE' and ores_utility_immutable_purge_allowed_fn() then
        return OLD;
    end if;
    raise exception 'ores_trading_structures_tbl rows are immutable: % is refused.', TG_OP
        using errcode = '55000';
end;
$$ language plpgsql set search_path = public, pg_temp;

create or replace trigger ores_trading_structures_immutable_trg
before update or delete on "ores_trading_structures_tbl"
for each row execute function ores_trading_structures_immutable_fn();

create or replace trigger ores_trading_structures_immutable_truncate_trg
before truncate on "ores_trading_structures_tbl"
for each statement execute function ores_trading_structures_immutable_fn();

