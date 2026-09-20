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
 * Permission Table
 *
 * An atomic permission that can be granted to roles. Permissions follow the
 * hierarchical naming convention component::resource:action (for example
 * iam::accounts:create; "*" grants everything and component::* grants
 * every action within one component).
 *
 * The table is temporal (see
 * projects/ores.sql/create/iam/iam_permissions_create.sql): it carries
 * valid_from/valid_to, the GIST exclusion and the delete rule, but it has
 * no version column and no audit tail -- permissions are system-defined
 * constants seeded from bootstrap data, not user-editable records, so they
 * need no change tracking. The :no_audit_columns: flag in the * SQL **
 * Flags drawer selects exactly that shape: it drops the version column and
 * the four audit columns while keeping the transaction-time window. The
 * :skip_uuid_check: suppression on id drops the nil-UUID check the
 * hand-written table never had.
 */

create table if not exists "ores_iam_permissions_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "description" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Unique code for active records
create unique index if not exists permissions_code_uniq_idx
on "ores_iam_permissions_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();


create unique index if not exists permissions_id_uniq_idx
on "ores_iam_permissions_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists permissions_tenant_idx
on "ores_iam_permissions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_iam_permissions_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Close the current active row before inserting the new one
    update "ores_iam_permissions_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
      and valid_from < clock_timestamp();

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_iam_permissions_insert_trg
before insert on "ores_iam_permissions_tbl"
for each row execute function ores_iam_permissions_insert_fn();

create or replace rule ores_iam_permissions_delete_rule as
on delete to "ores_iam_permissions_tbl" do instead (
    update "ores_iam_permissions_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
