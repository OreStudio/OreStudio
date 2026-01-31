/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
-- Many-to-many: accounts to roles.
-- Tracks who assigned the role.
-- =============================================================================

create table if not exists ores_iam_account_roles_tbl (
    "tenant_id" uuid not null,
    "account_id" uuid not null,
    "role_id" uuid not null,
    "assigned_by" text not null,
    "assigned_at" timestamp with time zone not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (account_id, role_id, valid_from),
    exclude using gist (
        account_id WITH =,
        role_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("change_reason_code" <> '')
);

create index if not exists ores_iam_account_roles_tenant_idx
on ores_iam_account_roles_tbl (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_iam_account_roles_account_idx
on ores_iam_account_roles_tbl (account_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_iam_account_roles_role_idx
on ores_iam_account_roles_tbl (role_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_iam_account_roles_insert_fn()
returns trigger as $$
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    update ores_iam_account_roles_tbl
    set valid_to = current_timestamp
    where account_id = new.account_id
    and role_id = new.role_id
    and valid_to = ores_utility_infinity_timestamp_fn()
    and valid_from < current_timestamp;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();
    new.assigned_at = current_timestamp;
    if new.assigned_by is null or new.assigned_by = '' then
        new.assigned_by = current_user;
    end if;

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_iam_account_roles_insert_trg
before insert on ores_iam_account_roles_tbl
for each row
execute function ores_iam_account_roles_insert_fn();

create or replace rule ores_iam_account_roles_delete_rule as
on delete to ores_iam_account_roles_tbl
do instead
  update ores_iam_account_roles_tbl
  set valid_to = current_timestamp
  where account_id = old.account_id
  and role_id = old.role_id
  and valid_to = ores_utility_infinity_timestamp_fn();
