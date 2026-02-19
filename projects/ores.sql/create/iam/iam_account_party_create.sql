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
 * Template: sql_schema_junction_create.mustache
 * To modify, update the template and regenerate.
 *
 * Account Party Table
 *
 * Junction table linking IAM accounts to parties. Each account can be
 * associated with one or more parties, controlling which parties a user
 * can act on behalf of.
 */

create table if not exists "ores_iam_account_parties_tbl" (
    "account_id" uuid not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "version" integer not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, account_id, party_id, valid_from),
    exclude using gist (
        tenant_id WITH =,
        account_id WITH =,
        party_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for looking up parties for an account
create index if not exists ores_iam_account_parties_account_idx
on "ores_iam_account_parties_tbl" (account_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding accounts associated with a party
create index if not exists ores_iam_account_parties_party_idx
on "ores_iam_account_parties_tbl" (party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists ores_iam_account_parties_uniq_idx
on "ores_iam_account_parties_tbl" (tenant_id, account_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_iam_account_parties_tenant_idx
on "ores_iam_account_parties_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_iam_account_parties_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Validate account_id (soft FK)
    if not exists (
        select 1 from ores_iam_accounts_tbl
        where tenant_id = new.tenant_id and id = new.account_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid account_id: %. No account found with this id.',
            new.account_id
            using errcode = '23503';
    end if;

    -- Validate party_id (soft FK)
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = new.tenant_id and id = new.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_id: %. No active party found with this id.',
            new.party_id
            using errcode = '23503';
    end if;

    -- Version management
    select version into current_version
    from "ores_iam_account_parties_tbl"
    where tenant_id = new.tenant_id
    and account_id = new.account_id
    and party_id = new.party_id
    and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        -- Close existing record
        update "ores_iam_account_parties_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
        and account_id = new.account_id
        and party_id = new.party_id
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = current_user;

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_iam_account_parties_insert_trg
before insert on "ores_iam_account_parties_tbl"
for each row
execute function ores_iam_account_parties_insert_fn();

create or replace rule ores_iam_account_parties_delete_rule as
on delete to "ores_iam_account_parties_tbl"
do instead
  update "ores_iam_account_parties_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and account_id = old.account_id
  and party_id = old.party_id
  and valid_to = ores_utility_infinity_timestamp_fn();
