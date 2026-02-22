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
 * Party Currency Table
 *
 * Junction table controlling which currencies are visible to which parties.
 * Currency definitions are shared at tenant level; this junction controls
 * party-level visibility.
 */

create table if not exists "ores_refdata_party_currencies_tbl" (
    "party_id" uuid not null,
    "tenant_id" uuid not null,
    "currency_iso_code" text not null,
    "version" integer not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, party_id, currency_iso_code, valid_from),
    exclude using gist (
        tenant_id WITH =,
        party_id WITH =,
        currency_iso_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for looking up currencies visible to a party
create index if not exists ores_refdata_party_currencies_party_idx
on "ores_refdata_party_currencies_tbl" (party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding parties that can see a currency
create index if not exists ores_refdata_party_currencies_currency_idx
on "ores_refdata_party_currencies_tbl" (currency_iso_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists ores_refdata_party_currencies_uniq_idx
on "ores_refdata_party_currencies_tbl" (tenant_id, party_id, currency_iso_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_refdata_party_currencies_tenant_idx
on "ores_refdata_party_currencies_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_party_currencies_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Version management
    select version into current_version
    from "ores_refdata_party_currencies_tbl"
    where tenant_id = new.tenant_id
    and party_id = new.party_id
    and currency_iso_code = new.currency_iso_code
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
        update "ores_refdata_party_currencies_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
        and party_id = new.party_id
        and currency_iso_code = new.currency_iso_code
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

create or replace trigger ores_refdata_party_currencies_insert_trg
before insert on "ores_refdata_party_currencies_tbl"
for each row
execute function ores_refdata_party_currencies_insert_fn();

create or replace rule ores_refdata_party_currencies_delete_rule as
on delete to "ores_refdata_party_currencies_tbl"
do instead
  update "ores_refdata_party_currencies_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and party_id = old.party_id
  and currency_iso_code = old.currency_iso_code
  and valid_to = ores_utility_infinity_timestamp_fn();
