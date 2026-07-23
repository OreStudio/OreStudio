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
-- User accounts with authentication credentials.
-- Supports optimistic locking via version field.
-- Username and email unique for current records.
-- =============================================================================

create table if not exists ores_iam_accounts_tbl (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "account_type" text not null default 'user',
    "username" text not null,
    "password_hash" text not null,
    "password_salt" text not null,
    "service_password_hash" text null,
    "totp_secret" text not null,
    "email" text not null,
    "default_party_id" uuid null,
    "image_id" uuid null,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "performed_by" text not null,
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

create unique index if not exists accounts_username_uniq_idx
on ores_iam_accounts_tbl (tenant_id, username)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists accounts_email_uniq_idx
on ores_iam_accounts_tbl (tenant_id, email)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists accounts_tenant_idx
on ores_iam_accounts_tbl (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists accounts_version_uniq_idx
on ores_iam_accounts_tbl (id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_iam_accounts_insert_fn()
returns trigger
security definer
set search_path = public
as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Validate account_type
    new.account_type := ores_iam_validate_account_type_fn(new.tenant_id, new.account_type);

    -- Validate image_id, if set (soft FK: no real FK given the temporal key shape)
    if new.image_id is not null and not exists (
        select 1 from ores_assets_images_tbl
        where tenant_id = new.tenant_id
          and image_id = new.image_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid image_id: %. Image must exist.', new.image_id
        using errcode = '23503';
    end if;

    select version into current_version
    from ores_iam_accounts_tbl
    where tenant_id = new.tenant_id
    and id = new.id
    and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. this composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        -- See ores_refdata_parties_insert_fn for the same pattern.
        update ores_iam_accounts_tbl
        set valid_to = clock_timestamp()
        where tenant_id = new.tenant_id
        and id = new.id
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < clock_timestamp();
    else
        new.version = 1;
    end if;

    new.valid_from = clock_timestamp();
    new.valid_to = ores_utility_infinity_timestamp_fn();
    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_iam_accounts_insert_trg
before insert on ores_iam_accounts_tbl
for each row
execute function ores_iam_accounts_insert_fn();

-- Bumps an account's own version when a child entity (e.g. account
-- contact information) is written -- composite entity versioning, same
-- pattern as ores_refdata_parties_touch_version_fn. See the "Temporal
-- composite entity versioning" architecture doc.
create or replace function ores_iam_accounts_touch_version_fn(
    p_tenant_id uuid,
    p_id uuid,
    p_reason_code text,
    p_commentary text,
    p_modified_by text,
    p_performed_by text,
    p_child_entity text
) returns void as $$
declare
    rec ores_iam_accounts_tbl%rowtype;
begin
    -- for update: takes the same row lock the parent's own insert
    -- trigger takes, so the snapshot in rec can't be based on a
    -- business-column value a concurrent direct edit is about to
    -- change.
    select * into rec
    from ores_iam_accounts_tbl
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

    insert into ores_iam_accounts_tbl
    select (rec).*;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
