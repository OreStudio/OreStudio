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
 * Account Contact Information Table
 *
 * An account's real-world identity: full name, plus the usual address/
 * phone/email/web page fields. Accounts (ores_iam_accounts_tbl) carry
 * only username/email/credentials — no name field — so this is the
 * only place an account's actual name is recorded. One contact record
 * per account (unlike party contact information, which allows several
 * by contact_type — a person doesn't need a Legal/Operations/
 * Settlement/Billing split).
 */

create table if not exists "ores_iam_account_contact_informations_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "account_id" uuid not null,
    "full_name" text not null,
    "street_line_1" text null,
    "street_line_2" text null,
    "city" text null,
    "state" text null,
    "country_code" text null,
    "postal_code" text null,
    "phone" text null,
    "email" text null,
    "web_page" text null,
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

-- Unique account_id for active records
create unique index if not exists account_contact_informations_account_id_uniq_idx
on "ores_iam_account_contact_informations_tbl" (tenant_id, account_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists account_contact_informations_version_uniq_idx
on "ores_iam_account_contact_informations_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists account_contact_informations_id_uniq_idx
on "ores_iam_account_contact_informations_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists account_contact_informations_tenant_idx
on "ores_iam_account_contact_informations_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_iam_account_contact_informations_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate account_id (soft FK to ores_iam_accounts_tbl)
    if not exists (
        select 1 from ores_iam_accounts_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.account_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid account_id: %. No active account found with this id.', NEW.account_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Bump ores_iam_accounts_tbl's version alongside this write (composite entity
    -- versioning — see the "Temporal composite entity versioning"
    -- architecture doc). The touch function re-validates modified_by
    -- itself; change_reason_code is passed through as-is since it was
    -- already validated above.
    perform ores_iam_accounts_touch_version_fn(
        NEW.tenant_id,
        NEW.account_id,
        NEW.change_reason_code,
        NEW.change_commentary,
        NEW.modified_by,
        NEW.performed_by,
        'account_contact_information'
    );

    -- Version management
    select version into current_version
    from "ores_iam_account_contact_informations_tbl"
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
        update "ores_iam_account_contact_informations_tbl"
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

create or replace trigger ores_iam_account_contact_informations_insert_trg
before insert on "ores_iam_account_contact_informations_tbl"
for each row execute function ores_iam_account_contact_informations_insert_fn();

create or replace rule ores_iam_account_contact_informations_delete_rule as
on delete to "ores_iam_account_contact_informations_tbl" do instead (
    update "ores_iam_account_contact_informations_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
    -- Bump ores_iam_accounts_tbl's version on delete too (composite entity
    -- versioning), symmetric with the insert-side call above.
    select ores_iam_accounts_touch_version_fn(
        OLD.tenant_id,
        OLD.account_id,
        OLD.change_reason_code,
        OLD.change_commentary,
        OLD.modified_by,
        OLD.performed_by,
        'account_contact_information'
    );
);
