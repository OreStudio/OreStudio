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
 * Account Credential Table
 *
 * The secret an account proves when it authenticates, held apart from the
 * account's own profile. One row per account, keyed by account_id, and no
 * row for an account that holds no credential.
 *
 * The split exists because the two are written by different access paths.
 * ores.iam.account is the profile: a screen writes it, and every column
 * travels on the wire. This entity is the credential: the IAM service alone
 * writes it, and no column travels anywhere. Held in one table, a profile
 * write replaced the whole row, and a column the domain type could not carry
 * came back empty.
 *
 * Every secret column is :no_wire:, so the domain struct carries the value
 * the service needs while a serialisation of that struct omits it.
 *
 * The table is bi-temporal and audited, like every other iam entity: it
 * carries version, the four audit columns and the valid_from/valid_to
 * pair with the GIST exclusion, so the model takes the ordinary audited
 * shape and needs no shape flag. A credential write therefore keeps its own
 * history, and the account's history never mentions it.
 *
 * The entity is projected across the whole stack, exactly as the account is: SQL
 * schema and notify trigger, domain type, repository, generator, protocol, NATS
 * handler and sub-registrar, generated CRUD service, history provider,
 * presentation mapper, shell command, HTTP route, eventing, and the TypeScript
 * twins. The secrets stay off the wire because the columns are :no_wire:, not
 * because the entity has no wire surface. See [[id:C650CBC9-FDED-4C68-981F-7902E296928E][NATS entity protocol specification]].
 *
 * The generated half is reads-only, and that is the specification's answer
 * rather than ours. A write carries "the fields the user owns, and no others",
 * and a server-owned field never appears in one; a credential's only meaningful
 * field is server-derived, so the resource has no write record to carry, and "a
 * read-only resource simply declares no write verb". The credential's writes are
 * domain operations, iam.v1.ops.*.
 */

create table if not exists "ores_iam_account_credentials_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "account_id" uuid not null,
    "password_hash" text null,
    "service_password_hash" text null,
    "totp_secret" text null,
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
    check ("valid_from" < "valid_to")
);

-- Unique account_id for active records
create unique index if not exists account_credentials_account_id_uniq_idx
on "ores_iam_account_credentials_tbl" (tenant_id, account_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists account_credentials_version_uniq_idx
on "ores_iam_account_credentials_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists account_credentials_id_uniq_idx
on "ores_iam_account_credentials_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists account_credentials_tenant_idx
on "ores_iam_account_credentials_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_iam_account_credentials_insert_fn()
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

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_iam_account_credentials_tbl"
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
                    'account_credential',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'account_credential',
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
        update "ores_iam_account_credentials_tbl"
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

create or replace trigger ores_iam_account_credentials_insert_trg
before insert on "ores_iam_account_credentials_tbl"
for each row execute function ores_iam_account_credentials_insert_fn();

create or replace rule ores_iam_account_credentials_delete_rule as
on delete to "ores_iam_account_credentials_tbl" do instead (
    update "ores_iam_account_credentials_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
