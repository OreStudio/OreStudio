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
 * Secret Table
 *
 * A secret one tenant keeps, sealed under a key that lives outside the database.
 * The row holds the sealed value and the version of the key that sealed it, and
 * the value is bound to the row it sits in, so a ciphertext copied into another
 * row, another tenant or another version does not open.
 *
 * The plaintext is never a column and never a wire field. A consumer asks for the
 * value by name, and the answer leaves sealed to that consumer's own key, so the
 * value exists in the clear only inside the process that must use it.
 *
 * The sealed value itself is on the generated wire surfaces, because the sealed box
 * hands over one opaque string and the codec carries it as one. That is the
 * baseline and not the contract: describe answers the metadata alone, and only
 * the write request and the resolve reply carry a sealed value.
 *
 * Two invariants decide the shape. A tenant keeps a secret once by its name,
 * which is the natural key: the name is what a consumer asks for and what the
 * audit records, so a tenant cannot hold two secrets that answer one ask. And the
 * owner names the consumer permitted to resolve it, which is a fact about the
 * row and not about the caller: the vault answers a resolve with the owner's key,
 * and a row with no owner is a row nobody may read.
 */

create table if not exists "ores_vault_secrets_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "owner" text not null,
    "sealed_value" text not null,
    "key_version" integer not null,
    "description" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "acting_agent" text not null default '',
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

-- Unique name for active records
create unique index if not exists secrets_name_uniq_idx
on "ores_vault_secrets_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists secrets_version_uniq_idx
on "ores_vault_secrets_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists secrets_id_uniq_idx
on "ores_vault_secrets_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists secrets_tenant_idx
on "ores_vault_secrets_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_vault_secrets_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_vault_secrets_tbl"
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
                    'secret',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'secret',
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
        update "ores_vault_secrets_tbl"
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
    NEW.acting_agent = coalesce(ores_iam_current_agent_fn(), '');

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_vault_secrets_insert_trg
before insert on "ores_vault_secrets_tbl"
for each row execute function ores_vault_secrets_insert_fn();

create or replace rule ores_vault_secrets_delete_rule as
on delete to "ores_vault_secrets_tbl" do instead (
    update "ores_vault_secrets_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Secret
-- =============================================================================
alter table ores_vault_secrets_tbl enable row level security;

drop policy if exists secrets_tbl_tenant_isolation_policy
    on ores_vault_secrets_tbl;

create policy secrets_tbl_tenant_isolation_policy
on ores_vault_secrets_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
