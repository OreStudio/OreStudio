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
 * External Identity Table
 *
 * The link from an ORE account to one account at one identity provider, and the
 * record that the person proved it. The provider names the person by an issuer and
 * a subject; the subject is opaque, stable at the provider, and the only thing ORE
 * trusts to identify that provider account.
 *
 * The address the provider stated at the moment of linking is kept beside the
 * subject, with whether the provider said it was verified. It is evidence of what
 * the person linked, not an identity: nothing in ORE looks a person up by it, and
 * no link is ever made because two addresses match. A person's address at a
 * provider can change after the link, so the column records a fact at a time and
 * is never treated as the current truth.
 *
 * Two invariants decide the shape. The pair (issuer, subject) names one provider
 * account, and with the tenant it is the natural key, so the same provider account
 * links to one ORE account in a tenant. And a link belongs to an account: an
 * account may hold several links, one per provider, and keeps at least one way in
 * at all times, which its own operations enforce.
 */

create table if not exists "ores_iam_external_identities_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "issuer" text not null,
    "subject" text not null,
    "account_id" uuid not null,
    "identity_provider_id" uuid not null,
    "email" text null,
    "email_verified" boolean not null default false,
    "linked_at" timestamp with time zone not null,
    "last_used_at" timestamp with time zone null,
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

-- Composite natural key: unique combination for active records
create unique index if not exists external_identities_issuer_subject_uniq_idx
on "ores_iam_external_identities_tbl" (tenant_id, issuer, subject)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists external_identities_version_uniq_idx
on "ores_iam_external_identities_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists external_identities_id_uniq_idx
on "ores_iam_external_identities_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists external_identities_tenant_idx
on "ores_iam_external_identities_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_iam_external_identities_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate account_id (soft FK to ores_iam_accounts_tbl)
    if not ores_iam_account_exists_fn(NEW.account_id) then
        raise exception 'Invalid account_id: %. No active account found with this id.', NEW.account_id
            using errcode = '23503';
    end if;

    -- Validate identity_provider_id (soft FK to ores_iam_identity_providers_tbl)
    if not exists (
        select 1 from ores_iam_identity_providers_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.identity_provider_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid identity_provider_id: %. No active identity provider found with this id.', NEW.identity_provider_id
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
    from "ores_iam_external_identities_tbl"
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
                    'external_identity',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'external_identity',
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
        update "ores_iam_external_identities_tbl"
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

create or replace trigger ores_iam_external_identities_insert_trg
before insert on "ores_iam_external_identities_tbl"
for each row execute function ores_iam_external_identities_insert_fn();

create or replace rule ores_iam_external_identities_delete_rule as
on delete to "ores_iam_external_identities_tbl" do instead (
    update "ores_iam_external_identities_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for External Identity
-- =============================================================================
alter table ores_iam_external_identities_tbl enable row level security;

drop policy if exists external_identities_tbl_tenant_isolation_policy
    on ores_iam_external_identities_tbl;

create policy external_identities_tbl_tenant_isolation_policy
on ores_iam_external_identities_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
