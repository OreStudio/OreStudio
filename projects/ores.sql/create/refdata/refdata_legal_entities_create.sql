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
 * Legal Entity Table
 *
 * The legal entities a trade may reference, keyed by LEI. A credit trade states
 * the legal entity its risk is on, and until now that reference was a string a
 * caller typed: a credit curve named ACME in an engine document and an LEI were
 * the same column, so nothing could be joined to either.
 *
 * The LEI is the key because it is the identifier the market uses and the one the
 * DQ dataset carries. A published dataset of 38,000 entities becomes readable by
 * every party through this table, which is what world data means: a legal entity
 * is not one tenant's private row.
 *
 * The name, the jurisdiction and the status travel with the entity because they
 * are facts about it, and a screen that shows only an LEI shows nothing a person
 * can read.
 *
 * This model holds the vocabulary only. Which entity a given trade references,
 * and whether that reference is an LEI or an internal identifier, is the trade's
 * statement and lives on the trade.
 */

create table if not exists "ores_refdata_legal_entities_tbl" (
    "lei" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "legal_name" text not null,
    "jurisdiction" text null,
    "entity_status" text null,
    "entity_category" text null,
    "display_order" integer not null default 0,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, lei, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        lei WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("lei" <> '')
);

-- Unique legal_name for active records
create unique index if not exists legal_entities_legal_name_uniq_idx
on "ores_refdata_legal_entities_tbl" (tenant_id, legal_name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists legal_entities_version_uniq_idx
on "ores_refdata_legal_entities_tbl" (tenant_id, lei, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists legal_entities_lei_uniq_idx
on "ores_refdata_legal_entities_tbl" (tenant_id, lei)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists legal_entities_tenant_idx
on "ores_refdata_legal_entities_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_legal_entities_insert_fn()
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
    from "ores_refdata_legal_entities_tbl"
    where tenant_id = NEW.tenant_id
      and lei = NEW.lei
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
                    'legal_entity',
                    'lei');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'legal_entity',
                'lei',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_legal_entities_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and lei = NEW.lei
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

create or replace trigger ores_refdata_legal_entities_insert_trg
before insert on "ores_refdata_legal_entities_tbl"
for each row execute function ores_refdata_legal_entities_insert_fn();

create or replace rule ores_refdata_legal_entities_delete_rule as
on delete to "ores_refdata_legal_entities_tbl" do instead (
    update "ores_refdata_legal_entities_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and lei = OLD.lei
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Legal Entity
-- =============================================================================
alter table ores_refdata_legal_entities_tbl enable row level security;

drop policy if exists legal_entities_tbl_tenant_isolation_policy
    on ores_refdata_legal_entities_tbl;

create policy legal_entities_tbl_tenant_isolation_policy
on ores_refdata_legal_entities_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
