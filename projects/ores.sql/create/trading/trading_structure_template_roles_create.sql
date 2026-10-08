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
 * Structure Template Role Table
 *
 * The roles a structure template allows, each with the number of legs it
 * holds. The table is what makes a template a mould: a straddle allows one
 * role holding exactly two legs, a butterfly allows a body of one and wings
 * of two.
 *
 * The row is keyed by its template and its role, because a template states
 * each role once. The leg counts are inclusive at both ends, and a role that
 * holds any number of legs states a maximum of zero meaning no upper bound.
 *
 * Examples: 'Straddle' / 'leg' / 2 to 2, 'Butterfly' / 'wing' / 2 to 2.
 */

create table if not exists "ores_trading_structure_template_roles_tbl" (
    "template_code" text not null,
    "role" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "min_legs" integer not null,
    "max_legs" integer not null,
    "description" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, template_code, role, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        template_code WITH =,
        role WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("template_code" <> ''),
    check ("role" <> ''),
    check ("min_legs" >= 0),
    check ("max_legs" >= 0),
    check ("max_legs" = 0 or "max_legs" >= "min_legs")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists structure_template_roles_version_uniq_idx
on "ores_trading_structure_template_roles_tbl" (tenant_id, template_code, role, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists structure_template_roles_template_code_role_uniq_idx
on "ores_trading_structure_template_roles_tbl" (tenant_id, template_code, role)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists structure_template_roles_tenant_idx
on "ores_trading_structure_template_roles_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_structure_template_roles_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate template_code (soft FK to ores_trading_structure_templates_tbl)
    if not exists (
        select 1 from ores_trading_structure_templates_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.template_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid template_code: %. No active structure template found with this code.', NEW.template_code
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
    from "ores_trading_structure_template_roles_tbl"
    where tenant_id = NEW.tenant_id
      and template_code = NEW.template_code and role = NEW.role
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
                    'structure_template_role',
                    'template_code');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'structure_template_role',
                'template_code',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_structure_template_roles_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and template_code = NEW.template_code and role = NEW.role
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

create or replace trigger ores_trading_structure_template_roles_insert_trg
before insert on "ores_trading_structure_template_roles_tbl"
for each row execute function ores_trading_structure_template_roles_insert_fn();

create or replace rule ores_trading_structure_template_roles_delete_rule as
on delete to "ores_trading_structure_template_roles_tbl" do instead (
    update "ores_trading_structure_template_roles_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and template_code = OLD.template_code and role = OLD.role
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Structure Template Role
-- =============================================================================
alter table ores_trading_structure_template_roles_tbl enable row level security;

drop policy if exists structure_template_roles_tbl_tenant_isolation_policy
    on ores_trading_structure_template_roles_tbl;

create policy structure_template_roles_tbl_tenant_isolation_policy
on ores_trading_structure_template_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
