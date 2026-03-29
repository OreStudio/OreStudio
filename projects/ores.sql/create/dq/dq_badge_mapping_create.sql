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
 * Badge Mapping Table
 *
 * Universal mapping table that associates any (code_domain, entity_code)
 * pair with a badge definition. This is the single join point between
 * domain values and their visual presentation.
 *
 * Examples:
 * - ('party_status', 'ACTIVE')  -> 'active'
 * - ('party_status', 'FROZEN')  -> 'frozen'
 * - ('fsm_state',    'DRAFT')   -> 'fsm_draft'
 * - ('login_status', 'Online')  -> 'login_online'
 * - ('dq_nature',    'Actual')  -> 'dq_actual'
 *
 * Populated via seed scripts; no management UI needed.
 */

create table if not exists "ores_dq_badge_mappings_tbl" (
    "code_domain_code" text not null,
    "tenant_id" uuid not null,
    "entity_code" text not null,
    "version" integer not null,
    "badge_code" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, code_domain_code, entity_code, valid_from),
    exclude using gist (
        tenant_id WITH =,
        code_domain_code WITH =,
        entity_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for looking up all mappings in a code domain
create index if not exists ores_dq_badge_mappings_code_domain_idx
on "ores_dq_badge_mappings_tbl" (code_domain_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding the badge for a specific entity code
create index if not exists ores_dq_badge_mappings_entity_idx
on "ores_dq_badge_mappings_tbl" (entity_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists ores_dq_badge_mappings_uniq_idx
on "ores_dq_badge_mappings_tbl" (tenant_id, code_domain_code, entity_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_dq_badge_mappings_tenant_idx
on "ores_dq_badge_mappings_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_dq_badge_mappings_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Version management
    select version into current_version
    from "ores_dq_badge_mappings_tbl"
    where tenant_id = new.tenant_id
    and code_domain_code = new.code_domain_code
    and entity_code = new.entity_code
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
        update "ores_dq_badge_mappings_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
        and code_domain_code = new.code_domain_code
        and entity_code = new.entity_code
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_actor_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_dq_badge_mappings_insert_trg
before insert on "ores_dq_badge_mappings_tbl"
for each row
execute function ores_dq_badge_mappings_insert_fn();

create or replace rule ores_dq_badge_mappings_delete_rule as
on delete to "ores_dq_badge_mappings_tbl"
do instead
  update "ores_dq_badge_mappings_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and code_domain_code = old.code_domain_code
  and entity_code = old.entity_code
  and valid_to = ores_utility_infinity_timestamp_fn();
