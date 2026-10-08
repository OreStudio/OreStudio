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
 * Entry Channel Type Table
 *
 * Reference data table holding the ways a trade reaches the firm's books:
 * manual (a user captured it), stp (straight-through processing from
 * an upstream system), ecn (an electronic communication network) and
 * allocation (a block trade split into its allocations).
 *
 * The set is closed, and the C++ enum domain::entry_channel holds the same
 * values. A code with no enum value cannot be read back, so the table is
 * immutable: it is seeded once and never changes at run time. Because it
 * is immutable, the trade anchor references it with a database foreign
 * key rather than a trigger check. The table has no tenant: the set is the
 * same for every tenant.
 */

create table if not exists "ores_trading_entry_channel_types_tbl" (
    "code" text not null,
    "version" integer not null,
    "description" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (code, valid_from, valid_to),
    exclude using gist (
        code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("code" <> '')
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists entry_channel_types_version_uniq_idx
on "ores_trading_entry_channel_types_tbl" (code, version)
where valid_to = ores_utility_infinity_timestamp_fn();


create or replace function ores_trading_entry_channel_types_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_trading_entry_channel_types_tbl"
    where code = NEW.code
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
                    'entry_channel_type',
                    'code');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'entry_channel_type',
                'code',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_entry_channel_types_tbl"
        set valid_to = clock_timestamp()
        where code = NEW.code
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

create or replace trigger ores_trading_entry_channel_types_insert_trg
before insert on "ores_trading_entry_channel_types_tbl"
for each row execute function ores_trading_entry_channel_types_insert_fn();

create or replace rule ores_trading_entry_channel_types_delete_rule as
on delete to "ores_trading_entry_channel_types_tbl" do instead (
    update "ores_trading_entry_channel_types_tbl"
    set valid_to = clock_timestamp()
    where code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Entry Channel Type
-- =============================================================================
alter table ores_trading_entry_channel_types_tbl enable row level security;

drop policy if exists entry_channel_types_tbl_tenant_isolation_policy
    on ores_trading_entry_channel_types_tbl;

create policy entry_channel_types_tbl_tenant_isolation_policy
on ores_trading_entry_channel_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
