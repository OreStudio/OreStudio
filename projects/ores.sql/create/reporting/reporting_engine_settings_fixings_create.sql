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
 * Engine Settings Fixings Table
 *
 * The fixings and cash flows part of a named engine settings configuration. Which fixings and cash flows count in a run. A change here changes the numbers. A column that is null means the engine default,
 * and a configuration with no row in this part uses the default for every setting in it. A write to this part bumps the version of the
 * central engine_settings row in the same transaction.
 */

create table if not exists "ores_reporting_engine_settings_fixings_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "engine_settings_id" uuid not null,
    "party_id" uuid not null,
    "imply_todays_fixings" boolean null,
    "include_todays_cash_flows" boolean null,
    "include_reference_date_events" boolean null,
    "enrich_index_fixings" boolean null,
    "ignore_fixing_lag" integer null,
    "ignore_fixing_lead" integer null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, party_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        party_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("ignore_fixing_lag" is null or "ignore_fixing_lag" >= 0),
    check ("ignore_fixing_lead" is null or "ignore_fixing_lead" >= 0)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists engine_settings_fixings_version_uniq_idx
on "ores_reporting_engine_settings_fixings_tbl" (tenant_id, id, party_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists engine_settings_fixings_id_uniq_idx
on "ores_reporting_engine_settings_fixings_tbl" (tenant_id, id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists engine_settings_fixings_tenant_idx
on "ores_reporting_engine_settings_fixings_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists engine_settings_fixings_one_per_settings_idx
on "ores_reporting_engine_settings_fixings_tbl" (tenant_id, engine_settings_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_engine_settings_fixings_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate engine_settings_id (soft FK to ores_reporting_engine_settings_tbl)
    if not exists (
        select 1 from ores_reporting_engine_settings_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.engine_settings_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid engine_settings_id: %. No active engine settings found with this id.', NEW.engine_settings_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Bump ores_reporting_engine_settings_tbl's version alongside this write (composite entity
    -- versioning — see the "Temporal composite entity versioning"
    -- architecture doc). The touch function re-validates modified_by
    -- itself; change_reason_code is passed through as-is since it was
    -- already validated above.
    perform ores_reporting_engine_settings_touch_version_fn(
        NEW.tenant_id,
        NEW.engine_settings_id,
        NEW.change_reason_code,
        NEW.change_commentary,
        NEW.modified_by,
        NEW.performed_by,
        'engine_settings_fixings'
    );

    -- Version management
    select version into current_version
    from "ores_reporting_engine_settings_fixings_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id and party_id = NEW.party_id
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
                    'engine_settings_fixings',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'engine_settings_fixings',
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
        update "ores_reporting_engine_settings_fixings_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id and party_id = NEW.party_id
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

create or replace trigger ores_reporting_engine_settings_fixings_insert_trg
before insert on "ores_reporting_engine_settings_fixings_tbl"
for each row execute function ores_reporting_engine_settings_fixings_insert_fn();

create or replace rule ores_reporting_engine_settings_fixings_delete_rule as
on delete to "ores_reporting_engine_settings_fixings_tbl" do instead (
    update "ores_reporting_engine_settings_fixings_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id and party_id = OLD.party_id
      and valid_to = ores_utility_infinity_timestamp_fn();
    -- Bump ores_reporting_engine_settings_tbl's version on delete too (composite entity
    -- versioning), symmetric with the insert-side call above.
    select ores_reporting_engine_settings_touch_version_fn(
        OLD.tenant_id,
        OLD.engine_settings_id,
        OLD.change_reason_code,
        OLD.change_commentary,
        OLD.modified_by,
        OLD.performed_by,
        'engine_settings_fixings'
    );
);

-- =============================================================================
-- Row-level security: tenant isolation for Engine Settings Fixings
-- =============================================================================
alter table ores_reporting_engine_settings_fixings_tbl enable row level security;

drop policy if exists engine_settings_fixings_tbl_tenant_isolation_policy
    on ores_reporting_engine_settings_fixings_tbl;

create policy engine_settings_fixings_tbl_tenant_isolation_policy
on ores_reporting_engine_settings_fixings_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation (RESTRICTIVE): ANDed with the permissive tenant
-- policy above, a session sees only rows whose party_id its visible
-- party set admits. The visible_party_ids-is-null passthrough applies
-- for sessions with no party restriction (tenant admins, service
-- contexts).
drop policy if exists engine_settings_fixings_tbl_party_isolation_policy
    on ores_reporting_engine_settings_fixings_tbl;

create policy engine_settings_fixings_tbl_party_isolation_policy
on ores_reporting_engine_settings_fixings_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
