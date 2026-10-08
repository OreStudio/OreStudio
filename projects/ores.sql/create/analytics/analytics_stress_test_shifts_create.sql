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
 * Stress Test Shift Table
 *
 * One entry of one shift block of a stress scenario: a market object, the kind of
 * shift applied to it, and the shift itself. The corpus uses fourteen of the
 * eighteen families the schema allows -- DiscountCurves, IndexCurves, FxSpots,
 * FxVolatilities, SwaptionVolatilities, CapFloorVolatilities and the rest -- in
 * three hundred and forty-eight entries.
 *
 * The families look different and are the same shape: an object named by one
 * attribute -- ccy, IndexCurve, FxVolatility, SwaptionVolatility -- a
 * ShiftType, and the shift as parallel lists of values and of tenors or
 * expiries. Fourteen tables would be fourteen copies of those five columns with
 * different attribute names. One table holds them all, with the family as a
 * column and the object's name in object_key, which is what makes adding a
 * family a seeded value rather than a table.
 *
 * The entries carry a few things besides the shift -- ShiftSize, IRCurves,
 * ShiftTerms -- and those are written into extras as a stated list of
 * name and value pairs, because they differ from family to family and none is
 * common to all.
 *
 * Each row belongs to exactly one scenario, in the order the document wrote it.
 */

create table if not exists "ores_analytics_stress_test_shifts_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "stress_test_scenario_id" uuid not null,
    "family" text not null,
    "object_key" text not null,
    "shift_type" text null,
    "shifts" text null,
    "shift_tenors" text null,
    "shift_expiries" text null,
    "extras" text null,
    "position" integer not null,
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

-- Version uniqueness for optimistic concurrency
create unique index if not exists stress_test_shifts_version_uniq_idx
on "ores_analytics_stress_test_shifts_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists stress_test_shifts_id_uniq_idx
on "ores_analytics_stress_test_shifts_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists stress_test_shifts_tenant_idx
on "ores_analytics_stress_test_shifts_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_analytics_stress_test_shifts_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate family (soft FK to ores_analytics_stress_shift_families_tbl)
    if not exists (
        select 1 from ores_analytics_stress_shift_families_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.family
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid family: %. No active stress shift family found with this code.', NEW.family
            using errcode = '23503';
    end if;

    -- Validate shift_type (optional soft FK to ores_analytics_shift_types_tbl)
    if NEW.shift_type is not null then
        if not exists (
            select 1 from ores_analytics_shift_types_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and code = NEW.shift_type
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid shift_type: %. No active shift type found with this code.', NEW.shift_type
                using errcode = '23503';
        end if;
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_analytics_stress_test_shifts_tbl"
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
                    'stress_test_shift',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'stress_test_shift',
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
        update "ores_analytics_stress_test_shifts_tbl"
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

create or replace trigger ores_analytics_stress_test_shifts_insert_trg
before insert on "ores_analytics_stress_test_shifts_tbl"
for each row execute function ores_analytics_stress_test_shifts_insert_fn();

create or replace rule ores_analytics_stress_test_shifts_delete_rule as
on delete to "ores_analytics_stress_test_shifts_tbl" do instead (
    update "ores_analytics_stress_test_shifts_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Stress Test Shift
-- =============================================================================
alter table ores_analytics_stress_test_shifts_tbl enable row level security;

drop policy if exists stress_test_shifts_tbl_tenant_isolation_policy
    on ores_analytics_stress_test_shifts_tbl;

create policy stress_test_shifts_tbl_tenant_isolation_policy
on ores_analytics_stress_test_shifts_tbl
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
drop policy if exists stress_test_shifts_tbl_party_isolation_policy
    on ores_analytics_stress_test_shifts_tbl;

create policy stress_test_shifts_tbl_party_isolation_policy
on ores_analytics_stress_test_shifts_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
