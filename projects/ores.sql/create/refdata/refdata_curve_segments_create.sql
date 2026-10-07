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
 * Curve Segment Table
 *
 * A yield curve is bootstrapped from one or more segments, and each states its
 * Type: Deposit, Cross Currency Basis Swap, Average OIS and eighteen more.
 * The type fixes the segment element it is written under, so the row holds only
 * the type and refers to curve_segment_type, and a type written under the wrong
 * element cannot be stored.
 *
 * The twelve segment elements share a skeleton and differ in a few settings
 * each: projection curves for simple and tenor basis segments, a discount curve
 * and a spot rate for cross currency ones, a reference curve for zero spread
 * ones. Each setting is its own typed column, null where the segment's element
 * has no such setting. The lists a segment holds are child rows: its quotes in
 * curve_quote, and the index curves and default curves of the bond and default
 * based segments in curve_segment_curve.
 *
 * The convention a segment names may be any of ORE's convention kinds, which are
 * one table each, so a function that looks in all of them checks the reference.
 */

create table if not exists "ores_refdata_curve_segments_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "segment_type" text not null,
    "position" integer not null,
    "conventions" text null,
    "pillar_choice" text null,
    "priority" integer null,
    "min_distance" integer null,
    "projection_curve" text null,
    "discount_curve" text null,
    "spot_rate" text null,
    "projection_curve_domestic" text null,
    "projection_curve_foreign" text null,
    "projection_curve_pay" text null,
    "projection_curve_receive" text null,
    "projection_curve_long" text null,
    "projection_curve_short" text null,
    "reference_curve" text null,
    "reference_curve_2" text null,
    "weight_1" double precision null,
    "weight_2" double precision null,
    "ibor_index" text null,
    "rfr_curve" text null,
    "rfr_index" text null,
    "spread" double precision null,
    "base_curve" text null,
    "base_curve_currency" text null,
    "numerator_curve" text null,
    "numerator_curve_currency" text null,
    "denominator_curve" text null,
    "denominator_curve_currency" text null,
    "extrapolate_flat" boolean null,
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
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("segment_type" <> ''),
    check ("position" >= 0)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists curve_segments_version_uniq_idx
on "ores_refdata_curve_segments_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists curve_segments_id_uniq_idx
on "ores_refdata_curve_segments_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists curve_segments_tenant_idx
on "ores_refdata_curve_segments_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_curve_segments_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate curve_definition_id (soft FK to ores_refdata_curve_definitions_tbl)
    if not exists (
        select 1 from ores_refdata_curve_definitions_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.curve_definition_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid curve_definition_id: %. No active curve definition found with this id.', NEW.curve_definition_id
            using errcode = '23503';
    end if;

    -- Validate segment_type (soft FK to ores_refdata_curve_segment_types_tbl)
    if not exists (
        select 1 from ores_refdata_curve_segment_types_tbl
        where tenant_id = NEW.tenant_id
          and code = NEW.segment_type
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid segment_type: %. No active curve segment type found with this code.', NEW.segment_type
            using errcode = '23503';
    end if;

    -- Validate ibor_index (optional soft FK to ores_refdata_floating_index_types_tbl)
    if NEW.ibor_index is not null then
        if not exists (
            select 1 from ores_refdata_floating_index_types_tbl
            where tenant_id = NEW.tenant_id
              and code = NEW.ibor_index
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid ibor_index: %. No active floating index type found with this code.', NEW.ibor_index
                using errcode = '23503';
        end if;
    end if;

    -- Validate rfr_index (optional soft FK to ores_refdata_floating_index_types_tbl)
    if NEW.rfr_index is not null then
        if not exists (
            select 1 from ores_refdata_floating_index_types_tbl
            where tenant_id = NEW.tenant_id
              and code = NEW.rfr_index
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid rfr_index: %. No active floating index type found with this code.', NEW.rfr_index
                using errcode = '23503';
        end if;
    end if;

    -- Validate conventions (optional field -- skip validation when null)
    if NEW.conventions is not null then
        NEW.conventions := ores_refdata_validate_convention_id_fn(NEW.tenant_id, NEW.conventions);
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_refdata_curve_segments_tbl"
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
                    'curve_segment',
                    'id',
                    NEW.id::text);
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'curve_segment',
                'id',
                NEW.id::text,
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_curve_segments_tbl"
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

create or replace trigger ores_refdata_curve_segments_insert_trg
before insert on "ores_refdata_curve_segments_tbl"
for each row execute function ores_refdata_curve_segments_insert_fn();

create or replace rule ores_refdata_curve_segments_delete_rule as
on delete to "ores_refdata_curve_segments_tbl" do instead (
    update "ores_refdata_curve_segments_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Curve Segment
-- =============================================================================
alter table ores_refdata_curve_segments_tbl enable row level security;

drop policy if exists curve_segments_tbl_tenant_isolation_policy
    on ores_refdata_curve_segments_tbl;

create policy curve_segments_tbl_tenant_isolation_policy
on ores_refdata_curve_segments_tbl
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
drop policy if exists curve_segments_tbl_party_isolation_policy
    on ores_refdata_curve_segments_tbl;

create policy curve_segments_tbl_party_isolation_policy
on ores_refdata_curve_segments_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
