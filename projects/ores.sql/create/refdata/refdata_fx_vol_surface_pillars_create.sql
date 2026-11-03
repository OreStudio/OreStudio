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
 * FX Volatility Surface Pillar Table
 *
 * One quoted point a surface's calibration reads: an expiry and a strike label, ATM,
 * a risk reversal or butterfly at a delta, or a call or put at a delta, such as 25RR
 * or 10P. Unlike a curve's pillars, a surface's pillars have no order the
 * construction depends on, so a pillar is keyed by its point rather than by a
 * sequence. Its quotes come from its sources in fx_vol_surface_pillar_source, one per
 * contributor, each with the weight it carries in the calibration.
 *
 * expiry_code is a soft tenor reference, as the IR pillar's tenor codes are: the
 * expiry is resolved to a date through the tenor machinery at the application layer.
 */

create table if not exists "ores_refdata_fx_vol_surface_pillars_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "blueprint_id" uuid not null,
    "expiry_code" text not null,
    "strike_label" text not null,
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
    check ("expiry_code" <> ''),
    check ("strike_label" = 'ATM' or substring("strike_label" from '^[0-9]{1,2}([A-Z]+)$') in ('RR', 'BF', 'C', 'P'))
);

-- Composite natural key: unique combination for active records
create unique index if not exists fx_vol_surface_pillars_party_id_blueprint_id_expiry_code_strike_label_uniq_idx
on "ores_refdata_fx_vol_surface_pillars_tbl" (tenant_id, party_id, blueprint_id, expiry_code, strike_label)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists fx_vol_surface_pillars_version_uniq_idx
on "ores_refdata_fx_vol_surface_pillars_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists fx_vol_surface_pillars_id_uniq_idx
on "ores_refdata_fx_vol_surface_pillars_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists fx_vol_surface_pillars_tenant_idx
on "ores_refdata_fx_vol_surface_pillars_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_fx_vol_surface_pillars_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate blueprint_id (soft FK to ores_refdata_fx_vol_surface_blueprints_tbl)
    if not exists (
        select 1 from ores_refdata_fx_vol_surface_blueprints_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.blueprint_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid blueprint_id: %. No active fx_vol_surface_blueprint found with this id.', NEW.blueprint_id
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
    from "ores_refdata_fx_vol_surface_pillars_tbl"
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
                    'fx_vol_surface_pillar',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'fx_vol_surface_pillar',
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
        update "ores_refdata_fx_vol_surface_pillars_tbl"
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

create or replace trigger ores_refdata_fx_vol_surface_pillars_insert_trg
before insert on "ores_refdata_fx_vol_surface_pillars_tbl"
for each row execute function ores_refdata_fx_vol_surface_pillars_insert_fn();

create or replace rule ores_refdata_fx_vol_surface_pillars_delete_rule as
on delete to "ores_refdata_fx_vol_surface_pillars_tbl" do instead (
    update "ores_refdata_fx_vol_surface_pillars_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for FX Volatility Surface Pillar
-- =============================================================================
alter table ores_refdata_fx_vol_surface_pillars_tbl enable row level security;

drop policy if exists fx_vol_surface_pillars_tbl_tenant_isolation_policy
    on ores_refdata_fx_vol_surface_pillars_tbl;

create policy fx_vol_surface_pillars_tbl_tenant_isolation_policy
on ores_refdata_fx_vol_surface_pillars_tbl
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
drop policy if exists fx_vol_surface_pillars_tbl_party_isolation_policy
    on ores_refdata_fx_vol_surface_pillars_tbl;

create policy fx_vol_surface_pillars_tbl_party_isolation_policy
on ores_refdata_fx_vol_surface_pillars_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
