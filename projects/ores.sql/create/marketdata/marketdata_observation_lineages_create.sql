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
 * Observation Lineage Table
 *
 * Per-observation provenance for a *derived* market_observation row: which
 * derivation config/version produced it, and which upstream source
 * series/as-of it read. Written only alongside a derived observation --
 * never for the common OBSERVED case, and never as a column on
 * market_observations_tbl itself (a TimescaleDB hypertable explicitly
 * documented as carrying no audit columns because tick-level volumes make
 * that impractical). A row's existence *is* the "derived" marker.
 *
 * Deliberately generic, not curve-specific: this is what lets CRM's own
 * derived-cross publishing (currently pull-only -- see the CRM
 * architecture decision to never broadcast the full derived set as
 * ticks) reuse this table unchanged whenever it starts persisting,
 * stamping derivation_kind to 'CRM_DERIVATION' exactly as the IR
 * curve bootstrapper stamps it to 'IR_CURVE_BOOTSTRAP'.
 *
 * Standard bitemporal entity (GIST exclusion, soft-update/delete
 * triggers), not a hypertable: unlike market_observations, lineage rows
 * are written only for the minority of observations that are derived, so
 * tick-level volume concerns that justify market_observations's own
 * hypertable/no-audit-columns treatment do not apply here. A rerun of a
 * derivation over the same tenor/point natural key closes the prior
 * generation's lineage row and inserts a new one -- the same soft-update
 * convention every other bitemporal entity in this codebase already
 * gets, mirroring (not literally reusing) how market_observations
 * itself handles a rerun.
 */

create table if not exists "ores_marketdata_observation_lineages_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "series_id" uuid not null,
    "observation_datetime" timestamp with time zone not null,
    "point_id" text not null,
    "derivation_config_id" uuid not null,
    "derivation_config_version" integer not null,
    "source_as_of" timestamp with time zone not null,
    "source_series_ids" jsonb not null,
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
    check (jsonb_array_length("source_series_ids") > 0)
);

-- Composite natural key: unique combination for active records
create unique index if not exists observation_lineages_party_series_obs_point_uniq_idx
on "ores_marketdata_observation_lineages_tbl" (tenant_id, party_id, series_id, observation_datetime, point_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists observation_lineages_version_uniq_idx
on "ores_marketdata_observation_lineages_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists observation_lineages_id_uniq_idx
on "ores_marketdata_observation_lineages_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists observation_lineages_tenant_idx
on "ores_marketdata_observation_lineages_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_marketdata_observation_lineages_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_marketdata_observation_lineages_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_marketdata_observation_lineages_tbl"
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
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_observation_lineages_insert_trg
before insert on "ores_marketdata_observation_lineages_tbl"
for each row execute function ores_marketdata_observation_lineages_insert_fn();

create or replace rule ores_marketdata_observation_lineages_delete_rule as
on delete to "ores_marketdata_observation_lineages_tbl" do instead (
    update "ores_marketdata_observation_lineages_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Observation Lineage
-- =============================================================================
alter table ores_marketdata_observation_lineages_tbl enable row level security;

create policy observation_lineages_tbl_tenant_isolation_policy
on ores_marketdata_observation_lineages_tbl
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
create policy observation_lineages_tbl_party_isolation_policy
on ores_marketdata_observation_lineages_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
