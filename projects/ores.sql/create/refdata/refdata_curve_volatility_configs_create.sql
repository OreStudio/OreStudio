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
 * Curve Volatility Config Table
 *
 * One way a volatility entry's surface may be built. CDS, equity and commodity
 * volatilities each name one or more volatility configurations, each with an
 * optional priority; ORE builds the first that succeeds. kind names the
 * element the document wrote, and the columns that kind uses are set. An entry
 * writes a kind either directly or inside a VolatilityConfig element, which
 * holds at most one of each kind; is_wrapped says which.
 *
 * The kinds the corpus writes are modelled: Constant, Curve, StrikeSurface,
 * DeltaSurface and ProxySurface. A Curve kind's quotes are rows of
 * curve_quote on the entry, in the list Curve or VolatilityConfig/Curve.
 * MoneynessSurface, ApoFutureSurface and a parametric smile inside a surface
 * appear in no corpus document, and the mapper refuses them.
 */

create table if not exists "ores_refdata_curve_volatility_configs_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "curve_definition_id" uuid not null,
    "kind" text not null,
    "is_wrapped" boolean not null,
    "priority" integer null,
    "quote_type" text null,
    "volatility_type" text null,
    "exercise_type" text null,
    "strikes" text null,
    "expiries" text null,
    "time_interpolation" text null,
    "strike_interpolation" text null,
    "extrapolation" text null,
    "time_extrapolation" text null,
    "time_extrapolation_variance" text null,
    "strike_extrapolation" text null,
    "calendar" text null,
    "quote" text null,
    "interpolation" text null,
    "enforce_monotone_variance" boolean null,
    "delta_type" text null,
    "atm_type" text null,
    "atm_delta_type" text null,
    "put_deltas" text null,
    "call_deltas" text null,
    "future_price_correction" text null,
    "proxy_volatility_curve" text null,
    "fx_volatility_curve" text null,
    "correlation_curve" text null,
    "cds_volatility_curve" text null,
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
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("kind" <> ''),
    check ("position" >= 0)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists curve_volatility_configs_version_uniq_idx
on "ores_refdata_curve_volatility_configs_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists curve_volatility_configs_id_uniq_idx
on "ores_refdata_curve_volatility_configs_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists curve_volatility_configs_tenant_idx
on "ores_refdata_curve_volatility_configs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_curve_volatility_configs_insert_fn()
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

    -- Validate calendar (optional field -- skip validation when null)
    if NEW.calendar is not null then
        NEW.calendar := ores_refdata_validate_calendar_fn(NEW.tenant_id, NEW.calendar);
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_refdata_curve_volatility_configs_tbl"
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
                    'curve_volatility_config',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'curve_volatility_config',
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
        update "ores_refdata_curve_volatility_configs_tbl"
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

create or replace trigger ores_refdata_curve_volatility_configs_insert_trg
before insert on "ores_refdata_curve_volatility_configs_tbl"
for each row execute function ores_refdata_curve_volatility_configs_insert_fn();

create or replace rule ores_refdata_curve_volatility_configs_delete_rule as
on delete to "ores_refdata_curve_volatility_configs_tbl" do instead (
    update "ores_refdata_curve_volatility_configs_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Curve Volatility Config
-- =============================================================================
alter table ores_refdata_curve_volatility_configs_tbl enable row level security;

drop policy if exists curve_volatility_configs_tbl_tenant_isolation_policy
    on ores_refdata_curve_volatility_configs_tbl;

create policy curve_volatility_configs_tbl_tenant_isolation_policy
on ores_refdata_curve_volatility_configs_tbl
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
drop policy if exists curve_volatility_configs_tbl_party_isolation_policy
    on ores_refdata_curve_volatility_configs_tbl;

create policy curve_volatility_configs_tbl_party_isolation_policy
on ores_refdata_curve_volatility_configs_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
