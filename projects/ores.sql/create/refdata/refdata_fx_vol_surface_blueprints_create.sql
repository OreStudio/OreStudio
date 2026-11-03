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
 * FX Volatility Surface Blueprint Table
 *
 * Records *how* one FX volatility surface is built, as a named, inspectable recipe, the
 * surface's counterpart of the IR curve bootstrap config. Its pillars, in
 * fx_vol_surface_pillar, name the quotes the calibration reads; the blueprint states
 * what is done with them: the smile model fitted at each expiry, how the fitted
 * expiries are joined, the grid of expiries and strikes the surface is written on, and
 * the series it is written to. See
 * [[id:53D14F31-EE32-493E-BBEB-140AA4906BE4][Constructing a volatility surface: from quotes to a published set]].
 *
 * output_series_id is a soft, cross-component reference into ores.marketdata's
 * market_series: the fx_vol_surface series the calibration alone writes. As for the
 * IR config, it is chosen and not minted, and it is the sole natural key, so exactly
 * one active blueprint, tenant-wide, writes any given surface series.
 *
 * fx_option_convention_id names the pair's quoting convention in
 * fx_option_convention, a soft reference: its ATM type, delta type, butterfly style
 * and switch tenor decide what each pillar's quote means. smile_model and
 * expiry_interpolation are small, fixed vocabularies intrinsic to the record, held
 * by check constraints, as the IR config's curve_family_role is.
 *
 * The output grid is two lists, the expiries as tenor codes and the strikes in the
 * oresmd strike grammar (ATM, DEL/Spot/Put/25, an absolute level), each separated
 * by commas: the grid is a property of the recipe, read whole by the calibration and
 * never addressed one point at a time, so it has no table of its own.
 */

create table if not exists "ores_refdata_fx_vol_surface_blueprints_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "output_series_id" uuid not null,
    "party_id" uuid not null,
    "name" text not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "fx_option_convention_id" text not null,
    "smile_model" text not null,
    "expiry_interpolation" text not null,
    "output_expiries" text not null,
    "output_strikes" text not null,
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
    check ("name" <> ''),
    check ("base_currency_code" <> "quote_currency_code"),
    check ("fx_option_convention_id" <> ''),
    check ("smile_model" in ('ssvi', 'sabr', 'spline')),
    check ("expiry_interpolation" in ('linear_total_variance', 'flat_forward_variance')),
    check ("output_expiries" <> ''),
    check ("output_strikes" <> '')
);

-- Unique output_series_id for active records
create unique index if not exists fx_vol_surface_blueprints_output_series_id_uniq_idx
on "ores_refdata_fx_vol_surface_blueprints_tbl" (tenant_id, output_series_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists fx_vol_surface_blueprints_version_uniq_idx
on "ores_refdata_fx_vol_surface_blueprints_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists fx_vol_surface_blueprints_id_uniq_idx
on "ores_refdata_fx_vol_surface_blueprints_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists fx_vol_surface_blueprints_tenant_idx
on "ores_refdata_fx_vol_surface_blueprints_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists fx_vol_surface_blueprints_party_name_idx
on "ores_refdata_fx_vol_surface_blueprints_tbl" (tenant_id, party_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_fx_vol_surface_blueprints_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate base_currency_code
    NEW.base_currency_code := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.base_currency_code);

    -- Validate quote_currency_code
    NEW.quote_currency_code := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.quote_currency_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_refdata_fx_vol_surface_blueprints_tbl"
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
                    'fx_vol_surface_blueprint',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'fx_vol_surface_blueprint',
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
        update "ores_refdata_fx_vol_surface_blueprints_tbl"
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

create or replace trigger ores_refdata_fx_vol_surface_blueprints_insert_trg
before insert on "ores_refdata_fx_vol_surface_blueprints_tbl"
for each row execute function ores_refdata_fx_vol_surface_blueprints_insert_fn();

create or replace rule ores_refdata_fx_vol_surface_blueprints_delete_rule as
on delete to "ores_refdata_fx_vol_surface_blueprints_tbl" do instead (
    update "ores_refdata_fx_vol_surface_blueprints_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for FX Volatility Surface Blueprint
-- =============================================================================
alter table ores_refdata_fx_vol_surface_blueprints_tbl enable row level security;

drop policy if exists fx_vol_surface_blueprints_tbl_tenant_isolation_policy
    on ores_refdata_fx_vol_surface_blueprints_tbl;

create policy fx_vol_surface_blueprints_tbl_tenant_isolation_policy
on ores_refdata_fx_vol_surface_blueprints_tbl
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
drop policy if exists fx_vol_surface_blueprints_tbl_party_isolation_policy
    on ores_refdata_fx_vol_surface_blueprints_tbl;

create policy fx_vol_surface_blueprints_tbl_party_isolation_policy
on ores_refdata_fx_vol_surface_blueprints_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
