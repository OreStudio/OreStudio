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
 * IR Curve Bootstrap Config Table
 *
 * Records *how* a curve is bootstrapped as a named, inspectable
 * artefact -- the "curve template" concept
 * [[id:A26CFA71-21C0-4E98-ABC4-25F0EAD517E3][Multicurve Management]] calls for. Owned by ores.refdata, matching
 * where every other recipe/config entity lives in this codebase
 * (crm_topology_config, curve_role, tenor), not ores.marketdata,
 * which owns only the generic value store the bootstrap output is
 * written into.
 *
 * source_series_id and output_series_id are soft, cross-component
 * references into ores.marketdata's market_series table (the raw
 * instrument grid this config bootstraps, and the official curve series
 * it publishes into) -- deliberately not hard FK constraints, matching
 * the same soft-reference principle already used for
 * market_series.derivation_config_id and ir_curve_tick's own
 * producer/config identity: the referenced table lives in a different
 * component/schema. output_series_id is minted (the market_series
 * catalog row created) at config-creation time by the owning service,
 * never left null/deferred -- there is no "not yet published" state to
 * guard against.
 *
 * curve_family_role (FUNDING/PROJECTION) and
 * discount_curve_config_id (self-referencing, nil-uuid sentinel for
 * FUNDING) encode
 * [[id:7CB0024B-84FB-4AE0-ADF2-763079E888D5][Multi-Curve Construction]]'s Funding-before-Projection build-order
 * dependency as data: a PROJECTION config's discount_curve_config_id
 * must point at a FUNDING config (strict two-tier, no chaining --
 * Multi-Curve Construction names basis-linked/cyclic Projection
 * dependencies as an explicit out-of-scope modelling gap, not something
 * this design should silently permit), and must not reference itself.
 *
 * interpolation_method and curve_family_role are small,
 * fixed-vocabulary fields intrinsic to this record (not references to
 * another entity), following the same plain-check-constraint pattern
 * ir_curve_generation_config.role's 'self_discounting'/discount/
 * projection vocabulary already establishes -- no lookup table for a
 * handful of values with no independent lifecycle of their own.
 * day_count_convention references the existing
 * day_count_fraction_type reference table. split_tenor_code
 * references tenor.code, following the same soft (undeclared,
 * documentation-only) tenor reference ir_curve_template_entry's own
 * start_tenor_code/end_tenor_code already use -- tenor validation
 * happens through the tenor_resolution machinery at the application
 * layer, not a per-consumer DB-level FK, consistent with that sibling
 * entity. For a single-segment interpolation_method, split_tenor_code
 * is a genuine value (the curve's own last pillar's end_tenor_code),
 * not a fabricated sentinel -- matching the spirit of
 * ir_curve_template_entries's own 'SPOT' tenor, which its own doc
 * comment is explicit isn't a sentinel hack either.
 */

create table if not exists "ores_refdata_ir_curve_bootstrap_configs_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "output_series_id" uuid not null,
    "party_id" uuid not null,
    "source_series_id" uuid not null,
    "curve_family_role" text not null,
    "discount_curve_config_id" uuid not null,
    "interpolation_method" text not null,
    "day_count_convention" text not null,
    "split_tenor_code" text not null,
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
    check ("curve_family_role" in ('FUNDING', 'PROJECTION')),
    check ("day_count_convention" <> ''),
    check ("split_tenor_code" <> ''),
    check ("discount_curve_config_id" <> "id"),
    check (("curve_family_role" = 'FUNDING' and "discount_curve_config_id" = ores_utility_nil_uuid_fn()) or ("curve_family_role" = 'PROJECTION' and "discount_curve_config_id" <> ores_utility_nil_uuid_fn()))
);

-- Unique output_series_id for active records
create unique index if not exists ir_curve_bootstrap_configs_output_series_id_uniq_idx
on "ores_refdata_ir_curve_bootstrap_configs_tbl" (tenant_id, output_series_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists ir_curve_bootstrap_configs_version_uniq_idx
on "ores_refdata_ir_curve_bootstrap_configs_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ir_curve_bootstrap_configs_id_uniq_idx
on "ores_refdata_ir_curve_bootstrap_configs_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ir_curve_bootstrap_configs_tenant_idx
on "ores_refdata_ir_curve_bootstrap_configs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_ir_curve_bootstrap_configs_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate day_count_convention
    NEW.day_count_convention := ores_refdata_validate_day_count_fraction_type_fn(NEW.tenant_id, NEW.day_count_convention);

    if NEW.curve_family_role = 'PROJECTION' then
        if not exists (
            select 1 from "ores_refdata_ir_curve_bootstrap_configs_tbl"
            where tenant_id = NEW.tenant_id
              and id = NEW.discount_curve_config_id
              and curve_family_role = 'FUNDING'
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid discount_curve_config_id: %. Must reference an active FUNDING config -- chaining a PROJECTION config off another PROJECTION config is not supported.', NEW.discount_curve_config_id
                using errcode = '23503';
        end if;
    end if;

    if NEW.curve_family_role <> 'FUNDING' then
        if exists (
            select 1 from "ores_refdata_ir_curve_bootstrap_configs_tbl"
            where tenant_id = NEW.tenant_id
              and discount_curve_config_id = NEW.id
              and curve_family_role = 'PROJECTION'
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Cannot change curve_family_role away from FUNDING for %: still referenced as the discount curve by one or more active PROJECTION configs.', NEW.id
                using errcode = '23503';
        end if;
    end if;
    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_refdata_ir_curve_bootstrap_configs_tbl"
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
        update "ores_refdata_ir_curve_bootstrap_configs_tbl"
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

create or replace trigger ores_refdata_ir_curve_bootstrap_configs_insert_trg
before insert on "ores_refdata_ir_curve_bootstrap_configs_tbl"
for each row execute function ores_refdata_ir_curve_bootstrap_configs_insert_fn();

create or replace rule ores_refdata_ir_curve_bootstrap_configs_delete_rule as
on delete to "ores_refdata_ir_curve_bootstrap_configs_tbl" do instead (
    update "ores_refdata_ir_curve_bootstrap_configs_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
