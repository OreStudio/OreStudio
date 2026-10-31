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
 * FX Volatility Smile Table
 *
 * ORE's fxVolatility is one xs:all with a mandatory Dimension and every
 * smile element optional, so the schema says only that the smile fields may be
 * absent. It cannot say that they are meaningless in one dimension: an ATM surface
 * has no smile, and a smile surface has one. That is the deficiency this part
 * corrects, because a part row exists only where the surface has a smile.
 *
 * The part holds SmileType, SmileInterpolation, SmileDelta,
 * SmileExtrapolation, TimeInterpolation, TimeWeighting and
 * ButterflyErrorTolerance. Dimension stays on the entry, because it does not
 * only decide whether a smile exists: it also decides how the entry's Deltas are
 * read.
 */

create table if not exists "ores_refdata_fx_volatility_smiles_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "fx_volatility_config_id" uuid not null,
    "party_id" uuid not null,
    "smile_type" text null,
    "smile_interpolation" text null,
    "smile_delta" text null,
    "smile_extrapolation" text null,
    "time_interpolation" text null,
    "time_weighting" text null,
    "butterfly_error_tolerance" double precision null,
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
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Unique fx_volatility_config_id for active records
create unique index if not exists fx_volatility_smiles_fx_volatility_config_id_uniq_idx
on "ores_refdata_fx_volatility_smiles_tbl" (tenant_id, fx_volatility_config_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists fx_volatility_smiles_version_uniq_idx
on "ores_refdata_fx_volatility_smiles_tbl" (tenant_id, id, party_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists fx_volatility_smiles_id_uniq_idx
on "ores_refdata_fx_volatility_smiles_tbl" (tenant_id, id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists fx_volatility_smiles_tenant_idx
on "ores_refdata_fx_volatility_smiles_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_fx_volatility_smiles_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate fx_volatility_config_id (soft FK to ores_refdata_fx_volatility_configs_tbl)
    if not exists (
        select 1 from ores_refdata_fx_volatility_configs_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.fx_volatility_config_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid fx_volatility_config_id: %. No active FX volatility entry found with this id.', NEW.fx_volatility_config_id
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
    from "ores_refdata_fx_volatility_smiles_tbl"
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
                    'fx_volatility_smile',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'fx_volatility_smile',
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
        update "ores_refdata_fx_volatility_smiles_tbl"
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

create or replace trigger ores_refdata_fx_volatility_smiles_insert_trg
before insert on "ores_refdata_fx_volatility_smiles_tbl"
for each row execute function ores_refdata_fx_volatility_smiles_insert_fn();

create or replace rule ores_refdata_fx_volatility_smiles_delete_rule as
on delete to "ores_refdata_fx_volatility_smiles_tbl" do instead (
    update "ores_refdata_fx_volatility_smiles_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id and party_id = OLD.party_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for FX Volatility Smile
-- =============================================================================
alter table ores_refdata_fx_volatility_smiles_tbl enable row level security;

drop policy if exists fx_volatility_smiles_tbl_tenant_isolation_policy
    on ores_refdata_fx_volatility_smiles_tbl;

create policy fx_volatility_smiles_tbl_tenant_isolation_policy
on ores_refdata_fx_volatility_smiles_tbl
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
drop policy if exists fx_volatility_smiles_tbl_party_isolation_policy
    on ores_refdata_fx_volatility_smiles_tbl;

create policy fx_volatility_smiles_tbl_party_isolation_policy
on ores_refdata_fx_volatility_smiles_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
