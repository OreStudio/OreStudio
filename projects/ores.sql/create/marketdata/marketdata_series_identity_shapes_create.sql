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
 * Series Identity Shape Table
 *
 * The shape of a market series identity, one row per family.
 *
 * A family is the pair of codes the taxonomy already carries -- the asset class
 * and the series subclass -- narrowed, for an index series, by the index family.
 * index_fixing is not one shape but several: an IBOR fixing states a currency
 * and an index name, a swap fixing a currency and a tenor, an FX fixing a currency
 * and the currency it is quoted in. A shape keyed on the subclass alone would be
 * wrong about all but one of them, so the family carries the index family too.
 *
 * A shape is named by a code, such as ir.index_fixing.ibor, so a person and a
 * screen can refer to it by one key, and it says in words what the family is for.
 * What the family makes of each field is stated by its entries,
 * series_identity_shape_entries: one row per field, required or forbidden.
 *
 * The writer that projects an identity row from an oresmd URI reads the shapes
 * and their entries, and refuses a row that misses a required field or carries a
 * forbidden one, naming the family and the field. A family with no shape is not
 * enforced: the writer projects its series unguarded, and the backfill counts them
 * as unstated. The catalogue grows family by family, and refusing every family
 * nobody has stated yet would stop an ingest for a reason that is the catalogue's.
 */

create table if not exists "ores_marketdata_series_identity_shapes_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "asset_class" text not null,
    "series_subclass" text not null,
    "index_family" text not null default '',
    "description" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, code, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("code" <> ''),
    check ("description" <> ''),
    check ("code" = concat_ws('.', "asset_class", "series_subclass", nullif("index_family", '')))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists series_identity_shapes_version_uniq_idx
on "ores_marketdata_series_identity_shapes_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists series_identity_shapes_code_uniq_idx
on "ores_marketdata_series_identity_shapes_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists series_identity_shapes_tenant_idx
on "ores_marketdata_series_identity_shapes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists series_identity_shapes_family_idx
on "ores_marketdata_series_identity_shapes_tbl" (tenant_id, asset_class, series_subclass, index_family)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_marketdata_series_identity_shapes_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate series_subclass
    NEW.series_subclass := ores_refdata_validate_series_subclass_code_fn(NEW.tenant_id, NEW.series_subclass);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_marketdata_series_identity_shapes_tbl"
    where tenant_id = NEW.tenant_id
      and code = NEW.code
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
                    'series_identity_shape',
                    'code');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'series_identity_shape',
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
        update "ores_marketdata_series_identity_shapes_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and code = NEW.code
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

create or replace trigger ores_marketdata_series_identity_shapes_insert_trg
before insert on "ores_marketdata_series_identity_shapes_tbl"
for each row execute function ores_marketdata_series_identity_shapes_insert_fn();

create or replace rule ores_marketdata_series_identity_shapes_delete_rule as
on delete to "ores_marketdata_series_identity_shapes_tbl" do instead (
    update "ores_marketdata_series_identity_shapes_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      -- A tenant reads the shared rows of the system tenant, so OLD can be one.
      -- The rule runs with the owner's rights, which row-level security does
      -- not bind, so the rule states the own-tenant limit that the delete
      -- policy cannot enforce here.
      and tenant_id = ores_iam_current_tenant_id_fn()
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Series Identity Shape
-- Every session reads its own rows and the system tenant's rows; writes
-- stay the current tenant's.
-- =============================================================================
alter table ores_marketdata_series_identity_shapes_tbl enable row level security;

drop policy if exists series_identity_shapes_tbl_tenant_isolation_policy
    on ores_marketdata_series_identity_shapes_tbl;

create policy series_identity_shapes_tbl_tenant_isolation_policy
on ores_marketdata_series_identity_shapes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- The read admits the system tenant's rows beside the caller's; the policy
-- above still binds every write to the caller's own rows. It must: a single
-- for-all policy that admitted them would let a tenant change or remove a
-- shared row. See doc/knowledge/security/data_read_across_tenants.org.
drop policy if exists series_identity_shapes_tbl_shared_reference_read_policy
    on ores_marketdata_series_identity_shapes_tbl;

create policy series_identity_shapes_tbl_shared_reference_read_policy
on ores_marketdata_series_identity_shapes_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
);
