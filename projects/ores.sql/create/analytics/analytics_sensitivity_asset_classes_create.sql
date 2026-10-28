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
 * Sensitivity Asset Class Table
 *
 * A sensitivity template key names an asset class, and the class is the reference
 * data's: interest_rates, fx, equity, commodity or credit. The key spells
 * that class as a short prefix a person recognises -- IR, FX, EQ, COMM,
 * CR -- and the two spellings are not the same, because ORE's template keys grew
 * their own vocabulary (it spells commodity COMM where the rest of ORE spells it
 * COM).
 *
 * So this table is the bridge. It holds one row per class ORE presents a template
 * prefix for: the reference data code the template points at, the prefix the key
 * spells, and the words a person reads. Bond and inflation are asset classes of the
 * reference data with no row here, because ORE's examples present no template prefix
 * for them; a tenant that needs one adds the row.
 *
 * The prefix is a presentation and never the identity: a key's spelling is the
 * tenant's, ORE resolves the key a configuration states, and the template's
 * asset_class_code is what the table checks. A screen that shows the class shows
 * the name from here, and the prefix stands beside it as what the key spells.
 *
 * The rows are seeded for the system tenant and read by every tenant.
 */

create table if not exists "ores_analytics_sensitivity_asset_classes_tbl" (
    "asset_class_code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "template_prefix" text not null,
    "name" text not null,
    "description" text not null,
    "display_order" integer not null default 0,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, asset_class_code, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        asset_class_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("asset_class_code" <> '')
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists sensitivity_asset_classes_version_uniq_idx
on "ores_analytics_sensitivity_asset_classes_tbl" (tenant_id, asset_class_code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists sensitivity_asset_classes_asset_class_code_uniq_idx
on "ores_analytics_sensitivity_asset_classes_tbl" (tenant_id, asset_class_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists sensitivity_asset_classes_tenant_idx
on "ores_analytics_sensitivity_asset_classes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_analytics_sensitivity_asset_classes_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate asset_class_code
    NEW.asset_class_code := ores_refdata_validate_asset_class_code_fn(NEW.tenant_id, NEW.asset_class_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_analytics_sensitivity_asset_classes_tbl"
    where tenant_id = NEW.tenant_id
      and asset_class_code = NEW.asset_class_code
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
                    'sensitivity_asset_class',
                    'asset_class_code');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'sensitivity_asset_class',
                'asset_class_code',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_analytics_sensitivity_asset_classes_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and asset_class_code = NEW.asset_class_code
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

create or replace trigger ores_analytics_sensitivity_asset_classes_insert_trg
before insert on "ores_analytics_sensitivity_asset_classes_tbl"
for each row execute function ores_analytics_sensitivity_asset_classes_insert_fn();

create or replace rule ores_analytics_sensitivity_asset_classes_delete_rule as
on delete to "ores_analytics_sensitivity_asset_classes_tbl" do instead (
    update "ores_analytics_sensitivity_asset_classes_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and asset_class_code = OLD.asset_class_code
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Sensitivity Asset Class
-- Every session reads its own rows and the system tenant's rows; writes
-- stay the current tenant's.
-- =============================================================================
alter table ores_analytics_sensitivity_asset_classes_tbl enable row level security;

drop policy if exists sensitivity_asset_classes_tbl_tenant_isolation_policy
    on ores_analytics_sensitivity_asset_classes_tbl;

create policy sensitivity_asset_classes_tbl_tenant_isolation_policy
on ores_analytics_sensitivity_asset_classes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
