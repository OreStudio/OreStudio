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
 * Party Product Type Deal Class Table
 *
 * A party that draws the flow line elsewhere than the tenant states it here: one
 * row per (party, product type) whose class differs from the catalogue's. A party
 * with no row takes the product type's own class, and a type nobody has classified
 * reads as non_flow.
 *
 * The product type is named by the code the catalogue states, a soft reference:
 * the catalogue belongs to the trading component and this row to the party that
 * classifies it.
 */

create table if not exists "ores_refdata_party_product_type_deal_classes_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "product_type_code" text not null,
    "deal_class" text not null,
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
    check ("deal_class" in ('flow', 'non_flow'))
);

-- Composite natural key: unique combination for active records
create unique index if not exists party_product_type_deal_classes_party_id_product_type_code_uniq_idx
on "ores_refdata_party_product_type_deal_classes_tbl" (tenant_id, party_id, product_type_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists party_product_type_deal_classes_version_uniq_idx
on "ores_refdata_party_product_type_deal_classes_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists party_product_type_deal_classes_id_uniq_idx
on "ores_refdata_party_product_type_deal_classes_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists party_product_type_deal_classes_tenant_idx
on "ores_refdata_party_product_type_deal_classes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists party_product_type_deal_classes_party_product_type_deal_class_idx
on "ores_refdata_party_product_type_deal_classes_tbl" (party_id, product_type_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_party_product_type_deal_classes_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate party_id (soft FK to ores_refdata_parties_tbl)
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_id: %. No active party found with this id.', NEW.party_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Bump ores_refdata_parties_tbl's version alongside this write (composite entity
    -- versioning — see the "Temporal composite entity versioning"
    -- architecture doc). The touch function re-validates modified_by
    -- itself; change_reason_code is passed through as-is since it was
    -- already validated above.
    perform ores_refdata_parties_touch_version_fn(
        NEW.tenant_id,
        NEW.party_id,
        NEW.change_reason_code,
        NEW.change_commentary,
        NEW.modified_by,
        NEW.performed_by,
        'party_product_type_deal_class'
    );

    -- Version management
    select version into current_version
    from "ores_refdata_party_product_type_deal_classes_tbl"
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
                    'party_product_type_deal_class',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'party_product_type_deal_class',
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
        update "ores_refdata_party_product_type_deal_classes_tbl"
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

create or replace trigger ores_refdata_party_product_type_deal_classes_insert_trg
before insert on "ores_refdata_party_product_type_deal_classes_tbl"
for each row execute function ores_refdata_party_product_type_deal_classes_insert_fn();

create or replace rule ores_refdata_party_product_type_deal_classes_delete_rule as
on delete to "ores_refdata_party_product_type_deal_classes_tbl" do instead (
    update "ores_refdata_party_product_type_deal_classes_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
    -- Bump ores_refdata_parties_tbl's version on delete too (composite entity
    -- versioning), symmetric with the insert-side call above.
    select ores_refdata_parties_touch_version_fn(
        OLD.tenant_id,
        OLD.party_id,
        OLD.change_reason_code,
        OLD.change_commentary,
        OLD.modified_by,
        OLD.performed_by,
        'party_product_type_deal_class'
    );
);

-- =============================================================================
-- Row-level security: tenant isolation for Party Product Type Deal Class
-- =============================================================================
alter table ores_refdata_party_product_type_deal_classes_tbl enable row level security;

drop policy if exists party_product_type_deal_classes_tbl_tenant_isolation_policy
    on ores_refdata_party_product_type_deal_classes_tbl;

create policy party_product_type_deal_classes_tbl_tenant_isolation_policy
on ores_refdata_party_product_type_deal_classes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
