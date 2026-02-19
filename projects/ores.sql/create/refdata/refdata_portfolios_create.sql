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
 *  Table
 *
 * Represents organizational, risk, or reporting groupings.
 * Never holds trades directly. Supports hierarchical structure
 * via self-referencing parent_portfolio_id.
 */

create table if not exists "ores_refdata_portfolios_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "parent_portfolio_id" uuid null,
    "owner_unit_id" uuid null,
    "purpose_type" text not null,
    "aggregation_ccy" text null,
    "is_virtual" integer not null,
    "status" text not null default 'Active',
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
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid)
);

-- Unique name for active records
create unique index if not exists ores_refdata_portfolios_name_uniq_idx
on "ores_refdata_portfolios_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_refdata_portfolios_version_uniq_idx
on "ores_refdata_portfolios_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_refdata_portfolios_id_uniq_idx
on "ores_refdata_portfolios_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_refdata_portfolios_tenant_idx
on "ores_refdata_portfolios_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_portfolios_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate parent_portfolio_id (nullable self-referencing FK)
    if NEW.parent_portfolio_id is not null then
        if not exists (
            select 1 from ores_refdata_portfolios_tbl
            where tenant_id = NEW.tenant_id and id = NEW.parent_portfolio_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid parent_portfolio_id: %. No active portfolio found with this id.',
                NEW.parent_portfolio_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate owner_unit_id (nullable soft FK to business_units)
    if NEW.owner_unit_id is not null then
        if not exists (
            select 1 from ores_refdata_business_units_tbl
            where tenant_id = NEW.tenant_id and id = NEW.owner_unit_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid owner_unit_id: %. No active business unit found with this id.',
                NEW.owner_unit_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate purpose_type
    NEW.purpose_type := ores_refdata_validate_purpose_type_fn(NEW.tenant_id, NEW.purpose_type);

    -- Validate aggregation_ccy (nullable)
    if NEW.aggregation_ccy is not null and NEW.aggregation_ccy != '' then
        NEW.aggregation_ccy := ores_refdata_validate_currency_fn(
            NEW.tenant_id, NEW.aggregation_ccy);
    end if;

    -- Version management
    select version into current_version
    from "ores_refdata_portfolios_tbl"
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

        update "ores_refdata_portfolios_tbl"
        set valid_to = current_timestamp
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = current_user;

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_refdata_portfolios_insert_trg
before insert on "ores_refdata_portfolios_tbl"
for each row execute function ores_refdata_portfolios_insert_fn();

create or replace rule ores_refdata_portfolios_delete_rule as
on delete to "ores_refdata_portfolios_tbl" do instead
    update "ores_refdata_portfolios_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
