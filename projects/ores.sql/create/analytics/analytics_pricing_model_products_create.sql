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
 * Detail row within a pricing model configuration. Each row maps a pricing engine type
 * (e.g. EuropeanSwaption, CMS) to a specific model (e.g. LGM, BlackBachelier) and
 * numerical engine (e.g. Grid, AMC). Product-specific parameters are stored in
 * pricing_model_product_parameters.
 */

create table if not exists "ores_analytics_pricing_model_products_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "pricing_model_config_id" uuid not null,
    "pricing_engine_type_code" text not null,
    "model" text not null,
    "engine" text not null,
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

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_analytics_pricing_model_products_version_uniq_idx
on "ores_analytics_pricing_model_products_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_analytics_pricing_model_products_id_uniq_idx
on "ores_analytics_pricing_model_products_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_analytics_pricing_model_products_tenant_idx
on "ores_analytics_pricing_model_products_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- FK lookup: all products for a config
create index if not exists ores_analytics_pricing_model_products_config_idx
on "ores_analytics_pricing_model_products_tbl" (tenant_id, pricing_model_config_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Search by pricing engine type
create index if not exists ores_analytics_pricing_model_products_engine_type_idx
on "ores_analytics_pricing_model_products_tbl" (tenant_id, pricing_engine_type_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Search by model
create index if not exists ores_analytics_pricing_model_products_model_idx
on "ores_analytics_pricing_model_products_tbl" (tenant_id, model)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Search by engine
create index if not exists ores_analytics_pricing_model_products_engine_idx
on "ores_analytics_pricing_model_products_tbl" (tenant_id, engine)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_analytics_pricing_model_products_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Version management
    select version into current_version
    from "ores_analytics_pricing_model_products_tbl"
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

        update "ores_analytics_pricing_model_products_tbl"
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
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_actor_fn(), current_user);

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Validate pricing_model_config_id (soft FK to pricing_model_configs)
    if not exists (
        select 1 from ores_analytics_pricing_model_configs_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.pricing_model_config_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid pricing_model_config_id: %. Config must exist for tenant.', NEW.pricing_model_config_id
            using errcode = '23503';
    end if;

    -- Validate pricing_engine_type_code (soft FK to pricing_engine_types)
    NEW.pricing_engine_type_code := ores_analytics_validate_pricing_engine_type_fn(NEW.tenant_id, NEW.pricing_engine_type_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_analytics_pricing_model_products_insert_trg
before insert on "ores_analytics_pricing_model_products_tbl"
for each row execute function ores_analytics_pricing_model_products_insert_fn();

create or replace rule ores_analytics_pricing_model_products_delete_rule as
on delete to "ores_analytics_pricing_model_products_tbl" do instead
    update "ores_analytics_pricing_model_products_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
