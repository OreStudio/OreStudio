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
 * Stores model parameters, engine parameters, and global parameters as normalised rows
 * for granular diffing. Product-scoped parameters have pricing_model_product_id set;
 * global parameters have it NULL with parameter_scope = 'global'.
 */

create table if not exists "ores_analytics_pricing_model_product_parameters_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "pricing_model_config_id" uuid not null,
    "pricing_model_product_id" uuid null,
    "parameter_scope" text not null,
    "parameter_name" text not null,
    "parameter_value" text not null,
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
create unique index if not exists ores_analytics_pricing_model_product_parameters_version_uniq_idx
on "ores_analytics_pricing_model_product_parameters_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_analytics_pricing_model_product_parameters_id_uniq_idx
on "ores_analytics_pricing_model_product_parameters_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_analytics_pricing_model_product_parameters_tenant_idx
on "ores_analytics_pricing_model_product_parameters_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- FK lookup: all parameters for a config
create index if not exists ores_analytics_pricing_model_product_parameters_config_idx
on "ores_analytics_pricing_model_product_parameters_tbl" (tenant_id, pricing_model_config_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- FK lookup: all parameters for a product
create index if not exists ores_analytics_pricing_model_product_parameters_product_idx
on "ores_analytics_pricing_model_product_parameters_tbl" (tenant_id, pricing_model_product_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_analytics_pricing_model_product_parameters_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Version management
    select version into current_version
    from "ores_analytics_pricing_model_product_parameters_tbl"
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

        update "ores_analytics_pricing_model_product_parameters_tbl"
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

    -- Validate parameter_scope
    if NEW.parameter_scope not in ('model', 'engine', 'global') then
        raise exception 'Invalid parameter_scope: %. Must be one of: model, engine, global.', NEW.parameter_scope
            using errcode = '23514';
    end if;

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

    -- Validate pricing_model_product_id (optional soft FK to pricing_model_products)
    if NEW.pricing_model_product_id is not null then
        if not exists (
            select 1 from ores_analytics_pricing_model_products_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.pricing_model_product_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid pricing_model_product_id: %. Product must exist for tenant.', NEW.pricing_model_product_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate consistency: global params must have NULL product_id
    if NEW.parameter_scope = 'global' and NEW.pricing_model_product_id is not null then
        raise exception 'Global parameters must have NULL pricing_model_product_id.'
            using errcode = '23514';
    end if;

    -- Validate consistency: model/engine params must have a product_id
    if NEW.parameter_scope in ('model', 'engine') and NEW.pricing_model_product_id is null then
        raise exception 'Model and engine parameters must have a non-NULL pricing_model_product_id.'
            using errcode = '23514';
    end if;

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_analytics_pricing_model_product_parameters_insert_trg
before insert on "ores_analytics_pricing_model_product_parameters_tbl"
for each row execute function ores_analytics_pricing_model_product_parameters_insert_fn();

create or replace rule ores_analytics_pricing_model_product_parameters_delete_rule as
on delete to "ores_analytics_pricing_model_product_parameters_tbl" do instead
    update "ores_analytics_pricing_model_product_parameters_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
