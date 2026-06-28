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

-- =============================================================================
-- Synthetic GMM components (weighted normal distributions making up the
-- Gaussian Mixture Model price process of an fx_spot_generation_config)
-- =============================================================================

create table if not exists "ores_synthetic_gmm_components_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "fx_spot_config_id" uuid not null,
    "version" integer not null,
    "component_index" integer not null,
    "description" text not null,
    "mean" double precision not null,
    "stdev" double precision not null,
    "weight" double precision not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    constraint sgmc_temporal_excl exclude using gist (
        tenant_id WITH =,
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("stdev" > 0),
    check ("weight" >= 0)
);

create unique index if not exists sgmc_version_uniq_idx
on "ores_synthetic_gmm_components_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists sgmc_id_uniq_idx
on "ores_synthetic_gmm_components_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- A component's position is unique within its owning FX spot config.
create unique index if not exists sgmc_config_index_uniq_idx
on "ores_synthetic_gmm_components_tbl" (tenant_id, fx_spot_config_id, component_index)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists sgmc_tenant_idx
on "ores_synthetic_gmm_components_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists sgmc_fx_spot_config_id_idx
on "ores_synthetic_gmm_components_tbl" (tenant_id, fx_spot_config_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_synthetic_gmm_components_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Validate change_reason_code
    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    -- Validate fx_spot_config_id (soft FK to ores_synthetic_fx_spot_generation_configs_tbl)
    if not exists (
        select 1 from ores_synthetic_fx_spot_generation_configs_tbl
        where tenant_id = new.tenant_id
          and id = new.fx_spot_config_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid fx_spot_config_id: %. No active FX spot generation config found with this id.', new.fx_spot_config_id
            using errcode = '23503';
    end if;

    select version into current_version
    from "ores_synthetic_gmm_components_tbl"
    where tenant_id = new.tenant_id
      and id = new.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "ores_synthetic_gmm_components_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
          and id = new.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();
    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_synthetic_gmm_components_insert_trg
before insert on "ores_synthetic_gmm_components_tbl"
for each row
execute function ores_synthetic_gmm_components_insert_fn();

create or replace rule ores_synthetic_gmm_components_delete_rule as
on delete to "ores_synthetic_gmm_components_tbl"
do instead
  update "ores_synthetic_gmm_components_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and id = old.id
  and valid_to = ores_utility_infinity_timestamp_fn();
