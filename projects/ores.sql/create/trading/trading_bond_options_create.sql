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
 * Bond Option Table
 *
 * One row per bond option trade, keyed by the instrument row it
 * extends. The ER row names option_type and option_strike, which map
 * from optionData and the strikeGroup of bondOptionData
 * (instruments.xsd lines 2273-2282).
 *
 * Three members of bondOptionData sit beside its option block rather
 * than inside it: the redemption code, the price type and the knock-out
 * flag. The shared option element states none of them, so an Ascot never
 * writes them and no other table can hold them. They ride here, on the
 * one row that is a bond option.
 *
 * The rest of the block lands elsewhere. The option element's own
 * members go to instrument_option and its keyed children, and the
 * exercise dates land as schedule rows in the shared instrument-keyed
 * schedule tables, under the owner role option.
 */

create table if not exists "ores_trading_bond_options_tbl" (
    "instrument_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "option_type" text not null,
    "option_strike" numeric(28, 10) not null,
    "redemption" text null,
    "price_type" text null,
    "knocks_out" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, instrument_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        instrument_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("instrument_id" <> ores_utility_nil_uuid_fn()),
    check ("option_type" in ('Call', 'Put')),
    check ("option_strike" >= 0)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists bond_options_version_uniq_idx
on "ores_trading_bond_options_tbl" (tenant_id, instrument_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists bond_options_id_uniq_idx
on "ores_trading_bond_options_tbl" (tenant_id, instrument_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists bond_options_tenant_idx
on "ores_trading_bond_options_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_bond_options_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_bond_options_tbl"
    where tenant_id = NEW.tenant_id
      and instrument_id = NEW.instrument_id
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
        update "ores_trading_bond_options_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and instrument_id = NEW.instrument_id
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

create or replace trigger ores_trading_bond_options_insert_trg
before insert on "ores_trading_bond_options_tbl"
for each row execute function ores_trading_bond_options_insert_fn();

create or replace rule ores_trading_bond_options_delete_rule as
on delete to "ores_trading_bond_options_tbl" do instead (
    update "ores_trading_bond_options_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and instrument_id = OLD.instrument_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
