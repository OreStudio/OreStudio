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
 * Tenor Convention Table
 *
 * Persisted catalog of the resolution schemes described in
 * [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][Tenor]]'s "Tenor conventions by
 * curve type" section — spot/forward curves, FX swap curves (near-leg
 * quoting), credit/CDS curves (IMM-anchored), and volatility surfaces
 * (measured from today rather than spot). Each row names the *default*
 * [[id:3F8B6C2A-1D4E-4A7F-9B3C-6E2D8F1A5C90][tenor anchor]] a convention's
 * regular PERIOD tenors resolve from; which [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][tenor]]
 * labels actually belong to a given convention, and any per-tenor anchor
 * override (needed for SPECIAL tenors such as O/N, which resolve
 * differently under the spot/forward convention than under the swap
 * convention), is recorded in
 * [[id:E1F5A9C3-6D2B-4E8A-B7F1-3C9D5A2E6B48][Tenor Convention Resolution]],
 * not here.
 */

create table if not exists "ores_marketdata_tenor_conventions_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "description" text null,
    "measured_from" text not null,
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
    check ("code" <> '')
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists tenor_conventions_version_uniq_idx
on "ores_marketdata_tenor_conventions_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists tenor_conventions_code_uniq_idx
on "ores_marketdata_tenor_conventions_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists tenor_conventions_tenant_idx
on "ores_marketdata_tenor_conventions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_marketdata_tenor_conventions_insert_fn()
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
    from "ores_marketdata_tenor_conventions_tbl"
    where tenant_id = NEW.tenant_id
      and code = NEW.code
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
        update "ores_marketdata_tenor_conventions_tbl"
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
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_tenor_conventions_insert_trg
before insert on "ores_marketdata_tenor_conventions_tbl"
for each row execute function ores_marketdata_tenor_conventions_insert_fn();

create or replace rule ores_marketdata_tenor_conventions_delete_rule as
on delete to "ores_marketdata_tenor_conventions_tbl" do instead (
    update "ores_marketdata_tenor_conventions_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
);
