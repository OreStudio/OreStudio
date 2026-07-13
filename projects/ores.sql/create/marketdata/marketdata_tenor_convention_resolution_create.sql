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
 * Template: sql_schema_junction_create.mustache
 * To modify, update the template and regenerate.
 *
 * Tenor Convention Resolution Table
 *
 * A [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][tenor]] does not resolve the
 * same way under every [[id:C4D8A2E6-3B7F-4A1D-9C5E-8F2A6D3B1E90][tenor
 * convention]] — O/N resolves from horizon to tomorrow under the
 * spot/forward convention, but means "today" under the FX swap convention —
 * and not every tenor belongs to every convention's set at all (the swap
 * convention's tenor set stops at S/N; the credit/CDS convention only uses
 * IMM-quarter labels). This junction records both facts as one row per valid
 * (convention, tenor) pair: the row's mere presence is set membership, and
 * its optional anchor_override column carries the per-row exception to the
 * convention's own default anchor, needed only for the SPECIAL tenors
 * whose resolution genuinely varies by convention. A PERIOD tenor's
 * resolution never needs an override — its offset from the anchor is fixed
 * by its own unit/multiplier, so only the anchor itself (the
 * convention's measured_from, or this row's override) changes.
 */

create table if not exists "ores_marketdata_tenor_convention_resolutions_tbl" (
    "convention_code" text not null,
    "tenant_id" uuid not null,
    "tenor_code" text not null,
    "version" integer not null,
    "anchor_override" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, convention_code, tenor_code, valid_from),
    exclude using gist (
        tenant_id WITH =,
        convention_code WITH =,
        tenor_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Tenor convention this resolution row belongs to.
create index if not exists tenor_convention_resolutions_convention_idx
on "ores_marketdata_tenor_convention_resolutions_tbl" (convention_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Tenor valid under this convention.
create index if not exists tenor_convention_resolutions_tenor_idx
on "ores_marketdata_tenor_convention_resolutions_tbl" (tenor_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists tenor_convention_resolutions_uniq_idx
on "ores_marketdata_tenor_convention_resolutions_tbl" (tenant_id, convention_code, tenor_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists tenor_convention_resolutions_tenant_idx
on "ores_marketdata_tenor_convention_resolutions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_marketdata_tenor_convention_resolutions_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Version management
    select version into current_version
    from "ores_marketdata_tenor_convention_resolutions_tbl"
    where tenant_id = new.tenant_id
    and convention_code = new.convention_code
    and tenor_code = new.tenor_code
    and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        -- Close existing record
        update "ores_marketdata_tenor_convention_resolutions_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
        and convention_code = new.convention_code
        and tenor_code = new.tenor_code
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_marketdata_tenor_convention_resolutions_insert_trg
before insert on "ores_marketdata_tenor_convention_resolutions_tbl"
for each row
execute function ores_marketdata_tenor_convention_resolutions_insert_fn();

create or replace rule ores_marketdata_tenor_convention_resolutions_delete_rule as
on delete to "ores_marketdata_tenor_convention_resolutions_tbl"
do instead
  update "ores_marketdata_tenor_convention_resolutions_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and convention_code = old.convention_code
  and tenor_code = old.tenor_code
  and valid_to = ores_utility_infinity_timestamp_fn();
