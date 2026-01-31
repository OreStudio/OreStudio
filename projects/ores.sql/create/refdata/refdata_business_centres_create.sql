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
-- The coding-scheme accepts a 4 character code of the real geographical business calendar location or FpML format of the rate publication calendar. While the 4 character codes of the business calendar location are implicitly locatable and used for identifying a bad business day for the purpose of payment and rate calculation day adjustments, the rate publication calendar codes are used in the context of the fixing day offsets.
-- =============================================================================

create table if not exists "ores_refdata_business_centres_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "source" text null,
    "description" text null,
    "coding_scheme_code" text not null,
    "image_id" uuid,
    "modified_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (code, coding_scheme_code, valid_from, valid_to),
    exclude using gist (
        code WITH =,
        coding_scheme_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("code" <> ''),
    check ("change_reason_code" <> '')
);

create unique index if not exists ores_refdata_business_centres_version_uniq_idx
on "ores_refdata_business_centres_tbl" (code, coding_scheme_code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_refdata_business_centres_code_uniq_idx
on "ores_refdata_business_centres_tbl" (tenant_id, code, coding_scheme_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_refdata_business_centres_tenant_idx
on "ores_refdata_business_centres_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_refdata_business_centres_coding_scheme_idx
on "ores_refdata_business_centres_tbl" (coding_scheme_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_business_centres_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Validate foreign key references
    if NEW.coding_scheme_code is not null and not exists (
        select 1 from ores_dq_coding_schemes_tbl
        where code = NEW.coding_scheme_code
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid coding_scheme_code: %. Coding scheme must exist.', NEW.coding_scheme_code
        using errcode = '23503';
    end if;

    select version into current_version
    from "ores_refdata_business_centres_tbl"
    where code = new.code
      and coding_scheme_code = new.coding_scheme_code
    and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        update "ores_refdata_business_centres_tbl"
        set valid_to = current_timestamp
        where code = new.code
          and coding_scheme_code = new.coding_scheme_code
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();
    if new.modified_by is null or new.modified_by = '' then
        new.modified_by = current_user;
    end if;

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_refdata_business_centres_insert_trg
before insert on "ores_refdata_business_centres_tbl"
for each row
execute function ores_refdata_business_centres_insert_fn();

create or replace rule ores_refdata_business_centres_delete_rule as
on delete to "ores_refdata_business_centres_tbl"
do instead
  update "ores_refdata_business_centres_tbl"
  set valid_to = current_timestamp
  where code = old.code
  and coding_scheme_code = old.coding_scheme_code
  and valid_to = ores_utility_infinity_timestamp_fn();
