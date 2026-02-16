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
 * External identifiers for parties, such as LEI codes, BIC/SWIFT codes,
 * national registration numbers, and tax identifiers. Each party can have
 * multiple identifiers across different schemes.
 */

create table if not exists "ores_refdata_party_identifiers_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "id_scheme" text not null,
    "id_value" text not null,
    "description" text null,
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
create unique index if not exists ores_refdata_party_identifiers_version_uniq_idx
on "ores_refdata_party_identifiers_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_refdata_party_identifiers_id_uniq_idx
on "ores_refdata_party_identifiers_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_refdata_party_identifiers_tenant_idx
on "ores_refdata_party_identifiers_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_refdata_party_identifiers_party_scheme_value_uniq_idx
on "ores_refdata_party_identifiers_tbl" (tenant_id, party_id, id_scheme, id_value)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_party_identifiers_insert_fn()
returns trigger as $$
declare
    current_version integer;
    v_max_cardinality integer;
    v_current_count integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate id_scheme
    NEW.id_scheme := ores_refdata_validate_party_id_scheme_fn(NEW.tenant_id, NEW.id_scheme);

    -- Validate party_id (soft FK)
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = NEW.tenant_id and id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_id: %. No active party found with this id.',
            NEW.party_id
            using errcode = '23503';
    end if;

    -- Validate max_cardinality for this scheme
    select max_cardinality into v_max_cardinality
    from ores_refdata_party_id_schemes_tbl
    where tenant_id = NEW.tenant_id
      and code = NEW.id_scheme
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_max_cardinality is not null then
        select count(*) into v_current_count
        from ores_refdata_party_identifiers_tbl
        where tenant_id = NEW.tenant_id
          and party_id = NEW.party_id
          and id_scheme = NEW.id_scheme
          and id != NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_current_count >= v_max_cardinality then
            raise exception 'Cardinality violation for scheme %: party % already has % identifier(s) (max %).',
                NEW.id_scheme, NEW.party_id, v_current_count, v_max_cardinality
                using errcode = '23514';
        end if;
    end if;

    -- Version management
    select version into current_version
    from "ores_refdata_party_identifiers_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update "ores_refdata_party_identifiers_tbl"
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

create or replace trigger ores_refdata_party_identifiers_insert_trg
before insert on "ores_refdata_party_identifiers_tbl"
for each row execute function ores_refdata_party_identifiers_insert_fn();

create or replace rule ores_refdata_party_identifiers_delete_rule as
on delete to "ores_refdata_party_identifiers_tbl" do instead
    update "ores_refdata_party_identifiers_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
