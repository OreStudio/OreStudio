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
 * Market State Payload Table
 *
 * The market state an engine run priced against, kept as the file the engine
 * wrote. A cube states a column for every curve and tenor the run built — the
 * widest checked-in sample states 789 — so a row for each value would be millions
 * of rows per run, and a read that needs one factor would unpack all of them.
 *
 * The payload is therefore stored once, compressed, with an index beside it: the
 * headings, the number of rows, and the distinct values of each key the catalogue
 * names, which are the dates, the scenarios, the factors or the market objects the
 * file covers. A reader that wants one slice asks the index first and unpacks only
 * what it must.
 *
 * This is market state and not a run result: it says what the world looked like,
 * it is what the engine read, and it belongs beside the series and the fixings the
 * market data store already keeps.
 */

create table if not exists "ores_marketdata_market_state_payloads_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "report_instance_id" uuid not null,
    "family" text not null,
    "file_name" text not null,
    "as_of" timestamp with time zone not null,
    "report_definition_id" uuid not null,
    "row_count" integer not null,
    "column_count" integer not null,
    "column_names" text not null,
    "key_values" text not null,
    "keys_truncated" boolean not null,
    "payload" text not null,
    "payload_bytes" integer not null,
    "content_sha256" text not null,
    "source" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, as_of),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Composite natural key: unique combination for active records
create unique index if not exists market_state_payloads_party_id_report_instance_id_family_file_name_as_of_uniq_idx
on "ores_marketdata_market_state_payloads_tbl" (tenant_id, party_id, report_instance_id, family, file_name, as_of)
where valid_to = ores_utility_infinity_timestamp_fn();



create or replace function ores_marketdata_market_state_payloads_insert_fn()
returns trigger as $$
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    update "ores_marketdata_market_state_payloads_tbl"
    set valid_to = current_timestamp
    where tenant_id = new.tenant_id
      and party_id = new.party_id
      and report_instance_id = new.report_instance_id
      and as_of = new.as_of
      and family = new.family
      and file_name = new.file_name
      and valid_to = ores_utility_infinity_timestamp_fn()
      and valid_from < current_timestamp;

    new.valid_from := current_timestamp;
    new.valid_to   := ores_utility_infinity_timestamp_fn();
    return new;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_market_state_payloads_insert_trg
before insert on "ores_marketdata_market_state_payloads_tbl"
for each row execute function ores_marketdata_market_state_payloads_insert_fn();

create or replace function ores_marketdata_market_state_payloads_delete_fn()
returns trigger as $$
begin
    update "ores_marketdata_market_state_payloads_tbl"
    set valid_to = current_timestamp
    where tenant_id = old.tenant_id
      and party_id = old.party_id
      and report_instance_id = old.report_instance_id
      and as_of = old.as_of
      and family = old.family
      and file_name = old.file_name
      and valid_to = ores_utility_infinity_timestamp_fn();
    return null;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_market_state_payloads_delete_trg
before delete on "ores_marketdata_market_state_payloads_tbl"
for each row execute function ores_marketdata_market_state_payloads_delete_fn();
do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice 'TimescaleDB detected - creating hypertable (30 days chunks)';

        perform public.create_hypertable(
            '"ores_marketdata_market_state_payloads_tbl"',
            'as_of',
            chunk_time_interval => interval '30 days',
            if_not_exists => true
        );
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

-- =============================================================================
-- Row-level security: tenant isolation for Market State Payload
-- =============================================================================
alter table ores_marketdata_market_state_payloads_tbl enable row level security;

drop policy if exists market_state_payloads_tbl_tenant_isolation_policy
    on ores_marketdata_market_state_payloads_tbl;

create policy market_state_payloads_tbl_tenant_isolation_policy
on ores_marketdata_market_state_payloads_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation (RESTRICTIVE): ANDed with the permissive tenant
-- policy above, a session sees only rows whose party_id its visible
-- party set admits. The visible_party_ids-is-null passthrough applies
-- for sessions with no party restriction (tenant admins, service
-- contexts).
drop policy if exists market_state_payloads_tbl_party_isolation_policy
    on ores_marketdata_market_state_payloads_tbl;

create policy market_state_payloads_tbl_party_isolation_policy
on ores_marketdata_market_state_payloads_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
