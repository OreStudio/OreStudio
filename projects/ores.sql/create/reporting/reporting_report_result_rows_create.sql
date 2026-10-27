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
 * Report result row Table
 *
 * One figure a run produced, for one subject in one measure, at the run's priced
 * date. The row is written once, with the run, and never edited; a correction is a
 * new run. as_of is the run's priced date, and it is the time column of the
 * series. The canonical row's shape and the reason it is narrow are stated in the
 * report post processing note.
 */

create table if not exists "ores_reporting_report_result_rows_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "report_instance_id" uuid not null,
    "as_of" timestamp with time zone not null,
    "subject_type" text not null,
    "subject_id" uuid not null,
    "measure" text not null,
    "currency" text not null,
    "report_definition_id" uuid not null,
    "value" double precision not null,
    "source" text not null,
    primary key (tenant_id, id),
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Composite natural key: unique combination for active records
create unique index if not exists report_result_rows_party_id_report_instance_id_as_of_subject_type_subject_id_measure_currency_uniq_idx
on "ores_reporting_report_result_rows_tbl" (tenant_id, party_id, report_instance_id, as_of, subject_type, subject_id, measure, currency)
;



create or replace function ores_reporting_report_result_rows_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_reporting_report_result_rows_insert_trg
before insert on "ores_reporting_report_result_rows_tbl"
for each row execute function ores_reporting_report_result_rows_insert_fn();

create or replace function ores_reporting_report_result_rows_immutable_fn()
returns trigger as $$
begin
    -- A tenant purge is the one sanctioned delete. It turns the switch on
    -- for its own transaction and off again after its delete.
    if TG_OP = 'DELETE' and ores_utility_immutable_purge_allowed_fn() then
        return OLD;
    end if;
    raise exception 'ores_reporting_report_result_rows_tbl rows are immutable: % is refused.', TG_OP
        using errcode = '55000';
end;
$$ language plpgsql set search_path = public, pg_temp;

create or replace trigger ores_reporting_report_result_rows_immutable_trg
before update or delete on "ores_reporting_report_result_rows_tbl"
for each row execute function ores_reporting_report_result_rows_immutable_fn();

create or replace trigger ores_reporting_report_result_rows_immutable_truncate_trg
before truncate on "ores_reporting_report_result_rows_tbl"
for each statement execute function ores_reporting_report_result_rows_immutable_fn();

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
            '"ores_reporting_report_result_rows_tbl"',
            'as_of',
            chunk_time_interval => interval '30 days',
            if_not_exists => true
        );
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;
