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
 * Report Cashflow Table
 *
 * A cashflow is not a metric. A metric states one figure about a subject; a
 * cashflow states a row per payment, keyed by the leg and the payment number, and
 * the stressed families state each of its figures three times, as a base, a
 * scenario and the difference between them.
 *
 * The row here is therefore narrow, and keeps the cashflow's own keys: one figure,
 * named by its heading, with the part of the stressed comparison it states. A
 * family of cashflows adds no column to this table, and no figure of a cashflow is
 * flattened into report_metric, because the keys that make a cashflow a cashflow
 * have no place in a metric.
 *
 * The row is written once, with the run, and never edited; a correction is a new
 * run.
 */

create table if not exists "ores_reporting_report_cashflows_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "report_instance_id" uuid not null,
    "family" text not null,
    "subject_type" text not null,
    "subject_id" text not null,
    "scenario" text not null,
    "cashflow_no" text not null,
    "leg_no" text not null,
    "as_of" timestamp with time zone not null,
    "measure" text not null,
    "part" text not null,
    "report_definition_id" uuid not null,
    "pay_date" timestamp with time zone null,
    "flow_type" text null,
    "currency" text null,
    "qualifiers" text null,
    "value" double precision not null,
    "source" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, as_of),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Composite natural key: unique combination for active records
create unique index if not exists report_cashflows_party_id_report_instance_id_family_subject_type_subject_id_scenario_cashflow_no_leg_no_as_of_measure_part_uniq_idx
on "ores_reporting_report_cashflows_tbl" (tenant_id, party_id, report_instance_id, family, subject_type, subject_id, scenario, cashflow_no, leg_no, as_of, measure, part)
where valid_to = ores_utility_infinity_timestamp_fn();



create or replace function ores_reporting_report_cashflows_insert_fn()
returns trigger as $$
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    update "ores_reporting_report_cashflows_tbl"
    set valid_to = current_timestamp
    where tenant_id = new.tenant_id
      and party_id = new.party_id
      and report_instance_id = new.report_instance_id
      and as_of = new.as_of
      and family = new.family
      and subject_type = new.subject_type
      and subject_id = new.subject_id
      and measure = new.measure
      and cashflow_no = new.cashflow_no
      and leg_no = new.leg_no
      and scenario = new.scenario
      and part = new.part
      and valid_to = ores_utility_infinity_timestamp_fn()
      and valid_from < current_timestamp;

    new.valid_from := current_timestamp;
    new.valid_to   := ores_utility_infinity_timestamp_fn();
    return new;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_reporting_report_cashflows_insert_trg
before insert on "ores_reporting_report_cashflows_tbl"
for each row execute function ores_reporting_report_cashflows_insert_fn();

create or replace function ores_reporting_report_cashflows_delete_fn()
returns trigger as $$
begin
    update "ores_reporting_report_cashflows_tbl"
    set valid_to = current_timestamp
    where tenant_id = old.tenant_id
      and party_id = old.party_id
      and report_instance_id = old.report_instance_id
      and as_of = old.as_of
      and family = old.family
      and subject_type = old.subject_type
      and subject_id = old.subject_id
      and measure = old.measure
      and cashflow_no = old.cashflow_no
      and leg_no = old.leg_no
      and scenario = old.scenario
      and part = old.part
      and valid_to = ores_utility_infinity_timestamp_fn();
    return null;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_reporting_report_cashflows_delete_trg
before delete on "ores_reporting_report_cashflows_tbl"
for each row execute function ores_reporting_report_cashflows_delete_fn();
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
            '"ores_reporting_report_cashflows_tbl"',
            'as_of',
            chunk_time_interval => interval '30 days',
            if_not_exists => true
        );
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

-- =============================================================================
-- Row-level security: tenant isolation for Report Cashflow
-- =============================================================================
alter table ores_reporting_report_cashflows_tbl enable row level security;

drop policy if exists report_cashflows_tbl_tenant_isolation_policy
    on ores_reporting_report_cashflows_tbl;

create policy report_cashflows_tbl_tenant_isolation_policy
on ores_reporting_report_cashflows_tbl
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
drop policy if exists report_cashflows_tbl_party_isolation_policy
    on ores_reporting_report_cashflows_tbl;

create policy report_cashflows_tbl_party_isolation_policy
on ores_reporting_report_cashflows_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
