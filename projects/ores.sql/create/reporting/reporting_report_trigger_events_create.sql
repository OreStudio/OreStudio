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
 * Report Trigger Event Table
 *
 * One row per report trigger. A trigger is how a run is admitted, and it is of two
 * types: a person asks for the run now, or a schedule asks for it later. The
 * scheduler does not write this table. Its job publishes a trigger event, and the
 * reporting service records the trigger here and admits the run, so both types
 * reach the same handler and differ only in the payload.
 *
 * The rows are append-only, so the model states :current_state:: an event is a
 * fact about an instant, and the bi-temporal pair has no meaning on it. A
 * current-state immutable table has no audit tail, so the actor and the time are
 * columns of its own and there is no version and no validity window. The table is
 * plain, not a hypertable: ores.iam.auth_event is one because logins are high
 * volume, and triggers are a handful a day.
 */

create table if not exists "ores_reporting_report_trigger_events_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "report_definition_id" uuid not null,
    "trigger_type" text not null,
    "schedule_job_id" text null,
    "schedule_job_run_id" bigint null,
    "triggered_by" text not null,
    "triggered_at" timestamp with time zone not null,
    "party_id" uuid not null,
    primary key (tenant_id, id),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("trigger_type" <> 'scheduled' or "schedule_job_id" is not null)
);



create or replace function ores_reporting_report_trigger_events_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate trigger_type (soft FK to ores_reporting_report_trigger_types_tbl)
    if not exists (
        select 1 from ores_reporting_report_trigger_types_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.trigger_type
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid trigger_type: %. No active report trigger type found with this code.', NEW.trigger_type
            using errcode = '23503';
    end if;

    -- Validate schedule_job_id (optional soft FK to ores_scheduler_job_definitions_tbl)
    if NEW.schedule_job_id is not null then
        if not exists (
            select 1 from ores_scheduler_job_definitions_tbl
            where id = NEW.schedule_job_id
        ) then
            raise exception 'Invalid schedule_job_id: %. No scheduler job definition found with this id.', NEW.schedule_job_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate schedule_job_run_id (optional soft FK to ores_scheduler_job_instances_tbl)
    if NEW.schedule_job_run_id is not null then
        if not exists (
            select 1 from ores_scheduler_job_instances_tbl
            where id = NEW.schedule_job_run_id
        ) then
            raise exception 'Invalid schedule_job_run_id: %. No scheduler job instance found with this id.', NEW.schedule_job_run_id
                using errcode = '23503';
        end if;
    end if;



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_reporting_report_trigger_events_insert_trg
before insert on "ores_reporting_report_trigger_events_tbl"
for each row execute function ores_reporting_report_trigger_events_insert_fn();

create or replace function ores_reporting_report_trigger_events_immutable_fn()
returns trigger as $$
begin
    -- A tenant purge is the one sanctioned delete. It turns the switch on
    -- for its own transaction and off again after its delete.
    if TG_OP = 'DELETE' and ores_utility_immutable_purge_allowed_fn() then
        return OLD;
    end if;
    raise exception 'ores_reporting_report_trigger_events_tbl rows are immutable: % is refused.', TG_OP
        using errcode = '55000';
end;
$$ language plpgsql set search_path = public, pg_temp;

create or replace trigger ores_reporting_report_trigger_events_immutable_trg
before update or delete on "ores_reporting_report_trigger_events_tbl"
for each row execute function ores_reporting_report_trigger_events_immutable_fn();

create or replace trigger ores_reporting_report_trigger_events_immutable_truncate_trg
before truncate on "ores_reporting_report_trigger_events_tbl"
for each statement execute function ores_reporting_report_trigger_events_immutable_fn();


-- =============================================================================
-- Row-level security: tenant isolation for Report Trigger Event
-- =============================================================================
alter table ores_reporting_report_trigger_events_tbl enable row level security;

drop policy if exists report_trigger_events_tbl_tenant_isolation_policy
    on ores_reporting_report_trigger_events_tbl;

create policy report_trigger_events_tbl_tenant_isolation_policy
on ores_reporting_report_trigger_events_tbl
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
drop policy if exists report_trigger_events_tbl_party_isolation_policy
    on ores_reporting_report_trigger_events_tbl;

create policy report_trigger_events_tbl_party_isolation_policy
on ores_reporting_report_trigger_events_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
