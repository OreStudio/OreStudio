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
 * Report Error Event Table
 *
 * +#+updated: 2026-10-17
 *
 * One error a report run produced, held against the run. A report run is one
 * execution of a report definition, and a run whose analytics fail is still a run
 * that completed: the engine finished, wrote its report, and said in its own log
 * that a trade would not build. Those errors are facts about the run, they belong
 * to it, and they are what this table holds.
 *
 * An error is a fact about an instant: it is written once and never changed. The
 * model therefore states :current_state: and :immutable: in the * SQL **
 * Flags drawer, as [[id:47E6903C-DFA7-4D2F-B044-C1C74428F719][ores.reporting.report_trigger_event]]
 * does. A current-state immutable table has no validity window and no audit tail:
 * there is no version, no valid_from/valid_to, and no modified_by,
 * performed_by, change_reason_code or change_commentary. Because it has no
 * audit tail, the time is a column of its own, event_time.
 *
 * event_time is the moment the errors were collected from the run's output. The
 * canonical rows do state the engine's own time, as the text the engine wrote, and
 * engine_log_error keeps it; this column does not hold it. The recorder writes
 * the collection instant because it is an instant the service can state about its
 * own act, and storing the engine's text as a timestamp here would put two
 * different facts in one column. A story that needs the engine's instant adds a
 * column for it.
 *
 * A row says four things: how bad the error is, what it is about, what it said,
 * and which subject it names. The first is severity. The second is scope_code,
 * drawn from the engine's own vocabulary, and it is what places the error:
 * Trade on the trade, Analytics on the analytic, Curve on the curve, and
 * Report on the run itself. The third is message, the engine's own words.
 *
 * The fourth is subject and subject_id, a discriminator and a value, holding
 * the identifier of the thing the error names: trade and a trade id,
 * analytic and an analytic type, curve and a curve id. It is a discriminator
 * and a value rather than one column because the ids are of different kinds — a
 * trade id is a UUID the portfolio builder minted, a curve id is a free-text name
 * such as [IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)] — and one column holding
 * both would be a text column no reader can key on. It is not a database foreign
 * key for the same reason: the engine names trades by its own ids and curves by
 * their ORE names, and neither is a key any table here holds. The pair is stored
 * so that a screen can find every error about one subject, and it is a
 * discriminator rather than a fixed column per kind because the engine adds
 * sub-fields to the groups it already has, as the corpus shows it doing when an
 * Analytics error grew a tradeId.
 *
 * scope_code and subject are not the same question. The scope is the group the
 * engine emitted, which is what the error is about at the level the engine named
 * it; the subject is the particular thing, and it can be narrower than the scope.
 * An Analytics error that names a tradeId is about that trade and not about
 * the analytic as a whole, and its subject says so.
 *
 * A run that fails before the engine sees it, because a workflow step did not
 * complete, produces a row too, with scope Report. That is why the table is one
 * table rather than two: a reader opening a report wants its errors in one place.
 */

create table if not exists "ores_reporting_report_error_events_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "report_instance_id" uuid not null,
    "party_id" uuid not null,
    "scope_code" text not null default 'Report',
    "severity" text not null default 'error',
    "message" text not null,
    "subject" text null,
    "subject_id" text null,
    "subject_version" integer null,
    "subject_as_of" timestamp with time zone null,
    "cause" text null,
    "detail" jsonb null,
    "event_time" timestamp with time zone not null,
    primary key (tenant_id, id),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check (("subject" is null) = ("subject_id" is null)),
    check ("severity" in ('error', 'warning')),
    check (("subject" is not null) or ("subject_version" is null and "subject_as_of" is null))
);



create index if not exists report_error_events_subject_idx
on "ores_reporting_report_error_events_tbl" (tenant_id, subject, subject_id);

create unique index if not exists report_error_events_run_subject_idx
on "ores_reporting_report_error_events_tbl" (tenant_id, report_instance_id, scope_code, subject, subject_id, message, detail)
where subject_id is not null and severity = 'error';

create unique index if not exists report_error_events_run_report_idx
on "ores_reporting_report_error_events_tbl" (tenant_id, report_instance_id, scope_code, message, detail)
where subject_id is null and severity = 'error';

create or replace function ores_reporting_report_error_events_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate report_instance_id (soft FK to ores_reporting_report_instances_tbl)
    if not exists (
        select 1 from ores_reporting_report_instances_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.report_instance_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid report_instance_id: %. No active report instance found with this id.', NEW.report_instance_id
            using errcode = '23503';
    end if;

    -- Validate scope_code (soft FK to ores_reporting_report_error_scopes_tbl)
    if not exists (
        select 1 from ores_reporting_report_error_scopes_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.scope_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid scope: %. No active report error scope found with this code.', NEW.scope_code
            using errcode = '23503';
    end if;



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_reporting_report_error_events_insert_trg
before insert on "ores_reporting_report_error_events_tbl"
for each row execute function ores_reporting_report_error_events_insert_fn();

create or replace function ores_reporting_report_error_events_immutable_fn()
returns trigger as $$
begin
    -- A tenant purge is the one sanctioned delete. It turns the switch on
    -- for its own transaction and off again after its delete.
    if TG_OP = 'DELETE' and ores_utility_immutable_purge_allowed_fn() then
        return OLD;
    end if;
    raise exception 'ores_reporting_report_error_events_tbl rows are immutable: % is refused.', TG_OP
        using errcode = '55000';
end;
$$ language plpgsql set search_path = public, pg_temp;

create or replace trigger ores_reporting_report_error_events_immutable_trg
before update or delete on "ores_reporting_report_error_events_tbl"
for each row execute function ores_reporting_report_error_events_immutable_fn();

create or replace trigger ores_reporting_report_error_events_immutable_truncate_trg
before truncate on "ores_reporting_report_error_events_tbl"
for each statement execute function ores_reporting_report_error_events_immutable_fn();


-- =============================================================================
-- Row-level security: tenant isolation for Report Error Event
-- =============================================================================
alter table ores_reporting_report_error_events_tbl enable row level security;

drop policy if exists report_error_events_tbl_tenant_isolation_policy
    on ores_reporting_report_error_events_tbl;

create policy report_error_events_tbl_tenant_isolation_policy
on ores_reporting_report_error_events_tbl
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
drop policy if exists report_error_events_tbl_party_isolation_policy
    on ores_reporting_report_error_events_tbl;

create policy report_error_events_tbl_party_isolation_policy
on ores_reporting_report_error_events_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
