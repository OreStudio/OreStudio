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
 * Report Error Table
 *
 * +#+updated: 2026-10-16
 *
 * One error a report run produced, held against the run. A report run is one
 * execution of a report definition, and a run whose analytics fail is still a run
 * that completed: the engine finished, wrote its report, and said in its own log
 * that a trade would not build. Those errors are facts about the run, they belong
 * to it, and they are what this table holds.
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

create table if not exists "ores_reporting_report_errors_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "report_instance_id" uuid not null,
    "party_id" uuid not null,
    "scope_code" text not null default 'Report',
    "severity" text not null default 'error',
    "message" text not null,
    "subject" text null,
    "subject_id" text null,
    "detail" jsonb null,
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
    check ("id" <> ores_utility_nil_uuid_fn()),
    check (("subject" is null) = ("subject_id" is null)),
    check ("severity" in ('error', 'warning'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists report_errors_version_uniq_idx
on "ores_reporting_report_errors_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists report_errors_id_uniq_idx
on "ores_reporting_report_errors_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists report_errors_tenant_idx
on "ores_reporting_report_errors_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists report_errors_subject_idx
on "ores_reporting_report_errors_tbl" (tenant_id, subject, subject_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_report_errors_insert_fn()
returns trigger as $$
declare
    current_version integer;
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

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_reporting_report_errors_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        -- The write states what it believes about the row, and the store is
        -- what decides. Version zero means one thing: no current row exists.
        -- So a create that collides with a live row is refused here, for every
        -- client, rather than by a check each client has to remember.
        if NEW.version = 0 then
            if not ores_utility_version_replace_allowed_fn() then
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'report_error',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'report_error',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_reporting_report_errors_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_reporting_report_errors_insert_trg
before insert on "ores_reporting_report_errors_tbl"
for each row execute function ores_reporting_report_errors_insert_fn();

create or replace rule ores_reporting_report_errors_delete_rule as
on delete to "ores_reporting_report_errors_tbl" do instead (
    update "ores_reporting_report_errors_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
