--
-- Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
--
-- This program is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
-- Street, Fifth Floor, Boston, MA 02110-1301, USA.
--
-- ores_scheduler_retention_runs_tbl records what a sweep did, one row per policy
-- it applied, and ores_scheduler_apply_retention_policies_fn applies the enabled
-- policies that are due. The sweep exists because a retention policy installed
-- as a storage engine job only exists where the engine's licensed edition
-- includes jobs; a chunk drop executed here runs on either edition, and records
-- which path ran.
--
-- A policy states a cadence, and the ledger states when it last ran. A policy is
-- due once its cadence has passed since the last row the ledger holds for it, so
-- an installation states a different cadence per store and one sweep honours
-- them all. The sweep writes nothing for a policy that is not due: a row per
-- policy per tick would grow the ledger faster than the stores it bounds shrink.

create table if not exists "ores_scheduler_retention_runs_tbl" (
    "id" uuid not null,
    "policy_code" text not null,
    "action" text not null,
    "target" text not null,
    "ran_at" timestamp with time zone not null,
    "removed" bigint not null,
    "outcome" text not null,
    "detail" text not null default '',
    primary key ("id")
);

create index if not exists scheduler_retention_runs_policy_idx
on "ores_scheduler_retention_runs_tbl" ("policy_code", "ran_at" desc);

create or replace function ores_scheduler_apply_retention_policies_fn()
returns table (
    "policy_code" text,
    "action" text,
    "target" text,
    "removed" bigint,
    "outcome" text
)
language plpgsql
as $$
declare
    policy record;
    cutoff timestamp with time zone;
    chunks_before bigint;
    chunks_after bigint;
    dropped bigint;
    target_name text;
    failure text;
begin
    for policy in
        -- Only the current version of a policy is applied. A policy is
        -- versioned, so turning one off closes its row and writes a new version
        -- that is disabled; the closed row still says enabled, and reading it
        -- would keep applying a policy a person has just turned off.
        --
        -- A policy that has no ledger row has never run, so it is due. The
        -- ledger is read rather than the policy row updated, because an update
        -- to a versioned row opens a new version: a sweep writing its own
        -- bookkeeping there would read, to a person, as a change they made.
        select * from "ores_scheduler_retention_policies_tbl" p
        where p."enabled"
          and p."tenant_id" = ores_utility_system_tenant_id_fn()
          and p."valid_to" = ores_utility_infinity_timestamp_fn()
          and not exists (
              select 1 from "ores_scheduler_retention_runs_tbl" r
              where r."policy_code" = p."code"
                and r."ran_at" > now() - make_interval(hours => p."cadence_hours")
          )
        order by p."code"
    loop
        cutoff := now() - make_interval(days => policy."retention_days");
        dropped := 0;
        failure := '';

        if policy."action" = 'drop_chunks' and policy."target_kind" = 'chunked_table' then
            begin
                -- Resolve the target to the name the catalogue states, so a
                -- policy naming its table without a schema still counts what it
                -- drops. A target that is no hypertable is reported rather than
                -- dropped and counted as nothing: a policy that matches nothing
                -- looks the same as a store with nothing to drop.
                select format('%I.%I', h.hypertable_schema, h.hypertable_name)
                into target_name
                from timescaledb_information.hypertables h
                where format('%I.%I', h.hypertable_schema, h.hypertable_name) = policy."target"
                   or h.hypertable_name = policy."target"
                limit 1;

                if target_name is null then
                    outcome := 'skipped';
                    failure := 'no hypertable matches ' || policy."target";
                else
                    select count(*) into chunks_before
                    from timescaledb_information.chunks c
                    where format('%I.%I', c.hypertable_schema, c.hypertable_name) = target_name
                      and c.range_end < cutoff;

                    perform public.drop_chunks(target_name, older_than => cutoff);

                    select count(*) into chunks_after
                    from timescaledb_information.chunks c
                    where format('%I.%I', c.hypertable_schema, c.hypertable_name) = target_name
                      and c.range_end < cutoff;

                    dropped := chunks_before - chunks_after;
                    outcome := 'applied';
                end if;
            exception when others then
                failure := sqlerrm;
                outcome := 'failed';
            end;
        else
            outcome := 'skipped';
            failure := 'the sweep applies only a chunk drop today';
        end if;

        policy_code := policy."code";
        action := policy."action";
        target := policy."target";
        removed := dropped;

        insert into "ores_scheduler_retention_runs_tbl"
            ("id", "policy_code", "action", "target", "ran_at", "removed", "outcome", "detail")
        values
            (gen_random_uuid(), policy."code", policy."action", policy."target", now(),
             dropped, outcome, failure);

        return next;
    end loop;
end;
$$;
