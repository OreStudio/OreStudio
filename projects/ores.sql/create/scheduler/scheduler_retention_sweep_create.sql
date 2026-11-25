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
-- it considered, and ores_scheduler_apply_retention_policies_fn applies the
-- enabled policies. The sweep exists because a retention policy installed as a
-- storage engine job only exists where the engine's licensed edition includes
-- jobs; a chunk drop executed here runs on either edition, and records which
-- path ran.

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
    failure text;
begin
    for policy in
        select * from "ores_scheduler_retention_policies_tbl" where "enabled" order by "code"
    loop
        cutoff := now() - make_interval(days => policy."retention_days");
        dropped := 0;
        failure := '';

        if policy."action" = 'drop_chunks' and policy."target_kind" = 'chunked_table' then
            begin
                select count(*) into chunks_before
                from timescaledb_information.chunks c
                where format('%I.%I', c.hypertable_schema, c.hypertable_name) = policy."target"
                  and c.range_end < cutoff;

                perform public.drop_chunks(policy."target", older_than => cutoff);

                select count(*) into chunks_after
                from timescaledb_information.chunks c
                where format('%I.%I', c.hypertable_schema, c.hypertable_name) = policy."target"
                  and c.range_end < cutoff;

                dropped := chunks_before - chunks_after;
                outcome := 'applied';
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
