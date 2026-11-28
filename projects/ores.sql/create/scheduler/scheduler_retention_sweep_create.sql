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
-- The sweep performs the actions SQL can perform: a row delete, a chunk drop.
-- An object prefix is performed by the object expiry action, which reaches the
-- storage service, and a job's folder is released by the node that holds it.
-- Each of those records its own run, so this function leaves their policies
-- alone rather than writing a second account of the same store.
--
-- A policy states a cadence, and the ledger states when it last ran. A policy is
-- due once its cadence has passed since the last row the ledger holds for it, so
-- an installation states a different cadence per store and one sweep honours
-- them all. The sweep writes nothing for a policy that is not due: a row per
-- policy per tick would grow the ledger faster than the stores it bounds shrink.

create table if not exists "ores_scheduler_retention_runs_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "policy_code" text not null,
    "action" text not null,
    "target" text not null,
    "ran_at" timestamp with time zone not null,
    "removed" bigint not null,
    "outcome" text not null,
    "detail" text not null default '',
    primary key ("tenant_id", "id")
);

create index if not exists scheduler_retention_runs_policy_idx
on "ores_scheduler_retention_runs_tbl" ("tenant_id", "policy_code", "ran_at" desc);

alter table ores_scheduler_retention_runs_tbl enable row level security;

drop policy if exists retention_runs_tbl_tenant_isolation_policy
    on ores_scheduler_retention_runs_tbl;

create policy retention_runs_tbl_tenant_isolation_policy
on ores_scheduler_retention_runs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- The sweep is the platform's maintenance path, and it deletes from tables other
-- components own: the report run output, the quotes and the delivery counters.
-- The alternative is to grant the scheduler service standing delete rights on
-- every store it might bound, which is a wider right than the job needs and one
-- no grant can keep in step with a catalogue that is data. So the function is
-- the definer's and the targets come from the catalogue, which only the platform
-- writes; a store whose column does not exist is refused rather than executed.
create or replace function ores_scheduler_apply_retention_policies_fn()
returns table (
    "policy_code" text,
    "action" text,
    "target" text,
    "removed" bigint,
    "outcome" text
)
language plpgsql
security definer
as $$
declare
    policy record;
    cutoff timestamp with time zone;
    chunks_before bigint;
    chunks_after bigint;
    dropped bigint;
    deleted bigint;
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
        --
        -- The store comes from the catalogue, not from the policy: the target,
        -- the shape and the column a delete compares are facts the catalogue
        -- states once, so a policy that has drifted from its store is visible
        -- here rather than acted on.
        select p.*, s.code as store_found, s.target as store_target,
               s.target_kind as store_target_kind,
               s.time_column as store_time_column
        from "ores_scheduler_retention_policies_tbl" p
        left join "ores_scheduler_retained_stores_tbl" s
               on s."tenant_id" = p."tenant_id"
              and s."code" = p."store_code"
              and s."valid_to" = ores_utility_infinity_timestamp_fn()
        where p."enabled"
          and p."tenant_id" = ores_utility_system_tenant_id_fn()
          and p."valid_to" = ores_utility_infinity_timestamp_fn()
          -- The sweep performs the actions SQL can perform. An object prefix is
          -- the object expiry action's, and a job's folder is the node's own
          -- release; each records its own run, so a row here would be a second
          -- and contradictory account of the same policy.
          and (p."action" in ('drop_chunks', 'delete_rows')
               or s."target_kind" is null)
          and not exists (
              select 1 from "ores_scheduler_retention_runs_tbl" r
              where r."tenant_id" = p."tenant_id"
                and r."policy_code" = p."code"
                and r."ran_at" > now() - make_interval(hours => p."cadence_hours")
          )
        order by p."code"
    loop
        cutoff := now() - make_interval(days => policy."retention_days");
        dropped := 0;
        deleted := 0;
        failure := '';

        policy_code := policy."code";
        action := policy."action";
        target := policy."target";
        outcome := 'skipped';

        if policy.store_found is null then
            failure := 'the catalogue names no store ' || policy."store_code";
            target := policy."target";
        elsif policy."action" = 'drop_chunks' and policy.store_target_kind = 'chunked_table' then
            target := policy.store_target;
            begin
                -- Resolve the target to the name the catalogue states, so a
                -- policy naming its table without a schema still counts what it
                -- drops. A target that is no hypertable is reported rather than
                -- dropped and counted as nothing: a policy that matches nothing
                -- looks the same as a store with nothing to drop.
                select format('%I.%I', h.hypertable_schema, h.hypertable_name)
                into target_name
                from timescaledb_information.hypertables h
                where format('%I.%I', h.hypertable_schema, h.hypertable_name) = policy.store_target
                   or h.hypertable_name = policy.store_target
                limit 1;

                if target_name is null then
                    outcome := 'skipped';
                    failure := 'no hypertable matches ' || policy.store_target;
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
        elsif policy."action" = 'delete_rows' and policy.store_target_kind = 'table' then
            target := policy.store_target;
            begin
                -- The column a delete compares is the catalogue's, and it is
                -- checked against the table before the delete runs: a store
                -- whose column no longer exists is reported, not deleted from
                -- by a guess.
                if policy.store_time_column is null then
                    outcome := 'skipped';
                    failure := 'the store names no column a delete compares';
                elsif not exists (
                    select 1
                    from pg_catalog.pg_attribute a
                    where a.attrelid = to_regclass(policy.store_target)
                      and a.attname = policy.store_time_column
                      and a.attnum > 0
                ) then
                    outcome := 'skipped';
                    failure := 'the store names no column ' || policy.store_time_column;
                else
                    execute format('delete from %s where %I < $1',
                                   policy.store_target, policy.store_time_column)
                    using cutoff;

                    get diagnostics deleted = row_count;
                    outcome := 'applied';
                end if;
            exception when others then
                failure := sqlerrm;
                outcome := 'failed';
            end;
        else
            outcome := 'skipped';
            failure := 'the sweep performs a chunk drop and a row delete, and this store asks for ' ||
                       policy."action";
        end if;

        removed := dropped + deleted;

        -- The ledger carries the tenant of the policy it ran, not the tenant the
        -- sweep ran in: a sweep that applies a tenant's own policies writes that
        -- tenant's runs, and one row per run is what the screen reads back.
        insert into "ores_scheduler_retention_runs_tbl"
            ("id", "tenant_id", "policy_code", "action", "target", "ran_at",
             "removed", "outcome", "detail")
        values
            (gen_random_uuid(), policy."tenant_id", policy."code", policy."action",
             target, now(), removed, outcome, failure);

        return next;
    end loop;
end;
$$;
