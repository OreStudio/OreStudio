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
-- An object prefix is bounded by a policy like a table is, but the sweep cannot
-- apply it: the sweep is SQL, and SQL cannot reach an object store. The two
-- sides are therefore split, and this file is the SQL side.
--
-- Which object policies are due is decided here, beside the rule the table sweep
-- uses: enabled, held by the installation, and its cadence passed since the
-- ledger last recorded it. A policy is due in one place only, so the two sweeps
-- cannot disagree about when a store is next treated.
--
-- What a run did is recorded here too, so the ledger's columns are written by
-- one function rather than by every caller that happens to have a row.

create or replace function ores_scheduler_due_object_policies_fn()
returns table (
    "policy_code" text,
    "target" text,
    "retention_days" integer
)
language sql
security definer
as $$
    select p."code", s."target", p."retention_days"
    from "ores_scheduler_retention_policies_tbl" p
    join "ores_scheduler_retained_stores_tbl" s
      on s."tenant_id" = p."tenant_id"
     and s."code" = p."store_code"
     and s."valid_to" = ores_utility_infinity_timestamp_fn()
    where p."enabled"
      and p."tenant_id" = ores_utility_system_tenant_id_fn()
      and p."valid_to" = ores_utility_infinity_timestamp_fn()
      and s."target_kind" = 'object_prefix'
      and not exists (
          select 1
          from "ores_scheduler_retention_runs_tbl" r
          where r."tenant_id" = p."tenant_id"
            and r."policy_code" = p."code"
            and r."ran_at" > now() - make_interval(hours => p."cadence_hours")
      )
    order by p."code";
$$;

create or replace function ores_scheduler_record_object_run_fn(
    p_policy_code text,
    p_target text,
    p_removed bigint,
    p_outcome text,
    p_detail text
)
returns void
language sql
security definer
as $$
    insert into "ores_scheduler_retention_runs_tbl" (
        "id", "tenant_id", "policy_code", "action", "target",
        "ran_at", "removed", "outcome", "detail")
    values (
        gen_random_uuid(),
        ores_utility_system_tenant_id_fn(),
        p_policy_code,
        'expire_prefix',
        p_target,
        now(),
        p_removed,
        p_outcome,
        coalesce(p_detail, ''));
$$;

comment on function ores_scheduler_due_object_policies_fn() is
    'The enabled object-prefix policies whose cadence has passed, in policy order.';
comment on function ores_scheduler_record_object_run_fn(text, text, bigint, text, text) is
    'Records what the object expiry did with one policy, in the run ledger.';
