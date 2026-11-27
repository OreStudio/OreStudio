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
 * What bounds each store, whether it is bounded now, and how large it is.
 *
 * The screen reads this view rather than joining three tables itself, and that
 * is the point: whether a store is bounded is a fact about the installation, not
 * an inference a screen makes. A store is bounded now only where the mechanism
 * its bound names exists and runs: the sweep performs a chunk drop and a row
 * delete, so those bound their store today. An object prefix waits on the
 * storage lifecycle and a job's folder on the node's own release, and until
 * either exists the store is not bounded, however clear its policy reads. A
 * sweep whose last attempt failed leaves its store unbounded too: the mechanism
 * exists and did nothing, and nothing bounds a store until something removes
 * from it. A store no sweep has reached yet is bounded by the policy that will
 * be applied.
 *
 * The size and the moment it was measured come from the latest sample, and a
 * store with no sample reports nothing rather than zero: the sampler measures the
 * stores the database can see, and an object prefix or a folder on a host is
 * measured by the component that owns it.
 *
 * The row states the tenant that holds the store, because a reader that acts on
 * every store rather than on one — the pass that raises the notices — must write
 * in the tenant that owns it, and a view that left the tenant out would make the
 * caller join the catalogue to find out what it is already looking at.
 *
 * The bound's own record travels with the answer — its period, its cadence, its
 * version and its words — because the screen that changes one has to send the row
 * back whole, and a second read of the policy would be a second answer to the
 * same question. Where no policy bounds the store these are the catalogue's
 * defaults, which is what makes the form that creates one open on the bound the
 * installation ships with.
 */

create or replace view ores_scheduler_store_bounds_vw as
select
    s."tenant_id",
    s."code",
    s."display_name",
    s."description",
    s."owner_component",
    s."target_kind",
    s."target",
    s."time_column",
    s."measure_kind",
    s."bound_kind",
    coalesce(p."action", s."default_action") as "action",
    coalesce(p."retention_days", s."default_retention_days") as "retention_days",
    coalesce(p."cadence_hours", s."default_cadence_hours") as "cadence_hours",
    p."code" as "policy_code",
    p."enabled" as "policy_enabled",
    coalesce(p."version", 0) as "policy_version",
    coalesce(p."description", s."default_reason") as "policy_description",
    case
        when s."bound_kind" = 'keying' then 'content_keying'
        when s."bound_kind" = 'none' then 'none'
        when p."code" is null then 'none'
        when not p."enabled" then 'policy_off'
        when p."action" in ('drop_chunks', 'delete_rows') then 'sweep'
        when p."action" = 'expire_prefix' then 'storage_lifecycle'
        when p."action" = 'release_directory' then 'node_release'
        else 'none'
    end as "mechanism",
    case
        when s."bound_kind" = 'keying' then true
        when s."bound_kind" = 'none' or p."code" is null then false
        when not p."enabled" then false
        when p."action" in ('drop_chunks', 'delete_rows')
            then l."outcome" is distinct from 'failed'
        else false
    end as "bounded_now",
    l."outcome" as "last_outcome",
    l."ran_at" as "last_run",
    l."removed" as "last_removed",
    l."detail" as "last_detail",
    m."size_bytes",
    m."measure_count",
    m."sampled_at"
from "ores_scheduler_retained_stores_tbl" s
left join "ores_scheduler_retention_policies_tbl" p
       on p."tenant_id" = s."tenant_id"
      and p."store_code" = s."code"
      and p."valid_to" = ores_utility_infinity_timestamp_fn()
left join lateral (
    select r."outcome", r."ran_at", r."removed", r."detail"
    from "ores_scheduler_retention_runs_tbl" r
    where r."tenant_id" = s."tenant_id"
      and r."policy_code" = p."code"
    order by r."ran_at" desc
    limit 1
) l on true
left join lateral (
    select sm."size_bytes", sm."measure_count", sm."sampled_at"
    from "ores_scheduler_retained_store_samples_tbl" sm
    where sm."tenant_id" = s."tenant_id"
      and sm."store_code" = s."code"
    order by sm."sampled_at" desc
    limit 1
) m on true
where s."valid_to" = ores_utility_infinity_timestamp_fn();
