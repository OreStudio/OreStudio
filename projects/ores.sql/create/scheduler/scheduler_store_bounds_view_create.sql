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
 * What bounds each store, and whether it is bounded now.
 *
 * The screen reads this view rather than joining three tables itself, and that
 * is the point: whether a store is bounded is a fact about the installation, not
 * an inference a screen makes. A store is bounded now only where the mechanism
 * its bound names exists and runs: the sweep performs a chunk drop and a row
 * delete, so those bound their store today. An object prefix waits on the
 * storage lifecycle and a job's folder on the node's own release, and until
 * either exists the store is not bounded, however clear its policy reads.
 */

create or replace view ores_scheduler_store_bounds_vw as
select
    s."code",
    s."display_name",
    s."owner_component",
    s."target_kind",
    s."target",
    s."bound_kind",
    coalesce(p."action", s."default_action") as "action",
    coalesce(p."retention_days", s."default_retention_days") as "retention_days",
    p."code" as "policy_code",
    p."enabled" as "policy_enabled",
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
        when p."action" in ('drop_chunks', 'delete_rows') then true
        else false
    end as "bounded_now",
    l."outcome" as "last_outcome",
    l."ran_at" as "last_run",
    l."removed" as "last_removed",
    l."detail" as "last_detail"
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
where s."valid_to" = ores_utility_infinity_timestamp_fn();
