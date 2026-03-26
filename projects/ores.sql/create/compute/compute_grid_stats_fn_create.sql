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

-- =============================================================================
-- ores_compute_grid_stats_fn
-- =============================================================================
-- Returns a single-row summary of compute grid state for a given tenant.
-- Counts only current (non-historical) rows by filtering valid_to = infinity.
-- =============================================================================

create or replace function ores_compute_grid_stats_fn(p_tenant_id uuid)
returns table (
    total_hosts         bigint,
    online_hosts        bigint,
    idle_hosts          bigint,
    results_inactive    bigint,
    results_unsent      bigint,
    results_in_progress bigint,
    results_done        bigint,
    outcomes_success    bigint,
    outcomes_client_error bigint,
    outcomes_no_reply   bigint,
    total_workunits     bigint,
    total_batches       bigint,
    active_batches      bigint
)
language sql
stable
as $$
    with
      host_stats as (
        select
          count(*)                                                              as total_hosts,
          count(*) filter (where last_rpc_time > now() - interval '5 minutes') as online_hosts
        from ores_compute_hosts_tbl
        where tenant_id = p_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
      ),
      result_stats as (
        select
          count(*) filter (where server_state = 1)  as results_inactive,
          count(*) filter (where server_state = 2)  as results_unsent,
          count(*) filter (where server_state = 4)  as results_in_progress,
          count(*) filter (where server_state = 5)  as results_done,
          count(distinct host_id) filter (where server_state = 4) as busy_hosts,
          count(*) filter (where server_state = 5
            and received_at > now() - interval '24 hours'
            and outcome = 1) as outcomes_success,
          count(*) filter (where server_state = 5
            and received_at > now() - interval '24 hours'
            and outcome = 3) as outcomes_client_error,
          count(*) filter (where server_state = 5
            and received_at > now() - interval '24 hours'
            and outcome = 4) as outcomes_no_reply
        from ores_compute_results_tbl
        where tenant_id = p_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
      ),
      workunit_count as (
        select count(*) as total_workunits
        from ores_compute_workunits_tbl
        where tenant_id = p_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
      ),
      batch_stats as (
        select count(*) as total_batches
        from ores_compute_batches_tbl
        where tenant_id = p_tenant_id
          and valid_to = ores_utility_infinity_timestamp_fn()
      ),
      active_batches as (
        select count(distinct wu.batch_id) as active_batches
        from ores_compute_results_tbl r
        join ores_compute_workunits_tbl wu
          on r.workunit_id = wu.id
         and wu.tenant_id = p_tenant_id
         and wu.valid_to = ores_utility_infinity_timestamp_fn()
        where r.tenant_id = p_tenant_id
          and r.valid_to = ores_utility_infinity_timestamp_fn()
          and r.server_state = 4
      )
    select
      h.total_hosts,
      h.online_hosts,
      greatest(h.online_hosts - r.busy_hosts, 0) as idle_hosts,
      r.results_inactive,
      r.results_unsent,
      r.results_in_progress,
      r.results_done,
      r.outcomes_success,
      r.outcomes_client_error,
      r.outcomes_no_reply,
      w.total_workunits,
      b.total_batches,
      a.active_batches
    from host_stats h, result_stats r, workunit_count w,
         batch_stats b, active_batches a
$$;
