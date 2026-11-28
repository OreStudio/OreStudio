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
 * Template: sql_schema_domain_entity_view_create.mustache
 * To modify, update the template and regenerate.
 *
 * Retention Run View
 *
 * The read model a person reads to see what housekeeping did: one row for each
 * policy the sweep considered, carrying the policy's period and whether it is on,
 * the store it bounds, what the run removed and its outcome.
 *
 * The sweep writes the rows and this view reads them. It is a view because the
 * ledger answers "what has housekeeping done" and a screen asks it beside the
 * policies themselves: the period a policy states now is the policy table's, not
 * the ledger's, and joining the two is the view's whole purpose. A policy
 * disabled after a run still shows the run, with the state it has now.
 */

create or replace view "ores_scheduler_retention_runs_vw"
with (security_invoker = true) as
select r.id,
       r.policy_code,
       r.target,
       r.action,
       coalesce(p.retention_days, 0) as retention_days,
       coalesce(p.enabled, false) as enabled,
       r.ran_at,
       r.removed,
       r.outcome,
       r.detail
  from ores_scheduler_retention_runs_tbl r
  left join ores_scheduler_retention_policies_tbl p
         on p.tenant_id = r.tenant_id
        and p.code = r.policy_code
        and p.valid_to = ores_utility_infinity_timestamp_fn();
