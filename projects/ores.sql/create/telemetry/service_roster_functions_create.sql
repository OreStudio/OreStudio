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
 * Services Roster
 *
 * One row per expected instance of every expected service, ordered by service
 * name and then slot. A service expects as many slots as its replicas in
 * ores_telemetry_expected_services_tbl. The instances that ever reported are
 * ranked by their last report, newest first, and the newest fill the slots.
 * An older instance ranked past the replicas is a restart's leftover, because
 * an instance id is new at every start, so it fills no slot. A slot no
 * instance fills has null report columns.
 *
 * The caller decides running or stopped from sampled_at; this function only
 * ranks, so the running window lives in one place.
 */
create or replace function ores_telemetry_service_roster_fn()
returns table (
    service_name text,
    slot integer,
    instance_id text,
    host_id text,
    version text,
    sampled_at timestamp with time zone
)
language sql
stable
as $$
    with latest as (
        select distinct on (s.service_name, s.instance_id)
               s.service_name, s.instance_id, s.host_id, s.version, s.sampled_at
        from ores_telemetry_service_samples_tbl s
        join ores_telemetry_expected_services_tbl e
          on e.service_name = s.service_name
        order by s.service_name, s.instance_id, s.sampled_at desc
    ),
    ranked as (
        select l.*,
               row_number() over (
                   partition by l.service_name
                   order by l.sampled_at desc, l.instance_id
               ) as rank
        from latest l
    ),
    slots as (
        select e.service_name, g.slot::integer as slot
        from ores_telemetry_expected_services_tbl e
        cross join lateral generate_series(1, e.replicas) as g(slot)
    )
    select sl.service_name, sl.slot,
           r.instance_id, r.host_id, r.version, r.sampled_at
    from slots sl
    left join ranked r
      on r.service_name = sl.service_name and r.rank = sl.slot
    order by sl.service_name, sl.slot;
$$;
