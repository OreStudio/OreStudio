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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

-- =============================================================================
-- Which series of a party answer a list of oresmd requirements, at a date
-- =============================================================================

-- A requirement is an oresmd address, and an address is a series identity, so
-- a requirement is answered by the party's series with that address. One row
-- comes back for each answering series; two providers may each hold a series
-- with one address, so a requirement can come back twice. A requirement no
-- series answers comes back once with no series: it is a gap, and the caller
-- reports it rather than skipping it. A series that exists but holds no point at or before the date also
-- comes back with no latest point, which is a gap of another kind.
--
-- A fixing series answers with its fixings and a quote series with its
-- observations. Ticks that were never written as observations are not read.
--
-- The date is a business date, read in UTC so the session's time zone does
-- not move it: a point counts when its fixing date or its observation time is
-- at or before it. The rows are read as they stand now,
-- because a series written today may hold points about an earlier day.
create or replace function ores_marketdata_requirement_coverage_fn(
    p_tenant_id    uuid,
    p_party_id     uuid,
    p_requirements text[],
    p_as_of        timestamptz default now()
)
returns table (oresmd_uri text, series_id uuid, latest_point timestamptz) as $$
begin
    return query
    select r.uri, s.id,
        greatest(
            (select max(f.fixing_date)::timestamp at time zone 'UTC'
             from ores_marketdata_market_fixings_tbl f
             where f.tenant_id = s.tenant_id and f.series_id = s.id
               and f.fixing_date <= (p_as_of at time zone 'UTC')::date
               and f.valid_to = ores_utility_infinity_timestamp_fn()),
            (select max(o.observation_datetime)
             from ores_marketdata_market_observations_tbl o
             where o.tenant_id = s.tenant_id and o.series_id = s.id
               and o.observation_datetime <= p_as_of
               and o.valid_to = ores_utility_infinity_timestamp_fn()))
    from (select distinct unnest(p_requirements) as uri) r
    left join ores_marketdata_market_series_tbl s
      on s.tenant_id = p_tenant_id
     and s.party_id = p_party_id
     and s.oresmd_uri = r.uri
     and s.valid_to = ores_utility_infinity_timestamp_fn();
end;
$$ language plpgsql stable;
