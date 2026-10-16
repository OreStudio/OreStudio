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
 * Curve series validation
 *
 * A trade names the curve it is discounted on as the oresmd URI of the series
 * that curve is, so the name is ours and the entity's own market decides which
 * series that is. The series catalogue is the only thing that knows the URI,
 * so a trade that names one the catalogue does not hold is refused here rather
 * than much later, in an engine that can only report a curve it could not
 * find.
 *
 * A series with no curve coordinate names a pillar rather than a curve, and a
 * discount engine cannot resolve one: a curve's URI carries a curve_id, and a
 * pillar's carries a term instead. The identity projection decodes that
 * coordinate, so a pillar is refused by the curve_id it has not got.
 *
 * The series is scoped to the party that owns it as well as to the tenant.
 * One curve id is defined once per entity -- GBP1D exists in three of them --
 * and each entity's series is its own row, so a tenant-scoped match would
 * silently accept another party's curve as this trade's. A session that names
 * no party, as a rebuild's does, leaves the party unconstrained.
 */

create or replace function ores_marketdata_validate_curve_series_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
declare
    v_party_id uuid;
begin
    if p_value is null or p_value = '' then
        return p_value;
    end if;

    v_party_id := ores_iam_current_party_id_fn();

    if not exists (
        select 1
        from ores_marketdata_market_series_tbl s
        join ores_marketdata_market_series_identity_tbl i
          on i.series_id = s.id
         and i.tenant_id = s.tenant_id
        where s.tenant_id = p_tenant_id
          and s.oresmd_uri = p_value
          and (v_party_id is null or s.party_id = v_party_id)
          and i.instrument_type in ('zero', 'discount')
          and coalesce(i.curve_id, '') <> ''
    ) then
        raise exception 'Invalid reference_curve_id: %. No curve series of this tenant and party names that curve. A curve series reads oresmd://ir/<currency>?type=series&instrument=zero&quote=rate&curve_id=<curve>.', p_value
            using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql;
