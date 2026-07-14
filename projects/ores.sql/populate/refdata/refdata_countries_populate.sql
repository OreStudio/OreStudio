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
 * Countries Population Script
 *
 * Seeds the ISO 3166-1 "ZZ" user-assigned sentinel country, representing
 * "supranational / not country-specific" -- used by calendar rows (e.g.
 * TARGET) with no single owning country. ZZ is one of ISO 3166-1's own
 * reserved user-assigned code elements (AA, QM-QZ, XA-XZ, ZZ), never
 * allocated to a real country, so this is real reference data rather
 * than fictional test data. The full ISO country list is not seeded
 * here; only this sentinel is required for calendar.country_code's
 * default value to validate.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Countries (ZZ sentinel) ---'

insert into ores_refdata_countries_tbl (
    tenant_id, alpha2_code, version, alpha3_code, numeric_code, name, official_name,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'ZZ', 0, 'ZZZ', '999',
     'Supranational / Not Country-Specific', 'Supranational / Not Country-Specific',
     current_user, current_user, 'system.initial_load',
     'ISO 3166-1 user-assigned sentinel for supranational calendars')
on conflict (tenant_id, alpha2_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    alpha3_code = excluded.alpha3_code,
    numeric_code = excluded.numeric_code,
    name = excluded.name,
    official_name = excluded.official_name,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_countries' as entity, count(*) as count
from ores_refdata_countries_tbl;
