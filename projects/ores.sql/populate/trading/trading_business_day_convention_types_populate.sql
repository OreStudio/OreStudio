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
 * Business Day Convention Types Population Script
 *
 * Seeds the database with ORE business day adjustment convention codes.
 * Values sourced from ORE ore_types.xsd BusinessDayConvention enumeration.
 * This script is idempotent.
 */

\echo '--- Business Day Convention Types ---'

insert into ores_trading_business_day_convention_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    ('Following',         ores_iam_system_tenant_id_fn(), 0, 'Following: adjust to next good business day',
     'ores_trading_service', 'system.initial_load', 'Seed business day convention types'),
    ('ModifiedFollowing', ores_iam_system_tenant_id_fn(), 0, 'Modified Following: next good day unless it crosses month end',
     'ores_trading_service', 'system.initial_load', 'Seed business day convention types'),
    ('Preceding',         ores_iam_system_tenant_id_fn(), 0, 'Preceding: adjust to previous good business day',
     'ores_trading_service', 'system.initial_load', 'Seed business day convention types'),
    ('ModifiedPreceding', ores_iam_system_tenant_id_fn(), 0, 'Modified Preceding: previous good day unless it crosses month start',
     'ores_trading_service', 'system.initial_load', 'Seed business day convention types'),
    ('Unadjusted',        ores_iam_system_tenant_id_fn(), 0, 'Unadjusted: no business day adjustment applied',
     'ores_trading_service', 'system.initial_load', 'Seed business day convention types'),
    ('HalfMonthModifiedFollowing', ores_iam_system_tenant_id_fn(), 0, 'Half-Month Modified Following',
     'ores_trading_service', 'system.initial_load', 'Seed business day convention types'),
    ('Nearest',           ores_iam_system_tenant_id_fn(), 0, 'Nearest: adjust to nearest good business day',
     'ores_trading_service', 'system.initial_load', 'Seed business day convention types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Business Day Convention Types' as entity, count(*) as count
from ores_trading_business_day_convention_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
