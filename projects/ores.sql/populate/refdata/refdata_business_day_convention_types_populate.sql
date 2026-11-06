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
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE so that
 * name/description text is kept up-to-date in a single atomic statement.
 */

\echo '--- Business Day Convention Types ---'

-- ORE accepts several spellings of each convention and the trade states the
-- one it was written with, so each spelling is a code of its own, as the day
-- count fractions do with A360 and Actual/360. A roundtrip then writes back
-- the spelling the document used.

insert into ores_refdata_business_day_convention_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'Following', 0, 'Following',
     'Adjust to the next good business day',
     1, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'ModifiedFollowing', 0, 'Modified Following',
     'Next good business day unless it crosses a month end, in which case the previous good day',
     2, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'Preceding', 0, 'Preceding',
     'Adjust to the previous good business day',
     3, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'ModifiedPreceding', 0, 'Modified Preceding',
     'Previous good business day unless it crosses a month start, in which case the next good day',
     4, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'Unadjusted', 0, 'Unadjusted',
     'No business day adjustment applied',
     5, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'HalfMonthModifiedFollowing', 0, 'Half-Month Modified Following',
     'Modified Following, additionally rolled back within the same half-month period',
     6, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'NEAREST', 0, 'Nearest',
     'Adjust to the nearest good business day, forward or backward',
     7, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'F', 0, 'Following',
     'Adjust to the next good business day. ORE''s spelling F of Following.',
     8, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'FOLLOWING', 0, 'Following',
     'Adjust to the next good business day. ORE''s spelling FOLLOWING of Following.',
     9, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'MF', 0, 'Modified Following',
     'Next good business day unless it crosses a month end, in which case the previous good day. ORE''s spelling MF of ModifiedFollowing.',
     10, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'Modified Following', 0, 'Modified Following',
     'Next good business day unless it crosses a month end, in which case the previous good day. ORE''s spelling Modified Following of ModifiedFollowing.',
     11, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'MODIFIEDF', 0, 'Modified Following',
     'Next good business day unless it crosses a month end, in which case the previous good day. ORE''s spelling MODIFIEDF of ModifiedFollowing.',
     12, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'MODFOLLOWING', 0, 'Modified Following',
     'Next good business day unless it crosses a month end, in which case the previous good day. ORE''s spelling MODFOLLOWING of ModifiedFollowing.',
     13, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'P', 0, 'Preceding',
     'Adjust to the previous good business day. ORE''s spelling P of Preceding.',
     14, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'PRECEDING', 0, 'Preceding',
     'Adjust to the previous good business day. ORE''s spelling PRECEDING of Preceding.',
     15, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'MP', 0, 'Modified Preceding',
     'Previous good business day unless it crosses a month start, in which case the next good day. ORE''s spelling MP of ModifiedPreceding.',
     16, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'Modified Preceding', 0, 'Modified Preceding',
     'Previous good business day unless it crosses a month start, in which case the next good day. ORE''s spelling Modified Preceding of ModifiedPreceding.',
     17, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'MODIFIEDP', 0, 'Modified Preceding',
     'Previous good business day unless it crosses a month start, in which case the next good day. ORE''s spelling MODIFIEDP of ModifiedPreceding.',
     18, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'U', 0, 'Unadjusted',
     'No business day adjustment applied. ORE''s spelling U of Unadjusted.',
     19, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'INDIFF', 0, 'Unadjusted',
     'No business day adjustment applied. ORE''s spelling INDIFF of Unadjusted.',
     20, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'NONE', 0, 'Unadjusted',
     'No business day adjustment applied. ORE''s spelling NONE of Unadjusted.',
     21, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'NotApplicable', 0, 'Unadjusted',
     'No business day adjustment applied. ORE''s spelling NotApplicable of Unadjusted.',
     22, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'HMMF', 0, 'Half-Month Modified Following',
     'Modified Following, additionally rolled back within the same half-month period. ORE''s spelling HMMF of HalfMonthModifiedFollowing.',
     23, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'HalfMonthMF', 0, 'Half-Month Modified Following',
     'Modified Following, additionally rolled back within the same half-month period. ORE''s spelling HalfMonthMF of HalfMonthModifiedFollowing.',
     24, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'Half Month Modified Following', 0, 'Half-Month Modified Following',
     'Modified Following, additionally rolled back within the same half-month period. ORE''s spelling Half Month Modified Following of HalfMonthModifiedFollowing.',
     25, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types'),
    (ores_utility_system_tenant_id_fn(), 'HALFMONTHMF', 0, 'Half-Month Modified Following',
     'Modified Following, additionally rolled back within the same half-month period. ORE''s spelling HALFMONTHMF of HalfMonthModifiedFollowing.',
     26, current_user, current_user, 'system.initial_load', 'Initial population of business day convention types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    name = excluded.name,
    description = excluded.description,
    display_order = excluded.display_order,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_business_day_convention_types' as entity, count(*) as count
from ores_refdata_business_day_convention_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
