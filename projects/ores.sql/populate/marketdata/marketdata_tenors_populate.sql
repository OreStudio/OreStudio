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
 * Tenors Population Script
 *
 * Seeds the standard tenor label catalog from doc/knowledge/domain/tenor.org's
 * "Standard tenor labels" table, plus SPOT (a selectable curve-pillar tenor,
 * distinct from the tenor_anchor SPOT reference point). O/N, T/N, and S/N are
 * SPECIAL: their duration is not fixed by this table, since it genuinely
 * differs by convention (see tenor_convention_resolutions). Every other tenor,
 * including S/W and SPOT, has a convention-invariant unit/multiplier and is
 * PERIOD.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Tenors ---'

insert into ores_marketdata_tenors_tbl (
    tenant_id, code, version, display_name, description, sort_order, kind, unit, multiplier,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'O/N', 0, 'Overnight',
     'Today to next business day. Earliest possible tenor.', 10, 'SPECIAL', null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), 'T/N', 0, 'Tom/Next',
     'Next business day to spot.', 20, 'SPECIAL', null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), 'S/N', 0, 'Spot/Next',
     'Spot to spot + 1 business day.', 30, 'SPECIAL', null, null,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), 'S/W', 0, 'Spot/Week',
     'Spot to spot + 1 week; also written WK1 or 1W.', 40, 'PERIOD', 'WEEK', 1,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), 'SPOT', 0, 'Spot',
     'The spot date itself: horizon date + spot days. A selectable curve-pillar tenor label, distinct from the tenor_anchor SPOT reference point.', 45, 'PERIOD', 'DAY', 0,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '1D', 0, 'One Day',
     'Used mainly on granular option-map/bucketing screens, not on bootstrapped curves.', 50, 'PERIOD', 'DAY', 1,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '2D', 0, 'Two Days',
     'Used mainly on granular option-map/bucketing screens, not on bootstrapped curves.', 60, 'PERIOD', 'DAY', 2,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '3D', 0, 'Three Days',
     'Used mainly on granular option-map/bucketing screens, not on bootstrapped curves.', 70, 'PERIOD', 'DAY', 3,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '1W', 0, 'One Week',
     'Standard pillar everywhere.', 80, 'PERIOD', 'WEEK', 1,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '2W', 0, 'Two Weeks',
     'Appears on consensus/IPV expiry ladders.', 90, 'PERIOD', 'WEEK', 2,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '3W', 0, 'Three Weeks',
     'Appears on consensus/IPV expiry ladders.', 100, 'PERIOD', 'WEEK', 3,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '4W', 0, 'Four Weeks',
     'Appears on consensus/IPV expiry ladders.', 110, 'PERIOD', 'WEEK', 4,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '1M', 0, 'One Month',
     'Standard short-end pillar (deposits/LIBOR).', 120, 'PERIOD', 'MONTH', 1,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '2M', 0, 'Two Months',
     'Appears on consensus/vol expiry ladders.', 130, 'PERIOD', 'MONTH', 2,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '3M', 0, 'Three Months',
     'Standard pillar; FRA/futures region begins here.', 140, 'PERIOD', 'MONTH', 3,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '4M', 0, 'Four Months',
     'Occasional intermediate point; not a universal pillar.', 150, 'PERIOD', 'MONTH', 4,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '5M', 0, 'Five Months',
     'Occasional intermediate point; not a universal pillar.', 160, 'PERIOD', 'MONTH', 5,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '6M', 0, 'Six Months',
     'Standard pillar; short-end deposits may extend this far.', 170, 'PERIOD', 'MONTH', 6,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '7M', 0, 'Seven Months',
     'Rare, mostly interpolated.', 180, 'PERIOD', 'MONTH', 7,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '8M', 0, 'Eight Months',
     'Rare, mostly interpolated.', 190, 'PERIOD', 'MONTH', 8,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '9M', 0, 'Nine Months',
     'Standard pillar on consensus/vol expiry ladders.', 200, 'PERIOD', 'MONTH', 9,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '12M', 0, 'Twelve Months',
     'Equivalent to 1Y; both labels appear depending on context (money-market vs. swap convention).', 210, 'PERIOD', 'MONTH', 12,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '18M', 0, 'Eighteen Months',
     'Explicit odd pillar between 1Y and 2Y, bootstrapped off a 3M fixed IRS.', 220, 'PERIOD', 'MONTH', 18,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '2Y', 0, 'Two Years',
     'Annual pillar; bootstrapped from 3M-reset IRS.', 230, 'PERIOD', 'YEAR', 2,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '3Y', 0, 'Three Years',
     'Annual pillar; bootstrapped from 3M-reset IRS.', 240, 'PERIOD', 'YEAR', 3,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '4Y', 0, 'Four Years',
     'Annual pillar; bootstrapped from 3M-reset IRS.', 250, 'PERIOD', 'YEAR', 4,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '5Y', 0, 'Five Years',
     'Annual pillar; bootstrapped from 3M-reset IRS.', 260, 'PERIOD', 'YEAR', 5,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '6Y', 0, 'Six Years',
     'Annual pillar; commonly an unsupported gap filled by interpolation.', 270, 'PERIOD', 'YEAR', 6,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '7Y', 0, 'Seven Years',
     'Annual pillar; bootstrapped from 3M-reset IRS.', 280, 'PERIOD', 'YEAR', 7,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '8Y', 0, 'Eight Years',
     'Annual pillar; commonly an unsupported gap filled by interpolation.', 290, 'PERIOD', 'YEAR', 8,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '9Y', 0, 'Nine Years',
     'Annual pillar; commonly an unsupported gap filled by interpolation.', 300, 'PERIOD', 'YEAR', 9,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '10Y', 0, 'Ten Years',
     'Annual pillar; bootstrapped from 3M-reset IRS.', 310, 'PERIOD', 'YEAR', 10,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '12Y', 0, 'Twelve Years',
     'Appears on consensus/vol expiry ladders between 10Y and 15Y.', 320, 'PERIOD', 'YEAR', 12,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '15Y', 0, 'Fifteen Years',
     'Standard long-end IRS pillar.', 330, 'PERIOD', 'YEAR', 15,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '20Y', 0, 'Twenty Years',
     'Standard long-end IRS pillar.', 340, 'PERIOD', 'YEAR', 20,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '25Y', 0, 'Twenty-Five Years',
     'Standard long-end IRS pillar.', 350, 'PERIOD', 'YEAR', 25,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors'),
    (ores_utility_system_tenant_id_fn(), '30Y', 0, 'Thirty Years',
     'Standard long-end IRS pillar; the ceiling for interest rate curves.', 360, 'PERIOD', 'YEAR', 30,
     current_user, current_user, 'system.initial_load', 'Initial population of tenors')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    display_name = excluded.display_name,
    description = excluded.description,
    sort_order = excluded.sort_order,
    kind = excluded.kind,
    unit = excluded.unit,
    multiplier = excluded.multiplier,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'marketdata_tenors' as entity, count(*) as count
from ores_marketdata_tenors_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
