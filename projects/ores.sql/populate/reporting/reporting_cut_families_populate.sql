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
 * Cut Families Population Script
 *
 * Seeds the families a cut kind belongs to, for the system tenant. A family
 * states the body rule and the source of the instant its kinds share, so the
 * kinds below name one rather than restating it. This script is idempotent.
 */

\echo '--- Cut Families ---'

insert into ores_reporting_cut_families_tbl (
    code, tenant_id, version,
    name, description, display_order,
    modified_by, change_reason_code, change_commentary
) values
    ('end_of_day',
     ores_utility_system_tenant_id_fn(), 0,
     'End of day',
     'The body fixed at the rates centre''s close, read by the centre''s own approved rates. Holds the ordinary close, the month end and the year end, which fix the same four inputs at the same instant and differ in what the occasion covers and how long the cut is kept.',
     10,
     current_user, 'system.initial_load', 'Seed cut family: end_of_day'),
    ('start_of_day',
     ores_utility_system_tenant_id_fn(), 0,
     'Start of day',
     'The body of the previous end of day cut carried forward, fixed at the rates centre''s open. The values are approved rather than computed at the opening, which is why the kind needs no authorisation of its own.',
     20,
     current_user, 'system.initial_load', 'Seed cut family: start_of_day'),
    ('verification',
     ores_utility_system_tenant_id_fn(), 0,
     'Verification',
     'The body of the end of day cut with selected market data values replaced by data the firm did not produce, fixed at the close of the cut it is built on. What the family adds is a condition on provenance.',
     30,
     current_user, 'system.initial_load', 'Seed cut family: verification'),
    ('intraday',
     ores_utility_system_tenant_id_fn(), 0,
     'Intraday',
     'The body the run reads at an instant before the official authorisation, with the values current at that instant. Its instant answers to no schedule, so a report reaches a cut of this family by pinning one.',
     40,
     current_user, 'system.initial_load', 'Seed cut family: intraday'),
    ('consumer',
     ores_utility_system_tenant_id_fn(), 0,
     'Consumer',
     'The body fixed at an instant the consumer supplies. Nothing outside the run fixes the instant: no centre publishes it, no person chooses it and no process names it, so the run mints the cut and consumes it in one act.',
     50,
     current_user, 'system.initial_load', 'Seed cut family: consumer')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Cut Families' as entity, count(*) as count
from ores_reporting_cut_families_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
