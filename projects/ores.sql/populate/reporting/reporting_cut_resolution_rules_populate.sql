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
 * Cut Resolution Rules Population Script
 *
 * Seeds the rules by which a report resolves a cut selection to one concrete
 * cut, for the system tenant. A kind names one of these and the run executes
 * it against the date the report covers, the report's scope and the scope's
 * rates centre. This script is idempotent.
 */

\echo '--- Cut Resolution Rules ---'

insert into ores_reporting_cut_resolution_rules_tbl (
    code, tenant_id, version,
    name, description, display_order,
    modified_by, change_reason_code, change_commentary
) values
    ('same_day_close',
     ores_utility_system_tenant_id_fn(), 0,
     'Same day close',
     'The end of day family''s cut of the date the report covers, at the rates centre''s close: plain, or the month end or year end cut where that date is one.',
     10,
     current_user, 'system.initial_load', 'Seed cut resolution rule: same_day_close'),
    ('last_business_day_of_month_close',
     ores_utility_system_tenant_id_fn(), 0,
     'Last business day of the month, close',
     'The month end cut of the month the report covers, taken on that month''s last business day, at the close of that day.',
     20,
     current_user, 'system.initial_load', 'Seed cut resolution rule: last_business_day_of_month_close'),
    ('last_business_day_of_year_close',
     ores_utility_system_tenant_id_fn(), 0,
     'Last business day of the year, close',
     'The year end cut of the year the report covers, taken on that year''s last business day, at the close of that day.',
     30,
     current_user, 'system.initial_load', 'Seed cut resolution rule: last_business_day_of_year_close'),
    ('same_day_open',
     ores_utility_system_tenant_id_fn(), 0,
     'Same day open',
     'The start of day cut of the date the report covers, at the rates centre''s open.',
     40,
     current_user, 'system.initial_load', 'Seed cut resolution rule: same_day_open'),
    ('cut_it_is_built_on',
     ores_utility_system_tenant_id_fn(), 0,
     'The cut it is built on',
     'The verification cut of the date the report covers, whose timepoint is the close of the cut it is built on.',
     50,
     current_user, 'system.initial_load', 'Seed cut resolution rule: cut_it_is_built_on'),
    ('run_instant',
     ores_utility_system_tenant_id_fn(), 0,
     'The run instant',
     'A cut the run mints at the instant it starts. Its values are indicative and no authorisation precedes it, because the run both mints the cut and consumes it.',
     60,
     current_user, 'system.initial_load', 'Seed cut resolution rule: run_instant'),
    ('the_cut_named',
     ores_utility_system_tenant_id_fn(), 0,
     'The cut named',
     'The cut the selection names, and nothing else. A kind whose occasion no rule can resolve names this one, and a report reaches its cuts by pinning one rather than by selecting a kind.',
     70,
     current_user, 'system.initial_load', 'Seed cut resolution rule: the_cut_named')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Cut Resolution Rules' as entity, count(*) as count
from ores_reporting_cut_resolution_rules_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
