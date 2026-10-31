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
 * Cut Kinds Population Script
 *
 * Seeds the seven kinds of cut the design fixes, for the system tenant. Each
 * row states the family, the inbox kind that authorises a cut of the occasion
 * or null where none does, whether a result priced from it is indicative, the
 * rule a report resolves it by, and how long a cut of it is kept.
 *
 * The end of day types point at the Finance kind and the verification at the
 * Market Risk one, because the deciding function differs. The start of day,
 * the intraday and the consumer kinds point at nothing: an occasion that needs
 * no signature says so rather than looking unsigned.
 *
 * The retention periods are the design's. A year end is the longest because it
 * is the body a yearly report states and the body an auditor asks for years
 * later; a live cut is the shortest because it is a view, and its result is
 * reproduced by naming it. This script is idempotent.
 */

\echo '--- Cut Kinds ---'

insert into ores_reporting_cut_kinds_tbl (
    code, tenant_id, version,
    name, description, family_code, approval_kind_code, output_is_indicative,
    resolution_rule_code, retention_months, retention_destination_code,
    legal_hold_default, display_order,
    modified_by, change_reason_code, change_commentary
) values
    ('EOD',
     ores_utility_system_tenant_id_fn(), 0,
     'End of day cut',
     'The day''s official revaluation of a book, fixed at the rates centre''s close with the approved rates applied. The movement from the previous cut to this one, and the reports built on both, read it.',
     'end_of_day', 'reporting.eod_cut_authorisation', false,
     'same_day_close', 25, 'cold',
     false, 10,
     current_user, 'system.initial_load', 'Seed cut kind: EOD'),
    ('EOM',
     ores_utility_system_tenant_id_fn(), 0,
     'Month end cut',
     'The body of the end of day cut fixed on the last business day of a month, kept longer because a monthly report states it.',
     'end_of_day', 'reporting.eod_cut_authorisation', false,
     'last_business_day_of_month_close', 84, 'cold',
     false, 20,
     current_user, 'system.initial_load', 'Seed cut kind: EOM'),
    ('EOY',
     ores_utility_system_tenant_id_fn(), 0,
     'Year end cut',
     'The body of the end of day cut fixed on the last business day of a year, kept longest because a yearly report states it and an auditor asks for it years later.',
     'end_of_day', 'reporting.eod_cut_authorisation', false,
     'last_business_day_of_year_close', 120, 'cold',
     false, 30,
     current_user, 'system.initial_load', 'Seed cut kind: EOY'),
    ('SOD',
     ores_utility_system_tenant_id_fn(), 0,
     'Start of day cut',
     'The body of the previous end of day cut carried forward, fixed at the rates centre''s open, so the day''s movement has a recorded starting point. Its values were approved when the previous day''s cut was fixed, which is why it needs no authorisation and is not indicative.',
     'start_of_day', null, false,
     'same_day_open', 25, 'cold',
     false, 40,
     current_user, 'system.initial_load', 'Seed cut kind: SOD'),
    ('IPV',
     ores_utility_system_tenant_id_fn(), 0,
     'Independent price verification cut',
     'The body of the end of day cut with selected market data values replaced by data the firm did not produce, so the firm''s own marks can be tested against an outside source.',
     'verification', 'reporting.ipv_cut_authorisation', false,
     'cut_it_is_built_on', 84, 'cold',
     false, 50,
     current_user, 'system.initial_load', 'Seed cut kind: IPV'),
    ('FLASH',
     ores_utility_system_tenant_id_fn(), 0,
     'Flash cut',
     'An indicative view of value while the day is still moving, fixed at an instant a person names. Its instant answers to no schedule, so a report reaches it by pinning one.',
     'intraday', null, true,
     'the_cut_named', 3, 'cold',
     false, 60,
     current_user, 'system.initial_load', 'Seed cut kind: FLASH'),
    ('LIVE',
     ores_utility_system_tenant_id_fn(), 0,
     'Live cut',
     'A view a report mints at the instant its run starts: the store''s values then, and the reference data and settings in force then. A rerun resolves to a new instant and a different body, so a LIVE result is reproduced by naming its cut.',
     'consumer', null, true,
     'run_instant', 1, 'cold',
     false, 70,
     current_user, 'system.initial_load', 'Seed cut kind: LIVE')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Cut Kinds' as entity, count(*) as count
from ores_reporting_cut_kinds_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
