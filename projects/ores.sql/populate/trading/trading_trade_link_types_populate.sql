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

/**
 * Trade Link Types Population Script
 *
 * Seeds the reasons two trades are joined, the role of each end and
 * whether the link carries an economic effect. The platform defines this
 * vocabulary, so it ships with the system and an installation does not
 * choose its own values. This script is idempotent.
 *
 * Linked cash is deliberately absent: it joins a trade to a cash flow
 * rather than to another trade. See the cash modelling capture.
 */

\echo '--- Trade Link Types ---'

insert into ores_trading_trade_link_types_tbl (
    code, tenant_id, version, description, from_role, to_role,
    has_economic_effect, modified_by, change_reason_code, change_commentary
) values
    ('CloseOut', ores_utility_system_tenant_id_fn(), 0,
     'The parties end a trade early, in full or in part',
     'original', 'closing', true,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('Novation', ores_utility_system_tenant_id_fn(), 0,
     'One party transfers its position to a third party, or assigns it',
     'transferred', 'new_counterparty', true,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('Roll', ores_utility_system_tenant_id_fn(), 0,
     'A position is rolled to a new date by closing the trade and booking a replacement',
     'rolled', 'replacement', true,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('Exercise', ores_utility_system_tenant_id_fn(), 0,
     'The holder exercises an option, in full or in part, into a new trade',
     'exercised', 'resulting', true,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('MisbookingCorrection', ores_utility_system_tenant_id_fn(), 0,
     'A trade was booked in error and is cancelled and rebooked',
     'correction', 'erroneous_original', true,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('Fixing', ores_utility_system_tenant_id_fn(), 0,
     'The rate of a non-deliverable forward fixes, so a fixing trade is booked',
     'fixed', 'fixing', true,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('BookMove', ores_utility_system_tenant_id_fn(), 0,
     'A trade closes out and is rebooked into a book with a different legal entity, branch, regulatory book or accounting classification',
     'backed_out', 'destination', true,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('Wash', ores_utility_system_tenant_id_fn(), 0,
     'Risk is routed from the client-facing trade to the book that owns it',
     'client_facing', 'risk_bearing', false,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('WhatIfCopy', ores_utility_system_tenant_id_fn(), 0,
     'A user copies a trade into a sandbox to experiment on it',
     'original', 'hypothetical', false,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('DealtFrom', ores_utility_system_tenant_id_fn(), 0,
     'A hypothetical trade is dealt, so a new actual trade is booked against it',
     'hypothetical', 'actual', false,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('Hedge', ores_utility_system_tenant_id_fn(), 0,
     'A desk records the risk relationship between a hedged trade and its hedge',
     'hedged', 'hedging', false,
     current_user, 'system.initial_load', 'Seed trade link types'),

    ('UserDefined', ores_utility_system_tenant_id_fn(), 0,
     'A user records an association between two trades that no other type names',
     'either', 'either', false,
     current_user, 'system.initial_load', 'Seed trade link types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Trade Link Types' as entity, count(*) as count
from ores_trading_trade_link_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
