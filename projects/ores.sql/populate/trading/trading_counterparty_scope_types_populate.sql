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
 * Counterparty Scope Types Population Script
 *
 * Seeds the closed set of counterparty scopes: who the firm faces on a
 * trade. The codes match the C++ enum domain::counterparty_scope.
 *
 * The table is temporal, so a re-run supersedes each live row with a new
 * version and leaves the live set unchanged. This script is idempotent.
 */

\echo '--- Counterparty Scope Types ---'

insert into ores_trading_counterparty_scope_types_tbl (
    code, version, description, modified_by, change_reason_code, change_commentary
) values
    ('external',     0, 'A party outside the group',                        current_user, 'system.initial_load', 'Seed counterparty_scope_type'),
    ('inter_entity', 0, 'Another legal entity of the same group',           current_user, 'system.initial_load', 'Seed counterparty_scope_type'),
    ('intra_entity', 0, 'Another book in the same legal entity and branch', current_user, 'system.initial_load', 'Seed counterparty_scope_type')
on conflict (code, version)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Counterparty Scope Types' as entity, count(*) as count
from ores_trading_counterparty_scope_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
