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
 * Entry Channel Types Population Script
 *
 * Seeds the closed set of entry channels: how a trade reached the firm's
 * books. The codes match the C++ enum domain::entry_channel.
 *
 * The table is temporal, so a re-run supersedes each live row with a new
 * version and leaves the live set unchanged. This script is idempotent.
 */

\echo '--- Entry Channel Types ---'

insert into ores_trading_entry_channel_types_tbl (
    code, version, description, modified_by, change_reason_code, change_commentary
) values
    ('manual',     0, 'A user captured the trade',                           current_user, 'system.initial_load', 'Seed entry_channel_type'),
    ('stp',        0, 'Straight-through processing from an upstream system', current_user, 'system.initial_load', 'Seed entry_channel_type'),
    ('ecn',        0, 'An electronic communication network',                  current_user, 'system.initial_load', 'Seed entry_channel_type'),
    ('allocation', 0, 'An allocation split from a block trade',              current_user, 'system.initial_load', 'Seed entry_channel_type')
on conflict (code, version)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Entry Channel Types' as entity, count(*) as count
from ores_trading_entry_channel_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
