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
 * The table is immutable, so a row is written once and a re-run inserts
 * nothing. This script is idempotent.
 */

\echo '--- Entry Channel Types ---'

insert into ores_trading_entry_channel_types_tbl (code, description) values
    ('manual',     'A user captured the trade'),
    ('stp',        'Straight-through processing from an upstream system'),
    ('ecn',        'An electronic communication network'),
    ('allocation', 'An allocation split from a block trade')
on conflict (code) do nothing;

select 'Entry Channel Types' as entity, count(*) as count
from ores_trading_entry_channel_types_tbl;
