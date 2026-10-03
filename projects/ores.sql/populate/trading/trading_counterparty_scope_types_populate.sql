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
 * The table is immutable, so a row is written once and a re-run inserts
 * nothing. This script is idempotent.
 */

\echo '--- Counterparty Scope Types ---'

insert into ores_trading_counterparty_scope_types_tbl (code, description) values
    ('external',     'A party outside the group'),
    ('inter_entity', 'Another legal entity of the same group'),
    ('intra_entity', 'Another book in the same legal entity and branch')
on conflict (code) do nothing;

select 'Counterparty Scope Types' as entity, count(*) as count
from ores_trading_counterparty_scope_types_tbl;
