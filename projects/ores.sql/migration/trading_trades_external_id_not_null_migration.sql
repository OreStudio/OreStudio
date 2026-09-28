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
 * One-shot migration: the trade's external_id becomes non-nullable
 *
 * projects/ores.trading/modeling/ores.trading.trade.org declares
 * external_id as the trade's key and declared it nullable. A writer that
 * left it empty stored NULL, and the generated read by key compares the
 * column to '', which NULL never matches, so the row could not be read or
 * removed by the key the protocol addresses it with.
 *
 * The column is now not null and every writer supplies a value. A row
 * written before this change with no key is backfilled from its uuid so
 * the column can carry the constraint.
 *
 * On a freshly recreated database the create script already emits not
 * null and this migration is unnecessary. It exists for databases created
 * before the change.
 */

update ores_trading_trades_tbl
set external_id = 'TRD-MIGRATED-' || replace(id::text, '-', '')
where external_id is null
   or external_id = '';

alter table ores_trading_trades_tbl
    alter column external_id set not null;
