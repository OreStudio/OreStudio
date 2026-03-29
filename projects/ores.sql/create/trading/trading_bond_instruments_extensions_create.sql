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

-- =============================================================================
-- Bond Instruments Extensions (Phase 7)
--
-- Adds nullable columns to support BondFuture, BondOption, BondTRS,
-- BondPosition, and Ascot trade types. All columns are nullable so that
-- existing rows (Bond, ForwardBond, etc.) are unaffected.
-- =============================================================================

alter table "ores_trading_bond_instruments_tbl"
    add column if not exists "future_expiry_date"  date             null,
    add column if not exists "option_type"          text             null,
    add column if not exists "option_expiry_date"   date             null,
    add column if not exists "option_strike"        numeric(28, 10)  null,
    add column if not exists "trs_return_type"      text             null,
    add column if not exists "trs_funding_leg_code" text             null,
    add column if not exists "ascot_option_type"    text             null;
